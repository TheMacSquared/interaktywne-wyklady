// Punkty stop rozdziałów 5–6 (stops_ch5.R, stops_ch6.R): Ryc. 5.9, 6.6.
// Silnik: window.lcSF, rozkłady i losowanie: window.lcDist, pomocniki: window.lc03Sim (sim_charts.js).
//
//   s3p-normal  — Ryc. 5.9 „Normalny czy nie?”: zmienne z ankiety (config) na tle N(m, s)
//   s3p-friends — Ryc. 6.6 „Grupka przyjaciół”: średnie z zależnych ocen a obietnica CTG
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist, U = window.lc03Sim;
  if (!C || !S || !D || !U) return;
  var fmt = S.fmt;
  function pct(x, d) { return fmt(100 * x, d == null ? 1 : d) + "%"; }

  // Słupki w skali gęstości: [[l, r, gęstość]].
  function density(bins, n) { return bins.map(function (b) { return [b.l, b.r, b.n / (n * (b.r - b.l))]; }); }

  // ---- Ryc. 5.9: Normalny czy nie? ------------------------------------------
  // Statystyki jak w R: s = sd() (mianownik n − 1), skośność mean((v − m)³) / s³.
  function stats5d(v) {
    var m = U.mean(v), s = U.sd(v), out = 0, m3 = 0;
    v.forEach(function (t) { if (Math.abs(t - m) > 2 * s) out++; m3 += Math.pow(t - m, 3); });
    return { m: m, s: s, tail: out / v.length, skew: m3 / v.length / Math.pow(s, 3) };
  }

  var NORMAL = {
    prepare: function (state, v) {
      var it = state.config.vars[v] || state.config.vars.hk;
      if (state.key !== v) {
        state.key = v;
        state.x = it.x.filter(function (t) { return t != null && isFinite(t); });
        state.st = stats5d(state.x);
      }
      state.lab = it.lab;
    },
    draw: function (v, P, k, state) {
      var x = state.x, st = state.st, m = st.m, s = st.s;
      var hd = density(S.binsGgplot(x, 30), x.length);
      var lo = Math.min(U.min(x), m - 3.5 * s), hi = Math.max(U.max(x), m + 3.5 * s);
      var curve = D.seq(lo, hi, 400).map(function (t) { return [t, D.dnorm(t, m, s)]; });
      var a = Math.min(lo, hd[0][0]), b = Math.max(hi, hd[hd.length - 1][1]), pad = (b - a) * 0.03;
      var top = Math.max(U.max(hd.map(function (r) { return r[2]; })), D.dnorm(m, m, s)) * 1.06;
      return { x: [a - pad, b + pad], y: [0, top], noXGrid: true, xName: state.lab, yName: "Gęstość",
               layers: [{ type: "bins", data: hd, color: P.secondary, alpha: 0.75, stroke: P.surface },
                        { type: "line", data: curve, color: P.accent, width: 2.6 }],
               legend: [{ text: "Słupki: dane z ankiety", color: P.secondary, box: true },
                        { text: "Krzywa: rozkład normalny o tej samej średniej i SD", color: P.accent }],
               aria: "Histogram gęstości: " + state.lab + " (" + x.length + " osób) na tle rozkładu normalnego o tej samej średniej i SD; poza μ ± 2σ leży " +
                     pct(st.tail) + " obserwacji, model normalny przewiduje 4.6%" };
    },
    update: function (root, state) {
      S.setValue(root, "tail", pct(state.st.tail));
      S.setValue(root, "skew", fmt(state.st.skew, 2));
    }
  };

  // ---- Ryc. 6.6: Grupka przyjaciół -------------------------------------------
  // 3000 grupek po n osób: wspólny składnik g (zgodność grupki, ρ) i własne oceny e.
  // Ocena prowadzącego w skali 1–10: μ = 6, σ = 2. Stałe ziarno: te same ustawienia
  // dają te same słupki (jak set.seed(3602) w R).
  var GROUPS = 3000, MU = 6, SIGMA = 2;
  function friendsMeans(rho, n) {
    var g = D.rng(3602), common = g.many(GROUPS, g.norm), out = new Array(GROUPS);
    var a = Math.sqrt(rho), b = Math.sqrt(1 - rho);
    for (var i = 0; i < GROUPS; i++) {
      var e = 0;
      for (var j = 0; j < n; j++) e += g.norm();
      out[i] = MU + SIGMA * (a * common[i] + b * e / n);
    }
    return out;
  }
  function friendsVals(vals) {
    var rho = Number(vals.s3p_6b_rho), n = Number(vals.s3p_6b_n);
    return { rho: isFinite(rho) ? rho : 0, n: n > 0 ? n : 10 };
  }

  var FRIENDS = {
    prepare: function (state, v, vals) {
      var p = friendsVals(vals);
      state.rho = p.rho; state.n = p.n;
      U.resample(state, p.rho + "|" + p.n, function () { return friendsMeans(p.rho, p.n); });
    },
    draw: function (v, P, k, state) {
      var m = state.data, n = state.n, se = SIGMA / Math.sqrt(n), X = [2, 10];
      // coord_cartesian(xlim = c(2, 10)): słupki przycięte do zakresu osi.
      var hd = density(S.binsWidth(m, 0.1), m.length)
        .filter(function (r) { return r[1] > X[0] && r[0] < X[1]; })
        .map(function (r) { return [Math.max(X[0], r[0]), Math.min(X[1], r[1]), r[2]]; });
      var curve = D.seq(X[0], X[1], 400).map(function (t) { return [t, D.dnorm(t, MU, se)]; });
      var top = Math.max(U.max(hd.map(function (r) { return r[2]; })), D.dnorm(MU, MU, se)) * 1.06;
      return { x: X, y: [0, top], xTicks: [2, 4, 6, 8, 10], noXGrid: true,
               xName: "Średnia ocena prowadzącego w grupce (1–10)", yName: "Gęstość",
               layers: [{ type: "bins", data: hd, color: P.secondary, alpha: 0.75, stroke: P.surface },
                        { type: "line", data: curve, color: P.accent, width: 2.6 }],
               legend: [{ text: "Słupki: " + GROUPS + " grupek", color: P.secondary, box: true },
                        { text: "Krzywa: obietnica CTG, N(μ, σ/√n)", color: P.accent }],
               aria: "Histogram średnich ocen z " + GROUPS + " grupek po " + n + " osób przy ρ = " + fmt(state.rho, 1) +
                     " na tle krzywej N(6, 2/√" + n + "); SE z symulacji " + fmt(U.sd(m), 2) + ", σ/√n = " + fmt(se, 2) };
    },
    update: function (root, state) {
      var n = state.n;
      S.setValue(root, "se_sim", fmt(U.sd(state.data), 2));
      S.setValue(root, "se_ctg", fmt(SIGMA / Math.sqrt(n), 2));
      S.setValue(root, "n_eff", fmt(n / (1 + (n - 1) * state.rho), 1));
    },
    // Podpis: niezależne oceny (ρ = 0) albo zgodna grupka (ρ > 0).
    noteKey: function (vals) { return friendsVals(vals).rho === 0 ? "indep" : "dep"; }
  };

  window.lc03Stops56 = { stats5d: stats5d, friendsMeans: friendsMeans };
  S.register({ "s3p-normal": NORMAL, "s3p-friends": FRIENDS });
})();
