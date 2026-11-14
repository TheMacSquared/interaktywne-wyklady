// Prototypy punktów stop wykładu 06, grupa rb: ryciny lcSF liczone w przeglądarce.
// Kontenery w R: lc_sf_figure() w stop_proto_rb.R (z6p_2a_ui, z6p_2b_ui, z6p_3_ui, z6p_5_ui).
//
//   z6_hist_a  — Zmiana po zmianie (P2A): histogram stałych przedziałów co 1°C,
//                dopisywanie pomiarów z N(82, 3), krzywa modelu na życzenie.
//   z6_hist_b  — Ogon dochodzi ostatni (P2B): histogram z ogonem ponad 85°C oraz
//                ścieżka odsetka ponad progiem w funkcji liczby pomiarów (log10).
//   z6_pole    — Pole między dwiema kreskami (P3): P(a < T ≤ b) jako pole pod krzywą.
//   z6_linijka — Linijka σ (P5): pole μ ± kσ i linijka odchyleń nad osią.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcZ6;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Wspólne osie temperatury: stałe, przy ruchu kontrolek zmienia się rysunek, nie skala.
  var X = [68, 98], YMAX = 0.3, XT = [70, 75, 80, 85, 90, 95];
  var LIMIT_A = 20000, LIMIT_B = 10000, START_A = 8, START_B = 10, PROG_HI = 91;

  // Pomiary z N(mu, sd) losowane w przeglądarce.
  function sample(cfg, n) {
    var out = [];
    for (var i = 0; i < n; i++) out.push(cfg.mu + cfg.sd * U.rnorm(Math.random));
    return out;
  }

  // Histogram stałych przedziałów co 1°C od 68 do 98: [l, r, gęstość].
  // Gęstość = udział pomiarów w przedziale / szerokość (1°C); n to liczba wszystkich pomiarów.
  function bins(values, n) {
    var cnt = [], out = [], j;
    for (j = 0; j < X[1] - X[0]; j++) cnt.push(0);
    values.forEach(function (t) {
      if (t >= X[0] && t < X[1]) cnt[Math.floor(t - X[0])]++;
    });
    for (j = 0; j < cnt.length; j++) out.push([X[0] + j, X[0] + j + 1, n ? cnt[j] / n : 0]);
    return out;
  }

  // Średnia i odchylenie próby (n − 1); NaN, gdy danych za mało.
  function stats(values) {
    var n = values.length, m = n ? U.mean(values) : NaN, ss = 0;
    if (n > 1) values.forEach(function (t) { ss += (t - m) * (t - m); });
    return { n: n, mean: m, sd: n > 1 ? Math.sqrt(ss / (n - 1)) : NaN };
  }

  function show(x, digits) { return isFinite(x) ? U.fmt(x, digits) : "–"; }

  // Krok P2A i P2B: dopisanie pomiarów z ograniczeniem liczby pomiarów.
  function grow(state, add, limit) {
    var room = limit - state.data.length, m = Math.min(add, room);
    if (m > 0) state.data = state.data.concat(sample(state.config, m));
  }

  // ---- P2A: histogram i opcjonalna krzywa modelu --------------------------------
  var figA = {
    init: function (state) { state.data = sample(state.config, START_A); },
    act: function (state, act) {
      if (act === "reset") { state.data = sample(state.config, START_A); return; }
      var add = { add8: 8, add100: 100, add1000: 1000 }[act];
      if (add) grow(state, add, LIMIT_A);
    },
    draw: function (v, P, k, state) {
      var cfg = state.config, n = state.data.length, layers;
      layers = [{ type: "bins", data: bins(state.data, n), color: P.secondary, alpha: 0.75,
                  stroke: P.surface, lineWidth: 0.6 }];
      if (v === "on") layers.push({ type: "line", data: U.curve(cfg.mu, cfg.sd, X[0], X[1]), color: P.accent, width: 2.4 });
      return { x: X, y: [0, YMAX], xTicks: XT, xName: "Temperatura łożyska (°C)", yName: "Gęstość",
        layers: layers,
        aria: "Histogram " + n + " pomiarów temperatury w przedziałach co 1 stopień" +
          (v === "on" ? ", z krzywą modelu N(82, 3)." : ".") };
    },
    update: function (root, state) {
      var s = stats(state.data);
      S.setValue(root, "n", String(s.n));
      S.setValue(root, "mean", show(s.mean, 2));
      S.setValue(root, "sd", show(s.sd, 2));
    }
  };

  // ---- P2B: histogram z ogonem i ścieżka odsetka --------------------------------
  // Stan: data (pomiary), over (ponad 85°C), over91 (ponad 91°C), path (odsetek od n = 10).
  function addB(state, values) {
    var c = state.config.c;
    values.forEach(function (t) {
      state.data.push(t);
      if (t > c) state.over++;
      if (t > PROG_HI) state.over91++;
      var n = state.data.length;
      if (n >= START_B) state.path.push([Math.log10(n), state.over / n]);
    });
  }

  function startB(state) {
    state.data = []; state.over = 0; state.over91 = 0; state.path = [];
    addB(state, sample(state.config, START_B));
  }

  var figB = {
    init: function (state) { startB(state); },
    act: function (state, act) {
      if (act === "reset") { startB(state); return; }
      var add = { add10: 10, add100: 100, add1000: 1000 }[act];
      if (!add) return;
      var room = LIMIT_B - state.data.length, m = Math.min(add, room);
      if (m > 0) addB(state, sample(state.config, m));
    },
    draw: function (v, P, k, state) {
      var cfg = state.config;
      if (k === 0) {
        var all = bins(state.data, state.data.length);
        var below = all.filter(function (b) { return b[0] < cfg.c; });
        var above = all.filter(function (b) { return b[0] >= cfg.c; });
        return { x: X, y: [0, YMAX], xTicks: XT, xName: "Temperatura łożyska (°C)", yName: "Gęstość",
          layers: [
            { type: "bins", data: below, color: P.secondary, alpha: 0.75, stroke: P.surface, lineWidth: 0.6 },
            { type: "bins", data: above, color: P.accent, alpha: 0.85, stroke: P.surface, lineWidth: 0.6 },
            { type: "line", data: U.curve(cfg.mu, cfg.sd, X[0], X[1]), color: P.ref, width: 1.2, dash: [4, 3] },
            { type: "vline", x: cfg.c, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "text", x: cfg.c, y: "top", text: "próg " + cfg.c + "°C", color: P.ink, align: "left", mono: true }
          ],
          aria: "Histogram " + state.data.length + " pomiarów; słupki od 85 stopni w górę w kolorze akcentu, przerywana krzywa modelu." };
      }
      // Ścieżka odsetka: oś x = log10(n), zakres [1, 4] to 10 … 10 000 pomiarów.
      var tail = 1 - U.pnorm(cfg.c, cfg.mu, cfg.sd);
      return { x: [1, 4], y: [0, 0.4], xTicks: [1, 2, 3, 4],
        xLabels: { 1: "10", 2: "100", 3: "1000", 4: "10 000" },
        xName: "Liczba pomiarów", yName: "Odsetek ponad progiem",
        layers: [
          { type: "line", data: state.path, color: P.accent, width: 2 },
          // Przy krótkiej ścieżce pojedyncze odsetki są widoczne jako kropki.
          state.path.length <= 60 ? { type: "points", data: state.path, color: P.accent, size: 5 } : null,
          { type: "line", data: [[1, tail], [4, tail]], color: P.ref, width: 1.4, dash: [5, 4] },
          { type: "text", x: 2.9, y: tail, text: "model: " + U.fmt(tail, 3), color: P.soft, align: "left" }
        ].filter(Boolean),
        aria: "Odsetek pomiarów ponad 85 stopni w zależności od liczby pomiarów, linia modelu na 0.159." };
    },
    update: function (root, state) {
      var n = state.data.length;
      S.setValue(root, "n", String(n));
      S.setValue(root, "over85", state.over + " z " + n + " (" + U.fmt(100 * state.over / n, 1) + "%)");
      S.setValue(root, "over91", state.over91 + " z " + n);
    }
  };

  // ---- P3: pole między dwiema kreskami ------------------------------------------
  var PRESETS = { wide: [79, 85], ex: [80, 85], narrow: [81.95, 82.05], point: [82, 82] };

  function sliders(root) {
    return {
      a: Number(root.querySelector("#z6p3_a").value),
      b: Number(root.querySelector("#z6p3_b").value)
    };
  }

  function setSlider(root, id, value) {
    var input = root.querySelector("#" + id);
    var out = root.querySelector('output[data-value="' + id + '"]');
    input.value = value;
    if (out) out.textContent = String(Number(value.toFixed(2)));
  }

  var figPole = {
    // Korzeń figury zapisany w stanie: draw czyta suwaki z DOM.
    init: function (state, root) { state.root = root; },
    act: function (state, act, root) {
      var pr = PRESETS[act];
      if (!pr) return;
      setSlider(root, "z6p3_a", pr[0]);
      setSlider(root, "z6p3_b", pr[1]);
    },
    draw: function (v, P, k, state) {
      var cfg = state.config, mu = cfg.mu, sd = cfg.sd;
      var s = sliders(state.root), lo = Math.min(s.a, s.b), hi = Math.max(s.a, s.b);
      return { x: X, y: [0, 0.16], xTicks: XT, xName: "Temperatura łożyska (°C)", yName: "Gęstość",
        layers: [
          { type: "line", data: U.curve(mu, sd, lo, hi), color: P.niebo, width: 0, fill: P.niebo, fillAlpha: 0.45 },
          { type: "line", data: U.curve(mu, sd, X[0], X[1]), color: P.secondary, width: 2.2 },
          { type: "vline", x: lo, color: P.ink, width: 1.8, from: 0, to: U.dnorm(lo, mu, sd) },
          { type: "vline", x: hi, color: P.ink, width: 1.8, from: 0, to: U.dnorm(hi, mu, sd) }
        ],
        aria: "Krzywa N(82, 3) z polem między " + U.fmt(lo, 2) + " a " + U.fmt(hi, 2) + " stopnia." };
    },
    update: function (root, state) {
      var mu = state.config.mu, sd = state.config.sd;
      var s = sliders(root), lo = Math.min(s.a, s.b), hi = Math.max(s.a, s.b);
      var p = Math.max(0, U.pnorm(hi, mu, sd) - U.pnorm(lo, mu, sd));
      S.setValue(root, "p", U.fmtP(p));
      S.setValue(root, "w", U.fmt(hi - lo, 2) + "°C");
      S.setValue(root, "h", U.fmt(U.dnorm((lo + hi) / 2, mu, sd), 3));
    }
  };

  // ---- P5: linijka σ -------------------------------------------------------------
  function sigmaLabel(z) { return z === 0 ? "μ" : (z > 0 ? "+" : "−") + Math.abs(z) + "σ"; }

  var figRuler = {
    draw: function (v, P, k, state) {
      var cfg = state.config, mu = cfg.mu, sd = cfg.sd, kk = Number(v);
      var lo = mu - kk * sd, hi = mu + kk * sd, layers, z, x;
      layers = [
        { type: "line", data: U.curve(mu, sd, lo, hi), color: P.szalwia, width: 0, fill: P.szalwia, fillAlpha: 0.45 },
        { type: "line", data: U.curve(mu, sd, X[0], X[1]), color: P.secondary, width: 2.2 }
      ];
      for (z = -3; z <= 3; z++) {
        x = mu + z * sd;
        layers.push({ type: "vline", x: x, color: P.subtle, width: 1, from: 0, to: U.dnorm(x, mu, sd) });
      }
      for (z = -3; z <= 3; z++) {
        layers.push({ type: "text", x: mu + z * sd, y: 0.006, text: sigmaLabel(z), color: P.ink,
                      mono: true, align: "center", halo: true });
      }
      return { x: [70, 94], y: [0, 0.16], xTicks: [73, 76, 79, 82, 85, 88, 91],
        xName: "Temperatura łożyska (°C)", yName: "Gęstość", layers: layers,
        aria: "Krzywa N(82, 3) z polem w przedziale μ ± " + kk + "σ, linijka odchyleń od −3σ do +3σ." };
    },
    update: function (root, state, v) {
      var cfg = state.config, mu = cfg.mu, sd = cfg.sd, kk = Number(v);
      var lo = mu - kk * sd, hi = mu + kk * sd;
      var inside = U.pnorm(hi, mu, sd) - U.pnorm(lo, mu, sd);
      var tail = 1 - U.pnorm(hi, mu, sd);
      S.setValue(root, "range", U.fmt(lo, 0) + "–" + U.fmt(hi, 0) + "°C");
      S.setValue(root, "inside", U.fmt(100 * inside, 1) + "%");
      S.setValue(root, "outside", U.fmt(100 * tail, 100 * tail < 1 ? 2 : 1) + "%");
    }
  };

  S.register({
    z6_hist_a: figA,
    z6_hist_b: figB,
    z6_pole: figPole,
    z6_linijka: figRuler
  });
})();
