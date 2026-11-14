// Wykresy wykładu 08 „Niezawodność systemu” liczone w przeglądarce (dawniej ggplot
// na serwerze). Silnik: window.lcSF (shared/lc_stat_figures.js), funkcje kursu:
// window.lcRisk (R/risk_figures.js). Przedrostek s8m_ odróżnia je od rycin
// prototypów punktów stop (modules/proto_*.js).
//
//   s8m_arch         — Te same R, inny system: słupki dwóch elementów i systemu
//   s8m_time         — R(t) trzech elementów i systemu mieszanego
//   s8m_common       — Wspólna przyczyna obniża niezawodność redundancji (odczyt udziału z R)
//   s8m_redundancy   — R układu równoległego względem liczby gałęzi
//   s8m_improvement  — Wzrost R szeregu po poprawie jednego elementu (liczby z R)
//
// Funkcje liczące są w window.lcS8Charts (testy: tests/testthat/test-s8-wykresy.R).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var R = root.lcRisk;

  // risk_series_reliability() i risk_parallel_reliability().
  function series(r) { return r.reduce(function (a, b) { return a * b; }, 1); }
  function parallel(r) { return 1 - r.reduce(function (a, b) { return a * (1 - b); }, 1); }
  // R elementów i systemu mieszanego w chwili t (MTTF 1800, 2000 i 2500 h, przykład 8.6).
  function timeValues(t) {
    var ra = Math.exp(-t / 1800), rb = Math.exp(-t / 2000), rc = Math.exp(-t / 2500);
    return { a: ra, b: rb, c: rc, system: rc * (1 - (1 - ra) * (1 - rb)) };
  }
  // Układ równoległy n jednakowych gałęzi: R, przyrost od poprzedniej gałęzi i koszt.
  function redundancy(n, r) {
    var now = 1 - Math.pow(1 - r, n);
    return { r: now, gain: n > 1 ? now - (1 - Math.pow(1 - r, n - 1)) : null, cost: n * 100 };
  }
  root.lcS8Charts = { series: series, parallel: parallel, timeValues: timeValues, redundancy: redundancy };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S || !R) return;

  var DASH = [6, 5], figures = {};
  function num(el, id) { return Number(el.querySelector("#" + id).value); }
  // Słupki kategorii o szerokości w wokół pozycji 1, 2, 3…
  function bars(values, w) { return values.map(function (y, i) { return [i + 1 - w / 2, i + 1 + w / 2, y]; }); }
  function labels(names) { var out = {}; names.forEach(function (n, i) { out[i + 1] = n; }); return out; }

  function archValue(el) {
    var r = [num(el, "s8_r1"), num(el, "s8_r2")];
    return el.querySelector("#s8_arch").value === "series" ? series(r) : parallel(r);
  }
  figures.s8m_arch = {
    draw: function (v, P, k, state, vals) {
      var r1 = Number(vals.s8_r1), r2 = Number(vals.s8_r2);
      var sys = vals.s8_arch === "series" ? series([r1, r2]) : parallel([r1, r2]);
      var all = bars([r1, r2, sys], 0.65);
      // Elementy w kolorze kontekstu, system w akcencie.
      var layers = [{ type: "bins", data: all.slice(0, 2), color: P.secondary, stroke: P.surface },
                    { type: "bins", data: all.slice(2), color: P.accent, stroke: P.surface }];
      [r1, r2, sys].forEach(function (y, i) { layers.push({ type: "text", x: i + 1, y: y, text: R.fixed(y, 3), color: P.ink, mono: true }); });
      return { x: [0.4, 3.6], y: [0, 1.08], layers: layers, xLabels: labels(["Element 1", "Element 2", "System"]),
        xName: "", yName: "Niezawodność", aria: "Słupki niezawodności dwóch elementów i systemu dla wybranej architektury." };
    },
    update: function (el) {
      var r = archValue(el);
      S.setValue(el, "arch_r", R.fmtP(r)); S.setValue(el, "arch_risk", R.fmtP(1 - r));
    }
  };

  figures.s8m_time = {
    draw: function (v, P) {
      var t = Number(v), cat = R.cat(4);
      // System w akcencie i grubszą linią; kolejność jak w dawnej legendzie.
      var parts = [["Sterownik", "c", P.secondary, 2], ["System", "system", P.accent, 3.6],
                   ["Wentylator A", "a", cat[2], 2], ["Wentylator B", "b", cat[3], 2]];
      var layers = parts.map(function (p) {
        return { type: "line", data: R.curve(function (x) { return timeValues(x)[p[1]]; }, 0, 3000, 400), color: p[2], width: p[3] };
      });
      layers.push({ type: "vline", x: t, color: P.ref, width: 1.4, dash: DASH });
      var now = timeValues(t);
      parts.forEach(function (p) { layers.push({ type: "point", x: t, y: now[p[1]], color: p[2], size: 11 }); });
      return { x: [0, 3000], y: [0, 1], layers: layers,
        legend: parts.map(function (p) { return { color: p[2], text: p[0] }; }),
        xName: "Czas (h)", yName: "R(t)", aria: "Krzywe niezawodności trzech elementów i systemu mieszanego." };
    },
    update: function (el, state, v) {
      var now = timeValues(Number(v));
      S.setValue(el, "time_a", R.fmtP(now.a)); S.setValue(el, "time_b", R.fmtP(now.b));
      S.setValue(el, "time_c", R.fmtP(now.c)); S.setValue(el, "time_sys", R.fmtP(now.system));
    }
  };

  figures.s8m_common = {
    draw: function (v, P, k, state) {
      var q = Number(v), base = state.config.base;
      return { x: [0, 0.15], y: [0.84, 1],
        layers: [{ type: "line", data: [[0, base], [0.15, 0.85 * base]], color: P.accent, width: 2.4 },
                 // Obietnica samego układu równoległego (przykład 8.4) jako linia odniesienia.
                 { type: "line", data: [[0, base], [0.15, base]], color: P.ref, width: 1.4, dash: DASH },
                 { type: "text", x: 0.15, y: base, text: state.config.base_text, color: P.ref, align: "right" },
                 { type: "point", x: q, y: (1 - q) * base, color: P.secondary, size: 11 }],
        xName: "P(wspólnej awarii)", yName: "R systemu",
        aria: "Malejąca niezawodność układu redundantnego wraz ze wzrostem wspólnej przyczyny." };
    },
    update: function (el, state, v) {
      var q = Number(v), r = (1 - q) * state.config.base;
      S.setValue(el, "common_r", R.fmtP(r)); S.setValue(el, "common_risk", R.fmtP(1 - r));
      // Część ryzyka, która pochodzi ze zdarzenia wspólnego (przykład 8.8: 0.72).
      S.setValue(el, "common_share", state.config.share[Math.round(q / state.config.step)]);
    }
  };

  figures.s8m_redundancy = {
    draw: function (v, P, k, state, vals) {
      var n = Number(vals.s8_branches), r = Number(vals.s8_branch_r), pts = [];
      for (var i = 1; i <= 6; i++) pts.push([i, redundancy(i, r).r]);
      // Stała oś: suwak R gałęzi zmienia krzywą, nie skalę.
      return { x: [0.75, 6.25], y: [0.5, 1], xTicks: [1, 2, 3, 4, 5, 6],
        layers: [{ type: "line", data: pts, color: P.accent, width: 2.4 },
                 { type: "points", data: pts, color: P.accent, size: 6 },
                 { type: "point", x: n, y: redundancy(n, r).r, color: P.secondary, size: 14 }],
        xName: "Liczba gałęzi", yName: "R systemu", aria: "Krzywa niezawodności równoległej względem liczby gałęzi." };
    },
    update: function (el) {
      var d = redundancy(num(el, "s8_branches"), num(el, "s8_branch_r"));
      S.setValue(el, "red_r", R.fmtP(d.r));
      S.setValue(el, "red_gain", d.gain === null ? "—" : R.fmtP(d.gain));
      S.setValue(el, "red_cost", d.cost + " jedn.");
    }
  };

  figures.s8m_improvement = {
    draw: function (v, P, k, state) {
      var cfg = state.config, layers = [{ type: "bins", data: bars(cfg.gains, 0.55), color: P.secondary, stroke: P.surface }];
      cfg.gains.forEach(function (g, i) { layers.push({ type: "text", x: i + 1, y: g, text: cfg.text[i], color: P.ink, mono: true }); });
      return { x: [0.4, 3.6], y: [0, 0.022], layers: layers, xLabels: labels(cfg.labels),
        xName: "", yName: "Wzrost R systemu",
        aria: "Słupki wzrostu niezawodności szeregu po poprawie każdego z trzech elementów o 0.02." };
    }
  };

  S.register(figures);
})();
