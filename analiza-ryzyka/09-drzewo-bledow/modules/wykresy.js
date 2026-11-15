// Ryciny wykładu 09 „Analiza drzewa błędów” liczone w przeglądarce (dawniej wykresy
// ggplot i odczyty z serwera). Silnik: window.lcSF (shared/lc_stat_figures.js),
// funkcje kursu: window.lcRisk (R/risk_figures.js). Przedrostek f9m_ odróżnia je
// od rycin prototypów punktów stop (modules/proto_*.js).
//
//   f9m_tree    — Małe drzewo błędów: rysunek z R, liczby przy węzłach z suwaków (bez wykresu)
//   f9m_repeat  — Powtórzony liść: jedno zdarzenie i dwa błędne rachunki (same odczyty)
//   f9m_common  — Wspólna przyczyna: P(TOP) bez niej i z nią (same odczyty; czyta też
//                 suwaki drzewa z rozdziału o rachunku)
//   f9m_rank    — Redukcja P(TOP) po tej samej względnej poprawie każdego liścia
//
// Funkcje liczące są w window.lcF9Charts (testy: tests/testthat/test-f9-wykresy.R).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var R = root.lcRisk;

  // risk_gate_or() i risk_fta_top(): P(I) · P(D ∪ S | I).
  function gateOr(p) { return 1 - p.reduce(function (a, b) { return a * (1 - b); }, 1); }
  function ftaTop(init, detect, suppress) { return init * gateOr([detect, suppress]); }
  // P(TOP) ze wspólną przyczyną C: I ∧ (C ∨ D₀ ∨ S₀).
  function commonTop(init, detect, suppress, q) { return init * (q + (1 - q) * gateOr([detect, suppress])); }
  // Spadek P(TOP) po zmniejszeniu jednego parametru o ułamek r (kolejność: I, D, S).
  function rankGains(base, r) {
    var top0 = ftaTop(base.init, base.detect, base.suppress);
    return ["init", "detect", "suppress"].map(function (name) {
      var x = { init: base.init, detect: base.detect, suppress: base.suppress };
      x[name] = x[name] * (1 - r);
      return top0 - ftaTop(x.init, x.detect, x.suppress);
    });
  }
  root.lcF9Charts = { gateOr: gateOr, ftaTop: ftaTop, commonTop: commonTop, rankGains: rankGains };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S || !R) return;

  var DASH = [6, 5], figures = {};
  // Suwaki drzewa stoją w rozdziale o rachunku; inne ryciny czytają je z całego dokumentu.
  function tree() {
    var get = function (id, fallback) { var el = document.getElementById(id); return el ? Number(el.value) : fallback; };
    return { init: get("f9_init", 0.005), detect: get("f9_detect", 0.05), suppress: get("f9_suppress", 0.08) };
  }

  figures.f9m_tree = {
    update: function (el) {
      var t = tree(), or = gateOr([t.detect, t.suppress]), top = ftaTop(t.init, t.detect, t.suppress);
      // Liczby z suwaków stoją przy węzłach: rysunek reaguje na parametry, a nie tylko odczyty.
      S.setValue(el, "tree_top", R.fmtPd(top, 6)); S.setValue(el, "tree_or", R.fmtP(or));
      S.setValue(el, "tree_init", String(t.init)); S.setValue(el, "tree_detect", String(t.detect));
      S.setValue(el, "tree_suppress", String(t.suppress));
      S.setValue(el, "tree_or_read", R.fmtP(or)); S.setValue(el, "tree_top_read", R.fmtP(top));
    }
  };

  figures.f9m_repeat = {
    update: function (el, state, v) {
      var q = Number(v);
      S.setValue(el, "repeat_one", R.fmtP(q)); S.setValue(el, "repeat_or", R.fmtP(gateOr([q, q])));
      S.setValue(el, "repeat_and", R.fmtP(q * q));
    }
  };

  figures.f9m_common = {
    // Zmiana suwaków drzewa (inny rozdział) odświeża odczyty.
    init: function (state, el) {
      document.addEventListener("input", function (e) {
        if (!el.isConnected) return;
        if (["f9_init", "f9_detect", "f9_suppress"].indexOf(e.target.id) < 0) return;
        figures.f9m_common.update(el, state, el.querySelector("#f9_common").value);
      });
    },
    update: function (el, state, v) {
      var t = tree();
      S.setValue(el, "common_without", R.fmtPd(ftaTop(t.init, t.detect, t.suppress), 6));
      S.setValue(el, "common_with", R.fmtPd(commonTop(t.init, t.detect, t.suppress, Number(v)), 6));
    }
  };

  figures.f9m_rank = {
    draw: function (v, P, k, state) {
      var cfg = state.config, gains = rankGains(cfg.base, Number(v)), cols = R.cat(3), layers = [], labels = {};
      gains.forEach(function (g, i) {
        labels[i + 1] = cfg.labels[i];
        layers.push({ type: "bins", data: [[i + 0.55, i + 1.45, g]], color: cols[i], stroke: P.surface });
      });
      // Stała oś Y do wyjściowego P(TOP): suwak zmienia wysokość słupków, a nie skalę.
      layers.push({ type: "line", data: [[0.4, cfg.top0], [3.6, cfg.top0]], color: P.ref, width: 1.4, dash: DASH });
      layers.push({ type: "text", x: 3.45, y: cfg.top0, text: cfg.top_text, color: P.ref, align: "right" });
      // Wartości z tekstu stoją nad słupkami.
      gains.forEach(function (g, i) { layers.push({ type: "text", x: i + 1, y: g, text: R.fixed(g, 6), color: P.ink }); });
      return { x: [0.4, 3.6], y: [0, 0.00068], layers: layers, xLabels: labels,
        xName: "", yName: "Redukcja P(TOP)",
        aria: "Słupki redukcji prawdopodobieństwa zdarzenia szczytowego po poprawie każdego liścia." };
    }
  };

  S.register(figures);
})();
