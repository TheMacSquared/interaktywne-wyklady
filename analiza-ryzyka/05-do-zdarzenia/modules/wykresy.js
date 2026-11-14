// Wykresy wykładu 05 liczone w przeglądarce (dawniej ggplot na serwerze).
// Silnik: window.lcSF (R/lc_stat_figures.js, ECharts SVG), rozkłady: window.lcDist.
//
//   d5_geom — Rozkład geometryczny dla trzech wartości p (pola wyboru), oś do
//             kwantyla 0.999 najdłuższego z wybranych, najwyżej 80 kontroli.
//
// Piaskownice z uchwytem (nb_chart.js, plan_chart.js, mix_chart.js) używają ECharts
// bezpośrednio, bez lcSF: przeciąganie kreski i klawisze obsługuje lcCharts.handleLayer.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist;
  if (!C || !S || !D) return;

  var CAT = ["grafit", "bursztyn", "niebo"];

  S.register({
    d5_geom: {
      prepare: function (state, v) { state.sel = (v || []).filter(function (k) { return state.config.defs[k]; }); },
      draw: function (v, P, k, state) {
        var sel = state.sel, defs = state.config.defs, m = sel.length;
        if (!m) return null;
        // qgeom(0.999, p) + 1 w R to D.qgeom (numer próby z pierwszym sukcesem)
        var xmax = Math.min(80, Math.max.apply(null, sel.map(function (key) { return D.qgeom(0.999, defs[key].p); })));
        var layers = [], legend = [], ymax = 0;
        sel.forEach(function (key, i) {
          var s = defs[key], color = C.token("--upwr-cat-" + CAT[i]);
          var off = m > 1 ? (i - (m - 1) / 2) * 0.5 / m : 0, pts = [], segs = [];
          for (var x = 1; x <= xmax; x++) {
            var y = D.dgeom(x, s.p); ymax = Math.max(ymax, y);
            pts.push([x + off, y]); segs.push({ from: [x + off, 0], to: [x + off, y] });
          }
          layers.push({ type: "segs", items: segs, color: color, width: 1.6, alpha: 0.6 },
                      { type: "points", data: pts, color: color, size: 9, alpha: 0.85 });
          legend.push({ text: s.label, color: color });
        });
        return { x: [0.4, xmax + 0.6], y: [0, ymax * 1.1], layers: layers, legend: legend,
                 xInt: true, noXGrid: true, xName: "Numer kontroli z pierwszą wadą (x)", yName: "P(X = x)",
                 aria: "Rozkłady geometryczne dla wybranych wartości p nałożone na jednej osi." };
      },
      update: function (root, state) {
        var out = root.querySelector('[data-role="stats"]');
        if (!out) return;
        out.innerHTML = state.sel.length ? "<ul>" + state.sel.map(function (key) {
          var s = state.config.defs[key];
          return "<li>" + S.esc(s.label + ": E(X) = " + s.mean + ", SD = " + s.sd) + "</li>";
        }).join("") + "</ul>" : "";
      }
    }
  });
})();
