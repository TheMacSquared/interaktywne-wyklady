// Wykresy wykładu 06 „Zmienność i próg” liczone w przeglądarce (dawniej ggplot
// na serwerze). Silnik: window.lcSF (shared/lc_stat_figures.js).
//
//   z6_shapes  — Trzy rozkłady o podobnym centrum: histogram albo wykres kwantylowy (dane z R)
//
// Pozostałe dawne wykresy zastąpiły przyjęte punkty stop (modules/proto_*.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S) return;

  var X_NAME = "Temperatura (°C)", figures = {};

  figures.z6_shapes = {
    draw: function (v, P, k, state, vals) {
      var d = state.config[vals.z6_shape];
      if (vals.z6_view === "qq") {
        var r = d.qq_range, padX = (r[1] - r[0]) * 0.05, padY = (r[3] - r[2]) * 0.05;
        var x0 = r[0] - padX, x1 = r[1] + padX, a = d.qq_line[0], b = d.qq_line[1];
        return { x: [x0, x1], y: [r[2] - padY, r[3] + padY],
          layers: [{ type: "points", data: d.qq, color: P.secondary, size: 5, alpha: 0.4 },
                   { type: "line", data: [[x0, a + b * x0], [x1, a + b * x1]], color: P.accent, width: 2.2 }],
          xName: "Kwantyle teoretyczne (normalne)", yName: "Kwantyle próby (°C)",
          aria: "Wykres kwantylowy wybranego rozkładu względem modelu normalnego." };
      }
      var top = Math.max.apply(null, d.bins.map(function (b) { return b[2]; }));
      return { x: [65, 105], y: [0, top * 1.05],
        layers: [{ type: "bins", data: d.bins, color: P.secondary, stroke: P.surface },
                 { type: "vline", x: 85, color: P.accent, width: 2.2, dash: [6, 5] }],
        xName: X_NAME, yName: "Liczba obserwacji", aria: "Histogram wybranego rozkładu z zaznaczonym progiem 85°C." };
    },
    // Odczyty z modelu (nie z próby): te same liczby co w przykładzie 6.10.
    update: function (el, state) {
      var d = state.config[el.querySelector("#z6_shape").value];
      S.setValue(el, "shapes_p85", d.p85); S.setValue(el, "shapes_p91", d.p91);
    }
  };

  S.register(figures);
})();
