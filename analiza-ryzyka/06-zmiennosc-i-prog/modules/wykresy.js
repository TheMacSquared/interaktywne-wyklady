// Wykresy wykładu 06 „Przekroczenie progu” liczone w przeglądarce (dawniej ggplot
// na serwerze). Silnik: window.lcSF (shared/lc_stat_figures.js).
//
//   z6_shapes  — Trzy kształty na wykresie kwantylowym (dane z R)
//
// Pozostałe dawne wykresy zastąpiły przyjęte punkty stop (modules/proto_*.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S) return;

  var figures = {};

  // Wykres kwantylowy (Q–Q) trzech kształtów o średniej 82 i SD 3 (dane z R):
  // punkty próby i prosta przez kwartyle. Porównanie samych ogonów pokazuje P14A.
  figures.z6_shapes = {
    draw: function (v, P, k, state, vals) {
      var d = state.config[vals.z6_shape];
      var r = d.qq_range, padX = (r[1] - r[0]) * 0.05, padY = (r[3] - r[2]) * 0.05;
      var x0 = r[0] - padX, x1 = r[1] + padX, a = d.qq_line[0], b = d.qq_line[1];
      return { x: [x0, x1], y: [r[2] - padY, r[3] + padY],
        layers: [{ type: "points", data: d.qq, color: P.secondary, size: 5, alpha: 0.4 },
                 { type: "line", data: [[x0, a + b * x0], [x1, a + b * x1]], color: P.accent, width: 2.2 }],
        xName: "Kwantyle teoretyczne (normalne)", yName: "Kwantyle próby (°C)",
        aria: "Wykres kwantylowy wybranego rozkładu względem modelu normalnego." };
    }
  };

  S.register(figures);
})();
