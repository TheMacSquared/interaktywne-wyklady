// Prototypy punktów stop wykładu 10, grupa rb. Scena „rok” (P3) rejestrowana
// w silniku window.lcI10 (sceny.js). R podaje w data-config okna niesprawności
// wentylatorów (i10p3_cfg w stop_proto_rb.R); w tym pliku nie ma liczb modelu.
//
//   rok — Scena „Awaria to nie niedostępność” (P3): oś roku, paski wentylatorów A i B
//         oraz układu chłodzenia; suwak wybiera chwilę zapotrzebowania.
//         Układ jest niedostępny, gdy niesprawne są oba wentylatory naraz
//         (części wspólne okien A i B).
(function () {
  "use strict";
  var Z = window.lcI10;
  if (!Z) return;
  var U = Z.util;

  var X0 = 170, W = 600, BH = 26;          // oś x: od X0 przez W pikseli
  var ROW_A = 50, ROW_B = 100, ROW_S = 150; // górne krawędzie pasków
  var AXIS_Y = 205, H = 250;

  // Czy dzień d leży w którymś z okien [od, do].
  function down(windows, d) {
    return windows.some(function (w) { return d >= w[0] && d <= w[1]; });
  }
  // Części wspólne okien A i B (układ niedostępny).
  function common(wa, wb) {
    var out = [];
    wa.forEach(function (a) {
      wb.forEach(function (b) {
        var lo = Math.max(a[0], b[0]), hi = Math.min(a[1], b[1]);
        if (lo <= hi) out.push([lo, hi]);
      });
    });
    return out;
  }
  // Suma długości okien (dni).
  function total(windows) {
    return windows.reduce(function (s, w) { return s + (w[1] - w[0]); }, 0);
  }

  Z.register({
    rok: function (cfg, ui) {
      var days = cfg.days;
      var sys = common(cfg.a, cfg.b);
      var xOf = function (d) { return X0 + d / days * W; };

      // Pasek: od 0 do end w klasie cls; okna w klasie warn/bad.
      function row(y, windows, cls) {
        var base = '<rect class="i10p-fill-ok" x="' + U.g(xOf(0)) + '" y="' + y + '" width="' +
          U.g(xOf(days) - xOf(0)) + '" height="' + BH + '" rx="3"/>';
        return base + windows.map(function (w) {
          return '<rect class="' + cls + '" x="' + U.g(xOf(w[0])) + '" y="' + y + '" width="' +
            U.g(xOf(w[1]) - xOf(w[0])) + '" height="' + BH + '" rx="3"/>';
        }).join("");
      }

      function label(y, text) {
        return '<text x="0" y="' + (y + 18) + '" font-size="14">' + U.esc(text) + "</text>";
      }

      function sentence(d) {
        var aDown = down(cfg.a, d), bDown = down(cfg.b, d);
        var head = "Dzień " + d + ": ";
        if (aDown && bDown) return head + "A i B w naprawie. Ochrony brak.";
        if (aDown) return head + "A w naprawie, B pracuje. Ochrona jest.";
        if (bDown) return head + "B w naprawie, A pracuje. Ochrona jest.";
        return head + "oba wentylatory sprawne. Ochrona jest.";
      }

      return {
        on: {},
        render: function () {
          var d = U.sliderValue(ui.root, "i10p3_day", cfg.day);
          var ticks = [0, 60, 120, 180, 240, 300, 360];
          var limitX = xOf(d);
          var anchor = d > days * 0.8 ? "end" : "middle";
          var body =
            label(ROW_A, "Wentylator A") + row(ROW_A, cfg.a, "i10p-fill-warn") +
            label(ROW_B, "Wentylator B") + row(ROW_B, cfg.b, "i10p-fill-warn") +
            label(ROW_S, "Układ chłodzenia") + row(ROW_S, sys, "i10p-fill-bad") +
            U.axis(xOf, AXIS_Y, ticks, "dzień", ticks.map(String)) +
            '<line class="i10p-limit" x1="' + U.g(limitX) + '" x2="' + U.g(limitX) + '" y1="32" y2="' + AXIS_Y + '"/>' +
            '<text class="i10p-subtle" x="' + U.g(limitX) + '" y="24" font-size="12" text-anchor="' + anchor + '">zapotrzebowanie</text>';
          ui.set("stage", U.svgWrap(800, H,
            "Okna niesprawności wentylatorów A i B oraz niedostępności układu w roku; zapotrzebowanie w dniu " + d + ".",
            body));
          ui.set("status",
            '<p class="i10p-read">' + U.esc(sentence(d)) + "</p>" +
            '<p class="i10p-caption">' + U.esc("A niesprawny przez " + total(cfg.a) + " dni w roku, układ niedostępny przez " +
              total(sys) + " dni.") + "</p>");
        }
      };
    }
  });
})();
