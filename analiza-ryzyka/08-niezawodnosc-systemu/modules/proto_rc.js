// Prototypy punktów stop wykładu 08: scena losowa „misje” (P2B).
//
//   misje — symulacja „Misja za misją”: w każdej misji losujemy stany dwóch
//           elementów (R = cfg.r) i oceniamy je w trzech logikach sukcesu:
//           oba wymagane, tylko E₁, wystarczy jeden. Liczniki porażek narastają
//           (+1 / +10 / +100 / +1000); pasek pokazuje ostatnie 100 misji.
// Kontener w R: s8p_scene_ui(…, "misje", …) w stop_proto_rc.R.
(function () {
  "use strict";
  var Z = window.lcS8;
  if (!Z) return;
  var U = Z.util;

  var LOGICS = [
    { key: "series", name: "Oba wymagane", fail: function (a, b) { return !(a && b); } },
    { key: "single", name: "Tylko E₁", fail: function (a) { return !a; } },
    { key: "parallel", name: "Wystarczy jeden", fail: function (a, b) { return !a && !b; } }
  ];
  var SCALE = 0.30;   // stała oś pasków odsetka: 0–30%

  Z.register({
    misje: function (cfg, ui, rnd) {
      var runs = [], fails = [0, 0, 0];
      function add(k) {
        for (var i = 0; i < k; i++) {
          var a = rnd() < cfg.r, b = rnd() < cfg.r;
          runs.push([a, b]);
          LOGICS.forEach(function (L, j) { if (L.fail(a, b)) fails[j]++; });
        }
        if (runs.length > 100) runs = runs.slice(runs.length - 100);
      }
      var total = 0;
      return {
        on: {
          add: function (btn) { var k = Number(btn.getAttribute("data-value")); add(k); total += k; },
          clear: function () { runs = []; fails = [0, 0, 0]; total = 0; }
        },
        render: function () {
          var W = 800, x0 = 170, cw = 4.6, rowH = 46, top = 46, s = "";
          // wiersz stanów elementów w ostatnich misjach
          s += '<text class="s8p-subtle" x="' + x0 + '" y="16" font-size="12">ostatnie misje (najnowsza po prawej)</text>';
          ["E₁", "E₂"].forEach(function (lab, e) {
            s += '<text class="s8p-mono s8p-subtle" x="' + (x0 - 10) + '" y="' + (30 + e * 9) + '" font-size="10" text-anchor="end">' + lab + "</text>";
            runs.forEach(function (r, i) {
              s += '<rect class="' + (r[e] ? "s8p-cell-idle" : "s8p-cell-one") + '" x="' + U.g(x0 + i * cw) + '" y="' + (23 + e * 9) + '" width="' + (cw - 1) + '" height="7"/>';
            });
          });
          LOGICS.forEach(function (L, j) {
            var y = top + j * rowH, share = total ? fails[j] / total : 0;
            s += '<text x="0" y="' + (y + 20) + '" font-size="15" font-weight="600">' + L.name + "</text>";
            runs.forEach(function (r, i) {
              s += '<rect class="' + (L.fail(r[0], r[1]) ? "s8p-cell-bad" : "s8p-cell-ok") + '" x="' + U.g(x0 + i * cw) + '" y="' + (y + 4) + '" width="' + (cw - 1) + '" height="24" rx="1"/>';
            });
            // licznik i pasek odsetka na stałej osi 0–30%
            var bx = 650, bw = 140, ex = bx + bw * cfg.exact[L.key] / SCALE;
            s += '<rect class="s8p-sunken" x="' + bx + '" y="' + (y + 8) + '" width="' + bw + '" height="14" rx="2"/>' +
                 '<rect class="s8p-cell-bad" x="' + bx + '" y="' + (y + 8) + '" width="' + U.g(bw * Math.min(share, SCALE) / SCALE) + '" height="14" rx="2"/>' +
                 '<line class="s8p-dash" x1="' + U.g(ex) + '" x2="' + U.g(ex) + '" y1="' + (y + 3) + '" y2="' + (y + 27) + '"/>';
          });
          ui.set("stage", U.svgWrap(W, top + 3 * rowH, "Ostatnie misje ocenione w trzech logikach sukcesu; zawiedzione misje w kolorze akcentu.", s));
          var read = LOGICS.map(function (L, j) {
            return U.esc(L.name) + ": " + fails[j] + (total ? " (" + U.fmt(100 * fails[j] / total, 1) + "%)" : "");
          }).join(" · ");
          ui.set("body", '<p class="s8p-read">misje: ' + total + " · zawiedzione — " + read + "</p>");
        }
      };
    }
  });
})();
