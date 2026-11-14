// Prototypy punktów stop wykładu 10, grupa rc. Scena w przeglądarce, bez serwera R.
// Parametry przychodzą z R w data-config (i10p_cfg oraz n = i10p5_n w stop_proto_rc.R);
// w tym pliku nie ma liczb modelu, poza stałymi geometrii rysunku.
//
//   detektor — Piaskownica „Co zmienia FPR, a co czułość” (P5B): dwa paski 100%.
//              Pasek 1: P(D | I) = 1 − czułość (do drzewa). Pasek 2: P(I | alarm)
//              dla operatora przy P(I) z konfiguracji. Suwaki FPR i czułości.
(function () {
  "use strict";
  var Z = window.lcI10;
  if (!Z) return;
  var U = Z.util;

  // Geometria paska (px w viewBox 800 × 230): odczyt po prawej mieści się w 800 px.
  var X0 = 20, W = 520, BH = 46, READ_X = 562;
  var NBSP = " ";  // twarda spacja: etykieta i liczba nie rozrywają się w wierszu

  function clamp01(x) { return Math.max(0, Math.min(1, isFinite(x) ? x : 0)); }
  // Liczba zdarzeń bez zbędnych zer: 47.5, 497.5, 2.5, 0.
  function count(x) { return String(Math.round(x * 10) / 10); }
  // Tysiące twardą spacją: 10 000.
  function thou(n) { return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, NBSP); }
  // Napis segmentu mieści się, gdy szerokość segmentu wystarcza na znaki (ok. 8 px na znak).
  function fitsLabel(w, name) { return w > name.length * 8 + 24; }

  // Pasek 100%: tytuł nad paskiem, segmenty od lewej, odczyt po prawej.
  function bar(y, title, segs, read, caption) {
    var s = '<text class="i10p-name" x="' + X0 + '" y="' + U.g(y - 12) + '" font-size="15">' + U.esc(title) + "</text>";
    var x = X0;
    segs.forEach(function (sg) {
      var w = W * clamp01(sg.share);
      if (w > 0) {
        s += '<rect class="' + sg.cls + '" x="' + U.g(x) + '" y="' + U.g(y) + '" width="' + U.g(w) + '" height="' + BH + '"/>';
      }
      if (fitsLabel(w, sg.name)) {
        s += '<text class="' + (sg.inv ? "i10p-inv" : "i10p-ink") + '" x="' + U.g(x + 12) + '" y="' + U.g(y + BH / 2 + 5) +
          '" font-size="14">' + U.esc(sg.name) + "</text>";
      }
      x += w;
    });
    s += '<text class="i10p-mono i10p-ink" x="' + READ_X + '" y="' + U.g(y + BH / 2 + 1) + '" font-size="18" font-weight="700">' +
      U.esc(read) + "</text>" +
      '<text class="i10p-subtle" x="' + READ_X + '" y="' + U.g(y + BH / 2 + 22) + '" font-size="13">' + U.esc(caption) + "</text>";
    return s;
  }

  Z.register({
    detektor: function (cfg, ui) {
      return {
        on: {},
        render: function () {
          var fpr = U.sliderValue(ui.root, "i10p5b_fpr", cfg.fpr);
          var sens = U.sliderValue(ui.root, "i10p5b_sens", cfg.sens);
          var prev = cfg.init, n = cfg.n;
          var post = U.bayes(prev, sens, fpr);
          var miss = 1 - sens;
          var postOk = isFinite(post) ? post : 0;

          var body = bar(52, "Wśród misji z zapotrzebowaniem", [
              { name: "alarm", share: sens, cls: "i10p-fill-ok", inv: false },
              { name: "przeoczenie", share: miss, cls: "i10p-fill-bad", inv: true }
            ], "P(D | I) = " + U.fmtP(miss), "do drzewa") +
            bar(162, "Wśród alarmów", [
              { name: "zapotrzebowanie", share: postOk, cls: "i10p-fill-ok", inv: false },
              { name: "alarm fałszywy", share: 1 - postOk, cls: "i10p-fill-warn", inv: false }
            ], "P(I | alarm) = " + U.fmtP(post), "dla operatora");

          ui.set("stage", U.svgWrap(800, 230,
            "Dwa paski: wśród misji z zapotrzebowaniem przeoczenia " + U.fmtP(miss) +
            "; wśród alarmów zapotrzebowania " + U.fmtP(post) + ".", body));

          // Częstości naturalne na n misji (bez zaokrągleń w modelu, tylko w druku).
          var I = prev * n;
          var pair = function (label, x) { return label + NBSP + count(x); };
          ui.set("read", '<p class="i10p-read">na ' + thou(n) + " misji: " + [pair("alarmy prawdziwe", I * sens),
            pair("alarmy fałszywe", (n - I) * fpr), pair("przeoczenia", I * miss)].join(" · ") + "</p>");
        }
      };
    }
  });
})();
