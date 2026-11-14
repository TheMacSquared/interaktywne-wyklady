// Prototypy punktów stop wykładu 06 „Zmienność i próg”, grupa re (rozdział 5,
// „Obciążenie–wytrzymałość”). Kontener w R: stop_proto_re.R.
//
//   dzwig — Scena „Zawiesie dźwigu” (P11): każde podniesienie losuje obciążenie
//           L ~ N(85, 8) i wytrzymałość S ~ N(95, 7) przez Math.random (U.rnorm).
//           Kadry: pasy ostatniego podniesienia, płaszczyzna L–S z punktami,
//           powtarzanie (+10, +100, +1000), linie średnich i odczyt z modelu.
(function () {
  "use strict";
  var Z = window.lcZ6;
  if (!Z) return;
  var U = Z.util;

  var W = 800, H = 400;
  var MAXDOTS = 1500;                     // najwyżej tyle ostatnich punktów na rysunku
  var TICKS = [60, 70, 80, 90, 100, 110, 120];
  var PL = { x0: 420, x1: 720, y0: 40, y1: 340 };   // płaszczyzna L–S, kwadrat
  var BAR = { x0: 20, x1: 250, lo: 50, hi: 130 };   // pasy: wspólna skala kN
  var BARTICKS = [50, 70, 90, 110, 130];

  function pxX(v) { return PL.x0 + (v - 55) / 70 * (PL.x1 - PL.x0); }
  function pxY(v) { return PL.y1 - (v - 55) / 70 * (PL.y1 - PL.y0); }
  // x1: prawy kraniec skali pasów; w kadrze 1 pasy zajmują całą szerokość rysunku.
  function bxTo(x1) { return function (v) { return BAR.x0 + (v - BAR.lo) / (BAR.hi - BAR.lo) * (x1 - BAR.x0); }; }

  Z.register({
    dzwig: function (cfg, ui, rnd) {
      var f = 0, n = cfg.frames.length;
      var count = 0, fails = 0, last = null, recent = [];

      function add(l, s) {
        count++;
        if (l > s) fails++;                  // awaria: obciążenie przekracza wytrzymałość
        last = { l: l, s: s };
        recent.push(last);
        if (recent.length > MAXDOTS) recent.shift();
      }
      function lift(times) {
        for (var i = 0; i < times; i++) {
          add(cfg.mu_l + cfg.sd_l * U.rnorm(rnd), cfg.mu_s + cfg.sd_s * U.rnorm(rnd));
        }
      }
      function reset() {
        count = 0; fails = 0; last = null; recent = [];
        add(cfg.first.l, cfg.first.s);
      }
      reset();

      // Pasy ostatniego podniesienia i werdykt (lewa część rysunku).
      function leftPart(wide) {
        var l = last.l, s = last.s, bad = l > s, bx = bxTo(wide ? 760 : BAR.x1);
        function bar(v, y, color) {
          return '<rect x="' + U.g(BAR.x0) + '" y="' + y + '" width="' + U.fmt(bx(v) - BAR.x0, 1) +
            '" height="26" rx="3" style="fill: ' + color + '"/>';
        }
        return '<text x="0" y="24" font-size="15" font-weight="700">Ostatnie podniesienie</text>' +
          '<text x="0" y="72" font-size="14">ładunek L = ' + U.fmt(l, 1) + ' kN</text>' +
          bar(l, 80, "var(--upwr-cat-bursztyn)") +
          '<text x="0" y="150" font-size="14">zawiesie S = ' + U.fmt(s, 1) + ' kN</text>' +
          bar(s, 158, "var(--upwr-cat-niebo)") +
          (bad
            ? '<text class="z6p-bad" x="0" y="232" font-size="17" font-weight="700">awaria: L &gt; S</text>'
            : '<text class="z6p-ok" x="0" y="232" font-size="17" font-weight="700">zapas ' + U.fmt(s - l, 1) + ' kN</text>') +
          U.axis(bx, 270, BARTICKS);
      }

      // Płaszczyzna L–S: osie, przekątna L = S, punkty, ostatni punkt z obwódką.
      function plane(withMeans) {
        var out = U.axis(pxX, PL.y1, TICKS, "obciążenie L (kN)") +
          U.axisY(pxY, PL.x0, TICKS) +
          '<text class="z6p-subtle" x="' + U.g(PL.x0) + '" y="24" font-size="12">wytrzymałość S (kN)</text>' +
          '<line class="z6p-limit" x1="' + U.fmt(pxX(55), 1) + '" y1="' + U.fmt(pxY(55), 1) +
          '" x2="' + U.fmt(pxX(125), 1) + '" y2="' + U.fmt(pxY(125), 1) + '"/>' +
          '<text class="z6p-mono" x="' + U.fmt(pxX(112) + 14, 1) + '" y="' + U.fmt(pxY(112) + 14, 1) +
          '" font-size="13">L = S</text>';
        if (withMeans) {
          out += '<line class="z6p-mean" x1="' + U.fmt(pxX(85), 1) + '" x2="' + U.fmt(pxX(85), 1) +
            '" y1="' + U.fmt(pxY(55), 1) + '" y2="' + U.fmt(pxY(125), 1) + '"/>' +
            '<line class="z6p-mean" x1="' + U.fmt(pxX(55), 1) + '" x2="' + U.fmt(pxX(125), 1) +
            '" y1="' + U.fmt(pxY(95), 1) + '" y2="' + U.fmt(pxY(95), 1) + '"/>';
        }
        var dots = "";
        for (var i = 0; i < recent.length; i++) {
          var p = recent[i];
          if (p.l < 52 || p.l > 128 || p.s < 52 || p.s > 128) continue;
          dots += '<circle class="' + (p.l > p.s ? "z6p-dot-bad" : "z6p-dot") + '" cx="' +
            U.fmt(pxX(p.l), 1) + '" cy="' + U.fmt(pxY(p.s), 1) + '" r="3.2"/>';
        }
        out += dots;
        if (last) {
          out += '<circle cx="' + U.fmt(pxX(last.l), 1) + '" cy="' + U.fmt(pxY(last.s), 1) +
            '" r="8" fill="none" style="stroke: var(--upwr-ink)" stroke-width="1.8"/>';
        }
        return out;
      }

      function stageBody() {
        var body = leftPart(f === 0);
        if (f >= 1) body += '<line class="z6p-grid" x1="290" x2="290" y1="10" y2="330"/>' + plane(f >= 3);
        return body;
      }

      function readText() {
        var t = "Podniesień: " + count + " · awarii: " + fails + " (" + U.fmt(100 * fails / count, 1) + "%)";
        if (f === 3) t += " · model: " + U.fmt(100 * cfg.p_model, 1) + "%";
        return t;
      }

      function aria() {
        return "Ostatnie podniesienie: ładunek " + U.fmt(last.l, 1) + " kN, zawiesie " + U.fmt(last.s, 1) +
          " kN. Podniesień " + count + ", awarii " + fails + ".";
      }

      return {
        on: {
          prev: function () { f = Math.max(0, f - 1); },
          next: function () { f = Math.min(n - 1, f + 1); },
          one: function () { lift(1); },
          m10: function () { lift(10); },
          m100: function () { lift(100); },
          m1000: function () { lift(1000); },
          reset: function () { reset(); }
        },
        render: function () {
          var fr = cfg.frames[f];
          ui.set("head", U.head(fr.name, "Kadr " + (f + 1) + " z " + n));
          ui.set("stage", U.svgWrap(W, f === 0 ? 310 : H, aria(), stageBody()));
          ui.set("read", '<p class="z6p-read">' + readText() + "</p>");
          ui.set("body", '<p class="z6p-caption">' + U.esc(fr.text) + "</p>");
          ui.ends(f, n);
          ["m10", "m100", "m1000"].forEach(function (a) {
            var b = ui.root.querySelector('[data-z6p="' + a + '"]');
            if (b) b.disabled = f < 2;
          });
        }
      };
    }
  });
})();
