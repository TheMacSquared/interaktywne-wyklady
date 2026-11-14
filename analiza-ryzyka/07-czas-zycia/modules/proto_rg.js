// Prototypy punktów stop wykładu 07, grupa rg: scena losowa (window.lcC7; kontener
// w R: stop_proto_rg.R).
//
//   magazyn — Scena „Magazyn z trzema zapasowymi” (P13): awarie w hali przychodzą
//             losowo, średnio co 500 h; okres kończy się trzecią awarią (zapas
//             pusty). Okres po okresie: oś czasu z kreskami awarii, licznik okresów,
//             w których zapas wystarczył na 1000 h, i histogram czasu do trzeciej
//             awarii. Model (Erlang k = 3) jest ukryty do ostatniego kroku.
(function () {
  "use strict";
  var Z = window.lcC7;
  if (!Z) return;
  var U = Z.util;

  Z.register({
    magazyn: function (cfg, ui) {
      var rnd, periods, ok, total, bins, model = "off";
      var W = 860, x0 = 56, w = 700, rows = 10, top = 34, dy = 22, base = top + rows * dy + 8;
      var hTop = base + 70, hH = 110, hBase = hTop + hH, bw = cfg.bin, nb = Math.round(cfg.tmax / bw), yMax = 0.2;
      function xOf(t) { return x0 + Math.min(t, cfg.tmax) / cfg.tmax * w; }
      function yOf(p) { return hBase - Math.min(p, yMax) / yMax * hH; }

      function restart() {
        rnd = U.mulberry32(cfg.seed);
        periods = []; ok = 0; total = 0; bins = [];
        for (var i = 0; i < nb; i++) bins.push(0);
        add(1);
      }
      // Jeden okres: k odstępów wykładniczych o średniej cfg.gap; zapis chwil awarii.
      function add(n) {
        for (var i = 0; i < n; i++) {
          var t = 0, ev = [];
          for (var j = 0; j < cfg.k; j++) { t += -cfg.gap * Math.log(1 - rnd()); ev.push(t); }
          periods.push(ev);
          total += t;
          if (t > cfg.limit) ok++;
          if (t < cfg.tmax) bins[Math.floor(t / bw)]++;
        }
      }
      // Gęstość Erlanga k = 3: λ³ t² e^(−λt) / 2.
      function erlang(t) { var l = 1 / cfg.gap; return l * l * l * t * t * Math.exp(-l * t) / 2; }

      function stage() {
        var N = periods.length, start = Math.max(0, N - rows), body = "", r, ev, y, j, last, good;
        for (r = 0; r < rows; r++) {
          ev = periods[start + r]; y = top + (r + 0.5) * dy;
          if (!ev) { body += '<line class="c7p-grid" x1="' + x0 + '" x2="' + (x0 + w) + '" y1="' + y + '" y2="' + y + '"/>'; continue; }
          last = ev[cfg.k - 1]; good = last > cfg.limit;
          body += '<text class="c7p-tick" style="text-anchor:end" x="' + (x0 - 10) + '" y="' + (y + 4) + '">' + (start + r + 1) + "</text>" +
            '<line class="c7p13-run" x1="' + x0 + '" x2="' + U.fmt(xOf(last), 1) + '" y1="' + y + '" y2="' + y + '"/>';
          for (j = 0; j < cfg.k; j++) {
            if (ev[j] > cfg.tmax) continue;
            body += '<line class="c7p-fail' + (j === cfg.k - 1 ? " c7p13-last" : "") + '" x1="' + U.fmt(xOf(ev[j]), 1) + '" x2="' + U.fmt(xOf(ev[j]), 1) +
              '" y1="' + (y - (j === cfg.k - 1 ? 9 : 6)) + '" y2="' + (y + (j === cfg.k - 1 ? 9 : 6)) + '"/>';
          }
          body += '<circle class="' + (good ? "c7p-ok" : "c7p-bad") + '" cx="' + (x0 + w + 22) + '" cy="' + y + '" r="6"/>' +
            '<text class="c7p-mono c7p-subtle" x="' + (x0 + w + 36) + '" y="' + (y + 4) + '" font-size="12">' + U.fmt(last, 0) + " h</text>";
        }
        body += U.axis(xOf, base, [0, 1000, 2000, 3000, 4000], "czas od uzupełnienia magazynu (h)") +
          '<line class="c7p-limit" x1="' + U.fmt(xOf(cfg.limit), 1) + '" x2="' + U.fmt(xOf(cfg.limit), 1) + '" y1="' + (top - 16) + '" y2="' + base + '"/>' +
          '<line class="c7p-limit" x1="' + U.fmt(xOf(cfg.limit), 1) + '" x2="' + U.fmt(xOf(cfg.limit), 1) + '" y1="' + hTop + '" y2="' + hBase + '"/>' +
          '<text class="c7p-mono" x="' + U.fmt(xOf(cfg.limit) + 5, 1) + '" y="' + (top - 4) + '" font-size="13">' + cfg.limit + " h: dostawa</text>";
        // Histogram czasu do trzeciej awarii (udział okresów w przedziale).
        body += '<text x="' + x0 + '" y="' + (hTop - 14) + '" font-size="13" font-weight="700">Kiedy przyszła trzecia awaria (wszystkie okresy)</text>';
        for (j = 0; j < nb; j++) {
          if (!bins[j]) continue;
          var p = bins[j] / N, cls = (j + 1) * bw <= cfg.limit ? "c7p13-bar is-bad" : "c7p13-bar";
          body += '<rect class="' + cls + '" x="' + U.fmt(xOf(j * bw) + 0.5, 1) + '" y="' + U.fmt(yOf(p), 1) + '" width="' + U.fmt(xOf((j + 1) * bw) - xOf(j * bw) - 1, 1) +
            '" height="' + U.fmt(hBase - yOf(p), 1) + '"/>';
        }
        if (model === "on") {
          var pts = [];
          for (j = 0; j <= 160; j++) { var t = cfg.tmax * j / 160; pts.push(U.fmt(xOf(t), 1) + "," + U.fmt(yOf(erlang(t) * bw), 1)); }
          body += '<path class="c7p13-model" d="M' + pts.join(" L") + '"/>' +
            '<line class="c7p-mean" x1="' + U.fmt(xOf(cfg.k * cfg.gap), 1) + '" x2="' + U.fmt(xOf(cfg.k * cfg.gap), 1) + '" y1="' + hTop + '" y2="' + hBase + '"/>' +
            '<text class="c7p-mono c7p-subtle" x="' + U.fmt(xOf(cfg.k * cfg.gap) + 5, 1) + '" y="' + (hTop + 10) + '" font-size="12">średnia ' + cfg.k * cfg.gap + " h</text>";
        }
        body += U.axis(xOf, hBase, [0, 1000, 2000, 3000, 4000], "czas do trzeciej awarii (h)");
        return U.svgWrap(W, hBase + 40, "Oś czasu ostatnich okresów z kreskami awarii i histogram czasu do trzeciej awarii z " + N +
          " okresów; zapas wystarczył na " + cfg.limit + " godzin w " + ok + " okresach.", body);
      }

      restart();
      return {
        on: {
          one: function () { add(1); }, m10: function () { add(10); }, m100: function () { add(100); }, m1000: function () { add(1000); },
          reset: restart,
          model: function (btn) { model = U.press(btn); }
        },
        render: function () {
          var N = periods.length;
          ui.set("stage", stage());
          ui.set("n", String(N));
          ui.set("ok", ok + " z " + N + " = " + U.fmt(ok / N, 3));
          ui.set("mean", U.fmtH(total / N));
          ui.set("model", model === "on" ? "Model (Erlang, k = 3): zapas wystarczy z prawdopodobieństwem " + U.fmtP(cfg.p) +
            ", średni czas do trzeciej awarii " + cfg.k * cfg.gap + " h." : "");
        }
      };
    }
  });
})();
