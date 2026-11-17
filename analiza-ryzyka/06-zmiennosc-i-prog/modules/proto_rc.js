// Prototypy punktów stop wykładu 06, grupa rc (P7 i P8). Ryciny lcSF liczone
// w przeglądarce, sceny losowe w window.lcZ6; kontenery w R: stop_proto_rc.R.
//
//   z6_prog — Piaskownica „Próg temperatury łożyska” (P7): krzywa N(82, 3) stała,
//             suwak progu c, pole ponad c, pas „następnego pół stopnia”.
//   zmiany  — Scena „Dzienniki zmian” (P8): zmiana = 8 niezależnych pomiarów co
//             godzinę; pytanie o pomiar, o zmianę albo oba; paski na wspólnej osi.
// P8b jest statyczny (rysunek z R), tu nie ma jego kodu.
(function () {
  "use strict";
  var S = window.lcSF, Z = window.lcZ6;

  // ---- P7: rycina lcSF ---------------------------------------------------------
  // Stałe osie jak w całym wykładzie: X = [68, 98], Y = [0, 0.16].
  if (S && window.lcZ6) {
    var U7 = Z.util, X7 = [68, 98], YMAX7 = 0.16;

    S.register({
      z6_prog: {
        draw: function (v, P, k, state, vals) {
          var cfg = state.config, c = Number(vals.z6p7_c), mu = cfg.mu, sd = cfg.sd;
          state.c = c;
          return { x: X7, y: [0, YMAX7], xLabelEvery: 5,
            layers: [
              // ogon ponad c: akcent o przezroczystości 0.4
              { type: "line", data: U7.curve(mu, sd, c, X7[1]), color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.4 },
              // następne pół stopnia: pas od c do c + 0.5, mocniej
              { type: "line", data: U7.curve(mu, sd, c, c + 0.5, 0.05), color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.75 },
              // stan bazowy N(82, 3)
              { type: "line", data: U7.curve(mu, sd, X7[0], X7[1]), color: P.secondary, width: 2.6 },
              // cienka kreska średniej
              { type: "vline", x: mu, color: P.subtle, width: 1.4, dash: [3, 3], from: 0, to: U7.dnorm(mu, mu, sd) },
              // próg c
              { type: "vline", x: c, color: P.ink, width: 1.8, dash: [6, 4] },
              { type: "text", x: c, y: "top", text: "próg " + String(c) + "°C", color: P.ink, align: "left", mono: true }
            ],
            legend: [{ text: "rozkład temperatury N(82, 3)", color: P.secondary },
                     { text: "pole ponad progiem", color: P.accent, box: true },
                     { text: "następne +0.5°C", color: P.accent, box: true }],
            xName: "Temperatura łożyska (°C)", yName: "Gęstość",
            aria: "Krzywa normalna o średniej 82 i odchyleniu 3 stopnie, z polem ponad progiem " + String(c) +
                  " stopni i pasem następnego pół stopnia." };
        },
        update: function (root, state) {
          var c = state.c, cfg = state.config;
          var p = 1 - U7.pnorm(c, cfg.mu, cfg.sd);
          var p2 = 1 - U7.pnorm(c + 0.5, cfg.mu, cfg.sd);
          var pp = (p - p2) * 100;
          S.setValue(root, "z", U7.fmt((c - cfg.mu) / cfg.sd, 2));
          S.setValue(root, "p", U7.fmtP(p));
          S.setValue(root, "f", U7.per1000(p));
          S.setValue(root, "d", (pp >= 1 ? U7.fmt(pp, 1) : U7.fmt(pp, 2)) + " pp");
        }
      }
    });
  }

  // ---- P8: scena „Dzienniki zmian” ------------------------------------------------
  if (Z) {
    var U = Z.util;

    Z.register({
      zmiany: function (cfg, ui) {
        var n = cfg.n, mu = cfg.mu, sd = cfg.sd, c = cfg.c;
        var model = false;                 // pokazanie wartości z modelu (przycisk oka)
        var q = "meas";                    // pytanie: o pomiar (kratki), o zmianę (wiersze) albo oba
        var rnd, changes, red, alarm;      // zmiany: tablice flag (true = ponad progiem)

        function restart() {
          rnd = cfg.seed ? U.mulberry32(cfg.seed) : Math.random;
          changes = []; red = 0; alarm = 0;
          add(1);
        }

        // Jedna zmiana: n niezależnych pomiarów N(mu, sd). Pomiary skorelowane pokazuje P8b.
        function oneChange() {
          var flags = [], j, hot, count = 0;
          for (j = 0; j < n; j++) {
            hot = mu + sd * U.rnorm(rnd) > c;
            flags.push(hot);
            if (hot) count++;
          }
          return { flags: flags, count: count };
        }

        function add(k) {
          for (var i = 0; i < k; i++) {
            var ch = oneChange();
            changes.push(ch.flags);
            red += ch.count;
            if (ch.count > 0) alarm++;
          }
        }

        function cell(x, y, cls) {
          return '<rect class="' + cls + '" x="' + U.g(x) + '" y="' + U.g(y - 6.5) + '" width="13" height="13" rx="2"/>';
        }

        // Siatka: 40 ostatnich zmian, 4 kolumny po 10 wierszy; wiersz = numer zmiany.
        function gridSvg() {
          var N = changes.length, start = Math.max(0, N - 40), body = "", k, j, col, row, x0, y, flags, any;
          for (k = 0; k < 40; k++) {
            col = Math.floor(k / 10); row = k % 10;
            x0 = col * 200; y = 36 + row * 21;
            flags = changes[start + k];
            if (!flags) {
              for (j = 0; j < n; j++) body += cell(x0 + 34 + j * 16, y, "z6p-cell-idle");
              continue;
            }
            any = flags.indexOf(true) >= 0;
            // Pytanie o zmianę: cały wiersz z alarmem dostaje tło, kratki bledną.
            if (q === "shift" && any) {
              body += '<rect class="z6p8-band" x="' + U.g(x0 + 28) + '" y="' + U.g(y - 9.5) + '" width="164" height="19" rx="4"/>';
            }
            body += '<text class="z6p-mono z6p-subtle" x="' + U.g(x0) + '" y="' + U.g(y + 4) + '" font-size="12">' + (start + k + 1) + "</text>";
            for (j = 0; j < n; j++) {
              body += cell(x0 + 34 + j * 16, y, (flags[j] ? "z6p-cell-bad" : "z6p-cell-ok") + (q === "shift" ? " z6p8-soft" : ""));
            }
            // Pytanie o pomiar: kółka alarmu zmiany bledną.
            body += '<circle class="' + (any ? "z6p-bad" : "z6p8-empty") + (q === "meas" ? " z6p-cell-dim" : "") +
              '" cx="' + U.g(x0 + 178) + '" cy="' + U.g(y) + '" r="6"/>';
          }
          return U.svgWrap(800, 240, "Ostatnie zmiany: kratki to pomiary, czerwone są ponad 85 stopni; kółko wypełnione oznacza alarm w zmianie.", body);
        }

        // Dwa paski na wspólnej osi 0–100%: pomiary ponad progiem i zmiany z alarmem.
        function barsSvg() {
          var N = changes.length, M = N * n, X0 = 230, W = 400, body = "";
          var rows = [
            { label: "pomiary ponad progiem", p: red / M, m: cfg.p, y: 30, q: "meas" },
            { label: "zmiany z alarmem", p: alarm / N, m: cfg.p_shift, y: 84, q: "shift" }
          ];
          rows.forEach(function (r) {
            // Pasek drugiego pytania blednie, ale zostaje do porównania; przy „oba” oba są pełne.
            body += '<g' + (q === "both" || r.q === q ? "" : ' class="z6p8-off"') + ">";
            body += '<text x="0" y="' + U.g(r.y + 5) + '" font-size="15" font-weight="600">' + r.label + "</text>";
            body += '<rect class="z6p8-track" x="' + X0 + '" y="' + U.g(r.y - 9) + '" width="' + W + '" height="18" rx="3"/>';
            body += '<rect class="z6p8-fill" x="' + X0 + '" y="' + U.g(r.y - 9) + '" width="' + U.g(W * r.p) + '" height="18" rx="3"/>';
            if (model) {
              body += '<line class="z6p-limit" x1="' + U.g(X0 + W * r.m) + '" x2="' + U.g(X0 + W * r.m) +
                '" y1="' + U.g(r.y - 13) + '" y2="' + U.g(r.y + 13) + '"/>';
            }
            body += '<text class="z6p-mono" x="' + (X0 + W + 14) + '" y="' + U.g(r.y + 5) + '" font-size="16" font-weight="700">' +
              U.fmt(100 * r.p, 1) + "%</text>";
            if (model) {
              body += '<text class="z6p-mono z6p-subtle" x="' + (X0 + W + 14) + '" y="' + U.g(r.y + 22) + '" font-size="12">model ' +
                U.fmt(100 * r.m, 1) + "%</text>";
            }
            body += "</g>";
          });
          body += U.axis(function (v) { return X0 + v * W; }, 118, [0, 0.5, 1], null, ["0%", "50%", "100%"]);
          return U.svgWrap(800, 140, "Odsetek pomiarów ponad progiem i odsetek zmian z alarmem, na wspólnej osi od 0 do 100 procent.", body);
        }

        function render() {
          var N = changes.length, M = N * n;
          var Q = {
            meas: { tag: "Pytanie o pomiar", text: "Jak często pojedynczy pomiar przekracza 85°C?",
                    how: "Licz czerwone kratki", k: red, of: M },
            shift: { tag: "Pytanie o zmianę", text: "Jak często w zmianie pojawia się co najmniej jeden alarm?",
                     how: "Licz wiersze z co najmniej jedną czerwoną kratką", k: alarm, of: N }
          };
          function block(h, big) {
            return '<span class="z6p8-q-tag">' + h.tag + "</span>" +
              '<p class="z6p8-q-text' + (big ? "" : " is-small") + '">' + h.text + "</p>" +
              '<p class="z6p-read">' + h.how + ": " + h.k + " z " + h.of +
              " (" + U.fmt(100 * h.k / h.of, 1) + "%)</p>";
          }
          // „Oba”: dwa pytania jedno pod drugim, mniejszą czcionką; siatka i paski bez wyróżnień.
          ui.set("read", '<div class="z6p8-q">' +
            (q === "both" ? block(Q.meas, false) + block(Q.shift, false) : block(Q[q], true)) +
            '<p class="z6p-subtle z6p8-q-n">Zmian w dzienniku: ' + N + "</p></div>");
          ui.set("grid", gridSvg());
          ui.set("bars", barsSvg());
        }

        restart();
        return {
          on: {
            one: function () { add(1); },
            m40: function () { add(40); },
            m1000: function () { add(1000); },
            reset: function () { restart(); },
            // Przycisk oka: przełącza wartości z modelu na paskach.
            model: function (btn) {
              model = !model;
              btn.setAttribute("aria-pressed", model ? "true" : "false");
              btn.setAttribute("aria-label", model ? "Ukryj wartości z modelu" : "Pokaż wartości z modelu");
              btn.setAttribute("title", btn.getAttribute("aria-label"));
            },
            question: function (btn) { q = U.press(btn); }
          },
          render: render
        };
      }
    });
  }
})();
