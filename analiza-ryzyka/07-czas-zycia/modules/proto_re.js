// Prototypy punktów stop wykładu 07, grupa re: sceny ruchome na parku wentylatorów
// (window.lcC7; kontenery w R: stop_proto_re.R). Park to n linii życia o długościach
// równych kwantylom modelu (U.park), ułożonych od najkrótszej (góra) do najdłuższej.
//
//   okno  — Piaskownica „Te same awarie, dwa mianowniki” (P7): okno Δt przesuwane
//           po parku; cztery funkcje z parku: F(t) i R(t) jako podział
//           linii, f(t) i h(t) jako awarie w oknie dzielone przez wszystkie nowe
//           albo przez działające w chwili t, w przeliczeniu na godzinę.
//   parki — Piaskownica „Trzy parki, jedna średnia” (P17B): trzy parki o tym samym
//           MTTF i kreska wymaganego czasu pracy z suwaka; liczba działających w każdym parku.
(function () {
  "use strict";
  var Z = window.lcC7;
  if (!Z) return;
  var U = Z.util;

  // Linie parku jako ścieżki pogrupowane według klasy: classOf(i, t) → nazwa klasy.
  function parkPaths(times, xOf, top, dy, tmax, classOf, width) {
    var groups = {}, i, cls, y;
    for (i = 0; i < times.length; i++) {
      cls = classOf(i, times[i]);
      y = top + (i + 0.5) * dy;
      (groups[cls] = groups[cls] || []).push("M" + U.fmt(xOf(0), 1) + "," + U.fmt(y, 1) + "H" + U.fmt(xOf(Math.min(times[i], tmax)), 1));
    }
    return Object.keys(groups).map(function (c) {
      return '<path class="c7p-life ' + c + '" style="stroke-width:' + width + '" d="' + groups[c].join("") + '"/>';
    }).join("");
  }
  function vmark(x, y0, y1, label) {
    return '<line class="c7p-limit" x1="' + U.fmt(x, 1) + '" x2="' + U.fmt(x, 1) + '" y1="' + y0 + '" y2="' + y1 + '"/>' +
      (label ? '<text class="c7p-mono" x="' + U.fmt(x + 5, 1) + '" y="' + (y0 + 12) + '" font-size="13">' + U.esc(label) + "</text>" : "");
  }
  function sliderOut(ui, id, text) {
    var out = ui.root.querySelector('output[data-for="' + id + '"]');
    if (out) out.textContent = text;
  }

  Z.register({
    // ---- P7 ---------------------------------------------------------------------
    okno: function (cfg, ui) {
      var model = "a", parks = {};
      Object.keys(cfg.models).forEach(function (k) { parks[k] = U.park(cfg.n, cfg.models[k].beta, cfg.models[k].eta); });
      var W = 860, x0 = 40, w = 730, top = 30, dy = 1.6, base = top + cfg.n * dy + 6;
      // Klamra przy prawej krawędzi parku: wiersze od r0 do r1 opisane symbolem funkcji.
      function brace(r0, r1, cls, sym) {
        if (r1 <= r0) return "";
        var x = x0 + w + 14, ya = top + r0 * dy + 1, yb = top + r1 * dy - 1;
        return '<path class="c7p7-brace ' + cls + '" d="M' + (x - 5) + "," + U.fmt(ya, 1) + "H" + x + "V" + U.fmt(yb, 1) + "H" + (x - 5) + '"/>' +
          '<text class="c7p7-sym' + (cls ? " c7p-ink" : " c7p-subtle") + '" x="' + (x + 8) + '" y="' + U.fmt((ya + yb) / 2 + 5, 1) + '">' + sym + "</text>";
      }
      function xOf(t) { return x0 + Math.min(t, cfg.tmax) / cfg.tmax * w; }
      return {
        on: { model: function (btn) { model = U.press(btn); } },
        render: function () {
          var t = U.sliderValue(ui.root, "c7p7_t", 1000), t1 = t + cfg.dt, times = parks[model], m = cfg.models[model];
          var alive = 0, fail = 0;
          times.forEach(function (x) { if (x > t) alive++; if (x > t && x <= t1) fail++; });
          var body = '<rect class="c7p-band" x="' + U.fmt(xOf(t), 1) + '" y="' + (top - 14) + '" width="' + U.fmt(xOf(t1) - xOf(t), 1) + '" height="' + U.fmt(base - top + 14, 1) + '"/>' +
            parkPaths(times, xOf, top, dy, cfg.tmax, function (i, x) {
              return x <= t ? "c7p-life-" + model + " c7p-life-dim" : x <= t1 ? "c7p7-hit" : "c7p-life-" + model;
            }, 1.15) +
            U.axis(xOf, base, [0, 1000, 2000, 3000, 4000], "czas pracy (h)") +
            vmark(xOf(t), top - 14, base, null) +
            '<line class="c7p-limit" style="stroke-dasharray:2 3" x1="' + U.fmt(xOf(t1), 1) + '" x2="' + U.fmt(xOf(t1), 1) + '" y1="' + (top - 14) + '" y2="' + base + '"/>' +
            '<text class="c7p-mono c7p7-halo" x="' + U.fmt(xOf(t) - 6, 1) + '" y="' + (top - 2) + '" font-size="13" text-anchor="end">t = ' + t + " h</text>" +
            '<text class="c7p7-sym c7p-bad c7p7-halo" x="' + U.fmt((xOf(t) + xOf(t1)) / 2, 1) + '" y="' + (top - 2) + '" text-anchor="middle">Δt</text>' +
            brace(0, cfg.n - alive, "", "F(t)") + brace(cfg.n - alive, cfg.n, "is-alive", "R(t)");
          ui.set("stage", U.svgWrap(W, base + 40, "Park " + cfg.n + " wentylatorów z oknem od " + t + " do " + t1 +
            " godzin: w oknie zawodzi " + fail + ", na początku okna działa " + alive + ".", body));
          sliderOut(ui, "c7p7_t", String(t));
          ui.set("before", (cfg.n - alive) + " z " + cfg.n + " = " + U.fmt(1 - alive / cfg.n, 3));
          ui.set("surv", alive + " z " + cfg.n + " = " + U.fmt(alive / cfg.n, 3));
          // Gęstość i hazard z parku: te same awarie w oknie, dwa mianowniki, na godzinę.
          ui.set("all", fail + " z " + cfg.n + " na " + cfg.dt + " h ≈ " + (fail / cfg.n / cfg.dt).toFixed(5) + " /h");
          ui.set("alive", alive ? fail + " z " + alive + " na " + cfg.dt + " h ≈ " + (fail / alive / cfg.dt).toFixed(5) + " /h" : "nikt nie działa");
        }
      };
    },

    // ---- P17B -------------------------------------------------------------------
    parki: function (cfg, ui) {
      var parks = cfg.models.map(function (m) { return U.park(cfg.n, m.beta, m.eta); });
      var W = 860, pw = 250, gap = 35, top = 62, dy = 2.6, base = top + cfg.n * dy + 6;
      return {
        on: {},
        render: function () {
          var t = U.sliderValue(ui.root, "c7p17b_t", 1000), body = "", counts = [];
          cfg.models.forEach(function (m, j) {
            var x0 = 20 + j * (pw + gap);
            function xOf(x) { return x0 + Math.min(x, cfg.tmax) / cfg.tmax * pw; }
            var alive = parks[j].filter(function (x) { return x > t; }).length;
            counts.push(alive);
            body += '<rect class="c7p-fill-' + m.cls + '" x="' + x0 + '" y="6" width="13" height="13" rx="2"/>' +
              '<text x="' + (x0 + 20) + '" y="17" font-size="14" font-weight="700">' + U.esc(m.name) + "</text>" +
              parkPaths(parks[j], xOf, top, dy, cfg.tmax, function (i, x) {
                return "c7p-life-" + m.cls + (x > t ? "" : " c7p-life-dim");
              }, 1.7) +
              U.axis(xOf, base, [0, 1000, 2000, 3000], j === 2 ? "czas pracy (h)" : null) +
              vmark(xOf(t), top - 8, base, null) +
              '<text class="c7p-mono c7p-ink" x="' + (x0 + pw) + '" y="50" font-size="30" font-weight="700" text-anchor="end">' + alive + "</text>" +
              '<text class="c7p-mono c7p-subtle" x="' + (x0 + pw - 44) + '" y="50" font-size="12" text-anchor="end">działa ze ' + cfg.n + "</text>";
          });
          ui.set("stage", U.svgWrap(W, base + 40, "Trzy parki po " + cfg.n + " wentylatorów o tym samym średnim czasie życia; po " + t +
            " godzinach działa " + counts.join(", ") + ".", body));
          sliderOut(ui, "c7p17b_t", String(t));
          var best = Math.max.apply(null, counts);
          ui.set("read", "po " + t + " h działa: " + cfg.models.map(function (m, j) { return m.short + " " + counts[j]; }).join("; ") +
            "; najwięcej: " + cfg.models.filter(function (m, j) { return counts[j] === best; }).map(function (m) { return m.short; }).join(" i "));
        }
      };
    }
  });
})();
