// Ryciny wykładu 08 „Co czyni dobry zbiór danych?” liczone w przeglądarce.
// Wykresy na stałych zbiorach opisuje R (lc_sf_ggplot() w modules/figures.R);
// tutaj jest to, co zależy od kliknięć studenta.
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S) return;
  var figures = {};

  // Lista kontrolna (rozdz. 1): pasek oceny z liczby zaznaczonych kryteriów.
  // Zasada: każde niespełnione kryterium krytyczne dyskwalifikuje zbiór; naprawialne
  // decydują tylko o tym, czy potrzebne jest czyszczenie.
  function checked(root, name) { return root.querySelectorAll('input[name="' + name + '"]:checked').length; }
  figures.s08_checklist = {
    // Pola wyboru nie są kontrolkami lcSF: zmiana zaznaczenia odświeża pasek.
    init: function (state, root) { root.addEventListener("change", function () { root.dispatchEvent(new CustomEvent("lc:sf-redraw")); }); },
    update: function (root, state) {
      var box = root.querySelector('[data-role="thermo"]');
      if (!box) return;
      var c = state.config.colors, nc = checked(root, "intro_critical"), nf = checked(root, "intro_fixable"), n = nc + nf;
      var color, verdict, label;
      if (n === 0) { color = "var(--upwr-reference)"; verdict = "info"; label = "Zaznacz kryteria, które spełnia oceniany zbiór."; }
      else if (nc < 6) { color = c.bad; verdict = "danger"; label = "Zbiór nie nadaje się do zaplanowanej analizy: poszukaj innego zbioru albo zbierz nowe dane."; }
      else if (nf < 3) { color = c.mixed; verdict = "warning"; label = "Zbiór wymaga czyszczenia i przygotowania przed analizą."; }
      else { color = c.good; verdict = "ok"; label = "Dane gotowe do analizy."; }
      box.innerHTML = '<div style="background: var(--upwr-rule); border-radius: 10px; height: 30px; margin-top: 15px;">' +
        '<div style="background: ' + color + "; height: 30px; border-radius: 10px; width: " + (n / 9 * 100) +
        '%; transition: width 0.3s; text-align: center; line-height: 30px; color: white; font-weight: bold;">' + n + "/9</div></div>" +
        S.status('<span class="lc-status-' + verdict + '">' + S.esc(label) + "</span>") +
        (nc < 6 && nf > 0 ? '<div class="lc-caption"><div>Naprawialne kryteria nie ratują krytycznych problemów.</div></div>' : "");
    }
  };

  // Rozdział 3: histogram ocen dla n obserwacji (te same losowania co w R), szerokość
  // przedziału ufności i moc testu t (krzywa z power.t.test liczona w R).
  var D = root.lcDist;
  function ciWidth(n) { return 2 * D.qt(0.975, n - 1) * 0.6 / Math.sqrt(n); }
  figures.s08_n = {
    draw: function (v, P, k, state, vals) {
      var c = state.config, n = Number(vals.tab2_n), i, pts = [];
      if (k === 0) {
        var oceny = c.z.slice(n, 2 * n).map(function (z) { return 3.8 + 0.6 * z; });
        var bins = S.binsGgplot(oceny, Math.max(5, Math.round(n / 5))), top = 0;
        bins.forEach(function (b) { if (b.n > top) top = b.n; });
        return { x: [bins[0].l - 0.05 * (bins[bins.length - 1].r - bins[0].l), bins[bins.length - 1].r + 0.05 * (bins[bins.length - 1].r - bins[0].l)],
          y: [0, top * 1.05], xName: "Średnia ocen", yName: "Liczebność",
          layers: [{ type: "bins", data: bins.map(function (b) { return [b.l, b.r, b.n]; }), color: C.alpha(P.niebo, 0.8), stroke: P.surface }],
          aria: "Histogram średnich ocen dla wybranej liczby obserwacji." };
      }
      if (k === 1) {
        for (i = 5; i <= 200; i++) pts.push([i, ciWidth(i)]);
        return { x: [-4.75, 209.75], y: [0, ciWidth(5) * 1.05], xName: "Liczba obserwacji (n)", yName: "Szerokość przedziału ufności",
          layers: [{ type: "line", data: pts, color: P.accent, width: 2.6 }, { type: "points", data: [[n, ciWidth(n)]], color: P.accent, size: 12 }],
          aria: "Szerokość przedziału ufności maleje wraz z liczbą obserwacji." };
      }
      // Moc w procentach (oś jak scales::percent).
      for (i = 5; i <= 200; i++) pts.push([i, 100 * c.power[i - 5]]);
      return { x: [-4.75, 209.75], y: [-5, 105], xName: "Liczba obserwacji (n)", yName: "Moc testu",
        yLabels: { 0: "0%", 25: "25%", 50: "50%", 75: "75%", 100: "100%" },
        layers: [{ type: "line", data: [[-10, 80], [220, 80]], color: P.secondary, width: 1.6, dash: [6, 5] },
                 { type: "text", x: 150, y: 83, text: "Moc 80%", color: P.secondary, valign: "bottom" },
                 { type: "line", data: pts, color: P.niebo, width: 2.6 }, { type: "points", data: [[n, 100 * c.power[n - 5]]], color: P.niebo, size: 12 }],
        aria: "Moc testu t rośnie wraz z liczbą obserwacji." };
    }
  };

  // Kafelki odpowiedzi: po wyborze kafelki znikają, a pod nimi jest komentarz z R (config.results).
  figures.s08_tiles = {
    init: function (state) { state.answer = null; },
    act: function (state, action) { if (!state.answer && action.indexOf("ans:") === 0) state.answer = action.slice(4); },
    update: function (root, state) {
      var tiles = root.querySelector('[data-role="tiles"]'), res = root.querySelector('[data-role="result"]');
      if (tiles) tiles.hidden = !!state.answer;
      if (res) res.innerHTML = state.answer ? state.config.results[state.answer] || "" : "";
    }
  };

  // Rozdział 9: pięć klasyfikacji i wynik po kliknięciu „Sprawdź odpowiedzi” (jak dawniej: wynik
  // zostaje z chwili kliknięcia, kolejne kliknięcie liczy go od nowa).
  figures.s08_quiz5 = {
    act: function (state, action, root) {
      if (action !== "check") return;
      var c = state.config, score = 0, items = c.correct.map(function (ok, i) {
        var pick = root.querySelector('input[name="tab8_q' + (i + 1) + '"]:checked'), good = pick && pick.value === ok;
        if (good) score++;
        return "<div style='padding: 6px 0; border-bottom: 1px solid var(--upwr-rule);'><b>Pyt. " + (i + 1) + ":</b> " +
          (good ? "<span class='lc-status-ok'>Dobrze.</span>" : "<span class='lc-status-danger'>Źle.</span>") + " " + S.esc(c.explanations[i]) + "</div>";
      });
      state.html = S.status('<span class="lc-status-' + (score >= 4 ? "ok" : "warning") + '">Wynik: ' + score + "/5</span>" + items.join(""));
    },
    update: function (root, state) {
      var res = root.querySelector('[data-role="result"]');
      if (res) res.innerHTML = state.html || "";
    }
  };

  S.register(figures);
})();
