// Gra „zgadnij średnią i medianę” (Ryc. 3.10). Rundy losowane w przeglądarce według
// tych samych zasad co dawniej w R: 200 obserwacji z rozkładu symetrycznego (normalny),
// prawoskośnego (gamma + przesunięcie) albo lewoskośnego (100 − gamma), zaokrąglone
// do 0.1. Pierwsze kliknięcie to typowana średnia, drugie — mediana; przed odsłonięciem
// typy można przeciągać. Kontener .lc-guess z <canvas data-role="hist">, przyciskami
// [data-g-act="new|reveal"] i polami [data-role="banner|feedback"].
(function () {
  var Charts = window.lcCharts;
  var U = window.lc01;
  if (!U) return;

  function generate() {
    var type = ["symmetric", "right_skew", "left_skew"][U.sampleInt(0, 2)], n = 200, x = [], i;
    if (type === "symmetric") {
      var mu = U.sampleInt(40, 80), sd = U.sampleInt(8, 15);
      for (i = 0; i < n; i++) x.push(U.rnorm(mu, sd));
    } else {
      var k = U.sampleInt(2, 4), sc = U.sampleInt(5, 12), sh = type === "right_skew" ? U.sampleInt(10, 30) : 0;
      for (i = 0; i < n; i++) {
        var g = U.rgammaInt(k, sc);
        x.push(type === "right_skew" ? g + sh : 100 - g);
      }
    }
    return { type: type, values: x.map(function (v) { return Math.round(v * 10) / 10; }) };
  }

  function init(root) {
    var canvas = root.querySelector('canvas[data-role="hist"]');
    if (!canvas) return false;
    var banner = root.querySelector('[data-role="banner"]');
    var feedback = root.querySelector('[data-role="feedback"]');
    var btnReveal = root.querySelector('[data-g-act="reveal"]');
    var round = null, guess = { mean: null, median: null }, revealed = false;
    var score = { total: 0, good: 0 }, LO = 0, HI = 1, drag = null;

    var chart = Charts.create(canvas, {
      type: "scatter",
      data: { datasets: [0, 1, 2, 3].map(function (k) {
        return { data: [], showLine: true, pointRadius: 0, borderWidth: k < 2 ? 2 : 3, borderDash: k < 2 ? [6, 4] : [] };
      }) },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 6, right: 12 } },
        scales: {
          x: { type: "linear", ticks: { includeBounds: false }, title: { display: true, text: "Wartość" } },
          y: { min: 0, ticks: { precision: 0 }, title: { display: true, text: "Liczebność" } }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false },
                   lcHist: { bins: [] }, lcLabels: { items: [] } }
      },
      plugins: [U.histPlugin, U.labelPlugin]
    });

    var P = null;
    function paint() {
      P = Charts.palette();
      Charts.styleAxes(chart, P, { maxRotation: 0 });
      Object.assign(chart.options.plugins.lcHist, { fill: Charts.alpha(P.ref, 0.5), gap: P.surface });
      Object.assign(chart.options.plugins.lcLabels, { font: P.mono, halo: Charts.alpha(P.surface, 0.85) });
      var ds = chart.data.datasets;
      ds[0].borderColor = ds[2].borderColor = P.accent;
      ds[1].borderColor = ds[3].borderColor = P.niebo;
      chart.update();
    }

    function newRound() {
      round = generate();
      var x = round.values, mn = Math.min.apply(null, x), mx = Math.max.apply(null, x), w = (mx - mn) / 25;
      var b = U.bins(x, mn, mx + 1e-9, w), top = 0;
      b.forEach(function (bb) { top = Math.max(top, bb.n); });
      LO = mn - 0.05 * (mx - mn); HI = mx + 0.05 * (mx - mn);
      chart.options.scales.x.min = LO; chart.options.scales.x.max = HI;
      chart.options.scales.y.max = Math.ceil(top * 1.45 / 5) * 5;
      chart.options.plugins.lcHist.bins = b;
      guess = { mean: null, median: null }; revealed = false;
      draw();
    }

    function vline(v) { var t = chart.options.scales.y.max; return v == null ? [] : [{ x: v, y: 0 }, { x: v, y: t }]; }

    function draw() {
      var x = round.values, m = U.mean(x), med = U.median(x), ds = chart.data.datasets, labels = [];
      ds[0].data = vline(guess.mean); ds[1].data = vline(guess.median);
      ds[2].data = revealed ? vline(m) : []; ds[3].data = revealed ? vline(med) : [];
      if (guess.mean != null) labels.push({ x: guess.mean, text: "Wskazana średnia", color: P.accent, row: 0 });
      if (guess.median != null) labels.push({ x: guess.median, text: "Wskazana mediana", color: P.niebo, row: 1 });
      if (revealed) {
        labels.push({ x: m, text: "Średnia " + Charts.fmt(m, 1), color: P.accent, row: 2 });
        labels.push({ x: med, text: "Mediana " + Charts.fmt(med, 1), color: P.niebo, row: 3 });
      }
      chart.options.plugins.lcLabels.items = labels;
      chart.update("none");

      var tone = "info", msg;
      if (guess.mean == null) msg = "Kliknij na wykresie w miejscu, gdzie powinna leżeć średnia.";
      else if (guess.median == null) msg = "Teraz kliknij, gdzie leży mediana.";
      else if (!revealed) { msg = "Gotowe. Kliknij „Pokaż odpowiedź”. Linie można jeszcze przeciągnąć."; tone = "ok"; }
      else msg = "Trafione rundy: " + score.good + " z " + score.total + ".";
      if (banner) {
        if (revealed) banner.removeAttribute("data-tone"); else banner.dataset.tone = tone;
        banner.firstElementChild.textContent = msg;
      }
      if (btnReveal) btnReveal.disabled = revealed || guess.mean == null || guess.median == null;

      if (feedback) {
        feedback.hidden = !revealed;
        if (revealed) {
          var rng = Math.max.apply(null, x) - Math.min.apply(null, x);
          var overall = (Math.abs(guess.mean - m) + Math.abs(guess.median - med)) / rng;
          var grade = overall < 0.08 ? ["Doskonale!", "info"] : overall < 0.15 ? ["Nieźle!", "warning"] : ["Można lepiej!", "danger"];
          feedback.innerHTML = "";
          var v = document.createElement("span");
          v.className = "lc-status-" + grade[1];
          var st = document.createElement("strong"); st.textContent = grade[0] + " ";
          v.appendChild(st);
          feedback.appendChild(v);
          feedback.appendChild(document.createTextNode("Błąd średniej: " + Charts.fmt(Math.abs(guess.mean - m), 1) +
            ", błąd mediany: " + Charts.fmt(Math.abs(guess.median - med), 1) + "."));
        }
      }
      root.lcState = { type: round.type, values: x, mean: m, median: med, guess: guess, revealed: revealed, score: score };
    }

    function reveal() {
      if (revealed || guess.mean == null || guess.median == null) return;
      var x = round.values, m = U.mean(x), med = U.median(x);
      var rng = Math.max.apply(null, x) - Math.min.apply(null, x);
      revealed = true;
      score.total++;
      if (Math.abs(guess.mean - m) / rng < 0.08 && Math.abs(guess.median - med) / rng < 0.08) score.good++;
      draw();
    }

    function at(e) { return Math.round(U.valueAtX(chart, e) * 10) / 10; }
    function px(v) { return chart.scales.x.getPixelForValue(v); }
    canvas.style.touchAction = "none";
    canvas.style.cursor = "crosshair";
    Charts.listen(canvas, "pointerdown", function (e) {
      if (revealed) return;
      var x = U.pixelX(chart, e), v = at(e), near = null, best = 12;
      ["mean", "median"].forEach(function (k) {
        if (guess[k] != null && Math.abs(px(guess[k]) - x) < best) { best = Math.abs(px(guess[k]) - x); near = k; }
      });
      if (near) drag = near;
      else if (guess.mean == null) { guess.mean = v; drag = "mean"; }
      else if (guess.median == null) { guess.median = v; drag = "median"; }
      if (!drag) return;
      canvas.setPointerCapture(e.pointerId);
      draw();
      e.preventDefault();
    });
    Charts.listen(canvas, "pointermove", function (e) {
      if (drag) { guess[drag] = at(e); draw(); return; }
      if (revealed) { canvas.style.cursor = "default"; return; }
      var x = U.pixelX(chart, e), near = ["mean", "median"].some(function (k) {
        return guess[k] != null && Math.abs(px(guess[k]) - x) < 12;
      });
      canvas.style.cursor = near ? "ew-resize" : (guess.median == null ? "crosshair" : "default");
    });
    ["pointerup", "pointercancel"].forEach(function (t) {
      Charts.listen(canvas, t, function () { drag = null; });
    });

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-g-act]");
      if (!b || !root.contains(b)) return;
      if (b.dataset.gAct === "new") newRound();
      if (b.dataset.gAct === "reveal") reveal();
    });

    paint();
    newRound();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-guess", init);
})();
