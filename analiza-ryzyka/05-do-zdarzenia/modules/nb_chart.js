// Rozkład ujemny dwumianowy na tle geometrycznego (P7). Kontener:
// .lc-nb[data-r][data-p] z <canvas data-role="pmf">, przyciskami [data-nb-p]
// i [data-nb-reset] oraz odczytami [data-role="r|mean|median|peak"].
// Kropka na średniej r/p: przeciąganie w bok albo strzałki zmieniają r (1–10).
// Oś X zależy tylko od p (99,5. percentyl dla r = 10), oś Y jest stała: szczyt
// rozkładu geometrycznego. Wzorzec jak normals_chart.js w statystyce.
(function () {
  var Charts = window.lcCharts;
  var R_MIN = 1, R_MAX = 10;

  // P(X = x) dla łącznej liczby prób; kolejne wyrazy z ilorazu x/(x − r + 1)·(1 − p).
  function pmf(r, p, xmax) {
    var out = [], P = Math.pow(p, r);
    for (var x = r; x <= xmax; x++) { out.push({ x: x, y: P }); P *= x / (x - r + 1) * (1 - p); }
    return out;
  }
  function quantile(r, p, a) {
    var P = Math.pow(p, r), F = 0;
    for (var x = r; ; x++) { F += P; if (F >= a) return x; P *= x / (x - r + 1) * (1 - p); }
  }

  function init(root) {
    var canvas = root.querySelector('canvas[data-role="pmf"]');
    if (!canvas) return false;
    var START = { r: parseInt(root.dataset.r || "3", 10), p: parseFloat(root.dataset.p || "0.10") };
    var S = { r: START.r, p: START.p };
    var read = {};
    ["r", "mean", "median", "peak"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });
    var cur = [];

    var layer = Charts.handleLayer(root);
    var handle = layer.add(canvas, {
      label: "Średnia rozkładu",
      hint: "Strzałki w prawo i w lewo zmieniają r, czyli liczbę wad, na które czekamy.",
      onKey: function (event) {
        var v = Charts.keyValue(event, S.r, R_MIN, R_MAX, 1, 3);
        if (v === null) return false;
        S.r = v; draw(); layer.announce(handle.element.getAttribute("aria-valuetext")); return true;
      },
      onPointerDown: function (event) { drag = true; canvas.setPointerCapture(event.pointerId); event.preventDefault(); }
    });

    function meanPoint() {
      var m = S.r / S.p, x = Math.max(S.r, Math.round(m));
      var y = cur.length ? cur[Math.min(cur.length - 1, x - S.r)].y : 0;
      return { x: m, y: y };
    }
    function syncHandle(ch) {
      var m = meanPoint();
      handle.position(ch.scales.x.getPixelForValue(m.x), ch.scales.y.getPixelForValue(m.y));
      handle.value(S.r, R_MIN, R_MAX, "r = " + S.r + ", średnia " + Charts.fmt(m.x, 1) + " kontroli");
    }

    var chart = Charts.create(canvas, {
      type: "line",
      plugins: [{ id: "nbHandle", afterRender: syncHandle }],
      data: { datasets: [
        { label: "r = 1 (geometryczny)", data: [], fill: false, borderWidth: 1.6, borderDash: [5, 4] },
        { label: "r", data: [], fill: "origin", borderWidth: 2.5 },
        { label: "średnia r/p", data: [], fill: false, borderWidth: 1.4, borderDash: [4, 3] },
        { label: "uchwyt", data: [], pointRadius: 8, pointHoverRadius: 8, borderWidth: 2, showLine: false }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 10, right: 8 } },
        scales: {
          x: { type: "linear", min: 0, title: { display: true, text: "Łączna liczba kontroli do r-tej wady" } },
          y: { min: 0, title: { display: true, text: "Prawdopodobieństwo" } }
        },
        plugins: {
          legend: { display: true, position: "top", align: "end",
            labels: { boxWidth: 28, boxHeight: 10, filter: function (item) { return item.datasetIndex < 3; } } },
          tooltip: { enabled: false }, colors: { enabled: false }
        },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });

    function paint() {
      var P = Charts.palette(), fs = Charts.axisFontSize(root);
      Charts.styleAxes(chart, P, { fontSize: fs, maxRotation: 0, includeBounds: false });
      chart.config.options.scales.x.ticks.maxTicksLimit = 8;
      chart.config.options.scales.y.ticks.maxTicksLimit = 5;
      chart.config.options.plugins.legend.labels.color = P.soft;
      chart.config.options.plugins.legend.labels.font = { family: P.mono, size: fs };
      var ds = chart.data.datasets;
      ds[0].borderColor = P.subtle;
      ds[1].borderColor = P.accent; ds[1].backgroundColor = Charts.alpha(P.accent, 0.25);
      ds[2].borderColor = P.ink;
      ds[3].pointBackgroundColor = ds[3].pointHoverBackgroundColor = P.accent;
      ds[3].pointBorderColor = ds[3].pointHoverBorderColor = P.surface;
      // Pełne update(): tryb "none" nie przelicza kolorów punktów po zmianie motywu.
      chart.update();
    }

    function draw() {
      var r = S.r, p = S.p, xmax = quantile(R_MAX, p, 0.995), ymax = p * 1.08, mean = r / p;
      var ds = chart.data.datasets;
      cur = pmf(r, p, xmax);
      ds[0].data = pmf(1, p, xmax);
      ds[1].data = cur;
      ds[1].label = "r = " + r;
      ds[2].data = [{ x: mean, y: 0 }, { x: mean, y: ymax }];
      ds[3].data = [meanPoint()];
      var sc = chart.config.options.scales;
      sc.x.max = xmax; sc.y.max = ymax;
      chart.update("none");
      syncHandle(chart);
      var peak = Math.max.apply(null, cur.map(function (d) { return d.y; }));
      if (read.r) read.r.textContent = "r = " + r;
      if (read.mean) read.mean.textContent = Charts.fmt(mean, Math.abs(mean - Math.round(mean)) < 1e-9 ? 0 : 1);
      if (read.median) read.median.textContent = String(quantile(r, p, 0.5));
      if (read.peak) read.peak.textContent = Charts.fmt(peak, 3);
      root.querySelectorAll("[data-nb-p]").forEach(function (b) {
        b.setAttribute("aria-pressed", parseFloat(b.dataset.nbP) === p ? "true" : "false");
      });
    }

    // Przeciąganie: pozioma pozycja kursora to średnia, z niej r = round(x · p).
    var drag = false;
    function move(e) {
      var rc = canvas.getBoundingClientRect(), a = chart.chartArea;
      var px = Math.max(a.left, Math.min(a.right, e.clientX - rc.left));
      var r = Math.round(chart.scales.x.getValueForPixel(px) * S.p);
      r = Math.max(R_MIN, Math.min(R_MAX, r));
      if (r !== S.r) { S.r = r; draw(); }
    }
    canvas.style.touchAction = "none";
    Charts.listen(canvas, "pointerdown", function (e) {
      var rc = canvas.getBoundingClientRect(), a = chart.chartArea, y = e.clientY - rc.top;
      if (y < a.top || y > a.bottom) return;
      drag = true; canvas.setPointerCapture(e.pointerId); move(e); e.preventDefault();
    });
    Charts.listen(canvas, "pointermove", function (e) {
      if (drag) { move(e); return; }
      var rc = canvas.getBoundingClientRect(), a = chart.chartArea, y = e.clientY - rc.top;
      canvas.style.cursor = y >= a.top && y <= a.bottom ? "ew-resize" : "default";
    });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(canvas, t, function () { drag = false; }); });
    Charts.listen(handle.element, "pointermove", function (e) { if (drag) move(e); });
    Charts.listen(handle.element, "pointerup", function () { drag = false; });

    Charts.listen(root, "click", function (e) {
      var pb = e.target.closest("[data-nb-p]");
      if (pb) { S.p = parseFloat(pb.dataset.nbP); draw(); return; }
      if (e.target.closest("[data-nb-reset]")) { S.r = START.r; S.p = START.p; draw(); }
    });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-nb", init);
})();
