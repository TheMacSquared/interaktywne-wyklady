// Dystrybuanta N(0, 1): dwa połączone wykresy Chart.js (gęstość z polem
// na lewo od x i krzywa F(x) z punktem). Kursor lub palec na którymkolwiek
// wykresie ustawia x. Kontener: .lc-cdf[data-x] z dwoma <canvas>
// (data-role="pdf" / "cdf") i odczytami [data-role="x"] / [data-role="F"].
// Rozdział renderuje się przez renderUI, więc wykresy startują, gdy
// kontener pojawi się w DOM.
(function () {
  var Charts = window.lcCharts;
  var token = Charts.token, alpha = Charts.alpha;
  var X_MIN = -4, X_MAX = 4, STEP = 0.025;

  function pdf(x) { return Math.exp(-x * x / 2) / Math.sqrt(2 * Math.PI); }

  // Abramowitz–Stegun 7.1.26 (błąd < 1.5e-7) — wystarcza do 4 miejsc.
  var cdf = Charts.normalCdf;

  function fmt(v, d) { return v.toFixed(d); }

  var XS = [];
  for (var i = 0; i <= (X_MAX - X_MIN) / STEP; i++) XS.push(+(X_MIN + i * STEP).toFixed(3));

  function axes(yMax, yTitle) {
    return {
      x: { type: "linear", min: X_MIN, max: X_MAX,
           ticks: { stepSize: 1 }, title: { display: true, text: "x" } },
      y: { min: 0, max: yMax, title: { display: true, text: yTitle } }
    };
  }

  function baseOptions(yMax, yTitle) {
    return {
      responsive: true, maintainAspectRatio: false, animation: false,
      locale: "en-US",  // kropka dziesiętna na osiach
      events: ["mousemove", "mousedown", "touchstart", "touchmove", "click"],
      layout: { padding: { top: 6, right: 8 } },
      scales: axes(yMax, yTitle),
      plugins: { legend: { display: false }, tooltip: { enabled: false },
                 colors: { enabled: false } },
      elements: { point: { radius: 0 }, line: { tension: 0 } }
    };
  }

  function init(root) {
    var pdfCanvas = root.querySelector('canvas[data-role="pdf"]');
    var cdfCanvas = root.querySelector('canvas[data-role="cdf"]');
    if (!pdfCanvas || !cdfCanvas) return false;

    var x0 = parseFloat(root.dataset.x || "1");
    var keyboard = false, dragging = false;
    var layer = Charts.handleLayer(root);
    var handle = layer.add(cdfCanvas, {
      label: "Położenie x na dystrybuancie",
      hint: "Strzałki zmieniają x o 0.05; Shift ze strzałką o 0.5. Home i End: krańce osi.",
      onKey: function (event) {
        var value = Charts.keyValue(event, x0, X_MIN, X_MAX, 0.05, 0.5);
        if (value === null) return false;
        keyboard = true; setX(value);
        layer.announce("x = " + fmt(x0, 2) + ". F(x) = " + fmt(cdf(x0), 4) + ".");
        return true;
      },
      onPointerDown: function (event) {
        keyboard = false; dragging = true;
        cdfCanvas.setPointerCapture(event.pointerId);
        pointerX(event);
        event.preventDefault();
      }
    });
    function positionHandle(ch) {
      handle.position(ch.scales.x.getPixelForValue(x0), ch.scales.y.getPixelForValue(cdf(x0)));
    }
    var readX = root.querySelector('[data-role="x"]');
    var readF = root.querySelector('[data-role="F"]');

    var pdfChart = Charts.create(pdfCanvas, {
      type: "line",
      data: { datasets: [
        { data: XS.map(function (x) { return { x: x, y: pdf(x) }; }), borderWidth: 2 },
        { data: [], fill: "origin", borderWidth: 0 },
        { data: [], borderWidth: 1.5 }
      ] },
      options: baseOptions(0.45, "Gęstość f(x)")
    });

    var cdfChart = Charts.create(cdfCanvas, {
      type: "line",
      data: { datasets: [
        { data: XS.map(function (x) { return { x: x, y: cdf(x) }; }), borderWidth: 2 },
        { data: [], borderWidth: 1.5, borderDash: [5, 4] },
        { data: [], borderWidth: 1.5, borderDash: [5, 4] },
        { data: [], pointRadius: 6, pointHoverRadius: 6, borderWidth: 2, showLine: false }
      ] },
      options: baseOptions(1.05, "F(x) = P(X ≤ x)"),
      plugins: [{ id: "cdfHandle", afterRender: positionHandle }]
    });
    cdfChart.options.scales.y.max = 1;
    cdfChart.options.scales.y.ticks = { stepSize: 0.2 };

    function paint() {
      var ink = token("--upwr-ink-soft") || "#444";
      var accent = token("--upwr-accent") || "#7a2e3b";
      [pdfChart, cdfChart].forEach(function (ch) {
        Charts.styleAxes(ch, null, { fontSize: Charts.axisFontSize(root), maxRotation: 0 });
        ch.options.scales.x.ticks.maxTicksLimit = 5;
        ch.data.datasets[0].borderColor = ink;
      });
      pdfChart.data.datasets[1].backgroundColor = alpha(accent, 0.28);
      pdfChart.data.datasets[2].borderColor = accent;
      cdfChart.data.datasets[1].borderColor = accent;
      cdfChart.data.datasets[2].borderColor = accent;
      var surface = token("--upwr-surface") || "#fff";
      var dot = cdfChart.data.datasets[3];
      dot.backgroundColor = dot.pointBackgroundColor = dot.pointHoverBackgroundColor = accent;
      dot.borderColor = dot.pointBorderColor = dot.pointHoverBorderColor = surface;
      // Pełny update: tryb "none" zostawia w punktach zbuforowane stare kolory.
      pdfChart.update();
      cdfChart.update();
    }

    function setX(x) {
      x0 = Math.max(X_MIN, Math.min(X_MAX, Math.round(x * 100) / 100));
      var F = cdf(x0);
      var shade = XS.filter(function (x) { return x <= x0; })
        .map(function (x) { return { x: x, y: pdf(x) }; });
      shade.push({ x: x0, y: pdf(x0) });
      pdfChart.data.datasets[1].data = shade;
      pdfChart.data.datasets[2].data = [{ x: x0, y: 0 }, { x: x0, y: pdf(x0) }];
      cdfChart.data.datasets[1].data = [{ x: x0, y: 0 }, { x: x0, y: F }];
      cdfChart.data.datasets[2].data = [{ x: X_MIN, y: F }, { x: x0, y: F }];
      cdfChart.data.datasets[3].data = [{ x: x0, y: F }];
      pdfChart.update("none");
      cdfChart.update("none");
      if (readX) readX.textContent = fmt(x0, 2);
      if (readF) readF.textContent = fmt(F, 4);
      handle.value(x0, X_MIN, X_MAX, "x = " + fmt(x0, 2) + "; F(x) = " + fmt(F, 4));
      positionHandle(cdfChart);
    }

    function pointerX(event) {
      var rect = cdfCanvas.getBoundingClientRect(), axis = cdfChart.scales.x;
      setX(axis.getValueForPixel(event.clientX - rect.left));
    }
    function follow(ch) {
      ch.options.onHover = function (event) {
        if (keyboard) return;
        var area = ch.chartArea;
        if (event.x < area.left || event.x > area.right) return;
        setX(ch.scales.x.getValueForPixel(event.x));
      };
      ch.options.onClick = function (event) {
        keyboard = false;
        setX(ch.scales.x.getValueForPixel(event.x));
      };
      Charts.listen(ch.canvas, "pointerdown", function () { keyboard = false; });
    }
    follow(pdfChart); follow(cdfChart);
    Charts.listen(cdfCanvas, "pointermove", function (event) { if (dragging) pointerX(event); });
    ["pointerup", "pointercancel"].forEach(function (type) {
      Charts.listen(cdfCanvas, type, function () { dragging = false; });
    });
    Charts.zoomControl(root);

    paint();
    setX(x0);

    // Przełącznik jasny/ciemny zmienia tokeny CSS — przemaluj wykresy.
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-cdf", init);
})();
