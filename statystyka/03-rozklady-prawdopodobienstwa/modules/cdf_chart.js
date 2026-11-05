// Dystrybuanta N(0, 1): dwa połączone wykresy ECharts (gęstość z polem
// na lewo od x i krzywa F(x) z punktem). Kursor lub palec na którymkolwiek
// wykresie ustawia x. Kontener: .lc-cdf[data-x] z dwoma <div class="lc-ec">
// (data-role="pdf" / "cdf") i odczytami [data-role="x"] / [data-role="F"].
// Rozdział renderuje się przez renderUI, więc wykresy startują, gdy
// kontener pojawi się w DOM.
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;
  var X_MIN = -4, X_MAX = 4, STEP = 0.025;

  function pdf(x) { return Math.exp(-x * x / 2) / Math.sqrt(2 * Math.PI); }

  // Abramowitz–Stegun 7.1.26 (błąd < 1.5e-7) — wystarcza do 4 miejsc.
  var cdf = Charts.normalCdf;

  function fmt(v, d) { return v.toFixed(d); }

  var XS = [];
  for (var i = 0; i <= (X_MAX - X_MIN) / STEP; i++) XS.push(+(X_MIN + i * STEP).toFixed(3));

  function line(id, width, dash) {
    return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
             lineStyle: { width: width, type: dash ? [5, 4] : "solid" } };
  }

  function option(yMax, yTitle, yStep, series) {
    return {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 12, top: 10, bottom: 8 },
      xAxis: { type: "value", min: X_MIN, max: X_MAX, interval: 2, name: "x" },
      yAxis: { type: "value", min: 0, max: yMax, interval: yStep, name: yTitle },
      series: series
    };
  }

  function init(root) {
    var pdfEl = root.querySelector('.lc-ec[data-role="pdf"]');
    var cdfEl = root.querySelector('.lc-ec[data-role="cdf"]');
    if (!pdfEl || !cdfEl) return false;

    var x0 = parseFloat(root.dataset.x || "1");
    var keyboard = false, dragging = false;
    var layer = Charts.handleLayer(root);
    var handle = layer.add(cdfEl, {
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
        cdfEl.setPointerCapture(event.pointerId);
        setX(Charts.ecPointer(cdfChart, cdfEl, event, [X_MIN, X_MAX]).x);
        event.preventDefault();
      }
    });
    function positionHandle() {
      var p = cdfChart.convertToPixel({ gridIndex: 0 }, [x0, cdf(x0)]);
      if (p) handle.position(p[0], p[1]);
    }
    var readX = root.querySelector('[data-role="x"]');
    var readF = root.querySelector('[data-role="F"]');

    var pdfChart = Charts.chart(pdfEl, option(0.45, "Gęstość f(x)", 0.1, [
      Object.assign(line("shade", 0), { areaStyle: {} }),
      Object.assign(line("curve", 2), { data: XS.map(function (x) { return [x, pdf(x)]; }) }),
      line("mark", 1.5)
    ]));
    var cdfChart = Charts.chart(cdfEl, option(1, "F(x) = P(X ≤ x)", 0.2, [
      Object.assign(line("curve", 2), { data: XS.map(function (x) { return [x, cdf(x)]; }) }),
      line("vert", 1.5, true),
      line("horiz", 1.5, true),
      { id: "dot", type: "scatter", data: [], symbolSize: 12, silent: true, animation: false }
    ]), { onResize: positionHandle });

    function axes(P, size, yTitleGap) {
      return {
        xAxis: Charts.ecAxis(P, size),
        yAxis: Charts.ecAxis(P, size, { nameGap: Math.round(size * yTitleGap) })
      };
    }

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      // Górny kraniec 0.45 nie jest podziałką co 0.1 — bez etykiety nachodziłby na 0.4.
      var pdfAxes = axes(P, size, 3.2);
      pdfAxes.yAxis.axisLabel.showMaxLabel = false;
      pdfChart.setOption(Object.assign(pdfAxes, { series: [
        { id: "shade", lineStyle: { width: 0 }, areaStyle: { color: alpha(P.accent, 0.28) } },
        { id: "curve", lineStyle: { color: P.soft } },
        { id: "mark", lineStyle: { color: P.accent } }
      ] }));
      cdfChart.setOption(Object.assign(axes(P, size, 2.8), { series: [
        { id: "curve", lineStyle: { color: P.soft } },
        { id: "vert", lineStyle: { color: P.accent } },
        { id: "horiz", lineStyle: { color: P.accent } },
        { id: "dot", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } }
      ] }));
      positionHandle();
    }

    function setX(x) {
      x0 = Math.max(X_MIN, Math.min(X_MAX, Math.round(x * 100) / 100));
      var F = cdf(x0);
      var shade = XS.filter(function (x) { return x <= x0; }).map(function (x) { return [x, pdf(x)]; });
      shade.push([x0, pdf(x0)]);
      pdfChart.setOption({ series: [
        { id: "shade", data: shade },
        { id: "mark", data: [[x0, 0], [x0, pdf(x0)]] }
      ] });
      cdfChart.setOption({ series: [
        { id: "vert", data: [[x0, 0], [x0, F]] },
        { id: "horiz", data: [[X_MIN, F], [x0, F]] },
        { id: "dot", data: [[x0, F]] }
      ] });
      if (readX) readX.textContent = fmt(x0, 2);
      if (readF) readF.textContent = fmt(F, 4);
      handle.value(x0, X_MIN, X_MAX, "x = " + fmt(x0, 2) + "; F(x) = " + fmt(F, 4));
      positionHandle();
    }

    // Najechanie kursorem ustawia x, dopóki wybór nie pochodzi z klawiatury.
    function follow(ch, el) {
      el.style.cursor = "crosshair";
      Charts.listen(el, "pointermove", function (event) {
        var p = Charts.ecPointer(ch, el, event);
        if (dragging && el === cdfEl) { setX(Math.max(X_MIN, Math.min(X_MAX, p.rawX))); return; }
        if (keyboard || !(p.rawX >= X_MIN && p.rawX <= X_MAX)) return;
        setX(p.rawX);
      });
      Charts.listen(el, "pointerdown", function (event) {
        keyboard = false;
        var p = Charts.ecPointer(ch, el, event);
        if (p.rawX >= X_MIN && p.rawX <= X_MAX) setX(p.rawX);
      });
    }
    follow(pdfChart, pdfEl); follow(cdfChart, cdfEl);
    ["pointerup", "pointercancel"].forEach(function (type) {
      Charts.listen(cdfEl, type, function () { dragging = false; });
    });
    Charts.zoomControl(root);

    paint();
    setX(x0);

    // Przełącznik jasny/ciemny zmienia tokeny CSS — przemaluj wykresy.
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-cdf", init);
})();
