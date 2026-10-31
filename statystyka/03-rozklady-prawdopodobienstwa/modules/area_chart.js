// Pole pod gęstością N(μ, σ) między dwiema granicami (Ryc. 4.3). Granice a i b
// przeciąga się kursorem lub palcem: wciśnięcie łapie bliższą granicę, ruch ją
// przesuwa. Kontener: .lc-area[data-mu][data-sd][data-a][data-b][data-lo][data-hi]
// z <canvas data-role="pdf"> i odczytami [data-role="a"], [data-role="b"],
// [data-role="P"]. Wzorzec jak cdf_chart.js (Ryc. 4.4): Chart.js, start po
// pojawieniu się kontenera w DOM (rozdział renderuje się przez renderUI).
(function () {
  var Charts = window.lcCharts;
  var token = Charts.token, alpha = Charts.alpha;
  var SNAP = 0.5, nextId = 0;

  // Abramowitz–Stegun 7.1.26 (błąd < 1.5e-7).
  var phi = Charts.normalCdf;

  function init(root) {
    var canvas = root.querySelector('canvas[data-role="pdf"]');
    if (!canvas) return false;

    var MU = parseFloat(root.dataset.mu || "0"), SD = parseFloat(root.dataset.sd || "1");
    var LO = parseFloat(root.dataset.lo || String(MU - 4 * SD));
    var HI = parseFloat(root.dataset.hi || String(MU + 4 * SD));
    var a = parseFloat(root.dataset.a || String(MU - SD));
    var b = parseFloat(root.dataset.b || String(MU + SD));
    var handles = {}, box = canvas.parentElement;
    var hint = document.createElement("span"), status = document.createElement("span");
    hint.id = "lc-area-keys-" + (++nextId);
    hint.setAttribute("class", "lc-chart-sr");
    hint.textContent = "Strzałki przesuwają granicę o 0.5 cm. Shift ze strzałką: 5 cm. Home i End: krańce zakresu.";
    status.setAttribute("class", "lc-chart-sr");
    status.setAttribute("role", "status");
    status.setAttribute("aria-live", "polite");
    status.setAttribute("aria-atomic", "true");
    root.appendChild(hint); root.appendChild(status);
    ["a", "b"].forEach(function (key) {
      var handle = document.createElement("button");
      handle.setAttribute("type", "button");
      handle.setAttribute("class", "lc-chart-handle");
      handle.setAttribute("role", "slider");
      handle.setAttribute("data-boundary", key);
      handle.setAttribute("aria-label", key === "a" ? "Dolna granica przedziału" : "Górna granica przedziału");
      handle.setAttribute("aria-orientation", "horizontal");
      handle.setAttribute("aria-describedby", hint.id);
      box.appendChild(handle);
      handles[key] = handle;
    });
    Charts.onDispose(root, function () {
      handles.a.remove(); handles.b.remove(); hint.remove(); status.remove();
    });
    function syncHandles(ch) {
      ["a", "b"].forEach(function (key) {
        handles[key].style.left = ch.scales.x.getPixelForValue(key === "a" ? a : b) + "px";
        handles[key].style.top = ch.scales.y.getPixelForValue(TOP * 0.94) + "px";
      });
    }
    var readA = root.querySelector('[data-role="a"]');
    var readB = root.querySelector('[data-role="b"]');
    var readP = root.querySelector('[data-role="P"]');

    function pdf(x) { var z = (x - MU) / SD; return Math.exp(-z * z / 2) / (SD * Math.sqrt(2 * Math.PI)); }
    var TOP = pdf(MU) * 1.18;
    var XS = [];
    for (var x = LO; x <= HI + 1e-9; x += (HI - LO) / 480) XS.push(+x.toFixed(4));

    var chart = Charts.create(canvas, {
      type: "line",
      data: { datasets: [
        { data: XS.map(function (x) { return { x: x, y: pdf(x) }; }), borderWidth: 2 },
        { data: [], fill: "origin", borderWidth: 0 },
        { data: [], borderWidth: 2, borderDash: [5, 4] },
        { data: [], borderWidth: 2, borderDash: [5, 4] },
        { data: [], pointRadius: 8, pointHoverRadius: 8, borderWidth: 2, showLine: false }
      ] },
      plugins: [{ id: "areaHandles", afterRender: syncHandles }],
      options: {
        responsive: true, maintainAspectRatio: false, animation: false,
        locale: "en-US",
        events: [],
        layout: { padding: { top: 6, right: 8 } },
        scales: {
          x: { type: "linear", min: LO, max: HI, ticks: { stepSize: 10 },
               title: { display: true, text: "Wzrost (cm)" } },
          y: { min: 0, max: TOP, title: { display: true, text: "Gęstość" } }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });

    function paint() {
      var ink = token("--upwr-ink-soft") || "#444";
      var accent = token("--upwr-accent") || "#7a2e3b";
      var surface = token("--upwr-surface") || "#fff";
      Charts.styleAxes(chart, null, { fontSize: Charts.axisFontSize(root) });
      chart.options.scales.x.ticks.maxRotation = 0;
      chart.options.scales.x.ticks.maxTicksLimit = 7;
      var ds = chart.data.datasets;
      ds[0].borderColor = ink;
      ds[1].backgroundColor = alpha(accent, 0.28);
      ds[2].borderColor = ds[3].borderColor = accent;
      ds[4].backgroundColor = ds[4].pointBackgroundColor = ds[4].pointHoverBackgroundColor = accent;
      ds[4].borderColor = ds[4].pointBorderColor = ds[4].pointHoverBorderColor = surface;
      chart.update();
    }

    function draw() {
      var lo = Math.min(a, b), hi = Math.max(a, b), P = phi((hi - MU) / SD) - phi((lo - MU) / SD);
      var shade = [{ x: lo, y: pdf(lo) }].concat(
        XS.filter(function (x) { return x > lo && x < hi; }).map(function (x) { return { x: x, y: pdf(x) }; }),
        [{ x: hi, y: pdf(hi) }]);
      var ds = chart.data.datasets, hy = TOP * 0.94;
      ds[1].data = shade;
      ds[2].data = [{ x: a, y: 0 }, { x: a, y: hy }];
      ds[3].data = [{ x: b, y: 0 }, { x: b, y: hy }];
      ds[4].data = [{ x: a, y: hy }, { x: b, y: hy }];
      chart.update("none");
      if (readA) readA.textContent = lo.toFixed(1);
      if (readB) readB.textContent = hi.toFixed(1);
      if (readP) readP.textContent = P.toFixed(3);
      ["a", "b"].forEach(function (key) {
        var value = key === "a" ? a : b;
        handles[key].setAttribute("aria-valuemin", key === "a" ? LO : a);
        handles[key].setAttribute("aria-valuemax", key === "a" ? b : HI);
        handles[key].setAttribute("aria-valuenow", value);
        handles[key].setAttribute("aria-valuetext", value.toFixed(1) + " cm");
      });
      syncHandles(chart);
    }

    // Przeciąganie: wciśnięcie łapie bliższą granicę, ruch z wciśniętym przyciskiem ją przesuwa.
    var drag = null;
    function valueAt(e) {
      var r = canvas.getBoundingClientRect(), area = chart.chartArea;
      var px = Math.max(area.left, Math.min(area.right, e.clientX - r.left));
      var v = chart.scales.x.getValueForPixel(px);
      return Math.max(LO, Math.min(HI, Math.round(v / SNAP) * SNAP));
    }
    function move(e) {
      var v = valueAt(e);
      setBoundary(drag, v);
    }
    function setBoundary(key, value) {
      value = Math.round(value / SNAP) * SNAP;
      if (key === "a") a = Math.max(LO, Math.min(b, value));
      else b = Math.max(a, Math.min(HI, value));
      draw();
    }
    function announce() {
      status.textContent = "Przedział od " + a.toFixed(1) + " do " + b.toFixed(1) +
        " cm. Prawdopodobieństwo " + (phi((b - MU) / SD) - phi((a - MU) / SD)).toFixed(3) + ".";
    }
    ["a", "b"].forEach(function (key) {
      Charts.listen(handles[key], "keydown", function (event) {
        var delta = event.shiftKey ? 5 : SNAP, current = key === "a" ? a : b;
        if (["ArrowRight", "ArrowUp"].indexOf(event.key) >= 0) setBoundary(key, current + delta);
        else if (["ArrowLeft", "ArrowDown"].indexOf(event.key) >= 0) setBoundary(key, current - delta);
        else if (event.key === "Home") setBoundary(key, key === "a" ? LO : a);
        else if (event.key === "End") setBoundary(key, key === "a" ? b : HI);
        else return;
        event.preventDefault();
        announce();
      });
    });
    canvas.style.touchAction = "none";
    canvas.style.cursor = "ew-resize";
    [canvas, handles.a, handles.b].forEach(function (target) {
      Charts.listen(target, "pointerdown", function (event) {
        var v = valueAt(event);
        drag = target === canvas ? (Math.abs(v - a) <= Math.abs(v - b) ? "a" : "b") : target.dataset.boundary;
        if (target !== canvas) target.focus();
        canvas.setPointerCapture(event.pointerId);
        move(event);
        event.preventDefault();
      });
    });
    Charts.listen(canvas, "pointermove", function (event) { if (drag) move(event); });
    ["pointerup", "pointercancel"].forEach(function (type) {
      Charts.listen(canvas, type, function () { if (drag) announce(); drag = null; });
    });
    Charts.zoomControl(root);

    paint();
    draw();

    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-area", init);
})();
