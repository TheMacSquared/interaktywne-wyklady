// Pole pod gęstością N(μ, σ) między dwiema granicami (Ryc. 4.3). Granice a i b
// przeciąga się kursorem lub palcem: wciśnięcie łapie bliższą granicę, ruch ją
// przesuwa. Kontener: .lc-area[data-mu][data-sd][data-a][data-b][data-lo][data-hi]
// z <div class="lc-ec" data-role="pdf"> i odczytami [data-role="a"], [data-role="b"],
// [data-role="P"]. ECharts (renderer SVG), start po pojawieniu się kontenera
// w DOM (rozdział renderuje się przez renderUI).
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;
  var SNAP = 0.5, nextId = 0;

  // Abramowitz–Stegun 7.1.26 (błąd < 1.5e-7).
  var phi = Charts.normalCdf;

  function init(root) {
    var el = root.querySelector('.lc-ec[data-role="pdf"]');
    if (!el) return false;

    var MU = parseFloat(root.dataset.mu || "0"), SD = parseFloat(root.dataset.sd || "1");
    var LO = parseFloat(root.dataset.lo || String(MU - 4 * SD));
    var HI = parseFloat(root.dataset.hi || String(MU + 4 * SD));
    var a = parseFloat(root.dataset.a || String(MU - SD));
    var b = parseFloat(root.dataset.b || String(MU + SD));
    var handles = {}, box = el.parentElement;
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

    var readA = root.querySelector('[data-role="a"]');
    var readB = root.querySelector('[data-role="b"]');
    var readP = root.querySelector('[data-role="P"]');

    function pdf(x) { var z = (x - MU) / SD; return Math.exp(-z * z / 2) / (SD * Math.sqrt(2 * Math.PI)); }
    var TOP = pdf(MU) * 1.18, HY = TOP * 0.94;
    var XS = [];
    for (var x = LO; x <= HI + 1e-9; x += (HI - LO) / 480) XS.push(+x.toFixed(4));

    function px(value, y) { return chart.convertToPixel({ gridIndex: 0 }, [value, y]); }
    function syncHandles() {
      ["a", "b"].forEach(function (key) {
        var p = px(key === "a" ? a : b, HY);
        if (!p) return;
        handles[key].style.left = p[0] + "px";
        handles[key].style.top = p[1] + "px";
      });
    }

    function line(id, width, dash) {
      return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
               lineStyle: { width: width, type: dash ? [5, 4] : "solid" } };
    }
    var chart = Charts.chart(el, {
      animation: false,
      aria: { enabled: false },
      grid: { left: 8, right: 12, top: 10, bottom: 8 },
      xAxis: { type: "value", min: LO, max: HI, interval: 10, name: "Wzrost (cm)" },
      yAxis: { type: "value", min: 0, max: TOP, splitNumber: 4, name: "Gęstość",
               axisLabel: { showMaxLabel: false, formatter: function (v) { return Charts.fmt(v, 3); } } },
      series: [
        Object.assign(line("shade", 0), { areaStyle: {} }),
        Object.assign(line("pdf", 2), { data: XS.map(function (x) { return [x, pdf(x)]; }) }),
        line("lineA", 2, true),
        line("lineB", 2, true),
        { id: "dots", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false }
      ]
    }, { onResize: syncHandles });

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var y = Charts.ecAxis(P, size, { nameGap: Math.round(size * 3.6) });
      y.axisLabel.showMaxLabel = false;
      y.axisLabel.formatter = function (v) { return Charts.fmt(v, 3); };
      chart.setOption({
        xAxis: Charts.ecAxis(P, size, { splitLine: { show: true, lineStyle: { color: P.ruleSoft } } }),
        yAxis: y,
        series: [
          { id: "shade", lineStyle: { width: 0 }, areaStyle: { color: alpha(P.accent, 0.28) } },
          { id: "pdf", lineStyle: { color: P.soft } },
          { id: "lineA", lineStyle: { color: P.accent } },
          { id: "lineB", lineStyle: { color: P.accent } },
          { id: "dots", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } }
        ]
      });
      syncHandles();
    }

    function draw() {
      var lo = Math.min(a, b), hi = Math.max(a, b), P = phi((hi - MU) / SD) - phi((lo - MU) / SD);
      var shade = [[lo, pdf(lo)]].concat(
        XS.filter(function (x) { return x > lo && x < hi; }).map(function (x) { return [x, pdf(x)]; }),
        [[hi, pdf(hi)]]);
      chart.setOption({ series: [
        { id: "shade", data: shade },
        { id: "lineA", data: [[a, 0], [a, HY]] },
        { id: "lineB", data: [[b, 0], [b, HY]] },
        { id: "dots", data: [[a, HY], [b, HY]] }
      ] });
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
      syncHandles();
    }

    // Przeciąganie: wciśnięcie łapie bliższą granicę, ruch z wciśniętym przyciskiem ją przesuwa.
    var drag = null;
    function valueAt(e) {
      var r = el.getBoundingClientRect();
      var left = px(LO, 0)[0], right = px(HI, 0)[0];
      var p = Math.max(left, Math.min(right, e.clientX - r.left));
      var v = chart.convertFromPixel({ gridIndex: 0 }, [p, 0])[0];
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
    el.style.touchAction = "none";
    el.style.cursor = "ew-resize";
    [el, handles.a, handles.b].forEach(function (target) {
      Charts.listen(target, "pointerdown", function (event) {
        var v = valueAt(event);
        drag = target === el ? (Math.abs(v - a) <= Math.abs(v - b) ? "a" : "b") : target.dataset.boundary;
        if (target !== el) target.focus();
        el.setPointerCapture(event.pointerId);
        move(event);
        event.preventDefault();
      });
    });
    Charts.listen(el, "pointermove", function (event) { if (drag) move(event); });
    ["pointerup", "pointercancel"].forEach(function (type) {
      Charts.listen(el, type, function () { if (drag) announce(); drag = null; });
    });
    Charts.zoomControl(root);

    paint();
    draw();

    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-area", init);
})();
