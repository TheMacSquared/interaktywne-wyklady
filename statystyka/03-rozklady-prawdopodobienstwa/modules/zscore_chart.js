// Kalkulator z-score w ECharts (Ryc. 5.4). Pola μ, σ, x
// ([data-role="mu|sd|x"] — zwykłe <input type="number">), x także przeciągany po wykresie.
// data-mode="two": dwa wykresy pod sobą (skala x i skala z, wyrównane);
// data-mode="dual": jedna krzywa z dwiema osiami pod spodem (x i z).
// Odczyty: [data-role="z"] i [data-role="calc"] (jedna linia wzoru).
// Wzorzec jak area_chart.js (Ryc. 4.3).
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;

  function pdf(v, mu, sd) { var z = (v - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }
  function num(v) { return String(+(+v).toFixed(2)); }

  function line(id, width) {
    return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
             lineStyle: { width: width } };
  }

  // Stałe marginesy (outerBoundsMode: "none") utrzymują wyrównanie osi x i z
  // mimo różnych etykiet.
  function makeChart(el, xAxis) {
    return Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 24, right: 24, top: 8, bottom: 40, outerBoundsMode: "none" },
      xAxis: xAxis,
      yAxis: { type: "value", min: 0, show: false },
      series: [
        Object.assign(line("pdf", 2.5), { areaStyle: {} }),
        line("mark", 2.5),
        { id: "dot", type: "scatter", data: [], symbolSize: 14, silent: true, animation: false }
      ]
    });
  }

  function init(root) {
    var mode = root.dataset.mode || "two";
    var elX = root.querySelector('.lc-ec[data-role="x"]'), elZ = root.querySelector('.lc-ec[data-role="z"]');
    if (!elX || (mode === "two" && !elZ)) return false;
    var inp = {};
    ["mu", "sd", "x"].forEach(function (k) { inp[k] = root.querySelector('input[data-role="' + k + '"]'); });
    var readZ = root.querySelector('[data-role="zval"]'), readC = root.querySelector('[data-role="calc"]');
    var P = { mu: +inp.mu.value, sd: +inp.sd.value, x: +inp.x.value };

    var charts = [];
    var cx = makeChart(elX, mode === "dual" ? [
      { type: "value", position: "bottom", name: "x" },
      { type: "value", position: "bottom", min: -4, max: 4, interval: 1, name: "z" }
    ] : { type: "value", name: "x" });
    charts.push(cx);
    var cz = null;
    if (mode === "two") {
      cz = makeChart(elZ, { type: "value", min: -4, max: 4, interval: 1, name: "z" });
      charts.push(cz);
    }

    function paint() {
      var C = Charts.palette(), size = Charts.axisFontSize(root), pad = Math.ceil(size * 1.6);
      charts.forEach(function (c) {
        function axis(offset, grid) {
          var a = Charts.ecAxis(C, size, { nameGap: Math.round(size * 1.8), offset: offset || 0,
            nameTextStyle: { color: C.subtle, fontFamily: C.mono, fontSize: size, verticalAlign: "top" } });
          a.axisLabel.showMinLabel = a.axisLabel.showMaxLabel = false;
          if (grid === false) a.splitLine = { show: false };
          a.splitNumber = 5;
          return a;
        }
        var dual = c === cx && mode === "dual";
        c.setOption({
          grid: { left: pad, right: pad, bottom: Math.ceil(size * (dual ? 7.2 : 3.6)) },
          xAxis: dual ? [axis(0), axis(Math.ceil(size * 3.6), false)] : axis(0),
          series: [
            { id: "pdf", lineStyle: { color: C.soft }, areaStyle: { color: alpha(C.rule, 0.5) } },
            { id: "mark", lineStyle: { color: C.accent } },
            { id: "dot", itemStyle: { color: C.accent, borderColor: C.surface, borderWidth: 2 } }
          ]
        });
      });
    }

    function draw() {
      var mu = P.mu, sd = Math.max(P.sd, 1e-6), lo = mu - 4 * sd, hi = mu + 4 * sd;
      var x = Math.max(lo, Math.min(hi, P.x)), z = (P.x - mu) / sd, top = pdf(mu, mu, sd);
      var xs = [];
      for (var v = lo; v <= hi + 1e-9; v += (hi - lo) / 300) xs.push(v);
      cx.setOption({
        xAxis: mode === "dual" ? [{ min: lo, max: hi }, {}] : { min: lo, max: hi },
        yAxis: { max: top * 1.2 },
        series: [
          { id: "pdf", data: xs.map(function (v) { return [v, pdf(v, mu, sd)]; }) },
          { id: "mark", data: [[x, 0], [x, top * 1.08]] },
          { id: "dot", data: [[x, top * 1.08]] }
        ]
      });
      if (cz) {
        var zz = Math.max(-4, Math.min(4, z)), zs = [];
        for (var t = -4; t <= 4.0001; t += 0.02) zs.push(t);
        cz.setOption({
          yAxis: { max: pdf(0, 0, 1) * 1.2 },
          series: [
            { id: "pdf", data: zs.map(function (t) { return [t, pdf(t, 0, 1)]; }) },
            { id: "mark", data: [[zz, 0], [zz, pdf(0, 0, 1) * 1.08]] },
            { id: "dot", data: [[zz, pdf(0, 0, 1) * 1.08]] }
          ]
        });
      }
      if (readZ) readZ.textContent = z.toFixed(2);
      if (readC) readC.textContent = "z = (" + num(P.x) + " − " + num(mu) + ") / " + num(sd) + " = " + z.toFixed(2);
    }

    ["mu", "sd", "x"].forEach(function (k) {
      if (!inp[k]) return;
      Charts.listen(inp[k], "input", function () {
        var v = parseFloat(inp[k].value);
        if (isFinite(v) && (k !== "sd" || v > 0)) { P[k] = v; draw(); }
      });
    });

    // Przeciąganie x po górnym wykresie.
    var drag = false;
    function move(e) {
      var sd = Math.max(P.sd, 1e-6), lo = P.mu - 4 * sd, hi = P.mu + 4 * sd;
      var v = Charts.ecPointer(cx, elX, e, [lo, hi]).x;
      var step = Math.pow(10, Math.floor(Math.log10(P.sd)) - 1);
      P.x = Math.round(v / step) * step;
      P.x = +P.x.toFixed(6);
      if (inp.x) inp.x.value = num(P.x);
      draw();
    }
    elX.style.touchAction = "none"; elX.style.cursor = "ew-resize";
    Charts.listen(elX, "pointerdown", function (e) { drag = true; elX.setPointerCapture(e.pointerId); move(e); e.preventDefault(); });
    Charts.listen(elX, "pointermove", function (e) { if (drag) move(e); });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(elX, t, function () { drag = false; }); });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-zcalc", init);
})();
