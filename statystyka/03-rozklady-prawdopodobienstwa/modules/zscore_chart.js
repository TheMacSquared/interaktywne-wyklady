// Kalkulator z-score w Chart.js (prototypy 5.4-A i 5.4-B). Pola μ, σ, x
// ([data-role="mu|sd|x"] — zwykłe <input type="number">), x także przeciągany po wykresie.
// data-mode="two": dwa wykresy pod sobą (skala x i skala z, wyrównane);
// data-mode="dual": jedna krzywa z dwiema osiami pod spodem (x i z).
// Odczyty: [data-role="z"] i [data-role="calc"] (jedna linia wzoru).
// Wzorzec jak area_chart.js (Ryc. 4.3).
(function () {
  var Charts = window.lcCharts;
  var token = Charts.token, alpha = Charts.alpha;

  function pdf(v, mu, sd) { var z = (v - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }
  function num(v) { return String(+(+v).toFixed(2)); }

  function chart(canvas, scales) {
    return Charts.create(canvas, {
      type: "line",
      data: { datasets: [
        { data: [], fill: "origin", borderWidth: 2.5 },
        { data: [], borderWidth: 2.5 },
        { data: [], pointRadius: 7, pointHoverRadius: 7, borderWidth: 2, showLine: false }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 6, right: 12 } }, scales: scales,
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });
  }

  function init(root) {
    var mode = root.dataset.mode || "two";
    var cvX = root.querySelector('canvas[data-role="x"]'), cvZ = root.querySelector('canvas[data-role="z"]');
    if (!cvX || (mode === "two" && !cvZ)) return false;
    var inp = {};
    ["mu", "sd", "x"].forEach(function (k) { inp[k] = root.querySelector('input[data-role="' + k + '"]'); });
    var readZ = root.querySelector('[data-role="zval"]'), readC = root.querySelector('[data-role="calc"]');
    var P = { mu: +inp.mu.value, sd: +inp.sd.value, x: +inp.x.value };

    var charts = [];
    var cx = chart(cvX, mode === "dual" ? {
      x: { type: "linear", position: "bottom", title: { display: true, text: "x" } },
      xz: { type: "linear", position: "bottom", min: -4, max: 4, ticks: { stepSize: 1 }, title: { display: true, text: "z" } },
      y: { min: 0, display: false }
    } : {
      x: { type: "linear", title: { display: true, text: "x" } },
      y: { min: 0, display: false }
    });
    charts.push(cx);
    var cz = null;
    if (mode === "two") {
      cz = chart(cvZ, { x: { type: "linear", min: -4, max: 4, ticks: { stepSize: 1 }, title: { display: true, text: "z" } },
                        y: { min: 0, display: false } });
      charts.push(cz);
    }

    function paint() {
      var ink = token("--upwr-ink-soft") || "#444", accent = token("--upwr-accent") || "#7a2e3b";
      var surface = token("--upwr-surface") || "#fff";
      var fill = alpha(token("--upwr-rule") || "#d9cfbc", 0.5);
      charts.forEach(function (c) {
        Charts.styleAxes(c, null, { includeBounds: false, titleSize: 13 });
        if (c.options.scales.xz) c.options.scales.xz.grid.display = false;
        var ds = c.data.datasets;
        ds[0].borderColor = ink; ds[0].backgroundColor = fill;
        ds[1].borderColor = accent;
        ds[2].pointBackgroundColor = ds[2].pointHoverBackgroundColor = accent;
        ds[2].pointBorderColor = ds[2].pointHoverBorderColor = surface;
        c.update();
      });
    }

    function draw() {
      var mu = P.mu, sd = Math.max(P.sd, 1e-6), lo = mu - 4 * sd, hi = mu + 4 * sd;
      var x = Math.max(lo, Math.min(hi, P.x)), z = (P.x - mu) / sd, top = pdf(mu, mu, sd);
      var xs = [];
      for (var v = lo; v <= hi + 1e-9; v += (hi - lo) / 300) xs.push(v);
      cx.options.scales.x.min = lo; cx.options.scales.x.max = hi;
      cx.options.scales.y.max = top * 1.2;
      var ds = cx.data.datasets;
      ds[0].data = xs.map(function (v) { return { x: v, y: pdf(v, mu, sd) }; });
      ds[1].data = [{ x: x, y: 0 }, { x: x, y: top * 1.08 }];
      ds[2].data = [{ x: x, y: top * 1.08 }];
      cx.update("none");
      if (cz) {
        var zz = Math.max(-4, Math.min(4, z)), zs = [];
        for (var t = -4; t <= 4.0001; t += 0.02) zs.push(t);
        cz.options.scales.y.max = pdf(0, 0, 1) * 1.2;
        var d2 = cz.data.datasets;
        d2[0].data = zs.map(function (t) { return { x: t, y: pdf(t, 0, 1) }; });
        d2[1].data = [{ x: zz, y: 0 }, { x: zz, y: pdf(0, 0, 1) * 1.08 }];
        d2[2].data = [{ x: zz, y: pdf(0, 0, 1) * 1.08 }];
        cz.update("none");
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
      var rc = cvX.getBoundingClientRect(), area = cx.chartArea;
      var px = Math.max(area.left, Math.min(area.right, e.clientX - rc.left));
      var step = Math.pow(10, Math.floor(Math.log10(P.sd)) - 1);
      P.x = Math.round(cx.scales.x.getValueForPixel(px) / step) * step;
      P.x = +P.x.toFixed(6);
      if (inp.x) inp.x.value = num(P.x);
      draw();
    }
    cvX.style.touchAction = "none"; cvX.style.cursor = "ew-resize";
    Charts.listen(cvX, "pointerdown", function (e) { drag = true; cvX.setPointerCapture(e.pointerId); move(e); e.preventDefault(); });
    Charts.listen(cvX, "pointermove", function (e) { if (drag) move(e); });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(cvX, t, function () { drag = false; }); });

    paint();
    draw();
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-zcalc", init);
})();
