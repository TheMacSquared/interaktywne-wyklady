// Galeria przypadków z rozkładem normalnym (prototyp 5a): przyciski przypadków,
// gęstość N(μ, σ), próg przeciągany po wykresie i zacieniony ogon.
// data-config (JSON z R): [{short, story, q, axis, unit, mu, sd, t, tail, step}], tail = "lower" | "upper"
// | "outside" (oba ogony dalej niż |t − μ| od μ). Kontener .lc-cases z <canvas data-role="pdf">,
// przyciskami [data-c-case] i odczytami [data-role="model|t|p|q"]. Wzorzec jak area_chart.js.
(function () {
  var Charts = window.lcCharts;
  var token = Charts.token, alpha = Charts.alpha;
  var phi = Charts.normalCdf;

  function pdf(v, mu, sd) { var z = (v - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }
  function decs(step) { return step >= 1 ? 0 : step >= 0.1 ? 1 : 2; }

  function init(root) {
    var canvas = root.querySelector('canvas[data-role="pdf"]');
    if (!canvas) return false;
    var CASES = JSON.parse(root.dataset.config || "[]"), C = null, t = 0, LO = 0, HI = 1, XS = [];
    var read = {};
    ["model", "t", "p", "q", "story"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });

    var chart = Charts.create(canvas, {
      type: "line",
      data: { datasets: [
        { data: [], borderWidth: 2 },
        { data: [], fill: "origin", borderWidth: 0 },
        { data: [], fill: "origin", borderWidth: 0 },
        { data: [], borderWidth: 2, borderDash: [5, 4] },
        { data: [], borderWidth: 2, borderDash: [5, 4] },
        { data: [], pointRadius: 8, pointHoverRadius: 8, borderWidth: 2, showLine: false }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 6, right: 10 } },
        scales: { x: { type: "linear", title: { display: true, text: "" } }, y: { min: 0, display: false } },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });

    function paint() {
      var ink = token("--upwr-ink-soft") || "#444";
      var accent = token("--upwr-accent") || "#7a2e3b";
      var surface = token("--upwr-surface") || "#fff";
      Charts.styleAxes(chart, null, { includeBounds: false, titleSize: 13 });
      var ds = chart.data.datasets;
      ds[0].borderColor = ink;
      ds[1].backgroundColor = ds[2].backgroundColor = alpha(accent, 0.32);
      ds[3].borderColor = ds[4].borderColor = accent;
      ds[5].pointBackgroundColor = ds[5].pointHoverBackgroundColor = accent;
      ds[5].pointBorderColor = ds[5].pointHoverBorderColor = surface;
      chart.update();
    }

    function setCase(i) {
      C = CASES[i]; t = C.t;
      LO = C.mu - 4 * C.sd; HI = C.mu + 4 * C.sd; XS = [];
      for (var v = LO; v <= HI + 1e-9; v += (HI - LO) / 400) XS.push(v);
      var sx = chart.options.scales.x;
      sx.min = LO; sx.max = HI; sx.title.text = C.axis + " (" + C.unit + ")";
      chart.options.scales.y.max = pdf(C.mu, C.mu, C.sd) * 1.2;
      chart.data.datasets[0].data = XS.map(function (v) { return { x: v, y: pdf(v, C.mu, C.sd) }; });
      if (read.q) read.q.textContent = C.q;
      if (read.story) read.story.textContent = C.story || "";
      draw();
    }

    function region(a, b) {
      return XS.filter(function (v) { return v >= a && v <= b; }).map(function (v) { return { x: v, y: pdf(v, C.mu, C.sd) }; })
        .concat([{ x: b, y: pdf(b, C.mu, C.sd) }]);
    }

    function draw() {
      var ds = chart.data.datasets, top = pdf(C.mu, C.mu, C.sd) * 1.08, p, d = decs(C.step);
      ds[1].data = []; ds[2].data = []; ds[4].data = [];
      if (C.tail === "lower") { ds[1].data = region(LO, t); p = phi((t - C.mu) / C.sd); }
      else if (C.tail === "upper") { ds[1].data = [{ x: t, y: pdf(t, C.mu, C.sd) }].concat(region(t, HI)); p = 1 - phi((t - C.mu) / C.sd); }
      else {
        var w = Math.abs(t - C.mu), a = C.mu - w, b = C.mu + w;
        ds[1].data = region(LO, a); ds[2].data = [{ x: b, y: pdf(b, C.mu, C.sd) }].concat(region(b, HI));
        ds[4].data = [{ x: 2 * C.mu - t, y: 0 }, { x: 2 * C.mu - t, y: top }];
        p = 2 * (1 - phi(w / C.sd));
      }
      ds[3].data = [{ x: t, y: 0 }, { x: t, y: top }];
      ds[5].data = [{ x: t, y: top }];
      chart.update("none");
      if (read.model) read.model.textContent = "N(" + C.mu.toFixed(d) + ", " + C.sd.toFixed(d) + ") " + C.unit;
      if (read.t) read.t.textContent = (C.tail === "outside" ? "± " + Math.abs(t - C.mu).toFixed(d) : t.toFixed(d)) + " " + C.unit;
      if (read.p) read.p.textContent = (100 * p).toFixed(1) + "%";
    }

    var drag = false;
    function move(e) {
      var rc = canvas.getBoundingClientRect(), area = chart.chartArea;
      var px = Math.max(area.left, Math.min(area.right, e.clientX - rc.left));
      t = Math.max(LO, Math.min(HI, Math.round(chart.scales.x.getValueForPixel(px) / C.step) * C.step));
      t = +t.toFixed(4);
      draw();
    }
    canvas.style.touchAction = "none"; canvas.style.cursor = "ew-resize";
    Charts.listen(canvas, "pointerdown", function (e) { drag = true; canvas.setPointerCapture(e.pointerId); move(e); e.preventDefault(); });
    Charts.listen(canvas, "pointermove", function (e) { if (drag) move(e); });
    ["pointerup", "pointercancel"].forEach(function (k) { Charts.listen(canvas, k, function () { drag = false; }); });

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-c-case]");
      if (!b) return;
      root.querySelectorAll("[data-c-case]").forEach(function (x) { x.setAttribute("aria-pressed", x === b ? "true" : "false"); });
      setCase(Number(b.getAttribute("data-c-case")));
    });

    paint();
    setCase(0);
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-cases", init);
})();
