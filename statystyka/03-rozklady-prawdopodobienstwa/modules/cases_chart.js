// Galeria przypadków z rozkładem normalnym (prototyp 5a): przyciski przypadków,
// gęstość N(μ, σ), próg przeciągany po wykresie i zacieniony ogon.
// data-config (JSON z R): [{short, story, q, axis, unit, mu, sd, t, tail, step}], tail = "lower" | "upper"
// | "outside" (oba ogony dalej niż |t − μ| od μ). Kontener .lc-cases z <div class="lc-ec" data-role="pdf">,
// przyciskami [data-c-case] i odczytami [data-role="model|t|p|q"]. ECharts, wzorzec jak area_chart.js.
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;
  var phi = Charts.normalCdf;

  function pdf(v, mu, sd) { var z = (v - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }
  function decs(step) { return step >= 1 ? 0 : step >= 0.1 ? 1 : 2; }

  function line(id, width, dash) {
    return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
             lineStyle: { width: width, type: dash ? [5, 4] : "solid" } };
  }

  function init(root) {
    var el = root.querySelector('.lc-ec[data-role="pdf"]');
    if (!el) return false;
    var CASES = JSON.parse(root.dataset.config || "[]"), C = null, t = 0, LO = 0, HI = 1, XS = [];
    var read = {};
    ["model", "t", "p", "q", "story"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });

    var drag = false;
    var layer = Charts.handleLayer(root), lastP = 0;
    var handle = layer.add(el, {
      label: "Próg",
      hint: "Strzałki przesuwają próg, Shift zwiększa krok dziesięciokrotnie. Home i End wybierają granice osi.",
      onKey: function (event) {
        var value = Charts.keyValue(event, t, LO, HI, C.step, 10 * C.step);
        if (value === null) return false;
        t = +value.toFixed(4); draw();
        layer.announce(t.toFixed(decs(C.step)) + " " + C.unit + ". Prawdopodobieństwo " + (100 * lastP).toFixed(1) + "%"); return true;
      },
      onPointerDown: function (event) { drag = true; el.setPointerCapture(event.pointerId); event.preventDefault(); }
    });
    function syncHandle() {
      if (!C) return;
      var q = chart.convertToPixel({ gridIndex: 0 }, [t, pdf(C.mu, C.mu, C.sd) * 1.08]);
      if (q) handle.position(q[0], q[1]);
      handle.element.setAttribute("aria-label", (C.tail === "outside" ? "Granica" : "Próg") + ": " + C.axis);
      handle.value(t, LO, HI, t.toFixed(decs(C.step)) + " " + C.unit);
    }
    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 24, right: 24, top: 10, bottom: 8, outerBoundsMode: "none" },
      xAxis: { type: "value", name: "" },
      yAxis: { type: "value", min: 0, show: false },
      series: [
        Object.assign(line("lower", 0), { areaStyle: {} }),
        Object.assign(line("upper", 0), { areaStyle: {} }),
        line("pdf", 2),
        line("mark", 2, true),
        line("mirror", 2, true),
        { id: "dot", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false }
      ]
    }, { onResize: function () { layout(); syncHandle(); } });

    // Margines i nazwa osi zależą od szerokości i kroju (lupa, S/M/L, telefon).
    function layout() {
      if (!C) return;
      var size = Charts.axisFontSize(root), width = chart.getWidth ? chart.getWidth() : 640;
      var lines = Charts.axisTitle(C.axis + " (" + C.unit + ")", width, size).length;
      var pad = Math.ceil(size * 1.6);
      chart.setOption({
        grid: { left: pad, right: pad, bottom: Math.ceil(size * (2.4 + 1.3 * lines)) },
        xAxis: { name: Charts.ecName(C.axis + " (" + C.unit + ")", width, size),
                 splitNumber: Math.max(3, Math.min(6, Math.floor(width / (size * 4.2)))) }
      });
    }

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var x = Charts.ecAxis(P, size, { nameGap: Math.round(size * 1.9), nameLocation: "middle", nameTextStyle: {
        color: P.subtle, fontFamily: P.mono, fontSize: size, verticalAlign: "top", lineHeight: size * 1.3 } });
      x.axisLabel.showMinLabel = x.axisLabel.showMaxLabel = false;
      chart.setOption({
        xAxis: x,
        series: [
          { id: "lower", lineStyle: { width: 0 }, areaStyle: { color: alpha(P.accent, 0.32) } },
          { id: "upper", lineStyle: { width: 0 }, areaStyle: { color: alpha(P.accent, 0.32) } },
          { id: "pdf", lineStyle: { color: P.soft } },
          { id: "mark", lineStyle: { color: P.accent } },
          { id: "mirror", lineStyle: { color: P.accent } },
          { id: "dot", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } }
        ]
      });
      layout();
      syncHandle();
    }

    function setCase(i) {
      C = CASES[i]; t = C.t;
      LO = C.mu - 4 * C.sd; HI = C.mu + 4 * C.sd; XS = [];
      for (var v = LO; v <= HI + 1e-9; v += (HI - LO) / 400) XS.push(v);
      chart.setOption({
        xAxis: { min: LO, max: HI },
        yAxis: { max: pdf(C.mu, C.mu, C.sd) * 1.2 },
        series: [{ id: "pdf", data: XS.map(function (v) { return [v, pdf(v, C.mu, C.sd)]; }) }]
      });
      layout();
      if (read.q) read.q.textContent = C.q;
      if (read.story) read.story.textContent = C.story || "";
      draw();
    }

    function region(a, b) {
      return XS.filter(function (v) { return v >= a && v <= b; }).map(function (v) { return [v, pdf(v, C.mu, C.sd)]; })
        .concat([[b, pdf(b, C.mu, C.sd)]]);
    }

    function draw() {
      var top = pdf(C.mu, C.mu, C.sd) * 1.08, p, d = decs(C.step);
      var lower = [], upper = [], mirror = [];
      if (C.tail === "lower") { lower = region(LO, t); p = phi((t - C.mu) / C.sd); }
      else if (C.tail === "upper") { lower = [[t, pdf(t, C.mu, C.sd)]].concat(region(t, HI)); p = 1 - phi((t - C.mu) / C.sd); }
      else {
        var w = Math.abs(t - C.mu), a = C.mu - w, b = C.mu + w;
        lower = region(LO, a); upper = [[b, pdf(b, C.mu, C.sd)]].concat(region(b, HI));
        mirror = [[2 * C.mu - t, 0], [2 * C.mu - t, top]];
        p = 2 * (1 - phi(w / C.sd));
      }
      lastP = p;
      chart.setOption({ series: [
        { id: "lower", data: lower }, { id: "upper", data: upper }, { id: "mirror", data: mirror },
        { id: "mark", data: [[t, 0], [t, top]] }, { id: "dot", data: [[t, top]] }
      ] });
      syncHandle();
      if (read.model) read.model.textContent = "N(" + C.mu.toFixed(d) + ", " + C.sd.toFixed(d) + ") " + C.unit;
      if (read.t) read.t.textContent = (C.tail === "outside" ? "± " + Math.abs(t - C.mu).toFixed(d) : t.toFixed(d)) + " " + C.unit;
      if (read.p) read.p.textContent = (100 * p).toFixed(1) + "%";
    }

    function move(e) {
      var v = Charts.ecPointer(chart, el, e, [LO, HI]).x;
      t = Math.max(LO, Math.min(HI, Math.round(v / C.step) * C.step));
      t = +t.toFixed(4);
      draw();
    }
    el.style.touchAction = "none"; el.style.cursor = "ew-resize";
    Charts.listen(el, "pointerdown", function (e) { drag = true; el.setPointerCapture(e.pointerId); move(e); e.preventDefault(); });
    Charts.listen(el, "pointermove", function (e) { if (drag) move(e); });
    ["pointerup", "pointercancel"].forEach(function (k) { Charts.listen(el, k, function () { drag = false; }); });

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-c-case]");
      if (!b) return;
      root.querySelectorAll("[data-c-case]").forEach(function (x) { x.setAttribute("aria-pressed", x === b ? "true" : "false"); });
      setCase(Number(b.getAttribute("data-c-case")));
    });

    Charts.zoomControl(root);
    paint();
    setCase(0);
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-cases", init);
})();
