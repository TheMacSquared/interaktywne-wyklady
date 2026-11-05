// Percentyle wzrostu (Ryc. 3.9): histogram z linią percentyla przeciąganą po wykresie
// (przyciąganie do całkowitych rzędów 0–100%), część danych na lewo od linii
// zacieniona; pod spodem wykres pudełkowy z tym samym punktem (seria custom).
// Kwantyl jak quantile(type = 7) w R. Kontener .lc-pctl[data-config] z
// <div class="lc-ec" data-role="hist">, <div class="lc-ec" data-role="box">,
// przyciskami [data-p-set] i odczytami [data-role="p|q|share|text"].
// data-config: {values, p, bin, unit}. ECharts, wymaga chart_utils.js (window.lc01).
(function () {
  var Charts = window.lcCharts;
  var U = window.lc01;
  if (!U) return;

  // Pudełko, wąsy i punkty odstające (reguła 1.5 · IQR jak w ggplot2) + znacznik
  // percentyla (romb i podpis „P50”). Styl i znacznik w obiekcie o.
  function boxRender(o) {
    return function (params, api) {
      var s = o.stats;
      function X(v) { return api.coord([v, 0])[0]; }
      var yT = api.coord([0, 0.42])[1], yB = api.coord([0, -0.42])[1], yM = api.coord([0, 0])[1];
      var line = { stroke: o.ink, lineWidth: 1.5 };
      var kids = [
        { type: "line", shape: { x1: X(s.lo), y1: yM, x2: X(s.q1), y2: yM }, style: line },
        { type: "line", shape: { x1: X(s.q3), y1: yM, x2: X(s.hi), y2: yM }, style: line },
        { type: "line", shape: { x1: X(s.lo), y1: yM - 7, x2: X(s.lo), y2: yM + 7 }, style: line },
        { type: "line", shape: { x1: X(s.hi), y1: yM - 7, x2: X(s.hi), y2: yM + 7 }, style: line },
        { type: "rect", shape: { x: X(s.q1), y: yT, width: X(s.q3) - X(s.q1), height: yB - yT },
          style: { fill: o.fill, stroke: o.ink, lineWidth: 1.5 } },
        { type: "line", shape: { x1: X(s.med), y1: yT, x2: X(s.med), y2: yB }, style: { stroke: o.ink, lineWidth: 2.5 } }
      ];
      s.out.forEach(function (v) { kids.push({ type: "circle", shape: { cx: X(v), cy: yM, r: 3 }, style: { fill: o.outlier } }); });
      if (o.mark != null) {
        var mx = X(o.mark), r = 8, size = o.fontSize || 12;
        var left = X(o.lo), right = X(o.hi), w = o.markText.length * size * 0.62;
        kids.push({ type: "polygon", shape: { points: [[mx, yM - r], [mx + r, yM], [mx, yM + r], [mx - r, yM]] },
                    style: { fill: o.accent, stroke: o.surface, lineWidth: 2 } });
        kids.push({ type: "text", x: Math.max(left, Math.min(right - w, mx - w / 2)), y: yT - 3,
                    style: { text: o.markText, fill: o.accent, font: "700 " + size + "px " + o.font, textVerticalAlign: "bottom" } });
      }
      return { type: "group", children: kids };
    };
  }

  function init(root) {
    var cfg = JSON.parse(root.dataset.config || "{}");
    var eHist = root.querySelector('.lc-ec[data-role="hist"]');
    var eBox = root.querySelector('.lc-ec[data-role="box"]');
    if (!eHist || !eBox) return false;
    var S = U.sorted(cfg.values), N = S.length, BIN = cfg.bin || 2, UNIT = cfg.unit || "cm";
    var LO = Math.floor(S[0] / 5) * 5, HI = Math.ceil(S[N - 1] / 5) * 5;
    var Q = []; for (var i = 0; i <= 100; i++) Q.push(U.quantile7(S, i / 100, true));
    var q1 = Q[25], med = Q[50], q3 = Q[75], iqr = q3 - q1;
    var inLo = S.filter(function (v) { return v >= q1 - 1.5 * iqr; })[0];
    var inHi = S.filter(function (v) { return v <= q3 + 1.5 * iqr; }).pop();
    var box = { q1: q1, med: med, q3: q3, lo: inLo, hi: inHi,
                out: S.filter(function (v) { return v < inLo || v > inHi; }) };
    var BINS = U.bins(S, LO, HI, BIN), TOP = 0;
    BINS.forEach(function (b) { TOP = Math.max(TOP, b.n); });
    TOP = Math.ceil(TOP * 1.25 / 5) * 5;
    var p = cfg.p != null ? cfg.p : 50, drag = false;
    var read = {};
    ["p", "q", "share", "text"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });
    var layer = Charts.handleLayer(root);
    var handle = layer.add(eHist, {
      label: "Rząd percentyla",
      hint: "Strzałki zmieniają rząd percentyla o 1 punkt procentowy; Shift ze strzałką o 10. Home: 0, End: 100.",
      onKey: function (event) {
        var value = Charts.keyValue(event, p, 0, 100, 1, 10);
        if (value === null) return false;
        p = value; draw();
        layer.announce("Percentyl " + p + ". Wzrost " + Charts.fmt(Q[p], 1) + " " + UNIT + ".");
        return true;
      },
      onPointerDown: function (event) {
        drag = true; eHist.setPointerCapture(event.pointerId);
        p = nearestP(U.valueAt(chHist, eHist, event, [LO, HI]).x); draw(); event.preventDefault();
      }
    });
    function positionHandle() {
      var q = chHist.convertToPixel({ gridIndex: 0 }, [Q[p], TOP * 0.9]);
      if (q) handle.position(q[0], q[1]);
    }

    var histStyle = { bins: BINS, fill: "#ccc", fillLeft: "#88a", gap: "#fff", split: null };
    var boxStyle = { stats: box, lo: LO, hi: HI };
    var chHist = Charts.chart(eHist, {
      animation: false, aria: { enabled: false },
      grid: { left: 58, right: 12, top: 6, bottom: 28, outerBoundsMode: "none" },
      xAxis: { type: "value", min: LO, max: HI, interval: 5 },
      yAxis: { type: "value", min: 0, max: TOP, minInterval: 1, name: "Liczba studentów" },
      series: [
        Object.assign(U.histSeries("hist", histStyle), { data: U.histData(BINS) }),
        { id: "line", type: "line", data: [], symbol: "none", silent: true, animation: false, lineStyle: { width: 2.5 } },
        { id: "dot", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false }
      ]
    }, { onResize: function () { draw(); } });
    var chBox = Charts.chart(eBox, {
      animation: false, aria: { enabled: false },
      grid: { left: 58, right: 12, top: 4, bottom: 48, outerBoundsMode: "none" },
      xAxis: { type: "value", min: LO, max: HI, interval: 5, name: "Wzrost (" + UNIT + ")" },
      yAxis: { type: "value", min: -1, max: 1.25, show: false },
      series: [{ id: "box", type: "custom", renderItem: boxRender(boxStyle), data: [[0]], silent: true, animation: false, clip: false }]
    });

    var P = null, FS = 12;
    function paint() {
      P = Charts.palette(); FS = Charts.axisFontSize(root);
      var left = Math.ceil(FS * 5), x = Charts.ecAxis(P, FS, { nameGap: Math.round(FS * 1.9) });
      chHist.setOption({ grid: { left: left, bottom: Math.ceil(FS * 2) }, xAxis: x,
                         yAxis: Charts.ecAxis(P, FS, { nameGap: Math.round(FS * 2.6) }) });
      chBox.setOption({ grid: { left: left, bottom: Math.ceil(FS * 3.6) }, xAxis: x });
      Object.assign(histStyle, { fill: Charts.alpha(P.ref, 0.45), fillLeft: Charts.alpha(P.niebo, 0.7), gap: P.surface });
      Object.assign(boxStyle, { ink: P.soft, fill: Charts.alpha(P.ref, 0.25), outlier: Charts.alpha(P.soft, 0.5),
        accent: P.accent, surface: P.surface, font: P.mono, fontSize: FS });
      chHist.setOption({ series: [
        { id: "line", lineStyle: { color: P.accent } },
        { id: "dot", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } }
      ] });
    }

    function draw() {
      var q = Q[p], qr = Charts.round(q, 1);
      var share = 100 * S.filter(function (v) { return v <= qr; }).length / N;
      var hy = TOP * 0.9;
      histStyle.split = q;
      boxStyle.mark = q; boxStyle.markText = "P" + p;
      chHist.setOption({ series: [
        { id: "hist", renderItem: U.histSeries("hist", histStyle).renderItem },
        { id: "line", data: [[q, 0], [q, hy]] }, { id: "dot", data: [[q, hy]] }
      ] });
      chBox.setOption({ series: [{ id: "box", renderItem: boxRender(boxStyle) }] });
      U.lineLabels(chHist, [{ x: q, text: Charts.fmt(q, 1) + " " + UNIT, color: P.accent, row: 0 }],
        { xmax: HI, ymax: TOP, font: P.mono, fontSize: FS, halo: Charts.alpha(P.surface, 0.85) });
      if (read.p) read.p.textContent = p + "%";
      if (read.q) read.q.textContent = Charts.fmt(q, 1) + " " + UNIT;
      if (read.share) read.share.textContent = Charts.fmt(share, 1) + "%";
      if (read.text) read.text.textContent = p + "% studentów ma wzrost poniżej " + Charts.fmt(qr, 1) + " " + UNIT +
        ". Dokładnie " + Charts.fmt(share, 1) + "% obserwacji ≤ " + Charts.fmt(qr, 1) + " " + UNIT + ".";
      root.querySelectorAll("[data-p-set]").forEach(function (b) {
        b.setAttribute("aria-pressed", String(+b.dataset.pSet === p));
      });
      handle.value(p, 0, 100, "Percentyl " + p + "; " + Charts.fmt(q, 1) + " " + UNIT);
      positionHandle();
      root.lcState = { p: p, q: q, share: share };
    }

    // Rząd percentyla, którego kwantyl leży najbliżej wskazanej wartości.
    function nearestP(v) {
      var best = 0;
      for (var k = 1; k <= 100; k++) if (Math.abs(Q[k] - v) < Math.abs(Q[best] - v)) best = k;
      return best;
    }

    [[eHist, chHist], [eBox, chBox]].forEach(function (pair) {
      var el = pair[0], chart = pair[1];
      el.style.touchAction = "none";
      el.style.cursor = "ew-resize";
      Charts.listen(el, "pointerdown", function (e) {
        drag = true;
        el.setPointerCapture(e.pointerId);
        p = nearestP(U.valueAt(chart, el, e, [LO, HI]).x); draw();
        e.preventDefault();
      });
      Charts.listen(el, "pointermove", function (e) {
        if (!drag) return;
        var np = nearestP(U.valueAt(chart, el, e, [LO, HI]).x);
        if (np !== p) { p = np; draw(); }
      });
      ["pointerup", "pointercancel"].forEach(function (t) {
        Charts.listen(el, t, function () { drag = false; });
      });
    });

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-p-set]");
      if (!b || !root.contains(b)) return;
      p = +b.dataset.pSet; draw();
    });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-pctl", init);
})();
