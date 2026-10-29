// Percentyle wzrostu (Ryc. 3.9): histogram z linią percentyla przeciąganą po wykresie
// (przyciąganie do całkowitych rzędów 0–100%), część danych na lewo od linii
// zacieniona; pod spodem wykres pudełkowy rysowany ręcznie z tym samym punktem.
// Kwantyl jak quantile(type = 7) w R. Kontener .lc-pctl[data-config] z
// <canvas data-role="hist">, <canvas data-role="box">, przyciskami [data-p-set]
// i odczytami [data-role="p|q|share|text"]. data-config: {values, p, bin, unit}.
(function () {
  var U = window.lc01;
  if (!U) return;

  // Pudełko, wąsy i punkty odstające (reguła 1.5 · IQR jak w ggplot2) + znacznik percentyla.
  var boxPlugin = {
    id: "lcBox",
    afterDatasetsDraw: function (chart, args, o) {
      if (!o || !o.stats) return;
      var ctx = chart.ctx, xs = chart.scales.x, ys = chart.scales.y, s = o.stats;
      var X = function (v) { return xs.getPixelForValue(v); };
      var yT = ys.getPixelForValue(0.42), yB = ys.getPixelForValue(-0.42), yM = ys.getPixelForValue(0);
      ctx.save();
      ctx.lineWidth = 1.5; ctx.strokeStyle = o.ink;
      ctx.beginPath();
      ctx.moveTo(X(s.lo), yM); ctx.lineTo(X(s.q1), yM);
      ctx.moveTo(X(s.q3), yM); ctx.lineTo(X(s.hi), yM);
      ctx.moveTo(X(s.lo), yM - 7); ctx.lineTo(X(s.lo), yM + 7);
      ctx.moveTo(X(s.hi), yM - 7); ctx.lineTo(X(s.hi), yM + 7);
      ctx.stroke();
      ctx.fillStyle = o.fill;
      ctx.fillRect(X(s.q1), yT, X(s.q3) - X(s.q1), yB - yT);
      ctx.strokeRect(X(s.q1), yT, X(s.q3) - X(s.q1), yB - yT);
      ctx.lineWidth = 2.5;
      ctx.beginPath(); ctx.moveTo(X(s.med), yT); ctx.lineTo(X(s.med), yB); ctx.stroke();
      ctx.fillStyle = o.outlier;
      s.out.forEach(function (v) { ctx.beginPath(); ctx.arc(X(v), yM, 3, 0, 2 * Math.PI); ctx.fill(); });
      // Znacznik percentyla: romb i podpis „P50” nad nim.
      if (o.mark != null) {
        var mx = X(o.mark), r = 8;
        ctx.fillStyle = o.accent; ctx.strokeStyle = o.surface; ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.moveTo(mx, yM - r); ctx.lineTo(mx + r, yM); ctx.lineTo(mx, yM + r); ctx.lineTo(mx - r, yM);
        ctx.closePath(); ctx.fill(); ctx.stroke();
        ctx.font = "700 12px " + o.font; ctx.textBaseline = "bottom";
        var w = ctx.measureText(o.markText).width, a = chart.chartArea;
        var tx = Math.max(a.left, Math.min(a.right - w, mx - w / 2));
        ctx.fillStyle = o.accent;
        ctx.fillText(o.markText, tx, yT - 3);
      }
      ctx.restore();
    }
  };

  function init(root) {
    var cfg = JSON.parse(root.dataset.config || "{}");
    var cHist = root.querySelector('canvas[data-role="hist"]');
    var cBox = root.querySelector('canvas[data-role="box"]');
    if (!cHist || !cBox) return;
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
    var p = cfg.p != null ? cfg.p : 50;
    var read = {};
    ["p", "q", "share", "text"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });
    var Y_W = 58;

    var chHist = new Chart(cHist, {
      type: "scatter",
      data: { datasets: [
        { data: [], showLine: true, pointRadius: 0, borderWidth: 2.5 },
        { data: [], pointRadius: 8, borderWidth: 2 }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 6, right: 12 } },
        scales: {
          x: { type: "linear", min: LO, max: HI, ticks: { stepSize: 5 } },
          y: { min: 0, max: TOP, ticks: { precision: 0 }, afterFit: U.fixedWidth(Y_W),
               title: { display: true, text: "Liczba studentów" } }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false },
                   lcHist: { bins: BINS }, lcLabels: { items: [] } }
      },
      plugins: [U.histPlugin, U.labelPlugin]
    });
    var chBox = new Chart(cBox, {
      type: "scatter",
      data: { datasets: [] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { right: 12 } },
        scales: {
          x: { type: "linear", min: LO, max: HI, ticks: { stepSize: 5 },
               title: { display: true, text: "Wzrost (" + UNIT + ")" } },
          y: { min: -1, max: 1.25, ticks: { display: false }, afterFit: U.fixedWidth(Y_W) }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, lcBox: { stats: box } }
      },
      plugins: [boxPlugin]
    });

    var P = null;
    function paint() {
      P = U.palette();
      U.styleAxes(chHist, P); U.styleAxes(chBox, P);
      chBox.options.scales.y.grid = { display: false };
      chBox.options.scales.y.border = { display: false };
      Object.assign(chHist.options.plugins.lcHist, { fill: U.alpha(P.ref, 0.45), fillLeft: U.alpha(P.niebo, 0.7), gap: P.surface });
      Object.assign(chHist.options.plugins.lcLabels, { font: P.mono, halo: U.alpha(P.surface, 0.85) });
      Object.assign(chBox.options.plugins.lcBox, { ink: P.soft, fill: U.alpha(P.ref, 0.25), outlier: U.alpha(P.soft, 0.5),
        accent: P.accent, surface: P.surface, font: P.mono });
      var ds = chHist.data.datasets;
      ds[0].borderColor = P.accent;
      ds[1].pointBackgroundColor = P.accent; ds[1].pointBorderColor = P.surface;
      chHist.update(); chBox.update();
    }

    function draw() {
      var q = Q[p], qr = U.round(q, 1);
      var share = 100 * S.filter(function (v) { return v <= qr; }).length / N;
      var hy = TOP * 0.9;
      chHist.data.datasets[0].data = [{ x: q, y: 0 }, { x: q, y: hy }];
      chHist.data.datasets[1].data = [{ x: q, y: hy }];
      chHist.options.plugins.lcHist.split = q;
      chHist.options.plugins.lcLabels.items = [{ x: q, text: U.fmt(q, 1) + " " + UNIT, color: P.accent, row: 0 }];
      chBox.options.plugins.lcBox.mark = q;
      chBox.options.plugins.lcBox.markText = "P" + p;
      chHist.update("none"); chBox.update("none");
      if (read.p) read.p.textContent = p + "%";
      if (read.q) read.q.textContent = U.fmt(q, 1) + " " + UNIT;
      if (read.share) read.share.textContent = U.fmt(share, 1) + "%";
      if (read.text) read.text.textContent = p + "% studentów ma wzrost poniżej " + U.fmt(qr, 1) + " " + UNIT +
        ". Dokładnie " + U.fmt(share, 1) + "% obserwacji ≤ " + U.fmt(qr, 1) + " " + UNIT + ".";
      root.querySelectorAll("[data-p-set]").forEach(function (b) {
        b.setAttribute("aria-pressed", String(+b.dataset.pSet === p));
      });
      root.lcState = { p: p, q: q, share: share };
    }

    // Rząd percentyla, którego kwantyl leży najbliżej wskazanej wartości.
    function nearestP(v) {
      var best = 0;
      for (var k = 1; k <= 100; k++) if (Math.abs(Q[k] - v) < Math.abs(Q[best] - v)) best = k;
      return best;
    }

    var drag = false;
    [[cHist, chHist], [cBox, chBox]].forEach(function (pair) {
      var canvas = pair[0], chart = pair[1];
      canvas.style.touchAction = "none";
      canvas.style.cursor = "ew-resize";
      canvas.addEventListener("pointerdown", function (e) {
        drag = true;
        canvas.setPointerCapture(e.pointerId);
        p = nearestP(U.valueAtX(chart, e)); draw();
        e.preventDefault();
      });
      canvas.addEventListener("pointermove", function (e) {
        if (!drag) return;
        var np = nearestP(U.valueAtX(chart, e));
        if (np !== p) { p = np; draw(); }
      });
      ["pointerup", "pointercancel"].forEach(function (t) {
        canvas.addEventListener(t, function () { drag = false; });
      });
    });

    root.addEventListener("click", function (e) {
      var b = e.target.closest("[data-p-set]");
      if (!b || !root.contains(b)) return;
      p = +b.dataset.pSet; draw();
    });

    paint();
    draw();
    U.onTheme(function () { paint(); draw(); });
  }

  U.onReady(".lc-pctl", init);
})();
