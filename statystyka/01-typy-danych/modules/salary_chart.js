// Pensje w firmie: histogram + pasek obserwacji ze średnią, medianą i (opcjonalnie)
// średnią uciętą liczonymi na żywo (Ryc. 3.4 i Ryc. 3.6). Kliknięcie na którymkolwiek
// wykresie dopisuje pensję w tym miejscu; punkt prezesa (Ryc. 3.4) przeciąga się po pasku.
// Kontener .lc-salary[data-config] z <canvas data-role="hist">, <canvas data-role="strip">,
// przyciskami [data-s-act="ceo|out1|out5|reset"] i odczytami [data-role=...].
// data-config (JSON z R): {n, shape, scale, shift, trim, ceo, ceo_value, hi, hi_wide,
// bin, bin_wide, snap}. Wymaga chart_utils.js (window.lc01).
(function () {
  var Charts = window.lcCharts;
  var U = window.lc01;
  if (!U) return;

  function init(root) {
    var cfg = JSON.parse(root.dataset.config || "{}");
    var cHist = root.querySelector('canvas[data-role="hist"]');
    var cStrip = root.querySelector('canvas[data-role="strip"]');
    if (!cHist || !cStrip) return false;
    var TRIM = cfg.trim || 0, SNAP = cfg.snap || 100;
    var read = {};
    ["mean", "median", "diff", "n", "trim", "count",
     "t-mean", "t-median", "t-trim", "d-mean", "d-median", "d-trim"].forEach(function (k) {
      read[k] = root.querySelector('[data-role="' + k + '"]');
    });
    var btn = {};
    root.querySelectorAll("[data-s-act]").forEach(function (b) { btn[b.dataset.sAct] = b; });

    // Obserwacje: {v, j (rozrzut pionowy na pasku), kind: "base" | "add" | "ceo"}.
    var items = [], baseStats = null;
    function jitter() { return U.runif(-0.55, 0.55); }
    function generate() {
      items = [];
      for (var i = 0; i < cfg.n; i++) {
        items.push({ v: Math.round(U.rgammaInt(cfg.shape, cfg.scale) + cfg.shift), j: jitter(), kind: "base" });
      }
      var b = items.map(function (it) { return it.v; });
      baseStats = { mean: U.mean(b), median: U.median(b), trim: TRIM ? U.trimMean(b, TRIM) : NaN };
    }
    function values() { return items.map(function (it) { return it.v; }); }
    function ceo() { for (var i = 0; i < items.length; i++) if (items[i].kind === "ceo") return items[i]; return null; }

    var hi = cfg.hi, dragging = null;
    function axisMax() {
      var wide = cfg.hi_wide && items.some(function (it) { return it.v > cfg.hi; });
      return wide ? cfg.hi_wide : cfg.hi;
    }

    var Y_W = 58;
    function lineDs(dash, width) {
      return { type: "scatter", data: [], showLine: true, pointRadius: 0, borderWidth: width, borderDash: dash };
    }
    var chHist = Charts.create(cHist, {
      type: "scatter",
      data: { datasets: [lineDs([], 2.5), lineDs([7, 4], 2.5), lineDs([2, 3], 2.5)] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 8, right: 12 } },
        scales: {
          x: { type: "linear", min: 0, max: hi, ticks: { callback: function (v) { return Charts.fmt(v, 0, true); } } },
          y: { min: 0, max: 5, ticks: { precision: 0 }, afterFit: U.fixedWidth(Y_W),
               title: { display: true, text: "Liczba osób" } }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } }
      },
      plugins: [U.histPlugin]
    });
    var chStrip = Charts.create(cStrip, {
      type: "scatter",
      data: { datasets: [
        { data: [], pointRadius: 4, borderWidth: 1.5 },
        { data: [], pointRadius: 4, borderWidth: 1.5 },
        { data: [], pointRadius: 8, borderWidth: 2.5 },
        lineDs([], 2.5), lineDs([7, 4], 2.5), lineDs([2, 3], 2.5)
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { right: 12 } },
        scales: {
          x: { type: "linear", min: 0, max: hi, ticks: { callback: function (v) { return Charts.fmt(v, 0, true); } },
               title: { display: true, text: "Zarobki (zł)" } },
          y: { min: -1, max: 1, ticks: { display: false }, grid: { display: false }, afterFit: U.fixedWidth(Y_W) }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } }
      }
    });

    var P = null;
    function paint() {
      P = Charts.palette();
      [chHist, chStrip].forEach(function (c) { Charts.styleAxes(c, P, { maxRotation: 0 }); });
      chStrip.options.scales.y.grid = { display: false };
      chStrip.options.scales.y.border = { display: false };
      chHist.options.plugins.lcHist = Object.assign(chHist.options.plugins.lcHist || {},
        { fill: Charts.alpha(P.ref, 0.55), gap: P.surface });
      var colors = [P.accent, P.niebo, P.szalwia];
      for (var k = 0; k < 3; k++) {
        chHist.data.datasets[k].borderColor = colors[k];
        chStrip.data.datasets[3 + k].borderColor = colors[k];
      }
      var s = chStrip.data.datasets;
      s[0].pointBackgroundColor = function (c) { return c.raw && c.raw.added ? Charts.alpha(P.terakota, 0.8) : Charts.alpha(P.secondary, 0.55); };
      s[0].pointBorderColor = function (c) { return c.raw && c.raw.added ? P.terakota : P.secondary; };
      s[1].pointBackgroundColor = "transparent";
      s[1].pointBorderColor = function (c) { return c.raw && c.raw.added ? P.terakota : P.subtle; };
      s[2].pointBackgroundColor = P.terakota;
      s[2].pointBorderColor = P.surface;
      chHist.update(); chStrip.update();
    }

    function setAxis(max) {
      hi = max;
      [chHist, chStrip].forEach(function (c) {
        c.options.scales.x.max = max;
        c.options.scales.x.ticks.stepSize = max > 30000 ? 10000 : 5000;
      });
    }

    function draw() {
      var x = values(), n = x.length;
      var m = U.mean(x), med = U.median(x), tr = TRIM ? U.trimMean(x, TRIM) : NaN;
      // Które obserwacje odpadają przy średniej uciętej (po tyle samo z każdej strony).
      var cut = TRIM ? Math.floor(n * TRIM) : 0;
      var order = items.map(function (it, i) { return i; }).sort(function (a, b) { return items[a].v - items[b].v; });
      var trimmed = {};
      for (var q = 0; q < cut; q++) { trimmed[order[q]] = true; trimmed[order[n - 1 - q]] = true; }

      var bw = hi > cfg.hi ? (cfg.bin_wide || cfg.bin) : cfg.bin;
      var b = U.bins(x, 0, hi, bw), top = 0;
      b.forEach(function (bb) { top = Math.max(top, bb.n); });
      chHist.options.scales.y.max = Math.max(5, Math.ceil(top * 1.15 / 5) * 5);
      chHist.options.plugins.lcHist.bins = b;
      var yTop = chHist.options.scales.y.max;
      var stats = [m, med, tr];
      for (var k = 0; k < 3; k++) {
        var on = isFinite(stats[k]);
        chHist.data.datasets[k].data = on ? [{ x: stats[k], y: 0 }, { x: stats[k], y: yTop }] : [];
        chStrip.data.datasets[3 + k].data = on ? [{ x: stats[k], y: -1 }, { x: stats[k], y: 1 }] : [];
      }
      var kept = [], cutPts = [], boss = [];
      items.forEach(function (it, i) {
        var pt = { x: it.v, y: it.j, added: it.kind !== "base" };
        if (it.kind === "ceo") boss.push(pt);
        else if (trimmed[i]) cutPts.push(pt); else kept.push(pt);
      });
      chStrip.data.datasets[0].data = kept;
      chStrip.data.datasets[1].data = cutPts;
      chStrip.data.datasets[2].data = boss;
      chHist.update("none"); chStrip.update("none");

      function set(k, t) { if (read[k]) read[k].textContent = t; }
      set("mean", Charts.fmt(m, 0, true) + " zł");
      set("median", Charts.fmt(med, 0, true) + " zł");
      set("trim", Charts.fmt(tr, 0, true) + " zł");
      set("n", String(n));
      var d = m - med;
      set("diff", Charts.fmt(d, 0, true) + " zł");
      if (read.diff) read.diff.closest(".lc-read").style.setProperty("--lc-read-color",
        Math.abs(d) < 500 ? "var(--upwr-cat-szalwia)" : "var(--upwr-cat-bursztyn)");
      set("count", String(items.filter(function (it) { return it.kind !== "base"; }).length));
      set("t-mean", Charts.fmt(m, 0, true)); set("t-median", Charts.fmt(med, 0, true)); set("t-trim", Charts.fmt(tr, 0, true));
      if (baseStats) {
        set("d-mean", Charts.fmtSigned(m - baseStats.mean, 0, true));
        set("d-median", Charts.fmtSigned(med - baseStats.median, 0, true));
        set("d-trim", Charts.fmtSigned(tr - baseStats.trim, 0, true));
      }
      if (btn.ceo) btn.ceo.disabled = !!ceo();
      root.lcState = { values: x, mean: m, median: med, trim: tr, axisMax: hi };
    }

    function refresh() { setAxis(axisMax()); draw(); }

    function snap(v) { return Math.max(SNAP, Math.min(hi, Math.round(v / SNAP) * SNAP)); }

    function nearCeo(chart, e) {
      var c = ceo(); if (!c || chart !== chStrip) return false;
      var r = cStrip.getBoundingClientRect();
      var dx = e.clientX - r.left - chStrip.scales.x.getPixelForValue(c.v);
      var dy = e.clientY - r.top - chStrip.scales.y.getPixelForValue(c.j);
      return Math.abs(dx) <= 14 && Math.abs(dy) <= 16;
    }

    function attach(canvas, chart) {
      canvas.style.touchAction = "none";
      canvas.style.cursor = "copy";
      Charts.listen(canvas, "pointerdown", function (e) {
        var v = snap(U.valueAtX(chart, e));
        if (nearCeo(chart, e)) {
          dragging = ceo();
          canvas.setPointerCapture(e.pointerId);
          canvas.style.cursor = "grabbing";
        } else {
          items.push({ v: v, j: jitter(), kind: "add" });
          refresh();
        }
        e.preventDefault();
      });
      Charts.listen(canvas, "pointermove", function (e) {
        if (dragging) {
          dragging.v = snap(U.valueAtX(chart, e));
          draw();
        } else if (chart === chStrip) {
          canvas.style.cursor = nearCeo(chart, e) ? "grab" : "copy";
        }
      });
      ["pointerup", "pointercancel"].forEach(function (t) {
        Charts.listen(canvas, t, function () {
          if (!dragging) return;
          dragging = null;
          canvas.style.cursor = "copy";
          refresh();
        });
      });
    }
    attach(cHist, chHist);
    attach(cStrip, chStrip);

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-s-act]");
      if (!b || !root.contains(b)) return;
      var act = b.dataset.sAct;
      if (act === "ceo" && !ceo()) items.push({ v: cfg.ceo_value || 50000, j: 0, kind: "ceo" });
      if (act === "out1" || act === "out5") {
        for (var i = 0; i < (act === "out5" ? 5 : 1); i++) {
          items.push({ v: Math.round(50000 + U.runif(-5000, 5000)), j: jitter(), kind: "add" });
        }
      }
      if (act === "reset") generate();
      refresh();
    });

    generate();
    paint();
    refresh();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-salary", init);
})();
