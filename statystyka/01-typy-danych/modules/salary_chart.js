// Pensje w firmie: histogram + pasek obserwacji ze średnią, medianą i (opcjonalnie)
// średnią uciętą liczonymi na żywo (Ryc. 3.4 i Ryc. 3.6). Kliknięcie na którymkolwiek
// wykresie dopisuje pensję w tym miejscu; punkt prezesa (Ryc. 3.4) przeciąga się po pasku.
// Kontener .lc-salary[data-config] z <div class="lc-ec" data-role="hist">,
// <div class="lc-ec" data-role="strip">, przyciskami [data-s-act="ceo|out1|out5|reset"]
// i odczytami [data-role=...]. data-config (JSON z R): {n, shape, scale, shift, trim,
// ceo, ceo_value, hi, hi_wide, bin, bin_wide, snap}. Wymaga chart_utils.js (window.lc01).
// ECharts: oba wykresy mają stałe marginesy siatki, więc osie x są wyrównane.
(function () {
  var Charts = window.lcCharts;
  var U = window.lc01;
  if (!U) return;

  function init(root) {
    var cfg = JSON.parse(root.dataset.config || "{}");
    var eHist = root.querySelector('.lc-ec[data-role="hist"]');
    var eStrip = root.querySelector('.lc-ec[data-role="strip"]');
    if (!eHist || !eStrip) return false;
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

    var hi = cfg.hi, dragging = null, yTop = 5;
    function axisMax() {
      var wide = cfg.hi_wide && items.some(function (it) { return it.v > cfg.hi; });
      return wide ? cfg.hi_wide : cfg.hi;
    }

    var layer = cfg.ceo ? Charts.handleLayer(root) : null, bossHandle = null;
    if (layer) bossHandle = layer.add(eStrip, {
      label: "Pensja prezesa",
      hint: "Strzałki zmieniają pensję; Shift zwiększa krok dziesięciokrotnie. Home i End: krańce zakresu.",
      onKey: function (event) {
        var boss = ceo(); if (!boss) return false;
        var value = Charts.keyValue(event, boss.v, SNAP, cfg.hi_wide || cfg.hi, SNAP, 10 * SNAP);
        if (value === null) return false;
        boss.v = value; refresh();
        layer.announce("Pensja prezesa " + Charts.fmt(boss.v, 0, true) + " zł. Średnia " + Charts.fmt(root.lcState.mean, 0, true) + ", mediana " + Charts.fmt(root.lcState.median, 0, true) + " zł.");
        return true;
      },
      onPointerDown: function (event) {
        if (!ceo()) return;
        dragging = ceo(); eStrip.setPointerCapture(event.pointerId); event.preventDefault();
      }
    });
    if (bossHandle) bossHandle.element.hidden = true;
    function positionBoss() {
      var boss = ceo(); if (!bossHandle || !boss) return;
      var q = chStrip.convertToPixel({ gridIndex: 0 }, [boss.v, boss.j]);
      if (q) bossHandle.position(q[0], q[1]);
    }

    var histStyle = { fill: "#ccc", gap: "#fff" };
    function vl(id, dash) {
      return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
               lineStyle: { width: 2.5, type: dash || "solid" } };
    }
    function money(v) { return Charts.fmt(v, 0, true); }
    var chHist = Charts.chart(eHist, {
      animation: false, aria: { enabled: false },
      grid: { left: 58, right: 20, top: 8, bottom: 28, outerBoundsMode: "none" },
      xAxis: { type: "value", min: 0, max: hi, axisLabel: { formatter: money } },
      yAxis: { type: "value", min: 0, max: 5, minInterval: 1, name: "Liczba osób" },
      series: [U.histSeries("hist", histStyle), vl("m"), vl("med", [7, 4]), vl("tr", [2, 3])]
    });
    var chStrip = Charts.chart(eStrip, {
      animation: false, aria: { enabled: false },
      grid: { left: 58, right: 20, top: 8, bottom: 48, outerBoundsMode: "none" },
      xAxis: { type: "value", min: 0, max: hi, axisLabel: { formatter: money }, name: "Zarobki (zł)" },
      yAxis: { type: "value", min: -1, max: 1, show: false },
      series: [
        { id: "kept", type: "scatter", data: [], symbolSize: 8, silent: true, animation: false },
        { id: "cut", type: "scatter", data: [], symbolSize: 8, silent: true, animation: false },
        { id: "boss", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false, clip: false },
        vl("m"), vl("med", [7, 4]), vl("tr", [2, 3])
      ]
    }, { onResize: positionBoss });

    var P = null;
    function paint() {
      P = Charts.palette();
      var fs = Charts.axisFontSize(root), left = Math.ceil(fs * 5), right = Math.ceil(fs * 2.6);  // miejsce na „60 000”
      var x = Charts.ecAxis(P, fs, { nameGap: Math.round(fs * 1.9), splitNumber: 6 });
      x.axisLabel.formatter = money;
      var y = Charts.ecAxis(P, fs, { nameGap: Math.round(fs * 2.6) });
      chHist.setOption({ grid: { left: left, right: right, bottom: Math.ceil(fs * 2) }, xAxis: x, yAxis: y });
      chStrip.setOption({ grid: { left: left, right: right, bottom: Math.ceil(fs * 3.6) }, xAxis: x });
      histStyle.fill = Charts.alpha(P.ref, 0.55); histStyle.gap = P.surface;
      var colors = [P.accent, P.niebo, P.szalwia], ids = ["m", "med", "tr"], upd = [{ id: "hist", renderItem: U.histSeries("hist", histStyle).renderItem }];
      ids.forEach(function (id, k) { upd.push({ id: id, lineStyle: { color: colors[k] } }); });
      chHist.setOption({ series: upd });
      var added = function (d) { return d.value[2]; };
      chStrip.setOption({ series: [
        { id: "kept", itemStyle: { color: function (d) { return added(d) ? Charts.alpha(P.terakota, 0.8) : Charts.alpha(P.secondary, 0.55); },
                                   borderColor: function (d) { return added(d) ? P.terakota : P.secondary; }, borderWidth: 1.5 } },
        { id: "cut", itemStyle: { color: "transparent",
                                  borderColor: function (d) { return added(d) ? P.terakota : P.subtle; }, borderWidth: 1.5 } },
        { id: "boss", itemStyle: { color: P.terakota, borderColor: P.surface, borderWidth: 2.5 } }
      ].concat(ids.map(function (id, k) { return { id: id, lineStyle: { color: colors[k] } }; })) });
    }

    function setAxis(max) {
      hi = max;
      var ax = { max: max, interval: max > 30000 ? 10000 : 5000 };
      chHist.setOption({ xAxis: ax }); chStrip.setOption({ xAxis: ax });
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
      yTop = Math.max(5, Math.ceil(top * 1.15 / 5) * 5);
      var stats = [m, med, tr], ids = ["m", "med", "tr"];
      chHist.setOption({ yAxis: { max: yTop }, series: [{ id: "hist", data: U.histData(b) }].concat(ids.map(function (id, k) {
        return { id: id, data: U.vline(stats[k], yTop) };
      })) });
      var kept = [], cutPts = [], boss = [];
      items.forEach(function (it, i) {
        var pt = [it.v, it.j, it.kind !== "base" ? 1 : 0];
        if (it.kind === "ceo") boss.push(pt);
        else if (trimmed[i]) cutPts.push(pt); else kept.push(pt);
      });
      chStrip.setOption({ series: [
        { id: "kept", data: kept }, { id: "cut", data: cutPts }, { id: "boss", data: boss }
      ].concat(ids.map(function (id, k) {
        return { id: id, data: isFinite(stats[k]) ? [[stats[k], -1], [stats[k], 1]] : [] };
      })) });

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
      if (bossHandle) {
        var bs = ceo(); bossHandle.element.hidden = !bs;
        if (bs) {
          bossHandle.value(bs.v, SNAP, cfg.hi_wide || cfg.hi, Charts.fmt(bs.v, 0, true) + " zł");
          positionBoss();
        }
      }
      root.lcState = { values: x, mean: m, median: med, trim: tr, axisMax: hi };
    }

    function refresh() { setAxis(axisMax()); draw(); }

    function snap(v) { return Math.max(SNAP, Math.min(hi, Math.round(v / SNAP) * SNAP)); }

    function nearCeo(chart, el, e) {
      var c = ceo(); if (!c || chart !== chStrip) return false;
      var r = el.getBoundingClientRect(), q = chStrip.convertToPixel({ gridIndex: 0 }, [c.v, c.j]);
      return Math.abs(e.clientX - r.left - q[0]) <= 14 && Math.abs(e.clientY - r.top - q[1]) <= 16;
    }

    function attach(el, chart) {
      el.style.touchAction = "none";
      el.style.cursor = "copy";
      function at(e) { return U.valueAt(chart, el, e, [0, hi]).x; }
      Charts.listen(el, "pointerdown", function (e) {
        var v = snap(at(e));
        if (nearCeo(chart, el, e)) {
          dragging = ceo();
          el.setPointerCapture(e.pointerId);
          el.style.cursor = "grabbing";
        } else {
          items.push({ v: v, j: jitter(), kind: "add" });
          refresh();
        }
        e.preventDefault();
      });
      Charts.listen(el, "pointermove", function (e) {
        if (dragging) {
          dragging.v = snap(at(e));
          draw();
        } else if (chart === chStrip) {
          el.style.cursor = nearCeo(chart, el, e) ? "grab" : "copy";
        }
      });
      ["pointerup", "pointercancel"].forEach(function (t) {
        Charts.listen(el, t, function () {
          if (!dragging) return;
          dragging = null;
          el.style.cursor = "copy";
          refresh();
        });
      });
    }
    attach(eHist, chHist);
    attach(eStrip, chStrip);

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

    Charts.zoomControl(root);
    generate();
    paint();
    refresh();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-salary", init);
})();
