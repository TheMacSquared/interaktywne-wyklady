// Prototypy E1–E2 (2026-11-05): możliwości ECharts w drugiej części wykładu.
// Dane liczy R (data-config w stop_proto_ec.R); tu tylko rysowanie i interakcja.
//
// Ryc. 6.5 „Mapa zbieżności” (.lc-ecmap): heatmapa gęstości średniej (wiersz = n,
//   kolor = gęstość względem maksimum wiersza, visualMap) i pod nią histogram
//   2000 wylosowanych średnich dla wybranego n (seria custom) z dzwonem CTG.
//   Wiersz wybiera kursor (zdarzenia ECharts) albo uchwyt klawiaturowy ↑/↓.
// E2 „Zaznacz przedział” (.lc-brushx): histogram schodkowy masy jabłek
//   z krzywą normalną. Przedział zaznacza się myszą (komponent brush, lineX)
//   albo uchwytami a/b; dataZoom (suwak, kółko) przybliża ogony.
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha, fmt = Charts.fmt, phi = Charts.normalCdf;

  function npdf(x, mu, sd) { var z = (x - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }
  function press(root, attr, btn) {
    root.querySelectorAll("[" + attr + "]").forEach(function (b) { b.setAttribute("aria-pressed", b === btn ? "true" : "false"); });
  }

  // ==========================================================================
  // E1 — MAPA ZBIEŻNOŚCI
  // ==========================================================================
  Charts.onReady(".lc-ecmap", function (root) {
    var el = root.querySelector('.lc-ec[data-role="map"]');
    if (!el) return false;
    var BASES = JSON.parse(root.dataset.config || "[]"), B = null, n = 1, CELLS = 0, NROW = 0;
    var read = {};
    ["n", "skew", "tv", "story"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });

    var layer = Charts.handleLayer(root);
    var handle = layer.add(el, {
      label: "Liczba obserwacji n w średniej",
      orientation: "vertical",
      hint: "Strzałki w górę i w dół zmieniają n o 1, Shift o 5. Home i End: n = 1 i n = 40.",
      onKey: function (event) {
        var v = event.key === "Home" ? 1 : event.key === "End" ? NROW :
          event.key === "ArrowDown" || event.key === "ArrowRight" ? n + (event.shiftKey ? 5 : 1) :
          event.key === "ArrowUp" || event.key === "ArrowLeft" ? n - (event.shiftKey ? 5 : 1) : null;
        if (v === null) return false;
        setN(v);
        layer.announce("n = " + n + ". Rozbieżność z dzwonem " + fmt(100 * B.tv[n - 1], 1) + "%.");
        return true;
      }
    });

    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: [
        { left: 8, right: 70, top: 10, height: "52%" },
        { left: 8, right: 70, top: "66%", bottom: 8 }
      ],
      xAxis: [
        { type: "category", gridIndex: 0, data: [], axisLabel: { show: false }, axisTick: { show: false },
          splitLine: { show: false } },
        { type: "value", gridIndex: 1, name: "" }
      ],
      yAxis: [
        { type: "category", gridIndex: 0, inverse: true, data: [], name: "n" },
        { type: "value", gridIndex: 1, min: 0, name: "Liczba średnich" }
      ],
      visualMap: { type: "continuous", seriesIndex: 0, min: 0, max: 1, calculable: true,
                   orient: "vertical", right: 4, top: 10, itemHeight: 140,
                   formatter: function (v) { return Math.round(100 * v) + "%"; } },
      series: [
        { id: "map", type: "heatmap", xAxisIndex: 0, yAxisIndex: 0, data: [], progressive: 0,
          emphasis: { disabled: true },
          markLine: { silent: true, symbol: "none", animation: false, data: [] } },
        // Histogram: seria custom rysuje słupki od krawędzi do krawędzi przedziału.
        { id: "hist", type: "custom", xAxisIndex: 1, yAxisIndex: 1, data: [], silent: true,
          animation: false, renderItem: histBar, encode: { x: [0, 1], y: 2 } },
        { id: "normal", type: "line", xAxisIndex: 1, yAxisIndex: 1, data: [], symbol: "none",
          silent: true, animation: false, lineStyle: { width: 2.5 } }
      ]
    }, { renderer: "canvas", onResize: function () { rowLabels(); positionHandle(); } });

    var barFill = "#ccc", barEdge = "#888";
    function histBar(params, api) {
      var a = api.coord([api.value(0), api.value(2)]), b = api.coord([api.value(1), 0]);
      return { type: "rect", shape: { x: a[0] + 0.5, y: a[1], width: Math.max(0, b[0] - a[0] - 1), height: b[1] - a[1] },
               style: { fill: barFill, stroke: barEdge, lineWidth: 1 } };
    }

    // Podpisy pierwszego i ostatniego wiersza: co oznacza wiersz (wariant A).
    function rowLabels() {
      if (!B) return;
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      function label(id, row, text) {
        var p = chart.convertToPixel({ xAxisIndex: 0, yAxisIndex: 0 }, [CELLS - 1, row]);
        if (!p) return null;
        // Prawy brzeg podpisu kilka pikseli przed końcem mapy — obok stoi legenda kolorów.
        return { id: id, type: "text", silent: true, z: 20, x: p[0] - 8, y: p[1],
          style: { text: text, fill: P.ink, font: "600 " + (size * 0.9) + "px " + P.mono,
                   textAlign: "right", textVerticalAlign: row === 0 ? "top" : "bottom",
                   backgroundColor: alpha(P.surface, 0.85), padding: [2, 6], borderRadius: 3 } };
      }
      var items = [label("lblFirst", 0, "n = 1: " + B.one),
                   label("lblLast", NROW - 1, "n = " + NROW + ": średnia z " + NROW + " " + B.many)];
      chart.setOption({ graphic: items.filter(Boolean) });
    }

    // Uchwyt na początku wybranego wiersza, wewnątrz mapy (nie na etykietach osi).
    function positionHandle() {
      var p = chart.convertToPixel({ xAxisIndex: 0, yAxisIndex: 0 }, [2, n - 1]);
      if (p) handle.position(p[0], p[1]);
    }

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var y0 = Charts.ecAxis(P, size, { nameLocation: "start", nameGap: 6 });
      y0.splitLine = { show: false };
      y0.axisLabel.interval = function (i) { return i === 0 || (i + 1) % 5 === 0; };
      var x1 = Charts.ecAxis(P, size, { nameGap: Math.round(size * 1.9) });
      var y1 = Charts.ecAxis(P, size, { nameGap: Math.round(size * 3) });
      y1.splitNumber = 3;
      y1.axisLabel.showMaxLabel = false;  // górny kraniec (np. 530) nie jest okrągłą podziałką
      barFill = alpha(P.niebo, 0.45); barEdge = P.niebo;
      chart.setOption({
        grid: [{ right: Math.round(size * 4.6) }, { right: Math.round(size * 4.6), bottom: Math.round(size * 3.4) }],
        xAxis: [{}, x1], yAxis: [y0, y1],
        visualMap: { inRange: { color: [P.surface, alpha(P.accent, 0.45), P.accent] },
                     textStyle: { color: P.subtle, fontFamily: P.mono, fontSize: size * 0.85 },
                     itemWidth: Math.round(size * 1.1) },
        series: [
          { id: "map", markLine: { lineStyle: { color: P.ink, width: 1.5, type: "solid" }, label: { show: false } } },
          { id: "hist", renderItem: histBar },
          { id: "normal", lineStyle: { color: P.accent } }
        ]
      });
      rowLabels();
      positionHandle();
    }

    function setBase(i) {
      B = BASES[i]; NROW = B.rows.length; CELLS = B.rows[0].length;
      var w = (B.xhi - B.xlo) / CELLS, cats = [], data = [];
      for (var c = 0; c < CELLS; c++) cats.push(fmt(B.xlo + (c + 0.5) * w, 3));
      B.rows.forEach(function (row, r) {
        var max = Math.max.apply(null, row) || 1;
        row.forEach(function (v, c) { data.push([c, r, v / max]); });
      });
      var ns = [];
      for (var k = 1; k <= NROW; k++) ns.push(String(k));
      chart.setOption({
        xAxis: [{ data: cats }, { min: B.xlo, max: B.xhi, name: B.axis + " (" + B.unit + ")" }],
        yAxis: [{ data: ns }, {}],
        series: [{ id: "map", data: data }]
      });
      if (read.story) read.story.textContent = B.story;
      rowLabels();
      setN(n);
    }

    function setN(v) {
      n = Math.max(1, Math.min(NROW, Math.round(v)));
      var se = B.sd / Math.sqrt(n), bw = (B.xhi - B.xlo) / B.bins, counts = B.counts[n - 1];
      var hist = counts.map(function (k, i) { return [B.xlo + i * bw, B.xlo + (i + 1) * bw, k]; });
      // Dzwon w liczbach średnich: oczekiwana liczność przedziału ≈ 2000 · szerokość · φ(x).
      var normal = [];
      for (var c = 0; c <= 300; c++) {
        var x = B.xlo + c * (B.xhi - B.xlo) / 300;
        normal.push([x, B.sims * bw * npdf(x, B.mu, se)]);
      }
      var top = Math.max(Math.max.apply(null, counts), B.sims * bw * npdf(B.mu, B.mu, se)) * 1.1;
      chart.setOption({
        yAxis: [{}, { max: Math.ceil(top / 10) * 10 }],
        series: [
          { id: "map", markLine: { data: [{ yAxis: String(n) }] } },
          { id: "hist", data: hist },
          { id: "normal", data: normal }
        ]
      });
      if (read.n) read.n.textContent = String(n);
      if (read.skew) read.skew.textContent = fmt(B.skew / Math.sqrt(n), 2);
      if (read.tv) read.tv.textContent = fmt(100 * B.tv[n - 1], 1) + "%";
      handle.value(n, 1, NROW, "n = " + n);
      positionHandle();
    }

    // Kursor nad mapą wybiera wiersz (zdarzenia ECharts działają też dotykiem).
    function pick(params) {
      if (params.seriesId === "map" && params.value) setN(params.value[1] + 1);
    }
    chart.on("mouseover", pick);
    chart.on("click", pick);

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-m-dist]");
      if (!b) return;
      press(root, "data-m-dist", b);
      setBase(Number(b.getAttribute("data-m-dist")));
    });

    Charts.zoomControl(root);
    paint();
    setBase(0);
    Charts.onTheme(root, paint);
  });

  // ==========================================================================
  // E2 — ZAZNACZ PRZEDZIAŁ
  // ==========================================================================
  Charts.onReady(".lc-brushx", function (root) {
    var el = root.querySelector('.lc-ec[data-role="hist"]');
    if (!el) return false;
    var C = JSON.parse(root.dataset.config || "{}");
    var X = C.values, N = X.length, LO = C.lo, HI = C.hi, BIN = C.bin;
    var a = C.a, b = C.b, STEP = 1;
    var read = {};
    ["range", "data", "model", "ratio"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });

    // Histogram gęstości (liczność / (N · szerokość)) jako schodki w środkach przedziałów.
    var bins = [];
    for (var lo = LO; lo < HI - 1e-9; lo += BIN) bins.push(0);
    X.forEach(function (x) {
      var i = Math.floor((x - LO) / BIN);
      if (i >= 0 && i < bins.length) bins[i]++;
    });
    var hist = bins.map(function (k, i) { return [LO + (i + 0.5) * BIN, k / (N * BIN)]; });
    var curve = [];
    for (var c = 0; c <= 420; c++) { var x = LO + c * (HI - LO) / 420; curve.push([x, npdf(x, C.mean, C.sd)]); }

    var layer = Charts.handleLayer(root), handles = {};
    ["a", "b"].forEach(function (key) {
      handles[key] = layer.add(el, {
        label: key === "a" ? "Dolna granica przedziału" : "Górna granica przedziału",
        hint: "Strzałki przesuwają granicę o 1 g, Shift o 10 g. Home i End: krańce osi.",
        onKey: function (event) {
          var cur = key === "a" ? a : b;
          var v = Charts.keyValue(event, cur, key === "a" ? LO : a, key === "a" ? b : HI, STEP, 10);
          if (v === null) return false;
          if (key === "a") a = v; else b = v;
          press(root, "data-b-preset", null);
          brushTo(); update();
          layer.announce(describe());
          return true;
        }
      });
    });

    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 16, top: 14, bottom: 64 },
      xAxis: { type: "value", min: LO, max: HI, name: "Masa jabłka (g)" },
      yAxis: { type: "value", min: 0, name: "Gęstość", axisLabel: { show: false } },
      dataZoom: [
        { type: "slider", xAxisIndex: 0, filterMode: "filter", height: 18, bottom: 6,
          labelFormatter: function (v) { return fmt(v, 0); } },
        { type: "inside", xAxisIndex: 0, filterMode: "filter", moveOnMouseMove: false, zoomOnMouseWheel: true }
      ],
      brush: { xAxisIndex: 0, brushLink: "none", transformable: true, removeOnClick: false,
               throttleType: "debounce", throttleDelay: 20 },
      series: [
        { id: "hist", type: "line", step: "middle", data: hist, symbol: "none", silent: true,
          animation: false, lineStyle: { width: 1.5 }, areaStyle: {} },
        { id: "model", type: "line", data: curve, symbol: "none", silent: true, animation: false,
          lineStyle: { width: 2.5 } }
      ]
    }, { onResize: positionHandles });

    // Zaznaczanie myszą bez paska narzędzi: kursor przeciąga pas lineX.
    chart.dispatchAction({ type: "takeGlobalCursor", key: "brush",
      brushOption: { brushType: "lineX", brushMode: "single" } });

    // Uchwyty stoją przy górnej krawędzi siatki (grid.top = 14 px); poza
    // przybliżonym zakresem osi są ukryte.
    function positionHandles() {
      ["a", "b"].forEach(function (key) {
        var p = chart.convertToPixel({ gridIndex: 0 }, [key === "a" ? a : b, 0]);
        if (!p) return;
        var inside = p[0] >= 0 && p[0] <= el.clientWidth;
        handles[key].element.style.visibility = inside ? "" : "hidden";
        handles[key].position(p[0], 22);
      });
    }

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var y = Charts.ecAxis(P, size, { nameGap: Math.round(size * 0.8) });
      y.axisLabel = { show: false };
      var x = Charts.ecAxis(P, size, { nameGap: Math.round(size * 1.9) });
      x.axisLabel.showMinLabel = false;  // kraniec 90 g nachodziłby na 100
      chart.setOption({
        grid: { bottom: Math.round(size * 5) },
        xAxis: x,
        yAxis: y,
        dataZoom: [{ textStyle: { color: P.subtle, fontFamily: P.mono },
                     borderColor: P.rule, fillerColor: alpha(P.accent, 0.12),
                     dataBackground: { lineStyle: { color: P.subtle }, areaStyle: { color: alpha(P.subtle, 0.2) } },
                     handleStyle: { color: P.surface, borderColor: P.accent } }, {}],
        brush: { brushStyle: { color: alpha(P.accent, 0.16), borderColor: P.accent, borderWidth: 1.5 },
                 outOfBrush: { colorAlpha: 1 } },
        series: [
          { id: "hist", lineStyle: { color: P.niebo }, areaStyle: { color: alpha(P.niebo, 0.25) } },
          { id: "model", lineStyle: { color: P.accent } }
        ]
      });
      brushTo();
      positionHandles();
    }

    // Kraniec osi oznacza przedział otwarty („> 250 g”); model liczy wtedy cały ogon.
    function bounds() {
      return { lo: a <= LO ? -Infinity : a, hi: b >= HI ? Infinity : b };
    }
    function describe() {
      var r = bounds();
      if (r.lo === -Infinity && r.hi === Infinity) return "Cały zakres";
      if (r.lo === -Infinity) return "< " + fmt(b, 0) + " g";
      if (r.hi === Infinity) return "> " + fmt(a, 0) + " g";
      return fmt(a, 0) + "–" + fmt(b, 0) + " g";
    }
    function update() {
      var r = bounds(), k = 0;
      X.forEach(function (x) { if (x > r.lo && x < r.hi) k++; });
      var pd = k / N;
      var pm = (r.hi === Infinity ? 1 : phi((r.hi - C.mean) / C.sd)) - (r.lo === -Infinity ? 0 : phi((r.lo - C.mean) / C.sd));
      if (read.range) read.range.textContent = describe();
      if (read.data) read.data.textContent = fmt(100 * pd, pd < 0.01 ? 2 : 1) + "% (" + k + ")";
      if (read.model) read.model.textContent = fmt(100 * pm, pm < 0.01 ? 2 : 1) + "%";
      if (read.ratio) read.ratio.textContent = pm > 0 ? fmt(pd / pm, 2) + "×" : "–";
      ["a", "b"].forEach(function (key) {
        var v = key === "a" ? a : b;
        handles[key].value(v, key === "a" ? LO : a, key === "a" ? b : HI, fmt(v, 0) + " g");
      });
      positionHandles();
    }
    var syncing = false;
    function brushTo() {
      syncing = true;
      chart.dispatchAction({ type: "brush", areas: [{ brushType: "lineX", xAxisIndex: 0, coordRange: [a, b] }] });
      syncing = false;
    }
    chart.on("brushSelected", function (params) {
      if (syncing) return;
      var area = params.batch && params.batch[0] && params.batch[0].areas[0];
      if (!area || !area.coordRange) return;
      a = Math.max(LO, Math.round(area.coordRange[0]));
      b = Math.min(HI, Math.round(area.coordRange[1]));
      press(root, "data-b-preset", null);
      update();
    });

    var PRESETS = {
      mid: { a: 160, b: 200, zoom: [LO, HI] },
      big: { a: 250, b: HI, zoom: [220, HI] },
      small: { a: LO, b: 120, zoom: [LO, 150] }
    };
    Charts.listen(root, "click", function (e) {
      var btn = e.target.closest("[data-b-preset]");
      if (!btn) return;
      var p = PRESETS[btn.getAttribute("data-b-preset")];
      press(root, "data-b-preset", btn);
      a = p.a; b = p.b;
      chart.dispatchAction({ type: "dataZoom", dataZoomIndex: 0, startValue: p.zoom[0], endValue: p.zoom[1] });
      brushTo(); update();
    });

    Charts.zoomControl(root);
    paint();
    update();
    Charts.onTheme(root, paint);
  });
  // ==========================================================================
  // RYC. 4.2 „ILE OSÓB MA DOKŁADNIE 170 CM?” — lupa
  // data-config: [{key, lab, d, count}] — d: szerokość pasu w cm (0 = pomiar idealny).
  // ==========================================================================
  var POP = 1e6;
  function stepUi(root, onStep) {
    var STEPS = JSON.parse(root.dataset.config || "[]");
    var readC = root.querySelector('[data-role="count"]'), readP = root.querySelector('[data-role="pct"]');
    function show(i) {
      var st = STEPS[i];
      if (readC) readC.textContent = fmt(st.count, 0, true);
      if (readP) readP.textContent = fmt(100 * st.count / POP, 4) + "%";
      onStep(st, i);
    }
    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-p-step]");
      if (!b) return;
      press(root, "data-p-step", b);
      show(Number(b.getAttribute("data-p-step")));
    });
    return { steps: STEPS, show: show };
  }
  function decimals(d) { return d >= 1 ? 1 : d >= 0.1 ? 2 : d >= 0.01 ? 3 : 4; }
  function bandText(st) {
    if (!st.d) return "pas szerokości 0 cm";
    var k = decimals(st.d);
    return "pas od " + fmt(170 - st.d / 2, k) + " do " + fmt(170 + st.d / 2, k) + " cm";
  }

  // --- lupa -----------------------------------------------------------
  Charts.onReady(".lc-zoom170", function (root) {
    var el = root.querySelector('.lc-ec[data-role="zoom"]');
    if (!el) return false;
    var cur = null, lastHalf = 6;
    var overview = [];
    for (var x = 140; x <= 200.001; x += 0.25) overview.push([x, npdf(x, 170, 8)]);
    var chart = Charts.chart(el, {
      animation: true, animationDurationUpdate: 650, animationEasingUpdate: "cubicOut", aria: { enabled: false },
      grid: [{ left: 8, right: 12, top: 6, height: "17%" }, { left: 8, right: 12, top: "30%", bottom: 8 }],
      xAxis: [{ type: "value", gridIndex: 0, min: 140, max: 200, axisLabel: { show: false }, axisTick: { show: false },
                splitLine: { show: false } },
              { type: "value", gridIndex: 1, name: "Wzrost (cm)" }],
      yAxis: [{ type: "value", gridIndex: 0, min: 0, max: 0.055, show: false },
              { type: "value", gridIndex: 1, min: 0, max: 0.055, name: "Gęstość" }],
      series: [
        { id: "ov", type: "line", xAxisIndex: 0, yAxisIndex: 0, data: overview, symbol: "none", silent: true,
          lineStyle: { width: 1.5 }, areaStyle: {},
          markArea: { silent: true, data: [] }, markLine: { silent: true, symbol: "none", label: { show: false }, data: [] } },
        { id: "curve", type: "line", xAxisIndex: 1, yAxisIndex: 1, data: [], symbol: "none", silent: true,
          lineStyle: { width: 2.5 } },
        { id: "band", type: "line", xAxisIndex: 1, yAxisIndex: 1, data: [], symbol: "none", silent: true,
          lineStyle: { width: 0 }, areaStyle: {},
          markLine: { silent: true, symbol: "none", label: { show: false }, data: [] } }
      ]
    }, { onResize: function () { if (cur) label(cur); } });

    function label(st) {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      // Lewa część wykresu, poniżej krzywej (ok. 0.05) — tam pas nie sięga.
      var p = chart.convertToPixel({ gridIndex: 1 }, [170 - lastHalf * 0.95, 0.034]);
      if (!p) return;
      chart.setOption({ graphic: [
        { id: "big", type: "text", silent: true, x: p[0], y: p[1], z: 20,
          style: { text: fmt(st.count, 0, true) + (st.count === 1 ? " osoba" : " osób"), fill: P.accent,
                   font: "700 " + (size * 2.3) + "px " + P.mono, textVerticalAlign: "top" } },
        { id: "sub", type: "text", silent: true, x: p[0], y: p[1] + size * 2.8, z: 20,
          style: { text: bandText(st), fill: P.soft, font: (size * 1.05) + "px " + P.mono, textVerticalAlign: "top" } }
      ] });
    }

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var y1 = Charts.ecAxis(P, size, { nameGap: Math.round(size * 3.4) });
      y1.axisLabel.showMaxLabel = false;
      chart.setOption({
        xAxis: [{}, Charts.ecAxis(P, size, { nameGap: Math.round(size * 1.9) })], yAxis: [{}, y1],
        series: [
          { id: "ov", lineStyle: { color: P.subtle }, areaStyle: { color: alpha(P.subtle, 0.12) },
            markArea: { itemStyle: { color: alpha(P.accent, 0.25), borderColor: P.accent, borderWidth: 1 } },
            markLine: { lineStyle: { color: P.accent, width: 1.5, type: "solid" } } },
          { id: "curve", lineStyle: { color: P.soft } },
          { id: "band", areaStyle: { color: alpha(P.accent, 0.75) },
            markLine: { lineStyle: { color: P.accent, width: 2, type: [4, 3] } } }
        ]
      });
      if (cur) label(cur);
    }

    var ui = stepUi(root, function (st) {
      cur = st;
      var half = st.d ? 6 * st.d : lastHalf, lo = 170 - half, hi = 170 + half;
      var k = decimals(st.d || 2 * half / 12) - 1;  // podziałki co 2·d: o jedno miejsce mniej niż granice pasu
      lastHalf = half;
      var curve = [];
      for (var c = 0; c <= 200; c++) { var x = lo + c * (hi - lo) / 200; curve.push([x, npdf(x, 170, 8)]); }
      var band = [];
      if (st.d) for (var c2 = 0; c2 <= 40; c2++) { var xb = 170 - st.d / 2 + c2 * st.d / 40; band.push([xb, npdf(xb, 170, 8)]); }
      chart.setOption({
        xAxis: [{}, { min: lo, max: hi, interval: half / 3,
                      axisLabel: { formatter: function (v) { return fmt(v, k); } } }],
        series: [
          { id: "ov", markArea: { data: [[{ xAxis: lo }, { xAxis: hi }]] }, markLine: { data: [{ xAxis: 170 }] } },
          { id: "curve", data: curve },
          { id: "band", data: band, markLine: { data: st.d ? [] : [{ xAxis: 170 }] } }
        ]
      });
      label(st);
    });
    Charts.zoomControl(root);
    paint();
    ui.show(0);
    Charts.onTheme(root, paint);
  });

})();
