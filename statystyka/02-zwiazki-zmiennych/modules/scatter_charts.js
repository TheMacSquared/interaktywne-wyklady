// Wykresy rozrzutu w ECharts (rozdz. 4 i 5). Wszystko liczy się w przeglądarce:
// prosta MNK, r Pearsona i SD reszt. Cztery kontenery:
//   .lc-strength (Ryc. 4.3) — stały trend, rozrzut zmienia uchwyt na brzegu pasa ±2 SD
//                             albo przyciski [data-s-preset]; data-config {x, e, sigma, start}
//   .lc-slope    (Ryc. 4.4) — y = b·(x + rel·e), nachylenie zmienia uchwyt na końcu prostej
//                             albo przyciski [data-s-preset]; data-config {x, e, rel, slopes, start}
//   .lc-outlier  (Ryc. 5.3) — losowanie N(0, sd) w JS, dopisywanie i przeciąganie wartości
//                             odstających; data-n, data-sd, data-shift, przyciski [data-o-new], [data-o-add]
//   .lc-simpson  (Ryc. 5.4) — dane z R, przełącznik [data-s-view] „Razem / Osobno”;
//                             data-config {x, y, g, groups, colors, xlim, ylim}
// Wykres: <div class="lc-ec" data-role="plot">. Odczyty w [data-role]. Start po
// pojawieniu się kontenera w DOM (rozdziały renderują się przez renderUI),
// przemalowanie przy zmianie motywu, S/M/L i lupy.
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;

  function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

  var fit = Charts.linearFit;
  function line(f, x0, x1) { return [[x0, f.a + f.b * x0], [x1, f.a + f.b * x1]]; }
  function fmt(v, d) { return isFinite(v) ? v.toFixed(d) : "–"; }

  function reads(root, keys) {
    var o = {};
    keys.forEach(function (k) { o[k] = root.querySelector('[data-role="' + k + '"]'); });
    return o;
  }
  function put(el, s) { if (el) el.textContent = s; }

  // Serie: punkty (scatter) i odcinki (line); id nadaje widget.
  function pts(id, size) { return { id: id, type: "scatter", data: [], symbolSize: size, silent: true, animation: false }; }
  function seg(id, width, dash) {
    return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
             lineStyle: { width: width, type: dash ? [5, 4] : "solid" } };
  }

  function makeChart(el, series, x, y, onResize) {
    return Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 12, top: 10, bottom: 8 },
      xAxis: Object.assign({ type: "value" }, x),
      yAxis: Object.assign({ type: "value" }, y),
      series: series
    }, { onResize: onResize });
  }

  // Wspólny styl osi; interwały i nazwy podaje widget.
  function axes(root, chart, x, y) {
    var P = Charts.palette(), size = Charts.axisFontSize(root);
    chart.setOption({
      xAxis: Charts.ecAxis(P, size, Object.assign({ nameGap: Math.round(size * 1.9) }, x || {})),
      yAxis: Charts.ecAxis(P, size, Object.assign({ nameGap: Math.round(size * 2.6) }, y || {}))
    });
    return P;
  }

  function toPx(chart, x, y) { return chart.convertToPixel({ gridIndex: 0 }, [x, y]) || [NaN, NaN]; }
  function pointer(el, e) {
    var rc = el.getBoundingClientRect();
    return { px: e.clientX - rc.left, py: e.clientY - rc.top };
  }
  function pixelDist(chart, p, x, y) {
    var q = toPx(chart, x, y), dx = q[0] - p.px, dy = q[1] - p.py;
    return Math.sqrt(dx * dx + dy * dy);
  }

  function pressPresets(root, attr, values, cur) {
    root.querySelectorAll("[" + attr + "]").forEach(function (btn) {
      var v = values[Number(btn.getAttribute(attr))];
      btn.setAttribute("aria-pressed", Math.abs(v - cur) < 1e-9 ? "true" : "false");
    });
  }

  // Uchwyt przeciągany w pionie: wspólne dla Ryc. 4.3 i 4.4.
  function handleDrag(el, chart, handlePos, ranges, onMove) {
    var drag = false;
    function near(p) { var h = handlePos(); return pixelDist(chart, p, h.x, h.y) < 40; }
    el.style.touchAction = "none";
    function start(e) {
      drag = true; el.setPointerCapture(e.pointerId); e.preventDefault();
    }
    Charts.listen(el, "pointerdown", function (e) { if (near(pointer(el, e))) start(e); });
    Charts.listen(el, "pointermove", function (e) {
      if (drag) { onMove(Charts.ecPointer(chart, el, e, ranges.x, ranges.y)); return; }
      el.style.cursor = near(pointer(el, e)) ? "ns-resize" : "default";
    });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(el, t, function () { drag = false; }); });
    return start;
  }

  // ==========================================================================
  // Ryc. 4.3: siła korelacji — stały trend, zmienny rozrzut
  // ==========================================================================
  function initStrength(root, el) {
    var C = JSON.parse(root.dataset.config), X = C.x, E = C.e;
    var SMAX = 3.5, HX = 2, XL = [-3, 3], YL = [-10, 10];
    var sigma = C.sigma[C.start || 0], f = null;
    var R = reads(root, ["r", "r2"]);

    var layer = Charts.handleLayer(root), startDrag;
    var handle = layer.add(el, {
      label: "Rozrzut wokół prostej", orientation: "vertical",
      hint: "Strzałki zmieniają wartość, Shift zwiększa krok. Home i End wybierają granice.",
      onKey: function (event) {
        var value = Charts.keyValue(event, sigma, 0, SMAX, 0.05, 0.5);
        if (value === null) return false;
        Charts.cancelTween(root); sigma = +value.toFixed(2); draw();
        layer.announce("Rozrzut wokół prostej: " + fmt(sigma, 2) + ". Korelacja " + fmt(f.r, 2)); return true;
      },
      onPointerDown: function (event) { startDrag(event); }
    });
    function syncHandle() {
      if (!f) return;
      var h = hpos(), q = toPx(chart, h.x, h.y);
      handle.position(q[0], q[1]);
      handle.value(sigma, 0, SMAX, fmt(sigma, 2));
    }
    var chart = makeChart(el, [pts("pts", 7), seg("up", 1.5, true), seg("lo", 1.5, true), seg("fit", 2.5), pts("dot", 16)],
      { min: XL[0], max: XL[1], interval: 1, name: "x" },
      { min: YL[0], max: YL[1], interval: 5, name: "y" }, syncHandle);

    function paint() {
      var P = axes(root, chart);
      chart.setOption({ series: [
        { id: "pts", itemStyle: { color: alpha(P.niebo, 0.65) } },
        { id: "up", lineStyle: { color: P.subtle } }, { id: "lo", lineStyle: { color: P.subtle } },
        { id: "fit", lineStyle: { color: P.accent } },
        { id: "dot", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } }
      ] });
      syncHandle();
    }
    function hpos() { return { x: HX, y: f.a + f.b * HX + 2 * f.sd }; }
    function draw() {
      var ys = X.map(function (x, i) { return x + sigma * E[i]; });
      f = fit(X, ys);
      var up = { a: f.a + 2 * f.sd, b: f.b }, lo = { a: f.a - 2 * f.sd, b: f.b }, h = hpos();
      chart.setOption({ series: [
        { id: "pts", data: X.map(function (x, i) { return [x, ys[i]]; }) },
        { id: "up", data: line(up, XL[0], XL[1]) }, { id: "lo", data: line(lo, XL[0], XL[1]) },
        { id: "fit", data: line(f, XL[0], XL[1]) }, { id: "dot", data: [[h.x, h.y]] }
      ] });
      put(R.r, fmt(f.r, 2));
      put(R.r2, fmt(100 * f.r * f.r, 0) + "%");
      syncHandle();
      pressPresets(root, "data-s-preset", C.sigma, sigma);
    }

    startDrag = handleDrag(el, chart, hpos, { x: XL, y: YL }, function (v) {
      Charts.cancelTween(root);
      sigma = clamp(Math.round((v.y - (f.a + f.b * HX)) / 2 * 100) / 100, 0, SMAX);
      draw();
    });
    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-s-preset]");
      if (!b) return;
      var target = C.sigma[Number(b.getAttribute("data-s-preset"))];
      Charts.tween(root, sigma, target, function (v) { sigma = v; draw(); }, function () { sigma = target; draw(); });
    });

    Charts.zoomControl(root);
    paint(); draw(); Charts.onTheme(root, paint);
  }

  // ==========================================================================
  // Ryc. 4.4: r nie zależy od nachylenia
  // ==========================================================================
  function initSlope(root, el) {
    var C = JSON.parse(root.dataset.config), X = C.x, E = C.e, REL = C.rel;
    var BMIN = 0.1, BMAX = 2, HX = 2.5, XL = [-3, 3], YL = [-6, 6];
    var slope = C.slopes[C.start || 0], f = null;
    var R = reads(root, ["b", "sd", "r"]);

    var layer = Charts.handleLayer(root), startDrag;
    var handle = layer.add(el, {
      label: "Nachylenie", orientation: "vertical",
      hint: "Strzałki zmieniają wartość, Shift zwiększa krok. Home i End wybierają granice.",
      onKey: function (event) {
        var value = Charts.keyValue(event, slope, BMIN, BMAX, 0.05, 0.5);
        if (value === null) return false;
        Charts.cancelTween(root); slope = +value.toFixed(2); draw();
        layer.announce("Nachylenie: " + fmt(slope, 2) + ". Korelacja " + fmt(f.r, 2)); return true;
      },
      onPointerDown: function (event) { startDrag(event); }
    });
    function syncHandle() {
      if (!f) return;
      var h = hpos(), q = toPx(chart, h.x, h.y);
      handle.position(q[0], q[1]);
      handle.value(slope, BMIN, BMAX, fmt(slope, 2));
    }
    var chart = makeChart(el, [pts("pts", 7), seg("fit", 2.5), pts("dot", 16)],
      { min: XL[0], max: XL[1], interval: 1, name: "x" },
      { min: YL[0], max: YL[1], interval: 2, name: "y" }, syncHandle);

    function paint() {
      var P = axes(root, chart);
      chart.setOption({ series: [
        { id: "pts", itemStyle: { color: alpha(P.niebo, 0.65) } },
        { id: "fit", lineStyle: { color: P.accent } },
        { id: "dot", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } }
      ] });
      syncHandle();
    }
    function hpos() { return { x: HX, y: f.a + f.b * HX }; }
    function draw() {
      var ys = X.map(function (x, i) { return slope * (x + REL * E[i]); });
      f = fit(X, ys);
      var h = hpos();
      chart.setOption({ series: [
        { id: "pts", data: X.map(function (x, i) { return [x, ys[i]]; }) },
        { id: "fit", data: line(f, XL[0], XL[1]) }, { id: "dot", data: [[h.x, h.y]] }
      ] });
      put(R.b, fmt(f.b, 2));
      put(R.sd, fmt(f.sd, 2));
      put(R.r, fmt(f.r, 2));
      syncHandle();
      pressPresets(root, "data-s-preset", C.slopes, slope);
    }

    startDrag = handleDrag(el, chart, hpos, { x: XL, y: YL }, function (v) {
      Charts.cancelTween(root);
      slope = clamp(Math.round((v.y - f.a) / HX * 20) / 20, BMIN, BMAX);
      draw();
    });
    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-s-preset]");
      if (!b) return;
      var target = C.slopes[Number(b.getAttribute("data-s-preset"))];
      Charts.tween(root, slope, target, function (v) { slope = v; draw(); }, function () { slope = target; draw(); });
    });

    Charts.zoomControl(root);
    paint(); draw(); Charts.onTheme(root, paint);
  }

  // ==========================================================================
  // Ryc. 5.3: wpływ wartości odstającej na r
  // ==========================================================================
  // Rozkład normalny metodą Boxa–Mullera.
  function rnorm(sd) {
    var u = 0, v = 0;
    while (u === 0) u = Math.random();
    while (v === 0) v = Math.random();
    return sd * Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }
  function r2(v) { return Math.round(v * 100) / 100; }

  function initOutlier(root, el) {
    var N = parseInt(root.dataset.n || "50", 10), SD = parseFloat(root.dataset.sd || "3");
    var SHIFT = parseFloat(root.dataset.shift || "15");
    var base = [], outs = [], LO = -10, HI = 10, STEP = 5;
    var R = reads(root, ["r", "r0", "k"]);

    var layer = Charts.handleLayer(root), pointHandles = [], lastR = NaN;
    function syncHandles() {
      pointHandles.forEach(function (handle, i) {
        var point = outs[i], q = toPx(chart, point.x, point.y);
        handle.position(q[0], q[1]);
        handle.element.setAttribute("aria-label", "Wartość odstająca " + (i + 1) + ". X " + fmt(point.x, 1) + ", Y " + fmt(point.y, 1));
      });
    }
    function makePointHandle(i) {
      var handle = layer.add(el, {
        role: "button", label: "Wartość odstająca " + (i + 1),
        hint: "Lewo i prawo zmieniają X, góra i dół zmieniają Y. Shift zwiększa krok dziesięciokrotnie.",
        onKey: function (event) {
          var axis = ["ArrowLeft", "ArrowRight"].indexOf(event.key) >= 0 ? "x" :
            (["ArrowUp", "ArrowDown"].indexOf(event.key) >= 0 ? "y" : null);
          if (!axis) return false;
          var step = Math.max(0.1, Math.round((HI - LO) / 100 * 10) / 10) * (event.shiftKey ? 10 : 1);
          var direction = ["ArrowRight", "ArrowUp"].indexOf(event.key) >= 0 ? 1 : -1;
          outs[i][axis] = r2(clamp(outs[i][axis] + direction * step, LO, HI));
          rescale(); draw();
          layer.announce("Punkt " + (i + 1) + ". X " + fmt(outs[i].x, 1) + ", Y " + fmt(outs[i].y, 1) + ". Korelacja " + fmt(lastR, 3));
          return true;
        },
        onPointerDown: function (event) {
          drag = i; el.setPointerCapture(event.pointerId); event.preventDefault();
        }
      });
      handle.element.setAttribute("data-boundary", i + 1);
      return handle;
    }
    var chart = makeChart(el, [pts("base", 8), pts("outs", 16), seg("fit0", 1.5, true), seg("fit", 2.5)],
      { name: "X" }, { name: "Y" }, syncHandles);

    function paint() {
      var P = axes(root, chart);
      chart.setOption({ series: [
        { id: "base", itemStyle: { color: alpha(P.niebo, 0.65) } },
        { id: "outs", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } },
        { id: "fit0", lineStyle: { color: P.subtle } },
        { id: "fit", lineStyle: { color: P.accent } }
      ] });
      syncHandles();
    }

    // Wspólny zakres osi x i y z zapasem, krok 5; stały w trakcie przeciągania.
    function rescale() {
      var all = base.concat(outs), mn = Infinity, mx = -Infinity;
      all.forEach(function (p) { mn = Math.min(mn, p.x, p.y); mx = Math.max(mx, p.x, p.y); });
      var lo = Math.min(-10, mn - 2), hi = Math.max(10, mx + 4);
      STEP = hi - lo > 40 ? 10 : 5;
      LO = Math.floor(lo / STEP) * STEP;
      HI = Math.ceil(hi / STEP) * STEP;
      var range = { min: LO, max: HI, interval: STEP };
      chart.setOption({ xAxis: range, yAxis: range });
    }

    function draw() {
      var all = base.concat(outs);
      var fa = fit(all.map(function (p) { return p.x; }), all.map(function (p) { return p.y; }));
      var f0 = fit(base.map(function (p) { return p.x; }), base.map(function (p) { return p.y; }));
      function xy(p) { return [p.x, p.y]; }
      chart.setOption({ series: [
        { id: "base", data: base.map(xy) }, { id: "outs", data: outs.map(xy) },
        { id: "fit0", data: outs.length ? line(f0, LO, HI) : [] }, { id: "fit", data: line(fa, LO, HI) }
      ] });
      lastR = fa.r;
      syncHandles();
      put(R.r, fmt(fa.r, 3));
      put(R.r0, fmt(f0.r, 3));
      put(R.k, String(outs.length));
    }

    function fresh() {
      pointHandles.forEach(function (handle) { handle.remove(); }); pointHandles = [];
      base = [];
      for (var i = 0; i < N; i++) base.push({ x: r2(rnorm(SD)), y: r2(rnorm(SD)) });
      outs = [];
      rescale(); draw();
    }
    function add() {
      var all = base.concat(outs);
      outs.push({
        x: r2(Math.max.apply(null, all.map(function (p) { return p.x; })) + SHIFT),
        y: r2(Math.max.apply(null, all.map(function (p) { return p.y; })) + SHIFT)
      });
      pointHandles.push(makePointHandle(outs.length - 1));
      rescale(); draw();
    }

    // Przeciąganie: wciśnięcie łapie najbliższą wartość odstającą (w pikselach).
    var drag = -1;
    function nearest(p) {
      var best = -1, bd = 36;
      outs.forEach(function (o, i) { var d = pixelDist(chart, p, o.x, o.y); if (d < bd) { bd = d; best = i; } });
      return best;
    }
    el.style.touchAction = "none";
    Charts.listen(el, "pointerdown", function (e) {
      var i = nearest(pointer(el, e));
      if (i < 0) return;
      drag = i; el.setPointerCapture(e.pointerId); e.preventDefault();
    });
    Charts.listen(el, "pointermove", function (e) {
      if (drag >= 0) {
        var v = Charts.ecPointer(chart, el, e, [LO, HI], [LO, HI]);
        outs[drag] = { x: Math.round(v.x * 10) / 10, y: Math.round(v.y * 10) / 10 };
        draw(); return;
      }
      el.style.cursor = nearest(pointer(el, e)) >= 0 ? "move" : "default";
    });
    ["pointerup", "pointercancel"].forEach(function (t) {
      Charts.listen(el, t, function () { if (drag >= 0) { drag = -1; rescale(); draw(); } });
    });

    Charts.listen(root, "click", function (e) {
      if (e.target.closest("[data-o-new]")) fresh();
      else if (e.target.closest("[data-o-add]")) add();
    });

    Charts.zoomControl(root);
    paint(); fresh(); Charts.onTheme(root, paint);
  }

  // ==========================================================================
  // Ryc. 5.4: paradoks Simpsona
  // ==========================================================================
  function initSimpson(root, el) {
    var C = JSON.parse(root.dataset.config), X = C.x, Y = C.y, G = C.g, K = C.groups.length;
    var view = "all", COLS = [];
    var R = reads(root, ["r", "verdict", "caption"]);
    var gread = [];
    for (var k = 0; k < K; k++) gread.push(root.querySelector('[data-role="r' + k + '"]'));
    var gbox = root.querySelectorAll("[data-s-group]");

    // Grupy: indeksy punktów, dopasowanie i zakres x (prosta tylko nad danymi grupy).
    var idx = [], fits = [], spans = [];
    for (k = 0; k < K; k++) {
      idx.push([]);
      for (var i = 0; i < X.length; i++) if (G[i] === k) idx[k].push(i);
      var xs = idx[k].map(function (j) { return X[j]; }), ys = idx[k].map(function (j) { return Y[j]; });
      fits.push(fit(xs, ys));
      spans.push([Math.min.apply(null, xs), Math.max.apply(null, xs)]);
    }
    var fall = fit(X, Y);
    var sall = [Math.min.apply(null, X), Math.max.apply(null, X)];

    var series = [pts("pts", 8), seg("all", 2.5)];
    for (k = 0; k < K; k++) series.push(seg("g" + k, 2.5));
    var chart = makeChart(el, series,
      { min: C.xlim[0], max: C.xlim[1], interval: 5, name: "Staż (lata)" },
      { min: C.ylim[0], max: C.ylim[1], interval: 2, name: "Pensja (tys. zł)" });
    chart.setOption({ series: [{ id: "pts", data: X.map(function (x, i) { return [x, Y[i], G[i]]; }) }] });

    function paint() {
      axes(root, chart);
      COLS = C.colors.map(function (t) { return Charts.token(t) || "#888"; });
      draw();
    }

    function draw() {
      var split = view === "split", P = Charts.palette();
      var upd = [
        { id: "pts", itemStyle: { color: function (d) { return split ? alpha(COLS[d.value[2]], 0.8) : alpha(P.subtle, 0.45); } } },
        { id: "all", data: line(fall, sall[0], sall[1]),
          lineStyle: { color: P.secondary, width: split ? 1.5 : 2.5, type: split ? [6, 4] : "solid" } }
      ];
      for (var k = 0; k < K; k++) {
        upd.push({ id: "g" + k, data: split ? line(fits[k], spans[k][0], spans[k][1]) : [], lineStyle: { color: COLS[k] } });
      }
      chart.setOption({ series: upd });

      put(R.r, fmt(fall.r, 2));
      for (k = 0; k < K; k++) put(gread[k], fmt(fits[k].r, 2));
      gbox.forEach(function (el) { el.hidden = !split; });
      if (R.verdict) {
        R.verdict.className = split ? "lc-status-ok" : "lc-status-warning";
        (R.verdict.querySelector("strong") || R.verdict).textContent = split ? "Osobno:" : "Razem:";
      }
      put(R.caption, split
        ? " w każdej branży r jest dodatnie (" + C.groups.map(function (g, k) {
            return g + ": " + fmt(fits[k].r, 2);
          }).join("; ") + "): dłuższy staż, wyższa pensja."
        : " r = " + fmt(fall.r, 2) + ". Dłuższy staż, niższa pensja?");
      root.querySelectorAll("[data-s-view]").forEach(function (b) {
        b.setAttribute("aria-pressed", b.getAttribute("data-s-view") === view ? "true" : "false");
      });
    }

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-s-view]");
      if (!b) return;
      view = b.getAttribute("data-s-view");
      draw();
    });

    Charts.zoomControl(root);
    paint(); Charts.onTheme(root, paint);
  }

  // ==========================================================================
  var KINDS = { "lc-strength": initStrength, "lc-slope": initSlope, "lc-outlier": initOutlier, "lc-simpson": initSimpson };

  function init(root) {
    var el = root.querySelector('.lc-ec[data-role="plot"]');
    if (!el) return false;
    for (var cls in KINDS) if (root.classList.contains(cls)) { KINDS[cls](root, el); return; }
  }

  Charts.onReady(".lc-scat", init);
})();
