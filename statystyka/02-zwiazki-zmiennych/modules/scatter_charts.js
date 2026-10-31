// Wykresy rozrzutu w Chart.js (rozdz. 4 i 5). Wszystko liczy się w przeglądarce:
// prosta MNK, r Pearsona i SD reszt. Cztery kontenery:
//   .lc-strength (Ryc. 4.3) — stały trend, rozrzut zmienia uchwyt na brzegu pasa ±2 SD
//                             albo przyciski [data-s-preset]; data-config {x, e, sigma, start}
//   .lc-slope    (Ryc. 4.4) — y = b·(x + rel·e), nachylenie zmienia uchwyt na końcu prostej
//                             albo przyciski [data-s-preset]; data-config {x, e, rel, slopes, start}
//   .lc-outlier  (Ryc. 5.3) — losowanie N(0, sd) w JS, dopisywanie i przeciąganie wartości
//                             odstających; data-n, data-sd, data-shift, przyciski [data-o-new], [data-o-add]
//   .lc-simpson  (Ryc. 5.4) — dane z R, przełącznik [data-s-view] „Razem / Osobno”;
//                             data-config {x, y, g, groups, colors, xlim, ylim}
// Odczyty w [data-role]. Start po pojawieniu się kontenera w DOM (rozdziały
// renderują się przez renderUI), przemalowanie przy zmianie data-lc-theme.
(function () {
  var Charts = window.lcCharts;
  var token = Charts.token, alpha = Charts.alpha;

  function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

  // Prosta MNK y = a + b·x, r Pearsona i SD reszt (jak sd(residuals(lm(y ~ x))) w R).
  function fit(xs, ys) {
    var n = xs.length, mx = 0, my = 0, sxx = 0, syy = 0, sxy = 0, i;
    for (i = 0; i < n; i++) { mx += xs[i]; my += ys[i]; }
    mx /= n; my /= n;
    for (i = 0; i < n; i++) {
      var dx = xs[i] - mx, dy = ys[i] - my;
      sxx += dx * dx; syy += dy * dy; sxy += dx * dy;
    }
    var b = sxy / sxx, r = sxy / Math.sqrt(sxx * syy), sse = Math.max(0, syy - b * sxy);
    return { a: my - b * mx, b: b, r: r, sd: Math.sqrt(sse / (n - 1)) };
  }
  function line(f, x0, x1) { return [{ x: x0, y: f.a + f.b * x0 }, { x: x1, y: f.a + f.b * x1 }]; }
  function fmt(v, d) { return isFinite(v) ? v.toFixed(d) : "–"; }

  function reads(root, keys) {
    var o = {};
    keys.forEach(function (k) { o[k] = root.querySelector('[data-role="' + k + '"]'); });
    return o;
  }
  function put(el, s) { if (el) el.textContent = s; }

  function makeChart(canvas, datasets, scales) {
    return Charts.create(canvas, {
      type: "scatter",
      data: { datasets: datasets },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 8, right: 10 } },
        scales: {
          x: Object.assign({ type: "linear", title: { display: true, text: "" } }, scales.x),
          y: Object.assign({ type: "linear", title: { display: true, text: "" } }, scales.y)
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { line: { tension: 0 } }
      }
    });
  }

  function paintAxes(chart) {
    Charts.styleAxes(chart);
  }

  function pointer(canvas, e) {
    var rc = canvas.getBoundingClientRect();
    return { px: e.clientX - rc.left, py: e.clientY - rc.top };
  }
  function pixelDist(chart, p, x, y) {
    var dx = chart.scales.x.getPixelForValue(x) - p.px, dy = chart.scales.y.getPixelForValue(y) - p.py;
    return Math.sqrt(dx * dx + dy * dy);
  }
  function valueAt(chart, p) {
    var a = chart.chartArea;
    return {
      x: chart.scales.x.getValueForPixel(clamp(p.px, a.left, a.right)),
      y: chart.scales.y.getValueForPixel(clamp(p.py, a.top, a.bottom))
    };
  }

  function pressPresets(root, attr, values, cur) {
    root.querySelectorAll("[" + attr + "]").forEach(function (btn) {
      var v = values[Number(btn.getAttribute(attr))];
      btn.setAttribute("aria-pressed", Math.abs(v - cur) < 1e-9 ? "true" : "false");
    });
  }


  // Zbiór punktów (x, e) i uchwyt przeciągany w pionie: wspólne dla Ryc. 4.3 i 4.4.
  function handleDrag(canvas, chart, handlePos, onMove) {
    var drag = false;
    function near(p) { var h = handlePos(); return pixelDist(chart, p, h.x, h.y) < 40; }
    canvas.style.touchAction = "none";
    Charts.listen(canvas, "pointerdown", function (e) {
      var p = pointer(canvas, e);
      if (!near(p)) return;
      drag = true; canvas.setPointerCapture(e.pointerId); e.preventDefault();
    });
    Charts.listen(canvas, "pointermove", function (e) {
      var p = pointer(canvas, e);
      if (drag) { onMove(valueAt(chart, p)); return; }
      canvas.style.cursor = near(p) ? "ns-resize" : "default";
    });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(canvas, t, function () { drag = false; }); });
  }

  // ==========================================================================
  // Ryc. 4.3: siła korelacji — stały trend, zmienny rozrzut
  // ==========================================================================
  function initStrength(root, canvas) {
    var C = JSON.parse(root.dataset.config), X = C.x, E = C.e;
    var SMAX = 3.5, HX = 2, XL = [-3, 3], YL = [-10, 10];
    var sigma = C.sigma[C.start || 0], f = null;
    var R = reads(root, ["r", "r2"]);

    var chart = makeChart(canvas, [
      { data: [], pointRadius: 3.5, borderWidth: 0 },
      { data: [], showLine: true, pointRadius: 0, borderWidth: 1.5, borderDash: [5, 4] },
      { data: [], showLine: true, pointRadius: 0, borderWidth: 1.5, borderDash: [5, 4] },
      { data: [], showLine: true, pointRadius: 0, borderWidth: 2.5 },
      { data: [], pointRadius: 8, borderWidth: 2 }
    ], {
      x: { min: XL[0], max: XL[1], ticks: { stepSize: 1 }, title: { display: true, text: "x" } },
      y: { min: YL[0], max: YL[1], ticks: { stepSize: 5 }, title: { display: true, text: "y" } }
    });

    function paint() {
      paintAxes(chart);
      var niebo = token("--upwr-cat-niebo") || "#1d5f9e", accent = token("--upwr-accent") || "#7a2e3b";
      var subtle = token("--upwr-ink-subtle") || "#888", surface = token("--upwr-surface") || "#fff";
      var ds = chart.data.datasets;
      ds[0].backgroundColor = alpha(niebo, 0.65);
      ds[1].borderColor = ds[2].borderColor = subtle;
      ds[3].borderColor = accent;
      ds[4].backgroundColor = accent; ds[4].borderColor = surface;
      chart.update();
    }
    function hpos() { return { x: HX, y: f.a + f.b * HX + 2 * f.sd }; }
    function draw() {
      var ys = X.map(function (x, i) { return x + sigma * E[i]; });
      f = fit(X, ys);
      var up = { a: f.a + 2 * f.sd, b: f.b }, lo = { a: f.a - 2 * f.sd, b: f.b };
      var ds = chart.data.datasets;
      ds[0].data = X.map(function (x, i) { return { x: x, y: ys[i] }; });
      ds[1].data = line(up, XL[0], XL[1]);
      ds[2].data = line(lo, XL[0], XL[1]);
      ds[3].data = line(f, XL[0], XL[1]);
      ds[4].data = [hpos()];
      chart.update("none");
      put(R.r, fmt(f.r, 2));
      put(R.r2, fmt(100 * f.r * f.r, 0) + "%");
      pressPresets(root, "data-s-preset", C.sigma, sigma);
    }

    handleDrag(canvas, chart, hpos, function (v) {
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

    paint(); draw(); Charts.onTheme(root, paint);
  }

  // ==========================================================================
  // Ryc. 4.4: r nie zależy od nachylenia
  // ==========================================================================
  function initSlope(root, canvas) {
    var C = JSON.parse(root.dataset.config), X = C.x, E = C.e, REL = C.rel;
    var BMIN = 0.1, BMAX = 2, HX = 2.5, XL = [-3, 3], YL = [-6, 6];
    var slope = C.slopes[C.start || 0], f = null;
    var R = reads(root, ["b", "sd", "r"]);

    var chart = makeChart(canvas, [
      { data: [], pointRadius: 3.5, borderWidth: 0 },
      { data: [], showLine: true, pointRadius: 0, borderWidth: 2.5 },
      { data: [], pointRadius: 8, borderWidth: 2 }
    ], {
      x: { min: XL[0], max: XL[1], ticks: { stepSize: 1 }, title: { display: true, text: "x" } },
      y: { min: YL[0], max: YL[1], ticks: { stepSize: 2 }, title: { display: true, text: "y" } }
    });

    function paint() {
      paintAxes(chart);
      var niebo = token("--upwr-cat-niebo") || "#1d5f9e", accent = token("--upwr-accent") || "#7a2e3b";
      var surface = token("--upwr-surface") || "#fff";
      var ds = chart.data.datasets;
      ds[0].backgroundColor = alpha(niebo, 0.65);
      ds[1].borderColor = accent;
      ds[2].backgroundColor = accent; ds[2].borderColor = surface;
      chart.update();
    }
    function hpos() { return { x: HX, y: f.a + f.b * HX }; }
    function draw() {
      var ys = X.map(function (x, i) { return slope * (x + REL * E[i]); });
      f = fit(X, ys);
      var ds = chart.data.datasets;
      ds[0].data = X.map(function (x, i) { return { x: x, y: ys[i] }; });
      ds[1].data = line(f, XL[0], XL[1]);
      ds[2].data = [hpos()];
      chart.update("none");
      put(R.b, fmt(f.b, 2));
      put(R.sd, fmt(f.sd, 2));
      put(R.r, fmt(f.r, 2));
      pressPresets(root, "data-s-preset", C.slopes, slope);
    }

    handleDrag(canvas, chart, hpos, function (v) {
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

  function initOutlier(root, canvas) {
    var N = parseInt(root.dataset.n || "50", 10), SD = parseFloat(root.dataset.sd || "3");
    var SHIFT = parseFloat(root.dataset.shift || "15");
    var base = [], outs = [], LO = -10, HI = 10;
    var R = reads(root, ["r", "r0", "k"]);

    var chart = makeChart(canvas, [
      { data: [], pointRadius: 4, borderWidth: 0 },
      { data: [], pointRadius: 8, borderWidth: 2 },
      { data: [], showLine: true, pointRadius: 0, borderWidth: 1.5, borderDash: [5, 4] },
      { data: [], showLine: true, pointRadius: 0, borderWidth: 2.5 }
    ], {
      x: { title: { display: true, text: "X" } },
      y: { title: { display: true, text: "Y" } }
    });

    function paint() {
      paintAxes(chart);
      var niebo = token("--upwr-cat-niebo") || "#1d5f9e", accent = token("--upwr-accent") || "#7a2e3b";
      var subtle = token("--upwr-ink-subtle") || "#888", surface = token("--upwr-surface") || "#fff";
      var ds = chart.data.datasets;
      ds[0].backgroundColor = alpha(niebo, 0.65);
      ds[1].backgroundColor = accent; ds[1].borderColor = surface;
      ds[2].borderColor = subtle;
      ds[3].borderColor = accent;
      chart.update();
    }

    // Wspólny zakres osi x i y z zapasem, krok 5; stały w trakcie przeciągania.
    function rescale() {
      var all = base.concat(outs), mn = Infinity, mx = -Infinity;
      all.forEach(function (p) { mn = Math.min(mn, p.x, p.y); mx = Math.max(mx, p.x, p.y); });
      var lo = Math.min(-10, mn - 2), hi = Math.max(10, mx + 4), step = hi - lo > 40 ? 10 : 5;
      LO = Math.floor(lo / step) * step;
      HI = Math.ceil(hi / step) * step;
      ["x", "y"].forEach(function (k) {
        var s = chart.options.scales[k];
        s.min = LO; s.max = HI; s.ticks.stepSize = step;
      });
    }

    function draw() {
      var all = base.concat(outs), ds = chart.data.datasets;
      var fa = fit(all.map(function (p) { return p.x; }), all.map(function (p) { return p.y; }));
      var f0 = fit(base.map(function (p) { return p.x; }), base.map(function (p) { return p.y; }));
      ds[0].data = base;
      ds[1].data = outs;
      ds[2].data = outs.length ? line(f0, LO, HI) : [];
      ds[3].data = line(fa, LO, HI);
      chart.update("none");
      put(R.r, fmt(fa.r, 3));
      put(R.r0, fmt(f0.r, 3));
      put(R.k, String(outs.length));
    }

    function fresh() {
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
      rescale(); draw();
    }

    // Przeciąganie: wciśnięcie łapie najbliższą wartość odstającą (w pikselach).
    var drag = -1;
    function nearest(p) {
      var best = -1, bd = 36;
      outs.forEach(function (o, i) { var d = pixelDist(chart, p, o.x, o.y); if (d < bd) { bd = d; best = i; } });
      return best;
    }
    canvas.style.touchAction = "none";
    Charts.listen(canvas, "pointerdown", function (e) {
      var i = nearest(pointer(canvas, e));
      if (i < 0) return;
      drag = i; canvas.setPointerCapture(e.pointerId); e.preventDefault();
    });
    Charts.listen(canvas, "pointermove", function (e) {
      var p = pointer(canvas, e);
      if (drag >= 0) {
        var v = valueAt(chart, p);
        outs[drag] = { x: Math.round(v.x * 10) / 10, y: Math.round(v.y * 10) / 10 };
        draw(); return;
      }
      canvas.style.cursor = nearest(p) >= 0 ? "move" : "default";
    });
    ["pointerup", "pointercancel"].forEach(function (t) {
      Charts.listen(canvas, t, function () { if (drag >= 0) { drag = -1; rescale(); draw(); } });
    });

    Charts.listen(root, "click", function (e) {
      if (e.target.closest("[data-o-new]")) fresh();
      else if (e.target.closest("[data-o-add]")) add();
    });

    paint(); fresh(); Charts.onTheme(root, paint);
  }

  // ==========================================================================
  // Ryc. 5.4: paradoks Simpsona
  // ==========================================================================
  function initSimpson(root, canvas) {
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

    var sets = [{ data: [], pointRadius: 4, borderWidth: 0 },
                { data: [], showLine: true, pointRadius: 0, borderWidth: 2.5 }];
    for (k = 0; k < K; k++) sets.push({ data: [], showLine: true, pointRadius: 0, borderWidth: 2.5 });
    var chart = makeChart(canvas, sets, {
      x: { min: C.xlim[0], max: C.xlim[1], ticks: { stepSize: 5 }, title: { display: true, text: "Staż (lata)" } },
      y: { min: C.ylim[0], max: C.ylim[1], ticks: { stepSize: 2 }, title: { display: true, text: "Pensja (tys. zł)" } }
    });
    chart.data.datasets[0].data = X.map(function (x, i) { return { x: x, y: Y[i] }; });

    function paint() {
      paintAxes(chart);
      COLS = C.colors.map(function (t) { return token(t) || "#888"; });
      draw();
    }

    function draw() {
      var ds = chart.data.datasets, split = view === "split";
      var grey = token("--upwr-ink-subtle") || "#888", sec = token("--upwr-secondary") || "#2d3b3d";
      ds[0].backgroundColor = G.map(function (g) { return split ? alpha(COLS[g], 0.8) : alpha(grey, 0.45); });
      ds[1].data = line(fall, sall[0], sall[1]);
      ds[1].borderColor = sec;
      ds[1].borderWidth = split ? 1.5 : 2.5;
      ds[1].borderDash = split ? [6, 4] : [];
      for (var k = 0; k < K; k++) {
        ds[2 + k].data = split ? line(fits[k], spans[k][0], spans[k][1]) : [];
        ds[2 + k].borderColor = COLS[k];
      }
      chart.update("none");

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

    paint(); Charts.onTheme(root, paint);
  }

  // ==========================================================================
  var KINDS = { "lc-strength": initStrength, "lc-slope": initSlope, "lc-outlier": initOutlier, "lc-simpson": initSimpson };

  function init(root) {
    var canvas = root.querySelector('canvas[data-role="plot"]');
    if (!canvas) return false;
    for (var cls in KINDS) if (root.classList.contains(cls)) { KINDS[cls](root, canvas); return; }
  }

  Charts.onReady(".lc-scat", init);
})();
