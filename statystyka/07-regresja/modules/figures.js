// Ryciny wykładu 07 „Regresja” liczone w przeglądarce (dawniej wykresy ggplot i wyjścia
// serwera). Silnik: window.lcSF (shared/lc_stat_figures.js), rozkłady i losowanie:
// window.lcDist, testy i regresja: window.lcStats (shared/lc_stats.js).
// Ryciny na stałych danych i o skończonej liczbie ustawień opisuje R (lc_sf_spec()
// w modules/figures.R); tutaj jest to, co zależy od losowania, suwaków i kliknięć.
//
// Funkcje liczące są w window.lcS07 (testy: tests/js/stat07.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist, T = root.lcStats;
  if (!D || !T) return;

  // Regresja prosta MNK: nachylenie, wyraz wolny, r, reszty i test nachylenia.
  function simpleFit(x, y) {
    var n = x.length, mx = T.mean(x), my = T.mean(y), sxx = 0, sxy = 0, syy = 0, i;
    for (i = 0; i < n; i++) { sxx += (x[i] - mx) * (x[i] - mx); sxy += (x[i] - mx) * (y[i] - my); syy += (y[i] - my) * (y[i] - my); }
    var b1 = sxy / sxx, b0 = my - b1 * mx, sse = 0;
    for (i = 0; i < n; i++) { var e = y[i] - b0 - b1 * x[i]; sse += e * e; }
    var sigma = Math.sqrt(sse / (n - 2)), se1 = sigma / Math.sqrt(sxx), se0 = sigma * Math.sqrt(1 / n + mx * mx / sxx), t1 = b1 / se1;
    return { n: n, b0: b0, b1: b1, r: sxy / Math.sqrt(sxx * syy), r2: 1 - sse / syy, sse: sse, sst: syy, sigma: sigma,
             se0: se0, se1: se1, t1: t1, p1: 2 * D.pt(-Math.abs(t1), n - 2), mx: mx, my: my, sx: Math.sqrt(sxx / (n - 1)), sy: Math.sqrt(syy / (n - 1)), sxx: sxx };
  }
  root.lcS07 = { simpleFit: simpleFit };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S) return;

  var rng = D.rng(), figures = {}, fmt = S.fmt;
  function b(x) { return "<b>" + S.esc(String(x)) + "</b>"; }
  function num(el, id) { return Number(el.querySelector("#" + id).value); }
  function range(v) { return [Math.min.apply(null, v), Math.max.apply(null, v)]; }
  function pad(r, share) { var d = (r[1] - r[0]) * (share == null ? 0.05 : share) || 1; return [r[0] - d, r[1] + d]; }
  // Role elementów kroków jak STEP_ROLES w shared/theme_upwr.R.
  function roleColor(P, role) { return role === "new" ? P.accent : role === "known" ? P.secondary : P.niebo; }
  function stepRole(step, from) { return step === from ? "new" : "known"; }

  // ---- Rozdział 1: regresja liniowa ---------------------------------------------
  // Ryc. 1.2: prosta o zadanych β₀ i β₁ z szumem o odchyleniu σ (te same losowania co w R: config.z).
  figures.s07_beta = {
    draw: function (v, P, k, state, vals) {
      var b0 = Number(vals.ch1_beta_b0), b1 = Number(vals.ch1_beta_b1), sigma = Number(vals.ch1_beta_sigma), z = state.config.z, pts = [], ys = [0, b0];
      z.forEach(function (e, i) { var x = 10 * i / (z.length - 1), y = b0 + b1 * x + sigma * e; pts.push([x, y]); ys.push(y, b0 + b1 * x); });
      return { x: [-0.5, 10.5], y: pad(range(ys)), yNegative: true,
        layers: [{ type: "points", data: pts, color: P.secondary, size: 6, alpha: 0.45 },
                 { type: "line", data: [[0, b0], [10, b0 + 10 * b1]], color: P.niebo, width: 2.8 },
                 { type: "vline", x: 0, from: Math.min(0, b0), to: Math.max(0, b0), color: P.bursztyn, width: 2.4 },
                 { type: "text", x: 0.6, y: b0, text: "β₀ = " + b0, color: P.bursztyn, bold: true, align: "left", valign: "middle" }],
        xName: "X", yName: "Y", aria: "Prosta o zadanym wyrazie wolnym i nachyleniu z punktami rozproszonymi przez szum." };
    },
    update: function (el) {
      var b1 = num(el, "ch1_beta_b1"), box = el.querySelector('[data-role="info"]');
      var change = b1 > 0 ? "rośnie o " + Math.abs(b1) : b1 < 0 ? "maleje o " + Math.abs(b1) : "nie zmienia się";
      if (box) box.innerHTML = S.status("<p><strong>Interpretacja:</strong> gdy X wzrasta o 1, oczekiwane Y " + change +
        ". Szum ε = " + num(el, "ch1_beta_sigma") + " rozprasza punkty wokół linii.</p>");
    }
  };

  // Dane jak generate_regression_data() w helpers.R (wariant „custom”): X ~ N(0, 3).
  function round2(v) { return Math.round(v * 100) / 100; }
  function regData(n, b0, b1, sigma) {
    var x = [], y = [];
    for (var i = 0; i < n; i++) { var xi = round2(rng.norm(0, 3)); x.push(xi); y.push(round2(b0 + b1 * xi + rng.norm(0, sigma))); }
    return { x: x, y: y };
  }
  function pairs(x, y) { return x.map(function (v, i) { return [v, y[i]]; }); }
  function stepOf(root) { var s = root.querySelector(".lc-stepper"); return s ? Number(s.getAttribute("data-lc-step")) || 1 : 1; }
  // Wzór złożony w HTML (strona statyczna nie ładuje MathJax): ułamek = .lc-hfrac.
  function frac(n, d) { return '<span class="lc-hfrac"><span class="lc-hfrac-n">' + n + '</span><span class="lc-hfrac-d">' + d + "</span></span>"; }
  function formula(html) { return '<div class="lc-formula-box"><span class="lc-hmath">' + html + "</span></div>"; }
  function iv(name, sub) { return "<i>" + name + "</i>" + (sub ? "<sub>" + sub + "</sub>" : ""); }
  function f(v, d) { return C.fmt(v, d); }
  function caption(text, tone) { return '<div class="lc-caption"' + (tone ? ' data-tone="' + tone + '"' : "") + "><div>" + text + "</div></div>"; }
  function readout(label, value, color) {
    return '<div class="lc-read" style="--lc-read-color:' + color + ';"><span class="lc-read-label">' + S.esc(label) +
      '</span><span class="lc-read-value">' + S.esc(String(value)) + "</span></div>";
  }
  var DASH = [6, 5], HELPER = [3, 3];

  // Ryc. 1.3: regresja z korelacji. Pierwszy krok paska („Dane”) to stan 0: sama chmura.
  figures.s07_corr = {
    init: function (state) { state.d = regData(65, 8, 1.6, 4); },
    act: function (state) { state.d = regData(65, 8, 1.6, 4); },
    draw: function (v, P, k, state, vals, pos) {
      var d = state.d, fit = simpleFit(d.x, d.y), step = (pos || 1) - 1, xb = fit.mx, yb = fit.my, b1 = fit.b1, b0 = fit.b0;
      var xmin = Math.min.apply(null, d.x), ymax = Math.max.apply(null, d.y);
      var xr = pad(range(d.x.concat([0]))), yr = pad(range(d.y.concat([0, b0, b1 * xmin])));
      var layers = [{ type: "points", data: pairs(d.x, d.y), color: P.niebo, alpha: step === 4 ? 0.22 : 0.7, size: step === 4 ? 6 : 7 }];
      function abline(a, s, color, width, dash) { return { type: "line", data: [[xr[0] - 50, a + s * (xr[0] - 50)], [xr[1] + 50, a + s * (xr[1] + 50)]], color: color, width: width, dash: dash }; }
      if (step >= 1 && step !== 4 && step !== 5) {
        var c = roleColor(P, stepRole(step, 1));
        layers.push({ type: "vline", x: xb, color: c, width: 1.4, dash: HELPER },
                    { type: "line", data: [[xr[0] - 50, yb], [xr[1] + 50, yb]], color: c, width: 1.4, dash: HELPER });
      }
      if (step === 2) {
        var segs = [];
        d.x.forEach(function (x, i) { segs.push({ from: [x, d.y[i]], to: [xb, d.y[i]] }, { from: [x, d.y[i]], to: [x, yb] }); });
        layers.push({ type: "segs", items: segs, color: P.accent, width: 0.9, alpha: 0.35, dash: [2, 2] },
          { type: "text", x: xb, y: ymax, text: "  odchylenia X", color: P.accent, bold: true, align: "left", valign: "top" },
          { type: "text", x: xmin, y: yb, text: "odchylenia Y", color: P.accent, bold: true, align: "left", valign: "bottom" });
      }
      if (step === 3) layers.push(abline(b0, b1, P.accent, 2.6));
      var spec = { x: xr, y: yr, yNegative: true, layers: layers, xName: "X", yName: "Y",
        aria: "Wykres rozrzutu z budową prostej regresji ze średnich, odchyleń standardowych i korelacji." };
      if (step === 4) {
        var sx0 = xb + 0.4, sx1 = sx0 + 1, sy0 = b0 + b1 * sx0, sy1 = b0 + b1 * sx1, mid = (sy0 + sy1) / 2, py = Math.max(1.8, Math.abs(b1) * 1.4);
        spec.x = [sx0 - 1.1, sx1 + 1.45]; spec.y = [mid - py, mid + py];
        xr = spec.x;
        layers.push(abline(b0, b1, P.secondary, 3),
          { type: "arrows", items: [{ from: [sx0, sy0], to: [sx1, sy0] }, { from: [sx1, sy0], to: [sx1, sy1] }], color: P.accent, width: 2.2, head: 0.8 },
          { type: "points", data: [[sx0, sy0], [sx1, sy0], [sx1, sy1]], color: P.secondary, size: 11 },
          { type: "points", data: [[sx0, sy0], [sx1, sy0], [sx1, sy1]], color: P.surface, size: 7, z: 6 },
          { type: "text", x: (sx0 + sx1) / 2, y: sy0 - 0.08 * 2 * py, text: "ΔX = 1", color: P.accent, bold: true, valign: "top" },
          { type: "text", x: sx1 + 0.08, y: mid, text: "ΔY = b₁ = " + fmt(b1, 2), color: P.accent, bold: true, align: "left", valign: "middle" });
      }
      if (step === 5) {
        layers.push(abline(0, b1, P.secondary, 1.8, HELPER), abline(b0, b1, P.secondary, 2.6),
          { type: "arrows", both: true, items: [{ from: [0, 0], to: [0, b0] }], color: P.accent, width: 2.2, head: 0.8 },
          { type: "points", data: [[0, 0], [0, b0]], color: P.secondary, size: 10 },
          { type: "points", data: [[0, 0], [0, b0]], color: P.surface, size: 6, z: 6 },
          { type: "text", x: xmin, y: b1 * xmin, text: "b₀ = 0", color: P.secondary, bold: true, align: "left", valign: "bottom" },
          { type: "text", x: 0.15, y: b0 / 2, text: "  b₀ = " + fmt(b0, 2), color: P.accent, bold: true, align: "left", valign: "middle" });
      }
      return spec;
    },
    // Opis kroku: statystyki z dotychczasowych kroków.
    text: function (state, v, vals, pos) {
      var d = state.d, fit = simpleFit(d.x, d.y), step = (pos || 1) - 1;
      if (step === 0) return "Chmura " + d.x.length + " punktów: każdy punkt to jedna obserwacja (X, Y).";
      var stats = [["x̄", fmt(fit.mx, 2)], ["ȳ", fmt(fit.my, 2)]];
      if (step >= 2) stats.push(["sX", fmt(fit.sx, 2)], ["sY", fmt(fit.sy, 2)]);
      if (step >= 3) stats.push(["r", fmt(fit.r, 3)]);
      return stats.map(function (s) { return S.esc(s[0]) + " = " + b(s[1]); }).join(", ") + ".";
    },
    // Wzory nachylenia i wyrazu wolnego pod opisem kroku (kroki 4–5).
    update: function (root, state) {
      var box = root.querySelector('[data-role="info"]'), step = stepOf(root) - 1;
      if (!box) return;
      if (step < 4) { box.innerHTML = ""; return; }
      var fit = simpleFit(state.d.x, state.d.y);
      var html = formula(iv("b", "1") + " = " + iv("r") + " · " + frac(iv("s", "Y"), iv("s", "X")) + " = " + f(fit.r, 3) + " · " +
        frac(f(fit.sy, 2), f(fit.sx, 2)) + " = " + f(fit.b1, 3));
      if (step >= 5) {
        html += formula(iv("b", "0") + " = <i>ȳ</i> − " + iv("b", "1") + "<i>x̄</i> = " + f(fit.my, 2) + " − " + f(fit.b1, 3) + " · " +
          f(fit.mx, 2) + " = " + f(fit.b0, 2));
        html += S.status("<p><strong>Końcowy model:</strong></p>" + formula("<i>Ŷ</i> = " + f(fit.b0, 2) + (fit.b1 < 0 ? " − " : " + ") +
          f(Math.abs(fit.b1), 3) + "<i>X</i>"));
      }
      box.innerHTML = html;
    }
  };

  // Ryc. 1.5: MNK krok po kroku (kroki 1–6); nowa próba nie zmienia kroku.
  function olsParts(d) {
    var fit = simpleFit(d.x, d.y), altB1 = fit.b1 * 0.45, altB0 = fit.my - altB1 * fit.mx, altSse = 0;
    d.x.forEach(function (x, i) { var e = d.y[i] - altB0 - altB1 * x; altSse += e * e; });
    return { fit: fit, altB0: altB0, altB1: altB1, altSse: altSse };
  }
  figures.s07_ols = {
    init: function (state) { state.d = regData(70, 4, 1.4, 4); },
    act: function (state) { state.d = regData(70, 4, 1.4, 4); },
    draw: function (v, P, k, state, vals, step) {
      var d = state.d, o = olsParts(d), fit = o.fit, xr0 = range(d.x), fitted = d.x.map(function (x) { return fit.b0 + fit.b1 * x; });
      var alt = d.x.map(function (x) { return o.altB0 + o.altB1 * x; }), ymax = Math.max.apply(null, d.y), yr0 = range(d.y);
      var xr = pad(xr0), yr = pad(range(d.y.concat(fitted, alt)));
      var layers = [{ type: "points", data: pairs(d.x, d.y), color: P.niebo, alpha: 0.7, size: 7 }];
      if (step >= 2) layers.push({ type: "line", data: [[xr[0], fit.my], [xr[1], fit.my]], color: roleColor(P, stepRole(step, 2)), width: 1.4, dash: HELPER });
      if (step >= 3) layers.push({ type: "line", data: [[xr0[0], fit.b0 + fit.b1 * xr0[0]], [xr0[1], fit.b0 + fit.b1 * xr0[1]]], color: roleColor(P, stepRole(step, 3)), width: 2.6 });
      if (step >= 4) layers.push({ type: "segs", items: d.x.map(function (x, i) { return { from: [x, d.y[i]], to: [x, fitted[i]] }; }), color: roleColor(P, stepRole(step, 4)), width: 1.2, alpha: 0.35 });
      if (step >= 6) {
        layers.push({ type: "line", data: [[xr[0], o.altB0 + o.altB1 * xr[0]], [xr[1], o.altB0 + o.altB1 * xr[1]]], color: P.accent, width: 2.2, dash: DASH },
          { type: "segs", items: d.x.map(function (x, i) { return { from: [x, d.y[i]], to: [x, alt[i]] }; }), color: P.accent, width: 1.2, alpha: 0.22 },
          { type: "text", x: xr0[0], y: ymax, text: "inna prosta", color: P.accent, bold: true, align: "left", valign: "top" },
          { type: "text", x: xr0[0], y: ymax - 0.1 * (yr0[1] - yr0[0]), text: "MNK", color: P.secondary, bold: true, align: "left", valign: "top" });
      }
      return { x: xr, y: yr, yNegative: true, layers: layers, xName: "X", yName: "Y",
        aria: "Wykres rozrzutu z budową prostej metodą najmniejszych kwadratów." };
    },
    text: function (state, v, vals, step) {
      var o = olsParts(state.d), fit = o.fit, ss = "SS<sub>res</sub>";
      if (step === 6) {
        return ss + " prostej MNK = " + b(fmt(fit.sse, 1)) + ", " + ss + " innej prostej = " + b(fmt(o.altSse, 1)) +
          " (+" + fmt((o.altSse / fit.sse - 1) * 100, 1) + "%).";
      }
      return { 1: "Najpierw mamy tylko punkty: pary obserwacji X i Y.",
               2: "Pozioma linia to średnia Y. To najprostszy model bez predyktora.",
               3: "Prosta MNK: najmniejsza suma kwadratów pionowych odległości od punktów.",
               4: "Każdy odcinek to reszta: obserwacja minus predykcja.",
               5: "Model: Ŷ = " + b(fmt(fit.b0, 2)) + " + " + b(fmt(fit.b1, 2)) + "X; " + ss + " = " + b(fmt(fit.sse, 1)) + "." }[step];
    }
  };

  // Ćwiczenie: narysuj prostą z tabeli współczynników. Kliknięcie na wykresie
  // zamienia się na współrzędne osi (ECharts convertFromPixel).
  var DRAW_SLOPES = [-1.6, -1.2, -0.8, 0.8, 1.2, 1.6];
  function drawModel() {
    var beta0 = rng.unif(2.5, 8.5), beta1 = DRAW_SLOPES[rng.int(DRAW_SLOPES.length) - 1], x = [], y = [];
    for (var i = 0; i < 35; i++) { var xi = rng.unif(-4.5, 4.5); x.push(xi); y.push(beta0 + beta1 * xi + rng.norm(0, 1.2)); }
    return { beta0: beta0, beta1: beta1, x: x, y: y };
  }
  function drawReset(state) { state.pts = []; state.revealed = false; }
  function userLine(pts) {
    if (pts.length < 2 || Math.abs(pts[1][0] - pts[0][0]) < 0.05) return null;
    var b1u = (pts[1][1] - pts[0][1]) / (pts[1][0] - pts[0][0]);
    return { b1: b1u, b0: pts[0][1] - b1u * pts[0][0] };
  }
  figures.s07_draw = {
    init: function (state) { state.m = drawModel(); drawReset(state); },
    act: function (state, action) {
      if (action === "new") { state.m = drawModel(); drawReset(state); }
      else if (action === "reset") drawReset(state);
      else if (action === "reveal" && state.pts.length === 2) state.revealed = true;
    },
    draw: function (v, P, k, state) {
      var m = state.m, pts = state.pts, X = [-5, 5], Y = [-4, 17], top = Y[1], layers = [
        { type: "line", data: [[X[0], 0], [X[1], 0]], color: P.rule, width: 1.4 },
        { type: "vline", x: 0, color: P.rule, width: 1.4 }];
      if (state.revealed) layers.push({ type: "points", data: pairs(m.x, m.y), color: P.secondary, alpha: 0.45, size: 7 });
      if (pts.length) {
        layers.push({ type: "points", data: pts, color: P.terakota, size: 13, z: 6 }, { type: "points", data: pts, color: P.surface, size: 8, z: 7 });
        pts.forEach(function (p, i) { layers.push({ type: "text", x: p[0], y: p[1] + 0.6, text: String(i + 1), color: P.terakota, bold: true, valign: "bottom" }); });
      }
      var u = userLine(pts);
      if (u) {
        layers.push({ type: "line", data: [[X[0], u.b0 + u.b1 * X[0]], [X[1], u.b0 + u.b1 * X[1]]], color: P.terakota, width: 2.8, dash: [10, 5] });
        if (state.revealed) {
          layers.push({ type: "line", data: [[X[0], m.beta0 + m.beta1 * X[0]], [X[1], m.beta0 + m.beta1 * X[1]]], color: P.niebo, width: 3 },
            { type: "text", x: X[0] + 0.25, y: top - 0.8, text: "poprawna prosta", color: P.niebo, bold: true, align: "left", valign: "middle" },
            { type: "text", x: X[0] + 0.25, y: top - 1.8, text: "prosta dopasowana ręcznie", color: P.terakota, bold: true, align: "left", valign: "middle" });
        } else {
          layers.push({ type: "text", x: X[0] + 0.25, y: top - 0.8, text: "prosta dopasowana ręcznie", color: P.terakota, bold: true, align: "left", valign: "middle" },
            { type: "text", x: 0, y: top - 1, text: "Kliknij „Pokaż odpowiedź”", color: P.ref, size: 1.3, valign: "middle" });
        }
      } else if (pts.length === 2) {
        layers.push({ type: "text", x: 0, y: top - 1, text: "Wybierz punkty bardziej oddalone poziomo", color: P.terakota, size: 1.3, valign: "middle" });
      } else {
        layers.push({ type: "text", x: 0, y: top - 1, text: "Kliknij dwa punkty na wykresie", color: P.ref, size: 1.3, valign: "middle" });
      }
      return { x: X, y: Y, yNegative: true, xLabelEvery: 1, xInterval: 1, yInterval: 1, layers: layers, xName: "X", yName: "Y",
        aria: "Pusty układ współrzędnych do zaznaczenia dwóch punktów prostej z tabeli współczynników." };
    },
    update: function (root, state) {
      var m = state.m, pts = state.pts;
      // Kliknięcie: jeden słuchacz na kontenerze wykresu; trzeci punkt zaczyna od nowa.
      if (!root._s07draw) {
        var el = root.querySelector(".lc-ec");
        root._s07draw = true;
        el.style.cursor = "crosshair";
        el.addEventListener("click", function (e) {
          var inst = window.echarts && window.echarts.getInstanceByDom(el);
          if (!inst || state.revealed) return;
          var r = el.getBoundingClientRect(), px = [e.clientX - r.left, e.clientY - r.top];
          if (!inst.containPixel({ gridIndex: 0 }, px)) return;
          var c = inst.convertFromPixel({ gridIndex: 0 }, px);
          if (state.pts.length >= 2) state.pts = [c]; else state.pts.push(c);
          root.dispatchEvent(new CustomEvent("lc:sf-redraw"));
        });
      }
      S.setValue(root, "draw_b0", f(m.beta0, 2));
      S.setValue(root, "draw_b1", f(m.beta1, 2));
      var fb = root.querySelector('[data-role="feedback"]'), st = root.querySelector('[data-role="stats"]'), P = C.palette();
      if (fb) fb.innerHTML = pts.length < 2 ? caption(pts.length === 0 ? "Kliknij pierwszy punkt prostej." : "Kliknij drugi punkt prostej.", "info")
        : !state.revealed ? caption("Gotowe. Kliknij „Pokaż odpowiedź”, żeby porównać z modelem.")
        : caption("Porównaj czerwoną przerywaną prostą z niebieską poprawną prostą.", "ok");
      if (!st) return;
      if (pts.length < 2 || !state.revealed) { st.innerHTML = ""; return; }
      var u = userLine(pts);
      st.innerHTML = !u ? caption("Punkty mają prawie ten sam X. Wybierz dwa punkty bardziej oddalone poziomo.")
        : readout("Wybrane b₀", fmt(u.b0, 2), P.terakota) + readout("Poprawne b₀", fmt(m.beta0, 2), P.niebo) +
          readout("Wybrane b₁", fmt(u.b1, 2), P.terakota) + readout("Poprawne b₁", fmt(m.beta1, 2), P.niebo);
    }
  };

  // Rycina „opis z R” z odpowiedzią na przycisk: [data-reveal="before"] do kliknięcia,
  // [data-reveal="after"] po nim; zmiana wariantu (kontrolek) chowa odpowiedź.
  var SPEC = S.figures.lc_spec;
  function showReveal(root, on) {
    root.querySelectorAll('[data-reveal="before"]').forEach(function (n) { n.hidden = on; });
    root.querySelectorAll('[data-reveal="after"]').forEach(function (n) { n.hidden = !on; });
  }
  figures.s07_reveal = {
    init: function (state) { state.revealed = false; },
    prepare: function (state, v, vals) {
      SPEC.prepare(state, v, vals);
      if (state.key !== state.lastKey) { state.revealed = false; state.lastKey = state.key; }
    },
    act: function (state, action) { if (action === "reveal") state.revealed = true; },
    draw: function (v, P, k, state) { return SPEC.draw(v, P, k, state); },
    update: function (root, state) { SPEC.update(root, state); showReveal(root, state.revealed); }
  };

  // Ryc. 1.9: predykcja z gotowego modelu CASchools dla wpisanej wartości X.
  // Wykres bazowy opisuje R (config.variants), współczynniki i teksty są w config.cases.
  function predCase(state, vals) { return state.config.cases[vals.ch1_pred_case] || state.config.cases[Object.keys(state.config.cases)[0]]; }
  function predX(root, cs) {
    var inp = root.querySelector("#ch1_pred_x"), x = inp ? Number(inp.value) : NaN;
    return isFinite(x) && inp.value !== "" ? x : cs.default;
  }
  figures.s07_pred = {
    init: function (state) { state.revealed = false; },
    prepare: function (state, v, vals, root) {
      SPEC.prepare(state, v, vals);
      var cs = predCase(state, vals), inp = root.querySelector("#ch1_pred_x");
      // Nowy model: pole X dostaje jego jednostkę, zakres i wartość domyślną.
      if (inp && state.caseKey !== state.key) {
        if (state.caseKey != null) inp.value = cs.default;
        inp.min = cs.min; inp.max = cs.max; inp.step = cs.step;
        var lab = root.querySelector('label[for="ch1_pred_x"]');
        if (lab) lab.textContent = "Wartość X (" + cs.unit + "):";
        state.caseKey = state.key;
      }
      var key = state.key + "|" + predX(root, cs);
      if (key !== state.lastKey) { state.revealed = false; state.lastKey = key; }
      state.cs = cs; state.x0 = predX(root, cs);
    },
    act: function (state, action) { if (action === "reveal") state.revealed = true; },
    draw: function (v, P, k, state) {
      var spec = SPEC.draw(v, P, k, state), cs = state.cs, x0 = state.x0;
      if (!spec || !state.revealed) return spec;
      var yhat = cs.b0 + cs.b1 * x0;
      spec.layers = spec.layers.concat([
        { type: "vline", x: x0, color: P.bursztyn, width: 1.8, dash: DASH },
        { type: "line", data: [[spec.x[0], yhat], [spec.x[1], yhat]], color: P.terakota, width: 1.8, dash: DASH },
        { type: "points", data: [[x0, yhat]], color: P.terakota, size: 12, z: 6 },
        { type: "points", data: [[x0, yhat]], color: P.surface, size: 7, z: 7 },
        { type: "text", x: x0, y: yhat, text: "   Ŷ = " + fmt(yhat, 1), color: P.terakota, bold: true, align: "left", valign: "bottom" }]);
      return spec;
    },
    update: function (root, state) {
      var cs = state.cs, x0 = state.x0, P = C.palette();
      var set = function (role, html) { var n = root.querySelector('[data-role="' + role + '"]'); if (n) n.innerHTML = html; };
      set("table", cs.table);
      set("question", S.status("<p><strong>Pytanie: </strong>" + S.esc(cs.question) + "</p><p>Podstaw do równania wartość X = <strong>" +
        S.esc(String(x0)) + "</strong> i spróbuj policzyć przewidywane Y.</p>"));
      if (!state.revealed) {
        set("answer", caption("Odpowiedź jest ukryta. Najpierw policz predykcję z tabeli współczynników."));
        set("stats", "");
        return;
      }
      // Liczymy z wartości pokazanych w tabeli, żeby rachunek ręczny się zgadzał.
      var yTab = Number(cs.b0txt) + Math.sign(cs.b1) * Number(cs.b1abs) * x0, yhat = cs.b0 + cs.b1 * x0;
      set("answer", S.status("<p><strong>Odpowiedź:</strong></p>" +
        formula("<i>Ŷ</i> = " + cs.b0txt + (cs.b1 < 0 ? " − " : " + ") + cs.b1abs + " · " + S.esc(String(x0)) + " = " + f(yTab, 2)) +
        "<p>Dla " + S.esc(cs.xlab) + " = " + S.esc(String(x0)) + " przewidywane " + S.esc(cs.ylab) + " wynosi " + f(yTab, 2) + ".</p>"));
      set("stats", readout("b₀", cs.b0txt, P.secondary) + readout("b₁", cs.b1txt, P.szalwia) +
        readout("X: " + cs.xlab, fmt(x0, 2), P.bursztyn) + readout("Ŷ: " + cs.ylab, fmt(yhat, 2), P.terakota));
    }
  };

  // Ryc. 2.7: ekstrapolacja modelu czytanie ~ dochód poza zakres danych (suwak dochodu).
  function verdict(type, title, body) {
    return S.status('<span class="lc-status-' + type + '"><strong>' + title + "</strong></span><p>" + body + "</p>");
  }
  figures.s07_extrap = {
    draw: function (v, P, k, state, vals) {
      var c = state.config, xv = Number(vals.ch2_extrap_x), lo = c.range[0], hi = c.range[1];
      var g0 = Math.min(1, xv - 2), g1 = Math.max(80, xv + 2), at = function (x) { return c.b0 + c.b1 * x; };
      var col = xv >= lo && xv <= hi ? P.niebo : P.terakota;
      var xr = pad([g0, g1]), yr = pad(range(c.y.concat([at(g0), at(g1)])));
      return { x: xr, y: yr, layers: [
        { type: "rect", x0: lo, x1: hi, y0: yr[0], y1: yr[1], fill: P.secondary, alpha: 0.08, z: 1 },
        { type: "points", data: pairs(c.x, c.y), color: P.secondary, alpha: 0.35, size: 5 },
        { type: "line", data: [[lo, at(lo)], [hi, at(hi)]], color: P.niebo, width: 2.2 },
        { type: "line", data: [[g0, at(g0)], [lo, at(lo)]], color: P.niebo, width: 2.2, dash: DASH },
        { type: "line", data: [[hi, at(hi)], [g1, at(g1)]], color: P.niebo, width: 2.2, dash: DASH },
        { type: "vline", x: xv, color: col, width: 1.8, dash: [2, 4] },
        { type: "points", data: [[xv, at(xv)]], color: col, size: 14, symbol: "diamond", z: 7 }],
        xName: "Dochód okręgu (tys. USD)", yName: "Wynik testu czytania",
        aria: "Dane CASchools z prostą regresji przedłużoną poza zakres dochodów i prognozą dla wybranego dochodu." };
    },
    update: function (root, state) {
      var c = state.config, xv = num(root, "ch2_extrap_x"), lo = c.range[0], hi = c.range[1], y = c.b0 + c.b1 * xv, P = C.palette();
      var inside = xv >= lo && xv <= hi, dist = Math.min(Math.abs(xv - lo), Math.abs(xv - hi)) / (hi - lo) * 100;
      var st = root.querySelector('[data-role="stats"]'), vd = root.querySelector('[data-role="verdict"]');
      if (st) st.innerHTML = readout("X podany", xv + " tys. USD", P.niebo) + readout("Predykcja", fmt(y, 1) + " pkt", P.bursztyn) +
        readout("Zakres X danych", C.fmt(lo, 0) + "–" + C.fmt(hi, 0) + " tys. USD", P.secondary);
      if (vd) vd.innerHTML = inside
        ? verdict("ok", "W zakresie danych", "Predykcja: " + f(y, 1) + " pkt. Jesteśmy wewnątrz zakresu danych — predykcja ma sens.")
        : verdict("warning", "Ekstrapolacja", "Predykcja: " + f(y, 1) + " pkt. Jesteśmy " + f(dist, 0) + "% zakresu danych poza granicą — brak gwarancji.");
    }
  };

  // Rycina krokowa opisana w R: config.variants[krok] (opis rysunku), config.texts[krok]
  // (opis kroku), config.html[krok] (fragmenty do [data-role]).
  figures.s07_steps = {
    prepare: function (state, v, vals, root) { state.key = String(stepOf(root)); },
    draw: function (v, P, k, state) { return SPEC.draw(v, P, k, state); },
    text: function (state) { return state.config.texts[state.key] || ""; },
    update: function (root, state) { SPEC.update(root, state); }
  };

  // Ryc. 3.1: model wieloraki CASchools. Tabela i miary każdego modelu są z R (config.html),
  // współczynniki w config.variants[klucz].coef; grupy kwantylowe i ich mediany też z R.
  // Pierwszy dodany predyktor koloruje grupy, kolejne dzielą wykres na panele.
  var GROUP_COLORS = ["szalwia", "bursztyn", "terakota"];
  function multiSetup(state, vals) {
    var c = state.config, preds = [].concat(vals.ch3_predictors || []), y = vals.ch3_outcome || "read";
    return { c: c, y: y, extra: preds, n: preds.length + 1, coef: (c.variants[state.key] || {}).coef || {} };
  }
  function multiPred(m, x, fixed) {
    var s = m.coef["(Intercept)"] + m.coef.income * x;
    m.extra.forEach(function (v) { s += m.coef[v] * (fixed[v] != null ? fixed[v] : m.c.means[v]); });
    return s;
  }
  function multiFacets(m) {
    // Panele: [{title, rows: indeksy obserwacji, fixed: {zmienna: mediana grupy}}]
    var n = m.c.data.income.length, all = [], i;
    for (i = 0; i < n; i++) all.push(i);
    if (m.n <= 2) return [{ title: "", rows: all, fixed: {} }];
    var f1 = m.extra[1], b1 = m.n === 3 ? m.c.bins3[f1] : m.c.bins2[f1], out = [];
    if (m.n === 3) {
      b1.levels.forEach(function (lev, a) {
        var fx = {}; fx[f1] = b1.med[a];
        out.push({ title: m.c.labels[f1] + " " + lev, rows: all.filter(function (j) { return b1.g[j] === a; }), fixed: fx });
      });
      return out;
    }
    var f2 = m.extra[2], b2 = m.c.bins2[f2];
    b1.levels.forEach(function (l1, a) {
      b2.levels.forEach(function (l2, bb) {
        var fx = {}; fx[f1] = b1.med[a]; fx[f2] = b2.med[bb];
        out.push({ title: m.c.labels[f1] + " " + l1 + " · " + m.c.labels[f2] + " " + l2,
                   rows: all.filter(function (j) { return b1.g[j] === a && b2.g[j] === bb; }), fixed: fx });
      });
    });
    return out;
  }
  function m0(state) { return state.config.variants[state.key] || {}; }
  // Tabela jak lc_table_split() w R: pełna na szerokim ekranie, na wąskim podzielona na grupy kolumn.
  function bareTable(cols, rows) { return S.table(cols, rows).replace(/^<div class="lc-tbl-block">/, "").replace(/<\/div>$/, ""); }
  function tableSplit(cols, rows, groups, label) {
    var narrow = groups.map(function (g) {
      var idx = [0].concat(g);
      return bareTable(idx.map(function (j) { return cols[j]; }), rows.map(function (r) { return idx.map(function (j) { return r[j]; }); }));
    }).join("");
    return '<div class="lc-tbl-block"><div class="lc-tbl-v-wide"><div class="lc-tbl-scroll" tabindex="0" role="region" aria-label="' +
      S.esc(label) + '">' + bareTable(cols, rows) + '</div></div><div class="lc-tbl-v-narrow">' + narrow + "</div></div>";
  }
  figures.s07_multi = {
    prepare: function (state, v, vals) { SPEC.prepare(state, v, vals); state.m = multiSetup(state, vals); },
    draw: function (v, P, k, state) {
      var m = state.m, c = m.c, X = c.data.income, Y = c.data[m.y];
      if (m.n > 4) return null;
      var xr = pad(range(X)), yr = pad(range(Y)), x0 = Math.min.apply(null, X), x1 = Math.max.apply(null, X);
      var common = { x: xr, y: yr, xName: c.labels.income, yName: c.labels[m.y] };
      if (k === 4) {
        if (m.n === 1) return null;
        var sb = c.simple[m.y];
        return Object.assign(common, { aria: "Linia regresji prostej i linia aktualnego modelu wielorakiego przy średnich wartościach pozostałych predyktorów.",
          layers: [{ type: "points", data: pairs(X, Y), color: P.secondary, alpha: 0.28, size: 5 },
            { type: "line", data: [[x0, sb[0] + sb[1] * x0], [x1, sb[0] + sb[1] * x1]], color: P.terakota, width: 2.2, dash: DASH },
            { type: "line", data: [[x0, multiPred(m, x0, {})], [x1, multiPred(m, x1, {})]], color: P.niebo, width: 2.2 }] });
      }
      var facets = multiFacets(m), fc = facets[k];
      if (!fc) return null;
      var layers = [];
      if (m.n === 1) {
        layers.push({ type: "points", data: pairs(X, Y), color: P.secondary, alpha: 0.45, size: 6 },
          { type: "line", data: [[x0, multiPred(m, x0, {})], [x1, multiPred(m, x1, {})]], color: P.niebo, width: 2.2 });
      } else {
        var cv = m.extra[0], b3 = c.bins3[cv], cols = GROUP_COLORS.map(function (n) { return P[n]; });
        layers.push({ type: "points", data: fc.rows.map(function (j) { return [X[j], Y[j]]; }), size: 6, alpha: 0.68,
                      colors: fc.rows.map(function (j) { return cols[b3.g[j]]; }) });
        b3.levels.forEach(function (lev, a) {
          var fx = Object.assign({}, fc.fixed); fx[cv] = b3.med[a];
          layers.push({ type: "line", data: [[x0, multiPred(m, x0, fx)], [x1, multiPred(m, x1, fx)]], color: cols[a], width: 2.2 });
        });
      }
      return Object.assign(common, { layers: layers, aria: "Wynik a dochód okręgu z liniami modelu dla grup pozostałych predyktorów." });
    },
    update: function (root, state) {
      SPEC.update(root, state);
      var coefs = root.querySelector('[data-role="coefs"]'), rows = (m0(state).rows) || [];
      if (coefs) coefs.innerHTML = tableSplit([{ label: "Zmienna", row: true }, { label: "Estymata", digits: 4 }, { label: "SE", digits: 4 },
        { label: "t", digits: 3 }, { label: "p" }], rows, [[1, 2], [3, 4]], "Współczynniki modelu");
      var m = state.m, c = m.c, facets = m.n > 4 ? [] : multiFacets(m), box = root.querySelector(".s07-facets");
      if (box) box.setAttribute("data-n", String(facets.length));
      for (var k = 0; k < 4; k++) {
        var h = root.querySelector('[data-role="facet' + k + '"]');
        if (!h) continue;
        h.textContent = facets[k] ? facets[k].title : "";
        h.parentNode.hidden = !facets[k];
      }
      var P = C.palette(), cmp = root.querySelector('[data-role="cmp-legend"]');
      if (cmp) {
        if (m.n >= 2 && m.n <= 4) S.legendItems(cmp, [{ text: "Regresja prosta", color: P.terakota, dash: true }, { text: "Aktualny model", color: P.niebo }]);
        else { cmp.textContent = ""; cmp.hidden = true; }
      }
      var legend = root.querySelector(".lc-sf-legend");
      if (!legend) return;
      if (m.n < 2 || m.n > 4) { legend.textContent = ""; legend.hidden = true; return; }
      var cv = m.extra[0];
      S.legendItems(legend, c.bins3[cv].levels.map(function (lev, a) { return { text: lev, color: P[GROUP_COLORS[a]] }; }));
      legend.insertAdjacentHTML("afterbegin", '<span class="lc-sf-legend-item"><b>' + S.esc(c.labels[cv]) + ":</b></span>");
    }
  };

  S.register(figures);
})();
