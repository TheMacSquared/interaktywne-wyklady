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
  // Dane jak generate_multi_data() w helpers.R.
  function multiData(n, rg) {
    var r = rg || rng, d = { ocena: [], godziny_nauki: [], frekwencja: [], stres: [], sen_h: [] };
    for (var i = 0; i < n; i++) {
      var g = r.unif(0, 40), f = r.unif(30, 100), s = r.unif(1, 10), h = r.norm(7, 1.5);
      var o = 1.5 + 0.04 * g + 0.015 * f - 0.08 * s + 0.1 * h + r.norm(0, 0.4);
      d.ocena.push(Math.min(Math.max(Math.round(o * 100) / 100, 2), 5));
      d.godziny_nauki.push(Math.round(g * 10) / 10); d.frekwencja.push(Math.round(f * 10) / 10);
      d.stres.push(Math.round(s * 10) / 10); d.sen_h.push(Math.round(h * 10) / 10);
    }
    return d;
  }
  // Miary jak compute_model_metrics(): R², R² skorygowany, AIC, BIC (logLik z σ), RMSE = √(SSE/n).
  function lmMetrics(columns, y) {
    var m = T.lm(columns, y);
    if (!m) return null;
    var p = columns.length + 1, n = y.length, sse = m.sigma * m.sigma * m.df;
    return { r2: m.r2, adjR2: m.adjR2, aic: m.aic, bic: m.aic + (Math.log(n) - 2) * (p + 1), rmse: Math.sqrt(sse / n), nParams: p };
  }
  // Wielomian ortogonalny jak poly(x, stopień) w R: trójczłonowa rekurencja na punktach uczących
  // (współczynniki alpha, norm2), dopasowanie MNK w tej bazie i przewidywanie dla nowych x.
  function polyFit(x, y, degree) {
    var n = x.length, alpha = [], norm2 = [1, n], basis = [x.map(function () { return 1; })], k, i;
    var xbar = T.mean(x);
    for (k = 0; k < degree; k++) {
      var pk = basis[k], sx = 0;
      for (i = 0; i < n; i++) sx += (x[i] - xbar) * pk[i] * pk[i];
      alpha.push(sx / norm2[k + 1]);
      var next = [];
      for (i = 0; i < n; i++) next.push((x[i] - xbar - alpha[k]) * pk[i] - (k > 0 ? norm2[k + 1] / norm2[k] * basis[k - 1][i] : 0));
      var s2 = 0;
      for (i = 0; i < n; i++) s2 += next[i] * next[i];
      if (!(s2 > 1e-12 * n)) return null;   // stopień ≥ liczba różnych punktów
      basis.push(next); norm2.push(s2);
    }
    var coef = basis.map(function (b, kk) { var s = 0; for (i = 0; i < n; i++) s += y[i] * b[i]; return s / norm2[kk + 1]; });
    function at(v) {
      var prev = 0, cur = 1, s = coef[0];
      for (var kk = 0; kk < degree; kk++) {
        var nx = (v - xbar - alpha[kk]) * cur - (kk > 0 ? norm2[kk + 1] / norm2[kk] * prev : 0);
        prev = cur; cur = nx; s += coef[kk + 1] * cur;
      }
      return s;
    }
    var sse = 0, my = T.mean(y), sst = 0;
    for (i = 0; i < n; i++) { var e = y[i] - at(x[i]); sse += e * e; sst += (y[i] - my) * (y[i] - my); }
    var p = degree + 1, r2 = 1 - sse / sst, aic = n * (Math.log(2 * Math.PI) + Math.log(sse / n) + 1) + 2 * (p + 1);
    return { predict: at, sse: sse, r2: r2, adjR2: 1 - (1 - r2) * (n - 1) / (n - p), aic: aic, bic: aic + (Math.log(n) - 2) * (p + 1) };
  }
  function rmseOn(fit, x, y) { var s = 0; for (var i = 0; i < x.length; i++) { var e = y[i] - fit.predict(x[i]); s += e * e; } return Math.sqrt(s / x.length); }
  // Dane jak generate_train_test_poly(): 35 uczących i 180 testowych z 3·sin(x) + N(0, 1).
  function sortNum(a) { return a.sort(function (p, q) { return p - q; }); }
  function trainTest(rg) {
    var r = rg || rng, tx = sortNum(r.many(35, function () { return r.unif(0, 10); })), sx = sortNum(r.many(180, function () { return r.unif(0, 10); }));
    return { train: { x: tx, y: tx.map(function (v) { return 3 * Math.sin(v) + r.norm(0, 1); }) },
             test: { x: sx, y: sx.map(function (v) { return 3 * Math.sin(v) + r.norm(0, 1); }) } };
  }

  // Regresja logistyczna jak glm(family = binomial): IRLS. X: kolumny macierzy planu (z kolumną
  // jedynek, jeśli model ma wyraz wolny), offset: opcjonalny składnik stały predyktora liniowego.
  // Zwraca współczynniki, SE i test Walda (z, p), dopasowane p, logLik, AIC i BIC.
  function invert(A) {
    var n = A.length, M = A.map(function (r, i) { return r.concat(A.map(function (_, j) { return i === j ? 1 : 0; })); }), i, j, k;
    for (k = 0; k < n; k++) {
      var piv = k;
      for (i = k + 1; i < n; i++) if (Math.abs(M[i][k]) > Math.abs(M[piv][k])) piv = i;
      var tmp = M[k]; M[k] = M[piv]; M[piv] = tmp;
      var d = M[k][k];
      if (!(Math.abs(d) > 1e-300)) return null;
      for (j = 0; j < 2 * n; j++) M[k][j] /= d;
      for (i = 0; i < n; i++) if (i !== k) { var f = M[i][k]; if (f) for (j = 0; j < 2 * n; j++) M[i][j] -= f * M[k][j]; }
    }
    return M.map(function (r) { return r.slice(n); });
  }
  function logitFit(X, y, offset) {
    var n = y.length, p = X.length, beta = null, eta = new Array(n), mu = new Array(n), it, i, j, k, info, rhs, dev = Infinity;
    function off(i) { return offset ? offset[i] : 0; }
    function devOf(m) { var s = 0; for (var r = 0; r < n; r++) s -= 2 * Math.log(Math.max(y[r] ? m[r] : 1 - m[r], 1e-300)); return s; }
    function setBeta(b) {
      for (var r = 0; r < n; r++) { var e = off(r); for (var c = 0; c < p; c++) e += X[c][r] * b[c]; eta[r] = e; mu[r] = 1 / (1 + Math.exp(-e)); }
    }
    // Start jak glm.fit: μ = (y + 0.5) / 2, η = logit(μ).
    for (i = 0; i < n; i++) { mu[i] = (y[i] + 0.5) / 2; eta[i] = Math.log(mu[i] / (1 - mu[i])); }
    for (it = 0; it < 100; it++) {
      // X'WX i X'Wz, z = η − offset + (y − μ)/w
      info = []; rhs = new Array(p).fill(0);
      for (j = 0; j < p; j++) info.push(new Array(p).fill(0));
      for (i = 0; i < n; i++) {
        var w = Math.max(mu[i] * (1 - mu[i]), 1e-12), z = eta[i] - off(i) + (y[i] - mu[i]) / w;
        for (j = 0; j < p; j++) { rhs[j] += X[j][i] * w * z; for (k = 0; k <= j; k++) info[j][k] += X[j][i] * w * X[k][i]; }
      }
      for (j = 0; j < p; j++) for (k = j + 1; k < p; k++) info[j][k] = info[k][j];
      if (!p) { dev = devOf(mu); break; }
      var inv = invert(info);
      if (!inv) return null;
      var next = inv.map(function (row) { return row.reduce(function (s, v, jj) { return s + v * rhs[jj]; }, 0); });
      setBeta(next);
      var newDev = devOf(mu), half = 0;
      // Połowienie kroku, gdy dewiancja rośnie (jak glm.fit).
      while (beta && !(newDev <= dev + 1e-10) && half++ < 30) {
        next = next.map(function (b, jj) { return (b + beta[jj]) / 2; });
        setBeta(next); newDev = devOf(mu);
      }
      beta = next;
      var done = Math.abs(newDev - dev) / (Math.abs(newDev) + 0.1) < 1e-10;
      dev = newDev;
      if (done) break;
    }
    // Macierz informacji w punkcie końcowym.
    info = [];
    for (j = 0; j < p; j++) info.push(new Array(p).fill(0));
    for (i = 0; i < n; i++) { var ww = Math.max(mu[i] * (1 - mu[i]), 1e-12); for (j = 0; j < p; j++) for (k = 0; k < p; k++) info[j][k] += X[j][i] * ww * X[k][i]; }
    var cov = p ? invert(info) : [], ll = -dev / 2;
    var coef = (beta || []).map(function (b, jj) { var se = Math.sqrt(cov[jj][jj]), z = b / se; return { estimate: b, se: se, z: z, p: 2 * D.pnorm(-Math.abs(z)) }; });
    return { coef: coef, fitted: mu.slice(), deviance: dev, logLik: ll, aic: dev + 2 * p, bic: dev + Math.log(n) * p, n: n };
  }
  // Przedział profilowy 95% dla współczynnika j (jak confint() dla glm): β, przy którym
  // dewiancja rośnie o qchisq(0.95, 1).
  function logitProfileCI(X, y, fit, j) {
    var target = fit.deviance + D.qchisq(0.95, 1), est = fit.coef[j].estimate, se = fit.coef[j].se;
    var rest = X.filter(function (_, k) { return k !== j; });
    function dev(c) { var f = logitFit(rest, y, X[j].map(function (v) { return c * v; })); return f ? f.deviance : Infinity; }
    function side(sign) {
      var a = est, b = est + sign * 2 * se, guard = 0;
      while (dev(b) < target && guard++ < 30) { a = b; b = est + (b - est) * 2; }
      for (var it = 0; it < 60; it++) { var m = (a + b) / 2; if (dev(m) < target) a = m; else b = m; }
      return (a + b) / 2;
    }
    return [side(-1), side(1)];
  }
  // Dane jak generate_logistic_data() w helpers.R.
  function logisticData(n, rg) {
    var r = rg || rng, d = { zdal_num: [], godziny_nauki: [], srednia_ocen: [] };
    for (var i = 0; i < n; i++) {
      var g = r.unif(0, 40), s = Math.min(Math.max(r.norm(3.5, 0.6), 2), 5), pr = 1 / (1 + Math.exp(-(-4 + 0.08 * g + 1.2 * s)));
      d.zdal_num.push(r.unif(0, 1) < pr ? 1 : 0);
      d.godziny_nauki.push(Math.round(g * 10) / 10); d.srednia_ocen.push(Math.round(s * 100) / 100);
    }
    return d;
  }
  function ones(n) { return new Array(n).fill(1); }

  root.lcS07 = { logitFit: logitFit, logitProfileCI: logitProfileCI, logisticData: logisticData, invert: invert, multiData: multiData, lmMetrics: lmMetrics, polyFit: polyFit, rmseOn: rmseOn, trainTest: trainTest, simpleFit: simpleFit };

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

  // ---- Rozdział 4: porównanie modeli ---------------------------------------------
  // Wykres-zachęta przed pierwszym kliknięciem (dawniej theme_void z napisem).
  function emptySpec(P, text) {
    return { x: [0, 1], y: [0, 1], hideY: true, xTicks: [], noXGrid: true, noYGrid: true, aria: text,
             layers: [{ type: "text", x: 0.5, y: 0.5, text: text, color: P.ref, size: 1.3, valign: "middle" }] };
  }
  function setRole(root, role, html) { var n = root.querySelector('[data-role="' + role + '"]'); if (n) n.innerHTML = html; }
  // Wyróżnienie najlepszych komórek tabeli (jak cell_class = "is-best" w lc_table()).
  function markBest(root, role, cells) {
    var rows = root.querySelectorAll('[data-role="' + role + '"] tbody tr');
    cells.forEach(function (c) { var tr = rows[c[0]]; if (tr) tr.children[c[1]].classList.add("is-best"); });
  }
  var STEP_SETS = [["godziny_nauki"], ["godziny_nauki", "frekwencja"], ["godziny_nauki", "frekwencja", "stres"],
                   ["godziny_nauki", "frekwencja", "stres", "sen_h"], ["godziny_nauki", "frekwencja", "stres", "sen_h", "szum"]];
  var METRIC_COLS = [{ label: "R²", digits: 3 }, { label: "R² skor.", digits: 3 }, { label: "AIC", digits: 1 }, { label: "BIC", digits: 1 }, { label: "RMSE", digits: 3 }];
  function metricRow(name, m) { return [name, m.r2, m.adjR2, m.aic, m.bic, m.rmse]; }

  // Ryc. 4.1: kolejne predyktory i szum — R² zawsze rośnie, R² skorygowany nie.
  figures.s07_build = {
    init: function (state) { state.rows = null; },
    act: function (state) {
      var d = multiData(150);
      d.szum = d.ocena.map(function () { return rng.norm(0, 1); });
      state.rows = STEP_SETS.map(function (set) { return { name: set.join(" + "), m: lmMetrics(set.map(function (v) { return d[v]; }), d.ocena) }; });
    },
    draw: function (v, P, k, state) {
      if (!state.rows) return emptySpec(P, "Kliknij „Buduj modele krok po kroku”");
      var r2 = state.rows.map(function (r, i) { return [i + 1, r.m.r2]; }), adj = state.rows.map(function (r, i) { return [i + 1, r.m.adjR2]; });
      var all = r2.concat(adj).map(function (p) { return p[1]; });
      return { x: [0.8, 5.2], y: pad(range(all)), xName: "Liczba predyktorów", yName: "Wartość", yNegative: true,
        xLabels: { 1: "1 pred.", 2: "2 pred.", 3: "3 pred.", 4: "4 pred.", 5: "+ szum" },
        legend: [{ text: "R²", color: P.niebo }, { text: "R² skorygowany", color: P.szalwia }],
        layers: [{ type: "line", data: r2, color: P.niebo, width: 2.4 }, { type: "points", data: r2, color: P.niebo, size: 10 },
                 { type: "line", data: adj, color: P.szalwia, width: 2.4 }, { type: "points", data: adj, color: P.szalwia, size: 10 }],
        aria: "R² i R² skorygowany dla modeli z rosnącą liczbą predyktorów." };
    },
    update: function (root, state) {
      setRole(root, "table", state.rows ? S.table([{ label: "Predyktory", row: true }].concat(METRIC_COLS),
        state.rows.map(function (r) { return metricRow(r.name, r.m); })) : "");
    }
  };

  // Ryc. 4.2: arena czterech modeli; panele miar z własną skalą Y (facet_wrap, scales = "free_y").
  var ARENA = [["1: nauka", ["godziny_nauki"]], ["2: nauka + frekw.", ["godziny_nauki", "frekwencja"]],
               ["3: nauka + frekw. + stres", ["godziny_nauki", "frekwencja", "stres"]],
               ["4: wszystkie", ["godziny_nauki", "frekwencja", "stres", "sen_h"]]];
  var ARENA_METRICS = [["adjR2", "R² skorygowany"], ["aic", "AIC"], ["bic", "BIC"], ["rmse", "RMSE"]];
  figures.s07_arena = {
    init: function (state) { state.rows = null; },
    act: function (state, action, root) {
      var d = multiData(num(root, "ch4_n"));
      state.rows = ARENA.map(function (a) { return { name: a[0], m: lmMetrics(a[1].map(function (v) { return d[v]; }), d.ocena) }; });
    },
    draw: function (v, P, k, state) {
      if (!state.rows) return k === 0 ? emptySpec(P, "Kliknij „Buduj i porównaj modele”") : null;
      var key = ARENA_METRICS[k][0], vals = state.rows.map(function (r) { return r.m[key]; }), cols = [P.niebo, P.szalwia, P.bursztyn, C.token("--upwr-cat-wrzos", "#7d5a8c")];
      var lo = Math.min(0, Math.min.apply(null, vals)), hi = Math.max(0, Math.max.apply(null, vals)), span = (hi - lo) || 1;
      return { x: [0.4, 4.6], y: [lo - (lo < 0 ? 0.05 * span : 0), hi + (hi > 0 ? 0.05 * span : 0)], yNegative: true,
        xLabels: { 1: "1", 2: "2", 3: "3", 4: "4" },
        legend: k === 0 ? ARENA.map(function (a, i) { return { text: a[0], color: cols[i], box: true }; }) : undefined,
        layers: [{ type: "bins", data: vals.map(function (val, i) { return [i + 0.55, i + 1.45, val]; }), colors: cols, alpha: 0.8 }],
        xName: "", yName: k === 0 ? "Wartość" : "", aria: ARENA_METRICS[k][1] + " czterech modeli." };
    },
    update: function (root, state) {
      root.querySelectorAll("[data-arena-head]").forEach(function (h) { h.hidden = !state.rows; });
      if (!state.rows) { setRole(root, "table", ""); return; }
      setRole(root, "table", S.table([{ label: "Model", row: true }].concat(METRIC_COLS), state.rows.map(function (r) { return metricRow(r.name, r.m); })));
      function arg(key, best) { var b = 0; state.rows.forEach(function (r, i) { if (best(r.m[key], state.rows[b].m[key])) b = i; }); return b; }
      // RMSE na danych uczących zawsze wskazuje największy model — bez wyróżnienia.
      markBest(root, "table", [[arg("adjR2", function (a, c) { return a > c; }), 2], [arg("aic", function (a, c) { return a < c; }), 3],
                               [arg("bic", function (a, c) { return a < c; }), 4]]);
    }
  };

  // Ryc. 4.3: wielomian wybranego stopnia na wylosowanych punktach.
  figures.s07_poly = {
    init: function (state) { state.d = null; },
    act: function (state, action, root) {
      var n = num(root, "ch4_poly_n"), x = sortNum(rng.many(n, function () { return rng.unif(0, 10); }));
      state.d = { x: x, y: x.map(function (v) { return 3 * Math.sin(v) + rng.norm(0, 1); }) };
    },
    draw: function (v, P, k, state, vals) {
      if (!state.d) return emptySpec(P, "Kliknij „Generuj”");
      var d = state.d, fit = polyFit(d.x, d.y, Number(vals.ch4_poly_degree)), r = range(d.x), curve = [], i, ys = d.y.slice();
      if (fit) for (i = 0; i < 200; i++) { var xv = r[0] + (r[1] - r[0]) * i / 199, yv = fit.predict(xv); curve.push([xv, yv]); ys.push(yv); }
      var layers = [{ type: "points", data: pairs(d.x, d.y), color: P.secondary, alpha: 0.5, size: 7 }];
      if (fit) layers.push({ type: "line", data: curve, color: P.niebo, width: 2.4 });
      else layers.push({ type: "text", x: (r[0] + r[1]) / 2, y: "top", text: "Stopień musi być mniejszy niż liczba punktów", color: P.terakota, valign: "top" });
      return { x: pad(r), y: pad(range(ys)), yNegative: true, layers: layers, xName: "X", yName: "Y",
               aria: "Punkty z dopasowanym wielomianem wybranego stopnia." };
    },
    update: function (root, state) {
      var st = root.querySelector('[data-role="stats"]'), P = C.palette();
      if (!st) return;
      var fit = state.d && polyFit(state.d.x, state.d.y, num(root, "ch4_poly_degree"));
      st.innerHTML = fit ? readout("R²", fmt(fit.r2, 3), P.niebo) + readout("R² skor.", fmt(fit.adjR2, 3), P.szalwia) +
        readout("AIC", fmt(fit.aic, 1), P.bursztyn) + readout("BIC", fmt(fit.bic, 1), P.secondary) : "";
    }
  };

  // Ryc. 4.4: zbiór uczący i testowy; błąd testowy dla stopni 1–15.
  function ttState(state, degree) {
    var t = state.d.train, s = state.d.test, all = [];
    for (var g = 1; g <= 15; g++) {
      var f = polyFit(t.x, t.y, g);
      all.push({ degree: g, fit: f, train: rmseOn(f, t.x, t.y), test: rmseOn(f, s.x, s.y) });
    }
    var best = all.reduce(function (b, r) { return r.test < b.test ? r : b; });
    return { cur: all[degree - 1], best: best };
  }
  figures.s07_tt = {
    init: function (state) { state.d = trainTest(); },
    act: function (state) { state.d = trainTest(); },
    draw: function (v, P, k, state, vals) {
      var d = state.d, st = ttState(state, Number(vals.ch4_tt_degree)), curve = [], ys = d.train.y.concat(d.test.y);
      for (var i = 0; i < 300; i++) { var xv = 10 * i / 299, yv = st.cur.fit.predict(xv); curve.push([xv, yv]); ys.push(yv); }
      return { x: [-0.5, 10.5], y: pad(range(ys)), yNegative: true, xName: "X", yName: "Y",
        layers: [{ type: "points", data: pairs(d.test.x, d.test.y), color: P.bursztyn, alpha: 0.25, size: 6 },
                 { type: "points", data: pairs(d.train.x, d.train.y), color: P.secondary, alpha: 0.75, size: 7 },
                 { type: "line", data: curve, color: P.niebo, width: 2.4 }],
        aria: "Zbiór uczący i testowy z wielomianem dopasowanym do zbioru uczącego." };
    },
    update: function (root, state) {
      var st = ttState(state, num(root, "ch4_tt_degree")), P = C.palette();
      setRole(root, "stats", readout("RMSE uczący", fmt(st.cur.train, 2), P.niebo) + readout("RMSE testowy", fmt(st.cur.test, 2), P.bursztyn) +
        readout("Najlepszy na teście", "stopień " + st.best.degree + " · RMSE " + fmt(st.best.test, 2), P.szalwia));
      setRole(root, "note", caption(st.cur.test > st.best.test * 1.25 ? "Błąd testowy wyraźnie wyższy niż przy najlepszym stopniu." : "Błąd testowy bliski najmniejszego."));
    }
  };

  // ---- Rozdział 5: regresja logistyczna --------------------------------------------
  function grafit() { return C.token("--upwr-cat-grafit", "#4a4f55"); }
  function wrzos() { return C.token("--upwr-cat-wrzos", "#7d5a8c"); }
  function pct(v) { return fmt(v * 100, 1) + "%"; }

  // Ryc. 5.1: próg zaliczenia dzieli wynik czytania na 0/1; model logistyczny z trzema predyktorami.
  var CAS_TERMS = [["(Intercept)", "Stała (szanse wyjściowe, nie OR)"], ["income", "Dochód okręgu (tys. USD)"],
                   ["lunch", "Uczniowie z dotacją do obiadu (%)"], ["english", "Angielski jako drugi język (%)"]];
  function casLogit(c, cut) {
    var y = c.read.map(function (v) { return v >= cut ? 1 : 0; });
    return { y: y, fit: logitFit([ones(y.length), c.income, c.lunch, c.english], y) };
  }
  figures.s07_caslogit = {
    prepare: function (state, v, vals) {
      var cut = Number(vals.ch5_cas_y_cut);
      if (state.cut !== cut) { state.cut = cut; state.m = casLogit(state.config, cut); }
    },
    draw: function (v, P, k, state) {
      var c = state.config, m = state.m, cols = [grafit(), P.szalwia];
      var legend = [{ text: "nie zdał", color: cols[0], box: true }, { text: "zdał", color: cols[1], box: true }];
      if (k === 0) {
        return { x: pad(range(c.income)), y: pad(range(c.read)), legend: legend, xName: "Dochód okręgu (tys. USD)", yName: "Wynik z czytania",
          layers: [{ type: "points", data: pairs(c.income, c.read), colors: m.y.map(function (q) { return cols[q]; }), alpha: 0.62, size: 7 },
            { type: "line", data: [[-1e3, state.cut], [1e3, state.cut]], color: P.accent, width: 2, dash: DASH },
            { type: "text", x: Math.min.apply(null, c.income), y: state.cut, text: "próg zaliczenia: " + state.cut + " pkt", color: P.accent, align: "left", valign: "bottom", halo: true }],
          aria: "Wynik z czytania a dochód okręgu z progiem zaliczenia." };
      }
      return { x: pad(range(c.income)), y: pad(range(m.fit.fitted)), xName: "Dochód okręgu (tys. USD)", yName: "Prawdopodobieństwo zdania",
        layers: [{ type: "points", data: pairs(c.income, m.fit.fitted), colors: m.y.map(function (q) { return cols[q]; }), alpha: 0.7, size: 7 }],
        aria: "Prawdopodobieństwo zdania z modelu logistycznego a dochód okręgu." };
    },
    update: function (root, state) {
      var m = state.m, fit = m.fit, P = C.palette(), n = m.y.length, rmse = 0;
      m.y.forEach(function (q, i) { rmse += (q - fit.fitted[i]) * (q - fit.fitted[i]); });
      setRole(root, "stats", readout("AIC", C.fmt(fit.aic, 1), wrzos()) + readout("BIC", C.fmt(fit.bic, 1), P.secondary) +
        readout("RMSE prawdop.", C.fmt(Math.sqrt(rmse / n), 3), P.bursztyn));
      var rows = fit.coef.map(function (cf, i) { return [CAS_TERMS[i][1], Math.exp(cf.estimate), cf.se, pText(cf.p), cf.p < 0.05 ? "tak" : "nie"]; });
      setRole(root, "table", tableSplit([{ label: "Zmienna", row: true }, { label: "Iloraz szans (OR)", digits: 3 }, { label: "Błąd stand.", digits: 3 },
        { label: "p" }, { label: "p < 0.05?" }], rows, [[1, 2], [3, 4]], "Model logistyczny: ilorazy szans"));
      var pass = m.y.reduce(function (s, q) { return s + q; }, 0);
      setRole(root, "note", caption("Y = 1 („zdał”) oznacza wynik czytania od " + state.cut + " pkt: " + pass + " z " + n + " okręgów.", "ok"));
    }
  };
  // p jak lc_pval(): trzy miejsca, poniżej 0.001 „< 0.001”.
  function pText(p) { return p < 0.001 ? "< 0.001" : fmt(p, 3); }

  // Ryc. 5.2: prosta i krzywa logistyczna na stałych danych 0/1 (współczynniki z R w config).
  figures.s07_linlog = {
    draw: function (v, P, k, state, vals) {
      var c = state.config, x0 = Number(vals.ch5_lin_log_x), lin = [], lg = [];
      function pl(x) { return 1 / (1 + Math.exp(-(c.log[0] + c.log[1] * x))); }
      for (var i = 0; i < 300; i++) { var x = 40 * i / 299; lin.push([x, c.lin[0] + c.lin[1] * x]); lg.push([x, pl(x)]); }
      return { x: [-2, 42], y: [-0.25, 1.25], yNegative: true, xName: "Godziny nauki", yName: "P(zdanie)",
        layers: [{ type: "line", data: [[-2, 0], [42, 0]], color: P.rule, width: 1.2, dash: [2, 3] },
          { type: "line", data: [[-2, 1], [42, 1]], color: P.rule, width: 1.2, dash: [2, 3] },
          { type: "points", data: pairs(c.x, c.y), color: P.secondary, alpha: 0.34, size: 5 },
          { type: "line", data: lin, color: P.niebo, width: 2, dash: DASH },
          { type: "line", data: lg, color: wrzos(), width: 2.4 },
          { type: "vline", x: x0, color: P.ref, width: 1.4 },
          { type: "points", data: [[x0, c.lin[0] + c.lin[1] * x0]], color: P.niebo, size: 12, z: 7 },
          { type: "points", data: [[x0, pl(x0)]], color: wrzos(), size: 12, z: 7 }],
        aria: "Dane zdał/nie zdał a godziny nauki z prostą i krzywą logistyczną." };
    },
    update: function (root, state) {
      var c = state.config, x0 = num(root, "ch5_lin_log_x"), P = C.palette();
      setRole(root, "stats", readoutSw("Prosta", C.fmt(c.lin[0] + c.lin[1] * x0, 2), P.niebo) +
        readoutSw("Krzywa logistyczna", C.fmt(1 / (1 + Math.exp(-(c.log[0] + c.log[1] * x0))), 2), wrzos()) +
        readout("Prosta poza [0, 1]", C.fmt(c.outside * 100, 1) + "%", P.terakota));
    }
  };
  // Odczyt z kwadracikiem koloru (lc_readout(swatch = TRUE)): pełni rolę legendy linii.
  function readoutSw(label, value, color) {
    return '<div class="lc-read" style="--lc-read-color:' + color + ';"><span class="lc-read-label"><i class="lc-read-swatch" aria-hidden="true"></i>' +
      S.esc(label) + '</span><span class="lc-read-value">' + S.esc(String(value)) + "</span></div>";
  }

  // Ryc. 5.3: sigmoida dla β₀ i β₁ z suwaków; przyciski ustawiają gotowe pary.
  var SIGMOID_PRESETS = { steep: [-5, 0.5], flat: [-1, 0.05], neg: [5, -0.3] };
  function setSlider(root, id, value) {
    var el = root.querySelector("#" + id);
    if (!el) return;
    el.value = value;
    el.dispatchEvent(new Event("input", { bubbles: true }));
  }
  figures.s07_sigmoid = {
    act: function (state, action, root) {
      var p = SIGMOID_PRESETS[action];
      if (p) { setSlider(root, "ch5_b0", p[0]); setSlider(root, "ch5_b1", p[1]); }
    },
    draw: function (v, P, k, state, vals) {
      var b0 = Number(vals.ch5_b0), b1 = Number(vals.ch5_b1), pts = [];
      for (var i = 0; i < 500; i++) { var x = -5 + 50 * i / 499; pts.push([x, 1 / (1 + Math.exp(-(b0 + b1 * x)))]); }
      return { x: [-7.5, 47.5], y: [0, 1], xName: "X", yName: "P(Y = 1)", yNegative: true,
        layers: [{ type: "line", data: [[-7.5, 0.5], [47.5, 0.5]], color: C.alpha(P.secondary, 0.5), width: 1.4, dash: DASH },
                 { type: "line", data: pts, color: wrzos(), width: 3 }],
        aria: "Krzywa logistyczna dla wybranych współczynników." };
    }
  };

  // Ryc. 5.4–5.6: jeden model dopasowany w Ryc. 5.4; ryciny 5.5 i 5.6 przerysowują się po dopasowaniu.
  var logitStore = { d: null, fit: null, X: null, jitter: null };
  function redrawOthers(names) {
    names.forEach(function (n) { document.querySelectorAll('.lc-sf[data-fig="' + n + '"]').forEach(function (el) { el.dispatchEvent(new CustomEvent("lc:sf-redraw")); }); });
  }
  function logitPredict(fit, hours, gpa) {
    var b = fit.coef.map(function (c) { return c.estimate; });
    return 1 / (1 + Math.exp(-(b[0] + b[1] * hours + b[2] * gpa)));
  }
  figures.s07_logit = {
    act: function (state, action, root) {
      var d = logisticData(num(root, "ch5_n")), X = [ones(d.zdal_num.length), d.godziny_nauki, d.srednia_ocen];
      logitStore.d = d; logitStore.X = X; logitStore.fit = logitFit(X, d.zdal_num); logitStore.ci = null;
      logitStore.jitter = d.zdal_num.map(function () { return rng.unif(-0.03, 0.03); });
      redrawOthers(["s07_or", "s07_threshold"]);
    },
    draw: function (v, P, k, state, vals) {
      var s = logitStore;
      if (!s.fit) return emptySpec(P, "Kliknij „Dopasuj model”");
      var pv = vals.ch5_predictor === "srednia_ocen" ? "srednia_ocen" : "godziny_nauki", other = pv === "srednia_ocen" ? "godziny_nauki" : "srednia_ocen";
      var xs = s.d[pv], om = T.mean(s.d[other]), r = range(xs), curve = [];
      for (var i = 0; i < 200; i++) {
        var x = r[0] + (r[1] - r[0]) * i / 199;
        curve.push([x, pv === "godziny_nauki" ? logitPredict(s.fit, x, om) : logitPredict(s.fit, om, x)]);
      }
      return { x: pad(r), y: [-0.05, 1.05], yNegative: true, xName: pv === "godziny_nauki" ? "Godziny nauki" : "Średnia ocen", yName: "P(zdanie egzaminu)",
        layers: [{ type: "points", data: xs.map(function (x, i) { return [x, s.d.zdal_num[i] + s.jitter[i]]; }), color: P.secondary, alpha: 0.3, size: 6 },
                 { type: "line", data: curve, color: wrzos(), width: 3 },
                 { type: "line", data: [[r[0] - 100, 0.5], [r[1] + 100, 0.5]], color: P.bursztyn, width: 1.6, dash: DASH }],
        aria: "Dane 0/1 i krzywa modelu logistycznego dla wybranego predyktora (drugi na średniej)." };
    },
    update: function (root, state) {
      var s = logitStore, P = C.palette();
      if (!s.fit) { setRole(root, "stats", ""); setRole(root, "pred", ""); return; }
      var ok = 0;
      s.fit.fitted.forEach(function (p, i) { if ((p >= 0.5 ? 1 : 0) === s.d.zdal_num[i]) ok++; });
      setRole(root, "stats", readout("AIC", fmt(s.fit.aic, 1), wrzos()) + readout("BIC", fmt(s.fit.bic, 1), P.secondary) +
        readout("Dokładność", fmt(ok / s.fit.n * 100, 1) + "%", P.szalwia));
      var prob = logitPredict(s.fit, num(root, "ch5_pred_hours"), num(root, "ch5_pred_gpa")), col = prob >= 0.5 ? P.szalwia : P.terakota;
      setRole(root, "pred", readout("P(zdanie)", fmt(prob, 3), col) + readout("Prognoza", prob >= 0.5 ? "Prawdopodobnie zda" : "Raczej nie zda", col));
    }
  };
  // Ryc. 5.5: ilorazy szans z modelu z Ryc. 5.4 (przedziały profilowe jak confint()).
  var OR_LABELS = ["Wyraz wolny", "Godziny nauki (+1h)", "Średnia ocen (+1 pkt)"];
  figures.s07_or = {
    update: function (root) {
      var s = logitStore;
      if (!s.fit) { setRole(root, "table", caption("Najpierw dopasuj model.")); return; }
      if (!s.ci) s.ci = [1, 2].map(function (j) { return logitProfileCI(s.X, s.d.zdal_num, s.fit, j); });
      var rows = [1, 2].map(function (j, i) {
        var c = s.fit.coef[j];
        return [OR_LABELS[j], c.estimate, Math.exp(c.estimate), "[" + fmt(Math.exp(s.ci[i][0]), 3) + " ; " + fmt(Math.exp(s.ci[i][1]), 3) + "]", pText(c.p)];
      });
      setRole(root, "table", S.table([{ label: "Zmienna", row: true }, { label: "β", digits: 3 }, { label: "OR", digits: 3 }, { label: "95% CI (OR)" }, { label: "p" }], rows));
    }
  };
  // Ryc. 5.6: macierz pomyłek dla progu decyzji (kolor komórki: liczba obserwacji).
  function mix(a, b, t) {
    function rgb(h) { h = h.replace("#", ""); return [0, 2, 4].map(function (i) { return parseInt(h.substr(i, 2), 16); }); }
    var x = rgb(a), y = rgb(b);
    return "rgb(" + x.map(function (v, i) { return Math.round(v + (y[i] - v) * t); }).join(",") + ")";
  }
  function confusion(threshold) {
    var s = logitStore, m = { tp: 0, tn: 0, fp: 0, fn: 0 };
    s.fit.fitted.forEach(function (p, i) {
      var pred = p >= threshold ? 1 : 0, y = s.d.zdal_num[i];
      if (pred && y) m.tp++; else if (!pred && !y) m.tn++; else if (pred) m.fp++; else m.fn++;
    });
    return m;
  }
  figures.s07_threshold = {
    draw: function (v, P, k, state, vals) {
      if (!logitStore.fit) return emptySpec(P, "Najpierw dopasuj model w Ryc. 5.4");
      var m = confusion(Number(vals.ch5_threshold));
      // Kolumny: predykcja Nie/Tak; wiersze: rzeczywistość Nie/Tak (Tak u góry).
      var cells = [[1, 1, m.tn], [2, 1, m.fp], [1, 2, m.fn], [2, 2, m.tp]], vals4 = cells.map(function (c) { return c[2]; });
      var lo = Math.min.apply(null, vals4), hi = Math.max.apply(null, vals4), span = hi - lo || 1;
      var layers = [{ type: "cells", items: cells.map(function (c) { return { r: [c[0] - 0.5, c[0] + 0.5, c[1] - 0.5, c[1] + 0.5], color: mix(P.niebo, P.secondary, (c[2] - lo) / span) }; }) },
        { type: "segs", items: [{ from: [1.5, 0.5], to: [1.5, 2.5] }, { from: [0.5, 1.5], to: [2.5, 1.5] }], color: P.surface, width: 2, z: 4 }];
      cells.forEach(function (c) { layers.push({ type: "text", x: c[0], y: c[1], text: String(c[2]), color: "#ffffff", bold: true, size: 1.8, valign: "middle" }); });
      return { x: [0.5, 2.5], y: [0.5, 2.5], xLabels: { 1: "Nie", 2: "Tak" }, yLabels: { 1: "Nie", 2: "Tak" }, noXGrid: true, noYGrid: true,
        xName: "Predykcja modelu", yName: "Rzeczywistość", layers: layers, aria: "Macierz pomyłek modelu dla wybranego progu." };
    },
    update: function (root) {
      var P = C.palette();
      if (!logitStore.fit) { setRole(root, "stats", ""); return; }
      var m = confusion(num(root, "ch5_threshold")), n = m.tp + m.tn + m.fp + m.fn;
      var sens = m.tp + m.fn ? m.tp / (m.tp + m.fn) : NaN, spec = m.tn + m.fp ? m.tn / (m.tn + m.fp) : NaN;
      setRole(root, "stats", readout("Dokładność", pct((m.tp + m.tn) / n), P.szalwia) + readout("Czułość (wykrywa Tak)", isNaN(sens) ? "NA%" : pct(sens), P.niebo) +
        readout("Swoistość (wykrywa Nie)", isNaN(spec) ? "NA%" : pct(spec), P.bursztyn));
    }
  };

  S.register(figures);
})();
