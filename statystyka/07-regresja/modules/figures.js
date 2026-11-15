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

  S.register(figures);
})();
