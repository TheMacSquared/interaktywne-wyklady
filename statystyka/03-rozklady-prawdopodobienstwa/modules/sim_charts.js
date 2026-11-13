// Symulacje wykładu 03 liczone w przeglądarce (dawniej losowanie i ggplot na
// serwerze). Silnik: window.lcSF (statystyka/R/lc_stat_figures.js), rozkłady
// i losowanie: window.lcDist (statystyka/R/lc_dist.js).
//
//   d03-emp   — Ryc. 1.3: próba z modelu (histogram lub słupki) i model
//   d03-freq  — Ryc. 1.4: częstości z rzutów a prawdopodobieństwa modelu
//   d03-unif  — Ryc. 3.1: moneta i kostki, częstości wobec 1/k
//   d03-hist  — Ryc. 4.1: od histogramu do krzywej gęstości (krokowa)
//   d03-ctg   — Ryc. 6.4: populacja i rozkład średnich z prób (CTG)
//
// Próba jest losowana od nowa przy zmianie kontrolek, które ją określają
// (rozkład, liczebność), oraz przyciskiem; inne kontrolki tylko przerysowują.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist;
  if (!C || !S || !D) return;
  var fmt = S.fmt, R = D.r;

  function max(a) { var m = -Infinity; for (var i = 0; i < a.length; i++) if (a[i] > m) m = a[i]; return m; }
  function min(a) { var m = Infinity; for (var i = 0; i < a.length; i++) if (a[i] < m) m = a[i]; return m; }
  function mean(a) { var s = 0; for (var i = 0; i < a.length; i++) s += a[i]; return s / a.length; }
  function sd(a) {
    var m = mean(a), s = 0;
    for (var i = 0; i < a.length; i++) s += (a[i] - m) * (a[i] - m);
    return Math.sqrt(s / (a.length - 1));
  }
  function range(a, b) { var out = []; for (var i = a; i <= b; i++) out.push(i); return out; }
  function cat(name, fallback) { return C.token("--upwr-cat-" + name, fallback); }
  // Liczebności wyników 1..k (table() w R).
  function tab(obs, k) { var n = new Array(k).fill(0); obs.forEach(function (o) { n[o - 1]++; }); return n; }
  // Próba zależna od klucza kontrolek: losujemy od nowa, gdy klucz się zmienił.
  function resample(state, key, draw) { if (state.key !== key) { state.key = key; state.data = draw(); } }
  function empty(text) {
    return { x: [0, 1], y: [0, 1], hideY: true, noYGrid: true, noXGrid: true, xTicks: [],
             layers: [{ type: "text", x: 0.5, y: 0.5, text: text, color: C.palette().ref, size: 1.2, valign: "middle" }],
             aria: text };
  }
  // Słupki kategorii 1..k: [[k − w/2, k + w/2, wysokość]].
  function bars(ys, w, from) {
    return ys.map(function (y, i) { var x = (from == null ? 1 : from) + i; return [x - w / 2, x + w / 2, y]; });
  }
  function labels(names) { var out = {}; names.forEach(function (t, i) { out[i + 1] = t; }); return out; }

  var FIGS = {};

  // ---- Ryc. 1.3: dane a model -----------------------------------------------
  var EMP = {
    normal:   { draw: function (n) { return R.many(n, function () { return R.norm(170, 8); }); },
                f: function (x) { return D.dnorm(x, 170, 8); }, lim: [170 - 32, 170 + 32] },
    skewed:   { draw: function (n) { return R.many(n, function () { return R.gamma(3, 10) + 5; }); },
                f: function (x) { return D.dgamma(x - 5, 3, 10); }, lim: [0, 5 + 92.33] },  // qgamma(0.999, 3, scale = 10)
    poisson:  { draw: function (n) { return R.many(n, function () { return R.pois(3); }); },
                k: range(0, 12), p: function (k) { return D.dpois(k, 3); } },
    binomial: { draw: function (n) { return R.many(n, function () { return R.binom(10, 0.6); }); },
                k: range(0, 10), p: function (k) { return D.dbinom(k, 10, 0.6); } }
  };

  FIGS.emp = {
    prepare: function (state, v, vals) {
      var dist = vals.ch1_emp_dist, n = Number(vals.ch1_emp_n);
      resample(state, dist + "|" + n, function () { return EMP[dist].draw(n); });
    },
    act: function (state) { state.key = null; },
    draw: function (v, P, k, state, vals) {
      var dist = vals.ch1_emp_dist, m = EMP[dist], x = state.data, n = x.length;
      var hist = vals.ch1_show_hist === "on", dens = vals.ch1_show_density === "on";
      if (!hist && !dens) return empty("Włącz przynajmniej jedną warstwę");
      var layers = [];
      if (m.k) {
        // Zmienna dyskretna: słupki częstości i lizaki P(X = k).
        var freq = new Array(m.k.length).fill(0);
        x.forEach(function (t) { if (t < m.k.length) freq[t]++; });
        freq = freq.map(function (c) { return c / n; });
        var pmf = m.k.map(m.p), top = max(freq.concat(pmf)) * 1.12;
        if (hist) layers.push({ type: "bins", data: bars(freq, 0.8, 0), color: P.niebo, alpha: 0.6 });
        if (dens) {
          layers.push({ type: "segs", width: 2, items: m.k.map(function (t, i) { return { from: [t, 0], to: [t, pmf[i]], color: P.terakota }; }) },
                      { type: "points", data: m.k.map(function (t, i) { return [t, pmf[i]]; }), color: P.terakota, size: 10 });
        }
        return { x: [m.k[0] - 0.6, m.k[m.k.length - 1] + 0.6], y: [0, top], xTicks: m.k, noXGrid: true,
                 xName: "Wartość (liczba całkowita)", yName: "Częstość względna", layers: layers,
                 aria: "Częstości z próby " + n + " obserwacji" + (dens ? " i prawdopodobieństwa modelu" : "") };
      }
      // Zmienna ciągła: histogram w skali gęstości (jak hist() w R) i krzywa modelu.
      var lo = Math.min(m.lim[0], min(x)), hi = Math.max(m.lim[1], max(x)), pad = (hi - lo) * 0.05;
      var xr = [lo - pad, hi + pad], nb = Math.min(50, Math.max(10, n / 10));
      var a = min(x), b = max(x), w = (b - a) / nb, cnt = new Array(Math.ceil(nb)).fill(0);
      var edges = []; for (var i = 0; i <= nb; i++) edges.push(a + i * w);
      x.forEach(function (t) { cnt[Math.max(0, Math.min(cnt.length - 1, Math.ceil((t - a) / w) - 1))]++; });
      var hd = cnt.map(function (c, i) { return [edges[i], edges[i] + w, c / (n * w)]; });
      var curve = D.seq(xr[0], xr[1], 500).map(function (t) { var y = m.f(t); return [t, isFinite(y) ? y : 0]; });
      var top2 = Math.max(max(curve.map(function (p) { return p[1]; })), max(hd.map(function (r) { return r[2]; }))) * 1.08;
      if (hist) layers.push({ type: "bins", data: hd, color: P.niebo, alpha: 0.6, stroke: P.surface });
      if (dens) layers.push({ type: "line", data: curve, color: P.terakota, width: 3, fill: P.terakota, fillAlpha: 0.1 });
      return { x: xr, y: [0, top2], xName: "Wartość", yName: "Gęstość", layers: layers,
               aria: (hist ? "Histogram próby " + n + " obserwacji" : "") + (hist && dens ? " i " : "") + (dens ? "krzywa gęstości modelu" : "") };
    }
  };

  // ---- Ryc. 1.4: częstości a prawdopodobieństwa -----------------------------
  var FREQ = {
    fair:   { p: [1, 1, 1, 1, 1, 1].map(function () { return 1 / 6; }), labels: ["1", "2", "3", "4", "5", "6"] },
    loaded: { p: [0.1, 0.1, 0.1, 0.1, 0.1, 0.5], labels: ["1", "2", "3", "4", "5", "6"] },
    coin:   { p: [0.5, 0.5], labels: ["Orzeł", "Reszka"] }
  };
  function freqOf(state, sc) { var c = tab(state.data, sc.p.length), n = state.data.length; return c.map(function (t) { return t / n; }); }

  FIGS.freq = {
    prepare: function (state, v, vals) {
      var sc = state.sc = FREQ[vals.ch1_scenario], n = Number(vals.ch1_n_obs);
      resample(state, vals.ch1_scenario + "|" + n, function () { return R.many(n, function () { return R.choose(sc.p); }); });
    },
    act: function (state) { state.key = null; },
    draw: function (v, P, k, state, vals) {
      var sc = FREQ[vals.ch1_scenario], f = freqOf(state, sc), K = sc.p.length;
      var top = max(f.concat(sc.p)) * 1.1, theo = sc.p.map(function (p, i) { return [i + 1, p]; });
      return { x: [0.4, K + 0.6], y: [0, top], xLabels: labels(sc.labels), noXGrid: true,
               xName: "Wynik", yName: "Proporcja / Prawdopodobieństwo",
               layers: [{ type: "bins", data: bars(f, 0.9), color: P.niebo, alpha: 0.85, stroke: P.surface },
                        { type: "line", data: theo, color: P.terakota, width: 2.4 },
                        { type: "points", data: theo, color: P.terakota, size: 12 }],
               legend: [{ text: "Obserwowane", color: P.niebo, box: true }, { text: "Teoretyczne", color: P.terakota }],
               aria: "Częstości z " + state.data.length + " obserwacji na tle prawdopodobieństw modelu" };
    },
    update: function (root, state) {
      var sc = state.sc, f = freqOf(state, sc);
      var d = max(f.map(function (t, i) { return Math.abs(t - sc.p[i]); }));
      S.setValue(root, "diff", "Maksymalna różnica między częstością a prawdopodobieństwem: " +
        C.fmt(d, 3) + (d < 0.05 ? " — dobra zgodność" : " — słabsza zgodność"));
    }
  };

  // ---- Ryc. 3.1: jednostajny dyskretny ---------------------------------------
  var UNIF_K = { coin: 2, die: 6, d12: 12 };
  FIGS.unif = {
    prepare: function (state, v, vals) {
      var K = UNIF_K[vals.ch3_unif_type], n = Number(vals.ch3_unif_n);
      resample(state, K + "|" + n, function () { return R.many(n, function () { return R.int(K); }); });
    },
    act: function (state) { state.key = null; },
    draw: function (v, P, k, state, vals) {
      var K = UNIF_K[vals.ch3_unif_type], n = state.data.length;
      var f = tab(state.data, K).map(function (c) { return c / n; }), top = Math.max(max(f), 1 / K) * 1.1;
      return { x: [0.4, K + 0.6], y: [0, top], xTicks: range(1, K), noXGrid: true, xName: "Wynik", yName: "Częstość względna",
               layers: [{ type: "bins", data: bars(f, 0.9), color: P.niebo, alpha: 0.7, stroke: P.surface },
                        { type: "line", data: [[0.4, 1 / K], [K + 0.6, 1 / K]], color: P.terakota, width: 2, dash: [6, 4] },
                        { type: "points", data: range(1, K).map(function (i) { return [i, 1 / K]; }), color: P.terakota, size: 10 }],
               aria: "Częstości " + K + " wyników w " + n + " próbach na tle 1/" + K };
    }
  };

  // ---- Ryc. 4.1: od histogramu do krzywej gęstości ----------------------------
  var HIST = {
    normal: { draw: function (n) { return R.many(n, function () { return R.norm(5, 1.5); }); }, bounds: null },
    exp:    { draw: function (n) { return R.many(n, function () { return R.exp(0.5); }); }, bounds: [0, Infinity] },
    unif:   { draw: function (n) { return R.many(n, function () { return R.unif(0, 10); }); }, bounds: [0, 10] }
  };
  var HIST_TEXT = [
    "Każda kreska to jedna obserwacja. Przy setkach kresek trudno ocenić, gdzie jest ich najwięcej.",
    "5 binów — widać ogólny zarys, ale mało szczegółów.",
    "15 binów — kształt staje się wyraźniejszy.",
    "30 binów — więcej szczegółów, ale słupki są nierówne.",
    "Oś Y w skali gęstości — łączne pole słupków wynosi 1.",
    "Gładka krzywa przybliża kształt histogramu.",
    "Zostaje sama krzywa gęstości f(x). Pole pod nią wynosi 1."
  ];

  FIGS.hist = {
    // Stała rama z pełnej próby: oś X wspólna dla kroków, oś Y wspólna dla kroków
    // w jednej skali. Zapas 14% mieści skrajne słupki 5 binów.
    prepare: function (state, v, vals) {
      var dist = vals.ch4_step_dist, n = Number(vals.ch4_step_n);
      if (state.key === dist + "|" + n) return;
      resample(state, dist + "|" + n, function () { return HIST[dist].draw(n); });
      var x = state.data, lo = min(x), hi = max(x), pad = (hi - lo) * 0.14, fr = { xlim: [lo - pad, hi + pad], bins: {} };
      [5, 15, 30].forEach(function (b) { fr.bins[b] = S.binsGgplot(x, b); });
      // Krzywa jak geom_density: na zakresie danych, z odbiciem przy granicach dziedziny.
      fr.kde = D.kde(x, lo, hi, 512, null, HIST[dist].bounds);
      var d30 = max(fr.bins[30].map(function (b) { return b.n / (n * (b.r - b.l)); }));
      fr.densMax = Math.max(d30, max(fr.kde.map(function (p) { return p[1]; })));
      state.frame = fr;
    },
    draw: function (v, P, k, state, vals, step) {
      var x = state.data, n = x.length, fr = state.frame;
      var fill = P.niebo, edge = P.ink;
      function hist(b, density, alpha) {
        return { type: "bins", color: fill, alpha: alpha == null ? 0.7 : alpha, stroke: edge, lineWidth: 0.6,
                 data: fr.bins[b].map(function (r) { return [r.l, r.r, density ? r.n / (n * (r.r - r.l)) : r.n]; }) };
      }
      function count(b) { return [0, max(fr.bins[b].map(function (r) { return r.n; })) * 1.08]; }
      var spec = { x: fr.xlim, xName: "Wartość", layers: [], aria: HIST_TEXT[step - 1] };
      var dens = [0, fr.densMax * 1.08];
      if (step === 1) {
        spec.y = [0, 1]; spec.hideY = true; spec.noYGrid = true; spec.yName = "";
        spec.layers.push({ type: "rug", values: x, color: fill, alpha: 0.3 });
      } else if (step <= 4) {
        var b = [5, 15, 30][step - 2];
        spec.y = count(b); spec.yName = "Liczebność";
        spec.layers.push(hist(b, false));
        if (step === 2) spec.layers.push({ type: "rug", values: x, color: fill, alpha: 0.22 });
      } else if (step === 5) {
        spec.y = dens; spec.yName = "Gęstość"; spec.layers.push(hist(30, true));
      } else if (step === 6) {
        spec.y = dens; spec.yName = "Gęstość";
        spec.layers.push(hist(30, true, 0.5), { type: "line", data: fr.kde, color: P.accent, width: 3 });
      } else {
        spec.y = dens; spec.yName = "Gęstość f(x)";
        spec.layers.push({ type: "line", data: fr.kde, color: P.secondary, width: 2, fill: fill, fillAlpha: 0.3 });
      }
      return spec;
    },
    text: function (state, v, vals, step) { return S.esc(HIST_TEXT[step - 1]); }
  };

  // ---- Ryc. 6.4: CTG ------------------------------------------------------------
  // Populacje jak generate_population_sample() w R/shared.R.
  var POP = {
    uniform:     { min: 0, mu: 5, sigma: Math.sqrt(100 / 12), draw: function () { return R.unif(0, 10); },
                   curve: function () { return [[-0.5, 0], [0, 0], [0, 0.1], [10, 0.1], [10, 0], [10.5, 0]]; } },
    exponential: { min: 0, mu: 2, sigma: 2, draw: function () { return R.exp(0.5); },
                   curve: function () { return D.seq(0, 14, 300).map(function (x) { return [x, D.dexp(x, 0.5)]; }); } },
    bimodal:     { mu: 5, sigma: Math.sqrt(0.64 + 4), draw: function () { return R.u() < 0.5 ? R.norm(7, 0.8) : R.norm(3, 0.8); },
                   curve: function () { return D.seq(0, 10, 300).map(function (x) { return [x, 0.5 * D.dnorm(x, 3, 0.8) + 0.5 * D.dnorm(x, 7, 0.8)]; }); } },
    // Beta(0.5, 0.5) · 10: gęstość rośnie do nieskończoności przy brzegach, obcinamy oś.
    u_shape:     { min: 0, mu: 5, sigma: Math.sqrt(100 / 8), draw: function () { return R.beta(0.5, 0.5) * 10; }, ymax: 0.3,
                   curve: function () { return D.seq(0.005, 9.995, 400).map(function (x) { return [x, Math.min(0.4, D.dbeta(x / 10, 0.5, 0.5) / 10)]; }); } },
    die:         { min: 1, mu: 3.5, sigma: Math.sqrt(35 / 12), draw: function () { return R.int(6); } }
  };

  FIGS.ctg = {
    prepare: function (state, v, vals) {
      // Zmiana rozkładu lub n czyści zebrane średnie.
      var key = vals.ch6_pop_dist + "|" + vals.ch6_sample_size;
      if (state.key !== key) { state.key = key; state.means = []; }
      state.dist = vals.ch6_pop_dist; state.n = Number(vals.ch6_sample_size);
    },
    act: function (state, name) {
      if (name === "reset") { state.means = []; return; }
      var k = Number(name.replace("take", "")), pop = POP[state.dist], n = state.n;
      for (var i = 0; i < k; i++) { var s = 0; for (var j = 0; j < n; j++) s += pop.draw(); state.means.push(s / n); }
    },
    draw: function (v, P, k, state) {
      var pop = POP[state.dist], amber = P.bursztyn;
      if (k === 0) {
        if (state.dist === "die") {
          return { x: [0.4, 6.6], y: [0, 0.3], xTicks: range(1, 6), noXGrid: true, yName: "P(X = k)",
                   layers: [{ type: "bins", data: bars(range(1, 6).map(function () { return 1 / 6; }), 0.6), color: amber, alpha: 0.85 }],
                   aria: "Populacja: rzut kostką, każdy wynik z prawdopodobieństwem 1/6" };
        }
        var c = pop.curve(), top = pop.ymax || max(c.map(function (p) { return p[1]; })) * 1.08;
        return { x: [c[0][0], c[c.length - 1][0]], y: [0, top], yName: "Gęstość",
                 layers: [{ type: "line", data: c, color: P.secondary, width: 1.6, fill: amber, fillAlpha: 0.5 }],
                 aria: "Gęstość populacji, μ = " + fmt(pop.mu, 2) + ", σ = " + fmt(pop.sigma, 2) };
      }
      var m = state.means, mu = pop.mu, se = pop.sigma / Math.sqrt(state.n);
      if (!m.length) return empty("Pobierz próby przyciskiem powyżej");
      // Oś obejmuje μ ± 4·σ/√n, ale nie wychodzi poza dziedzinę populacji (średnia ≥ 0).
      var lo = Math.min(mu - 4 * se, min(m)), hi = Math.max(mu + 4 * se, max(m));
      if (pop.min != null) lo = Math.max(lo, pop.min);
      var pad = (hi - lo) * 0.03;
      var layers = [], peak = D.dnorm(mu, mu, se), top;
      if (m.length >= 5) {
        var bins = S.binsGgplot(m, Math.round(Math.min(50, Math.max(10, m.length / 5))));
        var hd = bins.map(function (b) { return [b.l, b.r, b.n / (m.length * (b.r - b.l))]; });
        layers.push({ type: "bins", data: hd, color: P.niebo, alpha: 0.7, stroke: P.surface });
        top = max(hd.map(function (r) { return r[2]; }));
      } else {
        // Kilka średnich: kropki ułożone w stosy (jak geom_dotplot), bez skali gęstości.
        var bw = se / 3, stacks = {}, pts = [];
        m.forEach(function (t) {
          var b = Math.round(t / bw), h = stacks[b] = (stacks[b] || 0) + 1;
          pts.push([b * bw, (h - 0.5) * peak * 0.12]);
        });
        layers.push({ type: "points", data: pts, color: P.niebo, alpha: 0.85, size: 14 });
        top = peak * 0.8;
      }
      if (m.length >= 30) {
        layers.push({ type: "line", color: P.terakota, width: 3,
                      data: D.seq(pop.min == null ? min(m) - se : Math.max(pop.min, min(m) - se), max(m) + se, 200).map(function (x) { return [x, D.dnorm(x, mu, se)]; }) });
        top = Math.max(top, peak);
      }
      layers.push({ type: "vline", x: mu, color: P.terakota, width: 1.5, dash: [6, 4] });
      return { x: [lo - pad, hi + pad], y: [0, top * 1.08], xName: "Średnia z próby", yName: "Gęstość", layers: layers,
               aria: m.length + " średnich z prób o liczebności " + state.n };
    },
    update: function (root, state) {
      var m = state.means, se = POP[state.dist].sigma / Math.sqrt(state.n), ok = m.length >= 2;
      S.setValue(root, "count", String(m.length));
      S.setValue(root, "mean", ok ? fmt(Math.round(mean(m) * 1000) / 1000, 3) : "–");
      S.setValue(root, "sd", ok ? fmt(Math.round(sd(m) * 1000) / 1000, 3) : "–");
      S.setValue(root, "se", fmt(Math.round(se * 1000) / 1000, 3));
    }
  };

  S.register({ "d03-emp": FIGS.emp, "d03-freq": FIGS.freq, "d03-unif": FIGS.unif,
               "d03-hist": FIGS.hist, "d03-ctg": FIGS.ctg });
})();
