// Ryciny wykładu 06 „Założenia testów” liczone w przeglądarce (dawniej wykresy ggplot
// i wyniki testów z serwera). Silnik: window.lcSF (shared/lc_stat_figures.js), rozkłady
// i losowanie: window.lcDist, testy: window.lcStats (shared/lc_stats.js).
//
//   s06_normality  — Ryc. 1.1: histogram i wykres Q-Q wylosowanej próby
//   s06_sw         — Ryc. 1.2: test Shapiro–Wilka dla próby z Ryc. 1.1 (bez wykresu)
//   s06_power      — Ryc. 1.3: odsetek odrzuceń H₀ a liczebność (dane z R)
//   s06_transform  — Ryc. 1.4: wykres Q-Q danych surowych i po logarytmowaniu
//   s06_box        — Ryc. 2.1: dwie grupy o różnej wariancji
//   s06_levene     — Ryc. 2.2: testy Levene'a i Bartletta dla danych z Ryc. 2.1 (bez wykresu)
//   s06_ttests     — Ryc. 2.3: test t Studenta i Welcha dla danych z Ryc. 2.1 (bez wykresu)
//   s06_sim        — Ryc. 3.1: 500 prób z prawdziwą H₀, χ² a Fisher
//
// Dane wylosowane na Ryc. 1.1 i 2.1 są wspólne dla paneli, które je testują (store);
// po nowym losowaniu panele zależne dostają zdarzenie lc:sf-redraw.
// Funkcje liczące są w window.lcS06 (testy: tests/js/stat06.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist, T = root.lcStats;
  if (!D || !T) return;

  // generate_test_data(): pięć rozkładów o podobnym środku i rozrzucie.
  function sample(r, n, dist) {
    if (dist === "skewed") return r.many(n, function () { return r.gamma(2, 5) + 150; });
    // t(3) = Z / sqrt(χ²₃ / 3); χ²₃ to gamma o kształcie 1.5 i skali 2.
    if (dist === "heavy_tail") return r.many(n, function () { return 170 + 10 * r.norm() / Math.sqrt(r.gamma(1.5, 2) / 3); });
    if (dist === "bimodal") {
      return r.many(Math.floor(n / 2), function () { return r.norm(160, 4); })
        .concat(r.many(Math.floor(n / 2), function () { return r.norm(180, 4); }));
    }
    if (dist === "uniform") return r.many(n, function () { return r.unif(150, 190); });
    return r.many(n, function () { return r.norm(170, 10); });
  }
  // nclass.Sturges(); dla rozkładu dwumodalnego dwa razy więcej przedziałów.
  function histBins(n, dist) { return Math.ceil(Math.log2(n) + 1) * (dist === "bimodal" ? 2 : 1); }
  // Wykres pudełkowy jak geom_boxplot(): kwartyle typu 7, wąsy do 1.5 IQR, reszta to wartości odstające.
  function boxStats(x) {
    var q1 = T.quantile(x, 0.25), q3 = T.quantile(x, 0.75), iqr = q3 - q1;
    var inside = x.filter(function (v) { return v >= q1 - 1.5 * iqr && v <= q3 + 1.5 * iqr; });
    return { q1: q1, med: T.median(x), q3: q3, lo: Math.min.apply(null, inside), hi: Math.max.apply(null, inside),
             out: x.filter(function (v) { return v < q1 - 1.5 * iqr || v > q3 + 1.5 * iqr; }) };
  }
  // Jedna próba symulacji: tabela 2×2 z dwóch niezależnych zmiennych o tej samej częstości kategorii.
  function simTable(r, n, p) {
    var tab = [[0, 0], [0, 0]];
    for (var i = 0; i < n; i++) tab[r.u() < p ? 0 : 1][r.u() < p ? 0 : 1] += 1;
    return tab;
  }
  function simulate(r, n, p, sims) {
    var chi = [], fisher = [];
    for (var i = 0; i < sims; i++) {
      var tab = simTable(r, n, p), c = T.chisq2x2(tab).p;
      if (!isNaN(c)) chi.push(c);            // pusty wiersz lub kolumna: χ² nie da się policzyć
      fisher.push(T.fisher2x2(tab));
    }
    return { chi: chi, fisher: fisher };
  }
  // Odsetek p < 0.05 w procentach.
  function alarmRate(p) { return 100 * p.filter(function (v) { return v < 0.05; }).length / p.length; }
  // Liczebności w przedziałach (a, b] co 0.05, pierwszy także z lewym końcem.
  function pBins(p) {
    var counts = [], i;
    for (i = 0; i < 20; i++) counts.push(0);
    p.forEach(function (v) { counts[Math.max(0, Math.min(19, Math.ceil(v / 0.05 - 1e-9) - 1))] += 1; });
    return counts.map(function (c, j) { return [j * 0.05, (j + 1) * 0.05, c]; });
  }
  root.lcS06 = { sample: sample, histBins: histBins, boxStats: boxStats, simTable: simTable, simulate: simulate,
                 alarmRate: alarmRate, pBins: pBins };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S) return;

  var rng = D.rng(), store = { ch1: null, ch2: null }, figures = {};
  function notify(names) {
    names.forEach(function (name) {
      document.querySelectorAll('.lc-sf[data-fig="' + name + '"]').forEach(function (el) { el.dispatchEvent(new CustomEvent("lc:sf-redraw")); });
    });
  }
  function wrzos() { return C.token("--upwr-cat-wrzos", "#a07894"); }
  function pad(lo, hi, share) { var d = (hi - lo) * (share || 0.05) || 1; return [lo - d, hi + d]; }
  // Wykres Q-Q: punkty i prosta przez kwartyle na zakresie kwantyli teoretycznych.
  function qqSpec(x, color, P, aria) {
    var qq = T.qqNormal(x), pts = qq.points, x0 = pts[0][0], x1 = pts[pts.length - 1][0];
    var ys = pts.map(function (p) { return p[1]; }).concat([qq.intercept + qq.slope * x0, qq.intercept + qq.slope * x1]);
    return { x: pad(x0, x1), y: pad(Math.min.apply(null, ys), Math.max.apply(null, ys)),
      layers: [{ type: "points", data: pts, color: color.points, size: 7, alpha: color.alpha },
               { type: "line", data: [[x0, qq.intercept + qq.slope * x0], [x1, qq.intercept + qq.slope * x1]], color: color.line, width: 2.2 }],
      xName: "Kwantyle teoretyczne", yName: "Kwantyle próbkowe", aria: aria };
  }

  figures.s06_normality = {
    act: function (state, action, el) {
      var dist = el.querySelector("#ch1_dist").value;
      store.ch1 = { x: sample(rng, Number(el.querySelector("#ch1_n").value), dist), dist: dist };
      notify(["s06_sw"]);
    },
    draw: function (v, P, k) {
      var d = store.ch1;
      if (!d) return null;
      if (k === 1) return qqSpec(d.x, { points: P.niebo, line: P.szalwia, alpha: 0.6 }, P, "Wykres Q-Q wylosowanej próby względem rozkładu normalnego.");
      // Liczba przedziałów rośnie wolno z n (reguła Sturgesa): szerokie słupki także przy dużej próbie.
      var bins = S.binsGgplot(d.x, histBins(d.x.length, d.dist)), n = d.x.length, m = T.mean(d.x), s = T.sd(d.x);
      var lo = bins[0].l, hi = bins[bins.length - 1].r, top = 0, curve = [];
      var data = bins.map(function (b) { var h = b.n / n / (b.r - b.l); top = Math.max(top, h); return [b.l, b.r, h]; });
      for (var i = 0; i <= 200; i++) { var x = lo + (hi - lo) * i / 200; curve.push([x, D.dnorm(x, m, s)]); }
      top = Math.max(top, D.dnorm(m, m, s));
      return { x: [lo, hi], y: [0, top * 1.05],
        layers: [{ type: "bins", data: data, color: P.niebo, alpha: 0.6, stroke: P.surface },
                 { type: "line", data: curve, color: P.szalwia, width: 2.6, dash: [6, 5] }],
        xName: "Wartość", yName: "Gęstość", aria: "Histogram wylosowanej próby z krzywą rozkładu normalnego o tej samej średniej i odchyleniu." };
    },
    noteKey: function () { return store.ch1 ? "data" : "empty"; }
  };

  figures.s06_sw = {
    act: function (state) { state.tested = true; },
    update: function (el, state) {
      state.result = state.tested && store.ch1 ? T.shapiroWilk(store.ch1.x) : null;
      if (!state.result) return;
      S.setValue(el, "sw_w", state.result.W.toFixed(3)); S.setValue(el, "sw_p", T.fmtPValue(state.result.p));
    },
    noteKey: function (vals, state) {
      if (!state.tested) return "none";
      if (!store.ch1) return "nodata";
      return T.shapiroWilk(store.ch1.x).p >= 0.05 ? "ok" : "bad";
    }
  };

  // Oś liczebności logarytmiczna: pozycja = 100 · log10(n), podpisy z n.
  figures.s06_power = {
    draw: function (v, P, k, state) {
      var cfg = state.config, pos = cfg.n.map(function (n) { return Math.round(100 * Math.log10(n)); }), labels = {}, layers = [];
      var cols = { "Normalny": P.ref, "Prawoskośny": P.accent, "Ciężkie ogony": C.token("--upwr-cat-indygo", "#3a5a8a"), "Jednostajny": P.bursztyn };
      cfg.n.forEach(function (n, i) { labels[pos[i]] = String(n); });
      layers.push({ type: "line", data: [[pos[0] - 5, 5], [pos[pos.length - 1] + 45, 5]], color: P.ref, width: 1.4, dash: [6, 5] });
      cfg.series.forEach(function (s) {
        var pts = s.rej.map(function (r, i) { return [pos[i], r]; });
        layers.push({ type: "line", data: pts, color: cols[s.name], width: 2.4 });
        layers.push({ type: "points", data: pts, color: cols[s.name], size: 8 });
        layers.push({ type: "text", x: pos[pos.length - 1] + 3, y: s.label_y, text: s.name, color: cols[s.name], align: "left", valign: "middle" });
      });
      return { x: [pos[0] - 5, pos[pos.length - 1] + 45], y: [0, 110], layers: layers, xLabels: labels,
        yLabels: { 0: "0%", 25: "25%", 50: "50%", 75: "75%", 100: "100%" },
        xName: "Liczebność próby (n)", yName: "Odrzucenia H₀",
        aria: "Odsetek odrzuceń normalności w teście Shapiro-Wilka w zależności od liczebności próby dla czterech rozkładów" };
    }
  };

  figures.s06_transform = {
    act: function (state, action, el) {
      state.x = rng.many(Number(el.querySelector("#ch1_trans_n").value), function () { return rng.gamma(2, 5) + 1; });
    },
    draw: function (v, P, k, state) {
      if (!state.x) return null;
      return k === 0
        ? qqSpec(state.x, { points: P.accent, line: P.accent, alpha: 0.5 }, P, "Wykres Q-Q danych surowych.")
        : qqSpec(state.x.map(Math.log), { points: P.szalwia, line: P.szalwia, alpha: 0.5 }, P, "Wykres Q-Q danych po logarytmowaniu.");
    },
    noteKey: function (vals, state) { return state.x ? "data" : "empty"; }
  };

  figures.s06_box = {
    act: function (state, action, el) {
      var num = function (id) { return Number(el.querySelector("#" + id).value); };
      var a = rng.many(num("ch2_n1"), function () { return rng.norm(170, num("ch2_sd1")); });
      var b = rng.many(num("ch2_n2"), function () { return rng.norm(175, num("ch2_sd2")); });
      // Rozsunięcie punktów w poziomie losowane raz, żeby nie skakały przy przerysowaniu.
      store.ch2 = { a: a, b: b, jitter: rng.many(a.length + b.length, function () { return rng.unif(-0.15, 0.15); }) };
      notify(["s06_levene", "s06_ttests"]);
    },
    draw: function (v, P) {
      var d = store.ch2;
      if (!d) return null;
      var groups = [d.a, d.b], fills = [P.niebo, wrzos()], boxes = [], out = [], pts = [], all = d.a.concat(d.b);
      groups.forEach(function (gr, i) {
        var b = boxStats(gr);
        boxes.push({ pos: i + 1, width: 0.75, q1: b.q1, med: b.med, q3: b.q3, lo: b.lo, hi: b.hi, fill: C.alpha(fills[i], 0.6) });
        b.out.forEach(function (y) { out.push([i + 1, y]); });
        gr.forEach(function (y, j) { pts.push([i + 1 + d.jitter[i * d.a.length + j], y]); });
      });
      return { x: [0.4, 2.6], y: pad(Math.min.apply(null, all), Math.max.apply(null, all)), xLabels: { 1: "A", 2: "B" },
        layers: [{ type: "box", boxes: boxes, stroke: P.accent },
                 { type: "points", data: out, color: P.accent, size: 6 },
                 { type: "points", data: pts, color: P.accent, size: 6, alpha: 0.3 }],
        xName: "Grupa", yName: "Wartość", aria: "Wykresy pudełkowe dwóch grup z naniesionymi obserwacjami." };
    },
    update: function (el) {
      var d = store.ch2, reads = el.querySelector(".lc-reads");
      if (reads) reads.hidden = !d;
      if (!d) return;
      var va = T.variance(d.a), vb = T.variance(d.b);
      S.setValue(el, "box_sd_a", T.round(Math.sqrt(va), 2)); S.setValue(el, "box_sd_b", T.round(Math.sqrt(vb), 2));
      S.setValue(el, "box_ratio", T.round(Math.max(va, vb) / Math.min(va, vb), 2));
    },
    noteKey: function () { return store.ch2 ? "data" : "empty"; }
  };

  function decision(p) { return p >= 0.05 ? "Brak podstaw do odrzucenia H₀" : "Odrzucamy H₀: wariancje różne"; }
  // Komórka decyzji: zielona, gdy brak podstaw do odrzucenia, wyróżniona, gdy odrzucamy.
  function mark(el, key, p) {
    var cell = el.querySelector('[data-v="' + key + '"]');
    cell = cell && cell.closest("td");
    if (!cell) return;
    cell.classList.toggle("is-base", p < 0.05); cell.classList.toggle("is-best", !(p < 0.05));
  }
  figures.s06_levene = {
    act: function (state) { state.tested = true; },
    update: function (el, state) {
      var d = store.ch2;
      if (!state.tested || !d) return;
      var lev = T.levene([d.a, d.b]), bart = T.bartlett([d.a, d.b]);
      S.setValue(el, "lev_stat", "F = " + T.round(lev.F, 3)); S.setValue(el, "lev_p", T.fmtPValue(lev.p));
      S.setValue(el, "lev_dec", decision(lev.p)); mark(el, "lev_dec", lev.p);
      S.setValue(el, "bart_stat", "χ² = " + T.round(bart.K2, 3)); S.setValue(el, "bart_p", T.fmtPValue(bart.p));
      S.setValue(el, "bart_dec", decision(bart.p)); mark(el, "bart_dec", bart.p);
    },
    noteKey: function (vals, state) { return !state.tested ? "none" : store.ch2 ? "data" : "nodata"; }
  };

  figures.s06_ttests = {
    act: function (state) { state.tested = true; },
    update: function (el, state) {
      var d = store.ch2;
      if (!state.tested || !d) return;
      var st = T.tTest(d.a, d.b, true), we = T.tTest(d.a, d.b, false);
      S.setValue(el, "t_classic", "t(" + T.round(st.df, 1) + ") = " + T.round(st.t, 3)); S.setValue(el, "t_classic_p", T.fmtPValue(st.p));
      S.setValue(el, "t_welch", "t(" + T.round(we.df, 1) + ") = " + T.round(we.t, 3)); S.setValue(el, "t_welch_p", T.fmtPValue(we.p));
    },
    noteKey: function (vals, state) { return !state.tested ? "none" : store.ch2 ? "data" : "nodata"; }
  };

  figures.s06_sim = {
    act: function (state, action, el) {
      var rare = (el.querySelector('input[name="ch3_cat"]:checked') || {}).value === "rare";
      // „Rzadkie”: w obu zmiennych jedna kategoria ma 10% częstości.
      state.res = simulate(rng, Number(el.querySelector("#ch3_n").value), rare ? 0.1 : 0.5, 500);
    },
    draw: function (v, P, k, state) {
      if (!state.res) return null;
      var fisher = pBins(state.res.fisher), chi = pBins(state.res.chi), top = 0;
      fisher.concat(chi).forEach(function (b) { top = Math.max(top, b[2]); });
      return { x: [0, 1], y: [0, top * 1.05],
        layers: [{ type: "bins", data: fisher, color: P.niebo, alpha: 0.6, stroke: P.surface },
                 { type: "bins", data: chi, color: wrzos(), alpha: 0.6, stroke: P.surface },
                 { type: "vline", x: 0.05, color: P.accent, width: 1.6, dash: [6, 5] }],
        legend: [{ color: P.niebo, text: "Fisher", box: true }, { color: wrzos(), text: "χ²", box: true }],
        xName: "p-wartość", yName: "Liczba", aria: "Histogramy p-wartości testu χ² i testu Fishera w 500 próbach z prawdziwą H₀." };
    },
    update: function (el, state) {
      var reads = el.querySelector(".lc-reads");
      if (reads) reads.hidden = !state.res;
      if (!state.res) return;
      var P = C.palette();
      // Blisko α: zielony; za dużo fałszywych alarmów: czerwony; za mało (test konserwatywny): bursztynowy.
      function show(key, rate) {
        var node = el.querySelector('[data-v="' + key + '"]'), read = node && node.closest(".lc-read");
        S.setValue(el, key, T.round(rate, 1) + "%");
        if (read) read.style.setProperty("--lc-read-color", Math.abs(rate - 5) <= 2 ? P.szalwia : rate > 5 ? P.accent : P.bursztyn);
      }
      show("sim_chi", alarmRate(state.res.chi)); show("sim_fisher", alarmRate(state.res.fisher));
    },
    noteKey: function (vals, state) { return state.res ? "data" : "empty"; }
  };

  S.register(figures);
})();
