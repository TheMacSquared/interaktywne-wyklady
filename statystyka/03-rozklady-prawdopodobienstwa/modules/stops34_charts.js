// Punkty stop rozdziałów 3–4 i Ryc. 5.13 (stops_ch3.R, stops_ch4.R): Ryc. 3.6, 3.9, 4.10, 5.13.
// Silnik: window.lcSF, rozkłady i losowanie: window.lcDist, pomocniki: window.lc03Sim (sim_charts.js).
//
//   s3p-v1     — Ryc. 3.6: czy V-1 celowały? (krokowa: mapa, siatka, zliczenia, model)
//   s3p-horses — Ryc. 3.9: konie Bortkiewicza (krokowa: dane, średnia i wariancja, Pois)
//   s3p-bus    — Ryc. 4.10: paradoks autobusu (oś czasu i histogram czekania)
//   s3p-raises — Ryc. 5.13: losowe podwyżki (rozkład pensji po latach)
//
// Dane i teksty kroków przychodzą z R w config (data-config).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist, M = window.lc03Sim;
  if (!C || !S || !D || !M) return;
  var fmt = S.fmt;

  // Pierścień jak shape = 21 w ggplot: kółko w kolorze obrysu i mniejsze kółko tła na wierzchu.
  function rings(data, color, P) {
    // Wnętrze rysowane po obrysie, więc na wierzchu (kolejność warstw w silniku).
    return [{ type: "points", data: data, color: color, size: 15 },
            { type: "points", data: data, color: P.surface, size: 9 }];
  }
  // Liczby nad słupkami (geom_text(vjust = -0.4)).
  // above: wysokości kółek — napis nad kółkiem (pad: promień kółka w jednostkach osi y),
  // gdy kółko zasłoniłoby liczbę.
  function barLabels(xs, ys, P, above, pad) {
    return xs.map(function (x, i) {
      return { type: "text", x: x, y: above ? Math.max(ys[i], above[i] + pad) : ys[i], text: String(ys[i]), color: P.ink };
    });
  }

  // ---- Ryc. 3.6: czy V-1 celowały? ----------------------------------------------
  // Mapa 24 × 24 kwadraty; trafienia jednostajnie losowe (stan startowy z ziarnem 1944).
  var V1_N = 537, V1_SIDE = 24;
  function scatterHits(g) {
    var out = [];
    for (var i = 0; i < V1_N; i++) out.push([g.unif(0, V1_SIDE), g.unif(0, V1_SIDE)]);
    return out;
  }
  // Liczba trafień w każdej komórce siatki (table(floor(x), floor(y))).
  function cellCounts(hits) {
    var n = new Array(V1_SIDE * V1_SIDE).fill(0);
    hits.forEach(function (h) {
      var cx = Math.min(V1_SIDE - 1, Math.floor(h[0])), cy = Math.min(V1_SIDE - 1, Math.floor(h[1]));
      n[cy * V1_SIDE + cx]++;
    });
    return n;
  }
  var V1_MAP_NOTE = "Południowy Londyn, 1944 · 537 trafień (położenia losowe, nie historyczne)";
  var V1_FIT_NOTE = "Słupki: Londyn 1944 · kółka: 537 trafień rozrzuconych zupełnie losowo";

  var V1 = {
    init: function (state) { state.hits = scatterHits(D.rng(1944)); },
    act: function (state, name) { if (name === "shuffle") state.hits = scatterHits(D.r); },
    draw: function (v, P, k, state, vals, step) {
      var cfg = state.config;
      state.step = step;
      if (step <= 2) {
        var layers = [];
        if (step === 2) {
          // Komórki: kolor od tła do terakoty według min(liczba, 4) / 4; obrys siatki.
          var cnt = cellCounts(state.hits), items = [], lines = [];
          cnt.forEach(function (c, i) {
            if (!c) return;
            var cx = i % V1_SIDE, cy = Math.floor(i / V1_SIDE);
            items.push({ r: [cx, cx + 1, cy, cy + 1], color: C.alpha(P.terakota, Math.min(c, 4) / 4) });
          });
          for (var t = 0; t <= V1_SIDE; t++) {
            lines.push({ from: [t, 0], to: [t, V1_SIDE] }, { from: [0, t], to: [V1_SIDE, t] });
          }
          layers.push({ type: "cells", items: items }, { type: "segs", items: lines, color: P.rule, width: 0.8, z: 2 });
        }
        layers.push({ type: "points", data: state.hits, color: P.ink, alpha: 0.8, size: 5, z: 4 });
        return { x: [0, V1_SIDE], y: [0, V1_SIDE], xTicks: [], hideY: true, noXGrid: true, noYGrid: true, layers: layers,
                 legend: step === 2 ? { title: "Trafienia", from: "0", to: "4+", stops: [P.surface, P.terakota] } : null,
                 aria: step === 1
                   ? "Mapa południowego Londynu z 537 trafieniami V-1 rozrzuconymi losowo"
                   : "Mapa podzielona na 576 kwadratów; im więcej trafień w kwadracie, tym ciemniejszy kolor" };
      }
      var ks = M.range(0, 5), obs = cfg.obs;
      var spec = { x: [-0.6, 5.6], y: [0, 260], xLabels: { 0: "0", 1: "1", 2: "2", 3: "3", 4: "4", 5: "5+" }, noXGrid: true,
                   xName: "Liczba trafień w kwadracie", yName: "Liczba kwadratów",
                   layers: [{ type: "bins", data: M.bars(obs, 0.7, 0), color: P.secondary, alpha: 0.85 }]
                     .concat(barLabels(ks, obs, P, step === 4 ? cfg.exp : null, 7)),
                   aria: "Liczba kwadratów z 0, 1, 2, 3, 4 i 5 lub więcej trafieniami: " + obs.join(", ") };
      if (step === 4) {
        spec.layers = spec.layers.concat(rings(ks.map(function (x, i) { return [x, cfg.exp[i]]; }), P.accent, P));
        spec.aria += "; kółka: liczby oczekiwane przy zupełnie losowych trafieniach";
      }
      return spec;
    },
    update: function (root, state) {
      S.setValue(root, "v1_note", state.step <= 2 ? V1_MAP_NOTE : state.step === 4 ? V1_FIT_NOTE : "");
    },
    text: function (state, v, vals, step) { return S.esc(state.config.texts[step - 1]); }
  };

  // ---- Ryc. 3.9: konie Bortkiewicza -------------------------------------------------
  var HORSES = {
    draw: function (v, P, k, state, vals, step) {
      var cfg = state.config, ks = cfg.k, obs = cfg.obs;
      var layers = [{ type: "bins", data: M.bars(obs, 0.7, 0), color: P.secondary, alpha: 0.85 }]
        .concat(barLabels(ks, obs, P, step >= 3 ? cfg.exp : null, 3.5));
      var aria = "Liczba korpuso-lat z 0, 1, 2, 3 i 4 zabitymi: " + obs.join(", ");
      if (step >= 2) {
        layers.push({ type: "vline", x: cfg.mean, color: P.accent, width: 2, dash: [4, 4] },
                    { type: "text", x: cfg.mean, y: 118, text: cfg.label, color: P.accent, bold: true, align: "left", valign: "middle" });
        aria += "; " + cfg.label;
      }
      if (step >= 3) {
        var pts = ks.map(function (x, i) { return [x, cfg.exp[i]]; });
        layers = layers.concat(rings(pts, P.accent, P));
        // Wartości tuż za krawędzią słupka (na tle, nie na ciemnym słupku), po stronie
        // dalszej od linii średniej, żeby linia nie przecinała liczby.
        pts.forEach(function (p) {
          var left = p[0] < cfg.mean;
          layers.push({ type: "text", x: p[0] + (left ? -0.38 : 0.38), y: p[1], text: fmt(p[1], 1), color: P.accent,
                        align: left ? "right" : "left", valign: "middle", size: 0.9 });
        });
        aria += "; kółka: oczekiwane liczby z rozkładu Poissona " + cfg.exp.map(function (e) { return fmt(e, 1); }).join(", ");
      }
      return { x: [-0.6, 4.6], y: [0, 125], xTicks: ks, noXGrid: true,
               xName: "Zabici kopnięciem konia w korpusie w ciągu roku", yName: "Liczba korpuso-lat",
               layers: layers, aria: aria };
    },
    text: function (state, v, vals, step) { return S.esc(state.config.texts[step - 1]); }
  };

  // ---- Ryc. 4.10: paradoks autobusu --------------------------------------------------
  // regular: odjazdy co 10 min; random: odstępy wykładnicze o średniej 10 min.
  function busTimes(mode) {
    var out = [], t = 0;
    for (var i = 1; i <= 400; i++) { t = mode === "random" ? t + D.r.exp(1 / 10) : 10 * i; out.push(t); }
    return out;
  }
  // Liczba odjazdów ≤ t (findInterval w R), wyszukiwanie binarne.
  function countBefore(b, t) {
    var lo = 0, hi = b.length;
    while (lo < hi) { var mid = (lo + hi) >> 1; if (b[mid] <= t) lo = mid + 1; else hi = mid; }
    return lo;
  }
  function addPax(state, n) {
    var b = state.buses, end = b[b.length - 1] - 50;
    for (var i = 0; i < n; i++) {
      var t = D.r.unif(0, end), j = countBefore(b, t), prev = j === 0 ? 0 : b[j - 1];
      state.pax.push({ t: t, wait: b[j] - t, gap: b[j] - prev });
    }
  }
  function paxMean(state, key) { return M.mean(state.pax.map(function (p) { return p[key]; })); }

  var BUS = {
    prepare: function (state, v) {
      // Zmiana trybu: nowy rozkład odjazdów, pasażerowie od zera.
      if (state.mode !== v) { state.mode = v; state.buses = busTimes(v); state.pax = []; }
    },
    act: function (state, name) {
      if (name === "reset") { state.pax = []; return; }
      addPax(state, Number(name.replace("p", "")));
    },
    draw: function (v, P, k, state) {
      var pax = state.pax;
      if (k === 0) {
        var c0 = pax.length ? pax[pax.length - 1].t : 60, w0 = Math.max(0, c0 - 60), win = [w0, w0 + 120];
        var bb = state.buses.filter(function (t) { return t >= win[0] && t <= win[1]; });
        var layers = [{ type: "segment", from: [win[0], 0], to: [win[1], 0], color: P.rule, width: 3 },
                      { type: "points", data: bb.map(function (t) { return [t, 0]; }), color: P.secondary, symbol: "rect", size: 12 }];
        var legend = [{ text: "Odjazdy autobusów", color: P.secondary, box: true }], aria = "Oś czasu z odjazdami autobusów";
        if (pax.length) {
          var last = pax[pax.length - 1], lab = "czeka " + fmt(last.wait, 1) + " min";
          layers.push({ type: "arrow", from: [last.t, 0.4], to: [last.t + last.wait, 0.4], color: P.accent, width: 3 },
                      { type: "point", x: last.t, y: 0.4, color: P.accent, size: 11 },
                      { type: "text", x: last.t, y: 0.85, text: lab, color: P.accent, bold: true, align: "left", valign: "middle" });
          legend.push({ text: "Ostatni pasażer i jego czekanie", color: P.accent });
          aria += "; ostatni pasażer " + lab;
        }
        return { x: win, y: [-1, 1], hideY: true, noXGrid: true, noYGrid: true, xName: "Czas (min)",
                 layers: layers, legend: legend, aria: aria };
      }
      if (!pax.length) return M.empty("Wpuść pasażerów");
      // Histogram binwidth = 1 od 0, przedziały domknięte z prawej (geom_histogram).
      var waits = pax.map(function (p) { return p.wait; }), bins = S.binsWidth(waits, 1, 0);
      var mw = M.mean(waits), top = M.max(bins.map(function (b) { return b.n; })) * 1.12;
      return { x: [0, 45], y: [0, top], xTicks: [0, 10, 20, 30, 40],
               xName: "Czas czekania na przystanku (min)", yName: "Liczba pasażerów",
               layers: [{ type: "bins", data: bins.map(function (b) { return [b.l, b.r, b.n]; }), color: P.secondary, alpha: 0.85, stroke: P.surface },
                        { type: "vline", x: mw, color: P.accent, width: 2 },
                        { type: "text", x: mw, y: "top", text: "średnio " + fmt(mw, 1) + " min", color: P.accent, bold: true, align: "left" }],
               aria: "Histogram czasu czekania " + pax.length + " pasażerów; średnio " + fmt(mw, 1) + " min" };
    },
    update: function (root, state) {
      var n = state.pax.length;
      S.setValue(root, "bus_n", String(n));
      S.setValue(root, "bus_wait", n ? fmt(paxMean(state, "wait"), 1) + " min" : "–");
      S.setValue(root, "bus_gap", n ? fmt(paxMean(state, "gap"), 1) + " min" : "–");
    }
  };

  // ---- Ryc. 5.13: losowe podwyżki ----------------------------------------------------
  // 1000 osób od 5000 zł; co rok pensja × (1 + N(0.04, 0.12)), najmniej 100 zł.
  function startPay() { return { year: 0, x: new Array(1000).fill(5000) }; }
  function median(a) {
    var s = a.slice().sort(function (p, q) { return p - q; }), n = s.length;
    return n % 2 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2;
  }
  function payStats(x) {
    var m = M.mean(x), below = 0;
    x.forEach(function (t) { if (t < m) below++; });
    return { mean: m, median: median(x), below: below / x.length };
  }

  var RAISES = {
    init: function (state) { state.pay = startPay(); },
    act: function (state, name) {
      if (name === "reset") { state.pay = startPay(); return; }
      var years = name === "y10" ? 10 : 1, x = state.pay.x;
      for (var y = 0; y < years; y++) for (var i = 0; i < x.length; i++) x[i] *= 1 + D.r.norm(0.04, 0.12);
      for (var j = 0; j < x.length; j++) x[j] = Math.max(x[j], 100);
      state.pay.year += years;
    },
    draw: function (v, P, k, state) {
      var lg = v === "log", s = state.pay, tr = lg ? Math.log : function (t) { return t; };
      var spec = { xName: lg ? "ln(pensja)" : "Pensja (zł)", yName: "Liczba osób", xBig: !lg, layers: [] };
      if (s.year === 0) {
        var x0 = tr(s.x[0]), half = lg ? 0.2 : 1000;
        spec.x = [x0 - half, x0 + half]; spec.y = [0, 1100];
        spec.layers.push({ type: "vline", x: x0, from: 0, to: 1000, color: P.secondary, width: 5 });
        spec.aria = "Rok 0: wszystkie 1000 osób zarabia 5000 zł";
        return spec;
      }
      var vals = s.x.map(tr), st = payStats(s.x), bins = S.binsGgplot(vals, 40);
      var lo = bins[0].l, hi = bins[bins.length - 1].r, pad = (hi - lo) * 0.03;
      spec.x = [lo - pad, hi + pad];
      spec.y = [0, M.max(bins.map(function (b) { return b.n; })) * 1.06];
      spec.layers.push({ type: "bins", data: bins.map(function (b) { return [b.l, b.r, b.n]; }), color: P.secondary, alpha: 0.8, stroke: P.surface },
                       { type: "vline", x: tr(st.mean), color: P.accent, width: 2 },
                       { type: "vline", x: tr(st.median), color: P.ink, width: 2, dash: [5, 5] });
      spec.legend = [{ text: "Średnia", color: P.accent }, { text: "Mediana", color: P.ink, dash: true }];
      spec.aria = "Histogram " + (lg ? "logarytmu pensji" : "pensji") + " 1000 osób po " + s.year + " latach; średnia " +
        C.fmt(st.mean, 0, true) + " zł, mediana " + C.fmt(st.median, 0, true) + " zł";
      return spec;
    },
    update: function (root, state) {
      var s = state.pay, st = payStats(s.x);
      S.setValue(root, "pay_year", String(s.year));
      S.setValue(root, "pay_mean", C.fmt(st.mean, 0, true) + " zł");
      S.setValue(root, "pay_median", C.fmt(st.median, 0, true) + " zł");
      S.setValue(root, "pay_below", fmt(100 * st.below, 0) + "%");
    },
    noteKey: function (vals, state) { return state.pay.year === 0 ? "start" : vals.s3p_4f_scale; }
  };

  S.register({ "s3p-v1": V1, "s3p-horses": HORSES, "s3p-bus": BUS, "s3p-raises": RAISES });
})();
