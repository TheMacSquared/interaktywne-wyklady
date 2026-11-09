// Wspólne narzędzia rycin ECharts wykładu 01 (Ryc. 3.4, 3.6, 3.9, 3.10, 5.5):
// statystyki liczone jak w R, losowanie, słupki histogramu (seria custom)
// i podpisy linii (graphic). Cykl życia, kolory i formatowanie: R/lc_charts.js.
// Eksport: window.lc01.
(function () {
  var Charts = window.lcCharts;
  // ---- Statystyki (zgodne z R) -------------------------------------------
  function sorted(x) { return x.slice().sort(function (a, b) { return a - b; }); }
  // Sumowanie Neumaiera zachowuje małe składniki przy znoszących się liczbach.
  function sum(x) {
    var s = 0, correction = 0;
    for (var i = 0; i < x.length; i++) {
      var next = s + x[i];
      correction += Math.abs(s) >= Math.abs(x[i]) ? (s - next) + x[i] : (x[i] - next) + s;
      s = next;
    }
    return s + correction;
  }
  function mean(x) {
    var n = x.length; if (!n) return NaN;
    return sum(x) / n;
  }
  function median(x) {
    var s = sorted(x), n = s.length; if (!n) return NaN;
    var h = Math.floor(n / 2);
    return n % 2 ? s[h] : (s[h - 1] + s[h]) / 2;
  }
  // quantile(x, p, type = 7) z tą samą tolerancją co R (fuzz = 4 · eps).
  function quantile7(x, p, isSorted) {
    var s = isSorted ? x : sorted(x), n = s.length; if (!n) return NaN;
    var fuzz = 4 * Number.EPSILON, nppm = (n - 1) * p;
    var j = Math.floor(nppm + fuzz), h = nppm - j;
    if (Math.abs(h) < fuzz) h = 0;
    var lo = s[Math.max(0, Math.min(n - 1, j))], hi = s[Math.max(0, Math.min(n - 1, j + 1))];
    return h === 0 ? lo : (1 - h) * lo + h * hi;
  }
  // mean(x, trim = t) w R: lo = floor(n · t) + 1, hi = n + 1 − lo (indeksy od 1).
  function trimMean(x, t) {
    var n = x.length; if (!n) return NaN;
    if (t >= 0.5) return median(x);
    var lo = Math.floor(n * t) + 1, hi = n + 1 - lo;
    return mean(sorted(x).slice(lo - 1, hi));
  }

  // ---- Losowanie ---------------------------------------------------------
  function runif(a, b) { return a + (b - a) * Math.random(); }
  function sampleInt(a, b) { return a + Math.floor(Math.random() * (b - a + 1)); }
  var spare = null;
  function rnorm(mu, sd) {
    var z;
    if (spare !== null) { z = spare; spare = null; } else {
      var u = 1 - Math.random(), v = Math.random(), r = Math.sqrt(-2 * Math.log(u));
      z = r * Math.cos(2 * Math.PI * v); spare = r * Math.sin(2 * Math.PI * v);
    }
    return mu + sd * z;
  }
  // Gamma o całkowitym kształcie k: suma k wykładniczych (dokładnie, bez przybliżeń).
  function rgammaInt(k, scale) {
    var s = 0; for (var i = 0; i < k; i++) s -= Math.log(1 - Math.random());
    return s * scale;
  }

  // ---- Rozrzut i kształt (jak sd() w R i e1071 typu 3) -------------------------
  function sd(x) {
    var n = x.length; if (n < 2) return NaN;
    var m = mean(x);
    return Math.sqrt(sum(x.map(function (v) { return (v - m) * (v - m); })) / (n - 1));
  }
  // Moment centralny rzędu k podzielony przez s^k (s z mianownikiem n − 1).
  function stdMoment(x, k) {
    var n = x.length, m = mean(x), s = sd(x);
    return sum(x.map(function (v) { return Math.pow((v - m) / s, k); })) / n;
  }
  function skewness(x) { return x.length < 3 ? NaN : stdMoment(x, 3); }
  function kurtosis(x) { return x.length < 4 ? NaN : stdMoment(x, 4) - 3; }

  // ---- Krzywa gęstości (jądro gaussowskie, pasmo nrd0 jak density() w R) -------
  function bwNrd0(x) {
    var hi = sd(x), lo = Math.min(hi, (quantile7(x, 0.75) - quantile7(x, 0.25)) / 1.34);
    if (!(lo > 0)) lo = hi || Math.abs(x[0]) || 1;
    return 0.9 * lo * Math.pow(x.length, -0.2);
  }
  // Gęstość w n punktach od lo do hi, liczona dokładnie (density() w R przybliża
  // ją przez FFT; różnice są niewidoczne na wykresie). Zwraca [[x, y], ...].
  function kde(x, lo, hi, n, bw) {
    var h = bw || bwNrd0(x), k = n || 256, out = [], c = 1 / (x.length * h * Math.sqrt(2 * Math.PI));
    for (var i = 0; i < k; i++) {
      var t = lo + (hi - lo) * i / (k - 1), s = 0;
      for (var j = 0; j < x.length; j++) { var z = (t - x[j]) / h; s += Math.exp(-0.5 * z * z); }
      out.push([t, s * c]);
    }
    return out;
  }

  // ---- Losowanie z ziarnem ---------------------------------------------------
  // Stałe scenariusze (dawniej set.seed() w R) mają powtarzalne dane. Sekwencje
  // nie są identyczne z R; zgodne są rozkłady i parametry.
  function seeded(seed) {
    var state = seed >>> 0, spare2 = null;
    function u() {  // mulberry32
      state = (state + 0x6D2B79F5) >>> 0;
      var t = state;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    }
    function norm(mu, s) {
      var z;
      if (spare2 !== null) { z = spare2; spare2 = null; } else {
        var a = 1 - u(), b = u(), r = Math.sqrt(-2 * Math.log(a));
        z = r * Math.cos(2 * Math.PI * b); spare2 = r * Math.sin(2 * Math.PI * b);
      }
      return mu + s * z;
    }
    // Gamma o dowolnym kształcie (Marsaglia–Tsang; kształt < 1 przez wzmocnienie).
    function gamma(shape, scale) {
      if (shape < 1) return gamma(shape + 1, scale) * Math.pow(u(), 1 / shape);
      var d = shape - 1 / 3, c = 1 / Math.sqrt(9 * d);
      for (;;) {
        var z = norm(0, 1), v = 1 + c * z;
        if (v <= 0) continue;
        v = v * v * v;
        var w = u();
        if (w < 1 - 0.0331 * z * z * z * z || Math.log(w) < 0.5 * z * z + d * (1 - v + Math.log(v))) return d * v * scale;
      }
    }
    function many(k, f) { var out = []; for (var i = 0; i < k; i++) out.push(f()); return out; }
    return {
      runif: function (a, b) { return a + (b - a) * u(); },
      rnorm: function (k, mu, s) { return many(k, function () { return norm(mu, s); }); },
      rgamma: function (k, shape, scale) { return many(k, function () { return gamma(shape, scale); }); }
    };
  }

  // ---- Ankieta studencka (lc01_survey_script() w survey_data.R) -----------------
  // Kolumna liczbowa: tablica liczb; czynnik: etykiety kategorii w kolejności poziomów.
  function survey(name) {
    var col = window.lc01Survey && window.lc01Survey.columns[name];
    if (!col) throw new Error("lc01: brak kolumny ankiety " + name);
    if (!col.levels) return col.values;
    return col.values.map(function (code) { return col.levels[code - 1]; });
  }
  function surveyMeta(name) { return window.lc01Survey.columns[name]; }
  // Liczebności kategorii w kolejności poziomów (jak table() w R).
  function counts(values, levels) {
    var n = levels.map(function () { return 0; });
    values.forEach(function (v) { var i = levels.indexOf(v); if (i >= 0) n[i]++; });
    return n;
  }

  // ---- Pomocniki ECharts ------------------------------------------------------
  // Słupki histogramu jako seria custom: dane [l, r, n]. Wygląd w obiekcie o
  // (zmienianym przez widget): {fill, fillLeft, split, gap}; przy split słupek
  // na lewo od tej wartości (także jego część) dostaje kolor fillLeft.
  function histRender(o) {
    return function (params, api) {
      var l = api.value(0), r = api.value(1), n = api.value(2);
      if (!n) return null;
      var a = api.coord([l, n]), b = api.coord([r, 0]);
      var x0 = a[0], x1 = b[0], y = a[1], h = b[1] - a[1];
      var kids = [{ type: "rect", shape: { x: x0, y: y, width: x1 - x0, height: h }, style: { fill: o.fill } }];
      if (o.split != null && o.fillLeft && l < o.split) {
        var sx = Math.min(x1, api.coord([o.split, 0])[0]);
        kids.push({ type: "rect", shape: { x: x0, y: y, width: sx - x0, height: h }, style: { fill: o.fillLeft } });
      }
      kids.push({ type: "rect", shape: { x: x0 + 0.5, y: y + 0.5, width: Math.max(0, x1 - x0 - 1), height: h },
                  style: { fill: "none", stroke: o.gap, lineWidth: 1 } });
      return { type: "group", children: kids };
    };
  }
  function histSeries(id, o) {
    return { id: id, type: "custom", renderItem: histRender(o), data: [], silent: true, animation: false,
             encode: { x: [0, 1], y: 2 }, clip: true };
  }
  function histData(bins) { return bins.map(function (b) { return [b.l, b.r, b.n]; }); }

  // Podpisy przy pionowych liniach: items [{x, text, color, row}] w wierszach od
  // górnej krawędzi siatki (y = ymax); przy prawej krawędzi (x = xmax) napis
  // przechodzi na lewą stronę linii. Zastępuje całą warstwę graphic wykresu.
  function lineLabels(chart, items, o) {
    var size = o.fontSize || 12, out = [];
    items.forEach(function (it, k) {
      var p = chart.convertToPixel({ gridIndex: 0 }, [it.x, o.ymax]);
      var edge = chart.convertToPixel({ gridIndex: 0 }, [o.xmax, o.ymax]);
      if (!p || !edge) return;
      var w = it.text.length * size * 0.62, right = p[0] + 5 + w <= edge[0];
      out.push({ type: "text", silent: true, z: 20, x: right ? p[0] + 5 : p[0] - 5 - w,
        y: p[1] + 4 + (it.row || 0) * size * 4 / 3,
        style: { text: it.text, fill: it.color, font: "600 " + size + "px " + (o.font || "monospace"),
                 backgroundColor: o.halo || "transparent", padding: [1, 2] } });
    });
    chart.setOption({ graphic: out }, { replaceMerge: ["graphic"] });
  }

  // Pionowa linia od 0 do y (seria line z dwoma punktami).
  function vline(x, y) { return x == null || !isFinite(x) ? [] : [[x, 0], [x, y]]; }

  // Wartość osi pod wskaźnikiem (przycięta do zakresu rysunku).
  function valueAt(chart, el, e, xr, yr) { return Charts.ecPointer(chart, el, e, xr, yr); }

  // Biny o stałej szerokości od lo do hi.
  function bins(values, lo, hi, width) {
    var k = Math.max(1, Math.round((hi - lo) / width)), out = [];
    for (var i = 0; i < k; i++) out.push({ l: lo + i * width, r: lo + (i + 1) * width, n: 0 });
    values.forEach(function (v) {
      var i = Math.floor((v - lo) / width);
      if (i === k && v <= hi + 1e-9) i = k - 1;
      if (i >= 0 && i < k) out[i].n++;
    });
    return out;
  }

  window.lc01 = {
    sorted: sorted, mean: mean, median: median, quantile7: quantile7, trimMean: trimMean,
    sd: sd, skewness: skewness, kurtosis: kurtosis, bwNrd0: bwNrd0, kde: kde, seeded: seeded,
    survey: survey, surveyMeta: surveyMeta, counts: counts,
    runif: runif, sampleInt: sampleInt, rnorm: rnorm, rgammaInt: rgammaInt,
    histSeries: histSeries, histData: histData, lineLabels: lineLabels, vline: vline,
    valueAt: valueAt, bins: bins
  };
})();
