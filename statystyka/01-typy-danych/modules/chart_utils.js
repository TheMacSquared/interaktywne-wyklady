// Wspólne narzędzia rycin Chart.js wykładu 01 (Ryc. 3.4, 3.6, 3.9, 3.10, 5.5):
// statystyki liczone jak w R, losowanie, histogram i podpisy linii rysowane
// wtyczkami. Cykl życia, kolory i formatowanie: R/lc_charts.js.
// Eksport: window.lc01.
(function () {
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

  // Stała szerokość osi Y: wykresy jeden pod drugim mają wtedy wspólną skalę X w pikselach.
  function fixedWidth(w) { return function (scale) { scale.width = w; }; }

  // ---- Wtyczki rysujące ------------------------------------------------------
  // options.plugins.lcHist = { bins: [{l, r, n}], fill, split: x | null, fillLeft, gap }
  var histPlugin = {
    id: "lcHist",
    beforeDatasetsDraw: function (chart, args, o) {
      if (!o || !o.bins || !o.bins.length) return;
      var ctx = chart.ctx, a = chart.chartArea, xs = chart.scales.x, ys = chart.scales.y;
      function rects(color) {
        ctx.fillStyle = color;
        o.bins.forEach(function (b) {
          if (!b.n) return;
          var x0 = xs.getPixelForValue(b.l), x1 = xs.getPixelForValue(b.r), y = ys.getPixelForValue(b.n);
          ctx.fillRect(x0, y, x1 - x0, ys.getPixelForValue(0) - y);
        });
      }
      ctx.save();
      ctx.beginPath(); ctx.rect(a.left, a.top, a.right - a.left, a.bottom - a.top); ctx.clip();
      rects(o.fill);
      if (o.split != null && o.fillLeft) {
        var sx = xs.getPixelForValue(o.split);
        ctx.beginPath(); ctx.rect(a.left, a.top, Math.max(0, sx - a.left), a.bottom - a.top); ctx.clip();
        rects(o.fillLeft);
      }
      ctx.restore();
      // Odstępy między słupkami w kolorze tła.
      ctx.save();
      ctx.beginPath(); ctx.rect(a.left, a.top, a.right - a.left, a.bottom - a.top); ctx.clip();
      ctx.strokeStyle = o.gap; ctx.lineWidth = 1;
      o.bins.forEach(function (b) {
        if (!b.n) return;
        var x0 = xs.getPixelForValue(b.l), x1 = xs.getPixelForValue(b.r), y = ys.getPixelForValue(b.n);
        ctx.strokeRect(x0 + 0.5, y + 0.5, x1 - x0 - 1, ys.getPixelForValue(0) - y);
      });
      ctx.restore();
    }
  };
  // options.plugins.lcLabels = { items: [{x, text, color, row}], font }
  // Podpis przy pionowej linii w wierszu row (od góry obszaru wykresu);
  // przy prawej krawędzi napis przechodzi na lewą stronę linii.
  var labelPlugin = {
    id: "lcLabels",
    afterDatasetsDraw: function (chart, args, o) {
      if (!o || !o.items || !o.items.length) return;
      var ctx = chart.ctx, a = chart.chartArea, xs = chart.scales.x;
      ctx.save();
      ctx.font = "600 12px " + (o.font || "monospace");
      ctx.textBaseline = "top";
      o.items.forEach(function (it) {
        var px = xs.getPixelForValue(it.x), w = ctx.measureText(it.text).width;
        var right = px + 5 + w <= a.right;
        var tx = right ? px + 5 : px - 5 - w, ty = a.top + 4 + (it.row || 0) * 16;
        if (o.halo) { ctx.fillStyle = o.halo; ctx.fillRect(tx - 2, ty - 1, w + 4, 15); }
        ctx.fillStyle = it.color;
        ctx.fillText(it.text, tx, ty);
      });
      ctx.restore();
    }
  };

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

  // Wartość osi X pod wskaźnikiem (przycięta do obszaru wykresu).
  function valueAtX(chart, e) {
    var r = chart.canvas.getBoundingClientRect(), a = chart.chartArea;
    var px = Math.max(a.left, Math.min(a.right, e.clientX - r.left));
    return chart.scales.x.getValueForPixel(px);
  }
  function valueAtY(chart, e) {
    var r = chart.canvas.getBoundingClientRect(), a = chart.chartArea;
    var py = Math.max(a.top, Math.min(a.bottom, e.clientY - r.top));
    return chart.scales.y.getValueForPixel(py);
  }
  function pixelX(chart, e) { return e.clientX - chart.canvas.getBoundingClientRect().left; }

  window.lc01 = {
    sorted: sorted, mean: mean, median: median, quantile7: quantile7, trimMean: trimMean,
    runif: runif, sampleInt: sampleInt, rnorm: rnorm, rgammaInt: rgammaInt,
    fixedWidth: fixedWidth, histPlugin: histPlugin, labelPlugin: labelPlugin,
    bins: bins, valueAtX: valueAtX, valueAtY: valueAtY, pixelX: pixelX
  };
})();
