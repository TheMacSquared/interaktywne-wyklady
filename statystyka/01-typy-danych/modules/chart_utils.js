// Wspólne narzędzia rycin Chart.js wykładu 01 (Ryc. 3.4, 3.6, 3.9, 3.10, 5.5):
// statystyki liczone jak w R, losowanie, kolory z tokenów CSS, histogram
// i podpisy linii rysowane wtyczkami, start po pojawieniu się kontenera w DOM
// (rozdziały renderują się przez renderUI). Eksport: window.lc01.
(function () {
  // ---- Statystyki (zgodne z R) -------------------------------------------
  function sorted(x) { return x.slice().sort(function (a, b) { return a - b; }); }
  function sum(x) { var s = 0; for (var i = 0; i < x.length; i++) s += x[i]; return s; }
  // mean() w R: suma / n, potem poprawka o średnią reszt (jak w summary.c).
  function mean(x) {
    var n = x.length; if (!n) return NaN;
    var m = sum(x) / n, r = 0;
    for (var i = 0; i < n; i++) r += x[i] - m;
    return m + r / n;
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

  // ---- Liczby jako tekst: kropka dziesiętna, zwykły minus, opcjonalnie spacja tysięcy
  // toFixed zaokrągla dokładną wartość binarną, jak round() i sprintf() w R:
  // mediana (170.6 + 170.7) / 2 = 170.64999999999998 daje 170.6 w obu.
  function fmt(v, d, big) {
    if (!isFinite(v)) return "–";
    var s = Math.abs(v).toFixed(d || 0);
    if (big) {
      var parts = s.split(".");
      parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, " ");
      s = parts.join(".");
    }
    var neg = v < 0 && parseFloat(Math.abs(v).toFixed(d || 0)) !== 0;
    return (neg ? "-" : "") + s;
  }
  function round(v, d) { return parseFloat(v.toFixed(d || 0)); }
  function fmtSigned(v, d, big) {
    var r = parseFloat(Math.abs(v).toFixed(d || 0));
    return (v > 0 && r !== 0 ? "+" : "") + fmt(v, d, big);
  }

  // ---- Kolory --------------------------------------------------------------
  function token(name, fallback) {
    var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
    return v || fallback || "#888";
  }
  function alpha(hex, a) {
    var h = String(hex).replace("#", "");
    if (h.length === 3) h = h.split("").map(function (c) { return c + c; }).join("");
    if (!/^[0-9a-f]{6}$/i.test(h)) return hex;
    var n = parseInt(h, 16);
    return "rgba(" + (n >> 16 & 255) + "," + (n >> 8 & 255) + "," + (n & 255) + "," + a + ")";
  }
  function palette() {
    return {
      ink: token("--upwr-ink", "#222"), soft: token("--upwr-ink-soft", "#444"),
      subtle: token("--upwr-ink-subtle", "#888"), rule: token("--upwr-rule", "#ccc"),
      ruleSoft: token("--upwr-rule-soft", "#ddd"), surface: token("--upwr-surface", "#fff"),
      ref: token("--upwr-reference", "#8b8175"), accent: token("--upwr-accent", "#7a2e3b"),
      niebo: token("--upwr-cat-niebo", "#2f6fa8"), szalwia: token("--upwr-cat-szalwia", "#5b7f3a"),
      bursztyn: token("--upwr-cat-bursztyn", "#b06a12"), terakota: token("--upwr-cat-terakota", "#a4472b"),
      secondary: token("--upwr-secondary", "#555"), mono: token("--upwr-mono", "monospace")
    };
  }
  // Osie: siatka, ramka, podziałki i tytuły w kolorach motywu, czcionka mono.
  function styleAxes(chart, P) {
    Object.keys(chart.options.scales).forEach(function (k) {
      var s = chart.options.scales[k];
      s.grid = Object.assign(s.grid || {}, { color: P.ruleSoft });
      s.border = Object.assign(s.border || {}, { color: P.rule });
      s.ticks = Object.assign(s.ticks || {}, { color: P.subtle, font: { family: P.mono, size: 12 } });
      // Na wąskim ekranie podziałki X są pomijane, a nie obracane.
      if (k === "x") s.ticks.maxRotation = 0;
      if (s.title) { s.title.color = P.subtle; s.title.font = { family: P.mono, size: 12 }; }
    });
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

  // Start widgetów: init(root) dla każdego nowego kontenera pasującego do selektora.
  function onReady(selector, init) {
    function scan() {
      if (typeof Chart === "undefined") return;
      document.querySelectorAll(selector + ":not([data-ready])").forEach(function (root) {
        root.dataset.ready = "1";
        try { init(root); } catch (err) { console.error(selector, err); }
      });
    }
    new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
    document.addEventListener("DOMContentLoaded", scan);
    window.addEventListener("load", scan);
    scan();
  }
  // Przemalowanie przy zmianie motywu (jasny / ciemny / halloweenowy).
  function onTheme(fn) {
    new MutationObserver(fn).observe(document.documentElement,
      { attributes: true, attributeFilter: ["data-lc-theme"] });
  }

  window.lc01 = {
    sorted: sorted, mean: mean, median: median, quantile7: quantile7, trimMean: trimMean,
    runif: runif, sampleInt: sampleInt, rnorm: rnorm, rgammaInt: rgammaInt,
    fmt: fmt, fmtSigned: fmtSigned, round: round, token: token, alpha: alpha, palette: palette,
    styleAxes: styleAxes, fixedWidth: fixedWidth, histPlugin: histPlugin, labelPlugin: labelPlugin,
    bins: bins, valueAtX: valueAtX, valueAtY: valueAtY, pixelX: pixelX,
    onReady: onReady, onTheme: onTheme
  };
})();
