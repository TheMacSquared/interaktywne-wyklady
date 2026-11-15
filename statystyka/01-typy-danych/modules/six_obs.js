// Ryc. 3.5 „Szósta obserwacja we wzorach” (wykład 01): pięć pensji i pole x6.
// Średnią, medianę i rozpisane wzory liczy przeglądarka (dawniej ch3_server).
// Kontener: div[data-six-obs] z data-config = { base: [...], input, low, ceo, clear }
// (id pola i przycisków); odczyty to [data-six="mean" | "median"] w tym samym panelu.
// Wzory są składane w HTML (ułamek = .lc-hfrac), bo strona statyczna nie ładuje MathJax.
// Funkcje liczące są w globalThis.lcS01Six (testy: tests/js/six_obs.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;

  // Liczba jak lc_fmt(v, 1, big_mark = " ") w R: jedno miejsce po kropce bez
  // końcowego zera, tysiące oddzielone spacją.
  // Dokładną połówkę (np. 5.25) R zaokrągla do cyfry parzystej, toFixed() w górę.
  function fixed1(a) {
    var m = /^(\d+)\.(\d)(\d*)$/.exec(a.toFixed(25));
    if (m && /^50*$/.test(m[3]) && Number(m[2]) % 2 === 0) return m[1] + "." + m[2];
    return a.toFixed(1);
  }
  function fmt(v) {
    var s = fixed1(Math.abs(v)).replace(/\.0$/, "");
    var parts = s.split(".");
    parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, " ");
    s = parts.join(".");
    return (v < 0 && Number(s.replace(/ /g, "")) !== 0 ? "-" : "") + s;
  }
  function mean(x) { return x.reduce(function (a, b) { return a + b; }, 0) / x.length; }
  function median(x) {
    var s = x.slice().sort(function (a, b) { return a - b; }), n = s.length;
    return n % 2 === 1 ? s[(n - 1) / 2] : (s[n / 2 - 1] + s[n / 2]) / 2;
  }
  // Szósta wartość z pola: liczba skończona albo null (puste lub niepoprawne pole).
  function parse(text) {
    if (text === null || text === undefined || String(text).trim() === "") return null;
    var v = Number(text);
    return isFinite(v) ? v : null;
  }
  // Wszystko, czego potrzebują wzory: wartości, posortowane z numerami wyjściowymi,
  // pozycje środkowe (od 1), średnia, mediana i zmiana względem pięciu pensji.
  function compute(base, extra) {
    var x = extra === null ? base.slice() : base.concat([extra]);
    var n = x.length, hasNew = extra !== null;
    var order = x.map(function (v, i) { return i; }).sort(function (a, b) { return x[a] - x[b] || a - b; });
    var mid = n % 2 === 1 ? [(n + 1) / 2] : [n / 2, n / 2 + 1];
    var m = mean(x), med = median(x);
    return { x: x, n: n, hasNew: hasNew, order: order, sorted: order.map(function (i) { return x[i]; }), mid: mid,
      sum: x.reduce(function (a, b) { return a + b; }, 0), mean: m, median: med,
      meanExact: Math.abs(Math.round(m * 10) / 10 - m) <= 1e-9,
      dMean: m - mean(base), dMedian: med - median(base) };
  }
  function signed(d) {
    return (d > 0 ? "+" : d < 0 ? "−" : "") + fmt(Math.abs(d)) + " tys. zł";
  }
  function frac(num, den) {
    return '<span class="lc-hfrac"><span class="lc-hfrac-n">' + num + '</span><span class="lc-hfrac-d">' + den + "</span></span>";
  }
  function xsub(k) { return "<i>x</i><sub>(" + k + ")</sub>"; }

  function html(r) {
    var terms = r.x.map(function (v, i) {
      var t = fmt(v);
      return r.hasNew && i === r.n - 1 ? '<span class="ch3-six-new">' + t + "</span>" : t;
    });
    var meanEq = '<span class="lc-hmath"><i>x̄</i> = ' + frac(terms.join(" + "), r.n) + " = " +
      frac(fmt(r.sum), r.n) + (r.meanExact ? " = " : " ≈ ") + fmt(r.mean) + "</span>";
    var cells = r.sorted.map(function (v, i) {
      var cls = "ch3-six-cell" + (r.mid.indexOf(i + 1) >= 0 ? " is-mid" : "") +
        (r.hasNew && r.order[i] === r.n - 1 ? " is-new" : "");
      return '<div class="' + cls + '"><span class="ch3-six-idx">x(' + (i + 1) + ')</span><span class="ch3-six-val">' +
        fmt(v) + "</span></div>";
    }).join("");
    var medEq;
    if (r.n % 2 === 1) {
      medEq = "<i>n</i> = " + r.n + " (nieparzyste):&emsp;Me = " + xsub(r.mid[0]) + " = " + fmt(r.sorted[r.mid[0] - 1]);
    } else {
      var a = r.sorted[r.mid[0] - 1], b = r.sorted[r.mid[1] - 1];
      medEq = "<i>n</i> = " + r.n + " (parzyste):&emsp;Me = " + frac(xsub(r.mid[0]) + " + " + xsub(r.mid[1]), 2) +
        " = " + frac(fmt(a) + " + " + fmt(b), 2) + " = " + fmt(r.median);
    }
    var caption = r.hasNew
      ? "Względem pięciu pensji średnia zmieniła się o " + signed(r.dMean) + ", a mediana o " + signed(r.dMedian) + "."
      : "Pole x6 jest puste: liczymy z pięciu obserwacji.";
    return '<div class="ch3-six-label">Średnia: suma wszystkich wartości przez ich liczbę</div>' +
      '<div class="lc-formula-box">' + meanEq + "</div>" +
      '<div class="ch3-six-label">Mediana: najpierw sortujemy, potem bierzemy środek</div>' +
      '<div class="ch3-six-row is-sorted">' + cells + "</div>" +
      '<div class="lc-formula-box"><span class="lc-hmath">' + medEq + "</span></div>" +
      '<div class="lc-caption"><div>' + caption + "</div></div>";
  }
  root.lcS01Six = { fmt: fmt, parse: parse, compute: compute, signed: signed, html: html };
  if (typeof document === "undefined") return;

  function init(box) {
    box.setAttribute("data-ready", "true");
    var cfg = JSON.parse(box.getAttribute("data-config"));
    var panel = box.closest(".lc-figure-panel") || document;
    var input = document.getElementById(cfg.input);
    function render() {
      var r = compute(cfg.base, parse(input ? input.value : ""));
      box.innerHTML = html(r);
      var set = function (name, value) {
        var el = panel.querySelector('[data-six="' + name + '"]');
        if (el) el.textContent = fmt(value) + " tys. zł";
      };
      set("mean", r.mean); set("median", r.median);
    }
    function preset(id, value) {
      var btn = document.getElementById(id);
      if (btn) btn.addEventListener("click", function () {
        if (input) input.value = value;
        render();
      });
    }
    preset(cfg.low, "1"); preset(cfg.ceo, "60"); preset(cfg.clear, "");
    if (input) {
      input.addEventListener("input", render);
      input.addEventListener("change", render);
    }
    render();
  }

  function scan() { document.querySelectorAll("[data-six-obs]:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
