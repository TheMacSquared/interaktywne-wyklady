// Sceny wykładu 05 „Ile prób do zdarzenia” w przeglądarce, bez serwera R.
//
// Kontener .d5p-scene[data-scene][data-config]; sloty [data-d5p-out="…"],
// przyciski [data-d5p="…"]. Sceny:
//   kadry     — stałe kadry policzone w R (SVG i opis każdego kadru w config.frames);
//               JS tylko przełącza: Dwóch audytorów, Po dwudziestu kontrolach bez wady,
//               Jeden audyt, Trzy kolejki czekania, Dziesięć audytów u każdego dostawcy
//   geo       — „Seria za serią”: serie do pierwszej wady, stos na osi 1–80, linia cięcia
//   oddzialy  — „Dwadzieścia oddziałów”: kafelki 20 audytów, potem histogram końców X
//   ciecie    — „Taśma przecięta na n”: X do trzeciej wady a Y_n, wzór (5.7)
// Rysunki są wiernym przeniesieniem funkcji d5p_* z R (stop_proto_*.R z 2026-11-04);
// funkcje rysujące są w window.lcD5Scenes (testy: tests/js/d5_sceny.test.js).
(function () {
  "use strict";

  // --- Liczby jak w R --------------------------------------------------------

  function g(x) { return x === 0 ? "0" : String(Number(x.toPrecision(6))); }  // sprintf("%g")
  // round() z R: połówki do parzystej
  function rround(x) {
    var r = Math.round(x);
    if (Math.abs(x % 1) === 0.5) r = 2 * Math.round(x / 2);
    return r;
  }
  function sub(n) { var m = "₀₁₂₃₄₅₆₇₈₉"; return String(n).replace(/\d/g, function (d) { return m[+d]; }); }
  function esc(s) { return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;"); }
  function sum(a) { var s = 0; for (var i = 0; i < a.length; i++) s += a[i] ? +a[i] : 0; return s; }
  function mean(a) { return sum(a) / a.length; }
  function range(a, b) { var o = []; for (var i = a; i <= b; i++) o.push(i); return o; }
  // tabulate(x, nbins): liczniki wartości 1..nbins
  function tabulate(xs, nb) {
    var c = range(1, nb).map(function () { return 0; });
    xs.forEach(function (x) { if (x >= 1 && x <= nb) c[x - 1]++; });
    return c;
  }
  // P(X ≤ n) dla liczby kontroli do r-tej wady = pnbinom(n - r, r, p)
  function pnbinom(k, r, p) {
    var s = 0, t = Math.pow(p, r);  // dnbinom(0)
    for (var j = 0; j <= k; j++) { s += t; t *= (j + r) / (j + 1) * (1 - p); }
    return Math.min(1, s);
  }
  // Pierwszy indeks (od 1), w którym skumulowana liczba wad osiąga r; null, gdy brak.
  function nth(outcomes, r) {
    var c = 0;
    for (var i = 0; i < outcomes.length; i++) { if (outcomes[i]) c++; if (c >= r) return i + 1; }
    return null;
  }

  // --- Wspólne elementy (stop_proto_common.R) --------------------------------

  function svgWrap(w, h, aria, body) {
    return '<svg class="d5p-svg" style="max-width:' + w + 'px" viewBox="0 0 ' + w + " " + h + '" role="img" aria-label="' + esc(aria) + '">' + body + "</svg>";
  }
  function head(name, step) {
    return '<div class="d5p-head"><span class="d5p-name">' + esc(name) + '</span><span class="d5p-step">' + esc(step) + "</span></div>";
  }
  // d5p_tape(outcomes, x0, y, pitch, size, n_cells, dim, max_cells)
  function tape(outcomes, x0, y, pitch, size, nCells, dim, maxCells) {
    if (nCells == null) nCells = outcomes.length;
    var nDraw = Math.min(nCells, maxCells == null ? Infinity : maxCells), out = "";
    for (var i = 1; i <= nDraw; i++) {
      var cls = i > outcomes.length ? "d5p-cell-idle" : outcomes[i - 1] ? "d5p-cell-bad" : "d5p-cell-ok";
      if (dim && i <= dim.length && dim[i - 1]) cls += " d5p-cell-dim";
      out += '<rect class="' + cls + '" x="' + g(x0 + (i - 1) * pitch) + '" y="' + g(y) + '" width="' + g(size) + '" height="' + g(size) + '" rx="2"/>';
    }
    if (nCells > nDraw) out += '<text class="d5p-mono d5p-subtle" x="' + g(x0 + nDraw * pitch + 2) + '" y="' + g(y + size - 1) + '" font-size="' + g(size + 3) + '">→</text>';
    return out;
  }
  // d5p_axis(x_of, y, ticks, label, x_label)
  function axis(xOf, y, ticks, label) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="d5p-axis" x1="' + g(xOf(lo)) + '" x2="' + g(xOf(hi)) + '" y1="' + g(y) + '" y2="' + g(y) + '"/>' +
      ticks.map(function (t) {
        return '<line class="d5p-axis" x1="' + g(xOf(t)) + '" x2="' + g(xOf(t)) + '" y1="' + g(y) + '" y2="' + g(y + 5) + '"/><text class="d5p-tick" x="' +
          g(xOf(t)) + '" y="' + g(y + 18) + '">' + t + "</text>";
      }).join("") +
      (label ? '<text class="d5p-subtle" x="' + g(xOf(hi)) + '" y="' + g(y + 34) + '" font-size="12" text-anchor="end">' + esc(label) + "</text>" : "");
  }
  // d5p_series(r, p): kontrole aż do r-tej wady
  function series(r, p, rnd) {
    var x = [], k = 0;
    while (k < r) { var d = rnd() < p; x.push(d); if (d) k++; }
    return x;
  }

  // --- Seria za serią (stop_proto_geo.R) ----------------------------------------

  var KMAX = 80;
  function geoX(k) { return 40 + (k - 0.5) * 10.5; }

  function geoCutText(xs, cut) {
    if (!xs.length) return "X > " + cut + ": —";
    var k = xs.filter(function (x) { return x > cut; }).length;
    return "X > " + cut + ": " + k + " z " + xs.length + " serii (" + (100 * k / xs.length).toFixed(1) + "%)";
  }
  function geoReadout(xs) {
    if (!xs.length) return "Serii: 0 · najczęściej: — · średnia: — · najdłuższa: —";
    var t = tabulate(xs, Math.max.apply(null, xs)), mode = t.indexOf(Math.max.apply(null, t)) + 1;
    return "Serii: " + xs.length + " · najczęściej: " + mode + " · średnia: " + mean(xs).toFixed(1) + " · najdłuższa: " + Math.max.apply(null, xs);
  }
  function geoSvg(xs, last, cut) {
    var base = 290, counts = tabulate(xs, KMAX), cmax = Math.max.apply(null, counts), scaled = cmax > 20;
    var tp = last.length ? '<text class="d5p-mono d5p-subtle" x="40" y="12" font-size="12">ostatnia seria</text>' +
      tape(last, 40, 18, 11, 9, null, null, KMAX) : "";
    var empty = xs.length ? "" : '<text class="d5p-subtle" x="480" y="170" font-size="16" text-anchor="middle">Paleta czeka na pierwszą serię</text>';
    var marks = "";
    counts.forEach(function (n, i) {
      if (!n) return;
      var k = i + 1, cx = geoX(k), cls = k > cut ? "d5p-bad" : "d5p-ok";
      if (scaled) {
        var h = n / cmax * 220;
        marks += '<rect class="' + cls + '" x="' + g(cx - 4) + '" y="' + g(base - h) + '" width="8" height="' + g(h) + '" rx="1.5"/><text class="d5p-mono d5p-subtle" x="' +
          g(cx) + '" y="' + g(base - h - 5) + '" text-anchor="middle" font-size="11">' + n + "</text>";
      } else {
        for (var j = 0; j < n; j++) marks += '<circle class="' + cls + '" cx="' + g(cx) + '" cy="' + g(base - 5 - j * 10) + '" r="4.5"/>';
      }
    });
    var xEnd = geoX(KMAX) + 6;
    var baseLine = '<line class="d5p-axis" x1="' + g(geoX(1) - 5) + '" x2="' + g(xEnd) + '" y1="' + base + '" y2="' + base + '"/>';
    var ax = axis(geoX, base, [1, 10, 20, 30, 40, 50, 60, 70, 80]);
    var axLabel = '<text class="d5p-subtle" x="' + g(geoX(1) - 5) + '" y="' + (base + 40) + '" font-size="12">X: kontrola, przy której wypadła wada</text>';
    var etc = [12, 24, 36].map(function (d) { return '<circle cx="' + g(xEnd + d) + '" cy="' + base + '" r="3" style="fill:var(--upwr-ink-subtle)"/>'; }).join("");
    var xc = 40 + cut * 10.5;
    var cutLine = '<line class="d5p-limit" x1="' + g(xc) + '" x2="' + g(xc) + '" y1="' + (base - 236) + '" y2="' + (base + 4) + '"/><text class="d5p-mono" x="' + g(xc) + '" y="' +
      (base - 242) + '" text-anchor="middle" font-size="13">n = ' + cut + "</text>";
    return svgWrap(960, 340, "Stos kontroli do pierwszej wady: " + xs.length + " serii na osi od 1 do 80, linia cięcia po " + cut + ". kontroli.",
      tp + empty + baseLine + ax + axLabel + etc + marks + cutLine);
  }

  // --- Dwadzieścia oddziałów (stop_proto_oddzialy.R) ------------------------------

  var BUDGET = 30, ROWS = 20;
  function branchesSvg(xs) {
    var W = 780, x0 = 46, p = 5.5, base = 290, n = xs.length;
    function xOf(t) { return x0 + t * p; }
    var aria = "Oddziały: " + n + " audytów, pionowa kreska budżetu na " + BUDGET + " kontrolach.";
    if (n <= ROWS) {
      var cols = 5, tw = 144, th = 84, gap = 12, gx = (W - cols * tw - (cols - 1) * gap) / 2, tiles = "";
      for (var i = 1; i <= ROWS; i++) {
        var cx = gx + ((i - 1) % cols) * (tw + gap), cy = 8 + Math.floor((i - 1) / cols) * (th + gap);
        var name = '<text class="d5p-mono" x="' + g(cx + 10) + '" y="' + g(cy + 17) + '" font-size="11" style="fill:' +
          (i <= n && xs[i - 1] > BUDGET ? "var(--upwr-surface)" : "var(--upwr-ink-subtle)") + '">Oddział ' + i + "</text>";
        if (i > n) { tiles += '<rect class="d5p-cell-idle" x="' + g(cx) + '" y="' + g(cy) + '" width="' + tw + '" height="' + th + '" rx="6"/>' + name; continue; }
        var X = xs[i - 1], ok = X <= BUDGET, ink = ok ? "var(--upwr-ink)" : "var(--upwr-surface)";
        var bx = cx + 10, bw = tw - 20, by = cy + th - 14, bmax = 90, fillW = Math.min(X, bmax) / bmax * bw;
        tiles += '<rect class="' + (ok ? "d5p-cell-ok" : "d5p-cell-bad") + '" x="' + g(cx) + '" y="' + g(cy) + '" width="' + tw + '" height="' + th + '" rx="6"/>' + name +
          '<text class="d5p-mono" x="' + g(cx + 10) + '" y="' + g(cy + 52) + '" font-size="28" font-weight="700" style="fill:' + ink + '">' + X + "</text>" +
          '<text x="' + g(cx + tw - 12) + '" y="' + g(cy + 50) + '" font-size="24" font-weight="700" text-anchor="end" style="fill:' + ink + '">' + (ok ? "✓" : "×") + "</text>" +
          '<rect x="' + g(bx) + '" y="' + g(by) + '" width="' + g(bw) + '" height="5" rx="2" style="fill:' + ink + ';opacity:.25"/>' +
          '<rect x="' + g(bx) + '" y="' + g(by) + '" width="' + g(fillW) + '" height="5" rx="2" style="fill:' + ink + '"/>' +
          '<line x1="' + g(bx + BUDGET / bmax * bw) + '" x2="' + g(bx + BUDGET / bmax * bw) + '" y1="' + g(by - 3) + '" y2="' + g(by + 8) + '" style="stroke:' + ink + '" stroke-width="1.5"/>' +
          (X > bmax ? '<text x="' + g(bx + bw + 1) + '" y="' + g(by + 6) + '" font-size="11" style="fill:' + ink + '">→</text>' : "");
      }
      var legend = '<text class="d5p-subtle" x="' + g(gx) + '" y="' + g(8 + 4 * (th + gap) + 8) + '" font-size="12">pasek: długość audytu (do 90 kontroli), kreska: budżet ' + BUDGET + "</text>";
      return svgWrap(W, 8 + 4 * (th + gap) + 16, aria, tiles + legend);
    }
    var cnt = tabulate(xs.map(function (x) { return Math.min(Math.ceil(x / 3), 40); }), 40), cmax = Math.max.apply(null, cnt);
    var w = 3 * p, bars = "";
    cnt.forEach(function (c, i) {
      if (!c) return;
      var j = i + 1, h = c / cmax * 200, cls = 3 * j <= BUDGET ? "d5p-ok" : "d5p-bad";
      bars += '<rect class="' + cls + '" x="' + g(xOf(3 * (j - 1)) + 1) + '" y="' + g(base - h) + '" width="' + g(w - 2) + '" height="' + g(h) + '" rx="1.5"/>' +
        (n <= 200 ? '<text class="d5p-mono d5p-subtle" x="' + g(xOf(3 * (j - 1)) + w / 2) + '" y="' + g(base - h - 4) + '" font-size="10" text-anchor="middle">' + c + "</text>" : "");
    });
    var bxl = xOf(BUDGET);
    return svgWrap(W, base + 50, aria, bars + '<line class="d5p-limit" x1="' + g(bxl) + '" x2="' + g(bxl) + '" y1="30" y2="' + base + '"/>' +
      '<text class="d5p-subtle" x="' + g(bxl + 6) + '" y="24" font-size="12">budżet ' + BUDGET + "</text>" +
      axis(xOf, base, [0, 30, 60, 90, 120], "kontrola nr (koniec audytu)"));
  }
  function branchesRead(xs, model) {
    var n = xs.length, k = xs.filter(function (x) { return x > BUDGET; }).length, t = "Zabrakło: " + k + " z " + n;
    if (n > 0) t += " (" + rround(100 * k / n) + "%)";
    if (n >= 2000) t += " · model: " + model;
    return '<p class="d5p-read">' + t + "</p>";
  }
  function branchesCaption(xs, caps) {
    var n = xs.length, k = xs.filter(function (x) { return x > BUDGET; }).length;
    var t = n === 0 ? caps[0] : n < ROWS ? caps[1] : n === ROWS ? caps[2].replace("{k}", k) : n < 2000 ? caps[3] : caps[4];
    return '<p class="d5p-caption">' + esc(t) + "</p>";
  }

  // --- Taśma przecięta na n (stop_proto_oddzialy.R) -------------------------------

  var COL_X = "var(--upwr-accent)", COL_Y = "var(--upwr-cat-niebo)";
  function cutSvg(outcomes, n) {
    var x0 = 30, p = 9, s = 7, y = 78, len = outcomes.length;
    function xOf(t) { return x0 + t * p; }
    var X = nth(outcomes, 3), Y = sum(outcomes.slice(0, n));
    var dim = outcomes.map(function (o, i) { return i + 1 > n; });
    function bracket(i1, i2, yb, dir, col) {
      return '<path d="M ' + g(xOf(i1 - 1)) + " " + g(yb + 5 * dir) + " V " + g(yb) + " H " + g(xOf(i2) - 2) + " V " + g(yb + 5 * dir) +
        '" fill="none" style="stroke:' + col + '" stroke-width="1.8"/>';
    }
    var xend = X == null ? len : X;
    var xpart = bracket(1, xend, y - 10, 1, COL_X) +
      (X != null ? '<rect x="' + g(x0 + (X - 1) * p - 1.5) + '" y="' + g(y - 1.5) + '" width="' + g(s + 3) + '" height="' + g(s + 3) + '" rx="2" style="fill:none;stroke:' + COL_X + ';stroke-width:2.2"/>' : "") +
      '<text class="d5p-mono" x="' + g(xOf(0)) + '" y="' + g(y - 18) + '" font-size="13" font-weight="700" style="fill:' + COL_X + '">' +
      (X == null ? "X > 80: trzecia wada poza taśmą" : "X = " + X + ": kontrola z trzecią wadą") + "</text>";
    var dots = "";
    outcomes.slice(0, n).forEach(function (o, i) {
      if (o) dots += '<circle cx="' + g(x0 + i * p + s / 2) + '" cy="' + g(y + s + 4) + '" r="2.6" style="fill:' + COL_Y + '"/>';
    });
    var ypart = bracket(1, n, y + s + 10, -1, COL_Y) + dots +
      '<text class="d5p-mono" x="' + g(xOf(0)) + '" y="' + g(y + s + 28) + '" font-size="13" font-weight="700" style="fill:' + COL_Y + '">Y' + sub(n) + " = " + Y +
      ": wady w pierwszych " + n + " kontrolach</text>";
    var cx = xOf(n) - 1;
    var cut = '<line class="d5p-limit" x1="' + g(cx) + '" x2="' + g(cx) + '" y1="' + g(y - 6) + '" y2="' + g(y + s + 14) + '"/>' +
      '<text class="d5p-mono" x="' + g(cx + 4) + '" y="' + g(y - 38) + '" font-size="12" text-anchor="start">n = ' + n + "</text>";
    return svgWrap(780, 178, "Taśma " + len + " kontroli: klamra X do trzeciej wady nad taśmą, klamra pierwszych " + n + " kontroli pod taśmą.",
      tape(outcomes, x0, y, p, s, null, dim) + xpart + ypart + cut + axis(xOf, y + s + 42, [0, 20, 40, 60, 80], "numer kontroli"));
  }
  function cutRead(outcomes, n) {
    var X = nth(outcomes, 3), Y = sum(outcomes.slice(0, n)), okx = X != null && X <= n;
    var prob = pnbinom(n - 3, 3, 0.1);
    // Wzór (5.7) w zwykłym HTML (klasa d5p-math): wersja offline nie ma MathJax.
    function sx(t) { return '<span style="color:' + COL_X + '">' + t + "</span>"; }
    function sy(t) { return '<span style="color:' + COL_Y + '">' + t + "</span>"; }
    function col(color, rule, val) {
      return '<div class="d5p-cut-col" style="--c:' + color + '"><div class="d5p-cut-rule">' + esc(rule) + '</div><div class="d5p-cut-val">' + esc(val) + "</div></div>";
    }
    return '<div class="d5p-cut-two">' +
      col(COL_X, "Reguła „do trzeciej wady”: liczymy kontrole", X == null ? "X > 80 > " + n : "X = " + X + " " + (okx ? "≤" : ">") + " " + n) +
      col(COL_Y, "Reguła „" + n + " prób”: liczymy wady", "Y" + sub(n) + " = " + Y + " " + (Y >= 3 ? "≥" : "<") + " 3") + "</div>" +
      '<div class="d5p-cut-eq"><span class="d5p-math">' + sx("<i>P</i>(<i>X</i> ≤ " + n + ")") + " = " + prob.toFixed(3) + " = " +
        sy("<i>P</i>(<i>Y</i><sub>" + n + "</sub> ≥ 3)") + "</span></div>" +
      '<p class="d5p-cut-verdict" style="color:' + (okx ? "var(--upwr-ink)" : "var(--upwr-accent)") + '">' +
      (okx ? "Ta seria: audyt mieści się w limicie " + n + " kontroli ✓" : "Ta seria: przy limicie " + n + " kontroli zabraknie ×") + "</p>";
  }

  // --- Sterowanie --------------------------------------------------------------

  function mulberry32(a) {
    return function () {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      var t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }
  function typeset(el) {
    var MJ = typeof window !== "undefined" ? window.MathJax : null;
    if (MJ && MJ.Hub && MJ.Hub.Queue) MJ.Hub.Queue(["Typeset", MJ.Hub, el]);
    else if (MJ && MJ.typesetPromise) MJ.typesetPromise([el]);
  }
  function sliderValue(root, id, fallback) {
    var el = root.querySelector("#" + id);
    var v = el ? Number(el.value) : fallback;
    var out = root.querySelector('output[data-value="' + id + '"]');
    if (out) out.textContent = String(v);
    return v;
  }

  var scenes = {
    kadry: function (cfg, ui) {
      var f = 0, n = cfg.frames.length;
      return {
        on: { prev: function () { f = Math.max(0, f - 1); }, next: function () { f = Math.min(n - 1, f + 1); } },
        render: function () {
          var F = cfg.frames[f];
          ui.set("head", head(F.name, "Kadr " + (f + 1) + " z " + n));
          ui.set("stage", F.svg);
          if (ui.set("body", F.body)) typeset(ui.slot("body"));
          ui.ends(f, n);
        }
      };
    },
    geo: function (cfg, ui, rnd) {
      var s;
      function reset(p) { s = { p: p, xs: [], last: [] }; }
      function add(m) {
        for (var i = 0; i < m; i++) { var t = series(1, s.p, rnd); s.xs.push(t.length); s.last = t; }
      }
      function pValue() { var r = ui.root.querySelector('input[name="d5p_geo_p"]:checked'); return r ? Number(r.value) : 0.1; }
      reset(pValue());
      return {
        on: { one: function () { add(1); }, ten: function () { add(10); }, hund: function () { add(100); },
              reset: function () { reset(s.p); }, p: function () { reset(pValue()); } },
        render: function () {
          var cut = sliderValue(ui.root, "d5p_geo_cut", 10), n = s.xs.length;
          ui.set("head", head("Audyt palet", "p = " + s.p.toFixed(2)));
          ui.set("stage", geoSvg(s.xs, s.last, cut));
          ui.set("read", '<div class="d5p-read">' + esc(geoReadout(s.xs)) + "</div>");
          ui.set("caption", '<p class="d5p-caption">' + esc(n === 0 ? cfg.captions[0] : n < 10 ? cfg.captions[1] : n < 100 ? cfg.captions[2] : cfg.captions[3]) + "</p>");
          ui.set("cut", '<span class="d5p-mono" style="font-size:1.5em; font-weight:700; color:var(--upwr-accent);">' + esc(geoCutText(s.xs, cut)) + "</span>");
        }
      };
    },
    oddzialy: function (cfg, ui, rnd) {
      var xs = [];
      function nb3() { var x = 0, k = 0; while (k < 3) { x++; if (rnd() < 0.1) k++; } return x; }
      function add(m) { while (m-- > 0 && xs.length < ROWS) xs.push(cfg.seedX[xs.length]); }
      return {
        on: { one: function () { add(1); }, all: function () { add(ROWS); },
              sez: function () { for (var i = 0; i < 2000; i++) xs.push(nb3()); }, reset: function () { xs = []; } },
        render: function () {
          ui.set("head", head("Budżet: " + BUDGET + " kontroli na oddział", "Audytów: " + xs.length));
          ui.set("stage", branchesSvg(xs));
          ui.set("read", branchesRead(xs, cfg.model));
          ui.set("caption", branchesCaption(xs, cfg.captions));
        }
      };
    },
    ciecie: function (cfg, ui, rnd) {
      var t = cfg.start.slice();
      return {
        on: { shuffle: function () { t = range(1, cfg.len).map(function () { return rnd() < 0.1; }); } },
        render: function () {
          var n = sliderValue(ui.root, "d5p_cut_n", 30);
          ui.set("head", head("Taśma kontroli", "Cięcie po kontroli " + n));
          ui.set("stage", cutSvg(t, n));
          if (ui.set("read", cutRead(t, n))) typeset(ui.slot("read"));
        }
      };
    }
  };

  function init(root) {
    if (root.dataset.ready) return;
    var make = scenes[root.getAttribute("data-scene")];
    if (!make) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.getAttribute("data-config") || "{}");
    var rnd = cfg.seed ? mulberry32(cfg.seed) : Math.random;
    var ui = {
      root: root,
      slot: function (name) { return root.querySelector('[data-d5p-out="' + name + '"]'); },
      // Zwraca true, gdy treść się zmieniła (np. trzeba złożyć wzory).
      set: function (name, html) {
        var el = ui.slot(name);
        if (!el || el.innerHTML === html) return false;
        el.innerHTML = html;
        return true;
      },
      ends: function (f, n) {
        var prev = root.querySelector('[data-d5p="prev"]'), next = root.querySelector('[data-d5p="next"]');
        if (prev) prev.disabled = f === 0;
        if (next) next.disabled = f === n - 1;
      }
    };
    var scene = make(cfg, ui, rnd);
    root.addEventListener("click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-d5p]");
      if (!btn || !root.contains(btn)) return;
      var fn = scene.on[btn.getAttribute("data-d5p")];
      if (!fn) return;
      fn(btn);
      scene.render();
    });
    // Suwaki i przełącznik p: zmiana p zaczyna serie od nowa, suwak tylko przerysowuje.
    root.addEventListener("input", function (e) { if (e.target.type === "range") scene.render(); });
    root.addEventListener("change", function (e) {
      if (e.target.name === "d5p_geo_p" && scene.on.p) { scene.on.p(); scene.render(); }
    });
    scene.render();
  }

  (typeof window !== "undefined" ? window : globalThis).lcD5Scenes = {
    tape: tape, axis: axis, geoSvg: geoSvg, geoReadout: geoReadout, geoCutText: geoCutText,
    branchesSvg: branchesSvg, branchesRead: branchesRead, branchesCaption: branchesCaption,
    cutSvg: cutSvg, cutRead: cutRead, pnbinom: pnbinom, rround: rround, series: series,
    mulberry32: mulberry32, init: init
  };

  if (typeof document === "undefined") return;
  function scan() { document.querySelectorAll(".d5p-scene:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
