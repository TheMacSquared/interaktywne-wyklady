// Eksperyment → zmienna losowa → rozkład: animowane doświadczenie z licznikiem X.
// Kontener: .lc-exp[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind:
//   "dice"      n kostek, X = liczba szóstek (dwumianowy)
//   "geometric" kostka do pierwszej szóstki, X = liczba rzutów (geometryczny)
//   "poisson"   zdarzenia na osi czasu, X = liczba zdarzeń w oknie
//   "expo"      czas do pierwszego zdarzenia, X ciągły (wykładniczy)
//   "galton"    deska Galtona, X = liczba odbić w prawo (→ normalny)
//   "sumdice"   n kostek, X = suma oczek (CTG); n zmienia się przyciskami [data-exp-n]
// Numer kroku czyta z data-lc-step korzenia widgetu, przyciski z [data-exp].
// Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H0 = 386;
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  var PIPS = {
    1: [[1, 1]], 2: [[0, 0], [2, 2]], 3: [[0, 0], [1, 1], [2, 2]],
    4: [[0, 0], [2, 0], [0, 2], [2, 2]], 5: [[0, 0], [2, 0], [1, 1], [0, 2], [2, 2]],
    6: [[0, 0], [2, 0], [0, 1], [2, 1], [0, 2], [2, 2]]
  };

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }

  function choose(n, k) {
    var r = 1;
    for (var i = 1; i <= k; i++) r = r * (n - k + i) / i;
    return r;
  }
  function fmt(x, d) { return x.toFixed(d); }
  function randFace(faces) { return 1 + Math.floor(Math.random() * faces); }
  function normPdf(z) { return Math.exp(-z * z / 2) / Math.sqrt(2 * Math.PI); }

  function niceAxis(max, target) {
    var raw = Math.max(max, 1e-9) / target;
    var mag = Math.pow(10, Math.floor(Math.log10(raw)));
    var step = [1, 2, 5, 10].map(function (m) { return m * mag; })
      .filter(function (s) { return s >= raw; })[0];
    return { step: step, max: step * Math.ceil(max / step) };
  }

  // Kostka narysowana w (x, y) o boku d; v === null: pytajnik.
  function drawDie(g, x, y, d, v, on) {
    var q = svg("g", { transform: "translate(" + x + "," + y + ")" }, g);
    svg("rect", { width: d, height: d, rx: d * 0.17, class: "lc-exp-die" + (on ? " is-hit" : "") }, q);
    if (v === null) {
      svg("text", { x: d / 2, y: d / 2 + d * 0.15, "text-anchor": "middle", class: "lc-exp-q" }, q, "?");
    } else {
      PIPS[v].forEach(function (c) {
        svg("circle", { cx: d * 0.21 + c[0] * d * 0.29, cy: d * 0.21 + c[1] * d * 0.29, r: d * 0.08,
          class: "lc-exp-pip" + (on ? " is-hit" : "") }, q);
      });
    }
  }

  function plural(n) {
    var m10 = n % 10, m100 = n % 100;
    if (n === 1) return "zdarzenie";
    if (m10 >= 2 && m10 <= 4 && !(m100 >= 12 && m100 <= 14)) return "zdarzenia";
    return "zdarzeń";
  }

  // Oś czasu wspólna dla rozkładów Poissona i wykładniczego.
  function timeAxis(g, T, tickSuffix, ax0, ax1, ay) {
    svg("line", { x1: ax0, x2: ax1, y1: ay, y2: ay, class: "lc-exp-axis" }, g);
    for (var m = 0; m <= 4; m++) {
      var x = ax0 + (ax1 - ax0) * m / 4;
      svg("line", { x1: x, x2: x, y1: ay, y2: ay + 5, class: "lc-exp-axis" }, g);
      svg("text", { x: x, y: ay + 19, "text-anchor": "middle", class: "lc-exp-tick" }, g,
        String(Math.round(T * m / 4)) + tickSuffix);
    }
  }

  // Chmurka z kroplami (deszcz trwa) albo słoneczko (deszcz się skończył) w punkcie (x, y).
  function weatherIcon(g, x, y, sunny, phase) {
    var q = svg("g", { transform: "translate(" + x + "," + y + ") scale(1.3)" }, g);
    if (sunny) {
      svg("circle", { r: 10, class: "lc-exp-sun" }, q);
      for (var a = 0; a < 8; a++) {
        var t = a * Math.PI / 4;
        svg("line", { x1: Math.cos(t) * 15, y1: Math.sin(t) * 15, x2: Math.cos(t) * 21, y2: Math.sin(t) * 21,
          class: "lc-exp-sunray" }, q);
      }
      return;
    }
    svg("circle", { cx: -8, cy: 2, r: 8, class: "lc-exp-cloud" }, q);
    svg("circle", { cx: 2, cy: -5, r: 10, class: "lc-exp-cloud" }, q);
    svg("circle", { cx: 12, cy: 2, r: 8, class: "lc-exp-cloud" }, q);
    svg("rect", { x: -8, y: 2, width: 20, height: 8, class: "lc-exp-cloud" }, q);
    [-8, 2, 12].forEach(function (dx, i) {
      var off = ((phase || 0) * 60 + i * 5) % 12;
      svg("line", { x1: dx, y1: 14 + off, x2: dx - 2, y2: 19 + off, class: "lc-exp-drop" }, q);
    });
  }

  // --- rodzaje doświadczeń ---------------------------------------------------
  // Każdy zwraca: bins [{label, prob, tail?}], run(), binOf(x), draw(g, o, hi, prog),
  // animate(o, fast, redraw, done), log(o), sub (podpis pod X), unit (nazwa próby);
  // opcjonalnie: extra (dodatkowa wysokość sceny), overlay "curve" + curve(t),
  // edge(i) (etykiety na krawędziach przedziałów), fmtX, relLabel, noDrop.
  var KINDS = {};

  function shakeAnimate(randomOutcome) {
    return function (o, fast, redraw, done) {
      if (REDUCE) { done(); return; }
      var frames = fast ? 3 : 9, i = 0;
      var t = setInterval(function () {
        redraw(randomOutcome(), false);
        if (++i >= frames) { clearInterval(t); done(); }
      }, 55);
    };
  }

  KINDS.dice = function (cfg) {
    var n = cfg.n || 6, faces = cfg.faces || 6, hit = cfg.hit || 6, p = 1 / faces;
    var D = 52, GAP = 16, X0 = (W - (n * D + (n - 1) * GAP)) / 2, Y0 = 8;
    var bins = [];
    for (var k = 0; k <= n; k++) {
      bins.push({ label: String(k), prob: choose(n, k) * Math.pow(p, k) * Math.pow(1 - p, n - k) });
    }
    function count(f) { return f.filter(function (v) { return v === hit; }).length; }
    function randomFaces() {
      var f = [];
      for (var i = 0; i < n; i++) f.push(randFace(faces));
      return f;
    }
    return {
      bins: bins, sub: "liczba szóstek w tym rzucie", unit: "rzut",
      run: function () { var f = randomFaces(); return { faces: f, x: count(f) }; },
      binOf: function (x) { return x; },
      draw: function (g, o, hi) {
        for (var i = 0; i < n; i++) {
          var v = o ? o.faces[i] : null;
          drawDie(g, X0 + i * (D + GAP), Y0, D, v, hi && v === hit);
        }
      },
      animate: shakeAnimate(function () { return { faces: randomFaces() }; }),
      log: function (o) {
        return { cells: o.faces.map(function (v) { return { t: String(v), hit: v === hit }; }), x: o.x };
      }
    };
  };

  KINDS.sumdice = function (cfg) {
    var n = cfg.n || 1, faces = cfg.faces || 6;
    var D = Math.min(52, Math.floor((W - 100) / (n + (n - 1) * 0.3))), GAP = D * 0.3;
    var X0 = (W - (n * D + (n - 1) * GAP)) / 2, Y0 = 8 + (52 - D) / 2;
    var dist = [1];
    for (var i = 0; i < n; i++) {
      var next = new Array(dist.length + faces - 1).fill(0);
      dist.forEach(function (pr, j) { for (var f = 0; f < faces; f++) next[j + f] += pr / faces; });
      dist = next;
    }
    var bins = dist.map(function (pr, j) { return { label: String(n + j), prob: pr }; });
    var mu = n * (faces + 1) / 2, sd = Math.sqrt(n * (faces * faces - 1) / 12);
    function randomFaces() {
      var f = [];
      for (var j = 0; j < n; j++) f.push(randFace(faces));
      return f;
    }
    function sum(f) { return f.reduce(function (a, b) { return a + b; }, 0); }
    return {
      bins: bins, sub: "suma oczek w tym rzucie", unit: "rzut",
      overlay: "curve", curveLabel: "krzywa normalna",
      curve: function (t) { return normPdf((t + n - 0.5 - mu) / sd) / sd; },
      run: function () { var f = randomFaces(); return { faces: f, x: sum(f) }; },
      binOf: function (x) { return x - n; },
      draw: function (g, o) {
        for (var j = 0; j < n; j++) {
          drawDie(g, X0 + j * (D + GAP), Y0, D, o ? o.faces[j] : null, false);
        }
      },
      animate: shakeAnimate(function () { return { faces: randomFaces() }; }),
      log: function (o) {
        return { cells: o.faces.map(function (v) { return { t: String(v), hit: false }; }), x: o.x };
      }
    };
  };

  KINDS.geometric = function (cfg) {
    var faces = cfg.faces || 6, hit = cfg.hit || 6, p = 1 / faces;
    var KMAX = cfg.kmax || 12, SHOW = 12, D = 36, GAP = 8, Y0 = 14;
    var bins = [];
    for (var k = 1; k <= KMAX; k++) bins.push({ label: String(k), prob: Math.pow(1 - p, k - 1) * p });
    bins.push({ label: "…", prob: 0, tail: true });
    return {
      bins: bins, sub: "liczba rzutów do pierwszej szóstki", unit: "seria",
      run: function () {
        var f = [];
        do { f.push(randFace(faces)); } while (f[f.length - 1] !== hit);
        return { faces: f, x: f.length };
      },
      binOf: function (x) { return Math.min(x, KMAX + 1) - 1; },
      draw: function (g, o, hi, prog) {
        var f = o ? o.faces : [];
        var shown = prog === undefined ? f.length : prog;
        var from = Math.max(0, shown - SHOW);
        var items = f.slice(from, shown);
        var total = SHOW * D + (SHOW - 1) * GAP, x0 = (W - total) / 2;
        if (!o) {
          for (var i = 0; i < SHOW; i++) drawDie(g, x0 + i * (D + GAP), Y0, D, null, false);
          return;
        }
        if (from > 0) {
          svg("text", { x: x0 - 4, y: Y0 + D / 2 + 6, "text-anchor": "end", class: "lc-exp-read is-plain" }, g, "…");
        }
        items.forEach(function (v, i) {
          drawDie(g, x0 + i * (D + GAP), Y0, D, v, hi && v === hit);
        });
        for (var j = items.length; j < SHOW; j++) {
          svg("rect", { x: x0 + j * (D + GAP), y: Y0, width: D, height: D, rx: D * 0.17, class: "lc-exp-slot" }, g);
        }
      },
      animate: function (o, fast, redraw, done) {
        if (REDUCE) { done(); return; }
        var i = 0, gap = fast ? 35 : 190;
        var t = setInterval(function () {
          i++;
          redraw(Object.assign({}, o, { x: "?" }), false, i);
          if (i >= o.faces.length) { clearInterval(t); done(); }
        }, gap);
      },
      log: function (o) {
        var f = o.faces, long = f.length > 10;
        var cells = (long ? f.slice(-10) : f).map(function (v) { return { t: String(v), hit: v === hit }; });
        if (long) cells.unshift({ t: "…", hit: false });
        return { cells: cells, x: o.x };
      }
    };
  };

  KINDS.poisson = function (cfg) {
    var lam = cfg.lambda || 4, T = cfg.window || 60, KMAX = cfg.kmax || 11;
    var AX0 = 90, AX1 = 550, AY = 52;
    var bins = [], pk = Math.exp(-lam);
    for (var k = 0; k < KMAX; k++) {
      bins.push({ label: String(k), prob: pk });
      pk = pk * lam / (k + 1);
    }
    bins.push({ label: "…", prob: 0, tail: true });
    function px(t) { return AX0 + (AX1 - AX0) * t / T; }
    return {
      bins: bins, sub: cfg.sub || "liczba zdarzeń w oknie", unit: cfg.unit || "okno",
      run: function () {
        var pts = [], t = 0;
        for (;;) {
          t += -Math.log(1 - Math.random()) * T / lam;
          if (t > T) break;
          pts.push(t);
        }
        return { pts: pts, x: pts.length };
      },
      binOf: function (x) { return Math.min(x, KMAX); },
      draw: function (g, o, hi, prog) {
        timeAxis(g, T, cfg.tickSuffix === undefined ? " min" : cfg.tickSuffix, AX0, AX1, AY);
        if (!o) return;
        var shown = prog === undefined ? o.pts.length : prog, last = -99, lvl = 0;
        o.pts.slice(0, shown).forEach(function (t) {
          lvl = px(t) - last < 14 ? lvl + 1 : 0;
          last = px(t);
          svg("circle", { cx: px(t), cy: AY - 12 - (lvl % 3) * 13, r: 5.5,
            class: "lc-exp-event" + (hi ? " is-hit" : "") }, g);
        });
      },
      animate: function (o, fast, redraw, done) {
        if (REDUCE || !o.pts.length) { done(); return; }
        var i = 0, gap = fast ? 40 : 170;
        redraw(Object.assign({}, o, { x: "?" }), false, 0);
        var t = setInterval(function () {
          i++;
          redraw(Object.assign({}, o, { x: "?" }), false, i);
          if (i >= o.pts.length) { clearInterval(t); done(); }
        }, gap);
      },
      log: function (o) {
        return { cells: [{ t: o.x + " " + plural(o.x), hit: false }], x: o.x };
      }
    };
  };

  KINDS.expo = function (cfg) {
    var lam = cfg.lambda || 4, T = cfg.window || 60, w = cfg.width || 5, K = Math.round(T / w);
    var AX0 = 90, AX1 = 550, AY = 52, rate = lam / T;
    var bins = [];
    for (var i = 0; i < K; i++) {
      bins.push({ label: String(i * w), prob: Math.exp(-rate * i * w) - Math.exp(-rate * (i + 1) * w) });
    }
    bins.push({ label: "…", prob: 0, tail: true });
    function px(t) { return AX0 + (AX1 - AX0) * t / T; }
    function f1(x) { return typeof x === "number" ? x.toFixed(1) : String(x); }
    return {
      bins: bins, sub: cfg.sub || "czas oczekiwania w tej obserwacji", unit: cfg.unit || "obserwacja",
      overlay: "curve", curveLabel: "gęstość modelu",
      relLabel: "częstość w przedziale",
      edge: function (i) { return String(i * w); },
      curve: function (t) { return rate * Math.exp(-rate * t * w) * w; },
      fmtX: function (x) { return typeof x === "number" ? f1(x) + " min" : x; },
      run: function () { return { x: -Math.log(1 - Math.random()) / rate }; },
      binOf: function (x) { return Math.min(Math.floor(x / w), K); },
      draw: function (g, o, hi, prog) {
        timeAxis(g, T, " min", AX0, AX1, AY);
        if (!o) { weatherIcon(g, 46, 28, false, 0); return; }
        var frac = prog === undefined ? 1 : prog;
        weatherIcon(g, 46, 28, frac >= 1, frac);
        var x = o.xTrue !== undefined ? o.xTrue : o.x;
        var shown = Math.min(x, T) * frac;
        svg("line", { x1: px(0), x2: px(shown), y1: AY, y2: AY, class: "lc-exp-trail" }, g);
        svg("circle", { cx: px(shown), cy: AY - 12, r: 6, class: "lc-exp-event" + (hi ? " is-hit" : "") }, g);
        if (x > T && frac >= 1) svg("text", { x: AX1 + 22, y: AY + 4, class: "lc-exp-read is-plain" }, g, "→");
      },
      animate: function (o, fast, redraw, done) {
        if (REDUCE) { done(); return; }
        var frames = fast ? 8 : 24, i = 0;
        var t = setInterval(function () {
          i++;
          redraw({ x: "?", xTrue: o.x }, false, i / frames);
          if (i >= frames) { clearInterval(t); done(); }
        }, fast ? 25 : 40);
      },
      log: function (o) { return { cells: [], x: f1(o.x) + " min" }; }
    };
  };

  KINDS.galton = function (cfg) {
    var R = cfg.rows || 10, CX = W / 2, DX = 28, Y0 = 16, DY = 11;
    var bins = [];
    for (var k = 0; k <= R; k++) bins.push({ label: String(k), prob: choose(R, k) * Math.pow(0.5, R) });
    var mu = R / 2, sd = Math.sqrt(R) / 2;
    function pos(path, r) {
      var rights = 0;
      for (var i = 0; i < r; i++) rights += path[i];
      return { x: CX + (2 * rights - r) * DX / 2, y: Y0 + r * DY };
    }
    function point(path, f) {
      var r = Math.min(R, Math.floor(f)), a = pos(path, r), b = pos(path, Math.min(R, r + 1)), u = f - r;
      return r >= R ? a : { x: a.x + (b.x - a.x) * u, y: a.y + (b.y - a.y) * u };
    }
    return {
      bins: bins, sub: "liczba odbić w prawo", unit: "kulka", extra: 70, noDrop: true,
      overlay: "curve", curveLabel: "krzywa normalna",
      curve: function (t) { return normPdf((t - 0.5 - mu) / sd) / sd; },
      run: function () {
        var path = [], x = 0;
        for (var i = 0; i < R; i++) { var d = Math.random() < 0.5 ? 0 : 1; path.push(d); x += d; }
        return { path: path, x: x };
      },
      binOf: function (x) { return x; },
      draw: function (g, o, hi, prog) {
        for (var r = 0; r < R; r++) {
          for (var j = 0; j <= r; j++) {
            svg("circle", { cx: CX + (j - r / 2) * DX, cy: Y0 + r * DY + 2, r: 2.4, class: "lc-exp-peg" }, g);
          }
        }
        if (!o) return;
        var path = o.path || o.pathTrue, f = prog === undefined ? R : prog * R, pts = [pos(path, 0)];
        for (var r2 = 1; r2 <= Math.floor(f); r2++) pts.push(pos(path, r2));
        var head = point(path, f);
        if (f > Math.floor(f)) pts.push(head);
        svg("polyline", { points: pts.map(function (q) { return q.x + "," + q.y; }).join(" "),
          class: "lc-exp-trail", fill: "none" }, g);
        svg("circle", { cx: head.x, cy: head.y, r: 5.5, class: "lc-exp-ball" }, g);
      },
      animate: function (o, fast, redraw, done) {
        if (REDUCE) { done(); return; }
        var frames = fast ? R * 2 : R * 5, i = 0;
        var t = setInterval(function () {
          i++;
          redraw({ x: "?", pathTrue: o.path }, false, i / frames);
          if (i >= frames) { clearInterval(t); done(); }
        }, fast ? 14 : 28);
      },
      log: function (o) {
        return { cells: o.path.map(function (d) { return { t: d ? "P" : "L", hit: !!d }; }), x: o.x };
      }
    };
  };

  // --- widget ----------------------------------------------------------------
  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    if (!stepper) return;
    root.dataset.ready = "1";

    var cfg = JSON.parse(root.dataset.config || "{}");
    var kind, bins, nb, EX, READ_Y, PT, PB, LOG_Y;
    var PL = 74, PR = 616;
    var state, busy = false;

    var s = svg("svg", { class: "lc-exp-svg", role: "img",
      "aria-label": cfg.aria || "Doświadczenie, zmienna losowa X i histogram powtórzeń" }, root);
    var gStage = svg("g", {}, s), gLow = svg("g", {}, s), gBall = svg("g", {}, s);

    function build() {
      kind = KINDS[cfg.kind || "dice"](cfg);
      bins = kind.bins; nb = bins.length;
      EX = kind.extra || 0;
      READ_Y = 76 + EX; PT = 142 + EX; PB = 320 + EX; LOG_Y = 150 + EX;
      s.setAttribute("viewBox", "0 0 " + W + " " + (H0 + EX));
      state = { counts: new Array(nb).fill(0), total: 0, rolls: [], last: null };
    }
    build();

    function step() { return Number(stepper.getAttribute("data-lc-step")) || 1; }
    function fmtX(x) { return kind.fmtX ? kind.fmtX(x) : String(x); }

    // --- scena -----------------------------------------------------------------
    function drawStage(o, prog) {
      gStage.textContent = "";
      kind.draw(gStage, o, step() >= 2, prog);
      if (!o) return;
      var label = svg("text", { x: W / 2, y: READ_Y + 14, "text-anchor": "middle", class: "lc-exp-read" }, gStage);
      if (step() >= 2) {
        label.textContent = "X = " + fmtX(o.x);
        svg("text", { x: W / 2, y: READ_Y + 32, "text-anchor": "middle", class: "lc-exp-sub" }, gStage, kind.sub);
      } else {
        label.textContent = kind.unit + " nr " + (state.total + (o.x === "?" ? 1 : 0));
        label.setAttribute("class", "lc-exp-read is-plain");
      }
    }

    // --- dziennik (kroki 1-2) ---------------------------------------------------
    function drawLog() {
      var st = step(), y = LOG_Y;
      if (!state.rolls.length) {
        svg("text", { x: W / 2, y: y + 66, "text-anchor": "middle", class: "lc-exp-sub" }, gLow,
          "Wykonaj doświadczenie, żeby zobaczyć jego wynik.");
        return;
      }
      state.rolls.forEach(function (r, ri) {
        var g = svg("g", { opacity: 1 - ri * 0.13 }, gLow), row = kind.log(r.o), cx = 230;
        svg("text", { x: 110, y: y + ri * 28, class: "lc-exp-log" }, g, kind.unit + " " + r.no);
        row.cells.forEach(function (c) {
          var wide = c.t.length > 2;
          svg("text", { x: cx, y: y + ri * 28,
            class: "lc-exp-log" + (st >= 2 && c.hit ? " is-hit" : "") }, g, c.t);
          cx += wide ? 14 + c.t.length * 9 : 24;
        });
        if (st >= 2) {
          svg("text", { x: cx + 8, y: y + ri * 28, class: "lc-exp-log" }, g, "→");
          svg("text", { x: cx + 34, y: y + ri * 28, class: "lc-exp-log is-x" }, g, "X = " + row.x);
        }
      });
    }

    // --- histogram (kroki 3-4) ---------------------------------------------------
    function hist() {
      var rel = step() >= 4, tot = state.total, tailIdx = bins[nb - 1].tail ? nb - 1 : -1;
      var vals = state.counts.map(function (c, i) {
        return i === tailIdx ? 0 : (rel ? (tot ? c / tot : 0) : c);
      });
      var top = Math.max.apply(null, vals);
      if (rel) top = Math.max(top, Math.max.apply(null, bins.map(function (b) { return b.prob; })));
      var ax = rel ? niceAxis(Math.max(top * 1.08, 0.1), 4) : niceAxis(Math.max(top * 1.12, 5), 4);
      return { rel: rel, vals: vals, ax: ax, y: function (v) { return PB - (PB - PT) * v / ax.max; } };
    }
    function slotX(i) { return PL + (PR - PL) * (i + 0.5) / nb; }
    function edgeX(i) { return PL + (PR - PL) * i / nb; }

    function drawHist() {
      var h = hist(), bw = (PR - PL) / nb * (kind.edge ? 0.94 : 0.62), dense = nb > 14;
      var thin = nb > 14 ? Math.ceil(nb / 12) : 1, curve = kind.overlay === "curve";
      for (var t = 0; t <= h.ax.max + h.ax.step * 1e-6; t += h.ax.step) {
        svg("line", { x1: PL, x2: PR, y1: h.y(t), y2: h.y(t), class: "lc-exp-grid" }, gLow);
        svg("text", { x: PL - 8, y: h.y(t) + 4, "text-anchor": "end", class: "lc-exp-tick" }, gLow,
          h.rel ? fmt(t, 2) : String(Math.round(t)));
      }
      svg("line", { x1: PL, x2: PR, y1: PB, y2: PB, class: "lc-exp-axis" }, gLow);
      for (var i = 0; i < nb; i++) {
        var x = slotX(i), v = h.vals[i], isTail = !!bins[i].tail;
        if (v > 0 && !isTail) {
          svg("rect", { x: x - bw / 2, y: h.y(v), width: bw, height: PB - h.y(v), class: "lc-exp-bar" }, gLow);
          if (!h.rel && nb <= 9) svg("text", { x: x, y: h.y(v) - 5, "text-anchor": "middle", class: "lc-exp-val" }, gLow, String(v));
        }
        if (kind.edge) {
          svg("text", { x: edgeX(i), y: PB + 18, "text-anchor": "middle", class: "lc-exp-tick is-x" }, gLow, kind.edge(i));
          if (isTail) svg("text", { x: x, y: PB + 18, "text-anchor": "middle", class: "lc-exp-tick is-x" }, gLow, "…");
        } else if (i % thin === 0 || isTail) {
          svg("text", { x: x, y: PB + 18, "text-anchor": "middle", class: "lc-exp-tick is-x" }, gLow, bins[i].label);
        }
        if (h.rel && !curve && !isTail) {
          svg("circle", { cx: x, cy: h.y(bins[i].prob), r: 5.5, class: "lc-exp-theory" }, gLow);
          svg("text", { x: x, y: PB + 36, "text-anchor": "middle", class: "lc-exp-prob" }, gLow,
            fmt(bins[i].prob, dense ? 2 : 3));
        }
      }
      if (h.rel && curve) {
        var real = bins[nb - 1].tail ? nb - 1 : nb, pts = [];
        for (var q = 0; q <= 120; q++) {
          var tt = real * q / 120;
          pts.push((PL + (PR - PL) * tt / nb) + "," + h.y(kind.curve(tt)));
        }
        svg("polyline", { points: pts.join(" "), class: "lc-exp-curve", fill: "none" }, gLow);
      }
      svg("text", { x: (PL + PR) / 2, y: PB + (h.rel && !curve ? 54 : 40), "text-anchor": "middle", class: "lc-exp-axtitle" },
        gLow, cfg.xTitle || "X");
      svg("text", { x: 16, y: (PT + PB) / 2, "text-anchor": "middle", class: "lc-exp-axtitle",
        transform: "rotate(-90 16 " + (PT + PB) / 2 + ")" }, gLow,
        h.rel ? (kind.relLabel || "częstość względna") : "liczba powtórzeń");
      svg("text", { x: PR, y: PT - 10, "text-anchor": "end", class: "lc-exp-n" }, gLow, "n = " + state.total);
      if (h.rel) {
        if (curve) {
          svg("line", { x1: PL, x2: PL + 14, y1: PT - 14, y2: PT - 14, class: "lc-exp-curve" }, gLow);
          svg("text", { x: PL + 22, y: PT - 10, class: "lc-exp-n" }, gLow, kind.curveLabel || "model");
        } else {
          svg("circle", { cx: PL + 6, cy: PT - 14, r: 5.5, class: "lc-exp-theory" }, gLow);
          svg("text", { x: PL + 18, y: PT - 10, class: "lc-exp-n" }, gLow, "P(X = k) z modelu");
        }
      }
    }

    function render() {
      gLow.textContent = "";
      drawStage(state.last);
      if (step() >= 3) drawHist(); else drawLog();
      stepper.querySelectorAll("[data-exp]").forEach(function (b) { b.disabled = busy; });
    }

    // --- wykonanie doświadczenia -----------------------------------------------------
    function commit(o) {
      state.total += 1;
      state.last = o;
      state.rolls.unshift({ no: state.total, o: o });
      if (state.rolls.length > 6) state.rolls.pop();
    }

    function dropBall(i, ms, done) {
      if (REDUCE) { done(); return; }
      var h = hist();
      var x0 = W / 2, y0 = READ_Y + 40, x1 = slotX(i), y1 = h.y(h.vals[i]) - 10;
      var ball = svg("circle", { r: 9, cx: x0, cy: y0, class: "lc-exp-ball" }, gBall);
      var t0 = null;
      function frame(t) {
        if (t0 === null) t0 = t;
        var u = Math.min(1, (t - t0) / ms), e = u * u * (3 - 2 * u);
        ball.setAttribute("cx", x0 + (x1 - x0) * e);
        ball.setAttribute("cy", y0 + (y1 - y0) * e);
        if (u < 1) requestAnimationFrame(frame);
        else { gBall.removeChild(ball); done(); }
      }
      requestAnimationFrame(frame);
    }

    function runOnce(fast, done) {
      var o = kind.run(), i = kind.binOf(o.x);
      kind.animate(o, fast, function (tmp, hi, prog) {
        drawStage(tmp.x === undefined ? Object.assign({ x: "?" }, tmp) : tmp, prog);
      }, function () {
        commit(o);
        drawStage(o);
        var land = function () { state.counts[i] += 1; render(); done(); };
        if (step() >= 3 && !kind.noDrop) dropBall(i, fast ? 160 : 420, land); else land();
      });
    }

    function runMany(m) {
      if (m > 10) {
        var o;
        for (var i = 0; i < m; i++) {
          o = kind.run();
          state.counts[kind.binOf(o.x)] += 1;
        }
        commit(o);
        state.total += m - 1;
        state.rolls[0].no = state.total;
        render();
        return;
      }
      busy = true; render();
      var left = m;
      (function next() {
        if (left-- <= 0) { busy = false; render(); return; }
        runOnce(true, next);
      })();
    }

    stepper.addEventListener("click", function (e) {
      var np = e.target.closest("[data-exp-n]");
      if (np && stepper.contains(np)) {
        if (busy) return;
        cfg.n = Number(np.getAttribute("data-exp-n"));
        stepper.querySelectorAll("[data-exp-n]").forEach(function (b) {
          b.setAttribute("aria-pressed", b === np ? "true" : "false");
        });
        build();
        render();
        return;
      }
      var b = e.target.closest("[data-exp]");
      if (b && stepper.contains(b)) {
        if (busy) return;
        var a = b.getAttribute("data-exp");
        if (a === "roll") { busy = true; render(); runOnce(false, function () { busy = false; render(); }); }
        else runMany(Number(a.replace("roll", "")));
        return;
      }
      if (e.target.closest('[data-lc-nav="reset"]')) { build(); setTimeout(render, 0); }
    });

    new MutationObserver(function () { render(); })
      .observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
    render();
  }

  function scan() {
    document.querySelectorAll(".lc-exp:not([data-ready])").forEach(init);
  }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
