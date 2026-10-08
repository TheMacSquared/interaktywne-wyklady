// Sceny wykładu 02 (PROTOTYPY 2026-10-08): doświadczenie z życia → zmienna → rozkład / model.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper). Wzorzec init/KINDS
// jak w statystyce 00 (scenes.js), obok istniejącego experiment.js (.lc-exp), z którym nie koliduje.
// config.kind:
//   "group"   telefon do n losowych osób o czas dojazdu, X̄ grupki (rozkład średniej, CTG)
//   "bus"     pasażer przychodzi na przystanek, X = czas czekania (pole nad przedziałem, tolerancja)
//   "streak"  rzut po passie bez szóstki (kostka nie pamięta), kontrast: talia kart bez zwracania
//   "scratch" zdrapka z kiosku, X = wygrana (E(X) jako średnia na dłuższą metę, SD jako rozrzut)
// Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji)
// Rysuje SVG; serwer R podaje tylko konfigurację i teksty kroków.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640;
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
  function fmt(x, d) { return Number(x).toFixed(d); }
  function ease(u) { return u * u * (3 - 2 * u); }
  function niceAxis(max, target) {
    var raw = Math.max(max, 1e-9) / target;
    var mag = Math.pow(10, Math.floor(Math.log10(raw)));
    var step = [1, 2, 2.5, 5, 10].map(function (m) { return m * mag; })
      .filter(function (s) { return s >= raw; })[0];
    return { step: step, max: step * Math.ceil(max / step - 1e-9) };
  }
  function decs(step) { return step >= 1 ? 0 : step >= 0.1 ? 1 : step >= 0.01 ? 2 : 3; }

  // Animacja klatkowa: step(u) dla u z 0..1, potem done().
  function tween(ms, step, done) {
    if (REDUCE || ms <= 0) { step(1); done(); return; }
    var t0 = null;
    function frame(t) {
      if (t0 === null) t0 = t;
      var u = Math.min(1, (t - t0) / ms);
      step(u);
      if (u < 1) requestAnimationFrame(frame); else done();
    }
    requestAnimationFrame(frame);
  }

  // Żeton lecący ze sceny do histogramu.
  function fly(api, x0, y0, x1, y1, ms, done) {
    if (REDUCE) { done(); return; }
    var c = svg("circle", { r: 8, cx: x0, cy: y0, class: "lc-sc-token" }, api.fly);
    tween(ms, function (u) {
      var e = ease(u);
      c.setAttribute("cx", x0 + (x1 - x0) * e);
      c.setAttribute("cy", y0 + (y1 - y0) * e);
    }, function () { api.fly.removeChild(c); done(); });
  }

  function rnorm() {
    var u = 1 - Math.random(), v = Math.random();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }
  // Marsaglia–Tsang, shape >= 1
  function rgamma(shape) {
    var d = shape - 1 / 3, c = 1 / Math.sqrt(9 * d);
    for (;;) {
      var x = rnorm(), v = Math.pow(1 + c * x, 3);
      if (v <= 0) continue;
      if (Math.log(Math.random()) < 0.5 * x * x + d - d * v + d * Math.log(v)) return d * v;
    }
  }
  function lgamma(z) {
    var c = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
      -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
    if (z < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * z)) - lgamma(1 - z);
    z -= 1;
    var x = c[0];
    for (var i = 1; i < 9; i++) x += c[i] / (z + i);
    var t = z + 7.5;
    return 0.5 * Math.log(2 * Math.PI) + (z + 0.5) * Math.log(t) - t + Math.log(x);
  }
  function normPdf(x, m, s) { var z = (x - m) / s; return Math.exp(-z * z / 2) / (s * Math.sqrt(2 * Math.PI)); }

  // Postać: głowa w (cx, cy), skala s.
  function person(g, cx, cy, s, cls) {
    var q = svg("g", { class: "lc-sc-person" + (cls ? " " + cls : "") }, g);
    svg("circle", { cx: cx, cy: cy, r: 7 * s }, q);
    svg("rect", { x: cx - 8 * s, y: cy + 9 * s, width: 16 * s, height: 20 * s, rx: 6 * s }, q);
    return q;
  }
  function bubble(g, x, y, w, text) {
    svg("rect", { x: x, y: y, width: w, height: 24, rx: 10, class: "lc-sc-bubble" }, g);
    svg("text", { x: x + w / 2, y: y + 16, "text-anchor": "middle", class: "lc-sc-bubble-t" }, g, text);
  }
  function drawDie(g, x, y, d, v, on) {
    var q = svg("g", { transform: "translate(" + x + "," + y + ")" }, g);
    svg("rect", { width: d, height: d, rx: d * 0.17, class: "lc-sc-die" + (on ? " is-hit" : "") }, q);
    if (v === null) {
      svg("text", { x: d / 2, y: d / 2 + d * 0.17, "text-anchor": "middle", class: "lc-sc-q" }, q, "?");
    } else {
      PIPS[v].forEach(function (c) {
        svg("circle", { cx: d * 0.21 + c[0] * d * 0.29, cy: d * 0.21 + c[1] * d * 0.29, r: d * 0.08,
          class: "lc-sc-pip" + (on ? " is-hit" : "") }, q);
      });
    }
  }
  // Siatka i podziałka osi y; zwraca funkcję y(v).
  function yGrid(g, PL, PR, PT, PB, ax) {
    var y = function (v) { return PB - (PB - PT) * Math.min(v, ax.max * 1.04) / ax.max; };
    var d = decs(ax.step);
    for (var t = 0; t <= ax.max + ax.step * 1e-6; t += ax.step) {
      svg("line", { x1: PL, x2: PR, y1: y(t), y2: y(t), class: "lc-sc-grid" }, g);
      svg("text", { x: PL - 8, y: y(t) + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, fmt(t, d));
    }
    svg("line", { x1: PL, x2: PR, y1: PB, y2: PB, class: "lc-sc-axis" }, g);
    return y;
  }
  function yTitle(g, x, PT, PB, text) {
    var cy = (PT + PB) / 2;
    svg("text", { x: x, y: cy, "text-anchor": "middle", class: "lc-sc-axtitle",
      transform: "rotate(-90 " + x + " " + cy + ")" }, g, text);
  }
  function emptyNote(g, y, text) {
    svg("text", { x: W / 2, y: y, "text-anchor": "middle", class: "lc-sc-sub" }, g, text);
  }

  var KINDS = {};

  // =========================================================================
  // GROUP: telefon do n losowych osób, X̄ = średni czas dojazdu grupki
  // =========================================================================
  KINDS.group = function (cfg, api) {
    var SH = cfg.shape, SC = cfg.scale, SHIFT = cfg.shift, MU = cfg.mu, SIG = cfg.sigma;
    var XMAX = cfg.xmax || 90, BW = cfg.binw || 3, NB = Math.round(XMAX / BW);
    var N0 = cfg.n || 5, NS_ = cfg.ns || [1, 5, 30];
    var PL = 74, PR = 616, PT = 214, PB = 350, RX = 375, RY = 146;
    var LG = lgamma(SH);
    var st, memo = {}, yfun = null;

    function fresh(n) {
      st = { n: n, counts: new Array(NB).fill(0), k: 0, s1: 0, s2: 0, last: null, log: [] };
    }
    fresh(N0);

    function one(n) {
      var v = [], s = 0;
      for (var i = 0; i < n; i++) { var x = Math.round(SHIFT + rgamma(SH) * SC); v.push(x); s += x; }
      return { vals: v, mean: s / n };
    }
    function binOf(x) { return Math.max(0, Math.min(NB - 1, Math.floor(x / BW))); }
    function px(x) { return PL + (PR - PL) * Math.min(x, XMAX) / XMAX; }
    function sd() {
      if (st.k < 2) return null;
      var m = st.s1 / st.k;
      return Math.sqrt(Math.max(0, (st.s2 - st.k * m * m) / (st.k - 1)));
    }
    function add(o) {
      st.k += 1; st.s1 += o.mean; st.s2 += o.mean * o.mean; st.counts[binOf(o.mean)] += 1;
    }
    function commit(o) {
      o.no = st.k; st.last = o;
      st.log.unshift(o); if (st.log.length > 6) st.log.pop();
      if (st.k >= 20) memo[st.n] = sd();
    }
    function popPdf(x) {
      var z = x - SHIFT;
      if (z <= 0) return 0;
      return Math.exp((SH - 1) * Math.log(z) - z / SC - LG - SH * Math.log(SC));
    }

    function layout(n) {
      var per = n <= 5 ? n : Math.ceil(n / 2), rows = Math.ceil(n / per), s = n <= 5 ? 1 : 0.62;
      var x0 = 132, x1 = 624, dx = (x1 - x0) / per, pos = [];
      for (var i = 0; i < n; i++) {
        var r = Math.floor(i / per), c = i % per;
        pos.push({ x: x0 + dx * (c + 0.5), y: rows === 1 ? 52 : 28 + r * 56, s: s });
      }
      return pos;
    }

    function drawStage(o, shown) {
      var g = api.stage, step = api.step();
      g.textContent = "";
      bubble(g, 6, 2, 118, "Ile dojeżdżasz?");
      person(g, 46, 52, 1.2, "is-caller");
      svg("rect", { x: 54, y: 40, width: 7, height: 15, rx: 2, class: "lc-sc-phone" }, g);
      var n = o ? o.vals.length : st.n, pos = layout(n);
      pos.forEach(function (p, i) {
        var known = o && i < shown;
        person(g, p.x, p.y, p.s, known ? "" : "is-wait");
        if (!known) return;
        if (n <= 5) {
          svg("text", { x: p.x, y: p.y - 16, "text-anchor": "middle", class: "lc-sc-ans" }, g, o.vals[i] + " min");
        } else {
          svg("text", { x: p.x, y: p.y + 30, "text-anchor": "middle", class: "lc-sc-ans is-small" }, g, String(o.vals[i]));
        }
      });
      if (!o) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          "Grupka n = " + st.n + " czeka na telefon");
        return;
      }
      if (shown < n) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          "grupka nr " + (st.k + 1) + ": dzwonię…");
        return;
      }
      if (step >= 2) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read" }, g, "X̄ = " + fmt(o.mean, 1) + " min");
        svg("text", { x: RX, y: RY + 18, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "średni czas dojazdu w tej grupce, n = " + n);
      } else {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          "grupka nr " + o.no + ": średnio " + fmt(o.mean, 1) + " min");
      }
    }

    function drawLog() {
      var g = api.low, step = api.step(), y0 = 206;
      if (!st.log.length) { emptyNote(g, y0 + 60, "Zadzwoń do grupki, żeby usłyszeć jej czasy dojazdu."); return; }
      st.log.forEach(function (o, ri) {
        var q = svg("g", { opacity: 1 - ri * 0.13 }, g), y = y0 + ri * 28, x = 196;
        svg("text", { x: 92, y: y, class: "lc-sc-log" }, q, "grupka " + o.no);
        var vs = o.vals.slice(0, 10);
        vs.forEach(function (v) { svg("text", { x: x, y: y, "text-anchor": "end", class: "lc-sc-log" }, q, String(v)); x += 30; });
        if (o.vals.length > 10) { svg("text", { x: x - 18, y: y, class: "lc-sc-log" }, q, "…"); x += 14; }
        svg("text", { x: x - 6, y: y, class: "lc-sc-log" }, q, "→");
        svg("text", { x: x + 18, y: y, class: "lc-sc-log is-x" }, q,
          (step >= 2 ? "X̄ = " : "średnio ") + fmt(o.mean, 1));
      });
    }

    function drawHist() {
      var g = api.low, rel = api.step() >= 4, tot = st.k, sdN = SIG / Math.sqrt(st.n);
      var vals = st.counts.map(function (c) { return rel ? (tot ? c / (tot * BW) : 0) : c; });
      var top = Math.max.apply(null, vals);
      if (rel) top = Math.max(top, normPdf(MU, MU, sdN), popPdf(SHIFT + (SH - 1) * SC));
      var ax = rel ? niceAxis(top * 1.08, 4) : niceAxis(Math.max(top * 1.12, 5), 4);
      var y = yfun = yGrid(g, PL, PR, PT, PB, ax), i, x;
      if (rel) {
        var d = "M " + px(0) + " " + PB;
        for (i = 0; i <= 180; i++) { x = XMAX * i / 180; d += " L " + px(x) + " " + y(popPdf(x)); }
        svg("path", { d: d + " L " + px(XMAX) + " " + PB + " Z", class: "lc-sc-popfill" }, g);
      }
      var bw = (PR - PL) / NB;
      vals.forEach(function (v, j) {
        if (v > 0) svg("rect", { x: PL + j * bw + 0.5, y: y(v), width: bw - 1, height: PB - y(v), class: "lc-sc-bar is-hit" }, g);
      });
      if (rel) {
        var pts = [];
        for (i = 0; i <= 300; i++) { x = XMAX * i / 300; pts.push(px(x) + "," + y(normPdf(x, MU, sdN))); }
        svg("polyline", { points: pts.join(" "), class: "lc-sc-curve", fill: "none" }, g);
        svg("line", { x1: px(MU), x2: px(MU), y1: PT, y2: PB, class: "lc-sc-param" }, g);
        svg("text", { x: px(MU) + 6, y: PT + 12, class: "lc-sc-param-t" }, g, "μ = " + fmt(MU, 0) + " min");
        svg("line", { x1: 446, x2: 468, y1: PT + 10, y2: PT + 10, class: "lc-sc-curve" }, g);
        svg("text", { x: 474, y: PT + 14, class: "lc-sc-n" }, g, "krzywa normalna X̄");
        svg("rect", { x: 446, y: PT + 22, width: 22, height: 10, class: "lc-sc-popfill" }, g);
        svg("text", { x: 474, y: PT + 32, class: "lc-sc-n" }, g, "pojedyncze dojazdy");
      }
      for (x = 0; x <= XMAX; x += 10) {
        svg("line", { x1: px(x), x2: px(x), y1: PB, y2: PB + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: px(x), y: PB + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(x));
      }
      svg("text", { x: (PL + PR) / 2, y: PB + 38, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "X̄, średni czas dojazdu w grupce (min)");
      yTitle(g, 16, PT, PB, rel ? "skala gęstości" : "liczba grupek");
      var s = sd();
      svg("text", { x: PR, y: PT - 12, "text-anchor": "end", class: "lc-sc-n" }, g,
        "n = " + st.n + " · grupek: " + tot + " · rozrzut średnich (SD): " + (s === null ? "—" : fmt(s, 1) + " min"));
      svg("text", { x: W / 2, y: PB + 64, "text-anchor": "middle", class: "lc-sc-n" }, g,
        "SD średnich: " + NS_.map(function (k) {
          return "n = " + k + ": " + (memo[k] === undefined ? "—" : fmt(memo[k], 1));
        }).join(" · ") + " min");
    }

    function render() {
      api.low.textContent = "";
      drawStage(st.last, st.last ? st.last.vals.length : 0);
      if (api.step() >= 3) drawHist(); else drawLog();
    }

    return {
      render: render,
      reset: function () { memo = {}; fresh(N0); render(); },
      opt: function (name, v) { if (name === "n") { fresh(Number(v)); render(); } },
      go: function (done) {
        var o = one(st.n), n = st.n;
        tween(n <= 5 ? 900 : 1200, function (u) { drawStage(o, Math.floor(n * u)); }, function () {
          var j = binOf(o.mean);
          var land = function () { add(o); commit(o); render(); done(); };
          if (api.step() >= 3 && yfun) {
            o.no = st.k + 1; drawStage(o, n);
            var h = api.step() >= 4 ? (st.counts[j] + 1) / ((st.k + 1) * BW) : st.counts[j] + 1;
            fly(api, RX, RY + 6, PL + (j + 0.5) * (PR - PL) / NB, Math.max(PT, yfun(h)) - 8, 420, land);
          } else land();
        });
      },
      many: function (m, done) {
        var o;
        for (var i = 0; i < m; i++) { o = one(st.n); add(o); }
        commit(o); render(); done();
      }
    };
  };

  // =========================================================================
  // BUS: pasażer przychodzi w losowej chwili, X = czas czekania na autobus
  // =========================================================================
  KINDS.bus = function (cfg, api) {
    var T = cfg.period || 10, C = cfg.center || 5, TOL0 = cfg.tol === undefined ? 0.5 : cfg.tol;
    var AX0 = 150, AX1 = 540, AY = 90;
    var PL = 74, PR = 616, PT = 222, PB = 344, RX = 345, RY = 166;
    var st, yfun = null;
    function fresh() { st = { waits: [], counts: new Array(T).fill(0), last: null, log: [], tol: TOL0 }; }
    fresh();
    function ax(t) { return AX0 + (AX1 - AX0) * t / T; }
    function hx(t) { return PL + (PR - PL) * t / T; }
    function f2(x) { return fmt(x, 2); }

    function bus(g, x, y, op, cls) {
      var q = svg("g", { opacity: op, class: "lc-sc-bus" + (cls ? " " + cls : "") }, g);
      svg("rect", { x: x, y: y, width: 56, height: 24, rx: 5, class: "lc-sc-bus-body" }, q);
      for (var i = 0; i < 3; i++) svg("rect", { x: x + 5 + i * 17, y: y + 4, width: 12, height: 9, rx: 1.5, class: "lc-sc-bus-win" }, q);
      svg("circle", { cx: x + 12, cy: y + 25, r: 4.5, class: "lc-sc-bus-wheel" }, q);
      svg("circle", { cx: x + 44, cy: y + 25, r: 4.5, class: "lc-sc-bus-wheel" }, q);
    }

    // o: {t: chwila przyjścia, x: czekanie}; u: postęp animacji (undefined = koniec)
    function drawStage(o, u) {
      var g = api.stage, step = api.step();
      g.textContent = "";
      svg("line", { x1: 40, x2: 40, y1: 26, y2: AY + 10, class: "lc-sc-pole" }, g);
      svg("rect", { x: 22, y: 8, width: 36, height: 22, rx: 3, class: "lc-sc-stopsign" }, g);
      svg("text", { x: 40, y: 24, "text-anchor": "middle", class: "lc-sc-stopsign-t" }, g, "BUS");
      svg("line", { x1: AX0, x2: AX1, y1: AY, y2: AY, class: "lc-sc-axis" }, g);
      for (var m = 0; m <= T; m += 2) {
        svg("line", { x1: ax(m), x2: ax(m), y1: AY, y2: AY + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: ax(m), y: AY + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(m));
      }
      svg("text", { x: (AX0 + AX1) / 2, y: AY + 36, "text-anchor": "middle", class: "lc-sc-sub" }, g,
        "minuty od odjazdu poprzedniego autobusu");
      bus(g, AX0 - 62, AY - 32, 0.3);
      var p = u === undefined ? 1 : u, arrived = p >= 1;
      bus(g, AX1 + 6, AY - 32, arrived ? 1 : 0.3, arrived && o ? "is-here" : "");
      if (!o) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g, "Przystanek czeka na pasażera");
        return;
      }
      var A = 0.35;
      if (p < A) {
        var tc = o.t * p / A;
        svg("line", { x1: ax(tc), x2: ax(tc), y1: AY - 10, y2: AY + 2, class: "lc-sc-clock" }, g);
      } else {
        var v = (p - A) / (1 - A), tEnd = o.t + (T - o.t) * v;
        svg("line", { x1: ax(o.t), x2: ax(tEnd), y1: AY - 4, y2: AY - 4, class: "lc-sc-wait" }, g);
        person(g, ax(o.t), AY - 42, 0.85, "is-pass");
        if (arrived) {
          svg("line", { x1: ax(o.t), x2: ax(o.t), y1: AY - 60, y2: AY - 50, class: "lc-sc-brk" }, g);
          svg("line", { x1: ax(T), x2: ax(T), y1: AY - 60, y2: AY - 50, class: "lc-sc-brk" }, g);
          svg("line", { x1: ax(o.t), x2: ax(T), y1: AY - 55, y2: AY - 55, class: "lc-sc-brk" }, g);
          svg("text", { x: (ax(o.t) + ax(T)) / 2, y: AY - 62, "text-anchor": "middle", class: "lc-sc-brk-t" }, g,
            "czeka " + f2(o.x) + " min");
        }
      }
      if (!arrived) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          "pasażer nr " + (st.waits.length + 1) + " czeka…");
      } else if (step >= 2) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read" }, g, "X = " + f2(o.x) + " min");
        svg("text", { x: RX, y: RY + 18, "text-anchor": "middle", class: "lc-sc-sub" }, g, "czas czekania tego pasażera");
      } else {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          "pasażer nr " + o.no + " czekał " + f2(o.x) + " min");
      }
    }

    function drawLog() {
      var g = api.low, step = api.step(), y0 = 222;
      if (!st.log.length) { emptyNote(g, y0 + 60, "Przyjdź na przystanek, żeby zobaczyć, ile trzeba czekać."); return; }
      st.log.forEach(function (o, ri) {
        var q = svg("g", { opacity: 1 - ri * 0.13 }, g), y = y0 + ri * 28;
        svg("text", { x: 92, y: y, class: "lc-sc-log" }, q, "pasażer " + o.no);
        svg("text", { x: 236, y: y, class: "lc-sc-log" }, q, "przyszedł w " + f2(o.t) + " min");
        svg("text", { x: 440, y: y, class: "lc-sc-log" }, q, "→");
        svg("text", { x: 464, y: y, class: "lc-sc-log is-x" }, q, (step >= 2 ? "X = " : "czekał ") + f2(o.x) + " min");
      });
    }

    function hits() {
      var h = st.tol, k = 0;
      for (var i = 0; i < st.waits.length; i++) if (Math.abs(st.waits[i] - C) <= h) k++;
      return k;
    }

    function drawHist() {
      var g = api.low, rel = api.step() >= 4, tot = st.waits.length;
      var vals = st.counts.map(function (c) { return rel ? (tot ? c / tot : 0) : c; });
      var top = Math.max.apply(null, vals);
      var ax2 = rel ? niceAxis(Math.max(top * 1.1, 0.15), 3) : niceAxis(Math.max(top * 1.12, 5), 4);
      var y = yfun = yGrid(g, PL, PR, PT, PB, ax2), bw = (PR - PL) / T, h = st.tol;
      if (rel) {
        var a = hx(C - h), b = hx(C + h);
        if (h > 0) svg("rect", { x: a, y: y(1 / T), width: b - a, height: PB - y(1 / T), class: "lc-sc-tol" }, g);
      }
      vals.forEach(function (v, j) {
        if (v > 0) svg("rect", { x: hx(j) + 1.5, y: y(v), width: bw - 3, height: PB - y(v), class: "lc-sc-bar" }, g);
        if (!rel && v > 0) svg("text", { x: hx(j + 0.5), y: y(v) - 5, "text-anchor": "middle", class: "lc-sc-val" }, g, String(v));
      });
      // dywanik pojedynczych czekań pod osią
      var from = Math.max(0, tot - 3000);
      for (var i = from; i < tot; i++) {
        var w = st.waits[i], hit = rel && Math.abs(w - C) <= h;
        svg("line", { x1: hx(w), x2: hx(w), y1: PB + 3, y2: PB + 13, class: "lc-sc-rug" + (hit ? " is-hit" : "") }, g);
      }
      if (rel) {
        if (h > 0) {
          svg("rect", { x: hx(C - h), y: y(1 / T), width: Math.max(1, hx(C + h) - hx(C - h)), height: PB - y(1 / T), class: "lc-sc-tol-edge" }, g);
        } else {
          svg("line", { x1: hx(C), x2: hx(C), y1: y(1 / T), y2: PB + 14, class: "lc-sc-tol-line" }, g);
        }
        svg("line", { x1: PL, x2: PR, y1: y(1 / T), y2: y(1 / T), class: "lc-sc-param" }, g);
        svg("text", { x: PR, y: y(1 / T) - 6, "text-anchor": "end", class: "lc-sc-param-t" }, g, "model: 1/10 na każdą minutę");
        svg("text", { x: hx(C), y: PT - 2, "text-anchor": "middle", class: "lc-sc-n is-hit" }, g,
          h > 0 ? f2(C - h) + "–" + f2(C + h) + " min" : "dokładnie " + f2(C) + " min");
      }
      for (var m = 0; m <= T; m++) {
        svg("text", { x: hx(m), y: PB + 29, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(m));
      }
      svg("text", { x: (PL + PR) / 2, y: PB + 47, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "X, czas czekania (min)");
      yTitle(g, 16, PT, PB, rel ? "częstość (przedział 1 min)" : "liczba pasażerów");
      svg("text", { x: PL, y: PT - 14, class: "lc-sc-n" }, g, "n = " + tot);
      if (rel) {
        var k = hits(), wd = 2 * h;
        svg("text", { x: W / 2, y: PB + 74, "text-anchor": "middle", class: "lc-sc-n" }, g,
          tot ? (h > 0 ? "W przedział " + f2(C - h) + "–" + f2(C + h) + " min wpadło " : "Dokładnie " + f2(C) + " min: ")
            + k + " z " + tot + " czekań = " + fmt(k / tot, 3)
            : "Jeszcze nikt nie czekał.");
        svg("text", { x: W / 2, y: PB + 94, "text-anchor": "middle", class: "lc-sc-n is-hit" }, g,
          h > 0 ? "Pole prostokąta nad przedziałem: 1/10 × " + f2(wd) + " = " + fmt(wd / T, 3)
            : "Pole nad jednym punktem: 1/10 × 0 = 0");
      }
    }

    function render() {
      api.low.textContent = "";
      drawStage(st.last);
      if (api.step() >= 3) drawHist(); else drawLog();
    }
    function one() { var t = Math.random() * T; return { t: t, x: T - t }; }
    function add(o) { st.waits.push(o.x); st.counts[Math.min(T - 1, Math.floor(o.x))] += 1; }
    function commit(o) { o.no = st.waits.length; st.last = o; st.log.unshift(o); if (st.log.length > 6) st.log.pop(); }

    return {
      render: render,
      reset: function () { fresh(); render(); },
      // szerokość przedziału nie zmienia doświadczenia, tylko pytanie: czekania zostają
      opt: function (name, v) { if (name === "tol") { st.tol = Number(v); render(); } },
      go: function (done) {
        var o = one();
        tween(1300, function (u) { drawStage(o, u); }, function () {
          var land = function () { add(o); commit(o); render(); done(); };
          if (api.step() >= 3 && yfun) {
            o.no = st.waits.length + 1; drawStage(o);
            var j = Math.min(T - 1, Math.floor(o.x));
            var h = api.step() >= 4 ? (st.counts[j] + 1) / (st.waits.length + 1) : st.counts[j] + 1;
            fly(api, RX, RY + 6, hx(j + 0.5), Math.max(PT, yfun(h)) - 8, 420, land);
          } else land();
        });
      },
      many: function (m, done) {
        var o;
        for (var i = 0; i < m; i++) { o = one(); add(o); }
        commit(o); render(); done();
      }
    };
  };

  // =========================================================================
  // STREAK: rzut po passie bez szóstki; kontrast: talia kart bez zwracania
  // =========================================================================
  KINDS.streak = function (cfg, api) {
    var L = cfg.run || 5, LD = cfg.deckRun || 26, MODE0 = cfg.mode || "dice";
    var PL = 90, PR = 600, PT = 206, PB = 330, RX = 375, RY = 138;
    var RANKS = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"], SUITS = ["♠", "♥", "♦", "♣"];
    var st;
    function fresh(mode) { st = { mode: mode, n: 0, hit: 0, all: 0, allHit: 0, last: null, log: [] }; }
    fresh(MODE0);

    function roll() { return 1 + Math.floor(Math.random() * 6); }
    function runDice() {
      var r = [], s = 0;
      while (s < L) { var v = roll(); r.push(v); s = v === 6 ? 0 : s + 1; }
      var d = roll();
      return { items: r, d: d, hit: d === 6 };
    }
    function runDeck() {
      var sh = 0, deck = [];
      for (var c = 0; c < 52; c++) deck.push(c);
      for (;;) {
        sh++;
        for (var i = 51; i > 0; i--) { var j = Math.floor(Math.random() * (i + 1)), t = deck[i]; deck[i] = deck[j]; deck[j] = t; }
        var ok = true;
        for (var k = 0; k < LD; k++) if (deck[k] % 13 === 0) { ok = false; break; }
        if (ok) return { items: deck.slice(0, LD), d: deck[LD], hit: deck[LD] % 13 === 0, shuffles: sh };
      }
    }
    function run() { return st.mode === "deck" ? runDeck() : runDice(); }
    function add(o) {
      st.n += 1; if (o.hit) st.hit += 1;
      if (st.mode === "deck") { st.all += 52 * o.shuffles; st.allHit += 4 * o.shuffles; }
      else { st.all += o.items.length + 1; st.allHit += o.items.filter(function (v) { return v === 6; }).length + (o.hit ? 1 : 0); }
    }
    function commit(o) { o.no = st.n; st.last = o; st.log.unshift(o); if (st.log.length > 6) st.log.pop(); }
    function cardTxt(c) { return RANKS[c % 13] + SUITS[Math.floor(c / 13)]; }
    function isRed(c) { var s = Math.floor(c / 13); return s === 1 || s === 2; }

    function card(g, x, y, w, h, c, big, on) {
      svg("rect", { x: x, y: y, width: w, height: h, rx: 3, class: "lc-sc-card" + (on ? " is-hit" : "") }, g);
      if (c === null) {
        svg("text", { x: x + w / 2, y: y + h / 2 + 8, "text-anchor": "middle", class: "lc-sc-q" }, g, "?");
        return;
      }
      svg("text", { x: x + (big ? w / 2 : 2), y: y + (big ? h / 2 + 6 : 11), "text-anchor": big ? "middle" : "start",
        class: "lc-sc-card-t" + (big ? " is-big" : "") + (isRed(c) ? " is-red" : "") + (on ? " is-hit" : "") }, g, cardTxt(c));
    }

    // shown: ile elementów serii odsłonięto; dShown: czy odsłonięto rzut decydujący
    function drawStage(o, shown, dShown) {
      var g = api.stage, step = api.step(), deck = st.mode === "deck";
      g.textContent = "";
      bubble(g, 6, 4, 152, deck ? "As się należy!" : "Szóstka się należy!");
      person(g, 44, 54, 1.2, "is-caller");
      var X0 = 180, XD = 562;
      svg("text", { x: 624, y: 26, "text-anchor": "end", class: "lc-sc-sub" }, g, deck ? "karta po passie" : "rzut po passie");
      if (!o) {
        if (deck) card(g, XD, 34, 46, 60, null, true, false); else drawDie(g, XD, 36, 46, null, false);
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          deck ? "Talia czeka na tasowanie" : "Kostka czeka na rzut");
        return;
      }
      var items = o.items.slice(0, shown);
      if (deck) {
        items.forEach(function (c, i) { card(g, X0 + i * 13.8, 42, 22, 32, c, false, false); });
        if (shown >= o.items.length) {
          svg("path", { d: "M " + X0 + " 82 v 6 H " + (X0 + 25 * 13.8 + 22) + " v -6", class: "lc-sc-brk", fill: "none" }, g);
          svg("text", { x: (X0 + X0 + 25 * 13.8 + 22) / 2, y: 104, "text-anchor": "middle", class: "lc-sc-sub" }, g,
            LD + " kart bez asa (tasowanie nr " + o.shuffles + ")");
        }
        card(g, XD, 34, 46, 60, dShown ? o.d : null, true, dShown && o.hit);
      } else {
        var SHOW = 8, D = 36, GAP = 8, from = Math.max(0, items.length - SHOW), vis = items.slice(from);
        if (from > 0) svg("text", { x: X0 - 8, y: 64, "text-anchor": "end", class: "lc-sc-read is-plain" }, g, "…");
        vis.forEach(function (v, i) { drawDie(g, X0 + i * (D + GAP), 40, D, v, v === 6); });
        if (shown >= o.items.length) {
          var n = vis.length, a = X0 + (n - L) * (D + GAP), b = X0 + (n - 1) * (D + GAP) + D;
          svg("path", { d: "M " + a + " 82 v 6 H " + b + " v -6", class: "lc-sc-brk", fill: "none" }, g);
          svg("text", { x: (a + b) / 2, y: 104, "text-anchor": "middle", class: "lc-sc-sub" }, g, L + " rzutów bez szóstki");
        }
        drawDie(g, XD, 36, 46, dShown ? o.d : null, dShown && o.hit);
      }
      var unit = deck ? "talia" : "seria", what = deck ? "as" : "szóstka";
      if (!dShown) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g, unit + " nr " + (st.n + 1) + "…");
      } else if (step >= 2) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read" + (o.hit ? "" : " is-plain") }, g,
          o.hit ? "A zaszło: " + (deck ? cardTxt(o.d) : "6") : "A nie zaszło: " + (deck ? cardTxt(o.d) : String(o.d)));
        svg("text", { x: RX, y: RY + 18, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          deck ? "A = as zaraz po " + LD + " kartach bez asa" : "A = szóstka zaraz po " + L + " rzutach bez szóstki");
      } else {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          unit + " nr " + o.no + ": po passie " + (o.hit ? what + "!" : (deck ? cardTxt(o.d) : String(o.d)) + ", nie " + what));
      }
    }

    function drawLog() {
      var g = api.low, step = api.step(), y0 = 200, deck = st.mode === "deck";
      if (!st.log.length) { emptyNote(g, y0 + 60, deck ? "Potasuj talię i wykładaj karty." : "Rzucaj, aż przyjdzie passa bez szóstki."); return; }
      st.log.forEach(function (o, ri) {
        var q = svg("g", { opacity: 1 - ri * 0.13 }, g), y = y0 + ri * 28, x = 186;
        svg("text", { x: 80, y: y, class: "lc-sc-log" }, q, (deck ? "talia " : "seria ") + o.no);
        if (deck) {
          svg("text", { x: x, y: y, class: "lc-sc-log" }, q, LD + " kart bez asa  |");
          x += 186;
          svg("text", { x: x, y: y, class: "lc-sc-log" + (o.hit ? " is-hit" : "") }, q, cardTxt(o.d));
          x += 40;
        } else {
          var v = o.items, sh = v.length > 9 ? v.slice(-9) : v;
          if (v.length > 9) { svg("text", { x: x - 16, y: y, class: "lc-sc-log" }, q, "…"); }
          sh.forEach(function (d) { svg("text", { x: x, y: y, class: "lc-sc-log" + (d === 6 ? " is-hit" : "") }, q, String(d)); x += 20; });
          svg("text", { x: x, y: y, class: "lc-sc-log" }, q, "|");
          x += 16;
          svg("text", { x: x, y: y, class: "lc-sc-log" + (o.hit ? " is-hit" : "") }, q, String(o.d));
          x += 28;
        }
        svg("text", { x: x, y: y, class: "lc-sc-log" }, q, "→");
        svg("text", { x: x + 24, y: y, class: "lc-sc-log is-x" }, q,
          step >= 2 ? (o.hit ? "A zaszło" : "A nie zaszło") : (o.hit ? (deck ? "as" : "szóstka") : (deck ? "nie as" : "nie szóstka")));
      });
    }

    function drawHist() {
      var g = api.low, rel4 = api.step() >= 4, deck = st.mode === "deck";
      var f1 = st.n ? st.hit / st.n : 0, f2 = st.all ? st.allHit / st.all : 0;
      var p1 = deck ? 4 / (52 - LD) : 1 / 6, p2 = deck ? 4 / 52 : 1 / 6;
      var ax = niceAxis(Math.min(1, Math.max(0.3, f1 * 1.1, f2 * 1.1)), 4);
      var y = yGrid(g, PL, PR, PT, PB, ax);
      var bars = [
        { x: 245, f: f1, k: st.hit, n: st.n, p: p1, cls: "lc-sc-bar is-hit",
          lab: deck ? "as po " + LD + " kartach bez asa" : "szóstka po passie" },
        { x: 455, f: f2, k: st.allHit, n: st.all, p: p2, cls: "lc-sc-bar",
          lab: deck ? "asy we wszystkich kartach" : "szóstki we wszystkich rzutach" }
      ];
      bars.forEach(function (b) {
        if (b.n) svg("rect", { x: b.x - 60, y: y(b.f), width: 120, height: PB - y(b.f), class: b.cls }, g);
        if (b.n) {
          var inside = PB - y(b.f) > 24;
          svg("text", { x: b.x, y: inside ? y(b.f) + 17 : y(b.f) - 6, "text-anchor": "middle",
            class: "lc-sc-val" + (inside ? " is-in" : "") }, g, fmt(b.f, 3));
        }
        svg("text", { x: b.x, y: PB + 20, "text-anchor": "middle", class: "lc-sc-tick is-x" }, g, b.lab);
        svg("text", { x: b.x, y: PB + 38, "text-anchor": "middle", class: "lc-sc-n" }, g, b.k + " z " + b.n);
        if (rel4 && deck) {
          svg("line", { x1: b.x - 84, x2: b.x + 84, y1: y(b.p), y2: y(b.p), class: "lc-sc-param" }, g);
          svg("text", { x: b.x + 84, y: y(b.p) - 6, "text-anchor": "end", class: "lc-sc-param-t" }, g,
            (b === bars[0] ? "4/" + (52 - LD) : "4/52") + " ≈ " + fmt(b.p, 3));
        }
      });
      if (rel4 && !deck) {
        svg("line", { x1: PL, x2: PR, y1: y(1 / 6), y2: y(1 / 6), class: "lc-sc-param" }, g);
        svg("text", { x: PR, y: y(1 / 6) - 6, "text-anchor": "end", class: "lc-sc-param-t" }, g, "P(szóstka) = 1/6 ≈ 0.167");
      }
      yTitle(g, 30, PT, PB, "częstość względna");
      svg("text", { x: PR, y: PT - 12, "text-anchor": "end", class: "lc-sc-n" }, g,
        (deck ? "talii z passą: " : "serii: ") + st.n + (deck ? " · kart wyłożonych: " : " · rzutów: ") + st.all);
    }

    function render() {
      api.low.textContent = "";
      var o = st.last;
      drawStage(o, o ? o.items.length : 0, !!o);
      if (api.step() >= 3) drawHist(); else drawLog();
    }

    return {
      render: render,
      label: function () { return st.mode === "deck" ? "Tasuj i wykładaj" : null; },
      reset: function () { fresh(MODE0); render(); },
      opt: function (name, v) { if (name === "mode") { fresh(v); render(); } },
      go: function (done) {
        var o = run(), len = o.items.length, deck = st.mode === "deck";
        var per = deck ? 45 : 150, ms = len * per;
        tween(ms, function (u) { drawStage(o, Math.min(len, Math.floor(len * u) + 1), false); }, function () {
          drawStage(o, len, false);
          setTimeout(function () {
            add(o); commit(o); render(); done();
          }, REDUCE ? 0 : 450);
        });
      },
      many: function (m, done) {
        var o;
        for (var i = 0; i < m; i++) { o = run(); add(o); }
        commit(o); render(); done();
      }
    };
  };

  // =========================================================================
  // SCRATCH: zdrapka z kiosku, X = wygrana
  // =========================================================================
  KINDS.scratch = function (cfg, api) {
    var TK = cfg.tickets, K0 = cfg.ticket || "main", PRICE = cfg.price;
    var BL = 74, BR = 296, RL = 372, RR = 612, PT = 214, PB = 336, RX = 375, RY = 146;
    var st;
    function moments(t) {
      var e = 0, e2 = 0;
      t.prizes.forEach(function (x, i) { e += x * t.probs[i]; e2 += x * x * t.probs[i]; });
      return { e: e, sd: Math.sqrt(Math.max(0, e2 - e * e)) };
    }
    function fresh(key) {
      var t = TK[key];
      st = { key: key, t: t, m: moments(t), counts: new Array(t.prizes.length).fill(0), n: 0, sum: 0, means: [], last: null, log: [] };
    }
    fresh(K0);

    function one() {
      var u = Math.random(), acc = 0, t = st.t;
      for (var i = 0; i < t.prizes.length; i++) { acc += t.probs[i]; if (u < acc) return { i: i, x: t.prizes[i] }; }
      return { i: t.prizes.length - 1, x: t.prizes[t.prizes.length - 1] };
    }
    function add(o) { st.counts[o.i] += 1; st.n += 1; st.sum += o.x; st.means.push(st.sum / st.n); }
    function commit(o) { o.no = st.n; st.last = o; st.log.unshift(o); if (st.log.length > 6) st.log.pop(); }
    function zl(x) { return (Math.round(x) === x ? String(x) : fmt(x, 2)) + " zł"; }

    function drawStage(o, u) {
      var g = api.stage, step = api.step(), t = st.t;
      g.textContent = "";
      // kiosk
      for (var k = 0; k < 6; k++) svg("rect", { x: 12 + k * 22, y: 14, width: 22, height: 18, class: "lc-sc-awning" + (k % 2 ? " is-alt" : "") }, g);
      svg("rect", { x: 16, y: 32, width: 124, height: 86, class: "lc-sc-kiosk" }, g);
      svg("text", { x: 78, y: 52, "text-anchor": "middle", class: "lc-sc-kiosk-t" }, g, "KIOSK");
      svg("rect", { x: 32, y: 62, width: 92, height: 40, class: "lc-sc-kiosk-win" }, g);
      person(g, 196, 52, 1.2, "is-caller");
      // los
      var x0 = 252, y0 = 14;
      svg("rect", { x: x0, y: y0, width: 246, height: 104, rx: 8, class: "lc-sc-ticket" }, g);
      svg("text", { x: x0 + 14, y: y0 + 21, class: "lc-sc-ticket-h" }, g, t.name);
      svg("text", { x: x0 + 232, y: y0 + 21, "text-anchor": "end", class: "lc-sc-ticket-h" }, g, "cena " + zl(PRICE));
      var fx = x0 + 18, fy = y0 + 30, fw = 210, fh = 48;
      svg("rect", { x: fx, y: fy, width: fw, height: fh, rx: 4, class: "lc-sc-field" }, g);
      if (o) {
        svg("text", { x: fx + fw / 2, y: fy + 33, "text-anchor": "middle", class: "lc-sc-prize" + (o.x > 0 ? " is-hit" : "") }, g,
          o.x > 0 ? zl(o.x) : "0 zł");
      }
      var sc = o ? (u === undefined ? 1 : u) : 0;
      if (sc < 1) svg("rect", { x: fx + fw * sc, y: fy, width: fw * (1 - sc), height: fh, rx: 4, class: "lc-sc-silver" }, g);
      if (sc < 1 && !o) svg("text", { x: fx + fw / 2, y: fy + 30, "text-anchor": "middle", class: "lc-sc-silver-t" }, g, "zdrap tutaj");
      svg("text", { x: x0 + 123, y: y0 + 96, "text-anchor": "middle", class: "lc-sc-sub" }, g, t.foot);
      if (!o) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g, "Los czeka na zdrapanie");
      } else if (u !== undefined && u < 1) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g, "los nr " + (st.n + 1) + "…");
      } else if (step >= 2) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read" }, g, "X = " + zl(o.x));
        svg("text", { x: RX, y: RY + 18, "text-anchor": "middle", class: "lc-sc-sub" }, g, "wygrana z tego losu");
      } else {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          "los nr " + o.no + ": " + (o.x > 0 ? "wygrana " + zl(o.x) : "nic"));
      }
    }

    function drawLog() {
      var g = api.low, step = api.step(), y0 = 206;
      if (!st.log.length) { emptyNote(g, y0 + 60, "Kup los i zdrap go."); return; }
      st.log.forEach(function (o, ri) {
        var q = svg("g", { opacity: 1 - ri * 0.13 }, g), y = y0 + ri * 28;
        svg("text", { x: 180, y: y, class: "lc-sc-log" }, q, "los " + o.no);
        svg("text", { x: 300, y: y, class: "lc-sc-log" }, q, "→");
        svg("text", { x: 330, y: y, class: "lc-sc-log is-x" + (o.x > 0 ? " is-hit" : "") }, q, (step >= 2 ? "X = " : "") + zl(o.x));
      });
    }

    function drawLow() {
      var g = api.low, rel = api.step() >= 4, t = st.t, nb = t.prizes.length, n = st.n, m = st.m;
      // lewy: wygrane
      var vals = st.counts.map(function (c) { return rel ? (n ? c / n : 0) : c; });
      var top = Math.max.apply(null, vals);
      if (rel) top = Math.max(top, Math.max.apply(null, t.probs));
      var ax = rel ? niceAxis(Math.max(top * 1.08, 0.1), 4) : niceAxis(Math.max(top * 1.12, 5), 4);
      var y = yGrid(g, BL, BR, PT, PB, ax), slot = (BR - BL) / nb, bw = Math.min(46, slot * 0.6);
      vals.forEach(function (v, i) {
        var cx = BL + slot * (i + 0.5);
        if (v > 0) svg("rect", { x: cx - bw / 2, y: y(v), width: bw, height: PB - y(v), class: "lc-sc-bar" + (t.prizes[i] > 0 ? " is-hit" : "") }, g);
        if (!rel && v > 0) svg("text", { x: cx, y: y(v) - 5, "text-anchor": "middle", class: "lc-sc-val" }, g, String(v));
        if (rel) {
          svg("circle", { cx: cx, cy: y(t.probs[i]), r: 5.5, class: "lc-sc-theory" }, g);
          svg("text", { x: cx, y: PB + 34, "text-anchor": "middle", class: "lc-sc-n" }, g, fmt(t.probs[i], 2));
        }
        svg("text", { x: cx, y: PB + 18, "text-anchor": "middle", class: "lc-sc-tick is-x" }, g, zl(t.prizes[i]));
      });
      svg("text", { x: (BL + BR) / 2, y: PB + (rel ? 52 : 40), "text-anchor": "middle", class: "lc-sc-axtitle" }, g, "X, wygrana z losu");
      yTitle(g, 16, PT, PB, rel ? "częstość względna" : "liczba losów");
      if (rel) {
        svg("circle", { cx: BL + 6, cy: PT - 16, r: 5.5, class: "lc-sc-theory" }, g);
        svg("text", { x: BL + 16, y: PT - 12, class: "lc-sc-n" }, g, "P(X = x)");
      }

      // prawy: bieżąca średnia wygranej
      var means = st.means, k0 = Math.min(10, Math.max(0, n - 1));
      var mx = Math.max(1, PRICE * 1.3, m.e * 1.5);
      for (var j = k0; j < n; j++) if (means[j] > mx) mx = means[j];
      var ax2 = niceAxis(mx * 1.05, 4);
      var y2 = yGrid(g, RL, RR, PT, PB, ax2);
      var xr = function (i) { return RL + (RR - RL) * (n > 1 ? i / (n - 1) : 0); };
      if (n > 0) {
        var stepN = Math.max(1, Math.floor(n / 400)), pts = [];
        for (j = 0; j < n; j += stepN) pts.push(xr(j) + "," + Math.max(PT - 4, y2(means[j])));
        pts.push(xr(n - 1) + "," + Math.max(PT - 4, y2(means[n - 1])));
        svg("polyline", { points: pts.join(" "), class: "lc-sc-run", fill: "none" }, g);
        svg("circle", { cx: xr(n - 1), cy: Math.max(PT - 4, y2(means[n - 1])), r: 4, class: "lc-sc-run-dot" }, g);
      }
      if (rel) {
        svg("line", { x1: RL, x2: RR, y1: y2(m.e), y2: y2(m.e), class: "lc-sc-param" }, g);
        svg("text", { x: RL + 4, y: y2(m.e) + 15, class: "lc-sc-param-t" }, g, "E(X) = " + fmt(m.e, 2) + " zł");
        svg("line", { x1: RL, x2: RR, y1: y2(PRICE), y2: y2(PRICE), class: "lc-sc-price" }, g);
        svg("text", { x: RR, y: y2(PRICE) - 5, "text-anchor": "end", class: "lc-sc-price-t" }, g, "cena losu " + zl(PRICE));
      }
      svg("text", { x: RL, y: PB + 18, "text-anchor": "middle", class: "lc-sc-tick" }, g, "1");
      svg("text", { x: RR, y: PB + 18, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(Math.max(1, n)));
      svg("text", { x: (RL + RR) / 2, y: PB + (rel ? 52 : 40), "text-anchor": "middle", class: "lc-sc-axtitle" }, g, "liczba kupionych losów");
      svg("text", { x: RR, y: PT - 12, "text-anchor": "end", class: "lc-sc-n" }, g,
        "średnia wygrana: " + (n ? fmt(st.sum / n, 2) + " zł" : "—") + " · losów: " + n);
      if (rel) {
        svg("text", { x: W / 2, y: PB + 76, "text-anchor": "middle", class: "lc-sc-n" }, g,
          "E(X) = " + fmt(m.e, 2) + " zł, cena " + zl(PRICE) + ": średnio tracisz " + fmt(PRICE - m.e, 2) + " zł na losie.");
        svg("text", { x: W / 2, y: PB + 96, "text-anchor": "middle", class: "lc-sc-n is-hit" }, g,
          m.sd > 0 ? "SD = " + fmt(m.sd, 2) + " zł: typowa odległość wygranej od E(X)."
            : "SD = 0.00 zł: każda wygrana jest taka sama.");
      }
    }

    function render() {
      api.low.textContent = "";
      drawStage(st.last);
      if (api.step() >= 3) drawLow(); else drawLog();
    }

    return {
      render: render,
      reset: function () { fresh(K0); render(); },
      opt: function (name, v) { if (name === "ticket" && TK[v]) { fresh(v); render(); } },
      go: function (done) {
        var o = one();
        tween(900, function (u) { drawStage(o, u); }, function () {
          var land = function () { add(o); commit(o); render(); done(); };
          if (api.step() >= 3) {
            o.no = st.n + 1; drawStage(o);
            var slot = (BR - BL) / st.t.prizes.length;
            fly(api, RX, RY + 6, BL + slot * (o.i + 0.5), PB - 10, 380, land);
          } else land();
        });
      },
      many: function (m, done) {
        var o;
        for (var i = 0; i < m; i++) { o = one(); add(o); }
        commit(o); render(); done();
      }
    };
  };

  // =========================================================================
  // widget
  // =========================================================================
  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    if (!stepper) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    if (!KINDS[cfg.kind]) return;
    var s = svg("svg", { class: "lc-sc-svg", role: "img", viewBox: "0 0 " + W + " " + (cfg.height || 420),
      "aria-label": cfg.aria || "Scena doświadczenia i wykres wyników" }, root);
    var api = {
      stage: svg("g", {}, s), low: svg("g", {}, s), fly: svg("g", {}, s),
      step: function () { return Number(stepper.getAttribute("data-lc-step")) || 1; }
    };
    var scene = KINDS[cfg.kind](cfg, api), busy = false;
    var labelBtn = stepper.querySelector("[data-sc-act='go']");
    var labels = labelBtn && labelBtn.getAttribute("data-labels")
      ? JSON.parse(labelBtn.getAttribute("data-labels")) : null;
    // stan początkowy przełączników: reset wraca do niego
    var opts0 = [];
    stepper.querySelectorAll("[data-sc-opt]").forEach(function (b) { opts0.push([b, b.getAttribute("aria-pressed")]); });

    function refresh() {
      scene.render();
      stepper.querySelectorAll("[data-sc-act], [data-sc-opt]").forEach(function (b) { b.disabled = busy; });
      if (labels && labelBtn) {
        var span = labelBtn.querySelector("span");
        if (span) span.textContent = (scene.label && scene.label()) || labels[Math.min(labels.length, api.step()) - 1];
      }
    }
    refresh();

    stepper.addEventListener("click", function (e) {
      var o = e.target.closest("[data-sc-opt]");
      if (o && stepper.contains(o)) {
        if (busy) return;
        var kv = o.getAttribute("data-sc-opt").split(":");
        o.parentNode.querySelectorAll("[data-sc-opt]").forEach(function (b) {
          b.setAttribute("aria-pressed", b === o ? "true" : "false");
        });
        scene.opt(kv[0], kv[1]);
        refresh();
        return;
      }
      var b = e.target.closest("[data-sc-act]");
      if (b && stepper.contains(b)) {
        if (busy) return;
        var a = b.getAttribute("data-sc-act");
        busy = true; refresh();
        var fin = function () { busy = false; refresh(); };
        if (a === "go") scene.go(fin); else scene.many(Number(a.replace("m", "")), fin);
        return;
      }
      if (e.target.closest('[data-lc-nav="reset"]')) {
        setTimeout(function () {
          opts0.forEach(function (p) { p[0].setAttribute("aria-pressed", p[1]); });
          scene.reset(); refresh();
        }, 0);
      }
    });

    new MutationObserver(function () { if (!busy) refresh(); })
      .observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
  }

  function scan() { document.querySelectorAll(".lc-sc:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
