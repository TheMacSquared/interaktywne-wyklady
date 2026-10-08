// Sceny wykładu 02 (PROTOTYPY 2026-10-08): doświadczenie z życia → zmienna → rozkład / model.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper). Wzorzec init/KINDS
// jak w statystyce 00 (scenes.js), obok istniejącego experiment.js (.lc-exp), z którym nie koliduje.
// config.kind:
//   "group"   telefon do n losowych osób o czas dojazdu, X̄ grupki (rozkład średniej, CTG)
//   "scratch" zdrapka z kiosku, X = wygrana (E(X) jako średnia na dłuższą metę, SD jako rozrzut)
// Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji)
// Rysuje SVG; serwer R podaje tylko konfigurację i teksty kroków.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640;
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

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
  // Jeden wiersz odczytu pod wykresem, wyśrodkowany; items: [{mark: f(x, y) rysująca wzorzec 22 px, text}].
  // Szerokość tekstu z czcionki mono 13 px (≈ 7.8 px na znak), bez pomiaru DOM.
  function readout(g, y, items) {
    var CH = 7.8, MW = 22, GAP = 8, SEP = 26, tot = 0;
    items.forEach(function (it, i) { tot += (it.mark ? MW + GAP : 0) + it.text.length * CH + (i ? SEP : 0); });
    var x = W / 2 - tot / 2;
    items.forEach(function (it, i) {
      if (i) x += SEP;
      if (it.mark) { it.mark(x, y); x += MW + GAP; }
      svg("text", { x: x, y: y + 4.5, class: "lc-sc-n" }, g, it.text);
      x += it.text.length * CH;
    });
  }
  function markDash(g) {
    return function (x, y) { svg("line", { x1: x, x2: x + 22, y1: y, y2: y, class: "lc-sc-param" }, g); };
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
    var N0 = cfg.n || 5;
    var PL = 74, PR = 616, PT = 214, PB = 350, RX = 375, RY = 146;
    var LG = lgamma(SH);
    var st, yfun = null;

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
      var g = api.stage;
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
          "Grupka czeka na telefon");
        return;
      }
      if (shown < n) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          "grupka nr " + (st.k + 1) + "…");
        return;
      }
      svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read" }, g, "X̄ = " + fmt(o.mean, 1) + " min");
    }

    function drawLog() {
      var g = api.low, y0 = 206;
      if (!st.log.length) { emptyNote(g, y0 + 60, "Zadzwoń do grupki, żeby usłyszeć jej czasy dojazdu."); return; }
      st.log.forEach(function (o, ri) {
        var q = svg("g", { opacity: 1 - ri * 0.13 }, g), y = y0 + ri * 28, x = 196;
        svg("text", { x: 92, y: y, class: "lc-sc-log" }, q, "grupka " + o.no);
        var vs = o.vals.slice(0, 10);
        vs.forEach(function (v) { svg("text", { x: x, y: y, "text-anchor": "end", class: "lc-sc-log" }, q, String(v)); x += 30; });
        if (o.vals.length > 10) { svg("text", { x: x - 18, y: y, class: "lc-sc-log" }, q, "…"); x += 14; }
        svg("text", { x: x - 6, y: y, class: "lc-sc-log" }, q, "→");
        svg("text", { x: x + 18, y: y, class: "lc-sc-log is-x" }, q, "X̄ = " + fmt(o.mean, 1));
      });
    }

    function drawHist() {
      var g = api.low, rel = api.step() >= 3, tot = st.k, sdN = SIG / Math.sqrt(st.n);
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
      }
      for (x = 0; x <= XMAX; x += 10) {
        svg("line", { x1: px(x), x2: px(x), y1: PB, y2: PB + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: px(x), y: PB + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(x));
      }
      svg("text", { x: (PL + PR) / 2, y: PB + 38, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "X̄, średni czas dojazdu w grupce (min)");
      yTitle(g, 16, PT, PB, rel ? "skala gęstości" : "liczba grupek");
      // jeden odczyt pod wykresem; linia przerywana na wykresie to μ
      var s = sd(), items = [];
      if (rel) items.push({ mark: markDash(g), text: "μ = " + fmt(MU, 0) + " min" });
      items.push({ text: "SD(X̄) = " + (s === null ? "—" : fmt(s, 1) + " min") });
      items.push({ text: "grupek: " + tot });
      readout(g, PB + 62, items);
    }

    function render() {
      api.low.textContent = "";
      drawStage(st.last, st.last ? st.last.vals.length : 0);
      if (api.step() >= 2) drawHist(); else drawLog();
    }

    return {
      render: render,
      reset: function () { fresh(N0); render(); },
      opt: function (name, v) { if (name === "n") { fresh(Number(v)); render(); } },
      go: function (done) {
        var o = one(st.n), n = st.n;
        tween(n <= 5 ? 900 : 1200, function (u) { drawStage(o, Math.floor(n * u)); }, function () {
          var j = binOf(o.mean);
          var land = function () { add(o); commit(o); render(); done(); };
          if (api.step() >= 2 && yfun) {
            o.no = st.k + 1; drawStage(o, n);
            var h = api.step() >= 3 ? (st.counts[j] + 1) / ((st.k + 1) * BW) : st.counts[j] + 1;
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
  // SCRATCH: zdrapka z kiosku, X = bilans losu (wygrana minus cena)
  // =========================================================================
  KINDS.scratch = function (cfg, api) {
    var TK = cfg.tickets, K0 = cfg.ticket || "main", PRICE = cfg.price;
    var BL = 74, BR = 296, RL = 372, RR = 612, PT = 214, PB = 336, RX = 375, RY = 146;
    var st;
    function moments(t) {
      var e = 0, e2 = 0;
      t.prizes.forEach(function (p, i) { var x = p - PRICE; e += x * t.probs[i]; e2 += x * x * t.probs[i]; });
      return { e: e, sd: Math.sqrt(Math.max(0, e2 - e * e)) };
    }
    function fresh(key) {
      var t = TK[key];
      st = { key: key, t: t, m: moments(t), counts: new Array(t.prizes.length).fill(0), n: 0, sum: 0, means: [], last: null, log: [] };
    }
    fresh(K0);

    function one() {
      var u = Math.random(), acc = 0, t = st.t;
      var pick = function (i) { return { i: i, prize: t.prizes[i], x: t.prizes[i] - PRICE }; };
      for (var i = 0; i < t.prizes.length; i++) { acc += t.probs[i]; if (u < acc) return pick(i); }
      return pick(t.prizes.length - 1);
    }
    function add(o) { st.counts[o.i] += 1; st.n += 1; st.sum += o.x; st.means.push(st.sum / st.n); }
    function commit(o) { o.no = st.n; st.last = o; st.log.unshift(o); if (st.log.length > 6) st.log.pop(); }
    function zl(x) { return (Math.round(x) === x ? String(x) : fmt(x, 2)) + " zł"; }

    function drawStage(o, u) {
      var g = api.stage, t = st.t;
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
        svg("text", { x: fx + fw / 2, y: fy + 33, "text-anchor": "middle", class: "lc-sc-prize" + (o.prize > 0 ? " is-hit" : "") }, g,
          zl(o.prize));
      }
      var sc = o ? (u === undefined ? 1 : u) : 0;
      if (sc < 1) svg("rect", { x: fx + fw * sc, y: fy, width: fw * (1 - sc), height: fh, rx: 4, class: "lc-sc-silver" }, g);
      if (sc < 1 && !o) svg("text", { x: fx + fw / 2, y: fy + 30, "text-anchor": "middle", class: "lc-sc-silver-t" }, g, "zdrap tutaj");
      svg("text", { x: x0 + 123, y: y0 + 96, "text-anchor": "middle", class: "lc-sc-sub" }, g, t.foot);
      if (!o) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g, "Los czeka na zdrapanie");
      } else if (u !== undefined && u < 1) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g, "los nr " + (st.n + 1) + "…");
      } else {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read" }, g, "X = " + zl(o.x));
      }
    }

    function drawLog() {
      var g = api.low, y0 = 206;
      if (!st.log.length) { emptyNote(g, y0 + 60, "Kup los i zdrap go."); return; }
      st.log.forEach(function (o, ri) {
        var q = svg("g", { opacity: 1 - ri * 0.13 }, g), y = y0 + ri * 28;
        svg("text", { x: 180, y: y, class: "lc-sc-log" }, q, "los " + o.no);
        svg("text", { x: 300, y: y, class: "lc-sc-log" }, q, "→");
        svg("text", { x: 330, y: y, class: "lc-sc-log is-x" + (o.x > 0 ? " is-hit" : "") }, q,
          "X = " + zl(o.prize) + " - " + zl(PRICE) + " = " + zl(o.x));
      });
    }

    function drawLow() {
      var g = api.low, rel = api.step() >= 3, t = st.t, nb = t.prizes.length, n = st.n, m = st.m;
      // lewy: wygrane
      var vals = st.counts.map(function (c) { return rel ? (n ? c / n : 0) : c; });
      var top = Math.max.apply(null, vals);
      if (rel) top = Math.max(top, Math.max.apply(null, t.probs));
      var ax = rel ? niceAxis(Math.max(top * 1.08, 0.1), 4) : niceAxis(Math.max(top * 1.12, 5), 4);
      var y = yGrid(g, BL, BR, PT, PB, ax), slot = (BR - BL) / nb, bw = Math.min(46, slot * 0.6);
      vals.forEach(function (v, i) {
        var cx = BL + slot * (i + 0.5);
        if (v > 0) svg("rect", { x: cx - bw / 2, y: y(v), width: bw, height: PB - y(v), class: "lc-sc-bar" + (t.prizes[i] > PRICE ? " is-hit" : "") }, g);
        if (!rel && v > 0) svg("text", { x: cx, y: y(v) - 5, "text-anchor": "middle", class: "lc-sc-val" }, g, String(v));
        if (rel) svg("circle", { cx: cx, cy: y(t.probs[i]), r: 5.5, class: "lc-sc-theory" }, g);
        svg("text", { x: cx, y: PB + 18, "text-anchor": "middle", class: "lc-sc-tick is-x" }, g, zl(t.prizes[i] - PRICE));
      });
      svg("text", { x: (BL + BR) / 2, y: PB + 40, "text-anchor": "middle", class: "lc-sc-axtitle" }, g, "X, bilans losu (wygrana - cena)");
      yTitle(g, 16, PT, PB, rel ? "częstość względna" : "liczba losów");

      // prawy: bieżąca średnia bilansu; oś obejmuje wartości ujemne
      var means = st.means, k0 = Math.min(10, Math.max(0, n - 1));
      var lo = Math.min(-PRICE - 1, m.e * 1.5), hi = Math.max(PRICE, m.e + 1);
      for (var j = k0; j < n; j++) { if (means[j] > hi) hi = means[j]; if (means[j] < lo) lo = means[j]; }
      var ax2 = niceAxis((hi - lo) * 1.05, 4);
      lo = ax2.step * Math.floor(lo / ax2.step - 1e-9); hi = ax2.step * Math.ceil(hi / ax2.step + 1e-9);
      var y2 = function (v) { return PB - (PB - PT) * (Math.max(lo, Math.min(hi, v)) - lo) / (hi - lo); };
      for (var tk = lo; tk <= hi + ax2.step * 1e-6; tk += ax2.step) {
        svg("line", { x1: RL, x2: RR, y1: y2(tk), y2: y2(tk), class: "lc-sc-grid" }, g);
        svg("text", { x: RL - 8, y: y2(tk) + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, fmt(tk, decs(ax2.step)));
      }
      svg("line", { x1: RL, x2: RR, y1: PB, y2: PB, class: "lc-sc-axis" }, g);
      svg("line", { x1: RL, x2: RR, y1: y2(0), y2: y2(0), class: "lc-sc-price" }, g);
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
      }
      svg("text", { x: RL, y: PB + 18, "text-anchor": "middle", class: "lc-sc-tick" }, g, "1");
      svg("text", { x: RR, y: PB + 18, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(Math.max(1, n)));
      svg("text", { x: (RL + RR) / 2, y: PB + 40, "text-anchor": "middle", class: "lc-sc-axtitle" }, g, "liczba kupionych losów");
      svg("text", { x: RR, y: PT - 12, "text-anchor": "end", class: "lc-sc-n" }, g,
        "średni bilans: " + (n ? fmt(st.sum / n, 2) + " zł" : "—") + " · losów: " + n);
      if (rel) {
        // kółka na lewym wykresie to P(X = x), linia przerywana na prawym to E(X):
        // małe wzorce przy odczycie zamiast podpisów na wykresach
        readout(g, PB + 72, [
          { mark: function (x, yy) { svg("circle", { cx: x + 11, cy: yy, r: 5.5, class: "lc-sc-theory" }, g); }, text: "P(X = x)" },
          { mark: markDash(g), text: "E(X) = " + fmt(m.e, 2) + " zł" },
          { text: "Var(X) = " + fmt(m.sd * m.sd, 2) + " zł²" }
        ]);
      }
    }

    function render() {
      api.low.textContent = "";
      drawStage(st.last);
      if (api.step() >= 2) drawLow(); else drawLog();
    }

    return {
      render: render,
      reset: function () { fresh(K0); render(); },
      opt: function (name, v) { if (name === "ticket" && TK[v]) { fresh(v); render(); } },
      go: function (done) {
        var o = one();
        tween(900, function (u) { drawStage(o, u); }, function () {
          var land = function () { add(o); commit(o); render(); done(); };
          if (api.step() >= 2) {
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
