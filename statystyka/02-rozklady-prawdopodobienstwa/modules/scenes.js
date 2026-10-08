// Sceny wykładu 02 (PROTOTYPY 2026-10-08): doświadczenie z życia → zmienna → rozkład / model.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper). Wzorzec init/KINDS
// jak w statystyce 00 (scenes.js), obok istniejącego experiment.js (.lc-exp), z którym nie koliduje.
// config.kind:
//   "group"   ocena prowadzącego (1–10) od 5 losowych osób, X̄ grupki (rozkład średniej, CTG)
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
  // GROUP: pytamy 5 losowych osób o ocenę prowadzącego (1–10), X̄ = średnia ocena grupki
  // =========================================================================
  KINDS.group = function (cfg, api) {
    var VALS = cfg.vals, PROBS = cfg.probs, MU = cfg.mu, SIG = cfg.sigma, N = cfg.n || 5;
    var BW = 1 / N, LO = 0.5, HI = 10.5, X0 = VALS[0], NB = Math.round((VALS[VALS.length - 1] - X0) / BW) + 1;
    var PL = 74, PR = 616, PT = 214, PB = 350, RX = 375, RY = 146;
    var CUM = [], acc = 0;
    PROBS.forEach(function (p) { acc += p; CUM.push(acc); });
    var st, yfun = null;

    function fresh() { st = { counts: new Array(NB).fill(0), k: 0, s1: 0, s2: 0, last: null, log: [] }; }
    fresh();

    function rate() {
      var u = Math.random() * acc;
      for (var i = 0; i < CUM.length; i++) if (u < CUM[i]) return VALS[i];
      return VALS[VALS.length - 1];
    }
    function one() {
      var v = [], s = 0;
      for (var i = 0; i < N; i++) { var x = rate(); v.push(x); s += x; }
      return { vals: v, mean: s / N };
    }
    function binOf(x) { return Math.max(0, Math.min(NB - 1, Math.round((x - X0) / BW))); }
    function binX(j) { return X0 + j * BW; }
    function px(x) { return PL + (PR - PL) * (x - LO) / (HI - LO); }
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

    function drawStage(o, shown) {
      var g = api.stage;
      g.textContent = "";
      bubble(g, 6, 2, 206, "Jak oceniasz prowadzącego?");
      person(g, 46, 52, 1.2, "is-caller");
      var x0 = 240, x1 = 624, dx = (x1 - x0) / N;
      for (var i = 0; i < N; i++) {
        var cx = x0 + dx * (i + 0.5), known = o && i < shown;
        person(g, cx, 52, 1, known ? "" : "is-wait");
        if (known) svg("text", { x: cx, y: 36, "text-anchor": "middle", class: "lc-sc-ans" }, g, String(o.vals[i]));
      }
      if (o && shown >= N) {
        svg("text", { x: RX, y: RY, "text-anchor": "middle", class: "lc-sc-read" }, g, "X̄ = " + fmt(o.mean, 1));
      }
    }

    function drawLog() {
      var g = api.low, y0 = 206;
      st.log.forEach(function (o, ri) {
        var q = svg("g", { opacity: 1 - ri * 0.13 }, g), y = y0 + ri * 28, x = 220;
        svg("text", { x: 110, y: y, class: "lc-sc-log" }, q, "grupka " + o.no);
        o.vals.forEach(function (v) { svg("text", { x: x, y: y, "text-anchor": "end", class: "lc-sc-log" }, q, String(v)); x += 30; });
        svg("text", { x: x - 6, y: y, class: "lc-sc-log" }, q, "→");
        svg("text", { x: x + 18, y: y, class: "lc-sc-log is-x" }, q, "X̄ = " + fmt(o.mean, 1));
      });
    }

    function drawHist() {
      var g = api.low, rel = api.step() >= 3, tot = st.k, sdN = SIG / Math.sqrt(N);
      var vals = st.counts.map(function (c) { return rel ? (tot ? c / (tot * BW) : 0) : c; });
      var top = Math.max.apply(null, vals);
      if (rel) top = Math.max(top, normPdf(MU, MU, sdN), Math.max.apply(null, PROBS));
      var ax = rel ? niceAxis(top * 1.08, 4) : niceAxis(Math.max(top * 1.12, 5), 4);
      var y = yfun = yGrid(g, PL, PR, PT, PB, ax), i, x;
      var u = (PR - PL) / (HI - LO);
      if (rel) {
        // tło: rozkład pojedynczych ocen (słupki szerokości 1, wysokość = P)
        VALS.forEach(function (v, j) {
          svg("rect", { x: px(v - 0.5) + 1, y: y(PROBS[j]), width: u - 2, height: PB - y(PROBS[j]), class: "lc-sc-popfill" }, g);
        });
      }
      var bw = u * BW;
      vals.forEach(function (v, j) {
        if (v > 0) svg("rect", { x: px(binX(j)) - bw / 2 + 0.5, y: y(v), width: bw - 1, height: PB - y(v), class: "lc-sc-bar is-hit" }, g);
      });
      if (rel) {
        var pts = [];
        for (i = 0; i <= 300; i++) { x = LO + (HI - LO) * i / 300; pts.push(px(x) + "," + y(normPdf(x, MU, sdN))); }
        svg("polyline", { points: pts.join(" "), class: "lc-sc-curve", fill: "none" }, g);
        svg("line", { x1: px(MU), x2: px(MU), y1: PT, y2: PB, class: "lc-sc-param" }, g);
      }
      VALS.forEach(function (v) {
        svg("line", { x1: px(v), x2: px(v), y1: PB, y2: PB + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: px(v), y: PB + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(v));
      });
      svg("text", { x: (PL + PR) / 2, y: PB + 38, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "X̄, średnia ocena w grupce");
      yTitle(g, 16, PT, PB, rel ? "skala gęstości" : "liczba grupek");
      var s = sd(), items = [];
      if (rel) items.push({ mark: markDash(g), text: "μ = " + fmt(MU, 1) });
      items.push({ text: "SD(X̄) = " + (s === null ? "—" : fmt(s, 2)) });
      items.push({ text: "grupek: " + tot });
      readout(g, PB + 62, items);
    }

    function render() {
      api.low.textContent = "";
      drawStage(st.last, st.last ? N : 0);
      if (api.step() >= 2) drawHist(); else drawLog();
    }

    return {
      render: render,
      reset: function () { fresh(); render(); },
      opt: function () {},
      go: function (done) {
        var o = one();
        tween(900, function (uu) { drawStage(o, Math.floor(N * uu)); }, function () {
          var j = binOf(o.mean);
          var land = function () { add(o); commit(o); render(); done(); };
          if (api.step() >= 2 && yfun) {
            o.no = st.k + 1; drawStage(o, N);
            var h = api.step() >= 3 ? (st.counts[j] + 1) / ((st.k + 1) * BW) : st.counts[j] + 1;
            fly(api, RX, RY + 6, px(binX(j)), Math.max(PT, yfun(h)) - 8, 420, land);
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
