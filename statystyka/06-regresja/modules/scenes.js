// Sceny wykładu 06: konkretne doświadczenie → statystyka → rozkład wyników.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// Wzorzec silnika: statystyka/00-dane-i-populacja/modules/scenes.js.
// config.kind:
//   "rtm"   grupa pisze dwa kolokwia: najlepsi z pierwszego wypadają gorzej w drugim
//           (regresja do średniej; umiejętność + los dnia)
//   "slope" zbieramy grupę studentów (godziny nauki → wynik): b₁ zmienia się od próby do próby
//           (3 kroki: grupa z b₁ → powtarzamy → prawdziwe β₁)
// Numer kroku czyta z data-lc-step korzenia widgetu. Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji)
// Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H = 490;
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function fmt(x, d) { return (Math.abs(x) < 0.5 * Math.pow(10, -d) ? 0 : x).toFixed(d); }
  function sgn(x, d) { var s = fmt(x, d); return (x > 0 && Number(s) !== 0 ? "+" : "") + s; }
  function ease(u) { return u * u * (3 - 2 * u); }
  function mean(a) { var s = 0; for (var i = 0; i < a.length; i++) s += a[i]; return a.length ? s / a.length : 0; }
  function rnorm() {
    var u = 0, v = 0;
    while (u === 0) u = Math.random();
    while (v === 0) v = Math.random();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }
  function clamp(x, lo, hi) { return Math.max(lo, Math.min(hi, x)); }

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

  function axisX(g, x0, x1, y, ticks) {
    svg("line", { x1: x0, x2: x1, y1: y, y2: y, class: "lc-sc-axis" }, g);
    ticks.forEach(function (t) {
      svg("line", { x1: t.x, x2: t.x, y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
      svg("text", { x: t.x, y: y + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, t.label);
    });
  }

  // Jeden krótki odczyt pod/nad wykresem: [{glyph, cls, text}], glyph: "line" | "tri" | "dot" | brak.
  // Szerokości tekstów mierzy przeglądarka; całość wyrównana do x wg anchor (start | middle | end).
  function readout(g, x, y, items, anchor) {
    var GW = 20, GAP = 6, SEP = 22, box = svg("g", {}, g), cx = 0;
    items.forEach(function (it, i) {
      if (i > 0) { svg("text", { x: cx + SEP / 2, y: y, "text-anchor": "middle", class: "lc-sc-n" }, box, "·"); cx += SEP; }
      if (it.glyph === "line") svg("line", { x1: cx, x2: cx + GW, y1: y - 4, y2: y - 4, class: it.cls }, box);
      if (it.glyph === "tri") svg("path", { d: "M " + (cx + GW / 2) + " " + (y - 10) + " l -6 9 l 12 0 Z", class: it.cls }, box);
      if (it.glyph === "dot") svg("circle", { cx: cx + GW / 2, cy: y - 4, r: 5, class: it.cls }, box);
      if (it.glyph) cx += GW + GAP;
      var t = svg("text", { x: cx, y: y, class: "lc-sc-n" }, box, it.text);
      var w = 0;
      try { w = t.getComputedTextLength(); } catch (e) { w = 0; }
      cx += w || it.text.length * 7.8;
    });
    var dx = anchor === "middle" ? x - cx / 2 : anchor === "end" ? x - cx : x;
    box.setAttribute("transform", "translate(" + dx + " 0)");
  }

  // Histogram żetonów: counts[i] (wszystkie), hits[i] (wyróżnione, rysowane na dole stosu).
  function Hist(lo, hi, bw, PL, PR, PT, PB) {
    var nb = Math.round((hi - lo) / bw);
    var h = { lo: lo, hi: hi, bw: bw, nb: nb, PL: PL, PR: PR, PT: PT, PB: PB };
    h.reset = function () { h.counts = new Array(nb).fill(0); h.hits = new Array(nb).fill(0); };
    h.bin = function (v) { return clamp(Math.floor((v - lo) / bw + 1e-9), 0, nb - 1); };
    h.x = function (v) { return PL + (PR - PL) * (v - lo) / (hi - lo); };
    h.slot = function (i) { return PL + (PR - PL) * (i + 0.5) / nb; };
    h.info = function () {
      var mx = Math.max.apply(null, h.counts.concat([1]));
      var u = Math.min(13, (PB - PT - 14) / Math.max(mx, 6));
      return { u: u, r: Math.min(5.5, u * 0.46), dots: u >= 7 && (PR - PL) / nb >= 9 };
    };
    h.tokenY = function (c, inf) { return PB - inf.u * (c - 0.5) - 1; };
    h.add = function (v, hit) { var i = h.bin(v); h.counts[i] += 1; if (hit) h.hits[i] += 1; };
    // mark(i, c) → klasa żetonu c-tego od dołu w koszyku i
    h.draw = function (g, cls) {
      var inf = h.info();
      for (var i = 0; i < nb; i++) {
        var n = h.counts[i], k = h.hits[i];
        if (!n) continue;
        if (inf.dots) {
          for (var c = 1; c <= n; c++) {
            svg("circle", { cx: h.slot(i), cy: h.tokenY(c, inf), r: inf.r,
              class: "lc-sc-token" + (cls ? (c <= k ? cls[0] : cls[1]) : "") }, g);
          }
        } else {
          var bw = (PR - PL) / nb * 0.82;
          if (cls) {
            if (n - k > 0) svg("rect", { x: h.slot(i) - bw / 2, y: PB - inf.u * n, width: bw,
              height: inf.u * (n - k), class: "lc-sc-token is-bar" + cls[1] }, g);
            if (k > 0) svg("rect", { x: h.slot(i) - bw / 2, y: PB - inf.u * k, width: bw,
              height: inf.u * k, class: "lc-sc-token is-bar" + cls[0] }, g);
          } else {
            svg("rect", { x: h.slot(i) - bw / 2, y: PB - inf.u * n, width: bw,
              height: inf.u * n, class: "lc-sc-token is-bar" }, g);
          }
        }
      }
    };
    h.reset();
    return h;
  }

  // Żeton leci z (x0, y0) do wierzchołka stosu w histogramie.
  function flyToken(api, h, v, x0, y0, fast, done) {
    var inf = h.info(), i = h.bin(v);
    var x1 = h.slot(i), y1 = h.tokenY(h.counts[i] + 1, inf);
    var tok = svg("circle", { r: 6, cx: x0, cy: y0, class: "lc-sc-token" }, api.fly);
    tween(fast ? 140 : 450, function (u) {
      var e = ease(u);
      tok.setAttribute("cx", x0 + (x1 - x0) * e);
      tok.setAttribute("cy", y0 + (y1 - y0) * e);
    }, function () { api.fly.textContent = ""; done(); });
  }

  // Mała postać: głowa + tułów (klasa koloru w cls).
  function person(g, x, y, s, cls) {
    svg("circle", { cx: x, cy: y, r: 6 * s, class: "lc-sc-person " + (cls || "") }, g);
    svg("path", { d: "M " + (x - 10 * s) + " " + (y + 26 * s) + " Q " + (x - 10 * s) + " " + (y + 8 * s) + " " + x + " " + (y + 8 * s) +
      " Q " + (x + 10 * s) + " " + (y + 8 * s) + " " + (x + 10 * s) + " " + (y + 26 * s) + " Z",
      class: "lc-sc-person " + (cls || "") }, g);
  }
  // Kartka kolokwium.
  function paper(g, x, y) {
    svg("rect", { x: x, y: y, width: 13, height: 17, rx: 1.5, class: "lc-sc-paper" }, g);
    for (var i = 0; i < 3; i++) svg("line", { x1: x + 3, x2: x + 10, y1: y + 5 + i * 4, y2: y + 5 + i * 4, class: "lc-sc-paper-l" }, g);
  }

  var KINDS = {};

  // =========================================================================
  // RTM: dwa kolokwia, regresja do średniej
  // =========================================================================
  KINDS.rtm = function (cfg, api) {
    var N = cfg.n || 100, K = cfg.k || 10, MU = cfg.mu || 60, SK = cfg.sd_skill || 9;
    var AX0 = 130, AX1 = 612;
    var R1 = 84, RS = 150, R2 = 230, BAND = 38, AXY = 270;
    var hist = Hist(-30, 30, 1, 60, 612, 342, 440);
    var st = { luck: Number(cfg.luck || 7), grp: cfg.grp || "top", groups: [], last: null };

    function X(v) { return AX0 + (AX1 - AX0) * v / 100; }
    function score(s) { return Math.round(clamp(s + st.luck * rnorm(), 0, 100)); }

    function make() {
      var skill = [], s1 = [], s2 = [], j1 = [], j2 = [];
      for (var i = 0; i < N; i++) {
        var sk = MU + SK * rnorm();
        skill.push(sk); s1.push(score(sk)); s2.push(score(sk));
        j1.push(Math.random()); j2.push(Math.random());
      }
      var ord = s1.map(function (_, i) { return i; }).sort(function (a, b) {
        return s1[a] - s1[b] || j1[a] - j1[b];
      });
      var sel = st.grp === "top" ? ord.slice(N - K) : ord.slice(0, K);
      var pick = function (a) { return sel.map(function (i) { return a[i]; }); };
      var g = { skill: skill, s1: s1, s2: s2, j1: j1, j2: j2, sel: sel, grp: st.grp,
        m1: mean(pick(s1)), m2: mean(pick(s2)), msk: mean(pick(skill)),
        a1: mean(s1), a2: mean(s2), max: Math.max.apply(null, s1), min: Math.min.apply(null, s1) };
      g.isSel = new Array(N).fill(false);
      sel.forEach(function (i) { g.isSel[i] = true; });
      g.d = g.m2 - g.m1;
      return g;
    }

    function meanMark(g, x, yTop, cls) {
      svg("path", { d: "M " + x + " " + yTop + " l -6 9 l 12 0 Z", class: "lc-sc-tri " + (cls || "") }, g);
    }

    function drawStage(g, step, anim) {
      // osie i etykiety rzędów
      svg("line", { x1: AX0, x2: AX1, y1: R1, y2: R1, class: "lc-sc-grid" }, g);
      paper(g, 16, R1 - 30);
      svg("text", { x: 36, y: R1 - 16, class: "lc-sc-row" }, g, "1. kolokwium");
      if (step >= 3) {
        svg("line", { x1: AX0, x2: AX1, y1: R2, y2: R2, class: "lc-sc-grid" }, g);
        paper(g, 16, R2 - 30);
        svg("text", { x: 36, y: R2 - 16, class: "lc-sc-row" }, g, "2. kolokwium");
      }
      if (step >= 4) {
        svg("text", { x: 16, y: RS + 4, class: "lc-sc-row is-truth" }, g, "umiejętność");
      }
      var ticks = [];
      for (var t = 0; t <= 100; t += 20) ticks.push({ x: X(t), label: String(t) });
      axisX(g, AX0, AX1, AXY, ticks);
      svg("text", { x: AX1, y: AXY + 36, "text-anchor": "end", class: "lc-sc-axtitle" }, g, "punkty");

      var d = st.last;
      if (!d) {
        svg("text", { x: (AX0 + AX1) / 2, y: R1 - 12, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Grupa " + N + " studentów czeka na kolokwium.");
        return;
      }
      var u1 = anim ? anim.u1 : 1, u2 = anim ? anim.u2 : 1, hl = step >= 2 && (!anim || anim.hl);
      var shown = Math.ceil(u1 * N);
      // średnia całej grupy
      if (!anim || anim.done) {
        svg("line", { x1: X(d.a1), x2: X(d.a1), y1: R1 - BAND - 6, y2: R1, class: "lc-sc-avgall" }, g);
      }
      // 1. kolokwium
      for (var i = 0; i < shown; i++) {
        var on = hl && d.isSel[i];
        svg("circle", { cx: X(d.s1[i]), cy: R1 - 4 - d.j1[i] * (BAND - 8), r: on ? 4.6 : 3.8,
          class: "lc-sc-st" + (on ? " is-sel" : hl ? " is-dim" : "") }, g);
      }
      // ukryta umiejętność
      if (step >= 4 && (!anim || anim.done)) {
        svg("line", { x1: X(d.msk), x2: X(d.msk), y1: R1 - BAND, y2: R2 + 4, class: "lc-sc-param" }, g);
        d.sel.forEach(function (i) {
          svg("circle", { cx: X(d.skill[i]), cy: RS - 6 + d.j2[i] * 10, r: 4, class: "lc-sc-skill" }, g);
        });
      }
      // 2. kolokwium
      if (step >= 3 && u2 > 0) {
        var shown2 = Math.ceil(u2 * N);
        for (var j = 0; j < shown2; j++) {
          if (d.isSel[j]) continue;
          svg("circle", { cx: X(d.s2[j]), cy: R2 - 4 - d.j2[j] * (BAND - 8), r: 3.8, class: "lc-sc-st is-dim" }, g);
        }
        d.sel.forEach(function (i) {
          var x1 = X(d.s1[i]), y1 = R1 - 4 - d.j1[i] * (BAND - 8);
          var x2 = X(d.s2[i]), y2 = R2 - 4 - d.j2[i] * (BAND - 8);
          var e = ease(u2), xe = x1 + (x2 - x1) * e, ye = y1 + (y2 - y1) * e;
          svg("line", { x1: x1, y1: y1, x2: xe, y2: ye, class: "lc-sc-arrow" }, g);
          svg("circle", { cx: xe, cy: ye, r: 4.6, class: "lc-sc-st is-sel" }, g);
        });
        if (!anim || anim.done) {
          meanMark(g, X(d.m2), R2 + 3, "is-acc");
        }
      }
      if (hl && (!anim || anim.done || anim.u2 > 0)) {
        meanMark(g, X(d.m1), R1 + 3, "is-acc");
      }
      // jeden odczyt nad wykresem: średnia grupy, średnia wybranych (1. → 2.), ich umiejętność
      if (!anim || anim.done) {
        var items = [{ glyph: "line", cls: "lc-sc-avgall", text: "x̄ = " + fmt(d.a1, 1) }];
        if (step >= 2) items.push({ glyph: "tri", cls: "lc-sc-tri is-acc",
          text: "x̄₁₀ = " + fmt(d.m1, 1) + (step >= 3 ? " → " + fmt(d.m2, 1) : "") });
        if (step >= 4) items.push({ glyph: "line", cls: "lc-sc-param", text: "umiejętność = " + fmt(d.msk, 1) });
        readout(g, (AX0 + AX1) / 2, 18, items, "middle");
      }
    }

    function drawLog(g, step) {
      if (!st.groups.length) {
        svg("text", { x: W / 2, y: 360, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Napisz kolokwium, żeby zobaczyć wyniki grupy.");
        return;
      }
      st.groups.slice(-5).reverse().forEach(function (d, i) {
        var y = 340 + i * 26, g2 = svg("g", { opacity: 1 - i * 0.16 }, g);
        svg("text", { x: 60, y: y, class: "lc-sc-log" }, g2, "grupa " + d.no);
        if (step >= 2) {
          svg("text", { x: 170, y: y, class: "lc-sc-log" }, g2, "x̄ = " + fmt(d.a1, 1));
          svg("text", { x: 380, y: y, class: "lc-sc-log is-x" }, g2, "x̄₁₀ = " + fmt(d.m1, 1));
        } else {
          svg("text", { x: 170, y: y, class: "lc-sc-log" }, g2,
            "x̄ = " + fmt(d.a1, 1) + " · " + d.min + "–" + d.max + " pkt");
        }
      });
    }

    function drawHist(g, step) {
      var ticks = [-30, -20, -10, 0, 10, 20, 30].map(function (t) { return { x: hist.x(t), label: t > 0 ? "+" + t : String(t) }; });
      svg("line", { x1: hist.x(0), x2: hist.x(0), y1: hist.PT - 4, y2: hist.PB, class: "lc-sc-zero" }, g);
      hist.draw(g);
      axisX(g, hist.PL, hist.PR, hist.PB, ticks);
      svg("text", { x: (hist.PL + hist.PR) / 2, y: hist.PB + 36, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "zmiana x̄₁₀ na 2. kolokwium (pkt)");
      var info = "grup: " + st.groups.length;
      if (step >= 4 && st.groups.length) {
        info += " · średnio " + sgn(mean(st.groups.map(function (d) { return d.d; })), 1) + " pkt";
      }
      svg("text", { x: hist.PR, y: hist.PT - 14, "text-anchor": "end", class: "lc-sc-n" }, g, info);
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, null);
      if (step >= 3) drawHist(api.low, step); else drawLog(api.low, step);
    }

    function commit(d) {
      d.no = st.groups.length + 1;
      st.groups.push(d); st.last = d;
      hist.add(d.d);
    }

    function runOne(fast, done) {
      var d = make(), step = api.step();
      st.last = d;
      var draw = function (a) { api.stage.textContent = ""; drawStage(api.stage, step, a); };
      tween(fast ? 120 : 900, function (u) { draw({ u1: u, u2: 0, hl: false }); }, function () {
        tween(step >= 2 && !fast ? 350 : 0, function () { draw({ u1: 1, u2: 0, hl: true }); }, function () {
          tween(step >= 3 ? (fast ? 140 : 1000) : 0, function (u) {
            draw({ u1: 1, u2: step >= 3 ? u : 0, hl: true });
          }, function () {
            var land = function () { commit(d); render(); done(); };
            if (step >= 3) {
              draw({ u1: 1, u2: 1, hl: true, done: true });
              flyToken(api, hist, d.d, X(d.m2), R2 + 8, fast, land);
            } else land();
          });
        });
      });
    }

    return {
      render: render,
      reset: function () { st.groups = []; st.last = null; hist.reset(); render(); },
      opt: function (name, v) {
        if (name === "luck") st.luck = Number(v);
        if (name === "grp") st.grp = v;
        this.reset();
      },
      go: function (done) { runOne(false, done); },
      many: function (m, done) {
        if (m > 10) {
          for (var i = 0; i < m; i++) commit(make());
          render(); done(); return;
        }
        var left = m;
        (function next() { if (left-- <= 0) { done(); return; } runOne(true, next); })();
      }
    };
  };

  // =========================================================================
  // SLOPE: zbieramy grupę, nachylenie b₁ od próby do próby
  // =========================================================================
  KINDS.slope = function (cfg, api) {
    var B0 = cfg.beta0, B1 = cfg.beta1, SIG = cfg.sigma, XL = cfg.xmin, XH = cfg.xmax;
    var XMID = (XL + XH) / 2;
    var SX0 = 70, SX1 = 400, SY0 = 238, SY1 = 26, YMIN = 0, YMAX = 100, XMAXA = 13;
    var hist = Hist(-3, 6, 0.1, 60, 612, 348, 440);
    var st = { n: Number(cfg.n || 14), world: cfg.world || "yes", groups: [], last: null };

    function beta1() { return st.world === "yes" ? B1 : 0; }
    function beta0() { return st.world === "yes" ? B0 : B0 + B1 * XMID; }
    function PX(x) { return SX0 + (SX1 - SX0) * x / XMAXA; }
    function PY(y) { return SY0 + (SY1 - SY0) * (y - YMIN) / (YMAX - YMIN); }

    function make() {
      var n = st.n, x = [], y = [];
      for (var i = 0; i < n; i++) {
        var xi = Math.round((XL + (XH - XL) * Math.random()) * 2) / 2;
        x.push(xi);
        y.push(Math.round(clamp(beta0() + beta1() * xi + SIG * rnorm(), 0, 100)));
      }
      var mx = mean(x), my = mean(y), sxy = 0, sxx = 0, ssr = 0;
      for (var j = 0; j < n; j++) { sxy += (x[j] - mx) * (y[j] - my); sxx += (x[j] - mx) * (x[j] - mx); }
      var b1 = sxx > 0 ? sxy / sxx : 0, b0 = my - b1 * mx;
      for (var k = 0; k < n; k++) { var e = y[k] - b0 - b1 * x[k]; ssr += e * e; }
      var se = sxx > 0 ? Math.sqrt(ssr / (n - 2) / sxx) : Infinity;
      var crit = (cfg.crit && cfg.crit[String(n)]) || 2;
      return { x: x, y: y, b0: b0, b1: b1, n: n, my: my,
        xl: Math.min.apply(null, x), xh: Math.max.apply(null, x),
        sig: se > 0 && Math.abs(b1 / se) > crit, world: st.world };
    }

    function line(g, b0, b1, xa, xb, cls) {
      return svg("line", { x1: PX(xa), y1: PY(clamp(b0 + b1 * xa, YMIN, YMAX)), x2: PX(xb),
        y2: PY(clamp(b0 + b1 * xb, YMIN, YMAX)), class: cls }, g);
    }

    function drawStage(g, step, anim) {
      // ramka wykresu
      for (var yt = 20; yt <= 100; yt += 20) {
        svg("line", { x1: SX0, x2: SX1, y1: PY(yt), y2: PY(yt), class: "lc-sc-grid" }, g);
        svg("text", { x: SX0 - 8, y: PY(yt) + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, String(yt));
      }
      var ticks = [0, 2, 4, 6, 8, 10, 12].map(function (t) { return { x: PX(t), label: String(t) }; });
      axisX(g, SX0, SX1, SY0, ticks);
      svg("text", { x: (SX0 + SX1) / 2, y: SY0 + 36, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "godziny nauki w tygodniu");
      svg("text", { x: 16, y: SY1 - 10, class: "lc-sc-axtitle" }, g, "wynik egzaminu (pkt)");

      // ankieter z kartką
      person(g, 470, 40, 1.1, "");
      svg("rect", { x: 482, y: 50, width: 15, height: 20, rx: 2, class: "lc-sc-paper" }, g);

      // poprzednie proste w tle
      if (step >= 2) {
        st.groups.slice(-40).forEach(function (d) {
          if (d === st.last) return;
          line(g, d.b0, d.b1, d.xl, d.xh, "lc-sc-ghost");
        });
      }
      if (step >= 3) line(g, beta0(), beta1(), 0, XMAXA, "lc-sc-param");

      var d = st.last;
      if (!d) {
        svg("text", { x: (SX0 + SX1) / 2, y: PY(60), "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Zbierz grupę, żeby zobaczyć punkty.");
        return;
      }
      var shown = anim ? Math.ceil(anim.u1 * d.n) : d.n;
      var r = d.n > 30 ? 3.4 : 4.8;
      for (var i = 0; i < shown; i++) {
        svg("circle", { cx: PX(d.x[i]), cy: PY(d.y[i]), r: r, class: "lc-sc-pt" }, g);
      }
      var ul = anim ? anim.u2 : 1;
      if (ul > 0) {
        var xb = d.xl + (d.xh - d.xl) * ease(ul);
        line(g, d.b0, d.b1, d.xl, xb, "lc-sc-fit");
      }
      var px = 430;
      if (!anim || anim.done) {
        svg("text", { x: px, y: 140, class: "lc-sc-read" }, g, "b₁ = " + fmt(d.b1, 2));
      }
    }

    function drawLog(g, step) {
      if (!st.groups.length) {
        svg("text", { x: W / 2, y: 360, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Każda zebrana grupa zapisze się tutaj.");
        return;
      }
      st.groups.slice(-5).reverse().forEach(function (d, i) {
        var y = 340 + i * 26, g2 = svg("g", { opacity: 1 - i * 0.16 }, g);
        svg("text", { x: 60, y: y, class: "lc-sc-log" }, g2, "grupa " + d.no);
        svg("text", { x: 170, y: y, class: "lc-sc-log" }, g2, "n = " + d.n + " · ȳ = " + fmt(d.my, 1));
        svg("text", { x: 470, y: y, class: "lc-sc-log is-x" }, g2, "b₁ = " + fmt(d.b1, 2));
      });
    }

    function drawHist(g, step) {
      var ticks = [-2, 0, 2, 4, 6].map(function (t) { return { x: hist.x(t), label: String(t) }; });
      svg("line", { x1: hist.x(0), x2: hist.x(0), y1: hist.PT - 4, y2: hist.PB, class: "lc-sc-zero" }, g);
      if (step >= 3) {
        hist.draw(g, [" is-hit", " is-quiet"]);
        var xb = hist.x(beta1());
        svg("line", { x1: xb, x2: xb, y1: hist.PT - 6, y2: hist.PB, class: "lc-sc-param" }, g);
      } else hist.draw(g);
      axisX(g, hist.PL, hist.PR, hist.PB, ticks);
      svg("text", { x: (hist.PL + hist.PR) / 2, y: hist.PB + 36, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "nachylenie w grupie, b₁ (pkt za godzinę)");
      var N = st.groups.length, info = "grup: " + N;
      svg("text", { x: hist.PL, y: hist.PT - 22, class: "lc-sc-n" }, g, info);
      if (step >= 3) {
        // jeden odczyt: prawdziwe β₁ (linia przerywana) i odsetek grup z p < 0.05 (żetony w kolorze akcentu)
        var items = [{ glyph: "line", cls: "lc-sc-param", text: "β₁ = " + fmt(beta1(), 1) }];
        if (N > 0) {
          var hits = st.groups.filter(function (d) { return d.sig; }).length;
          items.push({ glyph: "dot", cls: "lc-sc-token", text: "p < 0.05: " + fmt(100 * hits / N, 1) + "%" });
        }
        readout(g, hist.PR, hist.PT - 22, items, "end");
      }
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, null);
      if (step >= 2) drawHist(api.low, step); else drawLog(api.low, step);
    }

    function commit(d) {
      d.no = st.groups.length + 1;
      st.groups.push(d); st.last = d;
      hist.add(d.b1, d.sig);
    }

    function runOne(fast, done) {
      var d = make(), step = api.step();
      st.last = d;
      var draw = function (a) { api.stage.textContent = ""; drawStage(api.stage, step, a); };
      tween(fast ? 120 : Math.min(1200, 300 + d.n * 25), function (u) { draw({ u1: u, u2: 0 }); }, function () {
        tween(fast ? 100 : 500, function (u) { draw({ u1: 1, u2: u }); }, function () {
          var land = function () { commit(d); render(); done(); };
          if (step >= 2) {
            draw({ u1: 1, u2: 1, done: true });
            flyToken(api, hist, d.b1, 470, 136, fast, land);
          } else land();
        });
      });
    }

    return {
      render: render,
      reset: function () { st.groups = []; st.last = null; hist.reset(); render(); },
      opt: function (name, v) {
        if (name === "n") st.n = Number(v);
        if (name === "world") st.world = v;
        this.reset();
      },
      go: function (done) { runOne(false, done); },
      many: function (m, done) {
        if (m > 10) {
          for (var i = 0; i < m; i++) commit(make());
          render(); done(); return;
        }
        var left = m;
        (function next() { if (left-- <= 0) { done(); return; } runOne(true, next); })();
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
    var s = svg("svg", { class: "lc-sc-svg", role: "img", viewBox: "0 0 " + W + " " + (cfg.height || H),
      "aria-label": cfg.aria || "Scena doświadczenia i wykres wyników" }, root);
    var api = {
      stage: svg("g", {}, s), low: svg("g", {}, s), fly: svg("g", {}, s),
      step: function () { return Number(stepper.getAttribute("data-lc-step")) || 1; }
    };
    var scene = KINDS[cfg.kind](cfg, api), busy = false;
    var labelBtn = stepper.querySelector("[data-sc-act='go']");
    var labels = labelBtn && labelBtn.getAttribute("data-labels")
      ? JSON.parse(labelBtn.getAttribute("data-labels")) : null;

    function refresh() {
      if (!busy) scene.render();
      stepper.querySelectorAll("[data-sc-act]").forEach(function (b) { b.disabled = busy; });
      if (labels && labelBtn) {
        var span = labelBtn.querySelector("span");
        if (span) span.textContent = labels[Math.min(labels.length, api.step()) - 1];
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
      if (e.target.closest('[data-lc-nav="reset"]')) setTimeout(function () { scene.reset(); refresh(); }, 0);
    });

    new MutationObserver(refresh).observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
  }

  function scan() { document.querySelectorAll(".lc-sc:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
