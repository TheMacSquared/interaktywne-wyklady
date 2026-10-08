// Sceny wykładu 03: konkretne doświadczenie → przedział → częstość trafień metody.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind:
//   "net"  student z miarką mierzy grupkę wychodzącą z sali; config.mode wybiera scenę:
//          "mean" (x̄ na stos żetonów, μ i SD(x̄)), "net" (siatka x̄ ± margines, stos siatek,
//          μ i pokrycie)
// Numer kroku czyta z data-lc-step korzenia widgetu. Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji, np. n:25)
// Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
// Silnik (init, tween, svg) skopiowany ze statystyki 00 (modules/scenes.js).
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H = 420;
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function fmt(x, d) { return x.toFixed(d); }
  function ease(u) { return u * u * (3 - 2 * u); }
  function gauss() {
    var u = 0, v = 0;
    while (u === 0) u = Math.random();
    while (v === 0) v = Math.random();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }

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

  var KINDS = {};

  // =========================================================================
  // NET: grupka z sali, x̄ na osi wzrostu, siatka x̄ ± margines, stos, odsłonięcie μ.
  // Jedna historia w trzech scenach; cfg.mode mówi, od którego kroku widać
  // siatkę (net), stos (stack) i μ (reveal):
  //   "mean"  Zmierz grupkę (rozdz. 1): bez siatki, x̄ spadają żetonami na stos; μ i SD(x̄)
  //   "net"   Zarzuć siatkę (rozdz. 2): siatka od kroku 1, potem stos siatek; μ i pokrycie
  // =========================================================================
  var MODES = {
    mean: { net: 99, stack: 2, reveal: 3 },
    net:  { net: 1,  stack: 2, reveal: 3 }
  };

  KINDS.net = function (cfg, api) {
    var M = MODES[cfg.mode || "net"];
    var MU = cfg.mu, SIGMA = cfg.sigma, TQ = cfg.tq || {}, ZQ = cfg.z || 1.96;
    var XMIN = cfg.xmin || 140, XMAX = cfg.xmax || 200;
    var PL = 50, PR = 610;                 // oś wzrostu
    var BASE = 132;                        // linia stóp grupki
    var NETY = 174;                        // środek siatki nad osią
    var DOTY = 206, AXY = 216;             // pomiary i oś
    var STK = 284, ROWH = 6.4, ROWS = 36;  // stos siatek pod osią
    var SBASE = (cfg.height || H) - 46, STOP = AXY + 40;   // stos żetonów x̄ (tryb mean)
    var BIN = 0.5, TOK = 4.6;                              // szerokość przegródki (cm), żeton (px)
    var st = { n: cfg.n || 25, mult: cfg.mult || "t", draws: [], hits: 0, last: null,
               bins: {}, sum: 0, sq: 0 };

    function X(v) { return PL + (PR - PL) * (Math.max(XMIN, Math.min(XMAX, v)) - XMIN) / (XMAX - XMIN); }
    function k() { return st.mult === "z" ? ZQ : Number(TQ[String(st.n)] || ZQ); }

    // jedna grupka → {h:[…], xbar, s, me, lo, hi, hit}
    function draw(keep) {
      var n = st.n, sum = 0, sq = 0, h = keep ? [] : null;
      for (var i = 0; i < n; i++) {
        var v = MU + SIGMA * gauss();
        sum += v; sq += v * v;
        if (keep) h.push(v);
      }
      var xbar = sum / n, s = Math.sqrt(Math.max(0, (sq - n * xbar * xbar) / (n - 1)));
      var me = k() * s / Math.sqrt(n);
      return { h: h, xbar: xbar, s: s, me: me, lo: xbar - me, hi: xbar + me,
               hit: xbar - me <= MU && MU <= xbar + me, n: n };
    }

    // --- postaci --------------------------------------------------------------
    function person(g, x, base, hpx, w, cls) {
      var r = w * 0.36;
      svg("circle", { cx: x, cy: base - hpx + r, r: r, class: "lc-sc-man " + (cls || "") }, g);
      svg("rect", { x: x - w / 2, y: base - hpx + 2 * r + 1, width: w, height: Math.max(1, hpx - 2 * r - 1),
        rx: w * 0.3, class: "lc-sc-man " + (cls || "") }, g);
    }
    function crowd(n) {
      // pozycje i skala grupki zależnie od n
      var x0 = 196, x1 = 620;
      if (n <= 5) {
        var sp = 64, start = (x0 + x1) / 2 - sp * (n - 1) / 2;
        return { w: 20, sc: 0.46, pos: function (i) { return [start + i * sp, BASE]; } };
      }
      if (n <= 25) {
        var sp2 = (x1 - x0) / n;
        return { w: Math.min(11, sp2 * 0.62), sc: 0.42, pos: function (i) { return [x0 + sp2 * (i + 0.5), BASE]; } };
      }
      var cols = Math.ceil(n / 2), sp3 = (x1 - x0) / cols;
      return { w: sp3 * 0.62, sc: 0.21, pos: function (i) {
        return [x0 + sp3 * ((i % cols) + 0.5), i < cols ? BASE - 48 : BASE];
      } };
    }

    function drawRoom(g) {
      svg("rect", { x: 22, y: 20, width: 82, height: 22, rx: 4, class: "lc-sc-sign" }, g);
      svg("text", { x: 63, y: 36, "text-anchor": "middle", class: "lc-sc-sign-t" }, g, "SALA");
      svg("rect", { x: 26, y: 48, width: 74, height: BASE - 48, rx: 3, class: "lc-sc-door" }, g);
      svg("rect", { x: 36, y: 58, width: 54, height: 30, rx: 2, class: "lc-sc-door-panel" }, g);
      svg("circle", { cx: 92, cy: 96, r: 3.5, class: "lc-sc-handle" }, g);
      // student z miarką
      person(g, 140, BASE, 78, 20, "is-actor");
      svg("rect", { x: 160, y: BASE - 84, width: 7, height: 84, class: "lc-sc-tape" }, g);
      for (var t = 0; t <= 8; t++) {
        svg("line", { x1: 160, x2: t % 2 ? 163 : 165, y1: BASE - t * 10.5, y2: BASE - t * 10.5, class: "lc-sc-tape-t" }, g);
      }
      svg("line", { x1: 18, x2: 624, y1: BASE + 0.5, y2: BASE + 0.5, class: "lc-sc-floor" }, g);
    }

    function drawAxis(g) {
      svg("line", { x1: PL, x2: PR, y1: AXY, y2: AXY, class: "lc-sc-axis" }, g);
      for (var v = XMIN; v <= XMAX; v += 10) {
        svg("line", { x1: X(v), x2: X(v), y1: AXY, y2: AXY + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: X(v), y: AXY + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(v));
      }
      svg("text", { x: PR, y: AXY + 38, "text-anchor": "end", class: "lc-sc-axtitle" }, g, "wzrost (cm)");
    }

    // siatka: oczka nad osią, pływaki na końcach
    function drawNet(g, d, half) {
      var cx = X(d.xbar), xl = cx - (cx - X(d.lo)) * half, xr = cx + (X(d.hi) - cx) * half;
      var q = svg("g", {}, g);
      svg("rect", { x: xl, y: NETY - 8, width: Math.max(0.5, xr - xl), height: 16, class: "lc-sc-net" }, q);
      for (var x = xl + 5; x < xr - 1; x += 5) {
        svg("line", { x1: x, x2: x, y1: NETY - 8, y2: NETY + 8, class: "lc-sc-mesh" }, q);
      }
      svg("line", { x1: xl, x2: xr, y1: NETY, y2: NETY, class: "lc-sc-mesh" }, q);
      svg("rect", { x: xl, y: NETY - 8, width: Math.max(0.5, xr - xl), height: 16, class: "lc-sc-net-edge" }, q);
      svg("circle", { cx: xl, cy: NETY, r: 4, class: "lc-sc-float" }, q);
      svg("circle", { cx: xr, cy: NETY, r: 4, class: "lc-sc-float" }, q);
    }

    function drawXbar(g, d) {
      svg("line", { x1: X(d.xbar), x2: X(d.xbar), y1: DOTY - 10, y2: AXY, class: "lc-sc-xbar" }, g);
      svg("text", { x: 620, y: 20, "text-anchor": "end", class: "lc-sc-read" }, g,
        "x̄ = " + fmt(d.xbar, 1) + " cm");
    }

    function drawStage(g, step, anim) {
      drawRoom(g);
      drawAxis(g);
      var d = st.last;
      if (!d) return;
      var C = crowd(d.n), upto = anim ? anim.upto : d.n;
      for (var i = 0; i < upto; i++) {
        var p = C.pos(i), hp = d.h[i] * C.sc;
        var op = anim && i === upto - 1 && anim.fresh < 1 ? anim.fresh : 1;
        var gg = svg("g", { opacity: op }, g);
        person(gg, p[0], p[1], hp, C.w, "is-crowd");
        svg("circle", { cx: X(d.h[i]), cy: DOTY, r: d.n > 25 ? 2.6 : 3.6, class: "lc-sc-hdot", opacity: op }, g);
      }
      if (anim && !anim.done) return;
      drawXbar(g, d);
      if (step >= M.net && !(anim && anim.noNet)) drawNet(g, d, anim && anim.half !== undefined ? anim.half : 1);
    }

    // jeden odczyt pod wykresem: po odsłonięciu wzorzec linii μ + wartości, wcześniej sam licznik
    function readout(g, y, step, rest, count) {
      if (step >= M.reveal) {
        svg("line", { x1: W / 2 - 190, x2: W / 2 - 168, y1: y - 5, y2: y - 5, class: "lc-sc-param" }, g);
        svg("text", { x: W / 2 - 160, y: y, class: "lc-sc-n" }, g, "μ = " + fmt(MU, 0) + " cm   ·   " + rest);
      } else if (st.draws.length) {
        svg("text", { x: W / 2, y: y, "text-anchor": "middle", class: "lc-sc-n" }, g, count);
      }
    }

    // stos siatek pod osią: najnowsza na górze
    function rowY(i) { return STK + i * ROWH; }
    function drawStack(g, step) {
      var N = st.draws.length, show = st.draws.slice(-ROWS).reverse();
      show.forEach(function (d, i) {
        var cls = step >= M.reveal ? (d.hit ? " is-hit" : " is-miss") : "";
        svg("line", { x1: X(d.lo), x2: X(d.hi), y1: rowY(i), y2: rowY(i), class: "lc-sc-ci" + cls }, g);
        svg("circle", { cx: X(d.xbar), cy: rowY(i), r: 1.8, class: "lc-sc-ci-dot" + cls }, g);
      });
      if (step >= M.reveal) {
        svg("line", { x1: X(MU), x2: X(MU), y1: NETY - 16, y2: rowY(ROWS - 1) + 6, class: "lc-sc-param" }, g);
      }
      var cov = N ? fmt(100 * st.hits / N, 1) + "% (" + st.hits + "/" + N + ")" : "—";
      readout(g, rowY(ROWS) + 22, step, "pokrycie = " + cov, "siatek: " + N);
    }

    // stos żetonów x̄ (tryb mean): przegródki po BIN cm; gdy żetony nie mieszczą się, słupki
    function binOf(v) { return Math.floor((Math.max(XMIN, Math.min(XMAX - 1e-9, v)) - XMIN) / BIN); }
    function binX(b) { return X(XMIN + (b + 0.5) * BIN); }
    function maxBin() { var m = 0; for (var b in st.bins) m = Math.max(m, st.bins[b]); return m; }
    function tokenY(b) {
      var c = st.bins[b] || 0, m = maxBin();
      if (m * TOK <= SBASE - STOP) return SBASE - TOK * (c - 0.5);
      return SBASE - (SBASE - STOP) * c / m;
    }
    function drawTokens(g, step) {
      var m = maxBin(), N = st.draws.length, dots = m * TOK <= SBASE - STOP;
      svg("line", { x1: PL, x2: PR, y1: SBASE + 0.5, y2: SBASE + 0.5, class: "lc-sc-axis" }, g);
      Object.keys(st.bins).forEach(function (b) {
        var c = st.bins[b];
        b = Number(b);
        if (dots) {
          for (var j = 0; j < c; j++) svg("circle", { cx: binX(b), cy: SBASE - TOK * (j + 0.5), r: TOK / 2 - 0.3, class: "lc-sc-tok" }, g);
        } else {
          var h = (SBASE - STOP) * c / m, x0 = X(XMIN + b * BIN);
          svg("rect", { x: x0 + 0.4, y: SBASE - h, width: Math.max(0.5, X(XMIN + (b + 1) * BIN) - x0 - 0.8), height: h, class: "lc-sc-tok" }, g);
        }
      });
      if (step >= M.reveal) {
        svg("line", { x1: X(MU), x2: X(MU), y1: DOTY - 14, y2: SBASE, class: "lc-sc-param" }, g);
      }
      var sd = N > 1 ? fmt(Math.sqrt(Math.max(0, (st.sq - N * Math.pow(st.sum / N, 2)) / (N - 1))), 2) + " cm" : "—";
      readout(g, SBASE + 30, step, "SD(x̄) = " + sd + "   ·   grup: " + N, "grup: " + N);
    }

    function drawLow(g, step) {
      if (step >= M.stack) { if (cfg.mode === "mean") drawTokens(g, step); else drawStack(g, step); }
      else drawLog(g, step);
    }

    function drawLog(g, step) {
      if (!st.draws.length) return;
      st.draws.slice(-6).reverse().forEach(function (d, i) {
        var y = STK + 10 + i * 26, g2 = svg("g", { opacity: 1 - i * 0.14 }, g);
        svg("text", { x: 90, y: y, class: "lc-sc-log" }, g2, "grupka " + d.no);
        svg("text", { x: 210, y: y, class: "lc-sc-log is-x" }, g2, "x̄ = " + fmt(d.xbar, 1) + " cm");
        if (step >= M.net) svg("text", { x: 390, y: y, class: "lc-sc-log" }, g2,
          fmt(d.lo, 1) + " – " + fmt(d.hi, 1) + " cm");
      });
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, null);
      drawLow(api.low, step);
    }

    function commit(d) {
      d.no = st.draws.length + 1;
      st.draws.push(d);
      if (d.hit) st.hits += 1;
      var b = binOf(d.xbar);
      st.bins[b] = (st.bins[b] || 0) + 1;
      st.sum += d.xbar; st.sq += d.xbar * d.xbar;
    }

    // żeton x̄ albo siatka spada z osi na stos
    function fall(d, fast, done) {
      var g = api.fly, mean = cfg.mode === "mean", b = binOf(d.xbar);
      var y0 = mean ? AXY : NETY, y1;
      if (mean) { st.bins[b] = (st.bins[b] || 0) + 1; y1 = tokenY(b); st.bins[b] -= 1; }
      else y1 = rowY(0);
      tween(fast ? 140 : 450, function (u) {
        var y = y0 + (y1 - y0) * ease(u);
        g.textContent = "";
        if (mean) svg("circle", { cx: X(d.xbar), cy: y, r: TOK / 2 + 0.6, class: "lc-sc-tok is-fly" }, g);
        else svg("line", { x1: X(d.lo), x2: X(d.hi), y1: y, y2: y, class: "lc-sc-ci is-fly" }, g);
      }, function () { g.textContent = ""; done(); });
    }

    function runOne(fast, done) {
      var d = draw(true), step = api.step();
      var total = fast ? 160 : Math.min(1400, 350 + d.n * 40);
      var finish = function () { commit(d); render(); done(); };
      st.last = d;
      api.low.textContent = "";
      drawLow(api.low, step);
      tween(total, function (u) {
        var x = u * d.n, upto = Math.max(1, Math.ceil(x));
        api.stage.textContent = "";
        drawStage(api.stage, step, { upto: upto, fresh: u >= 1 ? 1 : (x % 1 || 1), done: u >= 1, noNet: true });
      }, function () {
        var after = function () {
          if (step < M.stack) { finish(); return; }
          api.stage.textContent = "";
          drawStage(api.stage, step, { upto: d.n, fresh: 1, done: true, noNet: true });
          fall(d, fast, finish);
        };
        var pause = function () { if (step >= M.stack && !fast) setTimeout(after, REDUCE ? 0 : 350); else after(); };
        if (step < M.net) { pause(); return; }
        // siatka rozwija się od x̄ na boki
        tween(fast ? 120 : 520, function (u) {
          api.stage.textContent = "";
          drawStage(api.stage, step, { upto: d.n, fresh: 1, done: true, half: ease(u) });
        }, pause);
      });
    }

    function runMany(m) {
      var d;
      for (var i = 0; i < m - 1; i++) commit(draw(false));
      d = draw(true); commit(d); st.last = d;
      render();
    }

    return {
      render: render,
      reset: function () {
        st.draws = []; st.hits = 0; st.last = null; st.bins = {}; st.sum = 0; st.sq = 0;
        render();
      },
      opt: function (name, v) {
        if (name === "n") st.n = Number(v);
        if (name === "mult") st.mult = v;
        this.reset();
      },
      go: function (done) { runOne(false, done); },
      many: function (m, done) {
        if (m > 10) { runMany(m); done(); return; }
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
      scene.render();
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
