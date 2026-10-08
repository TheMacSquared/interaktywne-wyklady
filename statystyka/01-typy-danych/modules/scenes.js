// Sceny wykładu 01: konkretne doświadczenie → liczba → rozkład wyników.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind:
//   "bus"  czekanie na autobus linii A albo K: spóźnienie x, rozrzut i ryzyko spóźnienia
// Numer kroku czyta z data-lc-step korzenia widgetu. Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji, np. line:B)
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
  // BUS: czekanie na autobus linii A albo K
  // cfg.a, cfg.b: spóźnienia kursów (min) ze świata wykładu; losujemy z nich.
  // cfg.dep: odjazd wg rozkładu w minutach od północy; cfg.limit: zapas (min).
  // =========================================================================
  KINDS.bus = function (cfg, api) {
    var DEP = cfg.dep || 465, LIM = cfg.limit || 5, MEAN = cfg.mean || 2, XMAX = cfg.xmax || 25;
    var BW = 0.5, NB = Math.round(XMAX / BW);   // przedziały co pół minuty
    var PL = 82, PR = 614;
    var ROWS = { A: { top: 252, base: 326 }, B: { top: 336, base: 412 } };
    var POOL = { A: cfg.a, B: cfg.b };
    var STOP_X = 452;                    // przód autobusu przy przystanku
    var st = { line: "A", waits: [], counts: { A: new Array(NB).fill(0), B: new Array(NB).fill(0) },
      vals: { A: [], B: [] }, last: null };

    function clock(minutes) {
      var s = Math.round(minutes * 60), h = Math.floor(s / 3600), m = Math.floor(s / 60) % 60, sec = s % 60;
      return h + ":" + (m < 10 ? "0" : "") + m + ":" + (sec < 10 ? "0" : "") + sec;
    }
    function clockHM(minutes) { return clock(minutes).replace(/:\d\d$/, ""); }
    function binOf(x) { return Math.max(0, Math.min(NB - 1, Math.floor(x / BW))); }
    function xOf(v) { return PL + (PR - PL) * v / XMAX; }
    function slotX(i) { return xOf((i + 0.5) * BW); }
    function lineCls(l) { return l === "A" ? "is-a" : "is-b"; }
    // klucz B w kodzie, na ekranie linia K (jak we Wrocławiu)
    function lineName(l) { return l === "A" ? "A" : "K"; }

    // --- scena: zegar, droga, przystanek, student, autobus ------------------
    function drawBus(g, x, line) {
      var q = svg("g", { transform: "translate(" + x + ",0)" }, g);
      svg("rect", { x: -124, y: 118, width: 124, height: 50, rx: 9, class: "lc-sc-bus " + lineCls(line) }, q);
      [-114, -88, -62, -36].forEach(function (wx) {
        svg("rect", { x: wx, y: 126, width: 20, height: 16, rx: 2, class: "lc-sc-bus-win" }, q);
      });
      svg("rect", { x: -12, y: 126, width: 10, height: 26, rx: 2, class: "lc-sc-bus-win" }, q);
      svg("text", { x: -62, y: 162, "text-anchor": "middle", class: "lc-sc-bus-t" }, q, "linia " + lineName(line));
      svg("circle", { cx: -98, cy: 170, r: 9, class: "lc-sc-wheel" }, q);
      svg("circle", { cx: -26, cy: 170, r: 9, class: "lc-sc-wheel" }, q);
    }

    function drawStage(g, step, anim) {
      var line = st.line, w = anim ? anim.w : st.last;
      // zegar
      svg("rect", { x: 24, y: 16, width: 176, height: 74, rx: 8, class: "lc-sc-clock" }, g);
      svg("text", { x: 112, y: 36, "text-anchor": "middle", class: "lc-sc-sub" }, g,
        "rozkład " + clockHM(DEP));
      var now = w ? (anim ? DEP + w.x * anim.u : DEP + w.x) : DEP;
      svg("text", { x: 112, y: 72, "text-anchor": "middle", class: "lc-sc-clock-t" }, g, clock(now));
      // droga
      svg("rect", { x: 0, y: 180, width: W, height: 26, class: "lc-sc-road" }, g);
      svg("line", { x1: 0, x2: W, y1: 193, y2: 193, class: "lc-sc-road-mark" }, g);
      // przystanek
      svg("line", { x1: 470, x2: 470, y1: 74, y2: 180, class: "lc-sc-pole" }, g);
      svg("rect", { x: 452, y: 60, width: 36, height: 30, rx: 4, class: "lc-sc-stop " + lineCls(line) }, g);
      svg("text", { x: 470, y: 82, "text-anchor": "middle", class: "lc-sc-stop-t" }, g, lineName(line));
      // student
      var px = 540;
      svg("circle", { cx: px, cy: 128, r: 10, class: "lc-sc-person" }, g);
      svg("rect", { x: px - 12, y: 140, width: 24, height: 38, rx: 9, class: "lc-sc-person" }, g);
      // dymek tylko przy spóźnieniu ponad zapas
      if (w && (!anim || anim.u >= 1) && w.x > LIM) {
        svg("rect", { x: 488, y: 18, width: 146, height: 34, rx: 9, class: "lc-sc-bubble" }, g);
        svg("path", { d: "M 528 52 L 536 64 L 544 52 Z", class: "lc-sc-bubble" }, g);
        svg("text", { x: 561, y: 40, "text-anchor": "middle", class: "lc-sc-bubble-t is-late" }, g, "Nie zdążę!");
      }
      // autobus
      if (w) {
        var bx = anim ? -10 + (STOP_X + 10) * ease(Math.min(1, anim.u * 1.02)) : STOP_X;
        drawBus(g, bx, w.line);
        if (!anim || anim.u >= 1) {
          svg("text", { x: STOP_X - 80, y: 106, "text-anchor": "middle", class: "lc-sc-read" }, g,
            "x = " + fmt(w.x, 1) + " min");
        }
      }
    }

    // --- dziennik (krok 1) -------------------------------------------------
    function drawLog(g) {
      st.waits.slice(-6).reverse().forEach(function (d, i) {
        var y = 250 + i * 25, g2 = svg("g", { opacity: 1 - i * 0.14 }, g);
        svg("text", { x: 130, y: y, "text-anchor": "end", class: "lc-sc-log" }, g2, d.no + ".");
        svg("text", { x: 170, y: y, class: "lc-sc-log" }, g2, lineName(d.line));
        svg("text", { x: 230, y: y, class: "lc-sc-log" }, g2, clock(DEP + d.x));
        svg("text", { x: 380, y: y, class: "lc-sc-log is-x" }, g2, "x = " + fmt(d.x, 1) + " min");
      });
    }

    // --- dwa histogramy na wspólnej osi X, każdy z własną skalą Y (kroki 2–3) ---
    function histInfo(line) {
      var mx = Math.max.apply(null, st.counts[line].concat([1]));
      var u = Math.min(13, 72 / Math.max(mx, 5));
      return { u: u, tokR: Math.min(5, u * 0.46) };
    }
    function tokenY(line, count, h) { return ROWS[line].base - h.u * (count - 0.5) - 1; }

    function stats(v) {
      var n = v.length, m = 0, s = 0, late = 0;
      v.forEach(function (x) { m += x; if (x > LIM) late++; });
      m /= n;
      v.forEach(function (x) { s += (x - m) * (x - m); });
      return { n: n, mean: m, sd: n > 1 ? Math.sqrt(s / (n - 1)) : 0, late: late / n };
    }

    function drawHist(g, step) {
      var y0 = ROWS.A.top - 8, y1 = ROWS.B.base;
      if (step >= 3) {
        svg("rect", { x: xOf(LIM), y: y0, width: PR - xOf(LIM), height: y1 - y0, class: "lc-sc-risk" }, g);
      }
      ["A", "B"].forEach(function (l) {
        var R = ROWS[l], h = histInfo(l);
        svg("line", { x1: PL, x2: PR, y1: R.base, y2: R.base, class: "lc-sc-axis" }, g);
        svg("text", { x: 40, y: R.base - 34, "text-anchor": "middle", class: "lc-sc-row-t " + lineCls(l) }, g, lineName(l));
        svg("text", { x: 40, y: R.base - 14, "text-anchor": "middle", class: "lc-sc-n" }, g, "n = " + st.vals[l].length);
        var c = st.counts[l];
        for (var i = 0; i < NB; i++) {
          var hot = step >= 3 && i * BW >= LIM ? " is-late" : "";
          if (h.u >= 5) {
            for (var k = 1; k <= c[i]; k++) {
              svg("circle", { cx: slotX(i), cy: tokenY(l, k, h), r: h.tokR, class: "lc-sc-token " + lineCls(l) + hot }, g);
            }
          } else if (c[i] > 0) {
            var bw = (PR - PL) / NB * 0.8;
            var bh = Math.max(2, h.u * c[i]);   // pojedyncze kursy w ogonie też widać
            svg("rect", { x: slotX(i) - bw / 2, y: R.base - bh, width: bw, height: bh,
              class: "lc-sc-token is-bar " + lineCls(l) + hot }, g);
          }
        }
      });
      // oś wspólna pod B
      for (var v = 0; v <= XMAX; v += 5) {
        svg("line", { x1: xOf(v), x2: xOf(v), y1: y1, y2: y1 + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: xOf(v), y: y1 + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(v));
      }
      svg("text", { x: (PL + PR) / 2, y: y1 + 40, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "spóźnienie autobusu x (minuty)");
      if (step >= 3) {
        svg("line", { x1: xOf(MEAN), x2: xOf(MEAN), y1: y0, y2: y1, class: "lc-sc-param" }, g);
        svg("line", { x1: xOf(LIM), x2: xOf(LIM), y1: y0, y2: y1, class: "lc-sc-limit" }, g);
        // odczyt pod wykresem: wzorce linii zamiast podpisów na wykresie
        var ry = y1 + 66, rx = PL;
        svg("line", { x1: rx, x2: rx + 22, y1: ry - 5, y2: ry - 5, class: "lc-sc-param" }, g);
        svg("text", { x: rx + 30, y: ry, class: "lc-sc-n" }, g, "x̄ = " + fmt(MEAN, 1) + " min");
        svg("line", { x1: rx + 170, x2: rx + 192, y1: ry - 5, y2: ry - 5, class: "lc-sc-limit" }, g);
        svg("text", { x: rx + 200, y: ry, class: "lc-sc-n" }, g, LIM + " min");
        var ry2 = ry + 22;
        ["A", "B"].forEach(function (l, j) {
          var t = svg("text", { x: rx + j * 270, y: ry2, class: "lc-sc-n" }, g);
          svg("tspan", { class: "lc-sc-row-t " + lineCls(l) }, t, lineName(l) + ": ");
          if (st.vals[l].length) {
            var S = stats(st.vals[l]);
            svg("tspan", {}, t, "SD = " + fmt(S.sd, 1) + " min · ");
            svg("tspan", { class: "is-hit" }, t, "> " + LIM + " min: " + fmt(S.late * 100, 1) + "%");
          } else svg("tspan", {}, t, "—");
        });
      }
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, null);
      if (step >= 2) drawHist(api.low, step); else drawLog(api.low);
    }

    function draw() {
      var pool = POOL[st.line];
      return { line: st.line, x: pool[Math.floor(Math.random() * pool.length)] };
    }
    function commit(w) {
      w.no = st.waits.length + 1;
      st.waits.push(w);
      if (st.waits.length > 50) st.waits.shift();
      st.vals[w.line].push(w.x);
      st.last = w;
    }

    function runOne(fast, done) {
      var w = draw(), step = api.step();
      var total = fast ? 160 : Math.min(2600, 600 + 90 * w.x);
      tween(total, function (u) {
        api.stage.textContent = "";
        drawStage(api.stage, step, { w: w, u: u });
      }, function () {
        var land = function () { st.counts[w.line][binOf(w.x)] += 1; render(); done(); };
        if (step >= 2) {
          var h = histInfo(w.line), i = binOf(w.x);
          var x0 = STOP_X - 80, yy0 = 100, x1 = slotX(i), yy1 = tokenY(w.line, st.counts[w.line][i] + 1, h);
          commit(w); render();
          var tok = svg("circle", { r: 7, cx: x0, cy: yy0, class: "lc-sc-token " + lineCls(w.line) }, api.fly);
          tween(fast ? 140 : 450, function (u) {
            var e = ease(u);
            tok.setAttribute("cx", x0 + (x1 - x0) * e);
            tok.setAttribute("cy", yy0 + (yy1 - yy0) * e);
          }, function () { api.fly.textContent = ""; land(); });
        } else { commit(w); land(); }
      });
    }

    function runMany(m) {
      for (var i = 0; i < m; i++) {
        var w = draw(); commit(w); st.counts[w.line][binOf(w.x)] += 1;
      }
      render();
    }

    return {
      render: render,
      reset: function () {
        st.waits = []; st.last = null;
        st.counts = { A: new Array(NB).fill(0), B: new Array(NB).fill(0) };
        st.vals = { A: [], B: [] };
        render();
      },
      // Przełącznik wybiera przystanek; zebrane czekania obu linii zostają,
      // bo kroki 2–3 porównują dwa histogramy.
      opt: function (name, v) { if (name === "line") { st.line = v; st.last = null; render(); } },
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
