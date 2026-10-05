// Eksperyment → zmienna losowa → rozkład: rzut n kostkami, X = liczba szóstek.
// Kontener: .lc-exp[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// Numer kroku czyta z atrybutu data-lc-step korzenia widgetu, przyciski rzutu
// z [data-exp]. Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H = 386;
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
  function pmf(n, p, k) { return choose(n, k) * Math.pow(p, k) * Math.pow(1 - p, n - k); }
  function fmt(x, d) { return x.toFixed(d); }

  function niceAxis(max, target) {
    var raw = Math.max(max, 1e-9) / target;
    var mag = Math.pow(10, Math.floor(Math.log10(raw)));
    var step = [1, 2, 5, 10].map(function (m) { return m * mag; })
      .filter(function (s) { return s >= raw; })[0];
    return { step: step, max: step * Math.ceil(max / step) };
  }

  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    if (!stepper) return;
    root.dataset.ready = "1";

    var cfg = JSON.parse(root.dataset.config || "{}");
    var n = cfg.n || 6, faces = cfg.faces || 6, hit = cfg.hit || 6;
    var p = 1 / faces;
    var theory = [];
    for (var k = 0; k <= n; k++) theory.push(pmf(n, p, k));

    var state, busy = false;
    function reset() {
      state = { counts: new Array(n + 1).fill(0), total: 0, rolls: [], last: null };
    }
    reset();

    var s = svg("svg", { viewBox: "0 0 " + W + " " + H, class: "lc-exp-svg", role: "img",
      "aria-label": "Rzut " + n + " kostkami, liczba szóstek i histogram powtórzeń" }, root);
    var gDice = svg("g", {}, s), gLow = svg("g", {}, s), gBall = svg("g", {}, s);

    function step() { return Number(stepper.getAttribute("data-lc-step")) || 1; }
    function countHits(f) { return f.filter(function (v) { return v === hit; }).length; }

    // --- kostki -------------------------------------------------------------
    var D = 52, GAP = 16, X0 = (W - (n * D + (n - 1) * GAP)) / 2, Y0 = 8;
    function dieX(i) { return X0 + i * (D + GAP); }

    function drawDice(f, hi) {
      gDice.textContent = "";
      for (var i = 0; i < n; i++) {
        var v = f ? f[i] : null, on = hi && v === hit;
        var g = svg("g", { transform: "translate(" + dieX(i) + "," + Y0 + ")" }, gDice);
        svg("rect", { width: D, height: D, rx: 9, class: "lc-exp-die" + (on ? " is-hit" : "") }, g);
        if (v === null) {
          svg("text", { x: D / 2, y: D / 2 + 8, "text-anchor": "middle", class: "lc-exp-q" }, g, "?");
        } else {
          PIPS[v].forEach(function (c) {
            svg("circle", { cx: 11 + c[0] * 15, cy: 11 + c[1] * 15, r: 4.2,
              class: "lc-exp-pip" + (on ? " is-hit" : "") }, g);
          });
        }
      }
      var label = svg("text", { x: W / 2, y: Y0 + D + 30, "text-anchor": "middle",
        class: "lc-exp-read" }, gDice);
      if (state.last) {
        if (step() >= 2) {
          label.textContent = "X = " + countHits(state.last);
          var sub = svg("text", { x: W / 2, y: Y0 + D + 48, "text-anchor": "middle",
            class: "lc-exp-sub" }, gDice, "liczba szóstek w tym rzucie");
          void sub;
        } else {
          label.textContent = "rzut nr " + state.total;
          label.setAttribute("class", "lc-exp-read is-plain");
        }
      }
    }

    // --- dolna część: dziennik rzutów (kroki 1-2) albo histogram (3-4) ------
    function drawLog() {
      var st = step();
      var y = 142;
      if (!state.rolls.length) {
        svg("text", { x: W / 2, y: 200, "text-anchor": "middle", class: "lc-exp-sub" }, gLow,
          "Rzuć kostkami, żeby zobaczyć wynik doświadczenia.");
        return;
      }
      state.rolls.forEach(function (r, ri) {
        var g = svg("g", { opacity: 1 - ri * 0.13 }, gLow);
        svg("text", { x: 120, y: y + ri * 30, class: "lc-exp-log" }, g, "rzut " + r.no);
        r.faces.forEach(function (v, i) {
          svg("text", { x: 230 + i * 26, y: y + ri * 30,
            class: "lc-exp-log" + (st >= 2 && v === hit ? " is-hit" : "") }, g, String(v));
        });
        if (st >= 2) {
          svg("text", { x: 230 + n * 26 + 14, y: y + ri * 30, class: "lc-exp-log" }, g, "→");
          svg("text", { x: 230 + n * 26 + 44, y: y + ri * 30, class: "lc-exp-log is-x" }, g,
            "X = " + countHits(r.faces));
        }
      });
    }

    var PL = 74, PR = 616, PT = 138, PB = 312;
    function hist() {
      var st = step(), rel = st >= 4, tot = state.total;
      var vals = state.counts.map(function (c) { return rel ? (tot ? c / tot : 0) : c; });
      var top = Math.max.apply(null, vals);
      if (rel) top = Math.max(top, Math.max.apply(null, theory));
      var ax = rel ? niceAxis(Math.max(top * 1.08, 0.1), 4) : niceAxis(Math.max(top * 1.12, 5), 4);
      return { rel: rel, vals: vals, ax: ax, y: function (v) { return PB - (PB - PT) * v / ax.max; } };
    }
    function slotX(k) { return PL + (PR - PL) * (k + 0.5) / (n + 1); }

    function drawHist() {
      var h = hist(), bw = (PR - PL) / (n + 1) * 0.62;
      for (var t = 0; t <= h.ax.max + h.ax.step * 1e-6; t += h.ax.step) {
        svg("line", { x1: PL, x2: PR, y1: h.y(t), y2: h.y(t), class: "lc-exp-grid" }, gLow);
        svg("text", { x: PL - 8, y: h.y(t) + 4, "text-anchor": "end", class: "lc-exp-tick" }, gLow,
          h.rel ? fmt(t, 2) : String(Math.round(t)));
      }
      svg("line", { x1: PL, x2: PR, y1: PB, y2: PB, class: "lc-exp-axis" }, gLow);
      for (var k = 0; k <= n; k++) {
        var x = slotX(k), v = h.vals[k];
        if (v > 0) {
          svg("rect", { x: x - bw / 2, y: h.y(v), width: bw, height: PB - h.y(v), class: "lc-exp-bar" }, gLow);
          if (!h.rel) svg("text", { x: x, y: h.y(v) - 5, "text-anchor": "middle", class: "lc-exp-val" }, gLow, String(v));
        }
        svg("text", { x: x, y: PB + 18, "text-anchor": "middle", class: "lc-exp-tick is-x" }, gLow, String(k));
        if (h.rel) {
          svg("circle", { cx: x, cy: h.y(theory[k]), r: 5.5, class: "lc-exp-theory" }, gLow);
          svg("text", { x: x, y: PB + 36, "text-anchor": "middle", class: "lc-exp-prob" }, gLow, fmt(theory[k], 3));
        }
      }
      svg("text", { x: (PL + PR) / 2, y: PB + (h.rel ? 54 : 40), "text-anchor": "middle", class: "lc-exp-axtitle" },
        gLow, "X, czyli liczba szóstek");
      svg("text", { x: 16, y: (PT + PB) / 2, "text-anchor": "middle", class: "lc-exp-axtitle",
        transform: "rotate(-90 16 " + (PT + PB) / 2 + ")" }, gLow,
        h.rel ? "częstość względna" : "liczba rzutów");
      svg("text", { x: PR, y: PT - 10, "text-anchor": "end", class: "lc-exp-n" }, gLow,
        "n = " + state.total);
      if (h.rel) {
        svg("circle", { cx: PL + 6, cy: PT - 14, r: 5.5, class: "lc-exp-theory" }, gLow);
        svg("text", { x: PL + 18, y: PT - 10, class: "lc-exp-n" }, gLow, "P(X = k) z modelu");
      }
      return h;
    }

    function render() {
      gLow.textContent = "";
      drawDice(state.last, step() >= 2);
      if (step() >= 3) drawHist(); else drawLog();
      updateButtons();
    }

    function updateButtons() {
      stepper.querySelectorAll("[data-exp]").forEach(function (b) { b.disabled = busy; });
    }

    // --- rzut ---------------------------------------------------------------
    function randomFaces() {
      var f = [];
      for (var i = 0; i < n; i++) f.push(1 + Math.floor(Math.random() * faces));
      return f;
    }

    function commit(f) {
      state.total += 1;
      state.last = f;
      state.rolls.unshift({ no: state.total, faces: f });
      if (state.rolls.length > 6) state.rolls.pop();
    }

    function shake(final, frames, done) {
      if (REDUCE || frames <= 0) { done(); return; }
      var i = 0;
      var timer = setInterval(function () {
        drawDice(randomFaces(), false);
        if (++i >= frames) { clearInterval(timer); done(); }
      }, 55);
    }

    function dropBall(k, ms, done) {
      if (REDUCE) { done(); return; }
      var h = hist();
      var x0 = W / 2, y0 = Y0 + D + 6, x1 = slotX(k), y1 = h.y(h.vals[k]) - 10;
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

    function rollOnce(fast, done) {
      var f = randomFaces(), k = countHits(f);
      shake(f, fast ? 3 : 9, function () {
        commit(f);
        drawDice(f, step() >= 2);
        var land = function () { state.counts[k] += 1; render(); done(); };
        if (step() >= 3) dropBall(k, fast ? 160 : 420, land); else land();
      });
    }

    function rollMany(m) {
      if (m > 10) {
        for (var i = 0; i < m; i++) {
          var f = randomFaces();
          state.counts[countHits(f)] += 1;
          if (i === m - 1) commit(f);
        }
        state.total += m - 1;
        state.rolls[0].no = state.total;
        render();
        return;
      }
      busy = true; updateButtons();
      var left = m;
      (function next() {
        if (left-- <= 0) { busy = false; render(); return; }
        rollOnce(true, next);
      })();
    }

    stepper.addEventListener("click", function (e) {
      var b = e.target.closest("[data-exp]");
      if (b && stepper.contains(b)) {
        if (busy) return;
        var a = b.getAttribute("data-exp");
        if (a === "roll") { busy = true; updateButtons(); rollOnce(false, function () { busy = false; render(); }); }
        else rollMany(Number(a.replace("roll", "")));
        return;
      }
      if (e.target.closest('[data-lc-nav="reset"]')) { reset(); setTimeout(render, 0); }
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
