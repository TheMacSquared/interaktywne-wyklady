// Sceny wykładu 00: konkretne doświadczenie → statystyka → rozkład wyników.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind:
//   "bag"  garść kulek z worka: p̂ z kolejnych garści (zmienność próbkowa)
//   "tea"  herbata z mlekiem: ilu zgadujących trafia tyle, co pani (przypadek czy umiejętność)
//   "spot" latarka na tłumie: losowanie a próba wygodna (obciążenie)
// Numer kroku czyta z data-lc-step korzenia widgetu. Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji, np. n:25)
// Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
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
  function rng(seed) {
    var a = seed >>> 0;
    return function () {
      a = (a + 0x6D2B79F5) >>> 0;
      var t = a;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  }
  function shuffle(arr, rand) {
    for (var i = arr.length - 1; i > 0; i--) {
      var j = Math.floor(rand() * (i + 1)), t = arr[i]; arr[i] = arr[j]; arr[j] = t;
    }
    return arr;
  }
  function fmt(x, d) { return x.toFixed(d).replace(".", ","); }
  function ease(u) { return u * u * (3 - 2 * u); }
  function niceMax(m) {
    var steps = [5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000];
    for (var i = 0; i < steps.length; i++) if (steps[i] >= m) return steps[i];
    return Math.ceil(m / 1000) * 1000;
  }
  function choose(n, k) {
    var r = 1;
    for (var i = 1; i <= k; i++) r = r * (n - k + i) / i;
    return r;
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

  // --- wspólny szkielet histogramu z żetonami / słupkami ---------------------
  function axisX(g, x0, x1, y, ticks, labelFn) {
    svg("line", { x1: x0, x2: x1, y1: y, y2: y, class: "lc-sc-axis" }, g);
    ticks.forEach(function (t) {
      var x = t.x;
      svg("line", { x1: x, x2: x, y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
      svg("text", { x: x, y: y + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, t.label);
    });
  }

  // =========================================================================
  // BAG: garść kulek z worka
  // =========================================================================
  KINDS.bag = function (cfg, api) {
    var P = cfg.p, NB = 21;
    var PL = 60, PR = 610, PT = 232, PB = 372;
    var st = { n: cfg.n || 25, draws: [], counts: new Array(NB).fill(0), last: null, shown: 0 };
    var jar = (function () {
      // 60 kulek w słoiku w stałym układzie, odsetek czerwonych = P
      var r = rng(7), red = Math.round(P * 60), arr = [];
      for (var i = 0; i < 60; i++) arr.push(i < red ? 1 : 0);
      shuffle(arr, r);
      return arr;
    })();

    function binOf(ph) { return Math.max(0, Math.min(NB - 1, Math.round(ph * 20))); }
    function slotX(i) { return PL + (PR - PL) * (i + 0.5) / NB; }

    function layout(n) {
      var cols = n <= 10 ? 10 : n <= 25 ? 13 : 20;
      var s = Math.min(30, 360 / cols), rows = Math.ceil(n / cols);
      var x0 = 235 + (385 - cols * s) / 2 + s / 2, y0 = 70 - (rows - 1) * s / 2 + 14;
      var pts = [];
      for (var i = 0; i < n; i++) pts.push([x0 + (i % cols) * s, y0 + Math.floor(i / cols) * s]);
      return { pts: pts, r: s * 0.4 };
    }

    function drawBag(g, reveal) {
      if (!reveal) {
        svg("path", { d: "M 70 62 C 20 90 20 190 50 200 C 90 212 150 212 190 200 C 220 190 220 90 170 62 Z", class: "lc-sc-sack" }, g);
        svg("path", { d: "M 70 62 C 90 46 150 46 170 62 C 150 70 90 70 70 62 Z", class: "lc-sc-sack-top" }, g);
        svg("path", { d: "M 82 56 C 100 44 140 44 158 56", class: "lc-sc-rope", fill: "none" }, g);
        svg("text", { x: 120, y: 150, "text-anchor": "middle", class: "lc-sc-big" }, g, "?");
        svg("text", { x: 120, y: 226, "text-anchor": "middle", class: "lc-sc-sub" }, g, "wydział w worku");
      } else {
        svg("rect", { x: 30, y: 52, width: 180, height: 150, rx: 18, class: "lc-sc-jar" }, g);
        var cols = 10, r = 8.2, s = 17.2;
        jar.forEach(function (v, i) {
          var row = Math.floor(i / cols), col = i % cols;
          svg("circle", { cx: 42 + col * s + (row % 2 ? s / 2 : 0) + 8, cy: 66 + row * 21, r: r,
            class: v ? "lc-sc-ball is-hit" : "lc-sc-ball" }, g);
        });
        svg("text", { x: 120, y: 226, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "w worku " + fmt(P * 100, 0) + "% czerwonych");
      }
    }

    // pojedyncza garść → {balls:[0/1…], k, ph}
    function draw() {
      var n = st.n, balls = [], k = 0;
      for (var i = 0; i < n; i++) { var b = Math.random() < P ? 1 : 0; balls.push(b); k += b; }
      return { balls: balls, k: k, ph: k / n, n: n };
    }

    function histInfo() {
      var mx = Math.max.apply(null, st.counts.concat([1]));
      var u = Math.min(15, (PB - PT - 14) / Math.max(mx, 6));
      return { mx: mx, u: u, tokR: Math.min(11, u * 0.44) };
    }

    function tokenY(count, h) { return PB - h.u * (count - 0.5) - 1; }

    function drawHist(g, step) {
      var h = histInfo();
      axisX(g, PL, PR, PB, [0, 0.2, 0.4, 0.6, 0.8, 1].map(function (t) {
        return { x: PL + (PR - PL) * (t * 20 + 0.5) / NB, label: fmt(t, 1) };
      }));
      svg("text", { x: (PL + PR) / 2, y: PB + 38, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "odsetek czerwonych w garści, p̂");
      var info = "garści: " + st.draws.length;
      if (step >= 4 && st.draws.length > 1) {
        var lo = Math.min.apply(null, st.draws.map(function (d) { return d.ph; }));
        var hi = Math.max.apply(null, st.draws.map(function (d) { return d.ph; }));
        info += " · p̂ od " + fmt(lo, 2) + " do " + fmt(hi, 2);
      }
      svg("text", { x: PR, y: PT - 12, "text-anchor": "end", class: "lc-sc-n" }, g, info);
      for (var i = 0; i < NB; i++) {
        if (h.u >= 9) {
          for (var c = 1; c <= st.counts[i]; c++) {
            svg("circle", { cx: slotX(i), cy: tokenY(c, h), r: h.tokR, class: "lc-sc-token" }, g);
          }
        } else if (st.counts[i] > 0) {
          var bw = (PR - PL) / NB * 0.7;
          svg("rect", { x: slotX(i) - bw / 2, y: PB - h.u * st.counts[i], width: bw,
            height: h.u * st.counts[i], class: "lc-sc-token is-bar" }, g);
        }
      }
      if (step >= 4) {
        var x = PL + (PR - PL) * (P * 20 + 0.5) / NB;
        svg("line", { x1: x, x2: x, y1: PT - 4, y2: PB, class: "lc-sc-param" }, g);
        svg("text", { x: x + 6, y: PT + 4, class: "lc-sc-param-t" }, g, "p = " + fmt(P, 2));
      }
    }

    function drawLog(g, step) {
      if (!st.draws.length) {
        svg("text", { x: W / 2, y: 300, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Wyciągnij garść, żeby zobaczyć, co w niej jest.");
        return;
      }
      st.draws.slice(-6).reverse().forEach(function (d, i) {
        var y = 262 + i * 24, g2 = svg("g", { opacity: 1 - i * 0.14 }, g);
        svg("text", { x: 120, y: y, class: "lc-sc-log" }, g2, "garść " + d.no);
        if (step >= 2) {
          svg("text", { x: 220, y: y, class: "lc-sc-log" }, g2,
            d.k + " z " + d.n + " czerwonych");
          svg("text", { x: 400, y: y, class: "lc-sc-log is-x" }, g2, "p̂ = " + fmt(d.ph, 2));
        } else {
          svg("text", { x: 220, y: y, class: "lc-sc-log" }, g2, d.k + " z " + d.n + " czerwonych");
        }
      });
    }

    // scena górna: worek, taca z garścią, odczyt
    function drawStage(g, step, anim) {
      drawBag(g, step >= 4);
      var L = layout(st.n);
      if (st.last) {
        var d = st.last, upto = anim ? anim.upto : d.n;
        L.pts.forEach(function (pt, i) {
          if (i >= upto) return;
          var pos = anim && anim.flying && i === upto - 1
            ? [anim.from[0] + (pt[0] - anim.from[0]) * anim.u, anim.from[1] + (pt[1] - anim.from[1]) * anim.u]
            : pt;
          svg("circle", { cx: pos[0], cy: pos[1], r: L.r, class: d.balls[i] ? "lc-sc-ball is-hit" : "lc-sc-ball" }, g);
        });
        if (!anim || anim.done) {
          if (step >= 2) {
            svg("text", { x: 428, y: 196, "text-anchor": "middle", class: "lc-sc-read" }, g,
              "p̂ = " + d.k + "/" + d.n + " = " + fmt(d.ph, 2));
          } else {
            svg("text", { x: 428, y: 196, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
              d.k + " z " + d.n + " kulek jest czerwonych");
          }
        }
      } else {
        svg("text", { x: 428, y: 100, "text-anchor": "middle", class: "lc-sc-sub" }, g, "tu pojawi się garść");
      }
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, null);
      if (step >= 3) drawHist(api.low, step); else drawLog(api.low, step);
    }

    function commit(d) {
      d.no = st.draws.length + 1;
      st.draws.push(d);
      st.last = d;
    }

    function runOne(fast, done) {
      var d = draw(), L = layout(st.n), step = api.step();
      var total = fast ? 140 : Math.min(1500, 400 + st.n * 11);
      st.last = d;
      tween(total, function (u) {
        var upto = Math.max(1, Math.ceil(u * d.n)), local = (u * d.n) % 1;
        api.stage.textContent = "";
        drawStage(api.stage, step, { upto: upto, flying: u < 1, u: u >= 1 ? 1 : ease(local === 0 ? 1 : local),
          from: [190, 70], done: u >= 1 });
      }, function () {
        commit(d);
        var land = function () {
          st.counts[binOf(d.ph)] += 1; render(); done();
        };
        if (step >= 3) {
          var h = histInfo(), i = binOf(d.ph);
          var x0 = 428, y0 = 190, x1 = slotX(i), y1 = tokenY(st.counts[i] + 1, h);
          var tok = svg("circle", { r: 8, cx: x0, cy: y0, class: "lc-sc-token" }, api.fly);
          tween(fast ? 140 : 420, function (u) {
            var e = ease(u);
            tok.setAttribute("cx", x0 + (x1 - x0) * e);
            tok.setAttribute("cy", y0 + (y1 - y0) * e);
          }, function () { api.fly.textContent = ""; land(); });
        } else land();
      });
    }

    function runMany(m) {
      var d;
      for (var i = 0; i < m; i++) {
        d = draw(); d.no = st.draws.length + 1;
        st.draws.push(d); st.counts[binOf(d.ph)] += 1;
      }
      st.last = d;
      render();
    }

    return {
      render: render,
      reset: function () { st.draws = []; st.counts = new Array(NB).fill(0); st.last = null; render(); },
      opt: function (name, v) { if (name === "n") { st.n = Number(v); this.reset(); } },
      go: function (done) { runOne(false, done); },
      many: function (m, done) {
        if (m > 10) { runMany(m); done(); return; }
        var left = m;
        (function next() { if (left-- <= 0) { done(); return; } runOne(true, next); })();
      }
    };
  };

  // =========================================================================
  // TEA: pani i zgadujący
  // =========================================================================
  KINDS.tea = function (cfg, api) {
    var C = cfg.cups || 10, NBINS = C + 1;
    var PL = 70, PR = 610, PT = 236, PB = 366;
    var st = { hits: cfg.hits || 9, counts: new Array(NBINS).fill(0), total: 0, row: null, who: null };
    var prob = [];
    for (var k = 0; k <= C; k++) prob.push(choose(C, k) / Math.pow(2, C));

    function cupX(i) { return 108 + i * 50; }
    function slotX(i) { return PL + (PR - PL) * (i + 0.5) / NBINS; }

    function person(g, x, y, cls) {
      svg("circle", { cx: x, cy: y - 18, r: 8, class: cls }, g);
      svg("rect", { x: x - 10, y: y - 8, width: 20, height: 28, rx: 8, class: cls }, g);
    }

    function cup(g, x, y, state, lift) {
      var q = svg("g", { transform: "translate(" + x + "," + (y - lift) + ")" }, g);
      svg("ellipse", { cx: 0, cy: 24, rx: 20, ry: 5, class: "lc-sc-saucer" }, q);
      svg("path", { d: "M -14 4 L 14 4 C 14 24 8 26 0 26 C -8 26 -14 24 -14 4 Z", class: "lc-sc-cup" }, q);
      svg("path", { d: "M 14 9 C 24 9 24 20 13 20", class: "lc-sc-cup-handle", fill: "none" }, q);
      svg("ellipse", { cx: 0, cy: 4, rx: 14, ry: 3, class: "lc-sc-tea" }, q);
      if (lift > 0) {
        [-6, 2, 9].forEach(function (dx, i) {
          svg("path", { d: "M " + dx + " -4 C " + (dx - 4) + " -10 " + (dx + 4) + " -14 " + dx + " -20", class: "lc-sc-steam", fill: "none" }, q);
        });
      }
      if (state === 1) svg("text", { x: 0, y: 52, "text-anchor": "middle", class: "lc-sc-mark is-ok" }, q, "✓");
      if (state === 0) svg("text", { x: 0, y: 52, "text-anchor": "middle", class: "lc-sc-mark is-no" }, q, "✗");
    }

    // row: {res:[1/0/null…], who:"lady"|"guess", hits}
    function drawRow(g, row, tasting) {
      var lady = !row || row.who === "lady";
      person(g, 50, 50, lady ? "lc-sc-person is-lady" : "lc-sc-person");
      if (lady) svg("circle", { cx: 50, cy: 36, r: 3.5, class: "lc-sc-bow" }, g);
      svg("text", { x: 50, y: 96, "text-anchor": "middle", class: "lc-sc-sub" }, g,
        row ? (lady ? "pani" : "zgadujący") : "");
      for (var i = 0; i < C; i++) {
        var r = row ? row.res[i] : null;
        cup(g, cupX(i), 40, r === null || r === undefined ? -1 : r, tasting === i ? 8 : 0);
      }
      if (row) {
        var n = row.res.filter(function (v) { return v === 1; }).length;
        var done = row.res.every(function (v) { return v !== null && v !== undefined; });
        svg("text", { x: 340, y: 150, "text-anchor": "middle", class: "lc-sc-read" + (done ? "" : " is-plain") }, g,
          "trafień: " + n + " z " + C);
      } else {
        svg("text", { x: 340, y: 150, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "dziesięć filiżanek: w każdej mleko nalano przed herbatą albo po niej");
      }
    }

    function ladyResults() {
      var res = new Array(C).fill(1), idx = shuffle(Array.apply(null, Array(C)).map(function (_, i) { return i; }), Math.random);
      for (var j = 0; j < C - st.hits; j++) res[idx[j]] = 0;
      return res;
    }
    function guessResults() {
      var res = [];
      for (var i = 0; i < C; i++) res.push(Math.random() < 0.5 ? 1 : 0);
      return res;
    }

    function drawLog(g, step) {
      svg("text", { x: W / 2, y: 262, "text-anchor": "middle", class: "lc-sc-sub" }, g,
        step === 1 ? "Pani próbuje po kolei każdej filiżanki i mówi, co nalano najpierw."
                   : "Zgadujący nie ma pojęcia: do każdej filiżanki rzuca monetą.");
    }

    function ge() { return st.hits; }
    function tailShare() {
      var s = 0; for (var k = ge(); k <= C; k++) s += prob[k];
      return s;
    }

    function drawHist(g, step) {
      var rel = step >= 4, mx = Math.max.apply(null, st.counts.concat([1]));
      var top = rel ? Math.max.apply(null, prob.concat(st.total ? st.counts.map(function (c) { return c / st.total; }) : [0])) : mx;
      var ymax = rel ? Math.ceil(top * 1.15 * 20) / 20 : niceMax(mx * 1.1);
      function y(v) { return PB - (PB - PT) * v / ymax; }
      [0, 0.5, 1].forEach(function (f) {
        var v = ymax * f;
        svg("line", { x1: PL, x2: PR, y1: y(v), y2: y(v), class: "lc-sc-grid" }, g);
        svg("text", { x: PL - 8, y: y(v) + 4, "text-anchor": "end", class: "lc-sc-tick" }, g,
          rel ? fmt(v, 2) : String(Math.round(v)));
      });
      axisX(g, PL, PR, PB, st.counts.map(function (_, i) { return { x: slotX(i), label: String(i) }; }));
      var bw = (PR - PL) / NBINS * 0.66;
      st.counts.forEach(function (c, i) {
        var v = rel ? (st.total ? c / st.total : 0) : c;
        if (v > 0) svg("rect", { x: slotX(i) - bw / 2, y: y(v), width: bw, height: PB - y(v),
          class: "lc-sc-bar" + (i >= ge() ? " is-hit" : "") }, g);
        if (rel) {
          svg("circle", { cx: slotX(i), cy: y(prob[i]), r: 4.5, class: "lc-sc-theory" }, g);
        }
      });
      svg("text", { x: (PL + PR) / 2, y: PB + 38, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "ile filiżanek zgadł zgadujący");
      svg("text", { x: PR, y: PT - 34, "text-anchor": "end", class: "lc-sc-n" }, g,
        "zgadujących: " + st.total);
      var ge_n = st.counts.reduce(function (s, c, i) { return i >= ge() ? s + c : s; }, 0);
      svg("text", { x: PL, y: PT - 34, class: "lc-sc-n is-hit" }, g,
        st.total ? ("≥ " + ge() + " trafień: " + ge_n + " z " + st.total + " (" + fmt(100 * ge_n / st.total, 1) + "%)")
                 : ("trafień jak u pani: ≥ " + ge()));
      if (rel) {
        svg("circle", { cx: PL + 6, cy: PT - 18, r: 4.5, class: "lc-sc-theory" }, g);
        svg("text", { x: PL + 16, y: PT - 14, class: "lc-sc-n" }, g,
          "model: sama losowość daje ≥ " + ge() + " w " + fmt(100 * tailShare(), 1) + "% przypadków");
      }
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawRow(api.stage, st.row, null);
      if (step >= 3) drawHist(api.low, step); else drawLog(api.low, step);
    }

    function taste(who, fast, done) {
      var res = who === "lady" ? ladyResults() : guessResults();
      var row = { who: who, res: new Array(C).fill(null) };
      st.row = row;
      var i = 0, per = fast ? 28 : 230;
      function next() {
        if (i >= C) {
          api.stage.textContent = ""; drawRow(api.stage, row, null);
          done(res.filter(function (v) { return v === 1; }).length);
          return;
        }
        api.stage.textContent = ""; drawRow(api.stage, row, i);
        var k = i;
        setTimeout(function () {
          row.res[k] = res[k];
          api.stage.textContent = ""; drawRow(api.stage, row, null);
          i++; next();
        }, REDUCE ? 0 : per);
      }
      next();
    }

    function land(n, fast, done) {
      var step = api.step();
      var finish = function () { st.counts[n] += 1; st.total += 1; render(); done(); };
      if (step < 3) { render(); done(); return; }
      var x0 = 340, y0 = 150, x1 = slotX(n), y1 = PB - 20;
      var tok = svg("circle", { r: 8, cx: x0, cy: y0, class: "lc-sc-token" }, api.fly);
      tween(fast ? 120 : 380, function (u) {
        var e = ease(u);
        tok.setAttribute("cx", x0 + (x1 - x0) * e); tok.setAttribute("cy", y0 + (y1 - y0) * e);
      }, function () { api.fly.textContent = ""; finish(); });
    }

    return {
      render: render,
      reset: function () { st.counts = new Array(NBINS).fill(0); st.total = 0; st.row = null; render(); },
      opt: function (name, v) {
        if (name === "hits") { st.hits = Number(v); st.row = null; render(); }
      },
      go: function (done) {
        var step = api.step(), who = step === 1 ? "lady" : "guess";
        taste(who, false, function (n) {
          if (who === "lady") { render(); done(); } else land(n, false, done);
        });
      },
      many: function (m, done) {
        if (m > 10) {
          for (var i = 0; i < m; i++) {
            var n = 0; for (var j = 0; j < C; j++) if (Math.random() < 0.5) n++;
            st.counts[n] += 1; st.total += 1;
          }
          st.row = { who: "guess", res: guessResults() };
          render(); done(); return;
        }
        var left = m;
        (function next() {
          if (left-- <= 0) { done(); return; }
          taste("guess", true, function (n) { land(n, true, next); });
        })();
      }
    };
  };

  // =========================================================================
  // SPOT: latarka na tłumie
  // =========================================================================
  KINDS.spot = function (cfg, api) {
    var D = cfg.d, A = cfg.a, N = D.length, MU = cfg.mu;
    var AX0 = 60, AX1 = 600, RY = { rand: 296, spot: 336 };
    var AMIN = 0, AMAX = 60;
    var st = { n: cfg.n || 50, rand: [], spot: [], last: null, lastMode: null, beam: 0 };
    var LAMP = [24, 112];

    // położenie kropek: akademik po lewej, reszta w pozostałej części kampusu
    var pos = (function () {
      var r = rng(11), out = [];
      for (var i = 0; i < N; i++) {
        if (A[i]) {
          var ang = r() * Math.PI * 2, rad = Math.sqrt(r());
          out.push([92 + Math.cos(ang) * rad * 62, 112 + Math.sin(ang) * rad * 74]);
        } else {
          out.push([190 + r() * 430, 16 + r() * 190]);
        }
      }
      return out;
    })();

    // waga w próbie wygodnej: kto stoi bliżej latarki, ten częściej w niej jest
    var wSpot = pos.map(function (p) {
      var dx = p[0] - 40, dy = p[1] - 112;
      return Math.exp(-(dx * dx + dy * dy) / (2 * 110 * 110)) + 0.012;
    });

    function sampleRand(n) {
      var idx = [], used = {};
      while (idx.length < n) {
        var i = Math.floor(Math.random() * N);
        if (!used[i]) { used[i] = 1; idx.push(i); }
      }
      return idx;
    }
    function sampleSpot(n) {
      var keys = wSpot.map(function (w, i) { return [Math.pow(Math.random(), 1 / w), i]; });
      keys.sort(function (a, b) { return b[0] - a[0]; });
      return keys.slice(0, n).map(function (k) { return k[1]; });
    }
    function mean(idx) { return idx.reduce(function (s, i) { return s + D[i]; }, 0) / idx.length; }
    function ax(v) { return AX0 + (AX1 - AX0) * (Math.min(AMAX, Math.max(AMIN, v)) - AMIN) / (AMAX - AMIN); }

    function drawCrowd(g, sel, mode, beamU) {
      // latarka i wiązka (tylko w trybie latarki)
      if (mode === "spot") {
        var w = 330 * (beamU === undefined ? 1 : beamU);
        svg("path", { d: "M 34 112 L " + (34 + w) + " " + (112 - 105 * (w / 330 + 0.05)) + " L " +
          (34 + w) + " " + (112 + 105 * (w / 330 + 0.05)) + " Z", class: "lc-sc-beam" }, g);
      }
      var inSel = {};
      (sel || []).forEach(function (i) { inSel[i] = 1; });
      for (var i = 0; i < N; i++) {
        if (inSel[i]) continue;
        svg("circle", { cx: pos[i][0], cy: pos[i][1], r: 2.3, class: A[i] ? "lc-sc-dot is-dorm" : "lc-sc-dot" }, g);
      }
      (sel || []).forEach(function (i) {
        svg("circle", { cx: pos[i][0], cy: pos[i][1], r: 4.4, class: "lc-sc-dot is-sel " + (A[i] ? "is-dorm" : "") }, g);
      });
      // budynek akademika i latarka
      svg("text", { x: 92, y: 214, "text-anchor": "middle", class: "lc-sc-sub" }, g, "akademik");
      if (mode === "spot" || api.step() >= 2) {
        svg("g", { transform: "translate(" + LAMP[0] + "," + LAMP[1] + ")" }, g);
        svg("circle", { cx: LAMP[0], cy: LAMP[1], r: 9, class: "lc-sc-lamp" }, g);
        svg("rect", { x: LAMP[0] - 5, y: LAMP[1] + 8, width: 10, height: 18, rx: 3, class: "lc-sc-lamp-handle" }, g);
      }
    }

    function drawStrip(g, step) {
      svg("rect", { x: 0, y: 236, width: W, height: 150, class: "lc-sc-strip-bg" }, g);
      axisX(g, AX0, AX1, 372, [0, 10, 20, 30, 40, 50, 60].map(function (v) { return { x: ax(v), label: String(v) }; }));
      svg("text", { x: (AX0 + AX1) / 2, y: 410, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "średni czas dojazdu w próbie, x̄ (min)");
      svg("line", { x1: ax(MU), x2: ax(MU), y1: 250, y2: 372, class: "lc-sc-param" }, g);
      svg("text", { x: ax(MU) + 6, y: 262, class: "lc-sc-param-t" }, g, "μ = " + fmt(MU, 0) + " min");
      var rows = step >= 2 ? ["rand", "spot"] : ["rand"];
      var names = { rand: "losowanie", spot: "latarka" };
      rows.forEach(function (m) {
        var y = RY[m] + (step >= 2 ? 0 : 20);
        svg("text", { x: 6, y: y + 4, class: "lc-sc-sub" }, g, names[m]);
        var r = rng(3);
        st[m].forEach(function (v, i) {
          var last = i === st[m].length - 1 && st.lastMode === m;
          svg("circle", { cx: ax(v), cy: y + (r() - 0.5) * 26, r: last ? 5.6 : 3.4,
            class: "lc-sc-xbar is-" + m + (last ? " is-last" : ""),
            opacity: st[m].length > 40 ? 0.45 : 0.8 }, g);
        });
        if (st[m].length) {
          var avg = st[m].reduce(function (a, b) { return a + b; }, 0) / st[m].length;
          svg("line", { x1: ax(avg), x2: ax(avg), y1: y - 17, y2: y + 17, class: "lc-sc-avg is-" + m }, g);
        }
      });
      if (!st.rand.length && !st.spot.length) {
        svg("text", { x: W / 2, y: 330, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Tu pojawią się średnie z kolejnych prób.");
      }
    }

    function render(sel, mode, beamU) {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawCrowd(api.stage, sel === undefined ? (st.last ? st.last.idx : []) : sel,
        mode === undefined ? (st.last ? st.last.mode : (step >= 2 ? "spot" : "rand")) : mode, beamU);
      drawStrip(api.low, step);
      var rd = step >= 3 ? null : null; void rd;
    }

    function modeFor(step, alt) { return step === 1 ? "rand" : step === 2 ? "spot" : alt; }

    function one(mode, fast, done) {
      var idx = mode === "rand" ? sampleRand(st.n) : sampleSpot(st.n), xb = mean(idx);
      var step = api.step();
      var finish = function () {
        st[mode].push(xb);
        st.last = { idx: idx, mode: mode, xbar: xb }; st.lastMode = mode;
        render(); done();
      };
      if (mode === "spot") {
        tween(fast ? 110 : 600, function (u) { render(u >= 1 ? idx : [], "spot", ease(u)); }, finish);
      } else {
        tween(fast ? 110 : 500, function (u) {
          render(idx.slice(0, Math.ceil(idx.length * ease(u))), "rand");
        }, finish);
      }
      void step;
    }

    var altFlip = 0;
    return {
      render: function () { render(); },
      reset: function () { st.rand = []; st.spot = []; st.last = null; st.lastMode = null; render(); },
      opt: function (name, v) { if (name === "n") { st.n = Number(v); this.reset(); } },
      go: function (done) {
        var step = api.step();
        if (step >= 3) {
          // jedna próba każdego rodzaju naraz
          one("rand", false, function () { one("spot", false, done); });
        } else one(modeFor(step), false, done);
      },
      many: function (m, done) {
        var step = api.step();
        for (var i = 0; i < m; i++) {
          st.rand.push(mean(sampleRand(st.n)));
          st.spot.push(mean(sampleSpot(st.n)));
        }
        var idx = sampleSpot(st.n);
        st.last = { idx: idx, mode: "spot", xbar: st.spot[st.spot.length - 1] }; st.lastMode = "spot";
        render(); done();
        void step; void altFlip;
      }
    };
  };

  // =========================================================================
  // ROWS: student wchodzi do tabeli (obserwacja, zmienna, n)
  // =========================================================================
  KINDS.rows = function (cfg, api) {
    var N = cfg.rok.length;
    var COLS = [
      { key: "id", label: "Nr", x: 80 },
      { key: "rok", label: "Rok studiów", x: 195 },
      { key: "akademik", label: "Akademik", x: 320 },
      { key: "dojazd", label: "Dojazd (min)", x: 440 },
      { key: "praca", label: "Praca", x: 545 }
    ];
    var TY = 196, RH = 26, SHOW = 6;
    var st = { rows: [], v: "dojazd", person: null, anim: null };

    function newPerson() {
      var i = Math.floor(Math.random() * N);
      return { id: i + 1, rok: cfg.rok[i], akademik: cfg.a[i] ? "tak" : "nie",
        dojazd: cfg.d[i], praca: cfg.pr[i] ? "tak" : "nie" };
    }

    function personIcon(g, x, y, scale) {
      var q = svg("g", { transform: "translate(" + x + "," + y + ") scale(" + scale + ")" }, g);
      svg("circle", { cx: 0, cy: -34, r: 15, class: "lc-sc-person" }, q);
      svg("rect", { x: -20, y: -14, width: 40, height: 52, rx: 16, class: "lc-sc-person" }, q);
      svg("rect", { x: -14, y: 34, width: 11, height: 26, rx: 4, class: "lc-sc-person" }, q);
      svg("rect", { x: 3, y: 34, width: 11, height: 26, rx: 4, class: "lc-sc-person" }, q);
    }

    function chip(g, x, y, text, on) {
      var w = 20 + text.length * 8.2;
      svg("rect", { x: x, y: y, width: w, height: 28, rx: 14, class: "lc-sc-chip" }, g);
      svg("text", { x: x + w / 2, y: y + 19, "text-anchor": "middle", class: "lc-sc-chip-t" }, g, text);
    }

    function drawPerson(g, p, px, nChips) {
      personIcon(g, px, 82, 0.95);
      if (!p) return;
      var texts = [p.rok + ". rok studiów", p.akademik === "tak" ? "w akademiku" : "poza akademikiem",
        "dojazd " + p.dojazd + " min", p.praca === "tak" ? "pracuje" : "nie pracuje"];
      var pos = [[190, 22], [190, 62], [400, 22], [400, 62]];
      texts.forEach(function (t, i) { if (i < nChips) chip(g, pos[i][0], pos[i][1], t); });
    }

    function cell(row, key) { return key === "id" ? String(row.id) : String(row[key]); }

    function drawTable(g, step, newest) {
      var vcol = step >= 2 ? st.v : null;
      // nagłówek
      svg("rect", { x: 40, y: TY - 20, width: 560, height: RH, class: "lc-sc-th" }, g);
      COLS.forEach(function (c) {
        if (vcol && c.key === vcol) svg("rect", { x: c.x - 54, y: TY - 20, width: 108, height: RH + (Math.min(st.rows.length, SHOW)) * RH, class: "lc-sc-colhi" }, g);
      });
      COLS.forEach(function (c) {
        svg("text", { x: c.x, y: TY - 2, "text-anchor": "middle", class: "lc-sc-th-t" }, g, c.label);
      });
      var shown = st.rows.slice(-SHOW);
      shown.forEach(function (r, i) {
        var y = TY + 6 + i * RH, last = i === shown.length - 1 && step === 1;
        if (last) svg("rect", { x: 40, y: y - 4, width: 560, height: RH - 2, class: "lc-sc-rowhi" }, g);
        COLS.forEach(function (c) {
          svg("text", { x: c.x, y: y + 14, "text-anchor": "middle",
            class: "lc-sc-cell" + (vcol === c.key ? " is-hi" : "") }, g, cell(r, c.key));
        });
        svg("line", { x1: 40, x2: 600, y1: y + RH - 4, y2: y + RH - 4, class: "lc-sc-grid" }, g);
      });
      if (!shown.length) {
        svg("text", { x: W / 2, y: TY + 60, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Tabela jest pusta. Poproś pierwszą osobę, żeby podeszła.");
      }
      var extra = st.rows.length - shown.length;
      if (extra > 0) {
        svg("text", { x: 320, y: TY + 6 + SHOW * RH + 14, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "… i " + extra + " wcześniejszych wierszy");
      }
      svg("text", { x: 600, y: 16, "text-anchor": "end", class: "lc-sc-n" }, g, "n = " + st.rows.length);
      void newest;
    }

    function render(person, chips, px) {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawPerson(api.stage, person === undefined ? st.person : person,
        px === undefined ? 90 : px, chips === undefined ? 4 : chips);
      drawTable(api.low, step);
      if (step >= 2 && st.rows.length) {
        var names = { rok: "rok studiów", akademik: "mieszkanie w akademiku", dojazd: "czas dojazdu", praca: "praca zarobkowa" };
        svg("text", { x: 320, y: 412, "text-anchor": "middle", class: "lc-sc-sub" }, api.low,
          "zmienna: " + names[st.v] + " (jedna kolumna, po jednej wartości na każdą osobę)");
      }
    }

    function addOne(fast, done) {
      var p = newPerson(), step = api.step();
      st.person = p;
      var chipsMs = fast ? 0 : 140;
      tween(fast ? 120 : 520, function (u) { render(p, 0, -60 + (90 + 60) * ease(u)); }, function () {
        var k = 0;
        (function nextChip() {
          render(p, k, 90);
          if (k >= 4) {
            setTimeout(function () { st.rows.push(p); render(); done(); }, REDUCE ? 0 : (fast ? 20 : 260));
            return;
          }
          k++;
          setTimeout(nextChip, REDUCE ? 0 : chipsMs);
        })();
      });
      void step;
    }

    return {
      render: function () { render(); },
      reset: function () { st.rows = []; st.person = null; render(); },
      opt: function (name, v) { if (name === "var") { st.v = v; render(); } },
      go: function (done) { addOne(false, done); },
      many: function (m, done) {
        for (var i = 0; i < m; i++) { st.person = newPerson(); st.rows.push(st.person); }
        render(); done();
      }
    };
  };

  // =========================================================================
  // POP: populacja, operat i próba jako miniatura
  // =========================================================================
  KINDS.pop = function (cfg, api) {
    var N = cfg.rok.length, COLS = 80, SP = 7.1, X0 = 36, Y0 = 14, R = 2.3;
    var st = { n: cfg.n || 50, sel: null, off: null };
    var rand = rng(5);
    // operat: ok. 8% osób nie ma na liście (urlop dziekański, wymiana, zaoczne)
    var outside = new Array(N).fill(false);
    for (var i = 0; i < N; i++) outside[i] = rand() < 0.08;
    var inList = [];
    outside.forEach(function (o, i) { if (!o) inList.push(i); });

    function gp(i) { return [X0 + (i % COLS) * SP, Y0 + Math.floor(i / COLS) * SP]; }
    function share(arr, idx) {
      var s = 0; idx.forEach(function (i) { s += arr[i] ? 1 : 0; });
      return s / idx.length;
    }
    function allIdx() { var a = []; for (var i = 0; i < N; i++) a.push(i); return a; }
    function pick(n, pool) {
      var a = pool.slice(), out = [];
      for (var k = 0; k < n; k++) {
        var j = k + Math.floor(Math.random() * (a.length - k)), t = a[k]; a[k] = a[j]; a[j] = t;
        out.push(a[k]);
      }
      return out;
    }

    function trayPos(n) {
      var cols = n <= 20 ? 10 : n <= 50 ? 17 : 25, sp = Math.min(13, 250 / cols), pts = [];
      for (var i = 0; i < n; i++) pts.push([48 + (i % cols) * sp, 292 + Math.floor(i / cols) * sp]);
      return { pts: pts, r: Math.min(4.4, sp * 0.36) };
    }

    function dotClass(i, sel) {
      return "lc-sc-pdot" + (cfg.pr[i] ? " is-work" : "") + (sel ? " is-sel" : "");
    }

    function drawGrid(g, step, fly) {
      var isSel = {};
      if (st.sel) st.sel.forEach(function (i) { isSel[i] = 1; });
      for (var i = 0; i < N; i++) {
        var p = gp(i);
        if (step >= 2 && outside[i]) {
          svg("circle", { cx: p[0], cy: p[1], r: R, class: "lc-sc-pdot is-out" }, g);
        } else if (isSel[i]) {
          if (!fly) svg("circle", { cx: p[0], cy: p[1], r: R + 1.8, class: dotClass(i, true) }, g);
          else svg("circle", { cx: p[0], cy: p[1], r: R, class: dotClass(i, false), opacity: 0.25 }, g);
        } else {
          svg("circle", { cx: p[0], cy: p[1], r: R, class: dotClass(i, false), opacity: st.sel ? 0.5 : 1 }, g);
        }
      }
      var yb = Y0 + 30 * SP + 4;
      if (step === 1) {
        svg("text", { x: 36, y: yb + 12, class: "lc-sc-sub" }, g,
          "N = " + N + " · lista uporządkowana według roku studiów: pierwszy rok u góry, piąty na dole");
      } else {
        svg("text", { x: 36, y: yb + 12, class: "lc-sc-sub" }, g,
          "operat: " + inList.length + " osób na liście · puste kółka: poza listą, więc nie mogą trafić do próby");
      }
      // legenda
      svg("circle", { cx: 40, cy: yb + 30, r: 3.4, class: "lc-sc-pdot is-work" }, g);
      svg("text", { x: 48, y: yb + 34, class: "lc-sc-sub" }, g, "pracuje");
      svg("circle", { cx: 110, cy: yb + 30, r: 3.4, class: "lc-sc-pdot" }, g);
      svg("text", { x: 118, y: yb + 34, class: "lc-sc-sub" }, g, "nie pracuje");
    }

    function drawTray(g, u) {
      if (!st.sel) return;
      var T = trayPos(st.sel.length);
      st.sel.forEach(function (i, k) {
        var p = gp(i), q = T.pts[k], e = u === undefined ? 1 : ease(u);
        svg("circle", { cx: p[0] + (q[0] - p[0]) * e, cy: p[1] + (q[1] - p[1]) * e,
          r: R + (T.r - R) * e, class: dotClass(i, true) }, g);
      });
      if (u === undefined || u >= 1) {
        svg("text", { x: 48, y: 280, class: "lc-sc-sub" }, g, "próba: n = " + st.sel.length);
      }
    }

    function drawBars(g) {
      var items = [["pracuje zarobkowo", cfg.pr], ["mieszka w akademiku", cfg.a]];
      var x0 = 340, w = 190, y = 284;
      svg("text", { x: x0, y: 270, class: "lc-sc-sub" }, g, "udział w populacji i w próbie");
      items.forEach(function (it, k) {
        var pop = share(it[1], allIdx()), sam = st.sel ? share(it[1], st.sel) : null;
        var yy = y + k * 62;
        svg("text", { x: x0, y: yy + 8, class: "lc-sc-bar-t" }, g, it[0]);
        [["populacja", pop, 0], ["próba", sam, 1]].forEach(function (b) {
          var by = yy + 16 + b[2] * 18;
          svg("rect", { x: x0, y: by, width: w, height: 13, rx: 3, class: "lc-sc-pbar-bg" }, g);
          if (b[1] !== null) svg("rect", { x: x0, y: by, width: w * b[1], height: 13, rx: 3, class: "lc-sc-pbar" + (b[2] ? " is-sample" : "") }, g);
          svg("text", { x: x0 + w + 8, y: by + 11, class: "lc-sc-tick is-x" }, g,
            b[0] + " " + (b[1] === null ? "–" : Math.round(b[1] * 100) + "%"));
        });
      });
    }

    function render(prog, fly) {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawGrid(api.stage, step, fly);
      if (step >= 3) {
        if (!st.sel) {
          svg("text", { x: 48, y: 300, class: "lc-sc-sub" }, api.low, "Wylosuj próbę, żeby zobaczyć ją w miniaturze.");
        } else drawTray(api.low, prog);
        if (step >= 4) drawBars(api.low);
      } else if (!st.sel) {
        svg("text", { x: W / 2, y: 330, "text-anchor": "middle", class: "lc-sc-sub" }, api.low,
          step === 1 ? "Każda kropka to jedna osoba." : "Lista z dziekanatu nie obejmuje wszystkich.");
      } else {
        svg("text", { x: W / 2, y: 330, "text-anchor": "middle", class: "lc-sc-read" }, api.low,
          "wylosowano " + st.sel.length + " osób z " + (step === 1 ? N : inList.length));
      }
    }

    return {
      render: function () { render(); },
      reset: function () { st.sel = null; render(); },
      opt: function (name, v) { if (name === "n") { st.n = Number(v); st.sel = null; render(); } },
      go: function (done) {
        var step = api.step();
        st.sel = pick(st.n, step === 1 ? allIdx() : inList);
        if (step >= 3) {
          tween(900, function (u) { render(u, true); }, function () { render(); done(); });
        } else {
          tween(350, function (u) { render(); void u; }, function () { render(); done(); });
        }
      },
      many: function (m, done) { this.go(done); void m; }
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
