// Sceny wykładu 04: konkretne doświadczenie → statystyka → rozkład wyników.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind:
//   "perm"  kartki z wynikami testu: tasowanie na dwa stosy, p-wartość jako odsetek tasowań
//   "labs"  sto pracowni powtarza eksperyment z telefonem: α i moc jako częstości alarmów
//   "pairs" k kierunków o tej samej średniej: test t dla każdej pary a jedna ANOVA
// Numer kroku czyta z data-lc-step korzenia widgetu. Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji, np. n:40)
// Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
// p-wartości liczone dokładnie: test t Welcha (rozkład t) i test F ANOVA (rozkład F)
// przez regularyzowaną niekompletną funkcję beta.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H = 440;
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function fmt(x, d) { return x.toFixed(d); }
  function fmtP(p) { return p < 0.001 ? "p < 0.001" : "p = " + fmt(p, 3); }
  function pct(a, b) { return b ? fmt(100 * a / b, 1) + "%" : "-"; }
  function ease(u) { return u * u * (3 - 2 * u); }
  function niceMax(m) {
    var steps = [5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 1500, 2000, 5000];
    for (var i = 0; i < steps.length; i++) if (steps[i] >= m) return steps[i];
    return Math.ceil(m / 1000) * 1000;
  }
  function wait(ms, f) { setTimeout(f, REDUCE ? 0 : ms); }

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
  function flyToken(api, x0, y0, x1, y1, ms, done) {
    var tok = svg("circle", { r: 7, cx: x0, cy: y0, class: "lc-sc-token" }, api.fly);
    tween(ms, function (u) {
      var e = ease(u);
      tok.setAttribute("cx", x0 + (x1 - x0) * e); tok.setAttribute("cy", y0 + (y1 - y0) * e);
    }, function () { api.fly.textContent = ""; done(); });
  }

  // --- losowanie i statystyka ----------------------------------------------
  var spare = null;
  function randn() {
    if (spare !== null) { var s = spare; spare = null; return s; }
    var u = 0, v = 0;
    while (u === 0) u = Math.random();
    v = Math.random();
    var r = Math.sqrt(-2 * Math.log(u)), t = 2 * Math.PI * v;
    spare = r * Math.sin(t);
    return r * Math.cos(t);
  }
  function sample(n, mu, sd) {
    var a = [];
    for (var i = 0; i < n; i++) a.push(mu + sd * randn());
    return a;
  }
  function mean(a) { var s = 0; for (var i = 0; i < a.length; i++) s += a[i]; return s / a.length; }
  function variance(a, m) {
    var s = 0; for (var i = 0; i < a.length; i++) s += (a[i] - m) * (a[i] - m);
    return s / (a.length - 1);
  }
  function lgamma(x) {
    var g = 7, c = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
      -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
    if (x < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * x)) - lgamma(1 - x);
    x -= 1;
    var a = c[0], t = x + g + 0.5;
    for (var i = 1; i < g + 2; i++) a += c[i] / (x + i);
    return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
  }
  function betacf(a, b, x) {
    var FPMIN = 1e-300, qab = a + b, qap = a + 1, qam = a - 1, c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < FPMIN) d = FPMIN;
    d = 1 / d;
    var h = d;
    for (var m = 1; m <= 300; m++) {
      var m2 = 2 * m, aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      var del = d * c; h *= del;
      if (Math.abs(del - 1) < 3e-14) break;
    }
    return h;
  }
  // Regularyzowana niekompletna funkcja beta I_x(a, b).
  function ibeta(x, a, b) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    var bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
    if (x < (a + 1) / (a + b + 2)) return bt * betacf(a, b, x) / a;
    return 1 - bt * betacf(b, a, 1 - x) / b;
  }
  // Dwustronny test t Welcha: {d, t, df, p}.
  function welch(a, b) {
    var ma = mean(a), mb = mean(b), va = variance(a, ma) / a.length, vb = variance(b, mb) / b.length;
    var t = (ma - mb) / Math.sqrt(va + vb);
    var df = (va + vb) * (va + vb) / (va * va / (a.length - 1) + vb * vb / (b.length - 1));
    return { d: ma - mb, t: t, df: df, p: ibeta(df / (df + t * t), df / 2, 0.5) };
  }
  // Jednoczynnikowa ANOVA (klasyczny test F): {F, p}.
  function anova(groups) {
    var all = [].concat.apply([], groups), gm = mean(all), k = groups.length, N = all.length;
    var ssb = 0, ssw = 0;
    groups.forEach(function (g) {
      var m = mean(g);
      ssb += g.length * (m - gm) * (m - gm);
      g.forEach(function (v) { ssw += (v - m) * (v - m); });
    });
    var d1 = k - 1, d2 = N - k, F = (ssb / d1) / (ssw / d2);
    return { F: F, p: ibeta(d2 / (d2 + d1 * F), d2 / 2, d1 / 2) };
  }

  // --- wspólne elementy rysunku --------------------------------------------
  function person(g, x, y, cls) {
    svg("circle", { cx: x, cy: y - 18, r: 8, class: "lc-sc-person " + (cls || "") }, g);
    svg("rect", { x: x - 10, y: y - 8, width: 20, height: 28, rx: 8, class: "lc-sc-person " + (cls || "") }, g);
  }
  function axisX(g, x0, x1, y, ticks) {
    svg("line", { x1: x0, x2: x1, y1: y, y2: y, class: "lc-sc-axis" }, g);
    ticks.forEach(function (t) {
      svg("line", { x1: t.x, x2: t.x, y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
      svg("text", { x: t.x, y: y + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, t.label);
    });
  }
  // Słupki histogramu: counts[i], x(i) środek, isHit(i) kolor zdarzenia.
  function bars(g, counts, PL, PR, PT, PB, slotX, bw, isHit) {
    var mx = Math.max.apply(null, counts.concat([1])), ymax = niceMax(mx * 1.1);
    function y(v) { return PB - (PB - PT) * v / ymax; }
    [0, 0.5, 1].forEach(function (f) {
      var v = ymax * f;
      svg("line", { x1: PL, x2: PR, y1: y(v), y2: y(v), class: "lc-sc-grid" }, g);
      svg("text", { x: PL - 8, y: y(v) + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, String(Math.round(v)));
    });
    counts.forEach(function (c, i) {
      if (c > 0) svg("rect", { x: slotX(i) - bw / 2, y: y(c), width: bw, height: PB - y(c),
        class: "lc-sc-bar" + (isHit(i) ? " is-hit" : "") }, g);
    });
  }

  var KINDS = {};

  // =========================================================================
  // PERM: 80 kartek z wynikami testu, tasowanie na dwa stosy po 40
  // =========================================================================
  KINDS.perm = function (cfg, api) {
    var PL = 60, PR = 620, PT = 292, PB = 404, LO = -12, HI = 12, NB = HI - LO;
    var st = {};

    function load(key) {
      var s = cfg.sets[key];
      st.key = key; st.dobs = Number(key);
      st.vals = s.p.concat(s.b);                                     // wynik na kartce
      st.grp = s.p.map(function () { return 0; }).concat(s.b.map(function () { return 1; }));
      st.T = st.vals.reduce(function (a, v) { return a + v; }, 0);
      st.slot = st.vals.map(function (_, i) { return i; });          // kartka i → miejsce
      st.orig = true; st.counts = new Array(NB).fill(0); st.total = 0; st.hits = 0;
      st.log = []; st.D = Dof(st.slot);
    }
    // D = 40 · d (całkowite), d = średnia stosu 1 - średnia stosu 2.
    function Dof(slot) {
      var s1 = 0;
      for (var i = 0; i < slot.length; i++) if (slot[i] < 40) s1 += st.vals[i];
      return 2 * s1 - st.T;
    }
    function meanOf(slot, stack) {
      var s = 0;
      for (var i = 0; i < slot.length; i++) if ((slot[i] < 40 ? 0 : 1) === stack) s += st.vals[i];
      return s / 40;
    }
    function bin(D) {
      var m = Math.floor(Math.abs(D) / 40), b = D >= 0 ? -LO + m : -LO - 1 - m;
      return Math.max(0, Math.min(NB - 1, b));
    }
    function isHitBin(b) { var lo = LO + b; return lo >= st.dobs || lo + 1 <= -st.dobs; }
    function record(D) {
      st.counts[bin(D)] += 1; st.total += 1;
      if (Math.abs(D) >= 40 * st.dobs) st.hits += 1;
      st.log.unshift(D / 40); if (st.log.length > 6) st.log.pop();
    }
    function randSlots() {
      var idx = st.vals.map(function (_, i) { return i; });
      for (var i = idx.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1)), t = idx[i]; idx[i] = idx[j]; idx[j] = t;
      }
      var slot = new Array(idx.length);
      idx.forEach(function (card, s) { slot[card] = s; });
      return slot;
    }
    function slotXY(s) {
      var stack = s < 40 ? 0 : 1, j = s % 40;
      return { x: (stack ? 352 : 76) + (j % 8) * 32, y: 46 + Math.floor(j / 8) * 24 };
    }
    function slotX(b) { return PL + (PR - PL) * (b + 0.5) / NB; }
    function dx(d) { return PL + (PR - PL) * (d - LO) / NB; }

    function drawStage(g, pos) {
      var step = api.step();
      person(g, 30, 118, "is-lady");
      svg("rect", { x: 22, y: 118, width: 18, height: 12, rx: 2, class: "lc-sc-card is-pile" }, g);
      if (st.orig) {
        svg("text", { x: 76 + 124, y: 30, "text-anchor": "middle", class: "lc-sc-head is-a" }, g, "telefon w plecaku");
        svg("text", { x: 352 + 124, y: 30, "text-anchor": "middle", class: "lc-sc-head is-b" }, g, "telefon na biurku");
      } else {
        svg("text", { x: 76 + 124, y: 30, "text-anchor": "middle", class: "lc-sc-head" }, g, "stos 1");
        svg("text", { x: 352 + 124, y: 30, "text-anchor": "middle", class: "lc-sc-head" }, g, "stos 2");
      }
      st.vals.forEach(function (v, i) {
        var p = pos ? pos[i] : slotXY(st.slot[i]);
        var q = svg("g", { transform: "translate(" + p.x + "," + p.y + ")" }, g);
        svg("rect", { x: 0, y: 0, width: 28, height: 20, rx: 3, class: "lc-sc-card " + (st.grp[i] ? "is-b" : "is-a") }, q);
        svg("text", { x: 14, y: 14, "text-anchor": "middle", class: "lc-sc-card-t" }, q, String(v));
      });
      if (pos) return;
      var m1 = meanOf(st.slot, 0), m2 = meanOf(st.slot, 1), d = st.D / 40;
      svg("text", { x: 76 + 124, y: 184, "text-anchor": "middle", class: "lc-sc-sub" }, g, "średnia " + fmt(m1, 2));
      svg("text", { x: 352 + 124, y: 184, "text-anchor": "middle", class: "lc-sc-sub" }, g, "średnia " + fmt(m2, 2));
      var txt;
      if (step === 1) txt = "różnica średnich: " + fmt(d, 2) + " pkt";
      else if (st.orig) txt = "d_obs = " + fmt(m1, 2) + " - " + fmt(m2, 2) + " = " + fmt(d, 2) + " pkt";
      else txt = "d = " + fmt(m1, 2) + " - " + fmt(m2, 2) + " = " + fmt(d, 2) + " pkt";
      svg("text", { x: 340, y: 212, "text-anchor": "middle", class: "lc-sc-read" + (st.orig ? "" : " is-plain") }, g, txt);
      if (!st.orig && step >= 2) svg("text", { x: 340, y: 232, "text-anchor": "middle", class: "lc-sc-sub" }, g,
        "kartki rozdane na ślepo, bez patrzenia, gdzie leżał telefon (d_obs = " + fmt(st.dobs, 2) + ")");
    }

    function drawLow(g, step) {
      if (step < 3) {
        if (step === 1) {
          svg("text", { x: W / 2, y: 280, "text-anchor": "middle", class: "lc-sc-sub" }, g,
            "Asystentka ma 80 kartek z wynikami testu koncentracji, po jednej na studenta.");
          svg("text", { x: W / 2, y: 302, "text-anchor": "middle", class: "lc-sc-sub" }, g,
            "Kolor kartki mówi, gdzie leżał telefon tej osoby.");
          return;
        }
        svg("text", { x: W / 2, y: 274, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          st.log.length ? "ostatnie tasowania:" : "Przetasuj kartki i rozdaj je na dwa stosy po 40.");
        st.log.forEach(function (d, i) {
          svg("text", { x: 120 + (i % 3) * 200, y: 304 + Math.floor(i / 3) * 26, "text-anchor": "middle",
            class: "lc-sc-log" + (i === 0 ? " is-x" : "") }, g, "d = " + fmt(d, 2));
        });
        return;
      }
      bars(g, st.counts, PL, PR, PT, PB, slotX, (PR - PL) / NB * 0.86,
        function (b) { return step >= 4 && isHitBin(b); });
      var ticks = [];
      for (var t = LO; t <= HI; t += 4) ticks.push({ x: dx(t), label: String(t) });
      axisX(g, PL, PR, PB, ticks);
      svg("text", { x: (PL + PR) / 2, y: PB + 36, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "d po tasowaniu (pkt)");
      svg("text", { x: PR, y: PT - 36, "text-anchor": "end", class: "lc-sc-n" }, g, "tasowań: " + st.total);
      if (step >= 4) {
        [st.dobs, -st.dobs].forEach(function (v, i) {
          svg("line", { x1: dx(v), x2: dx(v), y1: PT - 8, y2: PB, class: "lc-sc-obs" + (i ? " is-mirror" : "") }, g);
          svg("text", { x: dx(v), y: PT - 10, "text-anchor": "middle", class: "lc-sc-obs-t" }, g,
            i ? fmt(v, 0) : "d_obs = " + fmt(v, 0));
        });
        svg("text", { x: PL, y: PT - 36, class: "lc-sc-n is-hit" }, g,
          st.total ? ("|d| ≥ " + fmt(st.dobs, 0) + ": " + st.hits + " z " + st.total + " → p ≈ " + fmt(st.hits / st.total, 3))
                   : ("tasowania co najmniej tak skrajne jak d_obs = " + fmt(st.dobs, 0)));
      }
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, null);
      drawLow(api.low, step);
    }

    // Kartki przechodzą z bieżących miejsc na nowe.
    function move(newSlot, orig, ms, done) {
      var from = st.vals.map(function (_, i) { return slotXY(st.slot[i]); });
      var to = newSlot.map(function (s) { return slotXY(s); });
      var g = api.stage;
      tween(ms, function (u) {
        var e = ease(u);
        g.textContent = "";
        drawStage(g, from.map(function (p, i) {
          return { x: p.x + (to[i].x - p.x) * e, y: p.y + (to[i].y - p.y) * e - Math.sin(Math.PI * u) * 18 * (i % 3) };
        }));
      }, function () {
        st.slot = newSlot; st.orig = orig; st.D = Dof(newSlot);
        done();
      });
    }

    function land(fast, done) {
      var step = api.step();
      if (step < 3) { st.log.unshift(st.D / 40); if (st.log.length > 6) st.log.pop(); render(); done(); return; }
      render();
      var b = bin(st.D);
      flyToken(api, 340, 212, slotX(b), PB - 10, fast ? 120 : 380, function () { record(st.D); render(); done(); });
    }

    load(cfg.set || Object.keys(cfg.sets)[0]);

    return {
      render: render,
      reset: function () { load(st.key); render(); },
      opt: function (name, v) { if (name === "set") { load(v); render(); } },
      go: function (done) {
        if (api.step() === 1) {
          move(st.vals.map(function (_, i) { return i; }), true, 700, function () { render(); done(); });
          return;
        }
        move(randSlots(), false, 650, function () { land(false, done); });
      },
      many: function (m, done) {
        if (m > 10) {
          var slot;
          for (var k = 0; k < m; k++) { slot = randSlots(); record(Dof(slot)); }
          st.slot = slot; st.orig = false; st.D = Dof(slot);
          render(); done(); return;
        }
        var left = m;
        (function next() {
          if (left-- <= 0) { done(); return; }
          move(randSlots(), false, 220, function () { land(true, next); });
        })();
      }
    };
  };

  // =========================================================================
  // LABS: kolejne pracownie powtarzają eksperyment z telefonem
  // =========================================================================
  KINDS.labs = function (cfg, api) {
    var XL = 210, XR = 620, VLO = 20, VHI = 120;
    var GX = 34, GY = 262, CW = 27, CH = 21;          // siatka pracowni 10 × 10
    var st = { n: cfg.n || 40, alpha: cfg.alpha || 0.05, world: Number(cfg.world || 0),
      studies: [], last: null, id: 0 };

    function sx(v) { return XL + (XR - XL) * (Math.max(VLO, Math.min(VHI, v)) - VLO) / (VHI - VLO); }
    function run() {
      var e = cfg.effect, a, b;
      if (st.world) { a = sample(st.n, e.mp, e.sp); b = sample(st.n, e.mb, e.sb); }
      else { a = sample(st.n, cfg.mu0, cfg.sd0); b = sample(st.n, cfg.mu0, cfg.sd0); }
      var r = welch(a, b);
      st.id += 1;
      return { id: st.id, a: a, b: b, d: r.d, p: r.p, alarm: r.p < st.alpha, w: st.world,
        ja: a.map(function () { return Math.random() * 2 - 1; }), jb: b.map(function () { return Math.random() * 2 - 1; }) };
    }
    function keep(s) { st.studies.push({ w: s.w, alarm: s.alarm }); }

    function lamp(g, x, y, r, state) {
      if (state === 1) {
        for (var i = 0; i < 8; i++) {
          var t = i * Math.PI / 4;
          svg("line", { x1: x + Math.cos(t) * (r + 3), y1: y + Math.sin(t) * (r + 3),
            x2: x + Math.cos(t) * (r + 9), y2: y + Math.sin(t) * (r + 9), class: "lc-sc-ray" }, g);
        }
      }
      svg("circle", { cx: x, cy: y, r: r, class: "lc-sc-lamp" + (state === 1 ? " is-on" : state === 0 ? " is-off" : "") }, g);
    }
    function house(g, x, y, s, alarm, mark) {
      var q = svg("g", { transform: "translate(" + x + "," + y + ")" }, g);
      svg("rect", { x: -s * 0.5, y: 0, width: s, height: s * 0.62, class: "lc-sc-lab" + (mark ? " is-world" : "") }, q);
      svg("path", { d: "M " + (-s * 0.62) + " 0 L 0 " + (-s * 0.42) + " L " + (s * 0.62) + " 0 Z", class: "lc-sc-roof" }, q);
      svg("circle", { cx: 0, cy: s * 0.32, r: s * 0.17, class: "lc-sc-lamp" + (alarm ? " is-on" : " is-off") }, q);
    }

    function drawStage(g, s, shown) {
      var step = api.step();
      // pracownia z lampką
      var q = svg("g", {}, g);
      svg("rect", { x: 22, y: 92, width: 116, height: 96, class: "lc-sc-room" }, q);
      svg("path", { d: "M 14 92 L 80 54 L 146 92 Z", class: "lc-sc-roof" }, q);
      svg("rect", { x: 66, y: 146, width: 28, height: 42, class: "lc-sc-door" }, q);
      svg("text", { x: 80, y: 120, "text-anchor": "middle", class: "lc-sc-sign-s" }, q,
        s ? "pracownia " + s.id : "pracownia");
      var lit = s && shown >= 2 ? (s.alarm ? 1 : 0) : -1;
      svg("line", { x1: 80, x2: 80, y1: 46, y2: 56, class: "lc-sc-axis" }, g);
      lamp(g, 80, 34, 12, lit);
      if (lit >= 0) svg("text", { x: 112, y: 30, class: "lc-sc-verdict" + (lit ? " is-hit" : "") }, g, lit ? "alarm" : "cisza");
      person(g, 166, 182, "");
      // dwie grupy studentów na osi wyniku
      [["plecak", 72], ["biurko", 128]].forEach(function (r, k) {
        svg("text", { x: XL - 8, y: r[1] + 4, "text-anchor": "end", class: "lc-sc-sub" }, g, r[0]);
        svg("line", { x1: XL, x2: XR, y1: r[1], y2: r[1], class: "lc-sc-grid" }, g);
        if (!s || shown < 1) return;
        var v = k ? s.b : s.a, jt = k ? s.jb : s.ja, m = mean(v);
        v.forEach(function (x, i) {
          svg("circle", { cx: sx(x), cy: r[1] + jt[i] * 12, r: st.n > 50 ? 2.4 : 3.2, class: "lc-sc-pdot " + (k ? "is-b" : "is-a") }, g);
        });
        svg("line", { x1: sx(m), x2: sx(m), y1: r[1] - 20, y2: r[1] + 20, class: "lc-sc-mean" }, g);
      });
      var ticks = [];
      for (var t = 30; t <= 110; t += 20) ticks.push({ x: sx(t), label: String(t) });
      axisX(g, XL, XR, 158, ticks);
      svg("text", { x: (XL + XR) / 2, y: 193, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "wynik testu koncentracji (pkt), " + st.n + " osób w grupie");
      if (!s || shown < 2) {
        svg("text", { x: 400, y: 222, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          s ? "liczymy test…" : "Pracownia czeka na swój eksperyment.");
        return;
      }
      var txt = step === 1
        ? "różnica średnich " + fmt(s.d, 1) + " pkt · werdykt: " + (s.alarm ? "alarm" : "cisza")
        : "d = " + fmt(s.d, 1) + " pkt · " + fmtP(s.p) + (s.alarm ? " < " : " ≥ ") + "α = " + st.alpha + " → " + (s.alarm ? "alarm" : "cisza");
      svg("text", { x: 400, y: 224, "text-anchor": "middle", class: "lc-sc-read" + (s.alarm ? "" : " is-plain") }, g, txt);
    }

    function drawLow(g, step) {
      if (step < 3) {
        svg("text", { x: W / 2, y: 274, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          step === 1 ? "Każde badanie: dwie grupy studentów, test i jedna decyzja pracowni."
                     : "Alarm, gdy p < α. Cisza, gdy p ≥ α.");
        (st.log || []).forEach(function (r, i) {
          svg("text", { x: W / 2, y: 304 + i * 24, "text-anchor": "middle", class: "lc-sc-log" + (i === 0 ? " is-x" : "") }, g, r);
        });
        return;
      }
      var list = st.studies.slice(-100), off = st.studies.length - list.length;
      for (var i = 0; i < 100; i++) {
        var cx = GX + (i % 10) * CW + CW / 2, cy = GY + Math.floor(i / 10) * CH;
        if (i < list.length) house(g, cx, cy + 6, 16, list[i].alarm, step >= 4 && list[i].w === 1);
        else svg("circle", { cx: cx, cy: cy + 10, r: 1.6, class: "lc-sc-slot" }, g);
      }
      svg("text", { x: GX, y: GY - 14, class: "lc-sc-n" }, g,
        off ? "ostatnie 100 pracowni" : "pracownie");
      var N = st.studies.length, A = st.studies.filter(function (s) { return s.alarm; }).length;
      var X0 = 340;
      if (step === 3) {
        svg("text", { x: X0, y: GY + 10, class: "lc-sc-n" }, g, "badań: " + N);
        svg("text", { x: X0, y: GY + 44, class: "lc-sc-big-n is-hit" }, g, "alarmów: " + A);
        svg("text", { x: X0, y: GY + 72, class: "lc-sc-n" }, g, "odsetek alarmów: " + pct(A, N));
        svg("text", { x: X0, y: GY + 104, class: "lc-sc-n" }, g, "ciszy: " + (N - A));
        svg("text", { x: X0, y: GY + 150, class: "lc-sc-sub" }, g, "Czy telefon naprawdę działa?");
        svg("text", { x: X0, y: GY + 170, class: "lc-sc-sub" }, g, "Pracownie tego nie wiedzą.");
        return;
      }
      // krok 4: tabela świat × decyzja
      var c = [[0, 0], [0, 0]];
      st.studies.forEach(function (s) { c[s.w][s.alarm ? 0 : 1] += 1; });
      var cols = [X0 + 118, X0 + 172, X0 + 232];
      svg("text", { x: cols[0], y: GY + 4, "text-anchor": "middle", class: "lc-sc-th-t is-hit" }, g, "alarm");
      svg("text", { x: cols[1], y: GY + 4, "text-anchor": "middle", class: "lc-sc-th-t" }, g, "cisza");
      svg("text", { x: cols[2], y: GY + 4, "text-anchor": "middle", class: "lc-sc-th-t" }, g, "% alarmów");
      var key = st.n + "_" + st.alpha, pw = cfg.power[key];
      [["telefon", "nie działa"], ["telefon działa", "(-" + cfg.effect.diff + " pkt)"]].forEach(function (lab, w) {
        var y = GY + 40 + w * 74, tot = c[w][0] + c[w][1];
        svg("rect", { x: X0 - 6, y: y - 22, width: 290, height: 60, rx: 4, class: "lc-sc-cellbg" + (w ? " is-world" : "") }, g);
        svg("text", { x: X0, y: y - 4, class: "lc-sc-th-t" }, g, lab[0]);
        svg("text", { x: X0, y: y + 12, class: "lc-sc-th-t" }, g, lab[1]);
        svg("text", { x: cols[0], y: y + 4, "text-anchor": "middle", class: "lc-sc-cell is-hi" }, g, String(c[w][0]));
        svg("text", { x: cols[1], y: y + 4, "text-anchor": "middle", class: "lc-sc-cell" }, g, String(c[w][1]));
        svg("text", { x: cols[2], y: y + 4, "text-anchor": "middle", class: "lc-sc-cell is-hi" }, g, pct(c[w][0], tot));
        svg("text", { x: cols[2], y: y + 26, "text-anchor": "middle", class: "lc-sc-theory-t" }, g,
          w ? "moc ≈ " + fmt(100 * pw, 0) + "%" : "α = " + fmt(100 * st.alpha, 0) + "%");
      });
      svg("rect", { x: GX, y: GY + 10 * CH + 4, width: 16, height: 10, class: "lc-sc-lab is-world" }, g);
      svg("text", { x: GX + 22, y: GY + 10 * CH + 13, class: "lc-sc-n" }, g, "obwódka: telefon działał");
    }

    function render(shown) {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, st.last, shown === undefined ? 2 : shown);
      drawLow(api.low, step);
    }
    function logLine(s) {
      st.log = st.log || [];
      st.log.unshift("pracownia " + s.id + ": d = " + fmt(s.d, 1) + " pkt, " + fmtP(s.p) + ", " + (s.alarm ? "alarm" : "cisza"));
      if (st.log.length > 5) st.log.pop();
    }
    function cellXY(k) {
      var i = Math.min(k, 99);
      return { x: GX + (i % 10) * CW + CW / 2, y: GY + Math.floor(i / 10) * CH + 10 };
    }
    function study(fast, done) {
      var s = run();
      st.last = s;
      render(1);
      wait(fast ? 60 : 450, function () {
        render(2);
        var step = api.step();
        if (step < 3) { keep(s); logLine(s); render(); done(); return; }
        var to = cellXY(st.studies.length);
        flyToken(api, 80, 34, to.x, to.y, fast ? 140 : 420, function () { keep(s); logLine(s); render(); done(); });
      });
    }
    function clear() { st.studies = []; st.last = null; st.log = []; st.id = 0; }

    return {
      render: function () { render(); },
      reset: function () { clear(); render(); },
      opt: function (name, v) {
        // n i α zmieniają procedurę: czyścimy liczniki. Świat nie czyści, żeby tabela w kroku 4
        // zebrała badania z obu światów.
        if (name === "n") { st.n = Number(v); clear(); }
        if (name === "alpha") { st.alpha = Number(v); clear(); }
        if (name === "world") { st.world = Number(v); st.last = null; }
        render();
      },
      go: function (done) { study(false, done); },
      many: function (m, done) {
        if (m > 10) {
          for (var i = 0; i < m; i++) { var s = run(); keep(s); if (i === m - 1) { st.last = s; logLine(s); } }
          render(); done(); return;
        }
        var left = m;
        (function next() {
          if (left-- <= 0) { done(); return; }
          study(true, next);
        })();
      }
    };
  };

  // =========================================================================
  // PAIRS: k kierunków o tej samej średniej, testy t dla wszystkich par
  // =========================================================================
  KINDS.pairs = function (cfg, api) {
    var CX = 186, CY = 120, R = 70, NODE = 20;
    var PL = 70, PR = 610, PT = 292, PB = 380, NB = 6;
    var st = { k: cfg.k || 5, mode: cfg.mode || "pairs", series: 0, counts: new Array(NB).fill(0),
      any: 0, last: null, log: [] };

    function m() { return st.k * (st.k - 1) / 2; }
    function node(i) {
      var t = -Math.PI / 2 + 2 * Math.PI * i / st.k;
      var c = Math.cos(t), x = CX + R * c, y = CY + R * Math.sin(t);
      // podpis: z boku węzła (lewa/prawa strona) albo nad / pod nim
      if (c > 0.3) return { x: x, y: y, lx: x + NODE + 6, ly: y + 4, la: "start" };
      if (c < -0.3) return { x: x, y: y, lx: x - NODE - 6, ly: y + 4, la: "end" };
      return { x: x, y: y, lx: x, ly: y + (Math.sin(t) < 0 ? -NODE - 8 : NODE + 18), la: "middle" };
    }
    function run() {
      var g = [];
      for (var i = 0; i < st.k; i++) g.push(sample(cfg.n, cfg.mu, cfg.sd));
      var res = { means: g.map(mean), pairs: [], alarms: 0 };
      if (st.mode === "anova") {
        var a = anova(g);
        res.F = a.F; res.p = a.p; res.alarms = a.p < cfg.alpha ? 1 : 0;
      } else {
        for (var x = 0; x < st.k; x++) for (var y = x + 1; y < st.k; y++) {
          var w = welch(g[x], g[y]);
          res.pairs.push({ i: x, j: y, p: w.p, hit: w.p < cfg.alpha });
          if (w.p < cfg.alpha) res.alarms += 1;
        }
      }
      st.series += 1; res.id = st.series;
      return res;
    }
    function keep(r) {
      st.counts[Math.min(NB - 1, r.alarms)] += 1;
      if (r.alarms > 0) st.any += 1;
    }
    function logLine(r) {
      var who = r.pairs.filter(function (p) { return p.hit; }).slice(0, 2)
        .map(function (p) { return cfg.names[p.i] + " - " + cfg.names[p.j]; }).join(", ");
      st.log.unshift("seria " + r.id + ": " + (st.mode === "anova" ? ("ANOVA, " + fmtP(r.p) + (r.alarms ? ", alarm" : ", cisza"))
        : ("alarmów " + r.alarms + (who ? " (" + who + (r.alarms > 2 ? ", …" : "") + ")" : ""))));
      if (st.log.length > 4) st.log.pop();
    }

    function drawStage(g, r, lit) {
      var step = api.step(), i, j;
      if (st.mode === "anova") {
        svg("circle", { cx: CX, cy: CY, r: R + 14, class: "lc-sc-ring" + (r && lit && r.alarms ? " is-hit" : "") }, g);
      } else {
        for (i = 0; i < st.k; i++) for (j = i + 1; j < st.k; j++) {
          var a = node(i), b = node(j), hit = false;
          if (r && lit) r.pairs.forEach(function (p) { if (p.i === i && p.j === j && p.hit) hit = true; });
          svg("line", { x1: a.x, y1: a.y, x2: b.x, y2: b.y, class: "lc-sc-edge" + (hit ? " is-hit" : "") }, g);
        }
      }
      for (i = 0; i < st.k; i++) {
        var n = node(i);
        svg("circle", { cx: n.x, cy: n.y, r: NODE, class: "lc-sc-node" }, g);
        svg("text", { x: n.x, y: n.y + 4, "text-anchor": "middle", class: "lc-sc-node-t" }, g,
          r ? fmt(r.means[i], 1) : "?");
        svg("text", { x: n.lx, y: n.ly, "text-anchor": n.la, class: "lc-sc-sub" }, g, cfg.names[i]);
      }
      // analityk i werdykt
      var X0 = 366;
      person(g, X0, 64, "");
      svg("text", { x: X0 + 24, y: 50, class: "lc-sc-sub" }, g, st.k + " kierunków, po " + cfg.n + " osób, ten sam test");
      svg("text", { x: X0 + 24, y: 70, class: "lc-sc-sub" }, g,
        st.mode === "anova" ? "jedna ANOVA dla wszystkich grup" :
          (step >= 2 ? "m = " + m() + " par, każda testem t przy α = " + cfg.alpha : m() + " par do porównania"));
      if (!r) return;
      if (!lit) { svg("text", { x: X0, y: 120, class: "lc-sc-sub" }, g, "liczymy testy…"); return; }
      if (st.mode === "anova") {
        svg("text", { x: X0, y: 118, class: "lc-sc-n" }, g, "F = " + fmt(r.F, 2) + ", " + fmtP(r.p));
      } else {
        svg("text", { x: X0, y: 118, class: "lc-sc-n" + (r.alarms ? " is-hit" : "") }, g,
          (step >= 2 ? "alarmów (p < " + cfg.alpha + "): " : "alarmów w tej serii: ") + r.alarms + " z " + m());
        r.pairs.filter(function (p) { return p.hit; }).slice(0, 3).forEach(function (p, q) {
          svg("text", { x: X0, y: 140 + q * 18, class: "lc-sc-n" }, g,
            cfg.names[p.i] + " - " + cfg.names[p.j] + ": " + fmtP(p.p));
        });
      }
      svg("text", { x: X0, y: 214, class: "lc-sc-read" + (r.alarms ? "" : " is-plain") }, g,
        r.alarms ? "był co najmniej jeden alarm" : "żadnego alarmu");
    }

    function binLabel(b) { return st.mode === "anova" ? (b ? "alarm" : "cisza") : (b === NB - 1 ? (NB - 1) + "+" : String(b)); }

    function drawLow(g, step) {
      if (step < 3) {
        svg("text", { x: W / 2, y: 268, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Wszystkie kierunki losujemy z tej samej populacji: średnia " + cfg.mu + " pkt, odchylenie " + cfg.sd + " pkt.");
        st.log.forEach(function (l, i) {
          svg("text", { x: W / 2, y: 300 + i * 24, "text-anchor": "middle", class: "lc-sc-log" + (i === 0 ? " is-x" : "") }, g, l);
        });
        return;
      }
      var nb = st.mode === "anova" ? 2 : NB, cnt = st.counts.slice(0, nb);
      var slotX = function (b) { return PL + (PR - PL) * (b + 0.5) / nb; };
      bars(g, cnt, PL, PR, PT, PB, slotX, Math.min(70, (PR - PL) / nb * 0.6), function (b) { return b >= 1; });
      axisX(g, PL, PR, PB, cnt.map(function (_, b) { return { x: slotX(b), label: binLabel(b) }; }));
      svg("text", { x: (PL + PR) / 2, y: PB + 36, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        st.mode === "anova" ? "wynik ANOVA w serii" : "liczba alarmów w serii");
      svg("text", { x: PR, y: PT - 22, "text-anchor": "end", class: "lc-sc-n" }, g, "serii: " + st.series);
      svg("text", { x: PL, y: PT - 22, class: "lc-sc-n is-hit" }, g,
        "serie z co najmniej jednym alarmem: " + st.any + (st.series ? " (" + pct(st.any, st.series) + ")" : ""));
      if (step < 4) return;
      // krok 4: odsetek serii z alarmem obok wzoru
      var theo = st.mode === "anova" ? cfg.alpha : 1 - Math.pow(1 - cfg.alpha, m());
      var BX0 = PL, BX1 = PR, BY = PB + 52, obs = st.series ? st.any / st.series : 0;
      svg("rect", { x: BX0, y: BY, width: BX1 - BX0, height: 12, class: "lc-sc-pbar-bg" }, g);
      if (st.series) svg("rect", { x: BX0, y: BY, width: (BX1 - BX0) * obs, height: 12, class: "lc-sc-pbar is-hit" }, g);
      var tx = BX0 + (BX1 - BX0) * theo;
      svg("line", { x1: tx, x2: tx, y1: BY - 6, y2: BY + 18, class: "lc-sc-param" }, g);
      svg("text", { x: BX0, y: BY + 34, class: "lc-sc-n is-hit" }, g, "odsetek serii z alarmem: " + pct(st.any, st.series));
      svg("text", { x: BX1, y: BY + 34, "text-anchor": "end", class: "lc-sc-param-t" }, g,
        st.mode === "anova" ? "α = " + fmt(100 * theo, 1) + "%"
          : "1 - 0.95^" + m() + " = " + fmt(100 * theo, 1) + "% (przybliżenie)");
    }

    function render(lit) {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, st.last, lit === undefined ? true : lit);
      drawLow(api.low, step);
    }
    function series(fast, done) {
      var r = run();
      st.last = r;
      render(false);
      wait(fast ? 60 : 500, function () {
        render(true);
        if (api.step() < 3) { keep(r); logLine(r); render(); done(); return; }
        var nb = st.mode === "anova" ? 2 : NB, b = Math.min(NB - 1, r.alarms);
        var x1 = PL + (PR - PL) * (b + 0.5) / nb;
        flyToken(api, 450, 210, x1, PB - 10, fast ? 140 : 420, function () { keep(r); logLine(r); render(); done(); });
      });
    }
    function clear() { st.series = 0; st.counts = new Array(NB).fill(0); st.any = 0; st.last = null; st.log = []; }

    return {
      render: function () { render(); },
      reset: function () { clear(); render(); },
      opt: function (name, v) {
        if (name === "k") st.k = Number(v);
        if (name === "mode") st.mode = v;
        clear(); render();
      },
      go: function (done) { series(false, done); },
      many: function (m, done) {
        if (m > 10) {
          for (var i = 0; i < m; i++) { var r = run(); keep(r); if (i === m - 1) { st.last = r; logLine(r); } }
          render(); done(); return;
        }
        var left = m;
        (function next() {
          if (left-- <= 0) { done(); return; }
          series(true, next);
        })();
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

    new MutationObserver(function () { if (!busy) refresh(); })
      .observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
  }

  function scan() { document.querySelectorAll(".lc-sc:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
