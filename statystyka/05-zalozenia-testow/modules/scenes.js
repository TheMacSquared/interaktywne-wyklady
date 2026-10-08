// Sceny wykładu 05: konkretne doświadczenie → werdykt testu → odsetek fałszywych alarmów.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind:
//   "welch"  brygadzista mierzy dwie zmiany o tej samej średniej i różnym rozrzucie;
//            test t Studenta albo Welcha ogłasza „różnica!” lub „brak różnicy”
// Numer kroku czyta z data-lc-step korzenia widgetu. Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji, np. test:welch)
// Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
// Silnik (init, tween, svg) skopiowany ze sceny wykładu 00.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H = 470;
  var REDUCE = typeof window !== "undefined" && window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

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

  // --- rozkład t: regularyzowana niekompletna funkcja beta -------------------
  // lgamma: przybliżenie Lanczosa (g = 7, 9 współczynników), błąd względny < 1e-14.
  var LZ = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
    -176.61502916214059, 12.507343278686905, -0.13857109526572012,
    9.9843695780195716e-6, 1.5056327351493116e-7];
  function lgamma(x) {
    if (x < 0.5) return Math.log(Math.PI / Math.abs(Math.sin(Math.PI * x))) - lgamma(1 - x);
    x -= 1;
    var a = LZ[0], t = x + 7.5;
    for (var i = 1; i < 9; i++) a += LZ[i] / (x + i);
    return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
  }
  // ułamek łańcuchowy (algorytm Lentza) dla I_x(a, b)
  function betacf(a, b, x) {
    var TINY = 1e-300, EPS = 1e-15;
    var qab = a + b, qap = a + 1, qam = a - 1, c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < TINY) d = TINY;
    d = 1 / d;
    var h = d;
    for (var m = 1; m <= 300; m++) {
      var m2 = 2 * m, aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < TINY) d = TINY;
      c = 1 + aa / c; if (Math.abs(c) < TINY) c = TINY;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < TINY) d = TINY;
      c = 1 + aa / c; if (Math.abs(c) < TINY) c = TINY;
      d = 1 / d;
      var del = d * c;
      h *= del;
      if (Math.abs(del - 1) < EPS) break;
    }
    return h;
  }
  function ibeta(x, a, b) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    var bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
    return x < (a + 1) / (a + b + 2) ? bt * betacf(a, b, x) / a : 1 - bt * betacf(b, a, 1 - x) / b;
  }
  // dystrybuanta rozkładu t (jak pt() w R) i dwustronna p-wartość
  function pt(t, df) {
    var tail = 0.5 * ibeta(df / (df + t * t), df / 2, 0.5);
    return t < 0 ? tail : 1 - tail;
  }
  function p2(t, df) { return ibeta(df / (df + t * t), df / 2, 0.5); }

  function rnorm() {
    var u = 0, v = 0;
    while (u === 0) u = Math.random();
    while (v === 0) v = Math.random();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }
  function meanVar(x) {
    var n = x.length, m = 0, s = 0, i;
    for (i = 0; i < n; i++) m += x[i];
    m /= n;
    for (i = 0; i < n; i++) s += (x[i] - m) * (x[i] - m);
    return { n: n, m: m, v: s / (n - 1) };
  }
  // test t dla dwóch grup: wersja Studenta (wspólna wariancja) i Welcha
  function ttest(a, b, welch) {
    var A = meanVar(a), B = meanVar(b), se, df;
    if (welch) {
      var va = A.v / A.n, vb = B.v / B.n;
      se = Math.sqrt(va + vb);
      df = (va + vb) * (va + vb) / (va * va / (A.n - 1) + vb * vb / (B.n - 1));
    } else {
      var sp2 = ((A.n - 1) * A.v + (B.n - 1) * B.v) / (A.n + B.n - 2);
      se = Math.sqrt(sp2 * (1 / A.n + 1 / B.n));
      df = A.n + B.n - 2;
    }
    var t = (A.m - B.m) / se;
    return { t: t, df: df, p: p2(t, df), ma: A.m, mb: B.m };
  }

  var KINDS = {};

  function axisX(g, x0, x1, y, ticks) {
    svg("line", { x1: x0, x2: x1, y1: y, y2: y, class: "lc-sc-axis" }, g);
    ticks.forEach(function (t) {
      svg("line", { x1: t.x, x2: t.x, y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
      svg("text", { x: t.x, y: y + 19, "text-anchor": t.anchor || "middle", class: "lc-sc-tick" }, g, t.label);
    });
  }

  // =========================================================================
  // WELCH: dwie zmiany w hali montażowej
  // =========================================================================
  KINDS.welch = function (cfg, api) {
    var MU = cfg.mu || 100, SDA = cfg.sd_chaos || 20, SDB = cfg.sd_calm || 5;
    var LAY = cfg.layouts || { eq: [50, 50], few: [20, 80], many: [80, 20] };
    var ALPHA = cfg.alpha || 0.05, NB = 20;
    // pasy pomiarów
    var SX0 = 190, SX1 = 610, XMIN = MU - 60, XMAX = MU + 60;
    var ROWA = 84, ROWB = 160, BAND = 18, AXY = 200;
    // licznik i histogram p-wartości
    var PL = 60, PR = 610, GY = 272, GMAX = 0.5, PT = 344, PB = 436;
    var LAMP = [92, 192];

    var st = { lay: cfg.layout || "few", test: cfg.test || "student", runs: [], counts: new Array(NB).fill(0),
      alarms: 0, last: null };

    function nA() { return LAY[st.lay][0]; }
    function nB() { return LAY[st.lay][1]; }
    function xOf(v) { return SX0 + (SX1 - SX0) * (Math.max(XMIN, Math.min(XMAX, v)) - XMIN) / (XMAX - XMIN); }
    function binOf(p) { return Math.max(0, Math.min(NB - 1, Math.floor(p * NB))); }
    function slotX(i) { return PL + (PR - PL) * (i + 0.5) / NB; }
    function testName() { return st.test === "welch" ? "Welch" : "Student"; }

    // jeden pomiar obu zmian
    function measure() {
      var a = [], b = [], ja = [], jb = [], i;
      for (i = 0; i < nA(); i++) { a.push(MU + SDA * rnorm()); ja.push(Math.random() * 2 - 1); }
      for (i = 0; i < nB(); i++) { b.push(MU + SDB * rnorm()); jb.push(Math.random() * 2 - 1); }
      var r = ttest(a, b, st.test === "welch");
      return { a: a, b: b, ja: ja, jb: jb, ma: r.ma, mb: r.mb, t: r.t, df: r.df, p: r.p, alarm: r.p < ALPHA };
    }

    function foreman(g) {
      svg("rect", { x: 14, y: 12, width: 132, height: 24, rx: 4, class: "lc-sc-sign" }, g);
      svg("text", { x: 80, y: 29, "text-anchor": "middle", class: "lc-sc-sign-t is-small" }, g, "HALA MONTAŻU");
      var x = 62, y = 96;
      svg("rect", { x: x - 13, y: y - 44, width: 26, height: 7, rx: 3, class: "lc-sc-helmet" }, g);
      svg("circle", { cx: x, cy: y - 30, r: 11, class: "lc-sc-person" }, g);
      svg("rect", { x: x - 15, y: y - 16, width: 30, height: 42, rx: 11, class: "lc-sc-person" }, g);
      svg("rect", { x: x - 11, y: y + 24, width: 9, height: 22, rx: 3, class: "lc-sc-person" }, g);
      svg("rect", { x: x + 2, y: y + 24, width: 9, height: 22, rx: 3, class: "lc-sc-person" }, g);
      // podkładka ze stoperem
      svg("rect", { x: x + 12, y: y - 8, width: 20, height: 26, rx: 2, class: "lc-sc-clip" }, g);
      svg("line", { x1: x + 16, x2: x + 28, y1: y, y2: y, class: "lc-sc-axis" }, g);
      svg("line", { x1: x + 16, x2: x + 28, y1: y + 6, y2: y + 6, class: "lc-sc-axis" }, g);
      svg("text", { x: x, y: y + 64, "text-anchor": "middle", class: "lc-sc-sub" }, g, "brygadzista");
    }

    function lamp(g, d, step) {
      var on = d && step >= 2;
      var cls = !on ? "lc-sc-lamp" : d.alarm ? "lc-sc-lamp is-alarm" : "lc-sc-lamp is-ok";
      if (on && d.alarm) svg("circle", { cx: LAMP[0], cy: LAMP[1], r: 22, class: "lc-sc-glow" }, g);
      svg("rect", { x: LAMP[0] - 6, y: LAMP[1] + 14, width: 12, height: 8, rx: 2, class: "lc-sc-lamp-base" }, g);
      svg("circle", { cx: LAMP[0], cy: LAMP[1], r: 14, class: cls }, g);
      if (on) {
        svg("text", { x: LAMP[0], y: LAMP[1] + 42, "text-anchor": "middle",
          class: d.alarm ? "lc-sc-verdict is-alarm" : "lc-sc-verdict is-ok" }, g,
          d.alarm ? "różnica!" : "brak różnicy");
      }
    }

    function strips(g, step, d, upto) {
      [["zmiana chaotyczna", nA(), ROWA, "is-chaos", d && d.a, d && d.ja, d && d.ma],
       ["zmiana spokojna", nB(), ROWB, "is-calm", d && d.b, d && d.jb, d && d.mb]].forEach(function (r, k) {
        svg("rect", { x: SX0, y: r[2] - BAND - 4, width: SX1 - SX0, height: 2 * BAND + 8, rx: 4, class: "lc-sc-band" }, g);
        svg("text", { x: SX0, y: r[2] - BAND - 12, class: "lc-sc-rowlab" }, g, r[0] + " · " + r[1] + " osób");
        if (!r[4]) return;
        var lim = upto === undefined ? r[4].length : Math.min(r[4].length, Math.round(upto * r[4].length));
        for (var i = 0; i < lim; i++) {
          svg("circle", { cx: xOf(r[4][i]), cy: r[2] + r[5][i] * BAND, r: 3.6, class: "lc-sc-wdot " + r[3] }, g);
        }
        if (upto === undefined || upto >= 1) {
          var mx = xOf(r[6]);
          svg("line", { x1: mx, x2: mx, y1: r[2] - BAND - 3, y2: r[2] + BAND + 3, class: "lc-sc-mean" }, g);
          svg("text", { x: SX1, y: r[2] - BAND - 12, "text-anchor": "end", class: "lc-sc-meanlab" }, g,
            "średnio " + fmt(r[6], 1) + " min");
        }
      });
      var ticks = [];
      for (var v = XMIN; v <= XMAX; v += 20) ticks.push({ x: xOf(v), label: String(v) });
      axisX(g, SX0, SX1, AXY, ticks);
      svg("text", { x: SX1, y: AXY + 36, "text-anchor": "end", class: "lc-sc-axtitle" }, g, "czas montażu (min)");
      if (step >= 4) {
        var x = xOf(MU);
        svg("line", { x1: x, x2: x, y1: ROWA - BAND - 6, y2: AXY, class: "lc-sc-param" }, g);
        svg("text", { x: x, y: 40, "text-anchor": "middle", class: "lc-sc-param-t is-small is-halo" }, g,
          "prawdziwa średnia obu zmian: " + MU + " min");
      }
    }

    function drawStage(g, step, anim) {
      foreman(g);
      var d = st.last;
      lamp(g, anim && !anim.done ? null : d, step);
      strips(g, step, d, anim ? anim.u : undefined);
      if (!d) {
        svg("text", { x: (SX0 + SX1) / 2, y: ROWA + 4, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Zmierz obie zmiany, żeby zobaczyć czasy montażu.");
      }
    }

    function fmtP(p) { return p < 0.001 ? "p < 0.001" : "p = " + fmt(p, 3); }

    function drawLog(g, step) {
      if (!st.runs.length) return;
      var X = [30, 95, 195, 295, 395, 505];
      svg("text", { x: X[0], y: 268, class: "lc-sc-th-t" }, g, "pomiar");
      svg("text", { x: X[1], y: 268, class: "lc-sc-th-t" }, g, "chaotyczna");
      svg("text", { x: X[2], y: 268, class: "lc-sc-th-t" }, g, "spokojna");
      svg("text", { x: X[3], y: 268, class: "lc-sc-th-t" }, g, "różnica");
      if (step >= 2) {
        svg("text", { x: X[4], y: 268, class: "lc-sc-th-t" }, g, "test " + testName());
        svg("text", { x: X[5], y: 268, class: "lc-sc-th-t" }, g, "lampka");
      }
      st.runs.slice(-6).reverse().forEach(function (d, i) {
        var y = 296 + i * 26, g2 = svg("g", { opacity: 1 - i * 0.13 }, g);
        svg("text", { x: X[0], y: y, class: "lc-sc-log" }, g2, "nr " + d.no);
        svg("text", { x: X[1], y: y, class: "lc-sc-log" }, g2, fmt(d.ma, 1) + " min");
        svg("text", { x: X[2], y: y, class: "lc-sc-log" }, g2, fmt(d.mb, 1) + " min");
        svg("text", { x: X[3], y: y, class: "lc-sc-log" }, g2, (d.ma - d.mb >= 0 ? "+" : "") + fmt(d.ma - d.mb, 1) + " min");
        if (step >= 2) {
          svg("text", { x: X[4], y: y, class: "lc-sc-log" }, g2, fmtP(d.p));
          svg("text", { x: X[5], y: y, class: d.alarm ? "lc-sc-log is-alarm" : "lc-sc-log is-ok" }, g2,
            d.alarm ? "różnica!" : "brak różnicy");
        }
      });
    }

    function histInfo() {
      var mx = Math.max.apply(null, st.counts.concat([1]));
      if (api.step() >= 4) mx = Math.max(mx, st.runs.length / NB);
      var u = Math.min(12, (PB - PT - 8) / Math.max(mx, 6));
      return { u: u, tokR: Math.min(10, u * 0.46) };
    }
    function tokenY(c, h) { return PB - h.u * (c - 0.5) - 1; }

    function drawLow(g, step) {
      var N = st.runs.length, rate = N ? st.alarms / N : 0;
      // licznik alarmów
      svg("text", { x: PL, y: GY - 10, class: "lc-sc-n" }, g,
        "test " + testName() + " · pomiarów: " + N + " · alarmów: " + st.alarms +
        (N ? " (" + fmt(100 * rate, 1) + "%)" : ""));
      var gx = function (r) { return PL + (PR - PL) * Math.min(r, GMAX) / GMAX; };
      svg("rect", { x: PL, y: GY, width: PR - PL, height: 12, rx: 3, class: "lc-sc-track" }, g);
      if (N) svg("rect", { x: PL, y: GY, width: Math.max(2, gx(rate) - PL), height: 12, rx: 3, class: "lc-sc-gauge" }, g);
      [0, 0.1, 0.2, 0.3, 0.4, 0.5].forEach(function (t) {
        svg("text", { x: gx(t), y: GY + 28, "text-anchor": t === 0 ? "start" : t === 0.5 ? "end" : "middle",
          class: "lc-sc-tick" }, g, (100 * t) + "%" + (t === 0.5 ? "+" : ""));
      });
      if (step >= 4) {
        var ax = gx(ALPHA);
        svg("line", { x1: ax, x2: ax, y1: GY - 4, y2: GY + 16, class: "lc-sc-param" }, g);
        svg("text", { x: ax, y: GY + 28, "text-anchor": "middle", class: "lc-sc-param-t is-small" }, g, "α = 5%");
        var msg = "", cls = "lc-sc-n";
        if (N < 300) msg = "dołóż pomiarów (co najmniej 300), by porównać z α";
        else {
          // margines na przypadek: 3 błędy standardowe odsetka, nie mniej niż 1 punkt procentowy
          var tol = Math.max(0.01, 3 * Math.sqrt(ALPHA * (1 - ALPHA) / N));
          if (rate > ALPHA + tol) { msg = "za dużo fałszywych alarmów: " + fmt(rate / ALPHA, 1) + " × α"; cls += " is-hit"; }
          else if (rate < ALPHA - tol) { msg = "prawie nigdy nie alarmuje: test za ostrożny"; cls += " is-hit"; }
          else { msg = "zgodnie z α = 5%"; cls += " is-good"; }
        }
        svg("text", { x: PL, y: GY + 50, class: cls }, g, msg);
      }
      // histogram p-wartości
      var ticks = [0, 0.2, 0.4, 0.6, 0.8, 1].map(function (t) {
        return { x: PL + (PR - PL) * t, label: fmt(t, 1) };
      });
      axisX(g, PL, PR, PB, ticks);
      svg("text", { x: (PL + PR) / 2, y: PB + 32, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "p-wartość z kolejnych pomiarów (czerwone: p < 0.05, alarm)");
      var h = histInfo();
      for (var i = 0; i < NB; i++) {
        var cls2 = "lc-sc-ptok " + (i === 0 ? "is-alarm" : "is-ok");
        if (h.u >= 7) {
          for (var c = 1; c <= st.counts[i]; c++) {
            svg("circle", { cx: slotX(i), cy: tokenY(c, h), r: h.tokR, class: cls2 }, g);
          }
        } else if (st.counts[i] > 0) {
          var bw = (PR - PL) / NB * 0.8;
          svg("rect", { x: slotX(i) - bw / 2, y: PB - h.u * st.counts[i], width: bw,
            height: h.u * st.counts[i], class: cls2 + " is-bar" }, g);
        }
      }
      if (step >= 4 && N) {
        var y = PB - h.u * N / NB;
        svg("line", { x1: PL, x2: PR, y1: y, y2: y, class: "lc-sc-param" }, g);
        svg("text", { x: PR, y: y - 6, "text-anchor": "end", class: "lc-sc-param-t is-small is-halo" }, g,
          "test trzymający α: każdy słupek po 5%");
      }
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, null);
      if (step >= 3) drawLow(api.low, step); else drawLog(api.low, step);
    }

    function commit(d) {
      d.no = st.runs.length + 1;
      st.runs.push({ no: d.no, ma: d.ma, mb: d.mb, p: d.p, alarm: d.alarm });
      st.last = d;
      st.counts[binOf(d.p)] += 1;
      if (d.alarm) st.alarms += 1;
    }

    function runOne(fast, done) {
      var d = measure(), step = api.step();
      st.last = d;
      tween(fast ? 160 : 1100, function (u) {
        api.stage.textContent = "";
        drawStage(api.stage, step, { u: u, done: u >= 1 });
      }, function () {
        commit(d);
        if (step >= 3) {
          // żeton przelatuje od lampki do słupka (render pokaże go już w histogramie)
          st.counts[binOf(d.p)] -= 1;
          var h = histInfo(), i = binOf(d.p);
          var x0 = LAMP[0], y0 = LAMP[1], x1 = slotX(i), y1 = tokenY(st.counts[i] + 1, h);
          var tok = svg("circle", { r: 7, cx: x0, cy: y0, class: "lc-sc-ptok " + (i === 0 ? "is-alarm" : "is-ok") }, api.fly);
          render();
          tween(fast ? 150 : 450, function (u) {
            var e = ease(u);
            tok.setAttribute("cx", x0 + (x1 - x0) * e);
            tok.setAttribute("cy", y0 + (y1 - y0) * e);
          }, function () { api.fly.textContent = ""; st.counts[i] += 1; render(); done(); });
        } else { render(); done(); }
      });
    }

    function runMany(m) {
      for (var i = 0; i < m; i++) commit(measure());
      render();
    }

    return {
      render: render,
      reset: function () {
        st.runs = []; st.counts = new Array(NB).fill(0); st.alarms = 0; st.last = null; render();
      },
      opt: function (name, v) {
        if (name === "n" && LAY[v]) st.lay = v;
        if (name === "test") st.test = v === "welch" ? "welch" : "student";
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

  // do testów numerycznych w node (poza przeglądarką)
  if (typeof module !== "undefined" && module.exports) {
    module.exports = { pt: pt, p2: p2, ibeta: ibeta, ttest: ttest };
    return;
  }

  function scan() { document.querySelectorAll(".lc-sc:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
