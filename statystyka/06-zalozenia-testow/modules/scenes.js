// Sceny wykładu 06: konkretne doświadczenie → werdykt testu → odsetek fałszywych alarmów.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind (PROTOTYPY 6B–6E, 2026-11-09; 6A odrzucony):
//   "welch"   6D brygadzista mierzy dwie zmiany o tej samej średniej i różnym rozrzucie;
//             te same dane idą do testu Studenta i Welcha (dwie lampki, dwa liczniki)
//   "plots"   6E dwa nawozy na poletkach; rośliny jako obserwacje a średnie poletek
//   "qq"      6B kolejka 20 osób → idealna kolejka → wykres Q-Q (kadry)
//   "sw"      6C Shapiro-Wilk na tym samym rozkładzie przy n = 20 i n = 2000
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

  // --- wspólne elementy scen z werdyktem ----------------------------------
  // Lampka werdyktu: state = null (zgaszona), true (alarm), false (spokój).
  function lampAt(g, x, y, state, label, verdict) {
    var on = state === true || state === false;
    if (label) svg("text", { x: x, y: y - 22, "text-anchor": "middle", class: "lc-sc-th-t" }, g, label);
    if (on && state) svg("circle", { cx: x, cy: y, r: 20, class: "lc-sc-glow" }, g);
    svg("rect", { x: x - 5, y: y + 12, width: 10, height: 7, rx: 2, class: "lc-sc-lamp-base" }, g);
    svg("circle", { cx: x, cy: y, r: 12,
      class: !on ? "lc-sc-lamp" : state ? "lc-sc-lamp is-alarm" : "lc-sc-lamp is-ok" }, g);
    if (on && verdict) {
      svg("text", { x: x, y: y + 36, "text-anchor": "middle",
        class: "lc-sc-verdict is-sm " + (state ? "is-alarm" : "is-ok") }, g, state ? verdict[0] : verdict[1]);
    }
  }

  // Licznik alarmów jako pasek. o: x0, x1, y, label, N, alarms, max, ticks, alpha,
  // showAlpha (krok „Poziom α”: linia α i kolor zgodności), dim (licznik nieaktywny).
  function gaugeRow(g, o) {
    var rate = o.N ? o.alarms / o.N : 0, cls = "lc-sc-n";
    if (o.showAlpha && o.N >= 300) {
      var tol = Math.max(0.01, 3 * Math.sqrt(o.alpha * (1 - o.alpha) / o.N));
      cls += Math.abs(rate - o.alpha) > tol ? " is-hit" : " is-good";
    }
    var gg = svg("g", { class: o.dim ? "lc-sc-dim" : "" }, g);
    svg("text", { x: o.x0, y: o.y - 8, class: cls }, gg,
      o.label + " · " + (o.unit || "pomiarów") + ": " + o.N + " · alarmów: " + (o.N ? fmt(100 * rate, 1) + "%" : "—"));
    var gx = function (r) { return o.x0 + (o.x1 - o.x0) * Math.min(r, o.max) / o.max; };
    svg("rect", { x: o.x0, y: o.y, width: o.x1 - o.x0, height: 12, rx: 3, class: "lc-sc-track" }, gg);
    if (o.N) svg("rect", { x: o.x0, y: o.y, width: Math.max(2, gx(rate) - o.x0), height: 12, rx: 3, class: "lc-sc-gauge" }, gg);
    (o.ticks || []).forEach(function (t, i, a) {
      svg("text", { x: gx(t), y: o.y + 26, "text-anchor": i === 0 ? "start" : i === a.length - 1 ? "end" : "middle",
        class: "lc-sc-tick" }, gg, fmt(100 * t, 0) + "%" + (i === a.length - 1 ? "+" : ""));
    });
    if (o.showAlpha) {
      var ax = gx(o.alpha);
      svg("line", { x1: ax, x2: ax, y1: o.y - 4, y2: o.y + 16, class: "lc-sc-param" }, gg);
    }
  }

  function personAt(g, x, y, sign) {
    svg("rect", { x: 14, y: 12, width: 150, height: 24, rx: 4, class: "lc-sc-sign" }, g);
    svg("text", { x: 89, y: 29, "text-anchor": "middle", class: "lc-sc-sign-t is-small" }, g, sign);
    svg("rect", { x: x - 13, y: y - 44, width: 26, height: 7, rx: 3, class: "lc-sc-helmet" }, g);
    svg("circle", { cx: x, cy: y - 30, r: 11, class: "lc-sc-person" }, g);
    svg("rect", { x: x - 15, y: y - 16, width: 30, height: 42, rx: 11, class: "lc-sc-person" }, g);
    svg("rect", { x: x - 11, y: y + 24, width: 9, height: 22, rx: 3, class: "lc-sc-person" }, g);
    svg("rect", { x: x + 2, y: y + 24, width: 9, height: 22, rx: 3, class: "lc-sc-person" }, g);
    svg("rect", { x: x + 12, y: y - 8, width: 20, height: 26, rx: 2, class: "lc-sc-clip" }, g);
    svg("line", { x1: x + 16, x2: x + 28, y1: y, y2: y, class: "lc-sc-axis" }, g);
    svg("line", { x1: x + 16, x2: x + 28, y1: y + 6, y2: y + 6, class: "lc-sc-axis" }, g);
  }

  function pFmt(p) { return p < 0.001 ? "< 0.001" : fmt(p, 3); }
  function pEq(p) { return p < 0.001 ? "p < 0.001" : "p = " + fmt(p, 3); }

  // Wspólny przebieg sceny z powtórzeniem: animowany pomiar, potem zapis.
  function repeater(api, st, measure, commit, render, drawStage) {
    function runOne(fast, done) {
      var d = measure(), step = api.step();
      tween(fast ? 160 : 1000, function (u) {
        api.stage.textContent = "";
        drawStage(api.stage, step, d, u);
      }, function () { commit(d); render(); done(); });
    }
    return {
      go: function (done) { runOne(false, done); },
      many: function (m, done) {
        if (m > 10) { for (var i = 0; i < m; i++) commit(measure()); render(); done(); return; }
        var left = m;
        (function next() { if (left-- <= 0) { done(); return; } runOne(true, next); })();
      }
    };
  }

  // =========================================================================
  // PROTOTYP 6D — WELCH: dwie zmiany w hali montażowej
  // Ta sama para pomiarów idzie do testu Studenta i Welcha (dwie lampki,
  // dwa liczniki). Klamry pod pasami: rozrzut zmiany (s) i rozrzut wspólny
  // (s_p), który zakłada test Studenta — to jest mechanizm błędu.
  // =========================================================================
  KINDS.welch = function (cfg, api) {
    var MU = cfg.mu || 100, SDA = cfg.sd_chaos || 20, SDB = cfg.sd_calm || 5;
    var LAY = cfg.layouts || { eq: [50, 50], few: [20, 80], many: [80, 20] };
    var ALPHA = cfg.alpha || 0.05;
    var SX0 = 190, SX1 = 610, XMIN = MU - 60, XMAX = MU + 60;
    var ROWA = 76, ROWB = 156, BAND = 15, AXY = 214;
    var st = { lay: cfg.layout || "few", runs: [], N: 0, aS: 0, aW: 0, last: null };

    function nA() { return LAY[st.lay][0]; }
    function nB() { return LAY[st.lay][1]; }
    function xOf(v) { return SX0 + (SX1 - SX0) * (Math.max(XMIN, Math.min(XMAX, v)) - XMIN) / (XMAX - XMIN); }
    function dx(s) { return (SX1 - SX0) * s / (XMAX - XMIN); }

    function measure() {
      var a = [], b = [], ja = [], jb = [], i;
      for (i = 0; i < nA(); i++) { a.push(MU + SDA * rnorm()); ja.push(Math.random() * 2 - 1); }
      for (i = 0; i < nB(); i++) { b.push(MU + SDB * rnorm()); jb.push(Math.random() * 2 - 1); }
      var A = meanVar(a), B = meanVar(b), S = ttest(a, b, false), Wl = ttest(a, b, true);
      var sp = Math.sqrt(((A.n - 1) * A.v + (B.n - 1) * B.v) / (A.n + B.n - 2));
      return { a: a, b: b, ja: ja, jb: jb, ma: A.m, mb: B.m, sa: Math.sqrt(A.v), sb: Math.sqrt(B.v), sp: sp,
        pS: S.p, pW: Wl.p, alS: S.p < ALPHA, alW: Wl.p < ALPHA };
    }

    function bracket(g, m, s, y, cls) {
      var x0 = xOf(m) - dx(s), x1 = xOf(m) + dx(s);
      svg("line", { x1: x0, x2: x1, y1: y, y2: y, class: cls }, g);
      svg("line", { x1: x0, x2: x0, y1: y - 4, y2: y + 4, class: cls }, g);
      svg("line", { x1: x1, x2: x1, y1: y - 4, y2: y + 4, class: cls }, g);
    }

    function drawStage(g, step, d, u) {
      personAt(g, 62, 96, "HALA MONTAŻU");
      var done = u === undefined || u >= 1;
      lampAt(g, 50, 196, step >= 2 && d && done ? d.alS : null, step >= 2 ? "Student" : "", ["różnica!", "brak"]);
      lampAt(g, 130, 196, step >= 2 && d && done ? d.alW : null, step >= 2 ? "Welch" : "", ["różnica!", "brak"]);
      [["chaotyczna", nA(), ROWA, "is-chaos", d && d.a, d && d.ja, d && d.ma, d && d.sa],
       ["spokojna", nB(), ROWB, "is-calm", d && d.b, d && d.jb, d && d.mb, d && d.sb]].forEach(function (r) {
        svg("rect", { x: SX0, y: r[2] - BAND - 4, width: SX1 - SX0, height: 2 * BAND + 8, rx: 4, class: "lc-sc-band" }, g);
        svg("text", { x: SX0, y: r[2] - BAND - 10, class: "lc-sc-rowlab" }, g, r[0] + " · n = " + r[1]);
        if (!r[4]) return;
        var lim = done ? r[4].length : Math.round(u * r[4].length);
        for (var i = 0; i < lim; i++) {
          svg("circle", { cx: xOf(r[4][i]), cy: r[2] + r[5][i] * BAND, r: 3.4, class: "lc-sc-wdot " + r[3] }, g);
        }
        if (!done) return;
        var mx = xOf(r[6]);
        svg("line", { x1: mx, x2: mx, y1: r[2] - BAND - 3, y2: r[2] + BAND + 3, class: "lc-sc-mean" }, g);
        bracket(g, r[6], r[7], r[2] + BAND + 11, "lc-sc-sd " + r[3]);
        if (step >= 2) bracket(g, r[6], d.sp, r[2] + BAND + 18, "lc-sc-sdp");
      });
      var ticks = [];
      for (var v = XMIN; v <= XMAX; v += 20) ticks.push({ x: xOf(v), label: String(v) });
      axisX(g, SX0, SX1, AXY, ticks);
      svg("text", { x: SX1, y: AXY + 34, "text-anchor": "end", class: "lc-sc-axtitle" }, g, "czas montażu (min)");
      if (step >= 4) {
        var x = xOf(MU);
        svg("line", { x1: x, x2: x, y1: ROWA - BAND - 6, y2: AXY, class: "lc-sc-param" }, g);
      }
      if (d && done) {
        // odczyt: wzorzec klamer, wartości s i s_p
        var y = AXY + 52;
        svg("line", { x1: SX0, x2: SX0 + 20, y1: y - 4, y2: y - 4, class: "lc-sc-sd is-chaos" }, g);
        var t = "s = " + fmt(d.sa, 1) + " i " + fmt(d.sb, 1) + " min";
        svg("text", { x: SX0 + 26, y: y, class: "lc-sc-n" }, g, t);
        if (step >= 2) {
          svg("line", { x1: SX0 + 190, x2: SX0 + 210, y1: y - 4, y2: y - 4, class: "lc-sc-sdp" }, g);
          svg("text", { x: SX0 + 216, y: y, class: "lc-sc-n" }, g, "wspólne s_p = " + fmt(d.sp, 1) + " min");
        }
      }
    }

    function drawLow(g, step) {
      if (step >= 3) {
        var T = [0, 0.1, 0.2, 0.3, 0.4, 0.5];
        gaugeRow(g, { x0: 60, x1: 610, y: 330, label: "Student", N: st.N, alarms: st.aS, max: 0.5, ticks: T,
          alpha: ALPHA, showAlpha: step >= 4 });
        gaugeRow(g, { x0: 60, x1: 610, y: 400, label: "Welch", N: st.N, alarms: st.aW, max: 0.5, ticks: T,
          alpha: ALPHA, showAlpha: step >= 4 });
        if (step >= 4) {
          svg("line", { x1: 60, x2: 82, y1: 452, y2: 452, class: "lc-sc-param" }, g);
          svg("text", { x: 90, y: 457, class: "lc-sc-n" }, g, "μ = " + MU + " min · α = " + fmt(100 * ALPHA, 0) + "%");
        }
        return;
      }
      if (!st.runs.length) return;
      var X = [30, 100, 220, 340, 470];
      svg("text", { x: X[0], y: 304, class: "lc-sc-th-t" }, g, "pomiar");
      svg("text", { x: X[1], y: 304, class: "lc-sc-th-t" }, g, "x̄ chaotyczna");
      svg("text", { x: X[2], y: 304, class: "lc-sc-th-t" }, g, "x̄ spokojna");
      if (step >= 2) {
        svg("text", { x: X[3], y: 304, class: "lc-sc-th-t" }, g, "p Studenta");
        svg("text", { x: X[4], y: 304, class: "lc-sc-th-t" }, g, "p Welcha");
      }
      st.runs.slice(-6).reverse().forEach(function (d, i) {
        var y = 330 + i * 24, g2 = svg("g", { opacity: 1 - i * 0.13 }, g);
        svg("text", { x: X[0], y: y, class: "lc-sc-log" }, g2, String(d.no));
        svg("text", { x: X[1], y: y, class: "lc-sc-log" }, g2, fmt(d.ma, 1));
        svg("text", { x: X[2], y: y, class: "lc-sc-log" }, g2, fmt(d.mb, 1));
        if (step >= 2) {
          [[X[3], d.pS, d.alS], [X[4], d.pW, d.alW]].forEach(function (c) {
            svg("circle", { cx: c[0] + 6, cy: y - 5, r: 6, class: c[2] ? "lc-sc-lamp is-alarm" : "lc-sc-lamp is-ok" }, g2);
            svg("text", { x: c[0] + 18, y: y, class: "lc-sc-log" + (c[2] ? " is-alarm" : "") }, g2, pFmt(c[1]));
          });
        }
      });
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, st.last);
      drawLow(api.low, step);
    }
    function commit(d) {
      st.N += 1; d.no = st.N;
      if (d.alS) st.aS += 1;
      if (d.alW) st.aW += 1;
      st.runs.push({ no: d.no, ma: d.ma, mb: d.mb, pS: d.pS, pW: d.pW, alS: d.alS, alW: d.alW });
      if (st.runs.length > 6) st.runs.shift();
      st.last = d;
    }
    var rep = repeater(api, st, measure, commit, render, drawStage);
    return {
      render: render,
      reset: function () { st.runs = []; st.N = 0; st.aS = 0; st.aW = 0; st.last = null; render(); },
      opt: function (name, v) { if (name === "n" && LAY[v]) st.lay = v; this.reset(); },
      go: rep.go, many: rep.many
    };
  };

  // =========================================================================
  // PROTOTYP 6E — POLE: dwa nawozy, 4 poletka na nawóz, 25 roślin na poletku
  // Nawozy działają tak samo. Te same dane analizujemy dwa razy: każda
  // roślina jako obserwacja (100 na grupę) albo średnia poletka (4 na grupę).
  // =========================================================================
  KINDS.plots = function (cfg, api) {
    var MU = cfg.mu || 50, TAU = cfg.tau || 3, SIG = cfg.sigma || 5, K = cfg.k || 4, M = cfg.m || 25;
    var ALPHA = cfg.alpha || 0.05;
    var SX0 = 340, SX1 = 620, XMIN = MU - 20, XMAX = MU + 20, ROWA = 84, ROWB = 160, BAND = 15, AXY = 212;
    var st = { runs: [], N: 0, aR: 0, aP: 0, last: null };

    function xOf(v) { return SX0 + (SX1 - SX0) * (Math.max(XMIN, Math.min(XMAX, v)) - XMIN) / (XMAX - XMIN); }

    function measure() {
      var plots = [], allA = [], allB = [], mA = [], mB = [];
      for (var p = 0; p < 2 * K; p++) {
        var eff = TAU * rnorm(), h = [], j = [];
        for (var i = 0; i < M; i++) { h.push(MU + eff + SIG * rnorm()); j.push(Math.random() * 2 - 1); }
        var mv = meanVar(h).m;
        plots.push({ h: h, j: j, m: mv });
        if (p < K) { allA = allA.concat(h); mA.push(mv); } else { allB = allB.concat(h); mB.push(mv); }
      }
      var R = ttest(allA, allB, false), P = ttest(mA, mB, false);
      return { plots: plots, ma: R.ma, mb: R.mb, pR: R.p, pP: P.p, alR: R.p < ALPHA, alP: P.p < ALPHA };
    }

    function drawStage(g, step, d, u) {
      svg("rect", { x: 14, y: 12, width: 190, height: 24, rx: 4, class: "lc-sc-sign" }, g);
      svg("text", { x: 109, y: 29, "text-anchor": "middle", class: "lc-sc-sign-t is-small" }, g, "POLE DOŚWIADCZALNE");
      var done = u === undefined || u >= 1;
      // pole: wiersz A i wiersz B, po K poletek
      for (var p = 0; p < 2 * K; p++) {
        var row = p < K ? 0 : 1, col = p % K, x = 44 + col * 66, y = 54 + row * 84;
        svg("rect", { x: x, y: y, width: 60, height: 64, rx: 4, class: "lc-sc-plot " + (row ? "is-b" : "is-a") }, g);
        if (!d) continue;
        var pl = d.plots[p], lim = done ? M : Math.round(u * M);
        for (var i = 0; i < lim; i++) {
          var r = Math.max(1.2, Math.min(5, 3 + (pl.h[i] - MU) / 4));
          svg("circle", { cx: x + 8 + (i % 5) * 11, cy: y + 9 + Math.floor(i / 5) * 11.5, r: r, class: "lc-sc-pl pc" + col }, g);
        }
      }
      svg("text", { x: 22, y: 92, class: "lc-sc-rowlab" }, g, "A");
      svg("text", { x: 22, y: 176, class: "lc-sc-rowlab" }, g, "B");
      // pasy wysokości roślin; kolor = poletko
      [["nawóz A", ROWA, 0], ["nawóz B", ROWB, K]].forEach(function (r) {
        svg("rect", { x: SX0, y: r[1] - BAND - 4, width: SX1 - SX0, height: 2 * BAND + 8, rx: 4, class: "lc-sc-band" }, g);
        svg("text", { x: SX0, y: r[1] - BAND - 10, class: "lc-sc-rowlab" }, g, r[0] + " · " + K + " poletka · " + K * M + " roślin");
        if (!d || !done) return;
        for (var q = 0; q < K; q++) {
          var pl = d.plots[r[2] + q];
          for (var i = 0; i < M; i++) {
            svg("circle", { cx: xOf(pl.h[i]), cy: r[1] + pl.j[i] * BAND, r: 2.6, class: "lc-sc-pl pc" + q }, g);
          }
        }
        for (q = 0; q < K; q++) {
          var mx = xOf(d.plots[r[2] + q].m);
          svg("rect", { x: mx - 5, y: r[1] - 5, width: 10, height: 10, transform: "rotate(45 " + mx + " " + r[1] + ")",
            class: "lc-sc-pmean pc" + q }, g);
        }
      });
      var ticks = [];
      for (var v = XMIN; v <= XMAX; v += 10) ticks.push({ x: xOf(v), label: String(v) });
      axisX(g, SX0, SX1, AXY, ticks);
      svg("text", { x: SX1, y: AXY + 34, "text-anchor": "end", class: "lc-sc-axtitle" }, g, "wysokość rośliny (cm)");
      if (step >= 4) {
        var x0 = xOf(MU);
        svg("line", { x1: x0, x2: x0, y1: ROWA - BAND - 6, y2: AXY, class: "lc-sc-param" }, g);
      }
      lampAt(g, 100, 252, step >= 2 && d && done ? d.alR : null, step >= 2 ? "100 roślin" : "", ["różnica!", "brak"]);
      lampAt(g, 220, 252, step >= 2 && d && done ? d.alP : null, step >= 2 ? "4 poletka" : "", ["różnica!", "brak"]);
    }

    function drawLow(g, step) {
      if (step >= 3) {
        var T = [0, 0.2, 0.4, 0.6];
        gaugeRow(g, { x0: 60, x1: 610, y: 330, label: "każda roślina osobno", N: st.N, alarms: st.aR, max: 0.6, ticks: T,
          alpha: ALPHA, showAlpha: step >= 4, unit: "doświadczeń" });
        gaugeRow(g, { x0: 60, x1: 610, y: 400, label: "średnia poletka", N: st.N, alarms: st.aP, max: 0.6, ticks: T,
          alpha: ALPHA, showAlpha: step >= 4, unit: "doświadczeń" });
        if (step >= 4) {
          svg("line", { x1: 60, x2: 82, y1: 452, y2: 452, class: "lc-sc-param" }, g);
          svg("text", { x: 90, y: 457, class: "lc-sc-n" }, g, "μ = " + MU + " cm dla obu nawozów · α = " + fmt(100 * ALPHA, 0) + "%");
        }
        return;
      }
      if (!st.runs.length) return;
      var X = [30, 140, 260, 420];
      svg("text", { x: X[0], y: 316, class: "lc-sc-th-t" }, g, "doświadczenie");
      svg("text", { x: X[1], y: 316, class: "lc-sc-th-t" }, g, "x̄ A − x̄ B (cm)");
      if (step >= 2) {
        svg("text", { x: X[2], y: 316, class: "lc-sc-th-t" }, g, "p (100 roślin)");
        svg("text", { x: X[3], y: 316, class: "lc-sc-th-t" }, g, "p (4 poletka)");
      }
      st.runs.slice(-5).reverse().forEach(function (d, i) {
        var y = 342 + i * 24, g2 = svg("g", { opacity: 1 - i * 0.15 }, g);
        svg("text", { x: X[0], y: y, class: "lc-sc-log" }, g2, String(d.no));
        svg("text", { x: X[1], y: y, class: "lc-sc-log" }, g2, fmt(d.diff, 1));
        if (step >= 2) {
          [[X[2], d.pR, d.alR], [X[3], d.pP, d.alP]].forEach(function (c) {
            svg("circle", { cx: c[0] + 6, cy: y - 5, r: 6, class: c[2] ? "lc-sc-lamp is-alarm" : "lc-sc-lamp is-ok" }, g2);
            svg("text", { x: c[0] + 18, y: y, class: "lc-sc-log" + (c[2] ? " is-alarm" : "") }, g2, pFmt(c[1]));
          });
        }
      });
    }

    function render() {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, st.last);
      drawLow(api.low, step);
    }
    function commit(d) {
      st.N += 1; d.no = st.N;
      if (d.alR) st.aR += 1;
      if (d.alP) st.aP += 1;
      st.runs.push({ no: d.no, diff: d.ma - d.mb, pR: d.pR, pP: d.pP, alR: d.alR, alP: d.alP });
      if (st.runs.length > 5) st.runs.shift();
      st.last = d;
    }
    var rep = repeater(api, st, measure, commit, render, drawStage);
    return {
      render: render,
      reset: function () { st.runs = []; st.N = 0; st.aR = 0; st.aP = 0; st.last = null; render(); },
      opt: function () {},
      go: rep.go, many: rep.many
    };
  };

  // =========================================================================
  // PROTOTYP 6B — KOLEJKA: jak powstaje wykres Q-Q (kadry, bez losowania
  // w krokach; przycisk ustawia nową grupę 20 osób)
  // =========================================================================
  KINDS.qq = function (cfg, api) {
    var Q = cfg.q, N = Q.length, MU = cfg.mu || 172, SD = cfg.sd || 9;
    var X0 = 80, SLOT = 26, BASE = 236, PX = 2.0, HMIN = 120;
    var PL = 150, PR = 520, PT = 294, PB = 428;
    var st = { h: (cfg.heights || []).slice() };
    if (st.h.length !== N) newGroup();

    function newGroup() {
      st.h = [];
      for (var i = 0; i < N; i++) st.h.push(Math.round((MU + SD * rnorm()) * 10) / 10);
    }
    function stats() {
      var mv = meanVar(st.h), order = st.h.map(function (v, i) { return i; });
      order.sort(function (a, b) { return st.h[a] - st.h[b]; });
      return { m: mv.m, s: Math.sqrt(mv.v), order: order };
    }
    function hpx(v) { return Math.max(10, (v - HMIN) * PX); }

    function person(g, x, h, cls) {
      var top = BASE - hpx(h);
      svg("rect", { x: x - 7, y: top + 12, width: 14, height: BASE - top - 12, rx: 6, class: "lc-sc-qper " + cls }, g);
      svg("circle", { cx: x, cy: top + 6, r: 6, class: "lc-sc-qper " + cls }, g);
    }

    function render() {
      var step = api.step(), g = api.stage, lo = api.low;
      g.textContent = ""; lo.textContent = "";
      var S = stats(), tallest = S.order[N - 1], shortest = S.order[0];
      svg("line", { x1: X0 - 14, x2: X0 + N * SLOT, y1: BASE, y2: BASE, class: "lc-sc-axis" }, g);
      for (var k = 0; k < N; k++) {
        var idx = step >= 2 ? S.order[k] : k, x = X0 + k * SLOT + SLOT / 2;
        if (step >= 3) {
          var ideal = S.m + S.s * Q[k], top = BASE - hpx(ideal);
          svg("rect", { x: x - 9, y: top, width: 18, height: BASE - top, rx: 7, class: "lc-sc-ghost" }, g);
        }
        person(g, x, st.h[idx], idx === tallest || idx === shortest ? "is-mark" : "");
      }
      svg("text", { x: X0 - 14, y: BASE + 22, class: "lc-sc-n" }, g,
        "20 osób · x̄ = " + fmt(S.m, 1) + " cm · s = " + fmt(S.s, 1) + " cm");
      if (step < 4) return;
      // wykres Q-Q: miejsce w idealnej kolejce (z) × wzrost
      var ys = st.h.concat([S.m + S.s * Q[0], S.m + S.s * Q[N - 1]]);
      var ymin = Math.floor(Math.min.apply(null, ys) / 5) * 5 - 2, ymax = Math.ceil(Math.max.apply(null, ys) / 5) * 5 + 2;
      var xq = function (z) { return PL + (PR - PL) * (z + 2.2) / 4.4; };
      var yq = function (v) { return PB - (PB - PT) * (v - ymin) / (ymax - ymin); };
      var ticks = [-2, -1, 0, 1, 2].map(function (z) { return { x: xq(z), label: String(z) }; });
      axisX(lo, PL, PR, PB, ticks);
      svg("line", { x1: PL, x2: PL, y1: PT, y2: PB, class: "lc-sc-axis" }, lo);
      for (var v = Math.ceil(ymin / 10) * 10; v <= ymax; v += 10) {
        svg("text", { x: PL - 8, y: yq(v) + 4, "text-anchor": "end", class: "lc-sc-tick" }, lo, String(v));
      }
      svg("text", { x: PR, y: PB + 34, "text-anchor": "end", class: "lc-sc-axtitle" }, lo, "miejsce w idealnej kolejce (z)");
      svg("text", { x: PL - 40, y: PT - 10, class: "lc-sc-axtitle" }, lo, "wzrost (cm)");
      svg("line", { x1: xq(-2.2), x2: xq(2.2), y1: yq(S.m - 2.2 * S.s), y2: yq(S.m + 2.2 * S.s), class: "lc-sc-qqline" }, lo);
      for (k = 0; k < N; k++) {
        var id = S.order[k];
        svg("circle", { cx: xq(Q[k]), cy: yq(st.h[id]), r: 4.5,
          class: "lc-sc-qpt" + (id === tallest || id === shortest ? " is-mark" : "") }, lo);
      }
    }
    return {
      render: render,
      reset: function () { st.h = (cfg.heights || []).slice(); render(); },
      opt: function () {},
      go: function (done) { newGroup(); render(); done(); },
      many: function (m, done) { done(); }
    };
  };

  // =========================================================================
  // PROTOTYP 6C — DWA ŚWIATY: ten sam lekko skośny rozkład, n = 20 i n = 2000.
  // Losowania i wynik testu Shapiro-Wilka liczy R (pula w cfg.pool).
  // =========================================================================
  KINDS.sw = function (cfg, api) {
    var POOL = cfg.pool, YMIN = cfg.ymin, YMAX = cfg.ymax, ALPHA = cfg.alpha || 0.05;
    var PANELS = [[60, 300], [370, 610]], PT = 30, PB = 320;
    var st = { k: 0 };
    function xq(p, z) { return p[0] + (p[1] - p[0]) * (z + 3.8) / 7.6; }
    function yq(v) { return PB - (PB - PT) * (v - YMIN) / (YMAX - YMIN); }

    function panel(g, p, d, on) {
      svg("rect", { x: p[0], y: PT, width: p[1] - p[0], height: PB - PT, class: "lc-sc-frame" }, g);
      var ticks = [-3, -2, -1, 0, 1, 2, 3].map(function (z) { return { x: xq(p, z), label: String(z) }; });
      axisX(g, p[0], p[1], PB, ticks);
      for (var v = Math.ceil(YMIN / 20) * 20; v <= YMAX; v += 20) {
        svg("text", { x: p[0] - 6, y: yq(v) + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, String(v));
      }
      if (!on) {
        svg("text", { x: (p[0] + p[1]) / 2, y: (PT + PB) / 2, "text-anchor": "middle", class: "lc-sc-axtitle" }, g, "?");
        return;
      }
      svg("line", { x1: xq(p, -3.8), x2: xq(p, 3.8), y1: yq(d.a - 3.8 * d.b), y2: yq(d.a + 3.8 * d.b), class: "lc-sc-qqline" }, g);
      var r = d.q.length > 50 ? 2.2 : 4.5;
      for (var i = 0; i < d.q.length; i++) {
        svg("circle", { cx: xq(p, d.q[i]), cy: yq(d.y[i]), r: r, class: "lc-sc-qpt" }, g);
      }
      var rej = d.p < ALPHA;
      svg("text", { x: p[0], y: PB + 44, class: "lc-sc-n" + (rej ? " is-hit" : " is-good") }, g,
        "n = " + d.n + " · W = " + fmt(d.W, 3) + " · " + pEq(d.p));
      svg("text", { x: p[0], y: PB + 64, class: "lc-sc-n" + (rej ? " is-hit" : " is-good") }, g,
        rej ? "odrzucamy normalność" : "brak podstaw do odrzucenia");
    }

    function render() {
      var step = api.step(), g = api.stage;
      g.textContent = ""; api.low.textContent = "";
      var d = POOL[st.k];
      panel(g, PANELS[0], d.small, true);
      panel(g, PANELS[1], d.big, step >= 2);
      svg("text", { x: PANELS[1][1], y: PB + 90, "text-anchor": "end", class: "lc-sc-axtitle" }, g,
        "oś pozioma: kwantyl teoretyczny (z) · oś pionowa: masa jabłka (g)");
    }
    return {
      render: render,
      reset: function () { st.k = 0; render(); },
      opt: function () {},
      go: function (done) { st.k = (st.k + 1) % POOL.length; render(); done(); },
      many: function (m, done) { done(); }
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
