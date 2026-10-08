// Sceny wykładu 07: konkretne doświadczenie → statystyka → rozkład wyników.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind:
//   "season"  barista notuje sezon dzień po dniu (kawy i temperatura): ile sezonów
//             daje „istotną” korelację, gdy kolejne dni są do siebie podobne
// Numer kroku czyta z data-lc-step korzenia widgetu. Sterowanie:
//   [data-sc-act]  go | m10 | m100 | m1000 (przycisk go zmienia podpis wg kroku: data-labels)
//   [data-sc-opt]  "nazwa:wartość" (przełączniki opcji, np. phi:0.9)
// Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
// Wzorzec silnika (init, KINDS, tween) skopiowany z wykładu 00.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H = 460;
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  // Kropka dziesiętna, „-” przed liczbą ujemną.
  function fmt(x, d) {
    var s = x.toFixed(d);
    return /^-0(\.0*)?$/.test(s) ? s.slice(1) : s;   // bez „-0.00”
  }
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

  // --- rozkłady ---------------------------------------------------------------
  function gauss() {
    var u = 0, v = 0;
    while (u === 0) u = Math.random();
    while (v === 0) v = Math.random();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }
  function lgamma(x) {
    // Lanczos (g = 7, n = 9)
    var c = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
      -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6,
      1.5056327351493116e-7];
    if (x < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * x)) - lgamma(1 - x);
    x -= 1;
    var a = c[0], t = x + 7.5;
    for (var i = 1; i < 9; i++) a += c[i] / (x + i);
    return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
  }
  // Ułamek łańcuchowy dla niekompletnej funkcji beta (Numerical Recipes, betacf).
  function betacf(a, b, x) {
    var MAXIT = 300, EPS = 3e-14, FPMIN = 1e-300;
    var qab = a + b, qap = a + 1, qam = a - 1, c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < FPMIN) d = FPMIN;
    d = 1 / d;
    var h = d;
    for (var m = 1; m <= MAXIT; m++) {
      var m2 = 2 * m, aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      var del = d * c;
      h *= del;
      if (Math.abs(del - 1) < EPS) break;
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
  // Dwustronna wartość p testu t: P(|T| ≥ |t|) przy df stopniach swobody.
  function pT2(t, df) { return ibeta(df / (df + t * t), df / 2, 0.5); }
  // Wartość krytyczna |r| dla poziomu alpha (bisekcja po r).
  function rCrit(n, alpha) {
    var lo = 0, hi = 1, df = n - 2;
    for (var i = 0; i < 60; i++) {
      var r = (lo + hi) / 2, t = r * Math.sqrt(df / (1 - r * r));
      if (pT2(t, df) > alpha) lo = r; else hi = r;
    }
    return (lo + hi) / 2;
  }
  function corr(x, y) {
    var n = x.length, mx = 0, my = 0, sxx = 0, syy = 0, sxy = 0, i;
    for (i = 0; i < n; i++) { mx += x[i]; my += y[i]; }
    mx /= n; my /= n;
    for (i = 0; i < n; i++) {
      var dx = x[i] - mx, dy = y[i] - my;
      sxx += dx * dx; syy += dy * dy; sxy += dx * dy;
    }
    return sxy / Math.sqrt(sxx * syy);
  }

  var KINDS = {};

  // =========================================================================
  // SEASON: barista notuje sezon; korelacja dwóch niezależnych serii
  // =========================================================================
  KINDS.season = function (cfg, api) {
    var N = cfg.n || 60, ALPHA = cfg.alpha || 0.05;
    var MK = cfg.kawy_mean || 120, SK = cfg.kawy_sd || 25;
    var MT = cfg.temp_mean || 9, ST = cfg.temp_sd || 6;
    var RC = rCrit(N, ALPHA);
    var NB = 40;                          // przedziały r co 0.05 od -1 do 1
    var st = { phi: Number(cfg.phi === undefined ? 0.9 : cfg.phi), seasons: [], last: null,
      sig: new Array(NB).fill(0), non: new Array(NB).fill(0), nsig: 0 };

    // wykresy serii (scena górna)
    var CX0 = 150, CX1 = 440, C1 = [26, 92], C2 = [122, 188];
    // dół: rozrzut (krok 2) i histogram r (krok 3–4)
    var SL = 130, SR = 420, ST0 = 232, SB = 410;
    var HL = 60, HT = 250, HB = 400;

    function ar1(phi) {
      var a = [], s = Math.sqrt(1 - phi * phi), v = gauss();
      a.push(v);
      for (var i = 1; i < N; i++) { v = phi * v + s * gauss(); a.push(v); }
      return a;
    }
    function season() {
      var zk = ar1(st.phi), zt = ar1(st.phi);
      var k = zk.map(function (z) { return Math.max(5, Math.round(MK + SK * z)); });
      var t = zt.map(function (z) { return Math.round((MT + ST * z) * 10) / 10; });
      var r = corr(t, k), df = N - 2;
      var tt = r * Math.sqrt(df / Math.max(1e-12, 1 - r * r));
      var p = pT2(tt, df);
      return { k: k, t: t, r: r, p: p, hit: p < ALPHA };
    }
    function binOf(r) { return Math.max(0, Math.min(NB - 1, Math.floor((r + 1) / 2 * NB))); }
    function commit(d) {
      d.no = st.seasons.length + 1;
      st.seasons.push(d); st.last = d;
      if (d.hit) { st.sig[binOf(d.r)] += 1; st.nsig += 1; } else st.non[binOf(d.r)] += 1;
    }

    // --- barista ---------------------------------------------------------
    function barista(g, writing) {
      // lada
      svg("rect", { x: 14, y: 150, width: 112, height: 40, rx: 4, class: "lc-sc-counter" }, g);
      // postać
      svg("circle", { cx: 52, cy: 92, r: 13, class: "lc-sc-person" }, g);
      svg("rect", { x: 36, y: 108, width: 32, height: 42, rx: 10, class: "lc-sc-person" }, g);
      svg("rect", { x: 40, y: 116, width: 24, height: 34, rx: 3, class: "lc-sc-apron" }, g);
      // filiżanka na ladzie
      svg("path", { d: "M 88 138 L 108 138 C 108 150 103 151 98 151 C 93 151 88 150 88 138 Z", class: "lc-sc-cup" }, g);
      svg("path", { d: "M 108 141 C 115 141 115 148 107 148", class: "lc-sc-cup-handle", fill: "none" }, g);
      // zeszyt w dłoni
      svg("rect", { x: 62, y: 112, width: 20, height: 26, rx: 2, class: "lc-sc-note",
        transform: "rotate(-12 72 125)" }, g);
      if (writing) svg("line", { x1: 66, x2: 78, y1: 120, y2: 117, class: "lc-sc-pen" }, g);
      svg("text", { x: 70, y: 208, "text-anchor": "middle", class: "lc-sc-sub" }, g, "barista notuje");
    }

    // --- dwie serie w czasie ----------------------------------------------
    function seriesChart(g, y, vals, upto, unit, title, lo, hi) {
      svg("line", { x1: CX0, x2: CX1, y1: y[1], y2: y[1], class: "lc-sc-axis" }, g);
      svg("line", { x1: CX0, x2: CX0, y1: y[0], y2: y[1], class: "lc-sc-axis" }, g);
      svg("text", { x: CX0, y: y[0] - 6, class: "lc-sc-axtitle" }, g, title);
      svg("text", { x: CX0 - 5, y: y[0] + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, fmt(hi, 0));
      svg("text", { x: CX0 - 5, y: y[1], "text-anchor": "end", class: "lc-sc-tick" }, g, fmt(lo, 0));
      if (!vals) return;
      var pts = [];
      for (var i = 0; i < Math.min(upto, N); i++) {
        var v = Math.max(lo, Math.min(hi, vals[i]));
        pts.push((CX0 + (CX1 - CX0) * i / (N - 1)).toFixed(1) + "," +
          (y[1] - (y[1] - y[0]) * (v - lo) / (hi - lo)).toFixed(1));
      }
      if (pts.length > 1) svg("polyline", { points: pts.join(" "), class: "lc-sc-line" }, g);
      if (pts.length) {
        var lp = pts[pts.length - 1].split(",");
        svg("circle", { cx: lp[0], cy: lp[1], r: 3, class: "lc-sc-line-dot" }, g);
      }
      void unit;
    }
    var KLO = MK - 3 * SK, KHI = MK + 3 * SK, TLO = MT - 3 * ST, THI = MT + 3 * ST;

    function drawStage(g, step, upto) {
      var d = st.last;
      barista(g, upto !== null && upto < N);
      seriesChart(g, C1, d ? d.k : null, upto === null ? N : upto, "", "kawy sprzedane danego dnia", KLO, KHI);
      seriesChart(g, C2, d ? d.t : null, upto === null ? N : upto, "", "temperatura na zewnątrz (°C)", TLO, THI);
      svg("text", { x: CX0, y: C2[1] + 16, class: "lc-sc-tick" }, g, "dzień 1");
      svg("text", { x: CX1, y: C2[1] + 16, "text-anchor": "end", class: "lc-sc-tick" }, g, "dzień " + N);
      var RX = 540;
      if (!d) {
        svg("text", { x: RX, y: 100, "text-anchor": "middle", class: "lc-sc-sub" }, g, "zeszyt jeszcze pusty");
        return;
      }
      var done = upto === null || upto >= N;
      if (step === 1 || !done) {
        svg("text", { x: RX, y: 90, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
          "sezon " + (done ? d.no || st.seasons.length : st.seasons.length + 1));
        svg("text", { x: RX, y: 116, "text-anchor": "middle", class: "lc-sc-n" }, g,
          "dzień " + Math.min(N, upto === null ? N : upto) + " z " + N);
        return;
      }
      // odczyt korelacji + lampka
      svg("text", { x: RX, y: 56, "text-anchor": "middle", class: "lc-sc-read" }, g, "r = " + fmt(d.r, 2));
      svg("text", { x: RX, y: 82, "text-anchor": "middle", class: "lc-sc-read is-plain" }, g,
        (d.p < 0.001 ? "p < 0.001" : "p = " + fmt(d.p, 3)));
      svg("circle", { cx: RX - 46, cy: 122, r: 11, class: "lc-sc-lamp" + (d.hit ? " is-on" : "") }, g);
      svg("text", { x: RX - 28, y: 127, class: "lc-sc-lamp-t" + (d.hit ? " is-on" : "") }, g,
        d.hit ? "istotne!" : "nieistotne");
      svg("text", { x: RX, y: 156, "text-anchor": "middle", class: "lc-sc-n" }, g,
        "alarm, gdy p < " + fmt(ALPHA, 2));
    }

    // --- krok 1: dziennik sezonów ------------------------------------------
    function drawLog(g) {
      if (!st.seasons.length) {
        svg("text", { x: W / 2, y: 300, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "Zbierz sezon: barista przez " + N + " dni zapisuje dwie liczby.");
        return;
      }
      st.seasons.slice(-6).reverse().forEach(function (d, i) {
        var y = 262 + i * 26, g2 = svg("g", { opacity: 1 - i * 0.14 }, g);
        var kmin = Math.min.apply(null, d.k), kmax = Math.max.apply(null, d.k);
        var tmin = Math.min.apply(null, d.t), tmax = Math.max.apply(null, d.t);
        svg("text", { x: 60, y: y, class: "lc-sc-log" }, g2, "sezon " + d.no);
        svg("text", { x: 180, y: y, class: "lc-sc-log" }, g2, "kawy od " + kmin + " do " + kmax);
        svg("text", { x: 400, y: y, class: "lc-sc-log" }, g2, "temp. od " + fmt(tmin, 1) + " do " + fmt(tmax, 1) + " °C");
      });
    }

    // --- krok 2: wykres rozrzutu --------------------------------------------
    function drawScatter(g) {
      var d = st.last;
      svg("line", { x1: SL, x2: SR, y1: SB, y2: SB, class: "lc-sc-axis" }, g);
      svg("line", { x1: SL, x2: SL, y1: ST0, y2: SB, class: "lc-sc-axis" }, g);
      [TLO, MT, THI].forEach(function (v) {
        var x = SL + (SR - SL) * (v - TLO) / (THI - TLO);
        svg("line", { x1: x, x2: x, y1: SB, y2: SB + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: x, y: SB + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, fmt(v, 0));
      });
      [KLO, MK, KHI].forEach(function (v) {
        var y = SB - (SB - ST0) * (v - KLO) / (KHI - KLO);
        svg("line", { x1: SL - 5, x2: SL, y1: y, y2: y, class: "lc-sc-axis" }, g);
        svg("text", { x: SL - 8, y: y + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, fmt(v, 0));
      });
      svg("text", { x: (SL + SR) / 2, y: SB + 38, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "temperatura (°C)");
      svg("text", { x: SL - 44, y: (ST0 + SB) / 2, "text-anchor": "middle", class: "lc-sc-axtitle",
        transform: "rotate(-90 " + (SL - 44) + " " + ((ST0 + SB) / 2) + ")" }, g, "kawy");
      if (!d) {
        svg("text", { x: (SL + SR) / 2, y: (ST0 + SB) / 2, "text-anchor": "middle", class: "lc-sc-sub" }, g,
          "tu staną dni sezonu");
        return;
      }
      for (var i = 0; i < N; i++) {
        var x = SL + (SR - SL) * (Math.max(TLO, Math.min(THI, d.t[i])) - TLO) / (THI - TLO);
        var y = SB - (SB - ST0) * (Math.max(KLO, Math.min(KHI, d.k[i])) - KLO) / (KHI - KLO);
        svg("circle", { cx: x, cy: y, r: 3.6, class: "lc-sc-sdot" + (d.hit ? " is-hit" : "") }, g);
      }
      svg("text", { x: SR + 24, y: ST0 + 30, class: "lc-sc-n" }, g, "punkt = jeden dzień");
      svg("text", { x: SR + 24, y: ST0 + 52, class: "lc-sc-n" }, g, N + " dni, r = " + fmt(d.r, 2));
      svg("text", { x: SR + 24, y: ST0 + 74, class: "lc-sc-n" + (d.hit ? " is-hit" : "") }, g,
        d.hit ? "test: związek „istotny”" : "test: brak alarmu");
    }

    // --- krok 3–4: histogram r ---------------------------------------------
    function histR(step) { return step >= 4 ? 440 : 610; }
    function slotX(i, hr) { return HL + (hr - HL) * (i + 0.5) / NB; }
    function rX(r, hr) { return HL + (hr - HL) * (r + 1) / 2; }
    function histInfo() {
      var mx = 1;
      for (var i = 0; i < NB; i++) mx = Math.max(mx, st.sig[i] + st.non[i]);
      return { mx: mx, u: (HB - HT - 30) / Math.max(mx, 8) };
    }
    function drawHist(g, step) {
      var hr = histR(step), h = histInfo(), bw = (hr - HL) / NB;
      svg("line", { x1: HL, x2: hr, y1: HB, y2: HB, class: "lc-sc-axis" }, g);
      [-1, -0.5, 0, 0.5, 1].forEach(function (t) {
        var x = rX(t, hr);
        svg("line", { x1: x, x2: x, y1: HB, y2: HB + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: x, y: HB + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, fmt(t, 1));
      });
      svg("text", { x: (HL + hr) / 2, y: HB + 38, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "korelacja r w sezonie");
      // pasy „istotności”: |r| > r_kryt
      [[-1, -RC], [RC, 1]].forEach(function (z) {
        svg("rect", { x: rX(z[0], hr), y: HT - 6, width: rX(z[1], hr) - rX(z[0], hr), height: HB - HT + 6,
          class: "lc-sc-zone" }, g);
      });
      svg("text", { x: rX(-0.75, hr), y: HT + 28, "text-anchor": "middle", class: "lc-sc-zone-t" }, g, "„istotne”");
      svg("text", { x: rX(0.75, hr), y: HT + 28, "text-anchor": "middle", class: "lc-sc-zone-t" }, g, "„istotne”");
      for (var i = 0; i < NB; i++) {
        var a = st.non[i], b = st.sig[i];
        if (a) svg("rect", { x: slotX(i, hr) - bw * 0.42, y: HB - h.u * a, width: bw * 0.84, height: h.u * a,
          class: "lc-sc-bar" }, g);
        if (b) svg("rect", { x: slotX(i, hr) - bw * 0.42, y: HB - h.u * (a + b), width: bw * 0.84, height: h.u * b,
          class: "lc-sc-bar is-hit" }, g);
      }
      var n = st.seasons.length;
      svg("text", { x: hr, y: HT - 16, "text-anchor": "end", class: "lc-sc-n" }, g,
        "sezonów: " + n);
      svg("text", { x: HL, y: HT - 16, class: "lc-sc-n is-hit" }, g,
        "„istotnych”: " + st.nsig + (n ? " (" + fmt(100 * st.nsig / n, 1) + "%)" : ""));
      if (step >= 4) {
        var x0 = rX(0, hr);
        svg("line", { x1: x0, x2: x0, y1: HT - 6, y2: HB, class: "lc-sc-param" }, g);
        svg("text", { x: x0 + 5, y: HT + 10, class: "lc-sc-param-t" }, g, "prawda: r = 0");
        drawAlarm(g);
      }
    }
    // krok 4: odsetek alarmów obok obiecanych 5%
    function drawAlarm(g) {
      var n = st.seasons.length, rate = n ? st.nsig / n : 0;
      var X0 = 480, X1 = 620, BT = HT + 10, BB = HB, sc = function (v) { return BB - (BB - BT) * v; };
      svg("line", { x1: X0, x2: X1, y1: BB, y2: BB, class: "lc-sc-axis" }, g);
      [0, 0.25, 0.5, 0.75, 1].forEach(function (v) {
        svg("text", { x: X0 - 4, y: sc(v) + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, fmt(100 * v, 0) + "%");
      });
      svg("rect", { x: X0 + 30, y: sc(rate), width: 46, height: BB - sc(rate), class: "lc-sc-bar is-hit" }, g);
      svg("text", { x: X0 + 53, y: sc(rate) - 6, "text-anchor": "middle", class: "lc-sc-read is-small" }, g,
        n ? fmt(100 * rate, 1) + "%" : "–");
      var ya = sc(ALPHA);
      svg("line", { x1: X0 + 6, x2: X1, y1: ya, y2: ya, class: "lc-sc-param" }, g);
      svg("text", { x: X1, y: ya - 6, "text-anchor": "end", class: "lc-sc-param-t" }, g, "α = " + fmt(100 * ALPHA, 0) + "%");
      svg("text", { x: (X0 + X1) / 2, y: HB + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, "alarmy");
      svg("text", { x: (X0 + X1) / 2, y: HB + 38, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "kawy i temp. losowane osobno");
    }

    function render(upto) {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      drawStage(api.stage, step, upto === undefined ? null : upto);
      if (step === 1) drawLog(api.low);
      else if (step === 2) drawScatter(api.low);
      else drawHist(api.low, step);
    }

    function runOne(fast, done) {
      var d = season(), step = api.step();
      st.last = d;
      tween(fast ? 260 : 1800, function (u) {
        api.stage.textContent = "";
        drawStage(api.stage, step, Math.max(1, Math.ceil(u * N)));
      }, function () {
        if (step >= 3) {
          // żeton z odczytu r leci do histogramu; słupek rośnie po lądowaniu
          var hr = histR(step), h = histInfo(), i = binOf(d.r);
          var x0 = 540, y0 = 52, x1 = slotX(i, hr), y1 = HB - h.u * (st.non[i] + st.sig[i]) - 6;
          render();
          var tok = svg("circle", { r: 7, cx: x0, cy: y0, class: "lc-sc-token" + (d.hit ? " is-hit" : "") }, api.fly);
          tween(fast ? 160 : 520, function (u) {
            var e = ease(u);
            tok.setAttribute("cx", x0 + (x1 - x0) * e);
            tok.setAttribute("cy", y0 + (y1 - y0) * e);
          }, function () { api.fly.textContent = ""; commit(d); render(); done(); });
        } else { commit(d); render(); done(); }
      });
    }

    function runMany(m) {
      for (var i = 0; i < m; i++) commit(season());
      render();
    }

    return {
      render: function () { render(); },
      reset: function () {
        st.seasons = []; st.last = null; st.nsig = 0;
        st.sig = new Array(NB).fill(0); st.non = new Array(NB).fill(0);
        render();
      },
      opt: function (name, v) { if (name === "phi") { st.phi = Number(v); this.reset(); } },
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
