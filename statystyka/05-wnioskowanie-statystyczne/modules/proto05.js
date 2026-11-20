// SCENY Ryc. 3.1, 7.1 i 9.1 (dawne prototypy 5A, 5E, 5D z 2026-11-09). Osobny silnik, żeby nie ruszać
// scen ANOVA w scenes.js (tamten skaner oznacza każde .lc-sc jako gotowe).
// Kontener: .p5-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper)
// albo wewnątrz .p5-host (jeden obraz z suwakiem, bez kroków).
// config.kind:
//   "smoke"   Ryc. 3.1 (dawny 5A) czujka dymu: próg wymienia fałszywe alarmy na przeoczone pożary
//   "jelly"   Ryc. 9.1 (dawny 5D) żelki i trądzik: jedna chmura wyników cięta na 20 podgrup kolorów (kadry)
//   "tickets" Ryc. 7.1 (dawny 5E) mandaty: kontrole bez podziału → pytanie → świat bez związku → archiwum → różnice (kadry)
// Sterowanie: [data-sc-act] go | m10 | m100 | m1000, [data-sc-opt] "nazwa:wartość",
// input[data-p5-range] "nazwa" (suwak, wartość liczbowa). Rysuje SVG bez serwera R.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640;
  var REDUCE = typeof window !== "undefined" && window.matchMedia &&
    window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function fmt(x, d) { return Number(x).toFixed(d); }
  function pct(x, d) { return fmt(100 * x, d === undefined ? 1 : d) + "%"; }

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

  // --- rozkład normalny ------------------------------------------------------
  // erf: Abramowitz–Stegun 7.1.26, błąd < 1.5e-7
  function erf(x) {
    var s = x < 0 ? -1 : 1; x = Math.abs(x);
    var t = 1 / (1 + 0.3275911 * x);
    var y = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-x * x);
    return s * y;
  }
  function pnorm(z) { return 0.5 * (1 + erf(z / Math.SQRT2)); }
  function dnorm(x, m, s) { var z = (x - m) / s; return Math.exp(-0.5 * z * z) / (s * Math.sqrt(2 * Math.PI)); }
  function rnorm() {
    var u = 0, v = 0;
    while (u === 0) u = Math.random();
    while (v === 0) v = Math.random();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }

  function axisX(g, x0, x1, y, lo, hi, by, d, title, map) {
    var X = map || function (v) { return x0 + (x1 - x0) * (v - lo) / (hi - lo); };
    svg("line", { x1: x0, x2: x1, y1: y, y2: y, class: "lc-sc-axis" }, g);
    for (var v = lo; v <= hi + 1e-9; v += by) {
      var x = X(v);
      svg("line", { x1: x, x2: x, y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
      svg("text", { x: x, y: y + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, fmt(v, d));
    }
    if (title) svg("text", { x: x1, y: y + 38, "text-anchor": "end", class: "lc-sc-axtitle" }, g, title);
  }
  // pole pod krzywą gęstości od a do b
  function areaPath(X, Y, f, a, b) {
    var pts = ["M", X(a), Y(0)];
    for (var i = 0; i <= 80; i++) { var v = a + (b - a) * i / 80; pts.push("L", X(v), Y(f(v))); }
    pts.push("L", X(b), Y(0), "Z");
    return pts.join(" ");
  }
  function curvePath(X, Y, f, a, b) {
    var pts = [];
    for (var i = 0; i <= 160; i++) { var v = a + (b - a) * i / 160; pts.push(i ? "L" : "M", X(v), Y(f(v))); }
    return pts.join(" ");
  }
  // jeden krótki odczyt: [znacznik] tekst · ...
  function readout(g, x, y, items) {
    items.forEach(function (it) {
      if (it.box) { svg("rect", { x: x, y: y - 10, width: 18, height: 12, rx: 2, class: it.box }, g); x += 24; }
      if (it.dash) { svg("line", { x1: x, x2: x + 20, y1: y - 4, y2: y - 4, class: it.dash }, g); x += 26; }
      var t = svg("text", { x: x, y: y, class: "lc-sc-n" + (it.cls ? " " + it.cls : "") }, g, it.text);
      x += it.text.length * 7.8 + 22;
      return t;
    });
  }
  function lampAt(g, x, y, state, r) {
    r = r || 10;
    var on = state === true || state === false;
    if (on && state) svg("circle", { cx: x, cy: y, r: r + 5, class: "lc-sc-halo" }, g);
    svg("circle", { cx: x, cy: y, r: r,
      class: !on ? "p5-lamp" : state ? "p5-lamp is-alarm" : "p5-lamp is-ok" }, g);
  }
  var KINDS = {};

  // =========================================================================
  // Ryc. 3.1 (dawny 5A) — CZUJKA DYMU: próg czułości (jeden obraz z „co, jeśli”)
  // =========================================================================
  KINDS.smoke = function (cfg, api) {
    var M0 = cfg.m0, S0 = cfg.s0, M1 = cfg.m1, S1 = cfg.s1, LO = 0, HI = cfg.xmax || 80;
    var st = { t: cfg.t || 32 };
    var PL = 60, PR = 610, PT = 118, PB = 350, YMAX = 1.1 * Math.max(dnorm(M0, M0, S0), dnorm(M1, M1, S1));
    var X = function (v) { return PL + (PR - PL) * (v - LO) / (HI - LO); };
    var Y = function (d) { return PB - (PB - PT) * d / YMAX; };
    var f0 = function (v) { return dnorm(v, M0, S0); }, f1 = function (v) { return dnorm(v, M1, S1); };

    // Toster z dwiema grzankami i smużką dymu; środek obudowy w (x, y).
    function toast(g, x, y) {
      svg("path", { d: "M" + (x - 9) + " " + (y - 24) + " q-5 -5 0 -10 q5 -5 0 -10 M" + (x + 8) + " " + (y - 24) + " q-5 -5 0 -10 q5 -5 0 -10", class: "p5-smoke" }, g);
      svg("rect", { x: x - 15, y: y - 21, width: 12, height: 16, rx: 3.5, class: "p5-bread" }, g);
      svg("rect", { x: x + 3, y: y - 21, width: 12, height: 16, rx: 3.5, class: "p5-bread" }, g);
      svg("rect", { x: x - 23, y: y - 9, width: 46, height: 25, rx: 8, class: "p5-toaster" }, g);
      svg("rect", { x: x - 17, y: y - 9, width: 34, height: 3, rx: 1.5, class: "p5-toaster-slot" }, g);
      svg("rect", { x: x + 23, y: y - 3, width: 5, height: 9, rx: 2, class: "p5-toaster-base" }, g);
      svg("circle", { cx: x - 12, cy: y + 6, r: 3.2, class: "p5-toaster-dial" }, g);
      svg("rect", { x: x - 26, y: y + 15, width: 52, height: 4.5, rx: 2, class: "p5-toaster-base" }, g);
    }
    // Płomień w trzech warstwach; środek w (x, y).
    function flame(g, x, y) {
      svg("path", { d: "M" + x + " " + (y + 20) + " C" + (x - 21) + " " + (y + 18) + " " + (x - 18) + " " + (y - 4) + " " + (x - 4) + " " + (y - 23) +
        " C" + (x - 4) + " " + (y - 10) + " " + (x + 4) + " " + (y - 8) + " " + (x + 6) + " " + (y - 16) +
        " C" + (x + 18) + " " + (y - 4) + " " + (x + 21) + " " + (y + 16) + " " + x + " " + (y + 20) + " Z", class: "p5-flame" }, g);
      svg("path", { d: "M" + x + " " + (y + 18) + " C" + (x - 12) + " " + (y + 16) + " " + (x - 10) + " " + (y + 3) + " " + (x - 1) + " " + (y - 8) +
        " C" + (x + 2) + " " + (y + 1) + " " + (x + 11) + " " + (y + 5) + " " + x + " " + (y + 18) + " Z", class: "p5-flame-in" }, g);
      svg("path", { d: "M" + x + " " + (y + 17) + " C" + (x - 6) + " " + (y + 15) + " " + (x - 4) + " " + (y + 8) + " " + x + " " + (y + 3) +
        " C" + (x + 3) + " " + (y + 8) + " " + (x + 6) + " " + (y + 13) + " " + x + " " + (y + 17) + " Z", class: "p5-flame-core" }, g);
    }
    // Czujka wisi pod sufitem dokładnie nad progiem: próg to miejsce, w którym decyduje.
    function detector(g, x) {
      svg("rect", { x: 14, y: 10, width: 612, height: 7, class: "p5-ceiling" }, g);
      svg("rect", { x: x - 36, y: 17, width: 72, height: 11, rx: 4, class: "p5-det-body" }, g);
      svg("path", { d: "M" + (x - 30) + " 28 Q" + x + " 58 " + (x + 30) + " 28 Z", class: "p5-det-dome" }, g);
      [-14, -7, 7, 14].forEach(function (dx) {
        svg("line", { x1: x + dx, x2: x + dx * 0.8, y1: 31, y2: 37 - Math.abs(dx) * 0.12, class: "p5-det-vent" }, g);
      });
      svg("circle", { cx: x, cy: 35, r: 7, class: "lc-sc-halo" }, g);
      svg("circle", { cx: x, cy: 35, r: 3.6, class: "p5-det-led" }, g);
    }

    function render() {
      var g = api.stage; g.textContent = ""; api.low.textContent = "";
      var t = st.t, fa = 1 - pnorm((t - M0) / S0), miss = pnorm((t - M1) / S1);
      var tx = X(t);
      svg("rect", { x: tx, y: 46, width: PR - tx, height: PB - 46, class: "p5-zone-alarm" }, g);
      svg("path", { d: areaPath(X, Y, f0, t, HI), class: "p5-area-fa" }, g);
      svg("path", { d: areaPath(X, Y, f1, LO, t), class: "p5-area-miss" }, g);
      svg("path", { d: curvePath(X, Y, f0, LO, HI), class: "p5-curve0" }, g);
      svg("path", { d: curvePath(X, Y, f1, LO, HI), class: "p5-curve1" }, g);
      var y0 = Y(f0(M0)) - 30, y1 = Y(f1(M1)) - 34;
      toast(g, X(M0), y0);
      svg("text", { x: X(M0) + 36, y: y0 + 4, class: "p5-icon-t" }, g, "zwykły dzień");
      flame(g, X(M1), y1);
      svg("text", { x: X(M1) + 30, y: y1 + 6, class: "p5-icon-t" }, g, "pożar");
      svg("line", { x1: tx, x2: tx, y1: 43, y2: PB, class: "p5-thresh" }, g);
      detector(g, tx);
      svg("text", { x: tx - 44, y: 40, "text-anchor": "end", class: "p5-zone-t" }, g, "czujka milczy");
      svg("text", { x: tx + 44, y: 40, class: "p5-zone-t is-alarm" }, g, "ALARM");
      axisX(g, PL, PR, PB, LO, HI, 10, 0, "odczyt czujki (gęstość dymu, jedn. umowne)");
      var days = Math.round(365 * fa);
      readout(g, PL, PB + 62, [
        { box: "p5-area-fa", text: "fałszywe alarmy: " + pct(fa) + " zwykłych dni (≈ " + days + " w roku)" }
      ]);
      readout(g, PL, PB + 84, [{ box: "p5-area-miss", text: "przeoczone pożary: " + pct(miss) }]);
      if (api.status) api.status("Próg " + fmt(t, 0) + ": fałszywe alarmy " + pct(fa) + ", przeoczone pożary " + pct(miss) + ".");
    }
    return { render: render, reset: function () { st.t = cfg.t || 32; render(); },
      range: function (name, v) { if (name === "t") { st.t = v; render(); } } };
  };

  // =========================================================================
  // 5D — ŻELKI I TRĄDZIK (kadry, dane stałe z R). Jedna chmura wyników
  // jedzących żelki zostaje pocięta na 20 podgrup według ulubionego koloru;
  // średnie małych podgrup rozrzuca przypadek i jedna wypada poza zakres.
  // Pas = średnia niejedzących ± 1.96·SE różnicy (dla całej grupy albo podgrupy 20 osób).
  // =========================================================================
  KINDS.jelly = function (cfg, api) {
    var COLORS = cfg.colors, NAMES = cfg.names, K = COLORS.length, ALPHA = cfg.alpha || 0.05;
    var CTRL = cfg.ctrl, EAT = cfg.eat, GRP = cfg.grp, RM = cfg.rm, P = cfg.p, HIT = cfg.hit - 1;
    var MC = cfg.m_ctrl, ME = cfg.m_eat;
    var X0 = 110, X1 = 600, LO = 0, HI = 24;
    var YC = 66, YE = 150, R0 = 132, DR = 20.5, BAND = 7;
    var st = { last: 0, token: 0 };
    // stały rozrzut pionowy kropek (bez losowania przy każdym rysowaniu)
    function jit(i) { var v = Math.sin(i * 12.9898) * 43758.5453; return 2 * (v - Math.floor(v)) - 1; }
    function X(v) { return X0 + (X1 - X0) * (Math.max(LO, Math.min(HI, v)) - LO) / (HI - LO); }
    function rowY(g) { return R0 + g * DR; }

    function bean(g, x, y, i, s, cls) {
      s = s || 1;
      svg("ellipse", { cx: x, cy: y, rx: 14 * s, ry: 8 * s, transform: "rotate(-18 " + x + " " + y + ")",
        fill: COLORS[i], class: "p5-bean" + (cls ? " " + cls : "") }, g);
    }
    function band(g, m, half, y0, y1) {
      svg("rect", { x: X(m - half), y: y0, width: X(m + half) - X(m - half), height: y1 - y0, class: "p5-band" }, g);
    }
    function axis(g, y) {
      svg("line", { x1: X0, x2: X1, y1: y, y2: y, class: "lc-sc-axis" }, g);
      for (var v = 0; v <= HI; v += 4) {
        svg("line", { x1: X(v), x2: X(v), y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: X(v), y: y + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(v));
      }
      svg("text", { x: X1, y: y + 38, "text-anchor": "end", class: "lc-sc-axtitle" }, g, "liczba wyprysków");
    }
    function meanTick(g, m, y, h, cls) {
      svg("line", { x1: X(m), x2: X(m), y1: y - h, y2: y + h, class: "p5-mtick" + (cls ? " " + cls : "") }, g);
    }
    function ctrlStrip(g) {
      svg("text", { x: X0, y: YC - 16, class: "lc-sc-rowlab" }, g, "nie jedzą żelków · " + CTRL.length + " osób");
      CTRL.forEach(function (v, i) {
        svg("circle", { cx: X(v + 0.35 * jit(i + 999)), cy: YC + BAND * jit(i), r: 2.4, class: "p5-dot is-ctrl" }, g);
      });
      meanTick(g, MC, YC, 11);
    }

    // u: 0 = wszyscy jedzący w jednym pasie, 1 = rozdzieleni na wiersze kolorów
    function eaters(g, u, showRows) {
      EAT.forEach(function (v, i) {
        var gi = GRP[i] - 1, y0 = YE + 1.6 * BAND * jit(i), y1 = rowY(gi) + 0.7 * BAND * jit(i);
        var at = { cx: X(v + 0.35 * jit(i + 555)), cy: y0 + (y1 - y0) * u, r: 2.2,
          class: "p5-dot" + (u > 0.5 ? " is-row" : " is-eat") };
        if (u > 0.5) at.style = "fill:" + COLORS[gi];
        svg("circle", at, g);
      });
      if (!showRows) return;
      for (var k = 0; k < K; k++) {
        var y = rowY(k), alarm = P[k] < ALPHA;
        bean(g, 46, y, k, 0.75);
        lampAt(g, 80, y, alarm, 5);
        meanTick(g, RM[k], y, 8, alarm ? "is-alarm" : "");
      }
    }

    function frameQuestion(g) {
      ctrlStrip(g);
      band(g, MC, cfg.half_all, YE - 22, YE + 22);
      svg("text", { x: X0, y: YE - 30, class: "lc-sc-rowlab" }, g, "jedzą żelki · " + EAT.length + " osób");
      eaters(g, 0, false);
      meanTick(g, ME, YE, 13);
      svg("line", { x1: X(MC), x2: X(MC), y1: YC - 12, y2: YE + 26, class: "p5-cmean" }, g);
      lampAt(g, 80, YE, false, 8);
      axis(g, 210);
      readout(g, X0, 268, [{ box: "p5-band", text: "zakres przypadku wokół średniej niejedzących" },
        { text: "p = " + fmt(cfg.p_all, 2) }]);
    }
    function frameRows(g, u) {
      ctrlStrip(g);
      var y0 = R0 - 12, y1 = rowY(K - 1) + 12;
      if (u >= 1) band(g, MC, cfg.half_row, y0, y1);
      eaters(g, u, u >= 1);
      svg("line", { x1: X(MC), x2: X(MC), y1: YC - 12, y2: y1, class: "p5-cmean" }, g);
      axis(g, y1 + 8);
      if (u >= 1) {
        readout(g, X0, y1 + 66, [{ box: "p5-band", text: "zakres przypadku dla grupy 20 osób" }]);
      }
    }
    function framePaper(g) {
      svg("rect", { x: 120, y: 60, width: 400, height: 300, rx: 4, class: "p5-paper" }, g);
      svg("text", { x: 320, y: 96, "text-anchor": "middle", class: "p5-masthead" }, g, "KURIER NAUKOWY");
      svg("line", { x1: 140, x2: 500, y1: 108, y2: 108, class: "p5-rule" }, g);
      svg("text", { x: 320, y: 146, "text-anchor": "middle", class: "p5-head" }, g, NAMES[HIT].toUpperCase() + " ŻELKI");
      svg("text", { x: 320, y: 174, "text-anchor": "middle", class: "p5-head" }, g, "A TRĄDZIK: JEST ZWIĄZEK!");
      bean(g, 320, 245, HIT, 3.4);
      svg("text", { x: 320, y: 330, "text-anchor": "middle", class: "lc-sc-n" }, g, "p = " + fmt(P[HIT], 3) + " < 0.05");
    }

    function draw(step, u) {
      var g = api.stage; g.textContent = ""; api.low.textContent = "";
      if (step === 1) frameQuestion(g);
      else if (step === 2) frameRows(g, u === undefined ? 1 : u);
      else if (step === 3) framePaper(g);
      else {
        frameRows(g, 1);
        var hits = P.filter(function (p) { return p < ALPHA; }).length;
        readout(g, X0, rowY(K - 1) + 100, [{ text: K + " podgrup × " + pct(ALPHA, 0) + " = średnio " +
          fmt(K * ALPHA, 0) + " fałszywy alarm · tutaj: " + hits }]);
      }
    }
    function render() {
      var step = api.step(), tok = ++st.token;
      if (step === 2 && st.last === 1 && !REDUCE) {
        st.last = 2;
        tween(1400, function (u) { if (tok === st.token) draw(2, u * u * (3 - 2 * u)); }, function () {});
      } else { st.last = step; draw(step); }
    }
    return { render: render, reset: function () { st.last = 0; render(); }, opt: function () {} };
  };

  // =========================================================================
  // 5E — MANDATY: świat bez związku (kadry, dane stałe z R)
  // =========================================================================
  KINDS.tickets = function (cfg, api) {
    var O = cfg.obs, E = cfg.exp, ROWS = cfg.rows, N = cfg.n_row;  // O[i] = [mandat, brak], E jak O
    var BLK = [[50, "is-k"], [345, "is-m"]], TOP = 64, DX = 24, DY = 21;

    function cell(i, k) { return { x: BLK[i][0] + (k % 10) * DX + 12, y: TOP + Math.floor(k / 10) * DY + 10 }; }

    // Kadry (kolejność od 2026-11-20: najpierw pytanie o niezależność, potem dane z podziałem):
    //   1 kontrole bez podziału, 2 podział na płeć bez mandatów (pytanie), 3 świat bez związku
    //   (oczekiwane), 4 archiwum (obserwowane), 5 różnice. Wkłady do χ² liczy Ryc. 7.2.
    function render() {
      var step = api.step(), g = api.stage, lo = api.low;
      g.textContent = ""; lo.textContent = "";
      var total = O[0][0] + O[1][0], all = N * 2, y = TOP + 10 * DY + 26, k, c;
      if (step === 1) {
        // Jedna grupa 200 kierowców; 80 mandatów rozrzuconych po całej siatce (stały układ).
        svg("text", { x: 50, y: TOP - 18, class: "lc-sc-rowlab" }, g, all + " kontroli · " + total + " z mandatem");
        for (k = 0; k < all; k++) {
          svg("circle", { cx: 62 + (k % 20) * 27.5, cy: TOP + Math.floor(k / 20) * DY + 10, r: 8,
            class: "p5-driver" + ((k * 73 + 11) % all < total ? " is-ticket" : "") }, g);
        }
        readout(g, 50, y, [{ box: "p5-driver is-ticket", text: "mandat" }, { text: total + " mandatów na " + all + " kontroli" }]);
        return;
      }
      for (var i = 0; i < 2; i++) {
        var obs = O[i][0], exp = E[i][0];
        var shown = step === 2 ? 0 : step === 3 ? exp : obs;
        svg("text", { x: BLK[i][0], y: TOP - 18, class: "lc-sc-rowlab" }, g,
          ROWS[i] + " · " + (step === 2 ? N + " kontroli" : step === 3 ? "oczekiwane " + fmt(exp, 0) + " z " + N : obs + " z " + N + " z mandatem"));
        for (k = 0; k < N; k++) {
          c = cell(i, k);
          var on = k < shown;
          svg("circle", { cx: c.x, cy: c.y, r: 8,
            class: "p5-driver" + (on ? (step === 3 ? " is-exp" : " is-ticket") : "") }, g);
          if (step >= 5) {
            // brakujące (oczekiwane, a nie ma) i nadmiarowe (są, a nie oczekiwane)
            var rc = { cx: c.x, cy: c.y, r: 10.5 };
            if (k >= obs && k < exp) svg("circle", Object.assign({ class: "p5-ring is-gap" }, rc), g);
            if (k >= exp && k < obs) svg("circle", Object.assign({ class: "p5-ring is-extra" }, rc), g);
          }
        }
      }
      if (step === 2) {
        readout(g, 50, y, [{ text: total + " mandatów na " + all + " kontroli. Kto je dostał?" }]);
      } else if (step === 3) {
        readout(g, 50, y, [{ box: "p5-driver is-exp", text: "oczekiwany mandat" },
          { text: total + " / " + all + " = " + pct(total / all, 0) + " w każdej grupie" }]);
      } else if (step === 4) {
        readout(g, 50, y, [{ box: "p5-driver is-ticket", text: "mandat" },
          { text: ROWS[0] + ": " + O[0][0] + " z " + N }, { text: ROWS[1] + ": " + O[1][0] + " z " + N }]);
      } else {
        readout(g, 50, y, [{ text: ROWS[0] + ": " + O[0][0] + " zamiast " + fmt(E[0][0], 0) },
          { text: ROWS[1] + ": " + O[1][0] + " zamiast " + fmt(E[1][0], 0) }]);
      }
    }
    return { render: render, reset: render, opt: function () {} };
  };

  // =========================================================================
  // widget
  // =========================================================================
  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    var host = stepper || root.closest(".p5-host");
    if (!host) return;
    var cfg = JSON.parse(root.dataset.config || "{}");
    if (!KINDS[cfg.kind]) return;
    root.dataset.ready = "1";
    var s = svg("svg", { class: "lc-sc-svg", role: "img", viewBox: "0 0 " + W + " " + (cfg.height || 420),
      "aria-label": cfg.aria || "Scena" }, root);
    var live = host.querySelector(".p5-status");
    var api = {
      stage: svg("g", {}, s), low: svg("g", {}, s),
      step: function () { return stepper ? (Number(stepper.getAttribute("data-lc-step")) || 1) : 1; },
      status: live ? function (t) { live.textContent = t; } : null
    };
    var scene = KINDS[cfg.kind](cfg, api), busy = false;

    function refresh() {
      scene.render();
      host.querySelectorAll("[data-sc-act]").forEach(function (b) { b.disabled = busy; });
    }
    refresh();

    host.querySelectorAll("input[data-p5-range]").forEach(function (inp) {
      var out = host.querySelector("[data-p5-out='" + inp.getAttribute("data-p5-range") + "']");
      var upd = function () {
        if (out) out.textContent = inp.value;
        scene.range(inp.getAttribute("data-p5-range"), Number(inp.value));
      };
      inp.addEventListener("input", upd);
      upd();
    });

    host.addEventListener("click", function (e) {
      var o = e.target.closest("[data-sc-opt]");
      if (o && host.contains(o)) {
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
      if (b && host.contains(b) && scene.go) {
        if (busy) return;
        var a = b.getAttribute("data-sc-act");
        busy = true; refresh();
        var fin = function () { busy = false; refresh(); };
        if (a === "go") scene.go(fin); else scene.many(Number(a.replace("m", "")), fin);
        return;
      }
      if (e.target.closest('[data-lc-nav="reset"]')) setTimeout(function () { scene.reset(); refresh(); }, 0);
    });

    if (stepper) {
      new MutationObserver(function () { if (!busy) refresh(); })
        .observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
    }
  }

  if (typeof module !== "undefined" && module.exports) {
    module.exports = { pnorm: pnorm, dnorm: dnorm };
    return;
  }
  function scan() { document.querySelectorAll(".p5-sc:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
