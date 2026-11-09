// PROTOTYPY PUNKTÓW STOP 5A–5E (2026-11-09). Osobny silnik, żeby nie ruszać
// scen ANOVA w scenes.js (tamten skaner oznacza każde .lc-sc jako gotowe).
// Kontener: .p5-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper)
// albo wewnątrz .p5-host (jeden obraz z suwakiem, bez kroków).
// config.kind:
//   "smoke"   5A czujka dymu: próg wymienia fałszywe alarmy na przeoczone pożary
//   "jelly"   5D żelki i trądzik: jedna chmura wyników cięta na 20 podgrup kolorów (kadry)
//   "tickets" 5E mandaty: świat bez związku → porównanie → χ² (kadry)
//   "tickets2" 5E-2 ta sama historia w oprawie i na danych sceny „stop” z wykładu 02
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
  // 5A — CZUJKA DYMU: próg czułości (jeden obraz z „co, jeśli”)
  // =========================================================================
  KINDS.smoke = function (cfg, api) {
    var M0 = cfg.m0, S0 = cfg.s0, M1 = cfg.m1, S1 = cfg.s1, LO = 0, HI = cfg.xmax || 80;
    var st = { t: cfg.t || 32 };
    var PL = 60, PR = 610, PT = 70, PB = 300, YMAX = 1.1 * Math.max(dnorm(M0, M0, S0), dnorm(M1, M1, S1));
    var X = function (v) { return PL + (PR - PL) * (v - LO) / (HI - LO); };
    var Y = function (d) { return PB - (PB - PT) * d / YMAX; };
    var f0 = function (v) { return dnorm(v, M0, S0); }, f1 = function (v) { return dnorm(v, M1, S1); };

    function toast(g, x, y) {
      svg("rect", { x: x - 16, y: y - 10, width: 32, height: 20, rx: 6, class: "p5-toaster" }, g);
      svg("rect", { x: x - 9, y: y - 16, width: 7, height: 8, rx: 2, class: "p5-bread" }, g);
      svg("rect", { x: x + 2, y: y - 16, width: 7, height: 8, rx: 2, class: "p5-bread" }, g);
    }
    function flame(g, x, y) {
      svg("path", { d: "M" + x + " " + (y + 12) + " C" + (x - 14) + " " + (y + 10) + " " + (x - 10) + " " + (y - 6) + " " + x + " " + (y - 16) +
        " C" + (x + 2) + " " + (y - 6) + " " + (x + 14) + " " + (y - 2) + " " + x + " " + (y + 12) + " Z", class: "p5-flame" }, g);
    }
    function detector(g, alarmish) {
      svg("rect", { x: 14, y: 12, width: 612, height: 6, class: "p5-ceiling" }, g);
      svg("rect", { x: 18, y: 18, width: 44, height: 10, rx: 3, class: "p5-detector" }, g);
      lampAt(g, 40, 36, alarmish ? true : null, 5);
    }

    function render() {
      var g = api.stage; g.textContent = ""; api.low.textContent = "";
      var t = st.t, fa = 1 - pnorm((t - M0) / S0), miss = pnorm((t - M1) / S1);
      detector(g, false);
      svg("path", { d: areaPath(X, Y, f0, t, HI), class: "p5-area-fa" }, g);
      svg("path", { d: areaPath(X, Y, f1, LO, t), class: "p5-area-miss" }, g);
      svg("path", { d: curvePath(X, Y, f0, LO, HI), class: "p5-curve0" }, g);
      svg("path", { d: curvePath(X, Y, f1, LO, HI), class: "p5-curve1" }, g);
      toast(g, X(M0), Y(f0(M0)) - 22);
      flame(g, X(M1), Y(f1(M1)) - 24);
      var tx = X(t);
      svg("line", { x1: tx, x2: tx, y1: PT - 20, y2: PB, class: "p5-thresh" }, g);
      svg("circle", { cx: tx, cy: PT - 20, r: 7, class: "p5-handle" }, g);
      svg("text", { x: tx + 12, y: PT - 15, class: "lc-sc-th-t" }, g, "próg");
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

    function render() {
      var step = api.step(), g = api.stage, lo = api.low;
      g.textContent = ""; lo.textContent = "";
      var total = O[0][0] + O[1][0], all = N * 2;
      for (var i = 0; i < 2; i++) {
        var obs = O[i][0], exp = E[i][0];
        var shown = step === 2 ? exp : obs;
        svg("text", { x: BLK[i][0], y: TOP - 18, class: "lc-sc-rowlab" }, g,
          ROWS[i] + " · " + (step === 2 ? "oczekiwane " + fmt(exp, 0) : obs) + " z " + N + " z mandatem");
        for (var k = 0; k < N; k++) {
          var c = cell(i, k), on = k < shown;
          svg("circle", { cx: c.x, cy: c.y, r: 8,
            class: "p5-driver" + (on ? (step === 2 ? " is-exp" : " is-ticket") : "") }, g);
          if (step >= 3) {
            // brakujące (oczekiwane, a nie ma) i nadmiarowe (są, a nie oczekiwane)
            if (k >= obs && k < exp) svg("circle", { cx: c.x, cy: c.y, r: 10.5, class: "p5-ring is-gap" }, g);
            if (k >= exp && k < obs) svg("circle", { cx: c.x, cy: c.y, r: 10.5, class: "p5-ring is-extra" }, g);
          }
        }
      }
      var y = TOP + 10 * DY + 26;
      if (step === 1) {
        readout(g, 50, y, [{ box: "p5-driver is-ticket", text: "mandat" }, { text: "łącznie " + total + " mandatów na " + all + " kontroli" }]);
      } else if (step === 2) {
        readout(g, 50, y, [{ box: "p5-driver is-exp", text: "oczekiwany mandat" },
          { text: total + " / " + all + " = " + pct(total / all, 0) + " w każdej grupie" }]);
      } else {
        readout(g, 50, y, [{ text: ROWS[0] + ": " + O[0][0] + " zamiast " + fmt(E[0][0], 0) },
          { text: ROWS[1] + ": " + O[1][0] + " zamiast " + fmt(E[1][0], 0) }]);
      }
      if (step < 4) return;
      // χ²: wkład każdej komórki (O − E)² / E
      var cells = [], chi = 0;
      for (i = 0; i < 2; i++) for (var j = 0; j < 2; j++) {
        var v = Math.pow(O[i][j] - E[i][j], 2) / E[i][j];
        chi += v;
        cells.push({ lab: ROWS[i] + (j === 0 ? ", mandat" : ", bez mandatu"), o: O[i][j], e: E[i][j], v: v });
      }
      var x0 = 50, x1 = 610, yb = 340, scale = (x1 - x0) / Math.max(10, Math.ceil(chi));
      var x = x0;
      cells.forEach(function (cc, k) {
        var w = cc.v * scale;
        svg("rect", { x: x, y: yb, width: Math.max(1, w - 2), height: 24, class: "p5-chi" + (k % 2 ? " is-no" : "") }, lo);
        svg("text", { x: x + w / 2, y: yb + 42, "text-anchor": "middle", class: "lc-sc-tick" }, lo, fmt(cc.v, 2));
        x += w;
      });
      svg("text", { x: x0, y: yb - 12, class: "lc-sc-n" }, lo,
        "(O − E)² / E w czterech komórkach · suma χ² = " + fmt(chi, 2));
      svg("text", { x: x0, y: yb + 66, class: "lc-sc-sub" }, lo,
        "od lewej: " + cells.map(function (cc) { return cc.lab; }).join(" · "));
    }
    return { render: render, reset: render, opt: function () {} };
  };

  // =========================================================================
  // 5E-2 — KONTROLA DROGOWA Z WYKŁADU 02 (ta sama oprawa i dane: 22 kobiety,
  // 48 mężczyzn, 4 i 15 mandatów). Kadry: tabela → świat bez związku (paski
  // 100% i wspólne 27%) → braki i nadmiary w przegródkach → χ².
  // Ludziki przechodzą między kadrami jak w scenie „stop” z wykładu 02.
  // =========================================================================
  KINDS.tickets2 = function (cfg, api) {
    var SEX = cfg.sex, FINE = cfg.fine, N = SEX.length;
    var ROWS = ["K", "M"], ROWNAME = { K: "kobiety", M: "mężczyźni" };
    var COLX = { 1: [170, 345], 0: [362, 626] }, ROWY = { K: [62, 138], M: [154, 290] };
    var GX = 20, GY = 28, NUMW = 50, BAR = { x0: 170, x1: 610, h: 30 };
    var cnt = { K: { 0: 0, 1: 0 }, M: { 0: 0, 1: 0 } }, nRow = { K: 0, M: 0 };
    var rank = new Array(N), rowRank = new Array(N), i, j;
    for (i = 0; i < N; i++) { rank[i] = cnt[SEX[i]][FINE[i]]++; nRow[SEX[i]]++; }
    var seen = { K: 0, M: 0 };
    for (var f = 1; f >= 0; f--) for (j = 0; j < N; j++) if (FINE[j] === f) rowRank[j] = seen[SEX[j]]++;
    var nFine = cnt.K[1] + cnt.M[1], share = nFine / N;
    var E = {};
    ROWS.forEach(function (r) { E[r] = { 1: nRow[r] * share, 0: nRow[r] * (1 - share) }; });
    var st = { last: 0, pos: null, stop: null };

    function per(fi) { return Math.floor((COLX[fi][1] - COLX[fi][0] - 12 - NUMW) / GX); }
    function slot(r, fi, k) {
      return { x: COLX[fi][0] + 16 + (k % per(fi)) * GX, y: ROWY[r][0] + 32 + Math.floor(k / per(fi)) * GY };
    }
    function barY(r) { return (ROWY[r][0] + ROWY[r][1]) / 2; }

    function tokens(step) {
      var out = [];
      for (var i = 0; i < N; i++) {
        var sx = SEX[i], fi = FINE[i], t;
        if (step === 2) {
          var w = (BAR.x1 - BAR.x0) / nRow[sx];
          t = { x: BAR.x0 + (rowRank[i] + 0.5) * w, y: barY(sx) + 8, s: 12, o: 0 };
        } else {
          var p = slot(sx, fi, rank[i]);
          t = { x: p.x, y: p.y, s: 20, o: step === 4 ? 0.25 : 1 };
        }
        out.push(t);
      }
      return out;
    }

    function person(g, x, y, s, cls, o, fine) {
      var q = svg("g", { class: "p5-person " + cls, opacity: o }, g);
      svg("circle", { cx: x, cy: y - s * 0.8, r: s * 0.17 }, q);
      svg("rect", { x: x - s * 0.18, y: y - s * 0.6, width: s * 0.36, height: s * 0.6, rx: s * 0.13 }, q);
      if (fine !== undefined) {
        svg("rect", { x: x + s * 0.12, y: y - s * 0.52, width: s * 0.26, height: s * 0.34, rx: s * 0.04,
          class: "p5-ticket " + (fine ? "is-fine" : "is-warn") }, q);
      }
      return q;
    }

    function cells(g) {
      ROWS.forEach(function (r) {
        [1, 0].forEach(function (f) {
          svg("rect", { x: COLX[f][0], y: ROWY[r][0], width: COLX[f][1] - COLX[f][0],
            height: ROWY[r][1] - ROWY[r][0], rx: 6, class: "p5-cell" }, g);
        });
      });
    }
    function heads(g) {
      svg("text", { x: (COLX[1][0] + COLX[1][1]) / 2, y: 52, "text-anchor": "middle", class: "p5-colhead" }, g, "mandat");
      svg("text", { x: (COLX[0][0] + COLX[0][1]) / 2, y: 52, "text-anchor": "middle", class: "p5-colhead" }, g, "pouczenie");
    }
    function rowLabels(g) {
      ROWS.forEach(function (r) {
        svg("text", { x: 156, y: barY(r) + 5, "text-anchor": "end", class: "p5-rowname is-" + r.toLowerCase() }, g, ROWNAME[r]);
      });
    }
    function pctRow(r) { return cnt[r][1] / nRow[r]; }

    function back(step, g) {
      if (step !== 2) cells(g);
      if (step === 2) {
        ROWS.forEach(function (r) {
          var y = barY(r) - BAR.h / 2, xm = BAR.x0 + (BAR.x1 - BAR.x0) * pctRow(r);
          svg("rect", { x: BAR.x0, y: y, width: xm - BAR.x0, height: BAR.h, class: "p5-bar is-fine" }, g);
          svg("rect", { x: xm, y: y, width: BAR.x1 - xm, height: BAR.h, class: "p5-bar is-warn" }, g);
        });
      }
    }

    function front(step, g) {
      rowLabels(g);
      if (step === 1) {
        heads(g);
        ROWS.forEach(function (r) {
          [1, 0].forEach(function (f) {
            svg("text", { x: COLX[f][1] - 10, y: barY(r) + 6, "text-anchor": "end",
              class: "p5-count" + (f ? " is-hit" : "") }, g, String(cnt[r][f]));
          });
        });
        readout(g, 170, 326, [{ text: "mandat: kobiety " + cnt.K[1] + " z " + nRow.K + " (" + pct(pctRow("K"), 0) +
          ") · mężczyźni " + cnt.M[1] + " z " + nRow.M + " (" + pct(pctRow("M"), 0) + ")" }]);
      }
      if (step === 2) {
        ROWS.forEach(function (r) {
          var xm = BAR.x0 + (BAR.x1 - BAR.x0) * pctRow(r);
          svg("text", { x: xm + 6, y: barY(r) - BAR.h / 2 - 8, class: "p5-count is-hit" }, g, pct(pctRow(r), 0));
        });
        var xs = BAR.x0 + (BAR.x1 - BAR.x0) * share;
        svg("line", { x1: xs, x2: xs, y1: ROWY.K[0] + 10, y2: ROWY.M[1] - 10, class: "lc-sc-param" }, g);
        [0, 50, 100].forEach(function (v) {
          var x = BAR.x0 + (BAR.x1 - BAR.x0) * v / 100, y = barY("M") + BAR.h / 2;
          svg("line", { x1: x, x2: x, y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
          svg("text", { x: x, y: y + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, v + "%");
        });
        readout(g, 170, 326, [{ dash: "lc-sc-param", text: "świat bez związku: w obu wierszach " +
          nFine + " z " + N + " = " + pct(share, 0) + " mandatów" }]);
      }
      if (step === 3) {
        heads(g);
        ROWS.forEach(function (r) {
          [1, 0].forEach(function (f) {
            var o = cnt[r][f], e = E[r][f], er = Math.round(e), k;
            // brakujący (oczekiwani, a ich nie ma): kontur; nadmiarowi: obwódka
            for (k = o; k < er; k++) { var p = slot(r, f, k); person(g, p.x, p.y, 20, "is-ghost", 1); }
            for (k = er; k < o; k++) {
              var q = slot(r, f, k);
              svg("circle", { cx: q.x, cy: q.y - 9, r: 11, class: "p5-ring is-extra" }, g);
            }
            svg("text", { x: COLX[f][1] - 10, y: barY(r) + 6, "text-anchor": "end", class: "p5-count" + (f ? " is-hit" : "") }, g, String(o));
            svg("text", { x: COLX[f][1] - 10, y: barY(r) + 24, "text-anchor": "end", class: "lc-sc-tick" }, g, "E " + fmt(e, 1));
          });
        });
        readout(g, 170, 326, [{ text: "kontur: oczekiwany, a go nie ma · obwódka: ponad oczekiwania" }]);
      }
      if (step === 4) {
        heads(g);
        var chi = 0, parts = [];
        ROWS.forEach(function (r) {
          [1, 0].forEach(function (f) {
            var v = Math.pow(cnt[r][f] - E[r][f], 2) / E[r][f];
            chi += v; parts.push(fmt(v, 2));
            svg("text", { x: (COLX[f][0] + COLX[f][1]) / 2, y: barY(r) + 8, "text-anchor": "middle", class: "p5-contrib" }, g, fmt(v, 2));
          });
        });
        readout(g, 170, 326, [{ text: "χ² = " + parts.join(" + ") + " = " + fmt(chi, 2) }]);
      }
    }

    function drawAll(step, pos) {
      var g = api.stage; g.textContent = ""; api.low.textContent = "";
      back(step, g);
      var showBadge = step !== 2;
      pos.forEach(function (t, i) {
        person(g, t.x, t.y, t.s, "is-" + SEX[i].toLowerCase(), t.o, showBadge ? FINE[i] : undefined);
      });
    }

    function render() {
      var step = api.step(), next = tokens(step), prev = st.pos;
      if (st.stop) st.stop = null;
      var tok = (st.tok = (st.tok || 0) + 1);
      if (prev && st.last && st.last !== step && !REDUCE) {
        tween(700, function (u) {
          if (tok !== st.tok) return;
          var e = u * u * (3 - 2 * u);
          drawAll(step, next.map(function (t, i) {
            var a = prev[i];
            return { x: a.x + (t.x - a.x) * e, y: a.y + (t.y - a.y) * e, s: a.s + (t.s - a.s) * e, o: a.o + (t.o - a.o) * e };
          }));
        }, function () { if (tok === st.tok) { drawAll(step, next); front(step, api.stage); } });
      } else { drawAll(step, next); front(step, api.stage); }
      st.pos = next; st.last = step;
    }
    return { render: render, reset: function () { st.last = 0; st.pos = null; render(); }, opt: function () {} };
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
