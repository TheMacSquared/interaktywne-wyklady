// PROTOTYPY PUNKTÓW STOP 5A–5E (2026-11-09). Osobny silnik, żeby nie ruszać
// scen ANOVA w scenes.js (tamten skaner oznacza każde .lc-sc jako gotowe).
// Kontener: .p5-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper)
// albo wewnątrz .p5-host (jeden obraz z suwakiem, bez kroków).
// config.kind:
//   "smoke"   5A czujka dymu: próg wymienia fałszywe alarmy na przeoczone pożary
//   "jelly"   5D żelki i trądzik: 20 kolorów, 20 testów, seria z alarmem
//   "tickets" 5E mandaty: świat bez związku → porównanie → χ² (kadry)
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
    if (on && state) svg("circle", { cx: x, cy: y, r: r + 7, class: "lc-sc-halo" }, g);
    svg("circle", { cx: x, cy: y, r: r,
      class: !on ? "p5-lamp" : state ? "p5-lamp is-alarm" : "p5-lamp is-ok" }, g);
  }
  function gauge(g, o) {
    var rate = o.N ? o.hits / o.N : 0;
    var gx = function (r) { return o.x0 + (o.x1 - o.x0) * Math.min(r, o.max) / o.max; };
    svg("text", { x: o.x0, y: o.y - 8, class: "lc-sc-n" }, g,
      o.label + ": " + o.N + " · " + o.hitLabel + ": " + (o.N ? pct(rate) : "—"));
    svg("rect", { x: o.x0, y: o.y, width: o.x1 - o.x0, height: 12, rx: 3, class: "p5-track" }, g);
    if (o.N) svg("rect", { x: o.x0, y: o.y, width: Math.max(2, gx(rate) - o.x0), height: 12, rx: 3, class: "p5-gauge" }, g);
    (o.ticks || []).forEach(function (t, i, a) {
      svg("text", { x: gx(t), y: o.y + 26, "text-anchor": i === 0 ? "start" : i === a.length - 1 ? "end" : "middle",
        class: "lc-sc-tick" }, g, pct(t, 0));
    });
    (o.marks || []).forEach(function (m) {
      var x = gx(m);
      svg("line", { x1: x, x2: x, y1: o.y - 4, y2: o.y + 16, class: "lc-sc-param" }, g);
    });
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
  // 5D — ŻELKI I TRĄDZIK: 20 kolorów, 20 testów przy α = 0.05
  // H₀ prawdziwa dla każdego koloru, więc p-wartość ciągłego testu ma rozkład
  // jednostajny: losujemy ją wprost (to samo co test t na danych bez efektu).
  // =========================================================================
  KINDS.jelly = function (cfg, api) {
    var COLORS = cfg.colors, NAMES = cfg.names, K = COLORS.length, ALPHA = cfg.alpha || 0.05;
    var st = { series: 0, hit: 0, last: null };

    function series() {
      var p = [];
      for (var i = 0; i < K; i++) p.push(Math.random());
      var first = -1;
      for (i = 0; i < K; i++) if (p[i] < ALPHA) { first = i; break; }
      return { p: p, first: first };
    }
    function beanX(i) { return 70 + (i % 10) * 54; }
    function beanY(i) { return i < 10 ? 110 : 190; }

    function drawStage(g, step, d, upto) {
      svg("rect", { x: 14, y: 12, width: 196, height: 24, rx: 4, class: "p5-sign" }, g);
      svg("text", { x: 112, y: 29, "text-anchor": "middle", class: "p5-sign-t" }, g, "BADANIE: ŻELKI");
      for (var i = 0; i < K; i++) {
        var x = beanX(i), y = beanY(i);
        svg("ellipse", { cx: x, cy: y, rx: 18, ry: 11, transform: "rotate(-18 " + x + " " + y + ")",
          fill: COLORS[i], class: "p5-bean" }, g);
        var tested = d && i < upto;
        lampAt(g, x, y + 30, tested ? d.p[i] < ALPHA : null, 6);
      }
      if (d && upto >= K) {
        if (d.first >= 0) {
          svg("rect", { x: 230, y: 230, width: 396, height: 34, rx: 4, class: "p5-headline" }, g);
          svg("text", { x: 428, y: 253, "text-anchor": "middle", class: "p5-headline-t" }, g,
            NAMES[d.first].toUpperCase() + " ŻELKI A TRĄDZIK: JEST ZWIĄZEK!");
          svg("text", { x: 70, y: 252, class: "lc-sc-n" }, g, "p = " + fmt(d.p[d.first], 3));
        } else {
          svg("text", { x: 70, y: 252, class: "lc-sc-n" }, g, "żaden kolor nie dał alarmu");
        }
      }
    }
    function drawLow(g, step) {
      if (step < 2) return;
      gauge(g, { x0: 60, x1: 610, y: 320, label: "serii po 20 kolorów", hitLabel: "z co najmniej jednym alarmem",
        N: st.series, hits: st.hit, max: 1, ticks: [0, 0.25, 0.5, 0.75, 1],
        marks: step >= 3 ? [ALPHA, 1 - Math.pow(1 - ALPHA, K)] : [] });
      if (step >= 3) {
        readout(g, 60, 392, [
          { dash: "lc-sc-param", text: "jeden test: " + pct(ALPHA, 0) + " · " + K + " testów: " + pct(1 - Math.pow(1 - ALPHA, K), 0) }
        ]);
      }
    }
    function render(d, upto) {
      var step = api.step();
      api.stage.textContent = ""; api.low.textContent = "";
      var dd = d === undefined ? st.last : d;
      drawStage(api.stage, step, dd, upto === undefined ? K : upto);
      drawLow(api.low, step);
    }
    function commit(d) { st.series += 1; if (d.first >= 0) st.hit += 1; st.last = d; }
    return {
      render: function () { render(); },
      reset: function () { st.series = 0; st.hit = 0; st.last = null; render(); },
      opt: function () {},
      go: function (done) {
        var d = series();
        tween(REDUCE ? 0 : 2200, function (u) { render(d, Math.floor(u * K + 1e-9)); }, function () { commit(d); render(); done(); });
      },
      many: function (m, done) { for (var i = 0; i < m; i++) commit(series()); render(); done(); }
    };
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
