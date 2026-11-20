// Ryc. 3.2 i 3.3 wykładu 04: budowa przedziału ufności dla średniej i dla różnicy średnich
// (wersja z 2026-11-20; wcześniej ci_construction.js kładł średnią, ± SE i przedział jeden
// pod drugim). Wspólny wygląd: pomiary jako stos kropek na osi, średnia jako gruba kreska,
// ± SE jako czarna klamra (tylko w swoim kroku), 95% przedział jako burgundowa klamra
// z granicami po bokach. Nigdy nie ma dwóch odcinków naraz.
//   .p32[data-x]                    Ryc. 3.2: 25 pomiarów wzrostu z N(170, 10)
//   .p32[data-kind="diff"][data-x][data-y]  Ryc. 3.3: 25 mężczyzn N(178, 7) i 25 kobiet N(165, 6);
//       u góry obie próby na osi wzrostu, niżej oś różnicy z zerem (wersja Welcha).
// Kroki prowadzi lc_step_widget(); przycisk [data-p32="new"] losuje nowe próby (lcDist).
(function () {
  "use strict";
  var NS = "http://www.w3.org/2000/svg", MU = 170, W = 640;
  function el(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function f(v, d) { return Number(v).toFixed(d); }
  function mean(x) { return x.reduce(function (a, b) { return a + b; }, 0) / x.length; }
  function variance(x) { var m = mean(x); return x.reduce(function (a, b) { return a + (b - m) * (b - m); }, 0) / (x.length - 1); }
  // Kwantyl 0.975 rozkładu t; bez lcDist (testy) zostaje wartość dla df = 24.
  function tcrit(df) { var D = typeof window !== "undefined" && window.lcDist; return D && D.qt ? D.qt(0.975, df) : 2.063899; }
  function stats(x) {
    var n = x.length, m = mean(x), s = Math.sqrt(variance(x)), se = s / Math.sqrt(n), t = tcrit(n - 1);
    return { x: x, n: n, m: m, s: s, se: se, t: t, me: t * se, lo: m - t * se, hi: m + t * se };
  }
  // Różnica średnich w wersji Welcha, jak ci_construction_stats(x, y) w R.
  function diffStats(x, y) {
    var a = variance(x) / x.length, b = variance(y) / y.length, d = mean(x) - mean(y), se = Math.sqrt(a + b);
    var df = (a + b) * (a + b) / (a * a / (x.length - 1) + b * b / (y.length - 1)), t = tcrit(df);
    return { x: x, y: y, m1: mean(x), m2: mean(y), s1: Math.sqrt(variance(x)), s2: Math.sqrt(variance(y)),
             d: d, se: se, df: df, t: t, me: t * se, lo: d - t * se, hi: d + t * se };
  }
  function draw(n, mu, sd) {
    var r = window.lcDist && window.lcDist.r;
    return r ? r.many(n, function () { return r.norm(mu, sd); }) : null;
  }
  function scale(lo, hi) { return function (v) { return 50 + (Math.min(hi, Math.max(lo, v)) - lo) / (hi - lo) * 540; }; }

  // Kropki ułożone w stos nad osią: każda w najniższym wolnym rzędzie.
  function dots(g, xs, X, base, r, cls) {
    var rows = [];
    xs.slice().sort(function (a, b) { return a - b; }).forEach(function (v) {
      var px = X(v), k = 0;
      while (rows[k] !== undefined && px - rows[k] < 2 * r + 1) k++;
      rows[k] = px;
      el("circle", { cx: px, cy: base - r - k * (2 * r + 1), r: r, class: "p32-dot" + (cls ? " " + cls : "") }, g);
    });
  }
  function axis(g, X, y, ticks) {
    el("line", { x1: 50, x2: 590, y1: y, y2: y, class: "p32-axis" }, g);
    ticks.forEach(function (v) {
      el("line", { x1: X(v), x2: X(v), y1: y, y2: y + 6, class: "p32-axis" }, g);
      el("text", { x: X(v), y: y + 24, class: "p32-tick" }, g, String(v));
    });
  }
  function seBracket(g, x1, x2, y, label) {
    el("line", { x1: x1, x2: x2, y1: y, y2: y, class: "p32-se" }, g);
    [x1, x2].forEach(function (x) { el("line", { x1: x, x2: x, y1: y - 8, y2: y + 8, class: "p32-se" }, g); });
    el("text", { x: x2 + 12, y: y + 5, class: "p32-lab is-start" }, g, label);
  }
  // Burgundowa klamra przedziału: górna belka na wysokości y, granice po bokach.
  function ciBracket(g, x1, x2, y, lo, hi) {
    el("path", { d: "M" + x1 + " " + (y + 14) + " v-14 H" + x2 + " v14", class: "p32-ci-bracket" }, g);
    el("text", { x: x1 - 8, y: y + 6, class: "p32-val is-ci is-end" }, g, lo);
    el("text", { x: x2 + 8, y: y + 6, class: "p32-val is-ci is-start" }, g, hi);
  }

  // ---- Ryc. 3.2: jedna średnia ---------------------------------------------------
  function drawMean(g, s, step) {
    var X = scale(145, 195), xm = X(s.m);
    dots(g, s.x, X, 190, 6);
    axis(g, X, 196, [150, 160, 170, 180, 190]);
    el("text", { x: 590, y: 246, class: "p32-axt" }, g, "wzrost (cm)");
    if (step < 2) return;
    if (step >= 4) {
      ciBracket(g, X(s.lo), X(s.hi), 72, f(s.lo, 2), f(s.hi, 2));
      el("text", { x: xm, y: 280, class: "p32-lab is-ci" }, g, "95% przedział ufności dla średniej");
    }
    el("line", { x1: xm, x2: xm, y1: step >= 4 ? 72 : 56, y2: 204, class: "p32-mean" }, g);
    el("text", { x: xm, y: 36, class: "p32-val" }, g, "x̄ = " + f(s.m, 2) + " cm");
    if (step === 3) seBracket(g, X(s.m - s.se), X(s.m + s.se), 66, "± SE = " + f(s.se, 2) + " cm");
  }
  function textMean(s, step) {
    if (step === 1) return s.n + " pomiarów wzrostu, każda kropka to jedna osoba. <i>x̄</i> = " + f(s.m, 2) + " cm, <i>s</i> = " + f(s.s, 2) + " cm.";
    if (step === 2) return "Średnia z próby <i>x̄</i> = " + f(s.m, 2) + " cm to estymata punktowa μ. Inna próba dałaby inną wartość.";
    if (step === 3) return "<i>SE</i> = <i>s</i>/√<i>n</i> = " + f(s.s, 2) + "/√" + s.n + " = " + f(s.se, 2) + " cm. Jeden błąd standardowy w każdą stronę to tylko około 67% ufności.";
    return "<i>t</i>* = " + f(s.t, 3) + " (df = " + (s.n - 1) + "), margines " + f(s.t, 3) + " · " + f(s.se, 2) + " = " + f(s.me, 2) + " cm. 95% CI: <b>[" + f(s.lo, 2) + ", " + f(s.hi, 2) +
      "]</b> cm. " + (s.lo <= MU && MU <= s.hi ? "Ten przedział zawiera μ = 170 cm." : "Ten przedział nie zawiera μ = 170 cm.");
  }

  // ---- Ryc. 3.3: różnica średnich ------------------------------------------------
  function drawDiff(g, s, step) {
    var X = scale(148, 198), XD = scale(-2, 22), x1 = X(s.m1), x2 = X(s.m2), xd = XD(s.d);
    el("text", { x: 50, y: 84, class: "p32-grp is-1" }, g, "Mężczyźni");
    el("text", { x: 590, y: 160, class: "p32-grp is-2 is-end" }, g, "Kobiety");
    dots(g, s.x, X, 92, 5, "is-1");
    dots(g, s.y, X, 168, 5, "is-2");
    axis(g, X, 174, [150, 160, 170, 180, 190]);
    el("text", { x: 590, y: 222, class: "p32-axt" }, g, "wzrost (cm)");
    if (step < 2) return;
    el("line", { x1: x1, x2: x1, y1: 34, y2: 96, class: "p32-mean" }, g);
    el("line", { x1: x2, x2: x2, y1: 112, y2: 172, class: "p32-mean" }, g);
    el("text", { x: x1, y: 24, class: "p32-val" }, g, "x̄₁ = " + f(s.m1, 2));
    el("text", { x: x2 - 10, y: 124, class: "p32-val is-end" }, g, "x̄₂ = " + f(s.m2, 2));
    if (step < 3) return;
    // Różnica jako odległość między średnimi, potem ta sama liczba na własnej osi z zerem.
    el("path", { d: "M" + x1 + " 100 V244 M" + x2 + " 176 V244", class: "p32-guide" }, g);
    el("path", { d: "M" + Math.min(x1, x2) + " 244 H" + Math.max(x1, x2), class: "p32-span" }, g);
    el("text", { x: (x1 + x2) / 2, y: 268, class: "p32-lab" }, g, "x̄₁ − x̄₂ = " + f(s.d, 2) + " cm");
    axis(g, XD, 384, [0, 5, 10, 15, 20]);
    el("line", { x1: XD(0), x2: XD(0), y1: 300, y2: 384, class: "p32-guide" }, g);
    el("text", { x: XD(0), y: 426, class: "p32-zero" }, g, "brak różnicy");
    el("text", { x: 590, y: 426, class: "p32-axt" }, g, "różnica średnich (cm)");
    if (step >= 5) {
      ciBracket(g, XD(s.lo), XD(s.hi), 330, f(s.lo, 2), f(s.hi, 2));
      el("text", { x: 320, y: 452, class: "p32-lab is-ci" }, g, "95% przedział ufności dla różnicy średnich");
    }
    el("line", { x1: xd, x2: xd, y1: step >= 5 ? 330 : 318, y2: 392, class: "p32-mean" }, g);
    el("text", { x: xd, y: 306, class: "p32-val" }, g, f(s.d, 2));
    if (step === 4) seBracket(g, XD(s.d - s.se), XD(s.d + s.se), 326, "± SE = " + f(s.se, 2) + " cm");
  }
  function textDiff(s, step) {
    var n1 = s.x.length, n2 = s.y.length;
    if (step === 1) return n1 + " mężczyzn (niebieskie kropki) i " + n2 + " kobiet (bursztynowe). Każda kropka to jedna osoba.";
    if (step === 2) return "<i>x̄</i><sub>1</sub> = " + f(s.m1, 2) + " cm, <i>x̄</i><sub>2</sub> = " + f(s.m2, 2) + " cm. Każda średnia ma własną niepewność.";
    if (step === 3) return "<i>x̄</i><sub>1</sub> − <i>x̄</i><sub>2</sub> = " + f(s.d, 2) + " cm. Dolna oś ma skalę różnicy: zero oznacza brak różnicy.";
    if (step === 4) return "<i>SE</i> = √(" + f(s.s1, 2) + "²/" + n1 + " + " + f(s.s2, 2) + "²/" + n2 + ") = " + f(s.se, 2) +
      " cm, mniej niż suma SE obu średnich (" + f(s.s1 / Math.sqrt(n1) + s.s2 / Math.sqrt(n2), 2) + " cm).";
    return "df Welcha ≈ " + f(s.df, 1) + ", <i>t</i>* = " + f(s.t, 3) + ", margines " + f(s.t, 3) + " · " + f(s.se, 2) + " = " + f(s.me, 2) +
      " cm. 95% CI: <b>[" + f(s.lo, 2) + ", " + f(s.hi, 2) + "]</b> cm. " +
      (s.lo <= 0 && 0 <= s.hi ? "Przedział obejmuje 0." : "Przedział nie obejmuje 0: mężczyźni są średnio wyżsi o co najmniej " + f(s.lo, 1) + " cm.");
  }

  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    if (!stepper) return;
    root.dataset.ready = "1";
    var diff = root.dataset.kind === "diff", x = JSON.parse(root.dataset.x);
    var s = diff ? diffStats(x, JSON.parse(root.dataset.y)) : stats(x);
    var out = document.getElementById(stepper.id + "_text"), box = root.querySelector(".p32-svg");
    function render() {
      var step = Number(stepper.getAttribute("data-lc-step")) || 1, last = step >= (diff ? 5 : 4);
      box.textContent = "";
      var svg = el("svg", { viewBox: "0 0 " + W + " " + (diff ? 466 : 296), role: "img",
        "aria-label": diff
          ? "Budowa przedziału ufności dla różnicy średnich, krok " + step + (step >= 3 ? ": różnica " + f(s.d, 2) + " cm" : "") +
            (last ? ", 95% przedział ufności od " + f(s.lo, 2) + " do " + f(s.hi, 2) + " cm" : "")
          : "Budowa przedziału ufności dla średniej, krok " + step + ": średnia " + f(s.m, 2) + " cm" +
            (last ? ", 95% przedział ufności od " + f(s.lo, 2) + " do " + f(s.hi, 2) + " cm" : "") }, box);
      if (diff) drawDiff(svg, s, step); else drawMean(svg, s, step);
      if (out) out.innerHTML = diff ? textDiff(s, step) : textMean(s, step);
    }
    stepper.addEventListener("click", function (e) {
      if (!e.target.closest('[data-p32="new"]')) return;
      var a = diff ? draw(25, 178, 7) : draw(25, 170, 10), b = diff ? draw(25, 165, 6) : null;
      if (!a) return;
      s = diff ? diffStats(a, b) : stats(a);
      render();
    });
    new MutationObserver(render).observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
    render();
  }
  // Funkcje liczące do testów (statystyka/tests/js/stat04_fig32.test.js).
  if (typeof window !== "undefined") window.lcStat04Fig32 = { stats: stats, diffStats: diffStats, text: textMean, textDiff: textDiff };
  function scan() { document.querySelectorAll(".p32:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
