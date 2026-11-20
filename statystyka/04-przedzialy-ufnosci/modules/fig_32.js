// Ryc. 3.2 „Konstruowanie przedziału” dla średniej (wersja z 2026-11-20, dawny prototyp 3.2-A3).
// 25 pomiarów wzrostu z N(170, 10) jako stos kropek na osi; średnia to gruba kreska; ± SE to
// czarna klamra (tylko krok 3); 95% przedział to burgundowa klamra nad kropkami z granicami
// po bokach (krok 4). Nigdy nie ma dwóch odcinków naraz. Poprzednia wersja (ci_construction.js)
// kładła średnią, ± SE i przedział jeden pod drugim; tamten skrypt rysuje nadal Ryc. 3.3.
// Kontener: .p32[data-x] (pierwsza próba z R) wewnątrz lc_step_widget(); przycisk [data-p32="new"].
(function () {
  "use strict";
  var NS = "http://www.w3.org/2000/svg", T24 = 2.063899, MU = 170, W = 640, LO = 145, HI = 195;
  function el(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function f(v, d) { return Number(v).toFixed(d); }
  function stats(x) {
    var n = x.length, m = x.reduce(function (a, b) { return a + b; }, 0) / n;
    var s = Math.sqrt(x.reduce(function (a, b) { return a + (b - m) * (b - m); }, 0) / (n - 1)), se = s / Math.sqrt(n);
    return { x: x, n: n, m: m, s: s, se: se, me: T24 * se, lo: m - T24 * se, hi: m + T24 * se };
  }
  function draw25() {
    var r = window.lcDist && window.lcDist.r;
    return r ? r.many(25, function () { return r.norm(MU, 10); }) : null;
  }
  var X = function (v) { return 50 + (Math.min(HI, Math.max(LO, v)) - LO) / (HI - LO) * 540; };

  // Kropki ułożone w stos nad osią: każda w najniższym wolnym rzędzie.
  function dots(g, xs, base, r, cls) {
    var rows = [];
    xs.slice().sort(function (a, b) { return a - b; }).forEach(function (v) {
      var px = X(v), k = 0;
      while (rows[k] !== undefined && px - rows[k] < 2 * r + 1) k++;
      rows[k] = px;
      el("circle", { cx: px, cy: base - r - k * (2 * r + 1), r: r, class: cls || "p32-dot" }, g);
    });
  }
  function axis(g, y, small) {
    el("line", { x1: X(LO), x2: X(HI), y1: y, y2: y, class: "p32-axis" }, g);
    for (var v = 150; v <= 190; v += 10) {
      el("line", { x1: X(v), x2: X(v), y1: y, y2: y + 6, class: "p32-axis" }, g);
      el("text", { x: X(v), y: y + (small ? 20 : 24), class: "p32-tick" + (small ? " is-small" : "") }, g, String(v));
    }
  }
  function bracket(g, x1, x2, y, cap) {
    el("line", { x1: x1, x2: x2, y1: y, y2: y, class: "p32-se" }, g);
    [x1, x2].forEach(function (x) { el("line", { x1: x, x2: x, y1: y - cap, y2: y + cap, class: "p32-se" }, g); });
  }

  function draw(g, s, step) {
    var xm = X(s.m), x1 = X(s.lo), x2 = X(s.hi);
    dots(g, s.x, 190, 6);
    axis(g, 196);
    el("text", { x: X(HI), y: 246, class: "p32-axt" }, g, "wzrost (cm)");
    if (step < 2) return;
    if (step >= 4) {
      el("path", { d: "M" + x1 + " 86 v-14 H" + x2 + " v14", class: "p32-ci-bracket" }, g);
      el("text", { x: x1 - 8, y: 78, class: "p32-val is-ci is-end" }, g, f(s.lo, 2));
      el("text", { x: x2 + 8, y: 78, class: "p32-val is-ci is-start" }, g, f(s.hi, 2));
      el("text", { x: xm, y: 280, class: "p32-lab is-ci" }, g, "95% przedział ufności dla średniej");
    }
    el("line", { x1: xm, x2: xm, y1: step >= 4 ? 72 : 56, y2: 204, class: "p32-mean" }, g);
    el("text", { x: xm, y: 36, class: "p32-val" }, g, "x̄ = " + f(s.m, 2) + " cm");
    if (step === 3) {
      bracket(g, X(s.m - s.se), X(s.m + s.se), 66, 8);
      el("text", { x: X(s.m + s.se) + 12, y: 71, class: "p32-lab is-start" }, g, "± SE = " + f(s.se, 2) + " cm");
    }
  }

  function text(s, step) {
    if (step === 1) return s.n + " pomiarów wzrostu, każda kropka to jedna osoba. <i>x̄</i> = " + f(s.m, 2) + " cm, <i>s</i> = " + f(s.s, 2) + " cm.";
    if (step === 2) return "Średnia z próby <i>x̄</i> = " + f(s.m, 2) + " cm to estymata punktowa μ. Inna próba dałaby inną wartość.";
    if (step === 3) return "<i>SE</i> = <i>s</i>/√<i>n</i> = " + f(s.s, 2) + "/√" + s.n + " = " + f(s.se, 2) + " cm. Jeden błąd standardowy w każdą stronę to tylko około 67% ufności.";
    return "<i>t</i>* = 2.064 (df = 24), margines 2.064 · " + f(s.se, 2) + " = " + f(s.me, 2) + " cm. 95% CI: <b>[" + f(s.lo, 2) + ", " + f(s.hi, 2) +
      "]</b> cm. " + (s.lo <= MU && MU <= s.hi ? "Ten przedział zawiera μ = 170 cm." : "Ten przedział nie zawiera μ = 170 cm.");
  }

  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    if (!stepper) return;
    root.dataset.ready = "1";
    var s = stats(JSON.parse(root.dataset.x)), H = 296;
    var out = document.getElementById(stepper.id + "_text"), box = root.querySelector(".p32-svg");
    function render() {
      var step = Number(stepper.getAttribute("data-lc-step")) || 1;
      box.textContent = "";
      var svg = el("svg", { viewBox: "0 0 " + W + " " + H, role: "img",
        "aria-label": "Budowa przedziału ufności dla średniej, krok " + step + ": średnia " + f(s.m, 2) + " cm" +
          (step >= 4 ? ", 95% przedział ufności od " + f(s.lo, 2) + " do " + f(s.hi, 2) + " cm" : "") }, box);
      draw(svg, s, step);
      if (out) out.innerHTML = text(s, step);
    }
    stepper.addEventListener("click", function (e) {
      if (e.target.closest('[data-p32="new"]')) { var x = draw25(); if (x) { s = stats(x); render(); } }
    });
    new MutationObserver(render).observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
    render();
  }
  if (typeof window !== "undefined") window.lcStat04Fig32 = { stats: stats, text: text };
  function scan() { document.querySelectorAll(".p32:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
