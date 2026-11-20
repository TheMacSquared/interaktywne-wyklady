// Ryc. 4.1 „Konstruowanie przedziału” dla proporcji (wersja z 2026-11-20, dawny prototyp 4.1-C).
// Próba 50 odpowiedzi TAK/NIE (p = 0.6) jako kropki; odsetek TAK jako pasek wypełniony od 0 do
// 100%; ± SE jako klamra nad krawędzią wypełnienia; 95% przedział jako ramka wokół tej krawędzi,
// z granicami w procentach. Cztery kroki prowadzi lc_step_widget(); przycisk [data-p41="new"]
// losuje nową próbę. Kontener: .p41[data-x] (pierwsza próba) wewnątrz widgetu krokowego.
// Poprzednia wersja (słupki TAK/NIE i oś 0–1 z trzema znakami na różnych wysokościach) rysowała
// przedział jako nakładające się odcinki; ramka na pasku czyta się wyraźniej.
(function () {
  "use strict";
  var NS = "http://www.w3.org/2000/svg", N = 50, P0 = 0.6, Z = 1.959964;
  function el(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function f(v, d) { return Number(v).toFixed(d); }
  function pct(v, d) { return f(100 * v, d === undefined ? 1 : d) + "%"; }
  function draw50() { var x = 0; for (var i = 0; i < N; i++) if (Math.random() < P0) x++; return x; }
  function stats(x) { var p = x / N, se = Math.sqrt(p * (1 - p) / N); return { x: x, p: p, se: se, me: Z * se, lo: p - Z * se, hi: p + Z * se }; }

  // 50 odpowiedzi jako kropki: TAK wypełnione, NIE puste.
  function waffle(g, s, x0, y0, cols, step, r) {
    for (var i = 0; i < N; i++) {
      el("circle", { cx: x0 + (i % cols) * step, cy: y0 + Math.floor(i / cols) * step, r: r,
        class: "p41-dot" + (i < s.x ? " is-yes" : "") }, g);
    }
  }
  function axis(g, X, y, lo, hi, by, asPct) {
    el("line", { x1: X(lo), x2: X(hi), y1: y, y2: y, class: "p41-axis" }, g);
    for (var v = lo; v <= hi + 1e-9; v += by) {
      el("line", { x1: X(v), x2: X(v), y1: y, y2: y + 7, class: "p41-axis" }, g);
      el("text", { x: X(v), y: y + 26, class: "p41-tick" }, g, asPct ? pct(v, 0) : f(v, 1));
    }
  }
  function bar(g, x1, x2, y, cls, cap) {
    el("line", { x1: x1, x2: x2, y1: y, y2: y, class: "p41-bar " + cls }, g);
    [x1, x2].forEach(function (x) { el("line", { x1: x, x2: x, y1: y - cap, y2: y + cap, class: "p41-cap " + cls }, g); });
  }

  function barChart(svg, s, step) {
    var x0 = 50, x1 = 600, X = function (v) { return x0 + (x1 - x0) * v; }, yt = 150, h = 46;
    waffle(svg, s, 62, 30, 25, 21.4, 7.5);
    el("text", { x: x0, y: 96, class: "p41-note" }, svg, s.x + " × TAK · " + (N - s.x) + " × NIE");
    if (step < 2) return;
    el("rect", { x: x0, y: yt, width: x1 - x0, height: h, rx: 6, class: "p41-full" }, svg);
    el("rect", { x: x0, y: yt, width: X(s.p) - x0, height: h, rx: 6, class: "p41-fill" }, svg);
    el("text", { x: x0 + 12, y: yt + 30, class: "p41-in" }, svg, "TAK " + pct(s.p, 0));
    el("text", { x: x1 - 12, y: yt + 30, class: "p41-in is-no" }, svg, "NIE " + pct(1 - s.p, 0));
    axis(svg, X, yt + h + 8, 0, 1, 0.25, true);
    if (step >= 4) {
      el("rect", { x: X(s.lo), y: yt - 10, width: X(s.hi) - X(s.lo), height: h + 20, rx: 4, class: "p41-band" }, svg);
      el("text", { x: X(s.lo), y: yt - 22, class: "p41-val is-ci" }, svg, pct(s.lo));
      el("text", { x: X(s.hi), y: yt - 22, class: "p41-val is-ci" }, svg, pct(s.hi));
      el("text", { x: X(s.p), y: yt + h + 70, class: "p41-lab is-ci" }, svg, "95% przedział ufności dla odsetka TAK");
    }
    if (step === 3) {
      bar(svg, X(s.p - s.se), X(s.p + s.se), yt - 14, "is-se", 8);
      el("text", { x: X(s.p), y: yt - 30, class: "p41-lab is-se" }, svg, "± SE = " + pct(s.se));
    }
    el("line", { x1: X(s.p), x2: X(s.p), y1: yt - 6, y2: yt + h + 6, class: "p41-edge" }, svg);
  }

  function text(s, step) {
    if (step === 1) return N + " odpowiedzi: " + s.x + " razy TAK, " + (N - s.x) + " razy NIE.";
    if (step === 2) return "<i>p̂</i> = <i>x</i>/<i>n</i> = " + s.x + "/" + N + " = " + f(s.p, 2) + ". To estymata odsetka w populacji.";
    if (step === 3) return "<i>SE</i> = √(<i>p̂</i>(1 − <i>p̂</i>)/<i>n</i>) = " + f(s.se, 3) + ". Jeden błąd standardowy w każdą stronę to jeszcze nie 95%.";
    return "<i>p̂</i> ± 1.96 · <i>SE</i> = " + f(s.p, 2) + " ± " + f(s.me, 3) + ", czyli 95% CI: <b>[" + f(s.lo, 3) + ", " + f(s.hi, 3) +
      "]</b>, od " + pct(s.lo) + " do " + pct(s.hi) + ".";
  }

  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    if (!stepper) return;
    root.dataset.ready = "1";
    var s = stats(Number(root.dataset.x) || draw50());
    var out = document.getElementById(stepper.id + "_text"), svgBox = root.querySelector(".p41-svg");
    function render() {
      var step = Number(stepper.getAttribute("data-lc-step")) || 1;
      svgBox.textContent = "";
      var svg = el("svg", { viewBox: "0 0 640 300", role: "img",
        "aria-label": "Budowa przedziału ufności dla proporcji, krok " + step + ": odsetek TAK " + pct(s.p, 0) +
          (step >= 4 ? ", 95% przedział ufności od " + pct(s.lo) + " do " + pct(s.hi) : "") }, svgBox);
      barChart(svg, s, step);
      if (out) out.innerHTML = text(s, step);
    }
    stepper.addEventListener("click", function (e) {
      if (e.target.closest('[data-p41="new"]')) { s = stats(draw50()); render(); }
    });
    new MutationObserver(render).observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
    render();
  }
  // Funkcje liczące do testów (statystyka/tests/js/stat04_fig41.test.js).
  if (typeof window !== "undefined") window.lcStat04Fig41 = { stats: stats, text: text };
  function scan() { document.querySelectorAll(".p41:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
