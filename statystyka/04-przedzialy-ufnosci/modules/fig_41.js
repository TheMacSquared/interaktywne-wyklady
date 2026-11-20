// Ryc. 4.1 i 4.3 wykładu 04: budowa przedziału ufności dla proporcji i dla różnicy proporcji
// (wersja z 2026-11-20; wcześniej słupki TAK/NIE i odcinki na osi 0–1 w sf_steps.js).
// Wspólny wygląd: odpowiedzi jako kropki (TAK wypełnione, NIE puste), odsetek TAK jako pasek
// wypełniony od 0 do 100%, ± SE jako czarna klamra tylko w swoim kroku.
//   .p41[data-x]                         Ryc. 4.1: 50 odpowiedzi, p = 0.6; 95% przedział to
//       ramka wokół krawędzi wypełnienia, z granicami w procentach.
//   .p41[data-kind="diff"][data-x][data-y]  Ryc. 4.3: dwie grupy po 60 osób (p = 0.7 i 0.5);
//       dwa paski, różnica jako odstęp między krawędziami, niżej oś różnicy z zerem,
//       95% przedział jako burgundowa klamra z granicami w punktach procentowych.
// Kroki prowadzi lc_step_widget(); przycisk [data-p41="new"] losuje nowe próby.
(function () {
  "use strict";
  var NS = "http://www.w3.org/2000/svg", Z = 1.959964;
  function el(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function f(v, d) { return Number(v).toFixed(d); }
  function pct(v, d) { return f(100 * v, d === undefined ? 1 : d) + "%"; }
  function pp(v) { return f(100 * v, 1).replace("-", "−"); }
  function rbinom(n, p) { var x = 0; for (var i = 0; i < n; i++) if (Math.random() < p) x++; return x; }
  function stats(x, n) {
    n = n || 50;
    var p = x / n, se = Math.sqrt(p * (1 - p) / n);
    return { x: x, n: n, p: p, se: se, me: Z * se, lo: p - Z * se, hi: p + Z * se };
  }
  // Różnica dwóch proporcji (przedział Walda), jak diffStats() w dawnym sf_steps.js.
  function diffStats(x1, x2, n) {
    n = n || 60;
    var p1 = x1 / n, p2 = x2 / n, d = p1 - p2, se = Math.sqrt(p1 * (1 - p1) / n + p2 * (1 - p2) / n);
    return { x1: x1, x2: x2, n: n, p1: p1, p2: p2, d: d, se: se, me: Z * se, lo: d - Z * se, hi: d + Z * se };
  }
  var X = function (v) { return 50 + 540 * Math.min(1, Math.max(0, v)); };

  // Odpowiedzi jako kropki: TAK wypełnione, NIE puste.
  function waffle(g, x, n, x0, y0, cols, step, r) {
    for (var i = 0; i < n; i++) {
      el("circle", { cx: x0 + (i % cols) * step, cy: y0 + Math.floor(i / cols) * step, r: r,
        class: "p41-dot" + (i < x ? " is-yes" : "") }, g);
    }
  }
  function pctAxis(g, y) {
    el("line", { x1: 50, x2: 590, y1: y, y2: y, class: "p41-axis" }, g);
    [0, 0.25, 0.5, 0.75, 1].forEach(function (v) {
      el("line", { x1: X(v), x2: X(v), y1: y, y2: y + 7, class: "p41-axis" }, g);
      el("text", { x: X(v), y: y + 26, class: "p41-tick" }, g, pct(v, 0));
    });
  }
  // Pasek 100% wypełniony do odsetka TAK, z kreską na krawędzi wypełnienia.
  function fillBar(g, p, y, h) {
    el("rect", { x: 50, y: y, width: 540, height: h, rx: 6, class: "p41-full" }, g);
    el("rect", { x: 50, y: y, width: X(p) - 50, height: h, rx: 6, class: "p41-fill" }, g);
    el("text", { x: 62, y: y + h / 2 + 6, class: "p41-in" }, g, "TAK " + pct(p, 0));
    el("text", { x: 578, y: y + h / 2 + 6, class: "p41-in is-no" }, g, "NIE " + pct(1 - p, 0));
  }
  function seBracket(g, x1, x2, y, label) {
    el("line", { x1: x1, x2: x2, y1: y, y2: y, class: "p41-bar is-se" }, g);
    [x1, x2].forEach(function (x) { el("line", { x1: x, x2: x, y1: y - 8, y2: y + 8, class: "p41-cap is-se" }, g); });
    return label;
  }

  // ---- Ryc. 4.1: jedna proporcja -------------------------------------------------
  function drawOne(svg, s, step) {
    var yt = 150, h = 46;
    waffle(svg, s.x, s.n, 62, 30, 25, 21.4, 7.5);
    el("text", { x: 50, y: 96, class: "p41-note" }, svg, s.x + " × TAK · " + (s.n - s.x) + " × NIE");
    if (step < 2) return;
    fillBar(svg, s.p, yt, h);
    pctAxis(svg, yt + h + 8);
    if (step >= 4) {
      el("rect", { x: X(s.lo), y: yt - 10, width: X(s.hi) - X(s.lo), height: h + 20, rx: 4, class: "p41-band" }, svg);
      el("text", { x: X(s.lo), y: yt - 22, class: "p41-val is-ci" }, svg, pct(s.lo));
      el("text", { x: X(s.hi), y: yt - 22, class: "p41-val is-ci" }, svg, pct(s.hi));
      el("text", { x: X(s.p), y: yt + h + 70, class: "p41-lab is-ci" }, svg, "95% przedział ufności dla odsetka TAK");
    }
    if (step === 3) {
      seBracket(svg, X(s.p - s.se), X(s.p + s.se), yt - 14);
      el("text", { x: X(s.p), y: yt - 30, class: "p41-lab is-se" }, svg, "± SE = " + pct(s.se));
    }
    el("line", { x1: X(s.p), x2: X(s.p), y1: yt - 6, y2: yt + h + 6, class: "p41-edge" }, svg);
  }
  function textOne(s, step) {
    if (step === 1) return s.n + " odpowiedzi: " + s.x + " razy TAK, " + (s.n - s.x) + " razy NIE.";
    if (step === 2) return "<i>p̂</i> = <i>x</i>/<i>n</i> = " + s.x + "/" + s.n + " = " + f(s.p, 2) + ". To estymata odsetka w populacji.";
    if (step === 3) return "<i>SE</i> = √(<i>p̂</i>(1 − <i>p̂</i>)/<i>n</i>) = " + f(s.se, 3) + ". Jeden błąd standardowy w każdą stronę to jeszcze nie 95%.";
    return "<i>p̂</i> ± 1.96 · <i>SE</i> = " + f(s.p, 2) + " ± " + f(s.me, 3) + ", czyli 95% CI: <b>[" + f(s.lo, 3) + ", " + f(s.hi, 3) +
      "]</b>, od " + pct(s.lo) + " do " + pct(s.hi) + ".";
  }

  // ---- Ryc. 4.3: różnica proporcji -----------------------------------------------
  function drawDiff(svg, s, step) {
    var XD = function (v) { return 50 + 540 * (Math.min(0.5, Math.max(-0.5, v)) + 0.5); };
    [[s.x1, s.p1, "Grupa 1", 0], [s.x2, s.p2, "Grupa 2", 122]].forEach(function (gr) {
      var off = gr[3];
      el("text", { x: 50, y: 22 + off, class: "p41-note" }, svg, gr[2]);
      el("text", { x: 590, y: 22 + off, class: "p41-note is-end" }, svg, gr[0] + " × TAK · " + (s.n - gr[0]) + " × NIE");
      waffle(svg, gr[0], s.n, 59, 40 + off, 30, 18, 6.5);
      if (step >= 2) {
        fillBar(svg, gr[1], 74 + off, 30);
        el("line", { x1: X(gr[1]), x2: X(gr[1]), y1: 70 + off, y2: 108 + off, class: "p41-edge" }, svg);
      }
    });
    if (step < 2) return;
    pctAxis(svg, 236);
    if (step < 3) return;
    // Różnica jako odstęp między krawędziami wypełnienia, potem ta sama liczba na osi z zerem.
    var a = X(Math.min(s.p1, s.p2)), b = X(Math.max(s.p1, s.p2));
    el("path", { d: "M" + X(s.p2) + " 118 V192", class: "p41-guide" }, svg);
    el("path", { d: "M" + a + " 118 H" + b, class: "p41-span" }, svg);
    el("text", { x: b > 440 ? a - 12 : b + 12, y: 123, class: "p41-lab is-se " + (b > 440 ? "is-end" : "is-start") }, svg,
      "różnica " + pp(s.d) + " p.p.");
    el("line", { x1: 50, x2: 590, y1: 404, y2: 404, class: "p41-axis" }, svg);
    [-0.5, -0.25, 0, 0.25, 0.5].forEach(function (v) {
      el("line", { x1: XD(v), x2: XD(v), y1: 404, y2: 411, class: "p41-axis" }, svg);
      el("text", { x: XD(v), y: 430, class: "p41-tick" }, svg, pp(v).replace(".0", ""));
    });
    el("line", { x1: XD(0), x2: XD(0), y1: 316, y2: 404, class: "p41-guide" }, svg);
    el("text", { x: XD(0), y: 448, class: "p41-zero" }, svg, "brak różnicy");
    el("text", { x: 590, y: 448, class: "p41-axt" }, svg, "grupa 1 − grupa 2 (p.p.)");
    var xd = XD(s.d);
    if (step >= 5) {
      el("path", { d: "M" + XD(s.lo) + " 360 v-14 H" + XD(s.hi) + " v14", class: "p41-ci-bracket" }, svg);
      el("text", { x: XD(s.lo) - 8, y: 352, class: "p41-val is-ci is-end" }, svg, pp(s.lo));
      el("text", { x: XD(s.hi) + 8, y: 352, class: "p41-val is-ci is-start" }, svg, pp(s.hi));
      el("text", { x: 320, y: 476, class: "p41-lab is-ci" }, svg, "95% przedział ufności dla różnicy odsetków");
    }
    el("line", { x1: xd, x2: xd, y1: step >= 5 ? 346 : 334, y2: 412, class: "p41-edge" }, svg);
    el("text", { x: xd, y: 322, class: "p41-val is-p" }, svg, pp(s.d));
    if (step === 4) {
      seBracket(svg, XD(s.d - s.se), XD(s.d + s.se), 342);
      el("text", { x: XD(s.d + s.se) + 12, y: 347, class: "p41-lab is-se is-start" }, svg, "± SE = " + pp(s.se) + " p.p.");
    }
  }
  function textDiff(s, step) {
    var p1 = "<i>p̂</i><sub>1</sub>", p2 = "<i>p̂</i><sub>2</sub>";
    if (step === 1) return "Grupa 1: " + s.x1 + " TAK i " + (s.n - s.x1) + " NIE. Grupa 2: " + s.x2 + " TAK i " + (s.n - s.x2) + " NIE.";
    if (step === 2) return p1 + " = " + s.x1 + "/" + s.n + " = " + f(s.p1, 3) + "&emsp;&emsp;" + p2 + " = " + s.x2 + "/" + s.n + " = " + f(s.p2, 3);
    if (step === 3) return p1 + " − " + p2 + " = " + f(s.p1, 3) + " − " + f(s.p2, 3) + " = " + f(s.d, 3) +
      ", czyli " + pp(s.d) + " punktu procentowego. Dolna oś ma skalę różnicy: zero oznacza brak różnicy.";
    if (step === 4) return "<i>SE</i> = √(" + p1 + "(1 − " + p1 + ")/<i>n</i><sub>1</sub> + " + p2 + "(1 − " + p2 + ")/<i>n</i><sub>2</sub>) = " + f(s.se, 3) + ".";
    var verdict = s.lo > 0 ? "Przedział nie obejmuje 0: w grupie 1 odsetek TAK jest wyższy, z 95% ufnością o co najmniej " + f(s.lo, 3) + "." :
      s.hi < 0 ? "Przedział nie obejmuje 0: w grupie 2 odsetek TAK jest wyższy, z 95% ufnością o co najmniej " + f(-s.hi, 3) + "." :
      "Przedział obejmuje 0: te próby nie rozstrzygają, która grupa ma wyższy odsetek.";
    return "Margines 1.96 · " + f(s.se, 3) + " = " + f(s.me, 3) + ". 95% CI: <b>[" + f(s.lo, 3) + ", " + f(s.hi, 3) + "]</b>. " + verdict;
  }

  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    if (!stepper) return;
    root.dataset.ready = "1";
    var diff = root.dataset.kind === "diff";
    var s = diff ? diffStats(Number(root.dataset.x) || rbinom(60, 0.7), Number(root.dataset.y) || rbinom(60, 0.5))
                 : stats(Number(root.dataset.x) || rbinom(50, 0.6));
    var out = document.getElementById(stepper.id + "_text"), svgBox = root.querySelector(".p41-svg");
    function render() {
      var step = Number(stepper.getAttribute("data-lc-step")) || 1, last = step >= (diff ? 5 : 4);
      svgBox.textContent = "";
      var svg = el("svg", { viewBox: "0 0 640 " + (diff ? 490 : 300), role: "img",
        "aria-label": diff
          ? "Budowa przedziału ufności dla różnicy proporcji, krok " + step + (step >= 3 ? ": różnica " + pp(s.d) + " punktu procentowego" : "") +
            (last ? ", 95% przedział ufności od " + pp(s.lo) + " do " + pp(s.hi) : "")
          : "Budowa przedziału ufności dla proporcji, krok " + step + ": odsetek TAK " + pct(s.p, 0) +
            (last ? ", 95% przedział ufności od " + pct(s.lo) + " do " + pct(s.hi) : "") }, svgBox);
      if (diff) drawDiff(svg, s, step); else drawOne(svg, s, step);
      if (out) out.innerHTML = diff ? textDiff(s, step) : textOne(s, step);
    }
    stepper.addEventListener("click", function (e) {
      if (!e.target.closest('[data-p41="new"]')) return;
      s = diff ? diffStats(rbinom(60, 0.7), rbinom(60, 0.5)) : stats(rbinom(50, 0.6));
      render();
    });
    new MutationObserver(render).observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
    render();
  }
  // Funkcje liczące do testów (statystyka/tests/js/stat04_fig41.test.js).
  if (typeof window !== "undefined") window.lcStat04Fig41 = { stats: stats, diffStats: diffStats, text: textOne, textDiff: textDiff };
  function scan() { document.querySelectorAll(".p41:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
