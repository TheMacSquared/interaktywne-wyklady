// Przykład „Ten sam start, różne sygnały” (rozdział „Wzór Bayesa”, sekcja
// „Pułapka częstości bazowej”, wzór 3.7).
//
// Kontener .lc-sg[data-config]: cfg.signals to sygnały {key, name, sym, text, lr,
// logLr, postOdds, p, up, lrName, lrWords, lrNote, lrCalc, lrText, postText, pText,
// postCalc, pCalc, arrowText}; cfg.prior to szanse a priori {odds, text,
// frac, calc, note}; cfg.summary to slajd zbiorczy {name, title, text}; cfg.logMin i
// cfg.logMax to zakres osi szans w log10.
// Slajdy 0..n-1 pokazują jeden sygnał jako łańcuch kroków 1–4 (szanse przed × LR
// = szanse po → prawdopodobieństwo) i oś logarytmiczną ze strzałką o długości
// log LR; slajd n zestawia wszystkie strzałki na jednej osi.
// Przyciski [data-sg-go="i"] (segment) i [data-sg="prev|next"].
//
// Działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var X0 = 60, X1 = 780;

  function el(tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined) e.textContent = text;
    return e;
  }
  function svg(tag, attrs, parent) {
    var e = document.createElementNS(NS, tag);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (parent) parent.appendChild(e);
    return e;
  }
  function txt(attrs, text, parent) { var t = svg("text", attrs, parent); t.textContent = text; return t; }
  function reduced() {
    return window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  var TICKS = ["1 : 10 000", "1 : 1000", "1 : 100", "1 : 10", "1 : 1"];

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var sg = cfg.signals || [], sum = cfg.summary || {}, prior = cfg.prior || {};
    var lo = cfg.logMin === undefined ? -4 : cfg.logMin, hi = cfg.logMax === undefined ? 0 : cfg.logMax;
    var n = sg.length, last = n;
    function X(lg) { return X0 + (X1 - X0) * Math.min(Math.max((lg - lo) / (hi - lo), 0), 1); }
    var lgStart = Math.log(prior.odds) / Math.LN10, xs = X(lgStart);

    function out(k) { return root.querySelector('[data-sg-out="' + k + '"]'); }
    var stage = root.querySelector(".lc-sg-stage");
    var prevBtn = root.querySelector('[data-sg="prev"]'), nextBtn = root.querySelector('[data-sg="next"]');
    var segBtns = root.querySelectorAll("[data-sg-go]");
    var current = -1;

    function ticks(g, y) {
      for (var k = lo; k <= hi; k++) {
        var x = X(k);
        svg("line", { class: "lc-sg-tick", x1: x, x2: x, y1: y - 5, y2: y + 7 }, g);
        txt({ class: "lc-sg-tick-label", x: x, y: y + 25 }, TICKS[k - lo] || ("10^" + k), g);
      }
    }
    function head(parent, x, y, dir, size) {
      var g = svg("g", { transform: "translate(" + x + "," + y + ") scale(" + dir + ",1)" }, parent);
      svg("polygon", { class: "lc-sg-head", points: "0,0 " + (-size) + "," + (-size * 0.6) + " " + (-size) + "," + (size * 0.6) }, g);
      return g;
    }

    // Jeden sygnał: oś szans, start (pusty krąg), strzałka × LR, wynik (kropka).
    var AY = 120, RY = 70;
    var one = svg("svg", { class: "lc-sg-svg lc-sg-one", viewBox: "0 0 820 185", "aria-hidden": "true" }, stage);
    txt({ class: "lc-sg-axis-name", x: X0, y: 16 }, "szanse awarii (skala logarytmiczna)", one);
    svg("line", { class: "lc-sg-axis", x1: X0, x2: X1, y1: AY, y2: AY }, one);
    ticks(one, AY);
    svg("line", { class: "lc-sg-drop", x1: xs, x2: xs, y1: RY, y2: AY }, one);
    var drop = svg("g", { class: "lc-sg-move" }, one);
    svg("line", { class: "lc-sg-drop", x1: 0, x2: 0, y1: RY, y2: AY }, drop);
    var arrow = svg("line", { class: "lc-sg-arrow lc-sg-draw", x1: xs, x2: xs, y1: RY, y2: RY }, one);
    var headG = svg("g", { class: "lc-sg-move" }, one);
    var headIn = head(headG, 0, RY, 1, 16);
    var arrowLabel = txt({ class: "lc-sg-arrow-label", x: 0, y: RY - 14 }, "", one);
    svg("circle", { class: "lc-sg-start", cx: xs, cy: AY, r: 9 }, one);
    txt({ class: "lc-sg-start-label", x: xs, y: AY + 55, "text-anchor": "middle" }, prior.text || "", one);
    var dot = svg("g", { class: "lc-sg-move" }, one);
    svg("circle", { class: "lc-sg-dot", cx: 0, cy: AY, r: 11 }, dot);
    var dotLabel = txt({ class: "lc-sg-dot-label", x: 0, y: AY + 55, "text-anchor": "middle" }, "", dot);

    // Zestawienie: wszystkie sygnały z tego samego startu.
    var rowY = function (i) { return 78 + i * 78; };
    var tickY = rowY(n - 1) + 46;
    var all = svg("svg", { class: "lc-sg-svg lc-sg-all", viewBox: "0 0 820 " + (tickY + 34), "aria-hidden": "true" }, stage);
    svg("line", { class: "lc-sg-start-line", x1: xs, x2: xs, y1: 30, y2: tickY }, all);
    txt({ class: "lc-sg-start-label", x: xs, y: 22, "text-anchor": "middle" }, "start " + (prior.text || ""), all);
    sg.forEach(function (S, i) {
      var y = rowY(i), xr = X(S.logLr + lgStart), dir = S.up ? 1 : -1;
      svg("line", { class: "lc-sg-axis", x1: X0, x2: X1, y1: y, y2: y, "stroke-width": 1, opacity: 0.35 }, all);
      txt({ class: "lc-sg-row-name", x: X0, y: y - 30 }, S.name, all);
      svg("line", { class: "lc-sg-arrow", x1: xs, x2: xr - dir * 12, y1: y, y2: y, "stroke-width": 3.5 }, all);
      head(all, xr - dir * 9, y, dir, 13);
      txt({ class: "lc-sg-arrow-label", x: (xs + xr) / 2, y: y - 12 }, S.arrowText, all);
      svg("circle", { class: "lc-sg-start", cx: xs, cy: y, r: 7, "stroke-width": 2.5 }, all);
      svg("circle", { class: "lc-sg-dot", cx: xr, cy: y, r: 8 }, all);
      txt({ class: "lc-sg-dot-label", x: xr + dir * 14, y: y + 5, "text-anchor": S.up ? "start" : "end" }, "p ≈ " + S.pText, all);
    });
    svg("line", { class: "lc-sg-axis", x1: X0, x2: X1, y1: tickY, y2: tickY }, all);
    ticks(all, tickY);

    function drawArrow(xr, dir) {
      var x2 = xr - dir * 14, len = Math.abs(x2 - xs);
      arrow.setAttribute("x2", x2);
      arrow.style.strokeDasharray = len;
      if (reduced() || root.dataset.anim === "0") { arrow.style.strokeDashoffset = 0; return; }
      arrow.style.transition = "none";
      arrow.style.strokeDashoffset = len;
      arrow.getBoundingClientRect();
      arrow.style.transition = "";
      arrow.style.strokeDashoffset = 0;
    }

    function show(i) {
      var summary = i === last;
      root.dataset.mode = summary ? "summary" : "one";
      segBtns.forEach(function (b) { b.setAttribute("aria-pressed", String(+b.dataset.sgGo === i)); });
      if (summary) {
        out("name").textContent = sum.title || sum.name;
        out("step").textContent = "Zestawienie";
        out("text").textContent = sum.text || "";
      } else {
        var S = sg[i], xr = X(S.logLr + lgStart), dir = S.up ? 1 : -1;
        out("name").textContent = S.name;
        out("step").textContent = "Sygnał " + (i + 1) + " z " + n;
        out("text").textContent = S.text;
        out("prior").textContent = prior.text;
        out("priorSub").textContent = prior.calc;
        out("priorNote").textContent = prior.note;
        out("lrLabel").textContent = S.lrName;
        out("lr").textContent = S.lrText;
        out("lrSub2").textContent = S.lrWords;
        out("lrSub").textContent = "= " + S.lrCalc;
        out("lrNote").textContent = S.lrNote;
        out("postLabel").textContent = "szanse po " + S.sym;
        out("post").textContent = S.postText;
        out("postSub").textContent = S.postCalc;
        out("postNote").textContent = "szanse przed × LR";
        out("pLabel").textContent = "P(A | " + S.sym + ")";
        out("p").textContent = "≈ " + S.pText;
        out("pSub").textContent = S.pCalc;
        out("pNote").textContent = "z szans a : b prawdopodobieństwo to a / (a + b)";
        drawArrow(xr, dir);
        headG.style.transform = "translate(" + (xr - dir * 9) + "px,0)";
        headIn.setAttribute("transform", "translate(0," + RY + ") scale(" + dir + ",1)");
        drop.style.transform = "translate(" + xr + "px,0)";
        dot.style.transform = "translate(" + xr + "px,0)";
        arrowLabel.setAttribute("x", (xs + xr) / 2);
        arrowLabel.textContent = S.arrowText;
        dotLabel.textContent = S.postText;
      }
      prevBtn.disabled = i === 0;
      nextBtn.disabled = i === last;
      current = i;
    }

    prevBtn.addEventListener("click", function () { if (current > 0) show(current - 1); });
    nextBtn.addEventListener("click", function () { if (current < last) show(current + 1); });
    segBtns.forEach(function (b) { b.addEventListener("click", function () { show(+b.dataset.sgGo); }); });
    root.addEventListener("keydown", function (e) {
      if (e.target && /INPUT|TEXTAREA|SELECT/.test(e.target.tagName)) return;
      if (e.key === "ArrowLeft" && current > 0) { show(current - 1); e.preventDefault(); }
      if (e.key === "ArrowRight" && current < last) { show(current + 1); e.preventDefault(); }
    });
    root.dataset.anim = "0";
    show(0);
    requestAnimationFrame(function () { requestAnimationFrame(function () { root.dataset.anim = "1"; }); });
  }

  function scan() { document.querySelectorAll(".lc-sg:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
