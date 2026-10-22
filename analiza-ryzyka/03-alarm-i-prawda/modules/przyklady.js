// Przykład „Ten sam wzór w trzech firmach” (rozdział „Próg reakcji”).
//
// Kontener .lc-ex[data-config]: cfg.examples to przykłady {key, name, text, q, qstar,
// qText, cText, LText, qstarText, qstarEq, cmp, react, verdict, read}; cfg.summary
// to slajd zbiorczy {name, title, text}; cfg.axisMax to koniec osi posterioru.
// Slajdy 0..n-1 pokazują jeden przykład (kafelki q, c, L, wzór q* = c/L, oś z progiem
// i werdykt), slajd n zestawia trzy progi na jednej osi.
// Przyciski [data-ex-go="i"] (segment) i [data-ex="prev|next"].
//
// Działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var X0 = 50, X1 = 770;

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
  function span(cls, text) { return el("span", cls, text); }

  var uid = 0;

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var ex = cfg.examples || [], sum = cfg.summary || {}, max = cfg.axisMax || 0.5;
    var n = ex.length, last = n;
    var id = "lc-ex-" + (++uid);
    function X(v) { return X0 + (X1 - X0) * Math.min(Math.max(v / max, 0), 1); }

    function out(k) { return root.querySelector('[data-ex-out="' + k + '"]'); }
    var stage = root.querySelector(".lc-ex-stage");
    var prevBtn = root.querySelector('[data-ex="prev"]'), nextBtn = root.querySelector('[data-ex="next"]');
    var segBtns = root.querySelectorAll("[data-ex-go]");
    var current = -1;

    function ticks(g, y) {
      for (var t = 0; t <= max + 1e-9; t += 0.1) {
        var x = X(t);
        svg("line", { class: "lc-ex-tick", x1: x, x2: x, y1: y, y2: y + 7 }, g);
        txt({ class: "lc-ex-tick-label", x: x, y: y + 25 }, t.toFixed(1), g);
      }
    }
    function anchorFor(x) { return x < X0 + 80 ? "start" : (x > X1 - 80 ? "end" : "middle"); }
    function dxFor(a) { return a === "start" ? -8 : (a === "end" ? 8 : 0); }

    // Jeden przykład: strefa „reaguj” od progu w prawo, kreska q*, kropka q.
    var one = svg("svg", { class: "lc-ex-svg lc-ex-one", viewBox: "0 0 820 150", "aria-hidden": "true" }, stage);
    var clip = svg("clipPath", { id: id + "-clip" }, svg("defs", {}, one));
    svg("rect", { x: X0, y: 0, width: X1 - X0, height: 150 }, clip);
    var zoneWrap = svg("g", { "clip-path": "url(#" + id + "-clip)" }, one);
    var zone = svg("g", { class: "lc-ex-move" }, zoneWrap);
    svg("rect", { class: "lc-ex-zone", x: 0, y: 30, width: X1 - X0, height: 55 }, zone);
    txt({ class: "lc-ex-zone-label", x: X1 - 8, y: 50, "text-anchor": "end" }, "reaguj", one);
    svg("line", { class: "lc-ex-axis", x1: X0, x2: X1, y1: 85, y2: 85 }, one);
    ticks(one, 85);
    var thr = svg("g", { class: "lc-ex-move" }, one);
    svg("line", { class: "lc-ex-thr", x1: 0, x2: 0, y1: 26, y2: 96 }, thr);
    var thrLabel = txt({ class: "lc-ex-thr-label", x: 0, y: 140 }, "", thr);
    var dot = svg("g", { class: "lc-ex-move" }, one);
    svg("circle", { class: "lc-ex-dot", cx: 0, cy: 85, r: 11 }, dot);
    var dotLabel = txt({ class: "lc-ex-dot-label", x: 0, y: 20 }, "", dot);

    // Zestawienie: trzy wiersze na wspólnej osi.
    var rowY = function (i) { return 64 + i * 96; };
    var tickY = rowY(n - 1) + 44;
    var all = svg("svg", { class: "lc-ex-svg lc-ex-all", viewBox: "0 0 820 " + (tickY + 34), "aria-hidden": "true" }, stage);
    ex.forEach(function (E, i) {
      var y = rowY(i), xs = X(E.qstar), xq = X(E.q);
      svg("rect", { class: "lc-ex-zone", x: xs, y: y - 20, width: X1 - xs, height: 20 }, all);
      svg("line", { class: "lc-ex-axis", x1: X0, x2: X1, y1: y, y2: y, "stroke-width": 1.5 }, all);
      var name = txt({ class: "lc-ex-row-name", x: X0, y: y - 32 }, E.name + "  ", all);
      var v = svg("tspan", { class: "lc-ex-row-verdict " + (E.react ? "is-react" : "is-verify") }, name);
      v.textContent = E.react ? "→ reagować" : "→ najpierw zweryfikować";
      svg("line", { class: "lc-ex-thr", x1: xs, x2: xs, y1: y - 24, y2: y + 8, "stroke-width": 2.5 }, all);
      var a = anchorFor(xs);
      txt({ class: "lc-ex-thr-label", x: xs, dx: dxFor(a), y: y + 27, "text-anchor": a }, "q* = " + E.qstarText, all);
      svg("circle", { class: "lc-ex-dot", cx: xq, cy: y, r: 8 }, all);
      txt({ class: "lc-ex-dot-label", x: xq + 13, y: y - 6, "text-anchor": "start" }, "q = " + E.qText, all);
    });
    svg("line", { class: "lc-ex-axis", x1: X0, x2: X1, y1: tickY, y2: tickY, "stroke-width": 1, opacity: 0.5 }, all);
    ticks(all, tickY);

    function fill(node, parts) {
      node.innerHTML = "";
      parts.forEach(function (p) { node.appendChild(p); });
    }

    function show(i) {
      var summary = i === last;
      root.dataset.mode = summary ? "summary" : "one";
      segBtns.forEach(function (b) { b.setAttribute("aria-pressed", String(+b.dataset.exGo === i)); });
      if (summary) {
        out("name").textContent = sum.title || sum.name;
        out("step").textContent = "Zestawienie";
        out("text").textContent = sum.text || "";
      } else {
        var E = ex[i], xs = X(E.qstar), xq = X(E.q);
        out("name").textContent = E.name;
        out("step").textContent = "Przykład " + (i + 1) + " z " + n;
        out("text").textContent = E.text;
        out("q").textContent = E.qText;
        out("c").textContent = E.cText;
        out("L").textContent = E.LText;
        fill(out("formula"), [
          span("lc-ex-is-qs", "q*"), span("lc-ex-op", "="),
          span("lc-ex-is-c", "c"), span("lc-ex-op", "/"), span("lc-ex-is-L", "L"), span("lc-ex-op", "="),
          span("lc-ex-is-c", E.cText), span("lc-ex-op", "/"), span("lc-ex-is-L", E.LText),
          span("lc-ex-op", E.qstarEq), span("lc-ex-is-qs", E.qstarText)
        ]);
        fill(out("cmp"), [
          span("lc-ex-is-q", "q = " + E.qText), span("lc-ex-op", " " + E.cmp + " "), span("lc-ex-is-qs", "q* = " + E.qstarText)
        ]);
        var v = out("verdict");
        v.textContent = E.verdict;
        v.className = "lc-ex-verdict " + (E.react ? "is-react" : "is-verify");
        out("read").textContent = E.read;
        zone.style.transform = "translate(" + xs + "px,0)";
        thr.style.transform = "translate(" + xs + "px,0)";
        dot.style.transform = "translate(" + xq + "px,0)";
        var a = anchorFor(xs), b = anchorFor(xq);
        thrLabel.setAttribute("text-anchor", a); thrLabel.setAttribute("dx", dxFor(a));
        thrLabel.textContent = "q* = " + E.qstarText;
        dotLabel.setAttribute("text-anchor", b); dotLabel.setAttribute("dx", b === "start" ? -10 : (b === "end" ? 10 : 0));
        dotLabel.textContent = "q = " + E.qText;
      }
      prevBtn.disabled = i === 0;
      nextBtn.disabled = i === last;
      current = i;
    }

    prevBtn.addEventListener("click", function () { if (current > 0) show(current - 1); });
    nextBtn.addEventListener("click", function () { if (current < last) show(current + 1); });
    segBtns.forEach(function (b) { b.addEventListener("click", function () { show(+b.dataset.exGo); }); });
    show(0);
  }

  function scan() { document.querySelectorAll(".lc-ex:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
