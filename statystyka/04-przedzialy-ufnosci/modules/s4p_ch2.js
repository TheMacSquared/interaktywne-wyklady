// PROTOTYPY PUNKTÓW STOP — rozdział 2, prototyp 2a „Przeciągnij x̄” (2026-10-30)
// Vanilla JS + SVG, bez serwera. Kontener (R: s4p_2a_ui):
//   .s4p-flip[data-mu][data-se][data-x][data-min][data-max]
//     .s4p-flip-stage  — tu wstawiamy SVG
//     .s4p-flip-read   — odczyt pod rysunkiem
// Przeciąganie: pointer events na całym rysunku (mysz, palec).
// Klawiatura: uchwyt x̄ to <g role="slider" tabindex="0">; strzałki ±0.1 cm,
// Shift+strzałka ±1 cm, Home/End na krańce zakresu.
// Brak animacji, więc prefers-reduced-motion nie wymaga osobnej obsługi.
(function () {
  "use strict";

  var NS = "http://www.w3.org/2000/svg";
  var VB_W = 640, VB_H = 300;
  var X0 = 40, X1 = 600;   // oś 160–180 cm w jednostkach viewBox
  var LO = 160, HI = 180;
  var BASE = 170;          // y linii osi
  var PEAK = 110;          // wysokość piku gęstości (jednostki viewBox)
  var IVY = 232;           // y przedziału x̄ ± 1.96·SE
  var STEP = 0.1;

  function el(name, attrs, parent) {
    var node = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { node.setAttribute(k, attrs[k]); });
    if (parent) parent.appendChild(node);
    return node;
  }

  function setStroke(node, value) { node.style.setProperty("stroke", value); }
  function setFill(node, value) { node.style.setProperty("fill", value); }

  function init(root) {
    if (root.dataset.ready) return;
    var stage = root.querySelector(".s4p-flip-stage");
    var readEl = root.querySelector(".s4p-flip-read");
    if (!stage) return;
    root.dataset.ready = "1";

    var MU = parseFloat(root.dataset.mu || "170");
    var SE = parseFloat(root.dataset.se || "2");
    var ME = 1.96 * SE;
    var XMIN = parseFloat(root.dataset.min || "163");
    var XMAX = parseFloat(root.dataset.max || "177");
    var x;

    function sx(v) { return X0 + (v - LO) * (X1 - X0) / (HI - LO); }
    function dens(v) { var z = (v - MU) / SE; return Math.exp(-z * z / 2); }
    function clamp(v) { return Math.min(XMAX, Math.max(XMIN, v)); }
    function snap(v) { return Math.round(clamp(v) * 10) / 10; }

    // --- SVG: elementy statyczne (krzywa, pas, oś) ---------------------------
    var svg = el("svg", {
      viewBox: "0 0 " + VB_W + " " + VB_H,
      role: "group",
      "aria-label": "Rozkład średniej z próby wokół μ z pasem μ ± 1.96·SE. Przeciągnij romb x̄: " +
        "przedział x̄ ± 1.96·SE pod nim obejmuje μ wtedy i tylko wtedy, gdy x̄ leży w pasie."
    });
    stage.appendChild(svg);

    var curve = [];
    for (var v = LO; v <= HI + 1e-9; v += 0.1) {
      curve.push((curve.length ? "L" : "M") + sx(v).toFixed(2) + "," + (BASE - PEAK * dens(v)).toFixed(2));
    }

    var bandPts = [sx(MU - ME).toFixed(2) + "," + BASE];
    for (var b = MU - ME; b <= MU + ME + 1e-9; b += 0.05) {
      bandPts.push(sx(b).toFixed(2) + "," + (BASE - PEAK * dens(b)).toFixed(2));
    }
    bandPts.push(sx(MU + ME).toFixed(2) + "," + BASE);
    setFill(el("polygon", { points: bandPts.join(" "), "fill-opacity": "0.35" }, svg), "var(--upwr-cat-niebo)");

    setStroke(el("path", { d: curve.join(" "), fill: "none", "stroke-width": "1.8" }, svg), "var(--upwr-ink-soft)");

    setStroke(el("line", { x1: X0, x2: X1, y1: BASE, y2: BASE, "stroke-width": "1" }, svg), "var(--upwr-ink-subtle)");

    // Oś: znaczniki i etykiety μ − 1.96·SE, μ, μ + 1.96·SE
    var ticks = [[MU - ME, "μ − 1.96·SE"], [MU, "μ"], [MU + ME, "μ + 1.96·SE"]];
    ticks.forEach(function (t) {
      var px = sx(t[0]);
      setStroke(el("line", { x1: px, x2: px, y1: BASE, y2: BASE + 6, "stroke-width": "1" }, svg), "var(--upwr-ink-subtle)");
      var label = el("text", { x: px, y: 290, "text-anchor": "middle", "font-size": "14" }, svg);
      label.textContent = t[1];
      setFill(label, "var(--upwr-ink-soft)");
    });

    // Prawdziwe μ: przerywana linia przez cały rysunek, z etykietą u góry
    setStroke(el("line", { x1: sx(MU), x2: sx(MU), y1: 18, y2: 262, "stroke-width": "1.6", "stroke-dasharray": "7 6" }, svg), "var(--upwr-ink)");
    var muLabel = el("text", { x: sx(MU) + 8, y: 26, "font-size": "18" }, svg);
    muLabel.textContent = "μ";
    setFill(muLabel, "var(--upwr-ink)");

    // --- SVG: elementy zależne od x̄ ------------------------------------------
    var connector = el("line", { "stroke-width": "1.4", "stroke-dasharray": "2 4" }, svg);
    setStroke(connector, "var(--upwr-ink-soft)");

    var interval = el("line", { y1: IVY, y2: IVY, "stroke-width": "9" }, svg);
    var capL = el("line", { y1: IVY - 9, y2: IVY + 9, "stroke-width": "3" }, svg);
    var capR = el("line", { y1: IVY - 9, y2: IVY + 9, "stroke-width": "3" }, svg);

    var handle = el("g", {
      class: "s4p-flip-handle",
      role: "slider",
      tabindex: "0",
      "aria-label": "x̄, średnia z próby",
      "aria-valuemin": XMIN.toFixed(1),
      "aria-valuemax": XMAX.toFixed(1)
    }, svg);
    var hit = el("circle", { r: "18", fill: "transparent", class: "s4p-flip-hit" }, handle);
    var rhomb = el("polygon", { "stroke-width": "2" }, handle);
    setFill(rhomb, "var(--upwr-cat-bursztyn)");
    setStroke(rhomb, "var(--upwr-surface)");

    // Odczyt pod rysunkiem: jeden wiersz, dwa fragmenty (tekst i werdykt)
    var txt = null, verdict = null;
    if (readEl) {
      txt = document.createElement("span");
      verdict = document.createElement("span");
      verdict.className = "s4p-flip-verdict";
      readEl.textContent = "";
      readEl.appendChild(txt);
      readEl.appendChild(verdict);
    }

    function render() {
      var hitNow = Math.abs(x - MU) <= ME;
      var col = hitNow ? "var(--upwr-cat-szalwia)" : "var(--upwr-cat-terakota)";
      var px = sx(x), pl = sx(x - ME), pr = sx(x + ME);

      connector.setAttribute("x1", px); connector.setAttribute("x2", px);
      connector.setAttribute("y1", BASE + 10); connector.setAttribute("y2", IVY - 10);

      interval.setAttribute("x1", pl); interval.setAttribute("x2", pr);
      capL.setAttribute("x1", pl); capL.setAttribute("x2", pl);
      capR.setAttribute("x1", pr); capR.setAttribute("x2", pr);
      setStroke(interval, col); setStroke(capL, col); setStroke(capR, col);

      hit.setAttribute("cx", px); hit.setAttribute("cy", BASE);
      rhomb.setAttribute("points",
        px + "," + (BASE - 9) + " " + (px + 9) + "," + BASE + " " +
        px + "," + (BASE + 9) + " " + (px - 9) + "," + BASE);

      handle.setAttribute("aria-valuenow", x.toFixed(1));
      handle.setAttribute("aria-valuetext", "x̄ = " + x.toFixed(1) + (hitNow ? ", łapie μ" : ", nie łapie μ"));

      if (txt) txt.textContent = "x̄ = " + x.toFixed(1) + " · |x̄ − μ| = " + Math.abs(x - MU).toFixed(1) + " · ";
      if (verdict) {
        verdict.textContent = hitNow ? "łapie μ" : "nie łapie μ";
        verdict.setAttribute("data-hit", hitNow ? "yes" : "no");
      }
    }

    // --- Przeciąganie myszą i palcem -----------------------------------------
    function toValue(e) {
      var pt = svg.createSVGPoint();
      pt.x = e.clientX; pt.y = e.clientY;
      var loc = pt.matrixTransform(svg.getScreenCTM().inverse());
      return LO + (loc.x - X0) * (HI - LO) / (X1 - X0);
    }

    var dragging = false;
    svg.addEventListener("pointerdown", function (e) {
      if (e.button !== undefined && e.button !== 0) return;
      dragging = true;
      try { svg.setPointerCapture(e.pointerId); } catch (err) { /* bez przechwycenia też działa */ }
      x = snap(toValue(e));
      render();
      e.preventDefault();
    });
    svg.addEventListener("pointermove", function (e) {
      if (!dragging) return;
      x = snap(toValue(e));
      render();
    });
    ["pointerup", "pointercancel"].forEach(function (t) {
      svg.addEventListener(t, function () { dragging = false; });
    });

    // --- Klawiatura -----------------------------------------------------------
    handle.addEventListener("keydown", function (e) {
      var step = e.shiftKey ? 1 : STEP;
      var next = null;
      if (e.key === "ArrowLeft" || e.key === "ArrowDown") next = x - step;
      else if (e.key === "ArrowRight" || e.key === "ArrowUp") next = x + step;
      else if (e.key === "Home") next = XMIN;
      else if (e.key === "End") next = XMAX;
      if (next === null) return;
      e.preventDefault();
      x = snap(next);
      render();
    });

    x = snap(parseFloat(root.dataset.x || "172.4"));
    render();
  }

  function scan() {
    document.querySelectorAll(".s4p-flip:not([data-ready])").forEach(init);
  }

  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener("DOMContentLoaded", scan);
  window.addEventListener("load", scan);
})();
