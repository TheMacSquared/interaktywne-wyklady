// Seria kontroli zaworów (wykład 04): n prób Bernoulliego z prawdopodobieństwem p.
// Element pre[data-p4-series-root] wskazuje suwak (data-slider), przycisk
// (data-button) i p (data-p); odczyty to [data-p4-series="bad" | "expected"]
// w tym samym panelu. Nowa seria po kliknięciu przycisku i po zmianie suwaka.
(function () {
  "use strict";

  // Seria jako lista 0/1; random to funkcja zwracająca liczby z [0, 1).
  function draw(n, p, random) {
    var out = [];
    for (var i = 0; i < n; i++) out.push(random() < p ? 1 : 0);
    return out;
  }
  function sequenceText(series) {
    return series.map(function (x) { return x === 1 ? "×" : "·"; }).join(" ");
  }
  function expectedText(n, p) {
    return String(Math.round(n * p * 10) / 10);
  }
  globalThis.lcP4Seria = { draw: draw, sequenceText: sequenceText, expectedText: expectedText };
  if (typeof document === "undefined") return;

  function init(root) {
    root.setAttribute("data-ready", "true");
    var panel = root.closest(".lc-figure-panel") || document;
    var slider = document.getElementById(root.getAttribute("data-slider"));
    var button = document.getElementById(root.getAttribute("data-button"));
    var p = Number(root.getAttribute("data-p"));
    var out = function (name) { return panel.querySelector('[data-p4-series="' + name + '"]'); };

    function run() {
      // Przed pierwszym ruchem pole suwaka bywa puste: wartość startowa jest w data-from.
      var raw = slider ? (slider.value !== "" ? slider.value : slider.getAttribute("data-from")) : 0;
      var n = Math.max(0, Math.round(Number(raw) || 0));
      var series = draw(n, p, Math.random);
      root.textContent = sequenceText(series);
      if (out("bad")) out("bad").textContent = String(series.reduce(function (a, b) { return a + b; }, 0));
      if (out("expected")) out("expected").textContent = expectedText(n, p);
    }
    if (button) button.addEventListener("click", run);
    if (slider) {
      // ionRangeSlider zgłasza zmianę przez jQuery; zwykłe pole przez zdarzenie input.
      if (window.jQuery) window.jQuery(slider).on("change", run);
      slider.addEventListener("input", run);
    }
    run();
  }

  function scan() { document.querySelectorAll("pre[data-p4-series-root]:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
