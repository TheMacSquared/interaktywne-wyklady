// Pole pod gęstością N(μ, σ) między dwiema granicami (Ryc. 4.3). Granice a i b
// przeciąga się kursorem lub palcem: wciśnięcie łapie bliższą granicę, ruch ją
// przesuwa. Kontener: .lc-area[data-mu][data-sd][data-a][data-b][data-lo][data-hi]
// z <canvas data-role="pdf"> i odczytami [data-role="a"], [data-role="b"],
// [data-role="P"]. Wzorzec jak cdf_chart.js (Ryc. 4.4): Chart.js, start po
// pojawieniu się kontenera w DOM (rozdział renderuje się przez renderUI).
(function () {
  var SNAP = 0.5;

  // Abramowitz–Stegun 7.1.26 (błąd < 1.5e-7).
  function phi(z) {
    var x = Math.abs(z) / Math.SQRT2, t = 1 / (1 + 0.3275911 * x);
    var erf = 1 - t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 +
      t * (-1.453152027 + t * 1.061405429)))) * Math.exp(-x * x);
    return z >= 0 ? (1 + erf) / 2 : (1 - erf) / 2;
  }

  function token(name) {
    return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
  }

  function alpha(hex, a) {
    var h = hex.replace("#", "");
    if (h.length === 3) h = h.split("").map(function (c) { return c + c; }).join("");
    var n = parseInt(h, 16);
    return "rgba(" + (n >> 16 & 255) + "," + (n >> 8 & 255) + "," + (n & 255) + "," + a + ")";
  }

  function init(root) {
    if (root.dataset.ready || typeof Chart === "undefined") return;
    var canvas = root.querySelector('canvas[data-role="pdf"]');
    if (!canvas) return;
    root.dataset.ready = "1";

    var MU = parseFloat(root.dataset.mu || "0"), SD = parseFloat(root.dataset.sd || "1");
    var LO = parseFloat(root.dataset.lo || String(MU - 4 * SD));
    var HI = parseFloat(root.dataset.hi || String(MU + 4 * SD));
    var a = parseFloat(root.dataset.a || String(MU - SD));
    var b = parseFloat(root.dataset.b || String(MU + SD));
    var readA = root.querySelector('[data-role="a"]');
    var readB = root.querySelector('[data-role="b"]');
    var readP = root.querySelector('[data-role="P"]');

    function pdf(x) { var z = (x - MU) / SD; return Math.exp(-z * z / 2) / (SD * Math.sqrt(2 * Math.PI)); }
    var TOP = pdf(MU) * 1.18;
    var XS = [];
    for (var x = LO; x <= HI + 1e-9; x += (HI - LO) / 480) XS.push(+x.toFixed(4));

    var chart = new Chart(canvas, {
      type: "line",
      data: { datasets: [
        { data: XS.map(function (x) { return { x: x, y: pdf(x) }; }), borderWidth: 2 },
        { data: [], fill: "origin", borderWidth: 0 },
        { data: [], borderWidth: 2, borderDash: [5, 4] },
        { data: [], borderWidth: 2, borderDash: [5, 4] },
        { data: [], pointRadius: 8, pointHoverRadius: 8, borderWidth: 2, showLine: false }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false,
        locale: "en-US",
        events: [],
        layout: { padding: { top: 6, right: 8 } },
        scales: {
          x: { type: "linear", min: LO, max: HI, ticks: { stepSize: 10 },
               title: { display: true, text: "Wzrost (cm)" } },
          y: { min: 0, max: TOP, title: { display: true, text: "Gęstość" } }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });

    function paint() {
      var ink = token("--upwr-ink-soft") || "#444";
      var subtle = token("--upwr-ink-subtle") || "#888";
      var rule = token("--upwr-rule-soft") || "#ddd";
      var accent = token("--upwr-accent") || "#7a2e3b";
      var surface = token("--upwr-surface") || "#fff";
      var font = token("--upwr-mono") || "monospace";
      ["x", "y"].forEach(function (k) {
        var s = chart.options.scales[k];
        s.grid = { color: rule };
        s.border = { color: rule };
        s.ticks = Object.assign(s.ticks || {}, { color: subtle, font: { family: font, size: 12 } });
        s.title.color = subtle;
        s.title.font = { family: font, size: 12 };
      });
      var ds = chart.data.datasets;
      ds[0].borderColor = ink;
      ds[1].backgroundColor = alpha(accent, 0.28);
      ds[2].borderColor = ds[3].borderColor = accent;
      ds[4].backgroundColor = ds[4].pointBackgroundColor = ds[4].pointHoverBackgroundColor = accent;
      ds[4].borderColor = ds[4].pointBorderColor = ds[4].pointHoverBorderColor = surface;
      chart.update();
    }

    function draw() {
      var lo = Math.min(a, b), hi = Math.max(a, b), P = phi((hi - MU) / SD) - phi((lo - MU) / SD);
      var shade = [{ x: lo, y: pdf(lo) }].concat(
        XS.filter(function (x) { return x > lo && x < hi; }).map(function (x) { return { x: x, y: pdf(x) }; }),
        [{ x: hi, y: pdf(hi) }]);
      var ds = chart.data.datasets, hy = TOP * 0.94;
      ds[1].data = shade;
      ds[2].data = [{ x: a, y: 0 }, { x: a, y: hy }];
      ds[3].data = [{ x: b, y: 0 }, { x: b, y: hy }];
      ds[4].data = [{ x: a, y: hy }, { x: b, y: hy }];
      chart.update("none");
      if (readA) readA.textContent = lo.toFixed(1);
      if (readB) readB.textContent = hi.toFixed(1);
      if (readP) readP.textContent = P.toFixed(3);
    }

    // Przeciąganie: wciśnięcie łapie bliższą granicę, ruch z wciśniętym przyciskiem ją przesuwa.
    var drag = null;
    function valueAt(e) {
      var r = canvas.getBoundingClientRect(), area = chart.chartArea;
      var px = Math.max(area.left, Math.min(area.right, e.clientX - r.left));
      var v = chart.scales.x.getValueForPixel(px);
      return Math.max(LO, Math.min(HI, Math.round(v / SNAP) * SNAP));
    }
    function move(e) {
      var v = valueAt(e);
      if (drag === "a") a = v; else b = v;
      draw();
    }
    canvas.style.touchAction = "none";
    canvas.style.cursor = "ew-resize";
    canvas.addEventListener("pointerdown", function (e) {
      var v = valueAt(e);
      drag = Math.abs(v - a) <= Math.abs(v - b) ? "a" : "b";
      canvas.setPointerCapture(e.pointerId);
      move(e);
      e.preventDefault();
    });
    canvas.addEventListener("pointermove", function (e) { if (drag) move(e); });
    ["pointerup", "pointercancel"].forEach(function (t) {
      canvas.addEventListener(t, function () { drag = null; });
    });

    paint();
    draw();

    new MutationObserver(paint).observe(document.documentElement,
      { attributes: true, attributeFilter: ["data-lc-theme"] });
  }

  function scan() {
    document.querySelectorAll(".lc-area:not([data-ready])").forEach(init);
  }

  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener("DOMContentLoaded", scan);
  window.addEventListener("load", scan);
})();
