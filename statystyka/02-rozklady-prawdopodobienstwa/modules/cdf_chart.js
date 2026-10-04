// Dystrybuanta N(0, 1): dwa połączone wykresy Chart.js (gęstość z polem
// na lewo od x i krzywa F(x) z punktem). Kursor lub palec na którymkolwiek
// wykresie ustawia x. Kontener: .lc-cdf[data-x] z dwoma <canvas>
// (data-role="pdf" / "cdf") i odczytami [data-role="x"] / [data-role="F"].
// Rozdział renderuje się przez renderUI, więc wykresy startują, gdy
// kontener pojawi się w DOM.
(function () {
  var X_MIN = -4, X_MAX = 4, STEP = 0.025;

  function pdf(x) { return Math.exp(-x * x / 2) / Math.sqrt(2 * Math.PI); }

  // Abramowitz–Stegun 7.1.26 (błąd < 1.5e-7) — wystarcza do 4 miejsc.
  function cdf(x) {
    var z = Math.abs(x) / Math.SQRT2, t = 1 / (1 + 0.3275911 * z);
    var erf = 1 - t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 +
      t * (-1.453152027 + t * 1.061405429)))) * Math.exp(-z * z);
    return x >= 0 ? (1 + erf) / 2 : (1 - erf) / 2;
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

  function fmt(v, d) { return v.toFixed(d); }

  var XS = [];
  for (var i = 0; i <= (X_MAX - X_MIN) / STEP; i++) XS.push(+(X_MIN + i * STEP).toFixed(3));

  function axes(yMax, yTitle) {
    return {
      x: { type: "linear", min: X_MIN, max: X_MAX,
           ticks: { stepSize: 1 }, title: { display: true, text: "x" } },
      y: { min: 0, max: yMax, title: { display: true, text: yTitle } }
    };
  }

  function baseOptions(yMax, yTitle) {
    return {
      responsive: true, maintainAspectRatio: false, animation: false,
      locale: "en-US",  // kropka dziesiętna na osiach
      events: ["mousemove", "mousedown", "touchstart", "touchmove", "click"],
      layout: { padding: { top: 6, right: 8 } },
      scales: axes(yMax, yTitle),
      plugins: { legend: { display: false }, tooltip: { enabled: false },
                 colors: { enabled: false } },
      elements: { point: { radius: 0 }, line: { tension: 0 } }
    };
  }

  function init(root) {
    if (root.dataset.ready || typeof Chart === "undefined") return;
    var pdfCanvas = root.querySelector('canvas[data-role="pdf"]');
    var cdfCanvas = root.querySelector('canvas[data-role="cdf"]');
    if (!pdfCanvas || !cdfCanvas) return;
    root.dataset.ready = "1";

    var x0 = parseFloat(root.dataset.x || "1");
    var readX = root.querySelector('[data-role="x"]');
    var readF = root.querySelector('[data-role="F"]');

    var pdfChart = new Chart(pdfCanvas, {
      type: "line",
      data: { datasets: [
        { data: XS.map(function (x) { return { x: x, y: pdf(x) }; }), borderWidth: 2 },
        { data: [], fill: "origin", borderWidth: 0 },
        { data: [], borderWidth: 1.5 }
      ] },
      options: baseOptions(0.45, "Gęstość f(x)")
    });

    var cdfChart = new Chart(cdfCanvas, {
      type: "line",
      data: { datasets: [
        { data: XS.map(function (x) { return { x: x, y: cdf(x) }; }), borderWidth: 2 },
        { data: [], borderWidth: 1.5, borderDash: [5, 4] },
        { data: [], borderWidth: 1.5, borderDash: [5, 4] },
        { data: [], pointRadius: 6, pointHoverRadius: 6, borderWidth: 2, showLine: false }
      ] },
      options: baseOptions(1.05, "F(x) = P(X ≤ x)")
    });
    cdfChart.options.scales.y.max = 1;
    cdfChart.options.scales.y.ticks = { stepSize: 0.2 };

    function paint() {
      var ink = token("--upwr-ink-soft") || "#444";
      var subtle = token("--upwr-ink-subtle") || "#888";
      var rule = token("--upwr-rule-soft") || "#ddd";
      var accent = token("--upwr-accent") || "#7a2e3b";
      var font = token("--upwr-mono") || "monospace";
      [pdfChart, cdfChart].forEach(function (ch) {
        ["x", "y"].forEach(function (k) {
          var s = ch.options.scales[k];
          s.grid = { color: rule };
          s.border = { color: rule };
          s.ticks = Object.assign(s.ticks || {}, { color: subtle, font: { family: font, size: 12 } });
          s.title.color = subtle;
          s.title.font = { family: font, size: 12 };
        });
        ch.data.datasets[0].borderColor = ink;
      });
      pdfChart.data.datasets[1].backgroundColor = alpha(accent, 0.28);
      pdfChart.data.datasets[2].borderColor = accent;
      cdfChart.data.datasets[1].borderColor = accent;
      cdfChart.data.datasets[2].borderColor = accent;
      var surface = token("--upwr-surface") || "#fff";
      var dot = cdfChart.data.datasets[3];
      dot.backgroundColor = dot.pointBackgroundColor = dot.pointHoverBackgroundColor = accent;
      dot.borderColor = dot.pointBorderColor = dot.pointHoverBorderColor = surface;
      // Pełny update: tryb "none" zostawia w punktach zbuforowane stare kolory.
      pdfChart.update();
      cdfChart.update();
    }

    function setX(x) {
      x0 = Math.max(X_MIN, Math.min(X_MAX, Math.round(x * 100) / 100));
      var F = cdf(x0);
      var shade = XS.filter(function (x) { return x <= x0; })
        .map(function (x) { return { x: x, y: pdf(x) }; });
      shade.push({ x: x0, y: pdf(x0) });
      pdfChart.data.datasets[1].data = shade;
      pdfChart.data.datasets[2].data = [{ x: x0, y: 0 }, { x: x0, y: pdf(x0) }];
      cdfChart.data.datasets[1].data = [{ x: x0, y: 0 }, { x: x0, y: F }];
      cdfChart.data.datasets[2].data = [{ x: X_MIN, y: F }, { x: x0, y: F }];
      cdfChart.data.datasets[3].data = [{ x: x0, y: F }];
      pdfChart.update("none");
      cdfChart.update("none");
      if (readX) readX.textContent = fmt(x0, 2);
      if (readF) readF.textContent = fmt(F, 4);
    }

    function follow(ch) {
      ch.options.onHover = ch.options.onClick = function (e) {
        var area = ch.chartArea;
        if (e.x < area.left || e.x > area.right) return;
        setX(ch.scales.x.getValueForPixel(e.x));
      };
    }
    follow(pdfChart);
    follow(cdfChart);

    paint();
    setX(x0);

    // Przełącznik jasny/ciemny zmienia tokeny CSS — przemaluj wykresy.
    new MutationObserver(paint).observe(document.documentElement,
      { attributes: true, attributeFilter: ["data-lc-theme"] });
  }

  function scan() {
    document.querySelectorAll(".lc-cdf:not([data-ready])").forEach(init);
  }

  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener("DOMContentLoaded", scan);
  window.addEventListener("load", scan);
})();
