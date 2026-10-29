// Dwie krzywe normalne (Ryc. 5.3): wzrost kobiet i mężczyzn. Każda krzywa ma
// uchwyt na szczycie: ruch w bok zmienia μ, w górę i w dół σ (wyższy szczyt to
// węższa krzywa, bo pole zawsze wynosi 1). Kontener:
// .lc-normals[data-mu1][data-sd1][data-mu2][data-sd2][data-lo][data-hi]
// z <canvas data-role="pdf">, przyciskiem [data-n-reset] i odczytami
// [data-role="n1"], [data-role="n2"], [data-role="a"], [data-role="b"].
// Wzorzec jak area_chart.js (Ryc. 4.3).
(function () {
  var SD_MIN = 4, SD_MAX = 20;

  function phi(z) {
    var x = Math.abs(z) / Math.SQRT2, t = 1 / (1 + 0.3275911 * x);
    var erf = 1 - t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 +
      t * (-1.453152027 + t * 1.061405429)))) * Math.exp(-x * x);
    return z >= 0 ? (1 + erf) / 2 : (1 - erf) / 2;
  }
  function token(name) { return getComputedStyle(document.documentElement).getPropertyValue(name).trim(); }
  function alpha(hex, a) {
    var h = hex.replace("#", "");
    if (h.length === 3) h = h.split("").map(function (c) { return c + c; }).join("");
    var n = parseInt(h, 16);
    return "rgba(" + (n >> 16 & 255) + "," + (n >> 8 & 255) + "," + (n & 255) + "," + a + ")";
  }
  function pdf(v, mu, sd) { var z = (v - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }

  function init(root) {
    if (root.dataset.ready || typeof Chart === "undefined") return;
    var canvas = root.querySelector('canvas[data-role="pdf"]');
    if (!canvas) return;
    root.dataset.ready = "1";

    var START = [
      { mu: parseFloat(root.dataset.mu1), sd: parseFloat(root.dataset.sd1) },
      { mu: parseFloat(root.dataset.mu2), sd: parseFloat(root.dataset.sd2) }
    ];
    var G = START.map(function (g) { return { mu: g.mu, sd: g.sd }; });
    var LO = parseFloat(root.dataset.lo || "130"), HI = parseFloat(root.dataset.hi || "215");
    var TOP = 0.12;
    var XS = [];
    for (var v = LO; v <= HI + 1e-9; v += (HI - LO) / 480) XS.push(+v.toFixed(4));
    var read = {};
    ["n1", "n2", "a", "b"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });

    var chart = new Chart(canvas, {
      type: "line",
      data: { datasets: [
        { data: [], fill: "origin", borderWidth: 2.5 },
        { data: [], fill: "origin", borderWidth: 2.5 },
        { data: [], pointRadius: 8, pointHoverRadius: 8, borderWidth: 2, showLine: false }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 10, right: 8 } },
        scales: {
          x: { type: "linear", min: LO, max: HI, ticks: { stepSize: 10 }, title: { display: true, text: "Wzrost (cm)" } },
          y: { min: 0, max: TOP, ticks: { stepSize: 0.02 }, title: { display: true, text: "Gęstość" } }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });

    var COLS = [];
    function paint() {
      var subtle = token("--upwr-ink-subtle") || "#888", rule = token("--upwr-rule-soft") || "#ddd";
      var surface = token("--upwr-surface") || "#fff", font = token("--upwr-mono") || "monospace";
      COLS = [token("--upwr-cat-niebo") || "#6a9dc4", token("--upwr-cat-terakota") || "#b25838"];
      ["x", "y"].forEach(function (k) {
        var s = chart.options.scales[k];
        s.grid = { color: rule }; s.border = { color: rule };
        s.ticks = Object.assign(s.ticks || {}, { color: subtle, font: { family: font, size: 12 } });
        s.title.color = subtle; s.title.font = { family: font, size: 12 };
      });
      var ds = chart.data.datasets;
      [0, 1].forEach(function (i) { ds[i].borderColor = COLS[i]; ds[i].backgroundColor = alpha(COLS[i], 0.18); });
      ds[2].pointBackgroundColor = ds[2].pointHoverBackgroundColor = COLS;
      ds[2].pointBorderColor = ds[2].pointHoverBorderColor = surface;
      chart.update();
    }

    function pct(x) { return (100 * x).toFixed(1) + "%"; }
    function num(x) { return x.toFixed(1); }
    function draw() {
      var ds = chart.data.datasets;
      [0, 1].forEach(function (i) {
        ds[i].data = XS.map(function (v) { return { x: v, y: pdf(v, G[i].mu, G[i].sd) }; });
      });
      ds[2].data = G.map(function (g) { return { x: g.mu, y: pdf(g.mu, g.mu, g.sd) }; });
      chart.update("none");
      if (read.n1) read.n1.textContent = "N(" + num(G[0].mu) + ", " + num(G[0].sd) + ")";
      if (read.n2) read.n2.textContent = "N(" + num(G[1].mu) + ", " + num(G[1].sd) + ")";
      // kobiety wyższe niż średni mężczyzna; mężczyźni niżsi niż średnia kobieta
      if (read.a) read.a.textContent = pct(1 - phi((G[1].mu - G[0].mu) / G[0].sd));
      if (read.b) read.b.textContent = pct(phi((G[0].mu - G[1].mu) / G[1].sd));
    }

    // Przeciąganie uchwytu: wciśnięcie łapie bliższy szczyt (w pikselach).
    var drag = -1;
    function at(e) {
      var rc = canvas.getBoundingClientRect();
      return { px: e.clientX - rc.left, py: e.clientY - rc.top };
    }
    function move(e) {
      var p = at(e), a = chart.chartArea;
      var mu = chart.scales.x.getValueForPixel(Math.max(a.left, Math.min(a.right, p.px)));
      var h = chart.scales.y.getValueForPixel(Math.max(a.top, Math.min(a.bottom, p.py)));
      G[drag].mu = Math.max(LO + 5, Math.min(HI - 5, Math.round(mu * 2) / 2));
      var sd = 1 / (Math.max(h, 1e-6) * Math.sqrt(2 * Math.PI));
      G[drag].sd = Math.max(SD_MIN, Math.min(SD_MAX, Math.round(sd * 10) / 10));
      draw();
    }
    canvas.style.touchAction = "none";
    canvas.addEventListener("pointerdown", function (e) {
      var p = at(e), best = -1, bd = Infinity;
      G.forEach(function (g, i) {
        var dx = chart.scales.x.getPixelForValue(g.mu) - p.px, dy = chart.scales.y.getPixelForValue(pdf(g.mu, g.mu, g.sd)) - p.py;
        var d = Math.sqrt(dx * dx + dy * dy);
        if (d < bd) { bd = d; best = i; }
      });
      if (best < 0 || bd > 60) return;
      drag = best; canvas.setPointerCapture(e.pointerId); move(e); e.preventDefault();
    });
    canvas.addEventListener("pointermove", function (e) {
      if (drag >= 0) { move(e); return; }
      var p = at(e), near = G.some(function (g) {
        var dx = chart.scales.x.getPixelForValue(g.mu) - p.px, dy = chart.scales.y.getPixelForValue(pdf(g.mu, g.mu, g.sd)) - p.py;
        return dx * dx + dy * dy < 60 * 60;
      });
      canvas.style.cursor = near ? "move" : "default";
    });
    ["pointerup", "pointercancel"].forEach(function (t) { canvas.addEventListener(t, function () { drag = -1; }); });

    root.addEventListener("click", function (e) {
      if (!e.target.closest("[data-n-reset]")) return;
      G = START.map(function (g) { return { mu: g.mu, sd: g.sd }; }); draw();
    });

    paint();
    draw();
    new MutationObserver(paint).observe(document.documentElement, { attributes: true, attributeFilter: ["data-lc-theme"] });
  }

  function scan() { document.querySelectorAll(".lc-normals:not([data-ready])").forEach(init); }
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener("DOMContentLoaded", scan);
  window.addEventListener("load", scan);
})();
