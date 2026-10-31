// Kwantyl jako odwrotne pytanie (Ryc. 4.5): gęstość N(μ, σ) z polem na lewo od x.
// Tryb „x”: granicę x przeciąga się po wykresie, szukane jest p = P(X ≤ x).
// Tryb „p”: wybiera się p, szukany jest x. Szukana liczba to „?” do kliknięcia „Odsłoń”;
// każda zmiana chowa ją z powrotem. Kontener: .lc-quant[data-mu][data-sd][data-x][data-lo][data-hi]
// z <canvas data-role="pdf">, przyciskami [data-q-mode], [data-q-p], [data-q-reveal]
// i odczytami [data-role="x"] / [data-role="p"]. Wzorzec jak area_chart.js (Ryc. 4.3).
(function () {
  var Charts = window.lcCharts;
  var token = Charts.token, alpha = Charts.alpha;
  var SNAP = 0.5;

  var phi = Charts.normalCdf;

  function init(root) {
    var canvas = root.querySelector('canvas[data-role="pdf"]');
    if (!canvas) return false;

    var MU = parseFloat(root.dataset.mu || "0"), SD = parseFloat(root.dataset.sd || "1");
    var LO = parseFloat(root.dataset.lo || String(MU - 4 * SD)), HI = parseFloat(root.dataset.hi || String(MU + 4 * SD));
    var mode = "x", x = parseFloat(root.dataset.x || String(MU + SD)), p = phi((x - MU) / SD), shown = false;
    var readX = root.querySelector('[data-role="x"]'), readP = root.querySelector('[data-role="p"]');
    var pGroup = root.querySelector('[data-role="pgroup"]'), revealBtn = root.querySelector("[data-q-reveal]");

    function pdf(v) { var z = (v - MU) / SD; return Math.exp(-z * z / 2) / (SD * Math.sqrt(2 * Math.PI)); }
    function quant(q) {
      var a = LO, b = HI;
      for (var i = 0; i < 60; i++) { var m = (a + b) / 2; if (phi((m - MU) / SD) < q) a = m; else b = m; }
      return (a + b) / 2;
    }
    var TOP = pdf(MU) * 1.25;
    var XS = [];
    for (var v = LO; v <= HI + 1e-9; v += (HI - LO) / 480) XS.push(+v.toFixed(4));

    // Duże liczby na wykresie: p w zacienionym polu, x nad granicą („?”, gdy szukane).
    var labels = {
      id: "quantLabels",
      afterDatasetsDraw: function (ch) {
        var c = ch.ctx, sx = ch.scales.x, sy = ch.scales.y, accent = token("--upwr-accent") || "#7a2e3b";
        var font = token("--upwr-mono") || "monospace";
        var pTxt = (mode === "x" && !shown) ? "?" : p.toFixed(2);
        var xTxt = "x = " + ((mode === "p" && !shown) ? "?" : x.toFixed(1));
        c.save();
        c.fillStyle = accent; c.textAlign = "center";
        c.font = "700 30px " + font;
        var cx = sx.getPixelForValue(Math.max(LO + 4, Math.min(x - 6, (Math.max(LO, x - 3 * SD) + x) / 2)));
        c.fillText(pTxt, cx, sy.getPixelForValue(TOP * 0.22));
        c.font = "700 18px " + font;
        c.fillText(xTxt, sx.getPixelForValue(x), sy.getPixelForValue(TOP * 0.93));
        c.restore();
      }
    };

    var chart = Charts.create(canvas, {
      type: "line",
      data: { datasets: [
        { data: XS.map(function (v) { return { x: v, y: pdf(v) }; }), borderWidth: 2 },
        { data: [], fill: "origin", borderWidth: 0 },
        { data: [], borderWidth: 2 },
        { data: [], pointRadius: 8, pointHoverRadius: 8, borderWidth: 2, showLine: false }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 6, right: 8 } },
        scales: {
          x: { type: "linear", min: LO, max: HI, ticks: { stepSize: 10 }, title: { display: true, text: "Wzrost (cm)" } },
          y: { min: 0, max: TOP, title: { display: true, text: "Gęstość" } }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      },
      plugins: [labels]
    });

    function paint() {
      var ink = token("--upwr-ink-soft") || "#444";
      var accent = token("--upwr-accent") || "#7a2e3b";
      var surface = token("--upwr-surface") || "#fff";
      Charts.styleAxes(chart);
      var ds = chart.data.datasets;
      ds[0].borderColor = ink;
      ds[1].backgroundColor = alpha(accent, 0.28);
      ds[2].borderColor = accent;
      ds[3].backgroundColor = ds[3].pointBackgroundColor = ds[3].pointHoverBackgroundColor = accent;
      ds[3].borderColor = ds[3].pointBorderColor = ds[3].pointHoverBorderColor = surface;
      chart.update();
    }

    function draw() {
      var ds = chart.data.datasets, hy = TOP * 0.82;
      ds[1].data = XS.filter(function (v) { return v < x; }).map(function (v) { return { x: v, y: pdf(v) }; })
        .concat([{ x: x, y: pdf(x) }]);
      ds[2].data = [{ x: x, y: 0 }, { x: x, y: hy }];
      ds[3].data = mode === "x" ? [{ x: x, y: hy }] : [];
      chart.update("none");
      if (readX) readX.textContent = (mode === "p" && !shown) ? "?" : x.toFixed(1);
      if (readP) readP.textContent = (mode === "x" && !shown) ? "?" : p.toFixed(2);
      if (pGroup) pGroup.hidden = mode !== "p";
      if (revealBtn) { var sp = revealBtn.querySelector("span") || revealBtn; sp.textContent = shown ? "Ukryj" : "Odsłoń"; }
      canvas.style.cursor = mode === "x" ? "ew-resize" : "default";
    }

    function press(group, btn) {
      root.querySelectorAll(group).forEach(function (b) { b.setAttribute("aria-pressed", b === btn ? "true" : "false"); });
    }

    Charts.listen(root, "click", function (e) {
      var m = e.target.closest("[data-q-mode]"), q = e.target.closest("[data-q-p]"), r = e.target.closest("[data-q-reveal]");
      if (m) {
        mode = m.getAttribute("data-q-mode"); press("[data-q-mode]", m); shown = false;
        if (mode === "p") {
          var cur = root.querySelector('[data-q-p][aria-pressed="true"]') || root.querySelector("[data-q-p]");
          press("[data-q-p]", cur); p = parseFloat(cur.getAttribute("data-q-p")); x = quant(p);
        }
        draw();
      } else if (q) {
        press("[data-q-p]", q); p = parseFloat(q.getAttribute("data-q-p")); x = quant(p); shown = false; draw();
      } else if (r) {
        shown = !shown; draw();
      }
    });

    // Przeciąganie granicy tylko w trybie „x”.
    var drag = false;
    function valueAt(e) {
      var rc = canvas.getBoundingClientRect(), area = chart.chartArea;
      var px = Math.max(area.left, Math.min(area.right, e.clientX - rc.left));
      return Math.max(LO, Math.min(HI, Math.round(chart.scales.x.getValueForPixel(px) / SNAP) * SNAP));
    }
    function move(e) { x = valueAt(e); p = phi((x - MU) / SD); shown = false; draw(); }
    canvas.style.touchAction = "none";
    Charts.listen(canvas, "pointerdown", function (e) {
      if (mode !== "x") return;
      drag = true; canvas.setPointerCapture(e.pointerId); move(e); e.preventDefault();
    });
    Charts.listen(canvas, "pointermove", function (e) { if (drag) move(e); });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(canvas, t, function () { drag = false; }); });

    paint();
    draw();
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-quant", init);
})();
