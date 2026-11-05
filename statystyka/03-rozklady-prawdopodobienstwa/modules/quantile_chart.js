// Kwantyl jako odwrotne pytanie (Ryc. 4.5): gęstość N(μ, σ) z polem na lewo od x.
// Tryb „x”: granicę x przeciąga się po wykresie, szukane jest p = P(X ≤ x).
// Tryb „p”: wybiera się p, szukany jest x. Szukana liczba to „?” do kliknięcia „Odsłoń”;
// każda zmiana chowa ją z powrotem. Kontener: .lc-quant[data-mu][data-sd][data-x][data-lo][data-hi]
// z <div class="lc-ec" data-role="pdf">, przyciskami [data-q-mode], [data-q-p], [data-q-reveal]
// i odczytami [data-role="x"] / [data-role="p"]. ECharts, wzorzec jak area_chart.js (Ryc. 4.3).
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;
  var SNAP = 0.5;

  var phi = Charts.normalCdf;

  function init(root) {
    var el = root.querySelector('.lc-ec[data-role="pdf"]');
    if (!el) return false;

    var MU = parseFloat(root.dataset.mu || "0"), SD = parseFloat(root.dataset.sd || "1");
    var LO = parseFloat(root.dataset.lo || String(MU - 4 * SD)), HI = parseFloat(root.dataset.hi || String(MU + 4 * SD));
    var mode = "x", x = parseFloat(root.dataset.x || String(MU + SD)), p = phi((x - MU) / SD), shown = false;
    var readX = root.querySelector('[data-role="x"]'), readP = root.querySelector('[data-role="p"]');
    var pGroup = root.querySelector('[data-role="pgroup"]'), revealBtn = root.querySelector("[data-q-reveal]");

    var layer = Charts.handleLayer(root);
    var handle = layer.add(el, {
      label: "Granica x w pytaniu o prawdopodobieństwo",
      hint: "Strzałki przesuwają x o 0.5 cm; Shift ze strzałką o 5 cm. Odpowiedź odsłania osobny przycisk.",
      onKey: function (event) {
        if (mode !== "x") return false;
        var value = Charts.keyValue(event, x, LO, HI, SNAP, 5);
        if (value === null) return false;
        x = value; p = phi((x - MU) / SD); shown = false; draw();
        layer.announce("x = " + x.toFixed(1) + " cm. Odpowiedź ukryta.");
        return true;
      },
      onPointerDown: function (event) { startDrag(event); }
    });
    function pdf(v) { var z = (v - MU) / SD; return Math.exp(-z * z / 2) / (SD * Math.sqrt(2 * Math.PI)); }
    function quant(q) {
      return Charts.normalQuantile(q, MU, SD, LO, HI);
    }
    var TOP = pdf(MU) * 1.25, HY = TOP * 0.82;
    var XS = [];
    for (var v = LO; v <= HI + 1e-9; v += (HI - LO) / 480) XS.push(+v.toFixed(4));

    function line(id, width) {
      return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
               lineStyle: { width: width } };
    }
    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 12, top: 10, bottom: 8 },
      xAxis: { type: "value", min: LO, max: HI, interval: 10, name: "Wzrost (cm)" },
      yAxis: { type: "value", min: 0, max: TOP, splitNumber: 4, name: "Gęstość" },
      series: [
        Object.assign(line("shade", 0), { areaStyle: {} }),
        Object.assign(line("pdf", 2), { data: XS.map(function (v) { return [v, pdf(v)]; }) }),
        line("mark", 2),
        { id: "dot", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false }
      ]
    }, { onResize: function () { labels(); positionHandle(); } });

    function positionHandle() {
      var q = chart.convertToPixel({ gridIndex: 0 }, [x, HY]);
      if (q) handle.position(q[0], q[1]);
    }

    // Duże liczby na wykresie: p w zacienionym polu, x nad granicą („?”, gdy szukane).
    function labels() {
      var P = Charts.palette(), size = Charts.axisFontSize(root), font = P.mono;
      var pTxt = (mode === "x" && !shown) ? "?" : p.toFixed(2);
      var xTxt = "x = " + ((mode === "p" && !shown) ? "?" : x.toFixed(1));
      var left = chart.convertToPixel({ gridIndex: 0 }, [LO, 0]), right = chart.convertToPixel({ gridIndex: 0 }, [HI, 0]);
      if (!left || !right) return;
      var pSize = size * 2.1, xSize = size * 1.3;
      function clampX(px, text, fs) {
        var half = text.length * fs * 0.31;
        return Math.max(left[0] + half, Math.min(right[0] - half, px));
      }
      var cp = chart.convertToPixel({ gridIndex: 0 },
        [Math.max(LO + 4, Math.min(x - 6, (Math.max(LO, x - 3 * SD) + x) / 2)), TOP * 0.22]);
      var cx = chart.convertToPixel({ gridIndex: 0 }, [x, TOP * 0.93]);
      chart.setOption({ graphic: [
        { id: "pLabel", type: "text", silent: true, z: 10,
          x: clampX(cp[0], pTxt, pSize), y: cp[1],
          style: { text: pTxt, fill: P.accent, font: "700 " + pSize + "px " + font,
                   textAlign: "center", textVerticalAlign: "bottom" } },
        { id: "xLabel", type: "text", silent: true, z: 10,
          x: clampX(cx[0], xTxt, xSize), y: cx[1],
          style: { text: xTxt, fill: P.accent, font: "700 " + xSize + "px " + font,
                   textAlign: "center", textVerticalAlign: "bottom" } }
      ] });
    }

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var y = Charts.ecAxis(P, size, { nameGap: Math.round(size * 3.6) });
      y.axisLabel.showMaxLabel = false;
      y.axisLabel.formatter = function (v) { return Charts.fmt(v, 3); };
      chart.setOption({
        xAxis: Charts.ecAxis(P, size),
        yAxis: y,
        series: [
          { id: "shade", lineStyle: { width: 0 }, areaStyle: { color: alpha(P.accent, 0.28) } },
          { id: "pdf", lineStyle: { color: P.soft } },
          { id: "mark", lineStyle: { color: P.accent } },
          { id: "dot", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } }
        ]
      });
      labels();
      positionHandle();
    }

    function draw() {
      chart.setOption({ series: [
        { id: "shade", data: XS.filter(function (v) { return v < x; }).map(function (v) { return [v, pdf(v)]; })
            .concat([[x, pdf(x)]]) },
        { id: "mark", data: [[x, 0], [x, HY]] },
        { id: "dot", data: mode === "x" ? [[x, HY]] : [] }
      ] });
      if (readX) readX.textContent = (mode === "p" && !shown) ? "?" : x.toFixed(1);
      if (readP) readP.textContent = (mode === "x" && !shown) ? "?" : p.toFixed(2);
      if (pGroup) pGroup.hidden = mode !== "p";
      if (revealBtn) { var sp = revealBtn.querySelector("span") || revealBtn; sp.textContent = shown ? "Ukryj" : "Odsłoń"; }
      el.style.cursor = mode === "x" ? "ew-resize" : "default";
      handle.element.hidden = mode !== "x";
      handle.value(x, LO, HI, "x = " + x.toFixed(1) + " cm");
      labels();
      positionHandle();
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
    function move(e) {
      var v = Charts.ecPointer(chart, el, e, [LO, HI]).x;
      x = Math.max(LO, Math.min(HI, Math.round(v / SNAP) * SNAP));
      p = phi((x - MU) / SD); shown = false; draw();
    }
    el.style.touchAction = "none";
    function startDrag(e) {
      if (mode !== "x") return;
      drag = true; el.setPointerCapture(e.pointerId); move(e); e.preventDefault();
    }
    Charts.listen(el, "pointerdown", startDrag);
    Charts.listen(el, "pointermove", function (e) { if (drag) move(e); });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(el, t, function () { drag = false; }); });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-quant", init);
})();
