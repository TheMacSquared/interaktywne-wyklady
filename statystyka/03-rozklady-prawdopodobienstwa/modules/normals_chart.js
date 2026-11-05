// Dwie krzywe normalne (Ryc. 5.3): wzrost kobiet i mężczyzn. Każda krzywa ma
// uchwyt na szczycie: ruch w bok zmienia μ, w górę i w dół σ (wyższy szczyt to
// węższa krzywa, bo pole zawsze wynosi 1). Kontener:
// .lc-normals[data-mu1][data-sd1][data-mu2][data-sd2][data-lo][data-hi]
// z <div class="lc-ec" data-role="pdf">, przyciskiem [data-n-reset] i odczytami
// [data-role="n1"], [data-role="n2"], [data-role="a"], [data-role="b"].
// ECharts, wzorzec jak area_chart.js (Ryc. 4.3).
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;
  var SD_MIN = 4, SD_MAX = 20;

  var phi = Charts.normalCdf;

  function pdf(v, mu, sd) { var z = (v - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }

  function init(root) {
    var el = root.querySelector('.lc-ec[data-role="pdf"]');
    if (!el) return false;

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

    var drag = -1;
    var layer = Charts.handleLayer(root);
    var handles = G.map(function (g, i) {
      return layer.add(el, {
        role: "button", label: ["Kobiety", "Mężczyźni"][i],
        hint: "Lewo i prawo zmieniają średnią. Góra zmniejsza odchylenie i podnosi szczyt, dół zwiększa odchylenie. Shift zwiększa krok dziesięciokrotnie.",
        onKey: function (event) {
          var horizontal = ["ArrowLeft", "ArrowRight"].indexOf(event.key) >= 0;
          if (!horizontal && ["ArrowUp", "ArrowDown"].indexOf(event.key) < 0) return false;
          var key = horizontal ? "mu" : "sd", step = (horizontal ? 0.5 : 0.1) * (event.shiftKey ? 10 : 1);
          var direction = ["ArrowRight", "ArrowDown"].indexOf(event.key) >= 0 ? 1 : -1;
          G[i][key] = +Math.max(horizontal ? LO + 5 : SD_MIN, Math.min(horizontal ? HI - 5 : SD_MAX, G[i][key] + direction * step)).toFixed(1);
          draw(); layer.announce(handles[i].element.getAttribute("aria-label")); return true;
        },
        onPointerDown: function (event) { drag = i; el.setPointerCapture(event.pointerId); event.preventDefault(); }
      });
    });
    function peak(g) { return chart.convertToPixel({ gridIndex: 0 }, [g.mu, pdf(g.mu, g.mu, g.sd)]); }
    function syncHandles() {
      handles.forEach(function (handle, i) {
        var g = G[i], q = peak(g);
        if (q) handle.position(q[0], q[1]);
        handle.element.setAttribute("aria-label", ["Kobiety", "Mężczyźni"][i] + ". Średnia " + num(g.mu) + " cm, odchylenie " + num(g.sd) + " cm");
      });
    }
    function curve(id) {
      return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
               lineStyle: { width: 2.5 }, areaStyle: {} };
    }
    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 12, top: 14, bottom: 8 },
      xAxis: { type: "value", min: LO, max: HI, interval: 10, name: "Wzrost (cm)" },
      yAxis: { type: "value", min: 0, max: TOP, interval: 0.02, name: "Gęstość" },
      series: [curve("g0"), curve("g1"),
        { id: "dots", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false }]
    }, { onResize: syncHandles });

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var cols = [P.niebo, P.terakota];
      var y = Charts.ecAxis(P, size, { nameGap: Math.round(size * 3.2) });
      y.axisLabel.formatter = function (v) { return Charts.fmt(v, 2); };
      // Kraniec 215 nie leży na podziałce co 10 — bez etykiety nachodziłby na 210.
      var x = Charts.ecAxis(P, size);
      x.axisLabel.showMaxLabel = false;
      chart.setOption({
        xAxis: x,
        yAxis: y,
        series: [0, 1].map(function (i) {
          return { id: "g" + i, lineStyle: { color: cols[i] }, areaStyle: { color: alpha(cols[i], 0.18) } };
        }).concat([{ id: "dots", itemStyle: {
          color: function (d) { return cols[d.dataIndex]; }, borderColor: P.surface, borderWidth: 2 } }])
      });
      syncHandles();
    }

    function pct(x) { return (100 * x).toFixed(1) + "%"; }
    function num(x) { return x.toFixed(1); }
    function draw() {
      chart.setOption({ series: [0, 1].map(function (i) {
        return { id: "g" + i, data: XS.map(function (v) { return [v, pdf(v, G[i].mu, G[i].sd)]; }) };
      }).concat([{ id: "dots", data: G.map(function (g) { return [g.mu, pdf(g.mu, g.mu, g.sd)]; }) }]) });
      syncHandles();
      if (read.n1) read.n1.textContent = "N(" + num(G[0].mu) + ", " + num(G[0].sd) + ")";
      if (read.n2) read.n2.textContent = "N(" + num(G[1].mu) + ", " + num(G[1].sd) + ")";
      // kobiety wyższe niż średni mężczyzna; mężczyźni niżsi niż średnia kobieta
      if (read.a) read.a.textContent = pct(1 - phi((G[1].mu - G[0].mu) / G[0].sd));
      if (read.b) read.b.textContent = pct(phi((G[0].mu - G[1].mu) / G[1].sd));
    }

    // Przeciąganie uchwytu: wciśnięcie łapie bliższy szczyt (w pikselach).
    function nearest(p) {
      var best = -1, bd = Infinity;
      G.forEach(function (g, i) {
        var q = peak(g), dx = q[0] - p.px, dy = q[1] - p.py, d = Math.sqrt(dx * dx + dy * dy);
        if (d < bd) { bd = d; best = i; }
      });
      return { i: best, d: bd };
    }
    function move(e) {
      var p = Charts.ecPointer(chart, el, e, [LO, HI], [0, TOP]);
      G[drag].mu = Math.max(LO + 5, Math.min(HI - 5, Math.round(p.x * 2) / 2));
      var sd = 1 / (Math.max(p.y, 1e-6) * Math.sqrt(2 * Math.PI));
      G[drag].sd = Math.max(SD_MIN, Math.min(SD_MAX, Math.round(sd * 10) / 10));
      draw();
    }
    el.style.touchAction = "none";
    Charts.listen(el, "pointerdown", function (e) {
      var n = nearest(Charts.ecPointer(chart, el, e));
      if (n.i < 0 || n.d > 60) return;
      drag = n.i; el.setPointerCapture(e.pointerId); move(e); e.preventDefault();
    });
    Charts.listen(el, "pointermove", function (e) {
      if (drag >= 0) { move(e); return; }
      el.style.cursor = nearest(Charts.ecPointer(chart, el, e)).d < 60 ? "move" : "default";
    });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(el, t, function () { drag = -1; }); });

    Charts.listen(root, "click", function (e) {
      if (!e.target.closest("[data-n-reset]")) return;
      G = START.map(function (g) { return { mu: g.mu, sd: g.sd }; }); draw();
    });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-normals", init);
})();
