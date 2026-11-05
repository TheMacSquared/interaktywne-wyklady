// „Kto bardziej wyjątkowy?” (Ryc. 5.5). Widok „raw”: dwa małe wykresy gęstości
// w oryginalnych skalach z wartością x do przeciągania; widok „z”: wspólna oś
// z = (x − μ)/σ z obiema wartościami ([data-d-view]). Pary z data-config (JSON z R): [{short, q, a, b}],
// gdzie a/b = {lab, grp, x, mu, sd, unit, step}. Kontener .lc-duel z <div class="lc-ec"
// data-role="a" | "b" | "z">, przyciskami [data-d-pair] i odczytami [data-role="la|lb|za|zb|q"].
// ECharts, wzorzec jak area_chart.js (Ryc. 4.3).
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;
  var Z_LO = -4, Z_HI = 7;

  function pdf(v, mu, sd) { var z = (v - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }
  function decs(step) { return step >= 1 ? 0 : step >= 0.1 ? 1 : 2; }

  function line(id, width) {
    return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
             lineStyle: { width: width } };
  }

  function baseChart(el, name, onResize) {
    return Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 24, right: 24, top: 8, bottom: 8, outerBoundsMode: "none" },
      xAxis: { type: "value", name: name },
      yAxis: { type: "value", min: 0, show: false },
      series: [
        Object.assign(line("pdf", 2), { areaStyle: {} }),
        line("ma", 2.5),
        line("mb", 2.5),
        { id: "dots", type: "scatter", data: [], symbolSize: 14, silent: true, animation: false }
      ]
    }, { onResize: onResize });
  }

  function init(root) {
    var cv = { a: root.querySelector('.lc-ec[data-role="a"]'), b: root.querySelector('.lc-ec[data-role="b"]'),
               z: root.querySelector('.lc-ec[data-role="z"]') };
    if (!cv.a || !cv.b || !cv.z) return false;
    var PAIRS = JSON.parse(root.dataset.config || "[]"), S = {}, view = "raw";
    var boxTop = root.querySelector(".lc-duel-top"), boxZ = root.querySelector(".lc-duel-z");
    var layer = Charts.handleLayer(root), handles = {}, starters = {};
    ["a", "b"].forEach(function (k) {
      handles[k] = layer.add(cv[k], {
        label: "Wartość " + k,
        hint: "Strzałki przesuwają wartość, Shift zwiększa krok dziesięciokrotnie. Home i End wybierają granice osi.",
        onKey: function (event) {
          if (view !== "raw") return false;
          var s = S[k], value = Charts.keyValue(event, s.x, s.lo, s.hi, s.o.step, 10 * s.o.step);
          if (value === null) return false;
          s.x = +value.toFixed(4); draw();
          layer.announce(s.o.lab + ": " + s.x.toFixed(decs(s.o.step)) + " " + s.o.unit + ". Z " + ((s.x - s.o.mu) / s.o.sd).toFixed(2)); return true;
        },
        onPointerDown: function (event) { starters[k](event); }
      });
    });
    function syncHandle(k) {
      if (!S[k]) return;
      var s = S[k];
      handles[k].element.hidden = view !== "raw";
      handles[k].element.setAttribute("aria-label", s.o.lab + ": " + s.o.grp);
      handles[k].value(s.x, s.lo, s.hi, s.x.toFixed(decs(s.o.step)) + " " + s.o.unit);
      var q = ch[k].convertToPixel({ gridIndex: 0 }, [s.x, pdf(s.o.mu, s.o.mu, s.o.sd) * 1.05]);
      if (q) handles[k].position(q[0], q[1]);
    }
    var ch = {};
    ch.a = baseChart(cv.a, "", function () { layout("a"); syncHandle("a"); });
    ch.b = baseChart(cv.b, "", function () { layout("b"); syncHandle("b"); });
    ch.z = baseChart(cv.z, "Wspólna skala: z", function () { layout("z"); });
    var read = {};
    ["la", "lb", "za", "zb", "q"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });

    // Margines, liczba podziałek i zawinięta nazwa osi według szerokości wykresu.
    function layout(k) {
      var size = Charts.axisFontSize(root), width = ch[k].getWidth ? ch[k].getWidth() : 640;
      var text = k === "z" ? "Wspólna skala: z" : (S[k] ? S[k].o.grp + " (" + S[k].o.unit + ")" : "");
      var lines = Charts.axisTitle(text, width, size).length, pad = Math.ceil(size * 1.6);
      var axis = { name: Charts.ecName(text, width, size) };
      if (k !== "z") axis.splitNumber = Math.max(3, Math.min(6, Math.floor(width / (size * 3.5))));
      ch[k].setOption({ grid: { left: pad, right: pad, bottom: Math.ceil(size * (2.4 + 1.3 * lines)) }, xAxis: axis });
    }

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var COL = { a: P.accent, b: Charts.token("--upwr-cat-indygo", "#3a5a8a") };
      Object.keys(ch).forEach(function (k) {
        var x = Charts.ecAxis(P, size, { nameGap: Math.round(size * 1.9), nameTextStyle: {
          color: P.subtle, fontFamily: P.mono, fontSize: size, verticalAlign: "top", lineHeight: size * 1.3 } });
        x.axisLabel.showMinLabel = x.axisLabel.showMaxLabel = false;
        var series = [{ id: "pdf", lineStyle: { color: P.soft }, areaStyle: { color: alpha(P.rule, 0.5) } }];
        if (k === "z") {
          series.push({ id: "ma", lineStyle: { color: COL.a } }, { id: "mb", lineStyle: { color: COL.b } },
            { id: "dots", itemStyle: { color: function (d) { return d.dataIndex === 0 ? COL.a : COL.b; },
                                       borderColor: P.surface, borderWidth: 2 } });
        } else {
          series.push({ id: "ma", lineStyle: { color: COL[k] } },
            { id: "dots", itemStyle: { color: COL[k], borderColor: P.surface, borderWidth: 2 } });
        }
        ch[k].setOption({ xAxis: x, series: series });
        layout(k);
      });
      syncHandle("a"); syncHandle("b");
    }

    function setPair(i) {
      var pr = PAIRS[i];
      ["a", "b"].forEach(function (k) {
        var o = pr[k], lo = Math.min(o.mu - 4 * o.sd, o.x - o.sd), hi = Math.max(o.mu + 4 * o.sd, o.x + o.sd);
        S[k] = { o: o, x: o.x, lo: lo, hi: hi };
        var xs = [];
        for (var v = lo; v <= hi + 1e-9; v += (hi - lo) / 300) xs.push(v);
        ch[k].setOption({
          xAxis: { min: lo, max: hi },
          yAxis: { max: pdf(o.mu, o.mu, o.sd) * 1.2 },
          series: [{ id: "pdf", data: xs.map(function (v) { return [v, pdf(v, o.mu, o.sd)]; }) }]
        });
        layout(k);
      });
      var zs = [];
      for (var z = Z_LO; z <= Z_HI + 1e-9; z += 0.025) zs.push(z);
      ch.z.setOption({
        xAxis: { min: Z_LO, max: Z_HI, interval: 1 },
        yAxis: { max: pdf(0, 0, 1) * 1.2 },
        series: [{ id: "pdf", data: zs.map(function (v) { return [v, pdf(v, 0, 1)]; }) }]
      });
      if (read.la) read.la.textContent = pr.a.lab;
      if (read.lb) read.lb.textContent = pr.b.lab;
      if (read.q) read.q.textContent = pr.q;
      draw();
    }

    function clampZ(v) { return Math.max(Z_LO, Math.min(v, Z_HI)); }
    function draw() {
      var zv = {};
      ["a", "b"].forEach(function (k) {
        var s = S[k], o = s.o, top = pdf(o.mu, o.mu, o.sd) * 1.05;
        ch[k].setOption({ series: [
          { id: "ma", data: [[s.x, 0], [s.x, top]] },
          { id: "dots", data: [[s.x, top]] }
        ] });
        syncHandle(k);
        zv[k] = (s.x - o.mu) / o.sd;
      });
      var zt = pdf(0, 0, 1) * 1.05, za = clampZ(zv.a), zb = clampZ(zv.b);
      ch.z.setOption({ series: [
        { id: "ma", data: [[za, 0], [za, zt]] },
        { id: "mb", data: [[zb, 0], [zb, zt * 0.8]] },
        { id: "dots", data: [[za, zt], [zb, zt * 0.8]] }
      ] });
      ["a", "b"].forEach(function (k) {
        var s = S[k], el = read["z" + k];
        if (el) el.textContent = view === "z" ? "z = " + zv[k].toFixed(2) : s.x.toFixed(decs(s.o.step)) + " " + s.o.unit;
      });
    }

    // Przeciąganie wartości x na małych wykresach.
    ["a", "b"].forEach(function (k) {
      var el = cv[k], drag = false;
      function move(e) {
        var s = S[k], v = Charts.ecPointer(ch[k], el, e, [s.lo, s.hi]).x;
        v = Math.round(v / s.o.step) * s.o.step;
        s.x = Math.max(s.lo, Math.min(s.hi, v)); draw();
      }
      el.style.touchAction = "none"; el.style.cursor = "ew-resize";
      starters[k] = function (e) { drag = true; el.setPointerCapture(e.pointerId); move(e); e.preventDefault(); };
      Charts.listen(el, "pointerdown", starters[k]);
      Charts.listen(el, "pointermove", function (e) { if (drag) move(e); });
      ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(el, t, function () { drag = false; }); });
    });

    // Widok: dwie skale oryginalne albo jedna skala z. Obserwator rozmiaru
    // dopasowuje wykres, który właśnie stał się widoczny.
    function setView(v) {
      view = v;
      if (boxTop) boxTop.hidden = v !== "raw";
      if (boxZ) boxZ.hidden = v !== "z";
      draw();
    }

    Charts.listen(root, "click", function (e) {
      var w = e.target.closest("[data-d-view]");
      if (w) {
        root.querySelectorAll("[data-d-view]").forEach(function (x) { x.setAttribute("aria-pressed", x === w ? "true" : "false"); });
        setView(w.getAttribute("data-d-view"));
        return;
      }
      var b = e.target.closest("[data-d-pair]");
      if (!b) return;
      root.querySelectorAll("[data-d-pair]").forEach(function (x) { x.setAttribute("aria-pressed", x === b ? "true" : "false"); });
      setPair(Number(b.getAttribute("data-d-pair")));
    });

    Charts.zoomControl(root);
    paint();
    setPair(0);
    setView("raw");
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".lc-duel", init);
})();
