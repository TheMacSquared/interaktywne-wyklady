// „Kto bardziej wyjątkowy?” (Ryc. 5.5). Widok „raw”: dwa małe wykresy gęstości
// w oryginalnych skalach z wartością x do przeciągania; widok „z”: wspólna oś
// z = (x − μ)/σ z obiema wartościami ([data-d-view]). Pary z data-config (JSON z R): [{short, q, a, b}],
// gdzie a/b = {lab, grp, x, mu, sd, unit, step}. Kontener .lc-duel z <canvas
// data-role="a" | "b" | "z">, przyciskami [data-d-pair] i odczytami [data-role="la|lb|za|zb|q"].
// Wzorzec jak area_chart.js (Ryc. 4.3).
(function () {
  var Charts = window.lcCharts;
  var token = Charts.token, alpha = Charts.alpha;
  var Z_LO = -4, Z_HI = 7;

  function pdf(v, mu, sd) { var z = (v - mu) / sd; return Math.exp(-z * z / 2) / (sd * Math.sqrt(2 * Math.PI)); }
  function decs(step) { return step >= 1 ? 0 : step >= 0.1 ? 1 : 2; }

  function baseChart(canvas, title, plugins) {
    return Charts.create(canvas, {
      type: "line",
      plugins: plugins || [],
      data: { datasets: [
        { data: [], fill: "origin", borderWidth: 2 },
        { data: [], borderWidth: 2.5 },
        { data: [], borderWidth: 2.5 },
        { data: [], pointRadius: 7, pointHoverRadius: 7, borderWidth: 2, showLine: false }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 4, right: 8 } },
        scales: { x: { type: "linear", title: { display: true, text: title } }, y: { min: 0, display: false } },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });
  }

  function init(root) {
    var cv = { a: root.querySelector('canvas[data-role="a"]'), b: root.querySelector('canvas[data-role="b"]'),
               z: root.querySelector('canvas[data-role="z"]') };
    if (!cv.a || !cv.b || !cv.z) return false;
    var PAIRS = JSON.parse(root.dataset.config || "[]"), cur = 0, S = {}, view = "raw";
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
    function syncHandle(k, chart) {
      if (!S[k]) return;
      var s = S[k];
      handles[k].element.hidden = view !== "raw";
      handles[k].element.setAttribute("aria-label", s.o.lab + ": " + s.o.grp);
      handles[k].value(s.x, s.lo, s.hi, s.x.toFixed(decs(s.o.step)) + " " + s.o.unit);
      handles[k].position(chart.scales.x.getPixelForValue(s.x), chart.scales.y.getPixelForValue(pdf(s.o.mu, s.o.mu, s.o.sd) * 1.05));
    }
    function plugins(k) { return [{ id: "duelHandle", afterRender: function (chart) { syncHandle(k, chart); }, beforeLayout: function (chart) {
      chart.options.scales.x.ticks.maxTicksLimit = Math.max(3, Math.min(6, Math.floor((chart.width || 640) / (Charts.axisFontSize(root) * 3.5))));
      if (S[k]) chart.options.scales.x.title.text = Charts.axisTitle(S[k].o.grp + " (" + S[k].o.unit + ")", chart.width, Charts.axisFontSize(root));
    } }]; }
    var ch = { a: baseChart(cv.a, "", plugins("a")), b: baseChart(cv.b, "", plugins("b")), z: baseChart(cv.z, "Wspólna skala: z") };
    var read = {};
    ["la", "lb", "za", "zb", "q"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });
    var COL = {};

    function paint() {
      var ink = token("--upwr-ink-soft") || "#444", surface = token("--upwr-surface") || "#fff";
      COL = { a: token("--upwr-accent") || "#6b1a2a", b: token("--upwr-cat-indygo") || "#3a5a8a" };
      Object.keys(ch).forEach(function (k) {
        Charts.styleAxes(ch[k], null, { includeBounds: false, fontSize: Charts.axisFontSize(root), maxRotation: 0 });
        ch[k].options.scales.x.ticks.maxTicksLimit = 6;
        ch[k].options.layout.padding.left = ch[k].options.layout.padding.right = Math.ceil(Charts.axisFontSize(root) * 1.6);
        var ds = ch[k].data.datasets;
        ds[0].borderColor = ink; ds[0].backgroundColor = alpha(token("--upwr-rule") || "#d9cfbc", 0.5);
        ds[3].pointBorderColor = ds[3].pointHoverBorderColor = surface;
      });
      ["a", "b"].forEach(function (k) {
        var ds = ch[k].data.datasets;
        ds[1].borderColor = COL[k];
        ds[3].pointBackgroundColor = ds[3].pointHoverBackgroundColor = COL[k];
      });
      ch.z.data.datasets[1].borderColor = COL.a;
      ch.z.data.datasets[2].borderColor = COL.b;
      ch.z.data.datasets[3].pointBackgroundColor = ch.z.data.datasets[3].pointHoverBackgroundColor = [COL.a, COL.b];
      Object.keys(ch).forEach(function (k) { ch[k].update(); });
    }

    function setPair(i) {
      cur = i; var pr = PAIRS[i];
      ["a", "b"].forEach(function (k) {
        var o = pr[k], lo = Math.min(o.mu - 4 * o.sd, o.x - o.sd), hi = Math.max(o.mu + 4 * o.sd, o.x + o.sd);
        S[k] = { o: o, x: o.x, lo: lo, hi: hi };
        var c = ch[k], xs = [];
        for (var v = lo; v <= hi + 1e-9; v += (hi - lo) / 300) xs.push(v);
        c.options.scales.x.min = lo; c.options.scales.x.max = hi;
        c.options.scales.x.title.text = o.grp + " (" + o.unit + ")";
        c.options.scales.y.max = pdf(o.mu, o.mu, o.sd) * 1.2;
        c.data.datasets[0].data = xs.map(function (v) { return { x: v, y: pdf(v, o.mu, o.sd) }; });
      });
      var zs = [];
      for (var z = Z_LO; z <= Z_HI + 1e-9; z += 0.025) zs.push(z);
      ch.z.options.scales.x.min = Z_LO; ch.z.options.scales.x.max = Z_HI;
      ch.z.options.scales.x.ticks.stepSize = 1;
      ch.z.options.scales.y.max = pdf(0, 0, 1) * 1.2;
      ch.z.data.datasets[0].data = zs.map(function (v) { return { x: v, y: pdf(v, 0, 1) }; });
      if (read.la) read.la.textContent = pr.a.lab;
      if (read.lb) read.lb.textContent = pr.b.lab;
      if (read.q) read.q.textContent = pr.q;
      draw();
    }

    function draw() {
      var zv = {};
      ["a", "b"].forEach(function (k) {
        var s = S[k], o = s.o, c = ch[k], top = pdf(o.mu, o.mu, o.sd) * 1.05;
        c.data.datasets[1].data = [{ x: s.x, y: 0 }, { x: s.x, y: top }];
        c.data.datasets[3].data = [{ x: s.x, y: top }];
        c.update("none");
        syncHandle(k, c);
        zv[k] = (s.x - o.mu) / o.sd;
      });
      var zt = pdf(0, 0, 1) * 1.05;
      ch.z.data.datasets[1].data = [{ x: Math.max(Z_LO, Math.min(zv.a, Z_HI)), y: 0 }, { x: Math.max(Z_LO, Math.min(zv.a, Z_HI)), y: zt }];
      ch.z.data.datasets[2].data = [{ x: Math.max(Z_LO, Math.min(zv.b, Z_HI)), y: 0 }, { x: Math.max(Z_LO, Math.min(zv.b, Z_HI)), y: zt * 0.8 }];
      ch.z.data.datasets[3].data = [{ x: Math.max(Z_LO, Math.min(zv.a, Z_HI)), y: zt }, { x: Math.max(Z_LO, Math.min(zv.b, Z_HI)), y: zt * 0.8 }];
      ch.z.update("none");
      ["a", "b"].forEach(function (k) {
        var s = S[k], el = read["z" + k];
        if (el) el.textContent = view === "z" ? "z = " + zv[k].toFixed(2) : s.x.toFixed(decs(s.o.step)) + " " + s.o.unit;
      });
    }

    // Przeciąganie wartości x na małych wykresach.
    ["a", "b"].forEach(function (k) {
      var c = ch[k], canvas = cv[k], drag = false;
      function move(e) {
        var rc = canvas.getBoundingClientRect(), area = c.chartArea, s = S[k];
        var px = Math.max(area.left, Math.min(area.right, e.clientX - rc.left));
        var v = Math.round(c.scales.x.getValueForPixel(px) / s.o.step) * s.o.step;
        s.x = Math.max(s.lo, Math.min(s.hi, v)); draw();
      }
      canvas.style.touchAction = "none"; canvas.style.cursor = "ew-resize";
      starters[k] = function (e) { drag = true; canvas.setPointerCapture(e.pointerId); move(e); e.preventDefault(); };
      Charts.listen(canvas, "pointerdown", starters[k]);
      Charts.listen(canvas, "pointermove", function (e) { if (drag) move(e); });
      ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(canvas, t, function () { drag = false; }); });
    });

    // Widok: dwie skale oryginalne albo jedna skala z.
    function setView(v) {
      view = v;
      if (boxTop) boxTop.hidden = v !== "raw";
      if (boxZ) boxZ.hidden = v !== "z";
      Object.keys(ch).forEach(function (k) { ch[k].resize(); });
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
