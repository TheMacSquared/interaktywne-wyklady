// Ryc. 4.2 „Zero braków” (rozdz. 4): przedziały Walda i Wilsona dla x wadliwych
// sztuk na n sprawdzonych, ufność 95%. Część Walda poniżej zera jest w terakocie,
// reszta w kolorze przedziału; Wilson w szałwii. x zmienia suwak albo przeciąganie
// punktu p̂ na wykresie (strzałki na uchwycie: ±1 sztuka). Kontener:
// .fig-zero[data-z][data-n][data-x][data-xmax] z <div class="lc-ec" data-role="ci">,
// przyciskami [data-zero-n], suwakiem [data-role="x"] i odczytami
// [data-role="phat|wald|wilson|caption|x-out"]. ECharts, wzorzec jak quantile_chart.js (stat. 03).
(function () {
  "use strict";
  var Charts = window.lcCharts;
  if (!Charts || !Charts.chart) return;
  var alpha = Charts.alpha;
  var X_LO = -0.10, Y_WALD = 2, Y_WILSON = 1, Y_MIN = 0, Y_MAX = 3;

  function wald(x, n, z) {
    var p = x / n, me = z * Math.sqrt(p * (1 - p) / n);
    return [p - me, p + me];
  }
  function wilson(x, n, z) {
    var p = x / n, d = 1 + z * z / n, c = (p + z * z / (2 * n)) / d;
    var me = (z / d) * Math.sqrt(p * (1 - p) / n + z * z / (4 * n * n));
    return [c - me, c + me];
  }
  // Procent z jedną cyfrą po kropce; zero bez znaku minus.
  function pct(v) {
    var s = (Math.round(1000 * v) / 10).toFixed(1);
    return (s === "-0.0" ? "0.0" : s) + "%";
  }
  function range(iv) { return "od " + pct(iv[0]) + " do " + pct(iv[1]); }

  function init(root) {
    var el = root.querySelector('.lc-ec[data-role="ci"]');
    var slider = root.querySelector('input[data-role="x"]');
    if (!el || !slider) return false;

    var Z = parseFloat(root.dataset.z || "1.96");
    var XMAX = parseInt(root.dataset.xmax || "15", 10);
    var n = parseInt(root.dataset.n || "50", 10), x = parseInt(root.dataset.x || "0", 10);
    function role(r) { return root.querySelector('[data-role="' + r + '"]'); }
    var out = { x: role("x-out"), phat: role("phat"), wald: role("wald"),
                wilson: role("wilson"), caption: role("caption") };
    function xTop() { return Math.min(XMAX, n); }

    var layer = Charts.handleLayer(root);
    var handle = layer.add(el, {
      label: "Liczba wadliwych sztuk x",
      hint: "Strzałki zmieniają x o 1 sztukę (Shift: o 5).",
      onKey: function (event) {
        var v = Charts.keyValue(event, x, 0, xTop(), 1, 5);
        if (v === null) return false;
        setX(v);
        layer.announce(out.caption ? out.caption.textContent : "");
        return true;
      },
      onPointerDown: function (event) { startDrag(event); }
    });

    function seg(id, width) {
      return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
               lineStyle: { width: width, cap: "butt" } };
    }
    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 16, top: 12, bottom: 8 },
      xAxis: { type: "value", min: X_LO, max: 0.4, interval: 0.1, name: "Odsetek wadliwych sztuk" },
      yAxis: { type: "value", min: Y_MIN, max: Y_MAX, interval: 1 },
      series: [
        Object.assign(seg("below", 0), { data: [[X_LO, Y_MAX], [0, Y_MAX]], areaStyle: { origin: "start" } }),
        seg("zero", 1.5),
        seg("waldNeg", 12), seg("waldPos", 12), seg("wilson", 12),
        { id: "dot", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false, z: 5 }
      ]
    }, { onResize: function () { paint(); } });

    function positionHandle() {
      var q = chart.convertToPixel({ gridIndex: 0 }, [x / n, Y_WALD]);
      if (q) handle.position(q[0], q[1]);
    }

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var xa = Charts.ecAxis(P, size, { nameGap: Math.round(size * 2.2) });
      xa.axisLabel.formatter = function (v) { return Math.round(100 * v) + "%"; };
      xa.name = Charts.ecName("Odsetek wadliwych sztuk", Math.max(250, el.clientWidth - 40), size);
      var ya = Charts.ecAxis(P, size, { splitLine: { show: false }, axisTick: { show: false } });
      ya.axisLabel.formatter = function (v) { return v === Y_WALD ? "Wald" : v === Y_WILSON ? "Wilson" : ""; };
      var bar = Math.max(10, Math.round(size * 0.95));
      chart.setOption({
        grid: { bottom: Math.round(size * 0.6) },
        xAxis: xa, yAxis: ya,
        series: [
          { id: "below", areaStyle: { color: alpha(P.ref, 0.16) } },
          { id: "zero", lineStyle: { color: P.secondary } },
          { id: "waldNeg", lineStyle: { color: P.terakota, width: bar } },
          { id: "waldPos", lineStyle: { color: P.niebo, width: bar } },
          { id: "wilson", lineStyle: { color: P.szalwia, width: bar } },
          { id: "dot", symbolSize: Math.round(bar * 1.35),
            itemStyle: { color: P.bursztyn, borderColor: P.surface, borderWidth: 2 } }
        ]
      });
      positionHandle();
    }

    function draw() {
      var p = x / n, w = wald(x, n, Z), l = wilson(x, n, Z);
      var hi = Math.max(0.4, Math.ceil(Math.max(w[1], l[1]) * 10 - 1e-9) / 10);
      var waldNeg = w[0] < 0 ? [[w[0], Y_WALD], [Math.min(w[1], 0), Y_WALD]] : [];
      var waldPos = w[1] > Math.max(w[0], 0) ? [[Math.max(w[0], 0), Y_WALD], [w[1], Y_WALD]] : [];
      chart.setOption({
        xAxis: { max: hi },
        series: [
          { id: "zero", data: [[0, Y_MIN], [0, Y_MAX]] },
          { id: "waldNeg", data: waldNeg },
          { id: "waldPos", data: waldPos },
          { id: "wilson", data: [[Math.max(l[0], 0), Y_WILSON], [l[1], Y_WILSON]] },
          { id: "dot", data: [[p, Y_WALD], [p, Y_WILSON]] }
        ]
      });
      var msg = x === 0 ? "Wald: przedział o zerowej szerokości — jakby braków na pewno nie było."
        : w[0] < 0 ? "Wald: dolna granica poniżej zera, czyli przedział wychodzi poza możliwe odsetki."
        : "Wald: dolna granica jeszcze nie wychodzi poniżej zera.";
      if (out.x) out.x.textContent = String(x);
      if (out.phat) out.phat.textContent = pct(p);
      if (out.wald) out.wald.textContent = range(w);
      if (out.wilson) out.wilson.textContent = range(l);
      if (out.caption) out.caption.textContent = msg;
      slider.value = String(x);
      el.setAttribute("aria-label", "Przedziały 95% dla " + x + " wadliwych na " + n +
        " sztuk: Wald " + range(w) + ", Wilson " + range(l) + ".");
      el.style.cursor = "ew-resize";
      handle.value(x, 0, xTop(), x + " z " + n + " sztuk, p̂ = " + pct(p));
      positionHandle();
    }

    function setX(v) { x = Math.max(0, Math.min(xTop(), Math.round(v))); draw(); }

    Charts.listen(slider, "input", function () { setX(parseInt(slider.value, 10)); });
    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-zero-n]");
      if (!b) return;
      n = parseInt(b.getAttribute("data-zero-n"), 10);
      root.querySelectorAll("[data-zero-n]").forEach(function (o) {
        o.setAttribute("aria-pressed", o === b ? "true" : "false");
      });
      setX(x);
    });

    // Przeciąganie punktu p̂: kursor na osi odsetka → najbliższa liczba sztuk x.
    var drag = false;
    function move(e) {
      var v = Charts.ecPointer(chart, el, e).x;
      if (isFinite(v)) setX(v * n);
    }
    el.style.touchAction = "none";
    function startDrag(e) { drag = true; el.setPointerCapture(e.pointerId); move(e); e.preventDefault(); }
    Charts.listen(el, "pointerdown", startDrag);
    Charts.listen(el, "pointermove", function (e) { if (drag) move(e); });
    ["pointerup", "pointercancel"].forEach(function (t) {
      Charts.listen(el, t, function () {
        if (drag && out.caption) layer.announce(out.caption.textContent);
        drag = false;
      });
    });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, paint);
  }

  Charts.onReady(".fig-zero", init);
})();
