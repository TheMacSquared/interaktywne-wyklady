// Rozkład statystyki testowej N(0, 1) z zacienionymi ogonami (ECharts, lcCharts).
//  - Ryc. 2.4 „Test jedno- i dwustronny” (ch2_hipotezy.R): obszar odrzucenia,
//    wartość krytyczna z serwera (komunikat ws_sided_chart).
//  - Ryc. 3.3 „p-wartość jako pole ogonów” (ch1_logika.R): oba ogony od |stat|.
// Kontener: .ws-chart-wrap[data-ws-id][data-ws-type="sided"|"pvalue"]
//   [data-sided][data-crit] albo [data-stat], z <div class="lc-ec">. Atrybuty
// dają stan początkowy, więc wykres rysuje się też bez serwera (wersja offline);
// komunikaty z R go aktualizują. window.wsRenderWhenReady zostaje dla mostka
// offline (build/widgets/05-ch2-hipotezy.js).
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;
  var latest = {}, live = {};

  function pdf(x) { return Math.exp(-0.5 * x * x) / Math.sqrt(2 * Math.PI); }

  // Granice ogonów: sided = two.sided | greater | less, crit z serwera.
  function tails(msg) {
    var sided = msg.sided || "two.sided", c = Number(msg.crit);
    if (msg.type === "pvalue") { sided = "two.sided"; c = Math.abs(Number(msg.stat)); }
    return {
      low: sided === "two.sided" ? -c : sided === "less" ? c : null,
      high: sided === "two.sided" ? c : sided === "greater" ? c : null
    };
  }

  function curve(from, to) {
    var out = [];
    for (var i = 0; i <= 120; i++) { var x = from + (to - from) * i / 120; out.push([x, pdf(x)]); }
    return out;
  }

  function draw(w, msg) {
    var t = tails(msg);
    w.chart.setOption({
      xAxis: { name: msg.xLabel || "Statystyka testowa" },
      series: [
        { id: "low", data: t.low === null ? [] : curve(-4, t.low) },
        { id: "high", data: t.high === null ? [] : curve(t.high, 4) },
        { id: "critLow", data: t.low === null ? [] : [[t.low, 0], [t.low, pdf(t.low)]] },
        { id: "critHigh", data: t.high === null ? [] : [[t.high, 0], [t.high, pdf(t.high)]] }
      ]
    });
  }

  function line(id, width, dash) {
    return { id: id, type: "line", data: [], symbol: "none", silent: true,
             lineStyle: { width: width, type: dash ? [6, 5] : "solid" } };
  }

  Charts.onReady(".ws-chart-wrap", function (root) {
    var el = root.querySelector(".lc-ec");
    if (!el) return false;
    var id = root.dataset.wsId, d = root.dataset;
    var chart = Charts.chart(el, {
      animationDurationUpdate: 650, animationEasingUpdate: "cubicOut", aria: { enabled: false },
      grid: { left: 8, right: 12, top: 10, bottom: 8 },
      xAxis: { type: "value", min: -4, max: 4, interval: 1, name: "Statystyka testowa" },
      yAxis: { type: "value", min: 0, max: 0.43, splitNumber: 4 },
      series: [
        Object.assign(line("low", 0), { areaStyle: {} }),
        Object.assign(line("high", 0), { areaStyle: {} }),
        Object.assign(line("pdf", 3), { data: curve(-4, 4) }),
        line("critLow", 2, true),
        line("critHigh", 2, true)
      ]
    });
    var w = { chart: chart };
    live[id] = w;
    Charts.onDispose(root, function () { if (live[id] === w) delete live[id]; });

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var y = Charts.ecAxis(P, size, { nameGap: 0 });
      y.axisLabel.showMaxLabel = false;
      y.axisLabel.formatter = function (v) { return Charts.fmt(v, 1); };
      chart.setOption({
        xAxis: Charts.ecAxis(P, size, { nameGap: Math.round(size * 1.9) }), yAxis: y,
        series: [
          { id: "low", areaStyle: { color: alpha(P.accent, 0.3) } },
          { id: "high", areaStyle: { color: alpha(P.accent, 0.3) } },
          { id: "pdf", lineStyle: { color: P.niebo } },
          { id: "critLow", lineStyle: { color: P.accent } },
          { id: "critHigh", lineStyle: { color: P.accent } }
        ]
      });
    }
    paint();
    Charts.onTheme(root, paint);
    var initial = { id: id, type: d.wsType || "sided", sided: d.sided, crit: d.crit, stat: d.stat };
    draw(w, latest[id] || initial);
  });

  // Ostatni stan każdego wykresu przeżywa zmianę rozdziału.
  window.wsRenderWhenReady = function (type, msg) {
    latest[msg.id] = Object.assign({ type: type }, msg);
    if (live[msg.id]) draw(live[msg.id], latest[msg.id]);
    return true;
  };
  Charts.onMessage("ws_sided_chart", function (msg) { window.wsRenderWhenReady("sided", msg); });
  Charts.onMessage("ws_pvalue_chart", function (msg) { window.wsRenderWhenReady("pvalue", msg); });
})();
