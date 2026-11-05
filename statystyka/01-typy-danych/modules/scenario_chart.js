// Ryc. 2.3: wykres kołowy i słupkowy (ECharts). Scenariusz przychodzi z serwera
// (komunikat render_scenario: {labels, data, colors, reveal}); ostatni przeżywa
// zmianę rozdziału. Słupki i wartości pojawiają się dopiero po odsłonięciu.
// Kontener .lc-scenario z <div class="lc-ec" id="ch2_pie_canvas"> i
// <div class="lc-ec" id="ch2_bar_canvas">.
(function () {
  var Charts = window.lcCharts;
  var latest = null, current = null;

  // ECharts nie rozumie var() ani color-mix(): kolor liczy przeglądarka.
  function resolve(css) {
    if (!/var\(|color-mix/.test(css)) return css;
    var probe = document.createElement("span");
    probe.style.color = css; probe.style.display = "none";
    document.body.appendChild(probe);
    var c = getComputedStyle(probe).color;
    probe.remove();
    return c;
  }

  function render(root, msg) {
    var colors = (msg.colors || []).map(resolve);
    current.pie.setOption({
      tooltip: { show: !!msg.reveal, trigger: "item", formatter: function (p) { return p.name + ": " + p.value + "%"; } },
      series: [{ id: "pie", data: msg.labels.map(function (l, i) {
        return { name: l, value: msg.data[i], itemStyle: { color: colors[i] } };
      }) }]
    });
    current.bar.setOption({
      yAxis: { data: msg.reveal ? msg.labels : [] },
      series: [{ id: "bar", data: msg.reveal ? msg.data.map(function (v, i) {
        return { value: v, itemStyle: { color: colors[i] } };
      }) : [] }]
    });
    current.el.bar.style.visibility = msg.reveal ? "" : "hidden";
  }

  // Zmiany wyglądu nie odtwarzają scenariusza ani nie odsłaniają odpowiedzi.
  function paint(root) {
    if (!current || current.root !== root) return;
    var P = Charts.palette(), fs = Charts.axisFontSize(root);
    var x = Charts.ecAxis(P, fs, { splitNumber: 4 });
    x.axisLabel.formatter = function (v) { return v + "%"; };
    var y = Charts.ecAxis(P, fs * 1.08);
    y.splitLine = { show: false };
    current.bar.setOption({ xAxis: x, yAxis: y });
    current.pie.setOption({ series: [{ id: "pie", itemStyle: { borderColor: Charts.alpha(P.surface, 0.6) } }] });
  }

  Charts.onReady(".lc-scenario", function (root) {
    var ePie = root.querySelector("#ch2_pie_canvas"), eBar = root.querySelector("#ch2_bar_canvas");
    if (!ePie || !eBar) return false;
    var pie = Charts.chart(ePie, {
      animationDuration: 500, animationDurationUpdate: 500, aria: { enabled: false },
      tooltip: { show: false },
      series: [{ id: "pie", type: "pie", radius: ["0%", "88%"], label: { show: false }, labelLine: { show: false },
                 itemStyle: { borderWidth: 1 }, emphasis: { scale: false }, data: [] }]
    });
    var bar = Charts.chart(eBar, {
      animationDuration: 500, animationDurationUpdate: 500, aria: { enabled: false },
      grid: { left: 8, right: 16, top: 8, bottom: 8 },
      tooltip: { trigger: "item", formatter: function (p) { return p.value + "%"; } },
      xAxis: { type: "value", min: 0, max: 50 },
      yAxis: { type: "category", data: [], inverse: true },
      series: [{ id: "bar", type: "bar", data: [], barWidth: "70%", itemStyle: { borderRadius: 4 } }]
    });
    current = { root: root, pie: pie, bar: bar, el: { pie: ePie, bar: eBar } };
    Charts.zoomControl(root);
    Charts.onTheme(root, function () { paint(root); });
    Charts.onDispose(root, function () { current = null; });
    paint(root);
    eBar.style.visibility = "hidden";
    if (latest) render(root, latest);
  });

  Charts.onMessage("render_scenario", function (msg) {
    latest = msg;
    if (current && current.root.isConnected) render(current.root, msg);
  });
})();
