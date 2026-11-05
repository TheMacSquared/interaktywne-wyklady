// Quiz (rozdz. 7): przykładowa realizacja po odpowiedzi. Kontener .lc-quizex
// z data-config z R (quiz_example_config w ch8_quiz.R): etykiety przedziałów,
// odsetki 500 losowań (emp) i prawdopodobieństwa teoretyczne (theo).
// Słupki: odsetki z próby; punkty (dyskretne) albo linia (ciągłe): model.
// Kontener powstaje w renderUI przy każdej odpowiedzi; lcCharts sprząta poprzedni.
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha, fmt = Charts.fmt;

  Charts.onReady(".lc-quizex", function (root) {
    var el = root.querySelector('.lc-ec[data-role="hist"]');
    if (!el) return false;
    var C = JSON.parse(root.dataset.config || "{}");
    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 12, top: 14, bottom: 8 },
      xAxis: { type: "category", data: C.labels, name: C.x },
      yAxis: { type: "value", min: 0, name: "Odsetek wyników" },
      series: [
        { id: "emp", type: "bar", data: C.emp, barCategoryGap: C.perValue ? "25%" : "4%", silent: true },
        C.discrete
          ? { id: "theo", type: "scatter", data: C.theo, symbolSize: 9, silent: true, z: 5 }
          : { id: "theo", type: "line", data: C.theo, symbol: "none", smooth: 0.3, silent: true, lineStyle: { width: 2.5 }, z: 5 }
      ]
    });

    function paint() {
      var P = Charts.palette(), size = Charts.axisFontSize(root);
      var x = Charts.ecAxis(P, size, { nameGap: Math.round(size * 1.9) });
      x.splitLine = { show: false };
      x.axisLabel.hideOverlap = true;
      var y = Charts.ecAxis(P, size, { nameGap: Math.round(size * 3.4) });
      y.axisLabel.formatter = function (v) { return fmt(100 * v, 0) + "%"; };
      y.splitNumber = 4;
      chart.setOption({
        xAxis: x, yAxis: y,
        series: [
          { id: "emp", itemStyle: { color: alpha(P.niebo, 0.55), borderColor: P.niebo, borderWidth: 1 } },
          C.discrete
            ? { id: "theo", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 1.5 } }
            : { id: "theo", lineStyle: { color: P.accent } }
        ]
      });
    }
    paint();
    Charts.onTheme(root, paint);
  });
})();
