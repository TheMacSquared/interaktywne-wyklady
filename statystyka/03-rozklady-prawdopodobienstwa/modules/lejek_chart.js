// Ryc. 1.2 „Lejek”: częstość szóstek w seriach rzutów na tle pasa 1/6 ± 2·SE.
// Chart.js: pas i linia 1/6 są stałe, każda seria to osobna linia. Nowa seria
// rysuje się punkt po punkcie (jak kolejne rzuty), starsze zostają w tle.
// Serwer wysyła komunikat "lejek_series": {series: [[...], ...]}, gdzie każda
// seria to częstości w punktach siatki NS (ta sama co s3p_11c_nn w R).
// Kontener: .lc-lejek z <canvas data-role="lejek">.
(function () {
  var Charts = window.lcCharts;
  var token = Charts.token, alpha = Charts.alpha;
  var P = 1 / 6, N_MIN = 1, N_MAX = 1000, Y_MAX = 0.6;
  var FIRST_SERIES = 3;  // indeksy datasetów: 0 dolny pas, 1 górny pas, 2 linia 1/6
  var FRAME_MS = 6;      // odstęp między punktami nowej serii

  // Punkty n, w których rysujemy serie (log-równomiernie, bez powtórzeń).
  var NS = [];
  for (var i = 0; i <= 199; i++) {
    var v = Math.round(Math.exp(i / 199 * Math.log(N_MAX)));
    if (NS.indexOf(v) < 0) NS.push(v);
  }

  function band(ns, sign) {
    return ns.map(function (n) {
      var se = 2 * Math.sqrt(P * (1 - P) / n);
      return { x: n, y: sign < 0 ? Math.max(0, P - se) : P + se };
    });
  }

  function toPoints(f) {
    return f.map(function (y, j) { return { x: NS[j], y: y }; });
  }

  function create(canvas) {
    return Charts.create(canvas, {
      type: "line",
      data: { datasets: [
        { data: band(NS, -1), borderWidth: 0, pointRadius: 0 },
        { data: band(NS, 1), borderWidth: 0, pointRadius: 0, fill: "-1" },
        { data: [{ x: N_MIN, y: P }, { x: N_MAX, y: P }], borderWidth: 1.5,
          borderDash: [4, 4], pointRadius: 0 }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, parsing: false,
        animation: false,
        locale: "pl-PL",
        events: [],
        layout: { padding: { top: 6, right: 8 } },
        scales: {
          x: { type: "logarithmic", min: N_MIN, max: N_MAX,
               title: { display: true, text: "Liczba rzutów w serii (skala log.)" },
               ticks: { callback: function (v) { return v.toLocaleString("pl-PL"); } },
               afterBuildTicks: function (axis) {
                 axis.ticks = axis.ticks.filter(function (t) { return [1, 10, 100, 1000].indexOf(t.value) >= 0; });
               } },
          y: { min: 0, max: Y_MAX,
               title: { display: true, text: "Częstość szóstek" },
               ticks: { stepSize: 0.2, callback: function (v) { return v.toFixed(1); } } }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0, borderWidth: 1 } }
      }
    });
  }

  function paint(w) {
    var ink = token("--upwr-ink-soft") || "#444";
    var accent = token("--upwr-accent") || "#7a2e3b";
    var secondary = token("--upwr-secondary") || "#6a9dc4";
    var ch = w.chart;
    Charts.styleAxes(ch);
    ch.data.datasets[0].backgroundColor = alpha("#cbb858", 0.3);
    ch.data.datasets[1].backgroundColor = alpha("#cbb858", 0.3);
    ch.data.datasets[2].borderColor = ink;
    for (var i = FIRST_SERIES; i < ch.data.datasets.length; i++) {
      var last = i === ch.data.datasets.length - 1;
      ch.data.datasets[i].borderColor = last ? accent : alpha(secondary, 0.35);
      ch.data.datasets[i].borderWidth = last ? 2 : 1;
    }
  }

  // Dorysowuje punkty nowej serii po jednym w krótkich odstępach.
  // Jeśli w międzyczasie przyjdzie kolejna seria, bieżąca dokańcza się od razu.
  function reveal(w, idx, points) {
    var ch = w.chart, k = 0;
    finish(w);
    w.pending = { idx: idx, points: points };
    function tick() {
      k += 1;
      ch.data.datasets[idx].data = points.slice(0, k);
      ch.update("none");
      if (k < points.length) w.timer = setTimeout(tick, FRAME_MS);
      else { w.timer = null; w.pending = null; }
    }
    tick();
  }

  function finish(w) {
    if (w.timer) clearTimeout(w.timer);
    if (w.pending) {
      w.chart.data.datasets[w.pending.idx].data = w.pending.points;
      w.pending = null;
    }
    w.timer = null;
  }

  function render(w, msg, animate) {
    var ch = w.chart, series = msg.series || [];
    var have = ch.data.datasets.length - FIRST_SERIES;

    // Reset: mniej serii niż było — zaczynamy od zera.
    if (series.length < have) {
      finish(w);
      ch.data.datasets.splice(FIRST_SERIES);
      have = 0;
    }

    var newIdx = -1, newPoints = null;
    for (var i = have; i < series.length; i++) {
      newPoints = toPoints(series[i]);
      // Serie dochodzące naraz (np. +10) pojawiają się od razu, animujemy tylko ostatnią.
      newIdx = ch.data.datasets.length;
      ch.data.datasets.push({ data: animate && i === series.length - 1 ? [] : newPoints, pointRadius: 0 });
    }

    paint(w);
    ch.update("none");
    if (animate && newIdx >= 0) reveal(w, newIdx, newPoints);
  }

  // Dane sesji przeżywają usunięcie rozdziału; instancja wykresu nie.
  var latest = null, current = null;

  Charts.onReady(".lc-lejek", function (root) {
    var canvas = root.querySelector('canvas[data-role="lejek"]');
    if (!canvas) return false;
    var w = { chart: create(canvas), timer: null, pending: null };
    current = w;
    Charts.onDispose(root, function () {
      finish(w);
      current = null;
    });
    Charts.onTheme(root, function () { paint(w); w.chart.update("none"); });
    paint(w);
    if (latest) render(w, latest, false);
    else w.chart.update("none");
  });

  Charts.onMessage("lejek_series", function (msg) {
    latest = msg;
    if (current && current.chart.canvas.isConnected) render(current, msg, true);
  });
})();
