// Ryc. 1.2 „Lejek”: częstość szóstek w seriach rzutów na tle pasa 1/6 ± 2·SE.
// ECharts (renderer canvas — serii przybywa z każdym kliknięciem): pas i linia
// 1/6 są stałe, starsze serie leżą w tle jako jedna seria „lines”, najnowsza
// rysuje się punkt po punkcie (jak kolejne rzuty).
// Serwer wysyła komunikat "lejek_series": {series: [[...], ...]}, gdzie każda
// seria to częstości w punktach siatki NS (ta sama co s3p_11c_nn w R).
// Kontener: .lc-lejek z <div class="lc-ec" data-role="lejek">.
(function () {
  var Charts = window.lcCharts;
  var alpha = Charts.alpha;
  var P = 1 / 6, N_MIN = 1, N_MAX = 1000, Y_MAX = 0.6;
  var FRAME_MS = 6;      // odstęp między punktami nowej serii
  var BAND = "#cbb858";
  var TITLE = "Liczba rzutów w serii (skala log.)";

  // Punkty n, w których rysujemy serie (log-równomiernie, bez powtórzeń).
  var NS = [];
  for (var i = 0; i <= 199; i++) {
    var v = Math.round(Math.exp(i / 199 * Math.log(N_MAX)));
    if (NS.indexOf(v) < 0) NS.push(v);
  }

  function se(n) { return 2 * Math.sqrt(P * (1 - P) / n); }
  var LOWER = NS.map(function (n) { return [n, Math.max(0, P - se(n))]; });
  var WIDTH = NS.map(function (n) { return [n, P + se(n) - Math.max(0, P - se(n))]; });

  function toPoints(f) {
    return f.map(function (y, j) { return [NS[j], y]; });
  }

  function create(el, root) {
    return Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 12, top: 10, bottom: 8 },
      xAxis: { type: "log", logBase: 10, min: N_MIN, max: N_MAX, name: TITLE,
               axisLabel: { formatter: function (v) { return String(v); } } },
      yAxis: { type: "value", min: 0, max: Y_MAX, interval: 0.2, name: "Częstość szóstek",
               axisLabel: { formatter: function (v) { return v.toFixed(1); } } },
      series: [
        // Pas: dolna krawędź (niewidoczna) + szerokość pasa nałożona stosem.
        { id: "bandLow", type: "line", data: LOWER, stack: "band", symbol: "none", silent: true,
          lineStyle: { width: 0 }, animation: false },
        { id: "bandWidth", type: "line", data: WIDTH, stack: "band", symbol: "none", silent: true,
          lineStyle: { width: 0 }, areaStyle: {}, animation: false },
        { id: "p", type: "line", data: [[N_MIN, P], [N_MAX, P]], symbol: "none", silent: true,
          lineStyle: { width: 1.5, type: [4, 4] }, animation: false },
        { id: "old", type: "lines", coordinateSystem: "cartesian2d", polyline: true, data: [],
          silent: true, animation: false, lineStyle: { width: 1 } },
        { id: "last", type: "line", data: [], symbol: "none", silent: true, animation: false,
          lineStyle: { width: 2 } }
      ]
    }, { renderer: "canvas", onResize: function () { title(root, w0()); } });
  }

  var current = null;
  function w0() { return current; }

  function title(root, w) {
    if (!w) return;
    var size = Charts.axisFontSize(root), width = w.chart.getWidth ? w.chart.getWidth() : 640;
    w.chart.setOption({ xAxis: { name: Charts.ecName(TITLE, width, size) } });
  }

  function paint(w) {
    var C = Charts.palette(), size = Charts.axisFontSize(w.root);
    var x = Charts.ecAxis(C, size), y = Charts.ecAxis(C, size, { nameGap: Math.round(size * 2.6) });
    x.axisLabel.formatter = function (v) { return String(v); };
    y.axisLabel.formatter = function (v) { return v.toFixed(1); };
    w.chart.setOption({
      xAxis: x, yAxis: y,
      series: [
        { id: "bandWidth", areaStyle: { color: alpha(BAND, 0.3) } },
        { id: "p", lineStyle: { color: C.soft } },
        { id: "old", lineStyle: { color: alpha(C.secondary, 0.35) } },
        { id: "last", lineStyle: { color: C.accent } }
      ]
    });
    title(w.root, w);
  }

  function show(w) {
    var all = w.series, n = all.length;
    w.chart.setOption({ series: [
      { id: "old", data: all.slice(0, Math.max(0, n - 1)).map(function (s) { return { coords: s }; }) },
      { id: "last", data: n ? (w.pending ? w.pending.shown : all[n - 1]) : [] }
    ] });
  }

  // Dorysowuje punkty nowej serii po jednym w krótkich odstępach.
  // Jeśli w międzyczasie przyjdzie kolejna seria, bieżąca dokańcza się od razu.
  function reveal(w, points) {
    var k = 0;
    finish(w);
    w.pending = { shown: [] };
    function tick() {
      k += 1;
      w.pending.shown = points.slice(0, k);
      show(w);
      if (k < points.length) w.timer = setTimeout(tick, FRAME_MS);
      else { w.timer = null; w.pending = null; }
    }
    tick();
  }

  function finish(w) {
    if (w.timer) clearTimeout(w.timer);
    w.timer = null;
    if (w.pending) { w.pending = null; show(w); }
  }

  function render(w, msg, animate) {
    var series = (msg.series || []).map(toPoints);
    var grew = series.length > w.series.length;
    finish(w);
    w.series = series;
    if (animate && grew && series.length) reveal(w, series[series.length - 1]);
    else show(w);
  }

  // Dane sesji przeżywają usunięcie rozdziału; instancja wykresu nie.
  var latest = null;

  Charts.onReady(".lc-lejek", function (root) {
    var el = root.querySelector('.lc-ec[data-role="lejek"]');
    if (!el) return false;
    var w = { root: root, el: el, chart: null, timer: null, pending: null, series: [] };
    current = w;
    w.chart = create(el, root);
    Charts.zoomControl(root);
    Charts.onDispose(root, function () {
      if (w.timer) clearTimeout(w.timer);
      w.timer = null; w.pending = null;
      if (current === w) current = null;
    });
    Charts.onTheme(root, function () { paint(w); });
    paint(w);
    if (latest) render(w, latest, false);
  });

  Charts.onMessage("lejek_series", function (msg) {
    latest = msg;
    if (current && current.el.isConnected) render(current, msg, true);
  });
})();
