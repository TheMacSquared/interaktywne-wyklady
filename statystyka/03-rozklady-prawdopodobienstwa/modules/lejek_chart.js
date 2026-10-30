// Ryc. 1.2 „Lejek”: częstość szóstek w seriach rzutów na tle pasa 1/6 ± 2·SE.
// Chart.js: pas i linia 1/6 są stałe, każda seria to osobna linia. Nowa seria
// rysuje się punkt po punkcie (jak kolejne rzuty), starsze zostają w tle.
// Serwer wysyła komunikat "lejek_series": {series: [[...], ...]}, gdzie każda
// seria to częstości w punktach siatki NS (ta sama co s3p_11c_nn w R).
// Kontener: .lc-lejek z <canvas data-role="lejek">.
(function () {
  var P = 1 / 6, N_MIN = 1, N_MAX = 1000, Y_MAX = 0.6;
  var FIRST_SERIES = 3;  // indeksy datasetów: 0 dolny pas, 1 górny pas, 2 linia 1/6
  var FRAME_MS = 6;      // odstęp między punktami nowej serii

  // Punkty n, w których rysujemy serie (log-równomiernie, bez powtórzeń).
  var NS = [];
  for (var i = 0; i <= 199; i++) {
    var v = Math.round(Math.exp(i / 199 * Math.log(N_MAX)));
    if (NS.indexOf(v) < 0) NS.push(v);
  }

  function token(name) {
    return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
  }

  function alpha(hex, a) {
    var h = hex.replace("#", "");
    var n = parseInt(h, 16);
    return "rgba(" + (n >> 16 & 255) + "," + (n >> 8 & 255) + "," + (n & 255) + "," + a + ")";
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
    return new Chart(canvas, {
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
    var subtle = token("--upwr-ink-subtle") || "#888";
    var rule = token("--upwr-rule-soft") || "#ddd";
    var accent = token("--upwr-accent") || "#7a2e3b";
    var secondary = token("--upwr-secondary") || "#6a9dc4";
    var font = token("--upwr-mono") || "monospace";
    var ch = w.chart;
    ["x", "y"].forEach(function (k) {
      var s = ch.options.scales[k];
      s.grid = { color: rule };
      s.border = { color: rule };
      s.ticks = Object.assign(s.ticks || {}, { color: subtle, font: { family: font, size: 12 } });
      s.title.color = subtle;
      s.title.font = { family: font, size: 12 };
    });
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

  function render(w, msg) {
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
      ch.data.datasets.push({ data: i === series.length - 1 ? [] : newPoints, pointRadius: 0 });
    }

    paint(w);
    ch.update("none");
    if (newIdx >= 0) reveal(w, newIdx, newPoints);
  }

  // Wykres tworzymy od razu, żeby pas był widoczny przed pierwszym rzutem.
  var w = null;

  function scan() {
    if (w || typeof Chart === "undefined") return;
    var canvas = document.querySelector('.lc-lejek canvas[data-role="lejek"]');
    if (!canvas) return;
    w = { chart: create(canvas), timer: null, pending: null };
    paint(w);
    w.chart.update("none");
    new MutationObserver(function () { paint(w); w.chart.update("none"); })
      .observe(document.documentElement, { attributes: true, attributeFilter: ["data-lc-theme"] });
  }

  function onMessage(msg) {
    scan();
    if (w) render(w, msg);
  }

  var registered = false;
  function register() {
    if (registered || !window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    registered = true;
    window.Shiny.addCustomMessageHandler("lejek_series", onMessage);
  }

  // Skrypt wykonuje się w <head>, więc kontener i Shiny mogą pojawić się później:
  // sprawdzamy, aż oba są gotowe (albo do ok. 10 s).
  var tries = 0;
  var timer = setInterval(function () {
    scan(); register();
    if ((w && registered) || ++tries > 100) clearInterval(timer);
  }, 100);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener("shiny:connected", register);
})();
