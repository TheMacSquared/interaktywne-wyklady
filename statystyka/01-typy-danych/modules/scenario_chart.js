// Ryc. 2.3: wykres kołowy i słupkowy (ECharts) w trzech scenariuszach, bez serwera.
// Kontener .lc-scenario z data-config = { input, scenarios: [{ labels, data, colors,
// order }] } oraz <div class="lc-ec" id="ch2_pie_canvas"> i id="ch2_bar_canvas">.
// Scenariusz wybiera segment (radio o nazwie `input`); treści scenariuszy przełącza
// lc_variants(). Słupki, wartości i werdykty ([data-scn-reveal]) pojawiają się po
// kliknięciu [data-scn-reveal-button]; zmiana scenariusza zaczyna rundę od nowa
// (schowane wartości, karty z powrotem w puli).
// Ocena ułożenia: window.lcS01Scenario.hits() (testy: tests/js/scenario.test.js).
(function () {
  // Liczba pozycji obsadzonych właściwą kartą. assignment: { pos1: "C", … },
  // order: identyfikatory kart od najmniejszego udziału do największego.
  function hits(assignment, order) {
    var n = 0;
    order.forEach(function (id, i) { if (assignment["pos" + (i + 1)] === id) n += 1; });
    return n;
  }
  (typeof window !== "undefined" ? window : globalThis).lcS01Scenario = { hits: hits };
  if (typeof document === "undefined") return;

  var Charts = window.lcCharts;
  var current = null;

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

  function scenarioIndex(root, cfg) {
    var checked = root.querySelector('input[name="' + cfg.input + '"]:checked');
    var i = checked ? Number(checked.value) - 1 : 0;
    return i >= 0 && i < cfg.scenarios.length ? i : 0;
  }

  // Stan widoku: scenariusz z segmentu, odsłonięcie z current.revealed.
  function apply(root) {
    if (!current || current.root !== root) return;
    var cfg = current.cfg, s = cfg.scenarios[scenarioIndex(root, cfg)];
    render(root, { labels: s.labels, data: s.data, colors: s.colors, reveal: current.revealed });
    root.querySelectorAll("[data-scn-reveal]").forEach(function (el) { el.hidden = !current.revealed; });
    root.querySelectorAll("[data-scn-hide]").forEach(function (el) { el.hidden = current.revealed; });
  }

  // Ułożenie kart w widocznym scenariuszu: { pos1: "C", … } (puste pola pominięte).
  function assignment(root, i) {
    var out = {};
    var dm = root.querySelector('[data-lc-variant="' + (i + 1) + '"] .lc-dm');
    if (!dm) return out;
    dm.querySelectorAll("[data-lc-dm-zone]").forEach(function (zone) {
      var card = zone.querySelector("[data-lc-dm-item]");
      if (card) out[zone.getAttribute("data-lc-dm-zone")] = card.getAttribute("data-lc-dm-item");
    });
    return out;
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
    current = { root: root, pie: pie, bar: bar, el: { pie: ePie, bar: eBar },
      cfg: JSON.parse(root.getAttribute("data-config")), revealed: false };
    Charts.zoomControl(root);
    Charts.onTheme(root, function () { paint(root); });
    Charts.onDispose(root, function () { current = null; });
    paint(root);
    eBar.style.visibility = "hidden";

    // Zmiana scenariusza: nowa runda.
    root.addEventListener("change", function (e) {
      if (!current || e.target.name !== current.cfg.input) return;
      current.revealed = false;
      root.querySelectorAll("[data-lc-dm-reset]").forEach(function (b) { b.click(); });
      apply(root);
    });
    // Odsłonięcie: liczba trafień zamrożona w tej chwili.
    root.addEventListener("click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-scn-reveal-button]");
      if (!btn || !current) return;
      var i = scenarioIndex(root, current.cfg);
      var n = hits(assignment(root, i), current.cfg.scenarios[i].order);
      root.querySelectorAll('[data-lc-variant="' + (i + 1) + '"] [data-scn-hits]').forEach(function (el) {
        el.textContent = String(n);
      });
      current.revealed = true;
      apply(root);
    });
    apply(root);
  });
})();
