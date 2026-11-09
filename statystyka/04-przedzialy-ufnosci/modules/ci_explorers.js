// Eksploratory CI (Ryc. 2.2, 2.4, 3.4, 3.5) w ECharts SVG. Dane prób i kwantyle t
// przychodzą z R w data-config (ci_explorers.R); tu tylko marginesy i rysunek.
// Kontener: .ci-explorer[data-kind][data-config] z .lc-ec, suwakami [data-ex-input],
// przyciskami [data-ex-action] i odczytem .ci-explorer-read.
(function () {
  "use strict";
  var C = window.lcCharts;
  if (!C || !C.chart) return;
  function fixed(v, d) { return C.fmt(v, d === undefined ? 2 : d); }
  function tick(v) { return String(Math.round(v * 1000) / 1000); }

  // Rama osi dla każdego rodzaju: zakresy są wielokrotnościami podziałki,
  // więc skrajne etykiety są zawsze okrągłymi liczbami.
  var FRAMES = {
    coverage:   { x: [156, 184], dx: 4, y: [0, 31], name: "Średni wzrost (cm)" },
    people:     { x: [130, 210], dx: 10, y: [-0.45, 1.08], name: "Wzrost (cm)" },
    difference: { x: [-0.6, 1.2], dx: 0.2, y: [0.4, 3.6], name: "Odchylenie od 9.08 lub różnica (g / 100 g)",
                  labels: { 3: "Linia 1", 2: "Linia 2", 1: "Różnica" } },
    useful:     { x: [-12, 16], dx: 4, y: [0.5, 1.5], name: "Oszczędność czasu: stara metoda − nowa (min)" }
  };

  C.onReady(".ci-explorer", function (root) {
    var el = root.querySelector(".lc-ec"), read = root.querySelector(".ci-explorer-read");
    if (!el || !read) return false;
    var cfg = JSON.parse(root.dataset.config), kind = root.dataset.kind, F = FRAMES[kind];
    if (!F) return false;
    var state = { batch: 0 }, inputs = {};
    root.querySelectorAll("[data-ex-input]").forEach(function (input) { inputs[input.dataset.exInput] = input; });
    function val(key) { return Number(inputs[key].value); }
    function set(key, value) { inputs[key].value = value; }
    function say(text) { read.textContent = text; el.setAttribute("aria-label", text); }

    var chart = C.chart(el, { animation: false }, { onResize: function () { draw(); } });

    // ---- Elementy rysunku ---------------------------------------------------
    function seg(lo, hi, y, color, width) {
      return { type: "line", data: [[lo, y], [hi, y]], symbol: "none", silent: true, z: 3,
               lineStyle: { color: color, width: width, cap: "butt" } };
    }
    function pts(data, color, size) {
      return { type: "scatter", data: data, symbolSize: size, silent: true, z: 4,
               itemStyle: { color: color } };
    }
    function ref(x, color) {
      return { type: "line", data: [[x, F.y[0]], [x, F.y[1]]], symbol: "none", silent: true, z: 2,
               lineStyle: { color: color, width: 1.2, type: [5, 5] } };
    }
    function band(lo, hi, color) {
      return { type: "line", data: [[lo, F.y[1]], [hi, F.y[1]]], symbol: "none", silent: true, z: 1,
               lineStyle: { width: 0 }, areaStyle: { color: color, origin: "start" } };
    }
    function label(x, y, text, color, fs) {
      return { type: "scatter", data: [[x, y]], symbolSize: 0, silent: true, z: 5,
               label: { show: true, position: "right", formatter: text, color: color,
                        fontSize: fs, fontFamily: C.token("--upwr-sans", "sans-serif") } };
    }

    function render(series, P, fs) {
      var W = el.clientWidth || 640, labels = F.labels;
      var xa = C.ecAxis(P, fs, { type: "value", min: F.x[0], max: F.x[1], interval: F.dx,
        name: C.ecName(F.name, Math.max(250, W - 40), fs), nameGap: Math.round(fs * 2.2) });
      xa.axisLabel.formatter = tick;
      var ya = C.ecAxis(P, fs, { type: "value", min: F.y[0], max: F.y[1], show: !!labels,
        interval: 1, splitLine: { show: false }, axisTick: { show: false }, axisLine: { show: false } });
      // Etykiety wierszy stoją na y = 1, 2, 3; podziałki od krańca osi (0.4) by je minęły.
      if (labels) ya.axisLabel.customValues = Object.keys(labels).map(Number);
      ya.axisLabel.formatter = function (v) { return labels && labels[Math.round(v)] || ""; };
      ya.axisLabel.fontFamily = C.token("--upwr-sans", "sans-serif");
      chart.setOption({
        animation: false, aria: { enabled: false }, tooltip: { show: false },
        grid: { left: 12, right: 20, top: 16, bottom: Math.round(fs * 0.6) },
        xAxis: xa, yAxis: ya, series: series
      }, true);
    }

    // ---- Uchwyt średniej linii 1 (Ryc. 3.4) ---------------------------------
    var layer, handle, dragging = false;
    function syncHandle() {
      if (!handle) return;
      var d = val("d"), q = chart.convertToPixel({ gridIndex: 0 }, [d, 3]);
      handle.value(d, 0, 0.8, fixed(d, 2) + " g / 100 g");
      if (q) handle.position(q[0], q[1]);
    }
    if (kind === "difference") {
      layer = C.handleLayer(root);
      handle = layer.add(el, {
        label: "Średnia linii 1",
        hint: "Strzałki zmieniają różnicę o 0.01 g/100 g; Shift o 0.1.",
        onKey: function (event) {
          var v = C.keyValue(event, val("d"), 0, 0.8, 0.01, 0.1);
          if (v === null) return false;
          set("d", fixed(v, 2)); draw(); layer.announce(read.textContent);
          return true;
        },
        onPointerDown: function (event) { startDrag(event); }
      });
      var move = function (event) {
        if (!dragging) return;
        var v = C.ecPointer(chart, el, event, [0, 0.8]).x;
        if (isFinite(v)) { set("d", fixed(v, 2)); draw(); }
      };
      var startDrag = function (event) {
        dragging = true;
        if (el.setPointerCapture) el.setPointerCapture(event.pointerId);
        move(event);
      };
      el.style.touchAction = "none";
      C.listen(el, "pointerdown", function (event) {
        var row = chart.convertToPixel({ gridIndex: 0 }, [0, 3]);
        if (row && Math.abs(C.ecPointer(chart, el, event).py - row[1]) < 28) startDrag(event);
      });
      C.listen(el, "pointermove", move);
      C.listen(el, "pointerup", function () { if (dragging) layer.announce(read.textContent); dragging = false; });
      C.listen(el, "pointercancel", function () { dragging = false; });
    }

    var map = null;
    if (kind === "coverage") {
      map = root.querySelector(".ci-explorer-map");
      if (map && !map.children.length) for (var m = 0; m < 100; m++) map.appendChild(document.createElement("span"));
    }

    // ---- Rysunek ------------------------------------------------------------
    function draw() {
      if (chart.isDisposed && chart.isDisposed()) return;
      var P = C.palette(), fs = C.axisFontSize(root), w = Math.max(1, fs / 14), s = [], lo, hi;
      if (kind === "coverage") {
        var rows = cfg.groups[state.batch % cfg.groups.length], tq = cfg.tq[val("level") - 80], hits = 0;
        s.push(ref(170, P.ink));
        rows.forEach(function (r, k) {
          var me = tq * r.s / 5, hit = r.xbar - me <= 170 && r.xbar + me >= 170;
          hits += hit ? 1 : 0;
          if (map && map.children[k]) map.children[k].classList.toggle("is-miss", !hit);
          if (k >= 70) s.push(seg(r.xbar - me, r.xbar + me, k - 69, hit ? P.szalwia : P.terakota, 3 * w));
        });
        if (map) map.setAttribute("aria-label", hits + " trafień na 100 badań przy ufności " + val("level") + "%");
        say("Ufność " + val("level") + "% · trafia " + hits + " / 100 · kolor szałwiowy = obejmuje μ, terakota = chybia");
      } else if (kind === "people") {
        var curve = [], area = [], i, h;
        for (i = 130; i <= 210; i += 0.25) {
          h = Math.exp(-0.5 * Math.pow((i - 170) / 10, 2));
          curve.push([i, h]);
          if (Math.abs(i - 170) <= 19.6) area.push([i, h]);
        }
        var me = cfg.z * 10 / Math.sqrt(val("n"));
        s.push({ type: "line", data: area, symbol: "none", silent: true, z: 1,
                 lineStyle: { width: 0 }, areaStyle: { color: C.alpha(P.niebo, 0.22), origin: 0 } });
        s.push({ type: "line", data: curve, symbol: "none", silent: true, z: 2,
                 lineStyle: { color: P.secondary, width: 2 } });
        s.push(ref(170, P.ink));
        s.push(seg(150.4, 189.6, -0.12, P.secondary, 4 * w), label(190.6, -0.12, "95% wzrostów", P.secondary, fs * 0.85));
        s.push(seg(170 - me, 170 + me, -0.32, P.accent, 7 * w), pts([[170, -0.32]], P.accent, 10 * w),
               label(170 + me + 1, -0.32, "95% CI średniej", P.accent, fs * 0.85));
        say("n = " + val("n") + " · pas wzrostów: 39.2 cm · CI średniej: " + fixed(2 * me, 2) +
            " cm · ludzie pozostają tak samo zróżnicowani");
      } else if (kind === "difference") {
        var d = val("d"), a = cfg.margins[0], b = cfg.margins[1], dm = cfg.diff_margin;
        lo = Math.max(d - a, -b); hi = Math.min(d + a, b);
        if (lo < hi) s.push(band(lo, hi, C.alpha(P.bursztyn, 0.16)));
        s.push(ref(0, P.ink));
        s.push(seg(d - a, d + a, 3, P.niebo, 5 * w), seg(-b, b, 2, P.secondary, 5 * w), seg(d - dm, d + dm, 1, P.accent, 5 * w));
        s.push(pts([[d, 3], [0, 2], [d, 1]], P.bursztyn, 10 * w));
        say("Różnica " + fixed(d, 2) + " · 95% CI [" + fixed(d - dm, 2) + ", " + fixed(d + dm, 2) + "] · " +
            (d <= dm ? "CI różnicy obejmuje zero" : d <= a + b ?
              "CI grup nakładają się, ale CI różnicy już nie obejmuje zera" : "CI grup są rozłączne"));
      } else if (kind === "useful") {
        var n = val("n"), effect = val("effect"), margin = cfg.tq[n - 20] * 10 * Math.sqrt(2 / n);
        lo = effect - margin; hi = effect + margin;
        s.push(band(-2, 2, C.alpha(P.szalwia, 0.14)), ref(0, P.ink), ref(2, P.ink));
        s.push(seg(lo, hi, 1, P.niebo, 7 * w), pts([[effect, 1]], P.accent, 12 * w));
        var stat = lo > 0 ? "kierunek dodatni" : hi < 0 ? "kierunek ujemny" : "kierunek nierozstrzygnięty";
        var practical = lo > 2 ? "oszczędność przekracza 2 min" : hi < -2 ? "nowa metoda wydłuża czas o ponad 2 min" :
          lo >= -2 && hi <= 2 ? "cały CI w pasie zmian mniejszych niż 2 min" : "użyteczność nierozstrzygnięta";
        say("95% CI [" + fixed(lo, 2) + ", " + fixed(hi, 2) + "] min · " + stat + " · " + practical);
      }
      render(s, P, fs);
      Object.keys(inputs).forEach(function (k) {
        var output = root.querySelector('[data-value="' + k + '"]');
        if (output) output.textContent = inputs[k].value;
      });
      syncHandle();
    }

    Object.keys(inputs).forEach(function (k) { C.listen(inputs[k], "input", draw); });
    C.listen(root, "click", function (e) {
      var btn = e.target.closest("[data-ex-action]");
      if (!btn) return;
      var a = btn.dataset.exAction;
      if (kind === "coverage" && a === "new") state.batch++;
      if (kind === "people") set("n", Number(a.slice(1)));
      if (kind === "difference") set("d", a === "near" ? 0.10 : a === "trap" ? 0.30 : 0.60);
      if (kind === "useful") {
        set("effect", a === "tiny" ? 0.4 : a === "uncertain" ? 3 : 4);
        set("n", a === "tiny" ? 1000 : a === "uncertain" ? 20 : 400);
      }
      draw();
    });
    C.zoomControl(root);
    C.onTheme(root, draw);
    draw();
  });
})();
