// Plan na stałym p a dwaj dostawcy (scalone P11 i P11B).
// Kontener .lc-mix[data-r][data-p][data-d][data-n] z <div class="lc-ec" data-role="pmf">,
// przyciskami [data-mix-d] i [data-mix-reset] oraz odczytami
// [data-role="n|pconst|pmix|q95"] i [data-role="split"]. Połowa partii ma
// p − d, połowa p + d. Pole: rozkład łącznej liczby kontroli w mieszance;
// przerywany kontur: stałe p. Kreska planu ma uchwyt (przeciąganie, strzałki),
// druga kreska to prawdziwy kwantyl 95% mieszanki. Kreska 95% i rozbicie na
// dostawców są schowane za przełącznikami [data-mix-show="showQ|showSplit"].
// ECharts (od 2026-11-14); rachunek w window.lcD5Mix (tests/js/d5_charts.test.js).
(function (root) {
  function pmfArr(r, p, xmax) {
    var out = new Array(xmax + 1).fill(0), P = Math.pow(p, r);
    for (var x = r; x <= xmax; x++) { out[x] = P; P *= x / (x - r + 1) * (1 - p); }
    return out;
  }
  function cdf(arr, n) { var F = 0; for (var x = 0; x <= n && x < arr.length; x++) F += arr[x]; return F; }
  function quantile(arr, a) { var F = 0; for (var x = 0; x < arr.length; x++) { F += arr[x]; if (F >= a) return x; } return arr.length; }
  function model(r, p, d, calc) {
    var A = pmfArr(r, p - d, calc), B = pmfArr(r, p + d, calc);
    var mix = A.map(function (v, i) { return 0.5 * v + 0.5 * B[i]; });
    return { A: A, B: B, mix: mix, q95: quantile(mix, 0.95) };
  }
  root.lcD5Mix = { pmfArr: pmfArr, cdf: cdf, quantile: quantile, model: model };

  var Charts = root.lcCharts;
  if (!Charts || typeof document === "undefined") return;

  function init(box) {
    var el = box.querySelector('.lc-ec[data-role="pmf"]');
    if (!el) return false;
    var r = parseInt(box.dataset.r || "3", 10), p = parseFloat(box.dataset.p || "0.10");
    var START = { d: parseFloat(box.dataset.d || "0.05"), n: parseInt(box.dataset.n || "61", 10) };
    var S = { d: START.d, n: START.n, showQ: false, showSplit: false }, XMAX = 180, CALC = 3000;
    var base = pmfArr(r, p, CALC), YMAX = Math.max.apply(null, base) * 1.35, ymax = YMAX;
    var read = {};
    ["n", "pconst", "pmix", "q95", "split"].forEach(function (k) { read[k] = box.querySelector('[data-role="' + k + '"]'); });
    var P = Charts.palette(), cur = null;

    var drag = false;
    var layer = Charts.handleLayer(box);
    var handle = layer.add(el, {
      label: "Plan kontroli",
      hint: "Strzałki w prawo i w lewo zmieniają plan o jedną kontrolę, z Shift o dziesięć.",
      onKey: function (event) {
        var v = Charts.keyValue(event, S.n, r, XMAX, 1, 10);
        if (v === null) return false;
        S.n = v; draw(); layer.announce(handle.element.getAttribute("aria-valuetext")); return true;
      },
      onPointerDown: function (event) { drag = true; el.setPointerCapture(event.pointerId); event.preventDefault(); }
    });

    function line(id, name) {
      return { id: id, name: name, type: "line", data: [], step: "middle", symbol: "none", silent: true, animation: false };
    }
    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 10, right: 14, top: 34, bottom: 8 },
      legend: { top: 0, right: 0, data: ["dwaj dostawcy", "stałe p = " + Charts.fmt(p, 2)], selectedMode: false },
      xAxis: { type: "value", min: 0, max: XMAX, name: "Łączna liczba kontroli do trzeciej wady" },
      yAxis: { type: "value", min: 0, max: YMAX, show: false },
      series: [line("mix", "dwaj dostawcy"), line("base", "stałe p = " + Charts.fmt(p, 2))]
    }, { onResize: paint });

    function overlay() {
      if (!cur) return;
      var fs = Charts.axisFontSize(box), font = "600 " + fs + "px " + P.mono;
      var lb = chart.convertToPixel({ gridIndex: 0 }, [0, 0]), rt = chart.convertToPixel({ gridIndex: 0 }, [XMAX, ymax]);
      if (!lb || !rt) return;
      var top = rt[1], bottom = lb[1];
      function px(v) { return chart.convertToPixel({ gridIndex: 0 }, [v, 0])[0]; }
      function seg(x, y2, color, width, dash) {
        return { type: "line", silent: true, z: 10, shape: { x1: x, y1: bottom, x2: x, y2: y2 },
                 style: { stroke: color, lineWidth: width, lineDash: dash || null } };
      }
      function text(x, y, t, color, align) {
        return { type: "text", silent: true, z: 11, x: x, y: y,
                 style: { text: t, fill: color, font: font, align: align || "center", verticalAlign: "bottom",
                          backgroundColor: P.surface, padding: [1, 3] } };  // podpis nad kreską, która go przecina
      }
      var xn = px(S.n), els = [seg(xn, top + 22, P.ink, 2, [6, 4]), text(xn, top + 14, "plan " + S.n, P.ink)];
      if (S.showQ) {
        var q = cur.q95, off = q > XMAX, xq = px(Math.min(q, XMAX));
        els.push(seg(xq, top + 44, P.accent, 2.5));
        els.push(text(off ? xq - 4 : xq, top + 36, "95% naprawdę: " + q + (off ? " →" : ""), P.accent, off ? "right" : "center"));
      }
      chart.setOption({ graphic: els }, { replaceMerge: ["graphic"] });
      handle.position(xn, (top + bottom) / 2 + 20);
      handle.value(S.n, r, XMAX, "plan " + S.n + " kontroli, szansa ukończenia " + Charts.fmt(cdf(cur.mix, S.n), 2));
    }

    function paint() {
      P = Charts.palette();
      var fs = Charts.axisFontSize(box), x = Charts.ecAxis(P, fs, { splitNumber: 7 }), w = el.clientWidth || 640;
      x.name = Charts.ecName("Łączna liczba kontroli do trzeciej wady", w - 60, fs);
      x.splitLine = { show: false };
      x.axisLabel.showMaxLabel = false;  // kraniec osi nie leży na podziałce i nachodzi na sąsiada
      chart.setOption({
        grid: { top: 10 + root.lcD5Nb.legendRows(["dwaj dostawcy", "stałe p = 0.10"], w, fs) * Math.round(fs * 1.5) },
        xAxis: x,
        // left: miejsce na przycisk lupy w lewym górnym rogu, gdy legenda się zawija.
        legend: { left: w < 480 ? 30 : "auto", textStyle: { color: P.soft, fontFamily: P.mono, fontSize: fs }, itemWidth: 28, itemHeight: 10,
                  itemStyle: { opacity: 0 } },  // sama linia w ikonie, bez kropki
        series: [
          { id: "mix", lineStyle: { color: P.accent, width: 2 }, itemStyle: { color: P.accent },
            areaStyle: { color: Charts.alpha(P.accent, 0.25) } },
          { id: "base", lineStyle: { color: P.subtle, width: 1.6, type: [5, 4] }, itemStyle: { color: P.subtle } }
        ]
      });
      overlay();
    }

    function series(arr) {
      var out = [];
      for (var x = r; x <= XMAX; x++) out.push([x, arr[x]]);
      return out;
    }

    function draw() {
      cur = model(r, p, S.d, CALC);
      ymax = Math.max(YMAX, Math.max.apply(null, cur.mix.slice(0, XMAX)) * 1.35);
      chart.setOption({ yAxis: { max: ymax }, series: [{ id: "mix", data: series(cur.mix) }, { id: "base", data: series(base) }] });
      overlay();
      var pc = cdf(base, S.n), pm = cdf(cur.mix, S.n), pa = cdf(cur.A, S.n), pb = cdf(cur.B, S.n);
      if (read.n) read.n.textContent = String(S.n);
      if (read.pconst) read.pconst.textContent = Charts.fmt(pc, 2);
      if (read.pmix) read.pmix.textContent = Charts.fmt(pm, 2);
      if (read.q95) read.q95.textContent = S.showQ ? String(cur.q95) : "?";
      if (read.split) read.split.hidden = !S.showSplit;
      if (read.split) read.split.textContent = "d = " + Charts.fmt(S.d, 2) + ", plan " + S.n + ": dostawca A (p = " + Charts.fmt(p - S.d, 2) + ") " +
        Charts.fmt(pa, 2) + " · dostawca B (p = " + Charts.fmt(p + S.d, 2) + ") " + Charts.fmt(pb, 2) +
        " · razem " + Charts.fmt(pm, 2) + " (przy stałym p: " + Charts.fmt(pc, 2) + ") · średnio " + Charts.fmt(0.5 * r / (p - S.d) + 0.5 * r / (p + S.d), 0) + " kontroli";
      box.querySelectorAll("[data-mix-show]").forEach(function (b) {
        b.setAttribute("aria-pressed", S[b.dataset.mixShow] ? "true" : "false");
      });
      box.querySelectorAll("[data-mix-d]").forEach(function (b) {
        b.setAttribute("aria-pressed", Math.abs(parseFloat(b.dataset.mixD) - S.d) < 1e-9 ? "true" : "false");
      });
    }

    function move(e) {
      var n = Math.max(r, Math.min(XMAX, Math.round(Charts.ecPointer(chart, el, e, [0, XMAX]).x)));
      if (n !== S.n) { S.n = n; draw(); }
    }
    el.style.touchAction = "none";
    Charts.listen(el, "pointerdown", function (e) { drag = true; el.setPointerCapture(e.pointerId); move(e); e.preventDefault(); });
    Charts.listen(el, "pointermove", function (e) { if (drag) move(e); else el.style.cursor = "ew-resize"; });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(el, t, function () { drag = false; }); });
    Charts.listen(handle.element, "pointermove", function (e) { if (drag) move(e); });
    Charts.listen(handle.element, "pointerup", function () { drag = false; });

    Charts.listen(box, "click", function (e) {
      var b = e.target.closest("[data-mix-d]");
      if (b) { S.d = parseFloat(b.dataset.mixD); draw(); return; }
      var t = e.target.closest("[data-mix-show]");
      // Przełączniki są niezależne (oba mogą być włączone), więc nie oddajemy kliknięcia
      // wspólnej obsłudze .lc-chip z layoutu, która szuka grupy [data-lc-chips].
      if (t) { e.stopPropagation(); S[t.dataset.mixShow] = !S[t.dataset.mixShow]; draw(); return; }
      if (e.target.closest("[data-mix-reset]")) { S.d = START.d; S.n = START.n; S.showQ = false; S.showSplit = false; draw(); }
    });

    Charts.zoomControl(box);
    paint();
    draw();
    Charts.onTheme(box, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-mix", init);
})(typeof window !== "undefined" ? window : globalThis);
