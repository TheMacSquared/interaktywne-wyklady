// Rozkład ujemny dwumianowy na tle geometrycznego (P7). Kontener:
// .lc-nb[data-r][data-p] z <div class="lc-ec" data-role="pmf">, przyciskami [data-nb-p]
// i [data-nb-reset] oraz odczytami [data-role="r|mean|median|peak"].
// Kropka na średniej r/p: przeciąganie w bok albo strzałki zmieniają r (1–10).
// Oś X zależy tylko od p (99,5. percentyl dla r = 10), oś Y jest stała: szczyt
// rozkładu geometrycznego. ECharts (od 2026-11-14), wzorzec jak normals_chart.js
// w statystyce; rachunek w window.lcD5Nb (testy: tests/js/d5_charts.test.js).
(function (root) {
  var R_MIN = 1, R_MAX = 10;

  // P(X = x) dla łącznej liczby prób; kolejne wyrazy z ilorazu x/(x − r + 1)·(1 − p).
  function pmf(r, p, xmax) {
    var out = [], P = Math.pow(p, r);
    for (var x = r; x <= xmax; x++) { out.push([x, P]); P *= x / (x - r + 1) * (1 - p); }
    return out;
  }
  function quantile(r, p, a) {
    var P = Math.pow(p, r), F = 0;
    for (var x = r; ; x++) { F += P; if (F >= a) return x; P *= x / (x - r + 1) * (1 - p); }
  }
  function cdf(r, p, n) {
    var P = Math.pow(p, r), F = 0;
    for (var x = r; x <= n; x++) { F += P; P *= x / (x - r + 1) * (1 - p); }
    return F;
  }
  root.lcD5Nb = { pmf: pmf, quantile: quantile, cdf: cdf, legendRows: legendRows };

  // Wąski wykres (telefon): legenda zawija się w kilka wierszy, więc obszar rysunku
  // schodzi niżej o tyle wierszy; nazwa osi X zawija się do szerokości (lcCharts.ecName).
  function legendRows(names, width, fs) {
    var rows = 1, used = 0;
    names.forEach(function (n) {
      var w = 28 + 5 + n.length * fs * 0.62 + 10;
      if (used && used + w > width) { rows++; used = 0; }
      used += w;
    });
    return rows;
  }

  var Charts = root.lcCharts;
  if (!Charts || typeof document === "undefined") return;

  function init(box) {
    var el = box.querySelector('.lc-ec[data-role="pmf"]');
    if (!el) return false;
    var START = { r: parseInt(box.dataset.r || "3", 10), p: parseFloat(box.dataset.p || "0.10") };
    var S = { r: START.r, p: START.p };
    var read = {};
    ["r", "mean", "median", "peak"].forEach(function (k) { read[k] = box.querySelector('[data-role="' + k + '"]'); });
    var cur = [], xmax = 1, ymax = 1;

    var drag = false;
    var layer = Charts.handleLayer(box);
    var handle = layer.add(el, {
      label: "Średnia rozkładu",
      hint: "Strzałki w prawo i w lewo zmieniają r, czyli liczbę wad, na które czekamy.",
      onKey: function (event) {
        var v = Charts.keyValue(event, S.r, R_MIN, R_MAX, 1, 3);
        if (v === null) return false;
        S.r = v; draw(); layer.announce(handle.element.getAttribute("aria-valuetext")); return true;
      },
      onPointerDown: function (event) { drag = true; el.setPointerCapture(event.pointerId); event.preventDefault(); }
    });

    function meanPoint() {
      var m = S.r / S.p, x = Math.max(S.r, Math.round(m));
      var y = cur.length ? cur[Math.min(cur.length - 1, x - S.r)][1] : 0;
      return [m, y];
    }
    function syncHandle() {
      var m = meanPoint(), q = chart.convertToPixel({ gridIndex: 0 }, m);
      if (q) handle.position(q[0], q[1]);
      handle.value(S.r, R_MIN, R_MAX, "r = " + S.r + ", średnia " + Charts.fmt(m[0], 1) + " kontroli");
    }

    function line(id, extra) {
      return Object.assign({ id: id, type: "line", data: [], symbol: "none", silent: true, animation: false }, extra);
    }
    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 12, top: 40, bottom: 8 },
      legend: { top: 0, right: 0, data: ["r = 1 (geometryczny)", "r", "średnia r/p"], selectedMode: false },
      xAxis: { type: "value", min: 0, name: "Łączna liczba kontroli do r-tej wady" },
      yAxis: { type: "value", min: 0, name: "Prawdopodobieństwo" },
      series: [
        line("geo", { name: "r = 1 (geometryczny)" }),
        line("nb", { name: "r" }),
        line("mean", { name: "średnia r/p" }),
        { id: "dot", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false }
      ]
    }, { onResize: paint });

    function paint() {
      var P = Charts.palette(), fs = Charts.axisFontSize(box);
      var x = Charts.ecAxis(P, fs, { splitNumber: 7 }), y = Charts.ecAxis(P, fs, { splitNumber: 4, nameGap: Math.round(fs * 3.4) });
      y.axisLabel.formatter = function (v) { return Charts.fmt(v, 2); };
      x.axisLabel.showMaxLabel = false; y.axisLabel.showMaxLabel = false;
      var w = el.clientWidth || 640;
      x.name = Charts.ecName("Łączna liczba kontroli do r-tej wady", w - 60, fs);
      // Na telefonie nazwa osi Y wchodziłaby na legendę; podziałki 0.00–0.09 mówią same.
      y.name = w < 480 ? "" : "Prawdopodobieństwo";
      chart.setOption({
        grid: { top: 16 + legendRows(["r = 1 (geometryczny)", "r = 10", "średnia r/p"], w, fs) * Math.round(fs * 1.5) },
        xAxis: x, yAxis: y,
        // left: miejsce na przycisk lupy w lewym górnym rogu, gdy legenda się zawija.
        legend: { left: w < 480 ? 30 : "auto", textStyle: { color: P.soft, fontFamily: P.mono, fontSize: fs }, itemWidth: 28, itemHeight: 10,
                  itemStyle: { opacity: 0 } },  // sama linia w ikonie, bez kropki
        series: [
          { id: "geo", lineStyle: { color: P.subtle, width: 1.6, type: [5, 4] }, itemStyle: { color: P.subtle } },
          { id: "nb", lineStyle: { color: P.accent, width: 2.5 }, itemStyle: { color: P.accent },
            areaStyle: { color: Charts.alpha(P.accent, 0.25) } },
          { id: "mean", lineStyle: { color: P.ink, width: 1.4, type: [4, 3] }, itemStyle: { color: P.ink } },
          { id: "dot", itemStyle: { color: P.accent, borderColor: P.surface, borderWidth: 2 } }
        ]
      });
      syncHandle();
    }

    function draw() {
      var r = S.r, p = S.p, mean = r / p;
      xmax = quantile(R_MAX, p, 0.995); ymax = p * 1.08;
      cur = pmf(r, p, xmax);
      chart.setOption({
        legend: { data: ["r = 1 (geometryczny)", "r = " + r, "średnia r/p"] },
        xAxis: { max: xmax }, yAxis: { max: ymax },
        series: [
          { id: "geo", data: pmf(1, p, xmax) },
          { id: "nb", name: "r = " + r, data: cur },
          { id: "mean", data: [[mean, 0], [mean, ymax]] },
          { id: "dot", data: [meanPoint()] }
        ]
      });
      syncHandle();
      var peak = Math.max.apply(null, cur.map(function (d) { return d[1]; }));
      if (read.r) read.r.textContent = "r = " + r;
      if (read.mean) read.mean.textContent = Charts.fmt(mean, Math.abs(mean - Math.round(mean)) < 1e-9 ? 0 : 1);
      if (read.median) read.median.textContent = String(quantile(r, p, 0.5));
      if (read.peak) read.peak.textContent = Charts.fmt(peak, 3);
      box.querySelectorAll("[data-nb-p]").forEach(function (b) {
        b.setAttribute("aria-pressed", parseFloat(b.dataset.nbP) === p ? "true" : "false");
      });
    }

    // Przeciąganie: pozioma pozycja kursora to średnia, z niej r = round(x · p).
    function move(e) {
      var pt = Charts.ecPointer(chart, el, e, [0, xmax]);
      var r = Math.max(R_MIN, Math.min(R_MAX, Math.round(pt.x * S.p)));
      if (r !== S.r) { S.r = r; draw(); }
    }
    function inPlot(e) {
      var pt = Charts.ecPointer(chart, el, e);
      return pt.rawY >= 0 && pt.rawY <= ymax && pt.rawX >= 0 && pt.rawX <= xmax;
    }
    el.style.touchAction = "none";
    Charts.listen(el, "pointerdown", function (e) {
      if (!inPlot(e)) return;
      drag = true; el.setPointerCapture(e.pointerId); move(e); e.preventDefault();
    });
    Charts.listen(el, "pointermove", function (e) {
      if (drag) { move(e); return; }
      el.style.cursor = inPlot(e) ? "ew-resize" : "default";
    });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(el, t, function () { drag = false; }); });
    Charts.listen(handle.element, "pointermove", function (e) { if (drag) move(e); });
    Charts.listen(handle.element, "pointerup", function () { drag = false; });

    Charts.listen(box, "click", function (e) {
      var pb = e.target.closest("[data-nb-p]");
      if (pb) { S.p = parseFloat(pb.dataset.nbP); draw(); return; }
      if (e.target.closest("[data-nb-reset]")) { S.r = START.r; S.p = START.p; draw(); }
    });

    Charts.zoomControl(box);
    draw();
    paint();
    Charts.onTheme(box, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-nb", init);
})(typeof window !== "undefined" ? window : globalThis);
