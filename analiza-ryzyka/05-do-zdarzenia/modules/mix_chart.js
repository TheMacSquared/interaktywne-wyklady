// Plan na stałym p a dwaj dostawcy (PROTOTYP 2026-11-04, scalone P11 i P11B).
// Kontener .lc-mix[data-r][data-p][data-d][data-n] z <canvas data-role="pmf">,
// przyciskami [data-mix-d] i [data-mix-reset] oraz odczytami
// [data-role="n|pconst|pmix|q95"] i [data-role="split"]. Połowa partii ma
// p − d, połowa p + d. Pole: rozkład łącznej liczby kontroli w mieszance;
// przerywany kontur: stałe p. Kreska planu ma uchwyt (przeciąganie, strzałki),
// druga kreska to prawdziwy kwantyl 95% mieszanki. Kreska 95% i rozbicie na
// dostawców są schowane za przełącznikami [data-mix-show="showQ|showSplit"].
(function () {
  var Charts = window.lcCharts;

  function pmfArr(r, p, xmax) {
    var out = new Array(xmax + 1).fill(0), P = Math.pow(p, r);
    for (var x = r; x <= xmax; x++) { out[x] = P; P *= x / (x - r + 1) * (1 - p); }
    return out;
  }
  function cdf(arr, n) { var F = 0; for (var x = 0; x <= n && x < arr.length; x++) F += arr[x]; return F; }
  function quantile(arr, a) { var F = 0; for (var x = 0; x < arr.length; x++) { F += arr[x]; if (F >= a) return x; } return arr.length; }

  function init(root) {
    var canvas = root.querySelector('canvas[data-role="pmf"]');
    if (!canvas) return false;
    var r = parseInt(root.dataset.r || "3", 10), p = parseFloat(root.dataset.p || "0.10");
    var START = { d: parseFloat(root.dataset.d || "0.05"), n: parseInt(root.dataset.n || "61", 10) };
    var S = { d: START.d, n: START.n, showQ: false, showSplit: false }, XMAX = 180, CALC = 3000;
    var base = pmfArr(r, p, CALC);
    var read = {};
    ["n", "pconst", "pmix", "q95", "split"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });
    var P = Charts.palette(), cur = null;

    function model() {
      var A = pmfArr(r, p - S.d, CALC), B = pmfArr(r, p + S.d, CALC);
      var mix = A.map(function (v, i) { return 0.5 * v + 0.5 * B[i]; });
      return { A: A, B: B, mix: mix, q95: quantile(mix, 0.95) };
    }

    var layer = Charts.handleLayer(root);
    var handle = layer.add(canvas, {
      label: "Plan kontroli",
      hint: "Strzałki w prawo i w lewo zmieniają plan o jedną kontrolę, z Shift o dziesięć.",
      onKey: function (event) {
        var v = Charts.keyValue(event, S.n, r, XMAX, 1, 10);
        if (v === null) return false;
        S.n = v; draw(); layer.announce(handle.element.getAttribute("aria-valuetext")); return true;
      },
      onPointerDown: function (event) { drag = true; canvas.setPointerCapture(event.pointerId); event.preventDefault(); }
    });

    var overlay = {
      id: "mixOverlay",
      afterDatasetsDraw: function (ch) {
        if (!cur) return;
        var ctx = ch.ctx, a = ch.chartArea, xs = ch.scales.x, fs = Charts.axisFontSize(root);
        ctx.save();
        ctx.font = "600 " + fs + "px " + P.mono; ctx.textAlign = "center";
        var xn = xs.getPixelForValue(S.n);
        ctx.strokeStyle = P.ink; ctx.lineWidth = 2; ctx.setLineDash([6, 4]);
        ctx.beginPath(); ctx.moveTo(xn, a.bottom); ctx.lineTo(xn, a.top + 22); ctx.stroke();
        ctx.setLineDash([]); ctx.fillStyle = P.ink;
        ctx.fillText("plan " + S.n, xn, a.top + 14);
        if (S.showQ) {
          var q = cur.q95, off = q > XMAX, xq = xs.getPixelForValue(Math.min(q, XMAX));
          ctx.strokeStyle = P.accent; ctx.lineWidth = 2.5;
          ctx.beginPath(); ctx.moveTo(xq, a.bottom); ctx.lineTo(xq, a.top + 44); ctx.stroke();
          ctx.fillStyle = P.accent; ctx.textAlign = off ? "right" : "center";
          ctx.fillText("95% naprawdę: " + q + (off ? " →" : ""), off ? xq - 4 : xq, a.top + 36);
        }
        ctx.restore();
      },
      afterRender: function (ch) {
        var a = ch.chartArea;
        handle.position(ch.scales.x.getPixelForValue(S.n), (a.top + a.bottom) / 2 + 20);
        if (cur) handle.value(S.n, r, XMAX, "plan " + S.n + " kontroli, szansa ukończenia " + Charts.fmt(cdf(cur.mix, S.n), 2));
      }
    };

    var chart = Charts.create(canvas, {
      type: "line",
      plugins: [overlay],
      data: { datasets: [
        { label: "dwaj dostawcy", data: [], fill: "origin", borderWidth: 2, stepped: "middle" },
        { label: "stałe p = 0.10", data: [], fill: false, borderWidth: 1.6, borderDash: [5, 4], stepped: "middle" }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 6, right: 10 } },
        scales: {
          x: { type: "linear", min: 0, max: XMAX, title: { display: true, text: "Łączna liczba kontroli do trzeciej wady" } },
          y: { min: 0, display: false }
        },
        plugins: {
          legend: { display: true, position: "top", align: "end", labels: { boxWidth: 28, boxHeight: 10 } },
          tooltip: { enabled: false }, colors: { enabled: false }
        },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });
    var YMAX = Math.max.apply(null, base) * 1.35;

    function paint() {
      P = Charts.palette();
      var fs = Charts.axisFontSize(root);
      Charts.styleAxes(chart, P, { fontSize: fs, maxRotation: 0 });
      chart.config.options.scales.x.ticks.maxTicksLimit = 8;
      chart.config.options.plugins.legend.labels.color = P.soft;
      chart.config.options.plugins.legend.labels.font = { family: P.mono, size: fs };
      var ds = chart.data.datasets;
      ds[0].borderColor = P.accent; ds[0].backgroundColor = Charts.alpha(P.accent, 0.25);
      ds[1].borderColor = P.subtle;
      chart.update();
    }

    function series(arr) {
      var out = [];
      for (var x = r; x <= XMAX; x++) out.push({ x: x, y: arr[x] });
      return out;
    }

    function draw() {
      cur = model();
      var ds = chart.data.datasets;
      ds[0].data = series(cur.mix);
      ds[1].data = series(base);
      chart.config.options.scales.y.max = Math.max(YMAX, Math.max.apply(null, cur.mix.slice(0, XMAX)) * 1.35);
      chart.update("none");
      var pc = cdf(base, S.n), pm = cdf(cur.mix, S.n), pa = cdf(cur.A, S.n), pb = cdf(cur.B, S.n);
      if (read.n) read.n.textContent = String(S.n);
      if (read.pconst) read.pconst.textContent = Charts.fmt(pc, 2);
      if (read.pmix) read.pmix.textContent = Charts.fmt(pm, 2);
      if (read.q95) read.q95.textContent = S.showQ ? String(cur.q95) : "?";
      if (read.split) read.split.hidden = !S.showSplit;
      if (read.split) read.split.textContent = "d = " + Charts.fmt(S.d, 2) + ", plan " + S.n + ": dostawca A (p = " + Charts.fmt(p - S.d, 2) + ") " +
        Charts.fmt(pa, 2) + " · dostawca B (p = " + Charts.fmt(p + S.d, 2) + ") " + Charts.fmt(pb, 2) +
        " · razem " + Charts.fmt(pm, 2) + " (przy stałym p: " + Charts.fmt(pc, 2) + ") · średnio " + Charts.fmt(0.5 * r / (p - S.d) + 0.5 * r / (p + S.d), 0) + " kontroli";
      root.querySelectorAll("[data-mix-show]").forEach(function (b) {
        b.setAttribute("aria-pressed", S[b.dataset.mixShow] ? "true" : "false");
      });
      root.querySelectorAll("[data-mix-d]").forEach(function (b) {
        b.setAttribute("aria-pressed", Math.abs(parseFloat(b.dataset.mixD) - S.d) < 1e-9 ? "true" : "false");
      });
    }

    var drag = false;
    function move(e) {
      var rc = canvas.getBoundingClientRect(), a = chart.chartArea;
      var px = Math.max(a.left, Math.min(a.right, e.clientX - rc.left));
      var n = Math.max(r, Math.min(XMAX, Math.round(chart.scales.x.getValueForPixel(px))));
      if (n !== S.n) { S.n = n; draw(); }
    }
    canvas.style.touchAction = "none";
    Charts.listen(canvas, "pointerdown", function (e) { drag = true; canvas.setPointerCapture(e.pointerId); move(e); e.preventDefault(); });
    Charts.listen(canvas, "pointermove", function (e) { if (drag) move(e); else canvas.style.cursor = "ew-resize"; });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(canvas, t, function () { drag = false; }); });
    Charts.listen(handle.element, "pointermove", function (e) { if (drag) move(e); });
    Charts.listen(handle.element, "pointerup", function () { drag = false; });

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-mix-d]");
      if (b) { S.d = parseFloat(b.dataset.mixD); draw(); return; }
      var t = e.target.closest("[data-mix-show]");
      if (t) { S[t.dataset.mixShow] = !S[t.dataset.mixShow]; draw(); return; }
      if (e.target.closest("[data-mix-reset]")) { S.d = START.d; S.n = START.n; S.showQ = false; S.showSplit = false; draw(); }
    });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-mix", init);
})();
