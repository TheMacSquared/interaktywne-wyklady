// Limit planistyczny, średnia i rezerwa na jednym wykresie (PROTOTYP 2026-11-04).
// Kontener .lc-plan[data-r][data-p][data-n] z <canvas data-role="pmf">,
// przyciskami [data-plan-alpha] i [data-plan-reset] oraz odczytami
// [data-role="mean|limit|alpha|reserve"]. Przycisk [data-plan-all] pokazuje
// trzy poziomy pewności naraz (dawny P10 „Koszt pewności”). Rozkład łącznej liczby kontroli do
// r-tej wady; pole do limitu to P(X ≤ n). Pionowa kreska limitu ma uchwyt:
// przeciąganie albo strzałki zmieniają n. Między średnią a limitem klamra rezerwy.
(function () {
  var Charts = window.lcCharts;

  function pmf(r, p, xmax) {
    var out = [], P = Math.pow(p, r);
    for (var x = r; x <= xmax; x++) { out.push({ x: x, y: P }); P *= x / (x - r + 1) * (1 - p); }
    return out;
  }
  function cdf(r, p, n) {
    var P = Math.pow(p, r), F = 0;
    for (var x = r; x <= n; x++) { F += P; P *= x / (x - r + 1) * (1 - p); }
    return F;
  }
  function quantile(r, p, a) {
    var P = Math.pow(p, r), F = 0;
    for (var x = r; ; x++) { F += P; if (F >= a) return x; P *= x / (x - r + 1) * (1 - p); }
  }

  function init(root) {
    var canvas = root.querySelector('canvas[data-role="pmf"]');
    if (!canvas) return false;
    var r = parseInt(root.dataset.r || "3", 10), p = parseFloat(root.dataset.p || "0.10");
    var START = parseInt(root.dataset.n || "30", 10);
    var mean = r / p, XMAX = quantile(r, p, 0.997), NMIN = r, NMAX = XMAX;
    var S = { n: START, all: false };
    // Widok „wszystkie”: trzy poziomy pewności naraz i koszt kolejnych punktów.
    var LEVELS = [0.9, 0.95, 0.99].map(function (a) { return { a: a, n: quantile(r, p, a) }; });
    var data = pmf(r, p, XMAX), ymax = Math.max.apply(null, data.map(function (d) { return d.y; })) * 1.45;
    var read = {};
    ["mean", "limit", "alpha", "reserve"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });
    var P = Charts.palette();

    var layer = Charts.handleLayer(root);
    var handle = layer.add(canvas, {
      label: "Limit planistyczny",
      hint: "Strzałki w prawo i w lewo zmieniają limit o jedną kontrolę, z Shift o dziesięć.",
      onKey: function (event) {
        var v = Charts.keyValue(event, S.n, NMIN, NMAX, 1, 10);
        if (v === null) return false;
        S.n = v; S.all = false; draw(); layer.announce(handle.element.getAttribute("aria-valuetext")); return true;
      },
      onPointerDown: function (event) { drag = true; canvas.setPointerCapture(event.pointerId); event.preventDefault(); }
    });

    // Kreski średniej i limitu, klamra rezerwy i podpisy rysowane na płótnie.
    var overlay = {
      id: "planOverlay",
      afterDatasetsDraw: function (ch) {
        var ctx = ch.ctx, a = ch.chartArea, xs = ch.scales.x, fs = Charts.axisFontSize(root);
        var xm = xs.getPixelForValue(mean), xl = xs.getPixelForValue(S.n);
        ctx.save();
        ctx.font = "600 " + fs + "px " + P.mono;
        // średnia
        ctx.strokeStyle = P.ink; ctx.lineWidth = 1.5; ctx.setLineDash([5, 4]);
        ctx.beginPath(); ctx.moveTo(xm, a.bottom); ctx.lineTo(xm, a.top + 34); ctx.stroke();
        ctx.setLineDash([]);
        ctx.fillStyle = P.ink; ctx.textAlign = "center";
        ctx.fillText("średnia " + Charts.fmt(mean, 0), xm, a.top + 28);
        // limit (albo trzy limity) i klamry rezerwy od średniej
        var limits = S.all ? LEVELS : [{ a: null, n: S.n }];
        limits.forEach(function (L, i) {
          var x = xs.getPixelForValue(L.n), res = L.n - mean;
          ctx.strokeStyle = P.accent; ctx.lineWidth = S.all ? 2 : 2.5;
          ctx.beginPath(); ctx.moveTo(x, a.bottom); ctx.lineTo(x, a.top + 34); ctx.stroke();
          ctx.fillStyle = P.accent;
          ctx.fillText(S.all ? Math.round(100 * L.a) + "%: " + L.n : "limit " + L.n, x, a.top + 12);
          var yb = a.top + 50 + i * 24;
          if (Math.abs(x - xm) > 6) {
            var c = res >= 0 ? P.bursztyn : P.accent;
            ctx.strokeStyle = c; ctx.lineWidth = 2;
            ctx.beginPath();
            ctx.moveTo(xm, yb + 6); ctx.lineTo(xm, yb); ctx.lineTo(x, yb); ctx.lineTo(x, yb + 6);
            ctx.stroke();
            ctx.fillStyle = c;
            var txt = S.all ? (res >= 0 ? "+" : "−") + Charts.fmt(Math.abs(res), 0)
              : (res >= 0 ? "rezerwa " : "niedobór ") + Charts.fmt(Math.abs(res), 0);
            ctx.textAlign = S.all ? "left" : "center";
            ctx.fillText(txt, S.all ? x + 6 : (xm + x) / 2, S.all ? yb + 5 : yb + 20);
            ctx.textAlign = "center";
          }
        });
        ctx.restore();
      },
      afterRender: function (ch) {
        var a = ch.chartArea;
        handle.element.hidden = S.all;
        handle.position(ch.scales.x.getPixelForValue(S.n), (a.top + a.bottom) / 2 + 30);
        handle.value(S.n, NMIN, NMAX, "limit " + S.n + " kontroli, P(X ≤ n) = " + Charts.fmt(cdf(r, p, S.n), 3));
      }
    };

    var chart = Charts.create(canvas, {
      type: "line",
      plugins: [overlay],
      data: { datasets: [
        { label: "w limicie", data: [], fill: "origin", borderWidth: 0, stepped: "middle" },
        { label: "rozkład", data: [], fill: false, borderWidth: 2, stepped: "middle" }
      ] },
      options: {
        responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
        layout: { padding: { top: 6, right: 10 } },
        scales: {
          x: { type: "linear", min: 0, max: XMAX, title: { display: true, text: "Łączna liczba kontroli do trzeciej wady" } },
          y: { min: 0, max: ymax, display: false }
        },
        plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } },
        elements: { point: { radius: 0 }, line: { tension: 0 } }
      }
    });

    function paint() {
      P = Charts.palette();
      Charts.styleAxes(chart, P, { fontSize: Charts.axisFontSize(root), maxRotation: 0 });
      chart.config.options.scales.x.ticks.maxTicksLimit = 8;
      var ds = chart.data.datasets;
      ds[0].backgroundColor = Charts.alpha(P.accent, 0.3);
      ds[1].borderColor = P.accent;
      chart.update();
    }

    function draw() {
      var ds = chart.data.datasets;
      ds[0].data = S.all ? [] : data.filter(function (d) { return d.x <= S.n; });
      ds[1].data = data;
      chart.update("none");
      var F = cdf(r, p, S.n), res = S.n - mean;
      if (read.mean) read.mean.textContent = Charts.fmt(mean, 0);
      root.classList.toggle("is-all", S.all);
      if (S.all) {
        if (read.limit) read.limit.textContent = LEVELS.map(function (L) { return L.n; }).join("·");
        if (read.alpha) read.alpha.textContent = LEVELS.map(function (L) { return Charts.fmt(cdf(r, p, L.n), 2); }).join("·");
        if (read.reserve) read.reserve.textContent = LEVELS.map(function (L) { return "+" + Charts.fmt(L.n - mean, 0); }).join("·");
      } else {
        if (read.limit) read.limit.textContent = String(S.n);
        if (read.alpha) read.alpha.textContent = Charts.fmt(F, 3);
        if (read.reserve) read.reserve.textContent = (res > 0 ? "+" : "") + Charts.fmt(res, 0);
      }
      root.querySelectorAll("[data-plan-alpha]").forEach(function (b) {
        b.setAttribute("aria-pressed", !S.all && quantile(r, p, parseFloat(b.dataset.planAlpha)) === S.n ? "true" : "false");
      });
      root.querySelectorAll("[data-plan-all]").forEach(function (b) { b.setAttribute("aria-pressed", S.all ? "true" : "false"); });
    }

    var drag = false;
    function move(e) {
      var rc = canvas.getBoundingClientRect(), a = chart.chartArea;
      var px = Math.max(a.left, Math.min(a.right, e.clientX - rc.left));
      var n = Math.round(chart.scales.x.getValueForPixel(px));
      n = Math.max(NMIN, Math.min(NMAX, n));
      if (n !== S.n || S.all) { S.n = n; S.all = false; draw(); }
    }
    canvas.style.touchAction = "none";
    Charts.listen(canvas, "pointerdown", function (e) { drag = true; canvas.setPointerCapture(e.pointerId); move(e); e.preventDefault(); });
    Charts.listen(canvas, "pointermove", function (e) { if (drag) move(e); else canvas.style.cursor = "ew-resize"; });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(canvas, t, function () { drag = false; }); });
    Charts.listen(handle.element, "pointermove", function (e) { if (drag) move(e); });
    Charts.listen(handle.element, "pointerup", function () { drag = false; });

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-plan-alpha]");
      if (b) { S.n = quantile(r, p, parseFloat(b.dataset.planAlpha)); S.all = false; draw(); return; }
      if (e.target.closest("[data-plan-all]")) { S.all = true; draw(); return; }
      if (e.target.closest("[data-plan-reset]")) { S.n = START; S.all = false; draw(); }
    });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-plan", init);
})();
