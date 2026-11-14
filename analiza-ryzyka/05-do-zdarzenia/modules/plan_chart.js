// Limit planistyczny, średnia i rezerwa na jednym wykresie. Kontener
// .lc-plan[data-r][data-p][data-n] z <div class="lc-ec" data-role="pmf">,
// przyciskami [data-plan-alpha] i [data-plan-reset] oraz odczytami
// [data-role="mean|limit|alpha|reserve"]. Przycisk [data-plan-all] pokazuje
// trzy poziomy pewności naraz (dawny P10 „Koszt pewności”). Rozkład łącznej liczby kontroli do
// r-tej wady; pole do limitu to P(X ≤ n). Pionowa kreska limitu ma uchwyt:
// przeciąganie albo strzałki zmieniają n. Między średnią a limitem klamra rezerwy.
// ECharts (od 2026-11-14); kreski, klamry i podpisy to elementy graphic w pikselach,
// liczone od nowa po każdej zmianie i zmianie rozmiaru. Rachunek: window.lcD5Nb.
(function (root) {
  var Charts = root.lcCharts, NB = root.lcD5Nb;
  if (!Charts || !NB || typeof document === "undefined") return;

  function init(box) {
    var el = box.querySelector('.lc-ec[data-role="pmf"]');
    if (!el) return false;
    var r = parseInt(box.dataset.r || "3", 10), p = parseFloat(box.dataset.p || "0.10");
    var START = parseInt(box.dataset.n || "30", 10);
    var mean = r / p, XMAX = NB.quantile(r, p, 0.997), NMIN = r, NMAX = XMAX;
    var S = { n: START, all: false };
    // Widok „wszystkie”: trzy poziomy pewności naraz i koszt kolejnych punktów.
    var LEVELS = [0.9, 0.95, 0.99].map(function (a) { return { a: a, n: NB.quantile(r, p, a) }; });
    var data = NB.pmf(r, p, XMAX), ymax = Math.max.apply(null, data.map(function (d) { return d[1]; })) * 1.45;
    var read = {};
    ["mean", "limit", "alpha", "reserve"].forEach(function (k) { read[k] = box.querySelector('[data-role="' + k + '"]'); });
    var P = Charts.palette();

    var drag = false;
    var layer = Charts.handleLayer(box);
    var handle = layer.add(el, {
      label: "Limit planistyczny",
      hint: "Strzałki w prawo i w lewo zmieniają limit o jedną kontrolę, z Shift o dziesięć.",
      onKey: function (event) {
        var v = Charts.keyValue(event, S.n, NMIN, NMAX, 1, 10);
        if (v === null) return false;
        S.n = v; S.all = false; draw(); layer.announce(handle.element.getAttribute("aria-valuetext")); return true;
      },
      onPointerDown: function (event) { drag = true; el.setPointerCapture(event.pointerId); event.preventDefault(); }
    });

    function area(id) {
      return { id: id, type: "line", data: [], step: "middle", symbol: "none", silent: true, animation: false };
    }
    var chart = Charts.chart(el, {
      animation: false, aria: { enabled: false },
      grid: { left: 10, right: 14, top: 6, bottom: 8 },
      xAxis: { type: "value", min: 0, max: XMAX, name: "Łączna liczba kontroli do trzeciej wady" },
      yAxis: { type: "value", min: 0, max: ymax, show: false },
      series: [area("in"), area("pmf")]
    }, { onResize: paint });

    // Kreski średniej i limitu, klamra rezerwy i podpisy (piksele, jak dawna nakładka Chart.js).
    function overlay() {
      var fs = Charts.axisFontSize(box), font = "600 " + fs + "px " + P.mono;
      var lb = chart.convertToPixel({ gridIndex: 0 }, [0, 0]), rt = chart.convertToPixel({ gridIndex: 0 }, [XMAX, ymax]);
      if (!lb || !rt) return;
      var top = rt[1], bottom = lb[1];
      function px(v) { return chart.convertToPixel({ gridIndex: 0 }, [v, 0])[0]; }
      function seg(x1, y1, x2, y2, color, width, dash) {
        return { type: "line", silent: true, z: 10, shape: { x1: x1, y1: y1, x2: x2, y2: y2 },
                 style: { stroke: color, lineWidth: width, lineDash: dash || null } };
      }
      function text(x, y, t, color, align) {
        return { type: "text", silent: true, z: 11, x: x, y: y,
                 style: { text: t, fill: color, font: font, align: align || "center", verticalAlign: "bottom",
                          backgroundColor: P.surface, padding: [1, 3] } };  // podpis nad kreską, która go przecina
      }
      var xm = px(mean), els = [];
      els.push(seg(xm, bottom, xm, top + 34, P.ink, 1.5, [5, 4]));
      els.push(text(xm, top + 28, "średnia " + Charts.fmt(mean, 0), P.ink));
      var limits = S.all ? LEVELS : [{ a: null, n: S.n }];
      limits.forEach(function (L, i) {
        var x = px(L.n), res = L.n - mean;
        els.push(seg(x, bottom, x, top + 34, P.accent, S.all ? 2 : 2.5));
        els.push(text(x, top + 12, S.all ? Math.round(100 * L.a) + "%: " + L.n : "limit " + L.n, P.accent));
        var yb = top + 50 + i * 24;
        if (Math.abs(x - xm) > 6) {
          var c = res >= 0 ? P.bursztyn : P.accent;
          els.push({ type: "polyline", silent: true, z: 10, shape: { points: [[xm, yb + 6], [xm, yb], [x, yb], [x, yb + 6]] },
                     style: { stroke: c, lineWidth: 2, fill: null } });
          var txt = S.all ? (res >= 0 ? "+" : "−") + Charts.fmt(Math.abs(res), 0)
            : (res >= 0 ? "rezerwa " : "niedobór ") + Charts.fmt(Math.abs(res), 0);
          els.push(S.all ? text(x + 6, yb + 5, txt, c, "left") : text((xm + x) / 2, yb + 20, txt, c));
        }
      });
      chart.setOption({ graphic: els }, { replaceMerge: ["graphic"] });
      handle.element.hidden = S.all;
      handle.position(px(S.n), (top + bottom) / 2 + 30);
      handle.value(S.n, NMIN, NMAX, "limit " + S.n + " kontroli, P(X ≤ n) = " + Charts.fmt(NB.cdf(r, p, S.n), 3));
    }

    function paint() {
      P = Charts.palette();
      var fs = Charts.axisFontSize(box), x = Charts.ecAxis(P, fs, { splitNumber: 7 });
      x.name = Charts.ecName("Łączna liczba kontroli do trzeciej wady", (el.clientWidth || 640) - 60, fs);
      x.splitLine = { show: false };
      x.axisLabel.showMaxLabel = false;  // kraniec osi nie leży na podziałce i nachodzi na sąsiada
      chart.setOption({
        xAxis: x,
        series: [
          { id: "in", lineStyle: { width: 0 }, areaStyle: { color: Charts.alpha(P.accent, 0.3) } },
          { id: "pmf", lineStyle: { color: P.accent, width: 2 } }
        ]
      });
      overlay();
    }

    function draw() {
      chart.setOption({ series: [
        { id: "in", data: S.all ? [] : data.filter(function (d) { return d[0] <= S.n; }) },
        { id: "pmf", data: data }
      ] });
      overlay();
      var F = NB.cdf(r, p, S.n), res = S.n - mean;
      if (read.mean) read.mean.textContent = Charts.fmt(mean, 0);
      box.classList.toggle("is-all", S.all);
      if (S.all) {
        if (read.limit) read.limit.textContent = LEVELS.map(function (L) { return L.n; }).join("·");
        if (read.alpha) read.alpha.textContent = LEVELS.map(function (L) { return Charts.fmt(NB.cdf(r, p, L.n), 2); }).join("·");
        if (read.reserve) read.reserve.textContent = LEVELS.map(function (L) { return "+" + Charts.fmt(L.n - mean, 0); }).join("·");
      } else {
        if (read.limit) read.limit.textContent = String(S.n);
        if (read.alpha) read.alpha.textContent = Charts.fmt(F, 3);
        if (read.reserve) read.reserve.textContent = (res > 0 ? "+" : "") + Charts.fmt(res, 0);
      }
      box.querySelectorAll("[data-plan-alpha]").forEach(function (b) {
        b.setAttribute("aria-pressed", !S.all && NB.quantile(r, p, parseFloat(b.dataset.planAlpha)) === S.n ? "true" : "false");
      });
      box.querySelectorAll("[data-plan-all]").forEach(function (b) { b.setAttribute("aria-pressed", S.all ? "true" : "false"); });
    }

    function move(e) {
      var n = Math.max(NMIN, Math.min(NMAX, Math.round(Charts.ecPointer(chart, el, e, [0, XMAX]).x)));
      if (n !== S.n || S.all) { S.n = n; S.all = false; draw(); }
    }
    el.style.touchAction = "none";
    Charts.listen(el, "pointerdown", function (e) { drag = true; el.setPointerCapture(e.pointerId); move(e); e.preventDefault(); });
    Charts.listen(el, "pointermove", function (e) { if (drag) move(e); else el.style.cursor = "ew-resize"; });
    ["pointerup", "pointercancel"].forEach(function (t) { Charts.listen(el, t, function () { drag = false; }); });
    Charts.listen(handle.element, "pointermove", function (e) { if (drag) move(e); });
    Charts.listen(handle.element, "pointerup", function () { drag = false; });

    Charts.listen(box, "click", function (e) {
      var b = e.target.closest("[data-plan-alpha]");
      if (b) { S.n = NB.quantile(r, p, parseFloat(b.dataset.planAlpha)); S.all = false; draw(); return; }
      if (e.target.closest("[data-plan-all]")) { S.all = true; draw(); return; }
      if (e.target.closest("[data-plan-reset]")) { S.n = START; S.all = false; draw(); }
    });

    Charts.zoomControl(box);
    paint();
    draw();
    Charts.onTheme(box, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-plan", init);
})(typeof window !== "undefined" ? window : globalThis);
