// Gra „zgadnij średnią i medianę” (Ryc. 3.10). Rundy losowane w przeglądarce według
// tych samych zasad co dawniej w R: 200 obserwacji z rozkładu symetrycznego (normalny),
// prawoskośnego (gamma + przesunięcie) albo lewoskośnego (100 − gamma), zaokrąglone
// do 0.1. Pierwsze kliknięcie to typowana średnia, drugie — mediana; przed odsłonięciem
// typy można przeciągać. Kontener .lc-guess z <div class="lc-ec" data-role="hist">,
// przyciskami [data-g-act="new|reveal"] i polami [data-role="banner|feedback"]. ECharts.
(function () {
  var Charts = window.lcCharts;
  var U = window.lc01;
  if (!U) return;

  function generate() {
    var type = ["symmetric", "right_skew", "left_skew"][U.sampleInt(0, 2)], n = 200, x = [], i;
    if (type === "symmetric") {
      var mu = U.sampleInt(40, 80), sd = U.sampleInt(8, 15);
      for (i = 0; i < n; i++) x.push(U.rnorm(mu, sd));
    } else {
      var k = U.sampleInt(2, 4), sc = U.sampleInt(5, 12), sh = type === "right_skew" ? U.sampleInt(10, 30) : 0;
      for (i = 0; i < n; i++) {
        var g = U.rgammaInt(k, sc);
        x.push(type === "right_skew" ? g + sh : 100 - g);
      }
    }
    return { type: type, values: x.map(function (v) { return Math.round(v * 10) / 10; }) };
  }

  function init(root) {
    var canvas = root.querySelector('.lc-ec[data-role="hist"]');
    if (!canvas) return false;
    var banner = root.querySelector('[data-role="banner"]');
    var feedback = root.querySelector('[data-role="feedback"]');
    var btnReveal = root.querySelector('[data-g-act="reveal"]');
    var round = null, guess = { mean: null, median: null }, revealed = false;
    var score = { total: 0, good: 0 }, LO = 0, HI = 1, YMAX = 5, drag = null;

    var layer = Charts.handleLayer(root), guessHandles = {};
    ["mean", "median"].forEach(function (key) {
      var handle = layer.add(canvas, {
        label: key === "mean" ? "Typowana średnia" : "Typowana mediana",
        hint: "Strzałki ustawiają i przesuwają odpowiedź. Shift zwiększa krok dziesięciokrotnie. Enter ustawia znacznik w środku osi. Odpowiedź odsłania osobny przycisk.",
        onKey: function (event) {
          if (revealed) return false;
          var current = guess[key] == null ? (LO + HI) / 2 : guess[key];
          var step = Math.max(0.1, Math.round((HI - LO) / 100 * 10) / 10);
          var value = Charts.keyValue(event, current, LO, HI, step, step * 10);
          if (value === null && (event.key === "Enter" || event.key === " ")) value = current;
          if (value === null) return false;
          guess[key] = Math.max(LO, Math.min(HI, Math.round(value * 10) / 10)); draw();
          layer.announce((key === "mean" ? "Typowana średnia " : "Typowana mediana ") + Charts.fmt(guess[key], 1) + ". Odpowiedź ukryta.");
          return true;
        },
        onPointerDown: function (event) {
          if (revealed) return;
          drag = key; guess[key] = at(event);
          canvas.setPointerCapture(event.pointerId); draw(); event.preventDefault();
        }
      });
      handle.element.setAttribute("data-boundary", key === "mean" ? "Śr." : "Me.");
      handle.element.setAttribute("data-guess", key);
      handle.element.setAttribute("class", "lc-chart-handle has-dot is-unset");
      guessHandles[key] = handle;
    });
    function syncHandles() {
      if (!round) return;
      ["mean", "median"].forEach(function (key, i) {
        var value = guess[key] == null ? (LO + HI) / 2 : guess[key];
        var q = chart.convertToPixel({ gridIndex: 0 }, [value, YMAX * (i ? 0.72 : 0.9)]);
        if (q) guessHandles[key].position(q[0], q[1]);
      });
    }
    var histStyle = { fill: "#ccc", gap: "#fff" };
    var ids = ["gMean", "gMed", "tMean", "tMed"];
    var chart = Charts.chart(canvas, {
      animation: false, aria: { enabled: false },
      grid: { left: 8, right: 12, top: 6, bottom: 8 },
      xAxis: { type: "value", name: "Wartość" },
      yAxis: { type: "value", min: 0, minInterval: 1, name: "Liczebność" },
      series: [U.histSeries("hist", histStyle)].concat(ids.map(function (id, k) {
        return { id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
                 lineStyle: { width: k < 2 ? 2 : 3, type: k < 2 ? [6, 4] : "solid" } };
      }))
    }, { onResize: function () { if (round) draw(); } });

    var P = null;
    function paint() {
      P = Charts.palette();
      var fs = Charts.axisFontSize(root), x = Charts.ecAxis(P, fs, { nameGap: Math.round(fs * 1.9) });
      x.axisLabel.showMinLabel = x.axisLabel.showMaxLabel = false;
      histStyle.fill = Charts.alpha(P.ref, 0.5); histStyle.gap = P.surface;
      chart.setOption({
        xAxis: x, yAxis: Charts.ecAxis(P, fs, { nameGap: Math.round(fs * 2.6) }),
        series: [{ id: "hist", renderItem: U.histSeries("hist", histStyle).renderItem }].concat(ids.map(function (id, k) {
          return { id: id, lineStyle: { color: k % 2 ? P.niebo : P.accent } };
        }))
      });
    }

    function newRound() {
      round = generate();
      var x = round.values, mn = Math.min.apply(null, x), mx = Math.max.apply(null, x), w = (mx - mn) / 25;
      var b = U.bins(x, mn, mx + 1e-9, w), top = 0;
      b.forEach(function (bb) { top = Math.max(top, bb.n); });
      LO = mn - 0.05 * (mx - mn); HI = mx + 0.05 * (mx - mn);
      YMAX = Math.ceil(top * 1.45 / 5) * 5;
      chart.setOption({ xAxis: { min: LO, max: HI }, yAxis: { max: YMAX }, series: [{ id: "hist", data: U.histData(b) }] });
      guess = { mean: null, median: null }; revealed = false;
      draw();
    }

    function vline(v) { return U.vline(v, YMAX); }

    function draw() {
      var x = round.values, m = U.mean(x), med = U.median(x), labels = [];
      chart.setOption({ series: [
        { id: "gMean", data: vline(guess.mean) }, { id: "gMed", data: vline(guess.median) },
        { id: "tMean", data: revealed ? vline(m) : [] }, { id: "tMed", data: revealed ? vline(med) : [] }
      ] });
      if (guess.mean != null) labels.push({ x: guess.mean, text: "Wskazana średnia", color: P.accent, row: 0 });
      if (guess.median != null) labels.push({ x: guess.median, text: "Wskazana mediana", color: P.niebo, row: 1 });
      if (revealed) {
        labels.push({ x: m, text: "Średnia " + Charts.fmt(m, 1), color: P.accent, row: 2 });
        labels.push({ x: med, text: "Mediana " + Charts.fmt(med, 1), color: P.niebo, row: 3 });
      }
      U.lineLabels(chart, labels, { xmax: HI, ymax: YMAX, font: P.mono, fontSize: Charts.axisFontSize(root),
        halo: Charts.alpha(P.surface, 0.85) });

      var tone = "info", msg;
      if (guess.mean == null) msg = "Wskaż średnią: kliknij na wykresie lub użyj Tab i strzałek.";
      else if (guess.median == null) msg = "Teraz wskaż medianę: kliknij lub użyj klawiatury.";
      else if (!revealed) { msg = "Gotowe. Kliknij „Pokaż odpowiedź”. Linie można jeszcze przeciągnąć."; tone = "ok"; }
      else msg = "Trafione rundy: " + score.good + " z " + score.total + ".";
      if (banner) {
        if (revealed) banner.removeAttribute("data-tone"); else banner.dataset.tone = tone;
        banner.firstElementChild.textContent = msg;
      }
      if (btnReveal) btnReveal.disabled = revealed || guess.mean == null || guess.median == null;

      if (feedback) {
        feedback.hidden = !revealed;
        if (revealed) {
          var rng = Math.max.apply(null, x) - Math.min.apply(null, x);
          var overall = (Math.abs(guess.mean - m) + Math.abs(guess.median - med)) / rng;
          var grade = overall < 0.08 ? ["Doskonale!", "info"] : overall < 0.15 ? ["Nieźle!", "warning"] : ["Można lepiej!", "danger"];
          feedback.innerHTML = "";
          var v = document.createElement("span");
          v.className = "lc-status-" + grade[1];
          var st = document.createElement("strong"); st.textContent = grade[0] + " ";
          v.appendChild(st);
          feedback.appendChild(v);
          feedback.appendChild(document.createTextNode("Błąd średniej: " + Charts.fmt(Math.abs(guess.mean - m), 1) +
            ", błąd mediany: " + Charts.fmt(Math.abs(guess.median - med), 1) + "."));
        }
      }
      ["mean", "median"].forEach(function (key) {
        var handle = guessHandles[key], unset = guess[key] == null;
        handle.element.hidden = revealed;
        handle.element.setAttribute("class", "lc-chart-handle has-dot" + (unset ? " is-unset" : ""));
        handle.value(unset ? (LO + HI) / 2 : guess[key], LO, HI,
          unset ? "Nie wskazano odpowiedzi. Enter ustawia znacznik w środku osi." : "Twój typ: " + Charts.fmt(guess[key], 1));
      });
      syncHandles();
      root.lcState = { type: round.type, values: x, mean: m, median: med, guess: guess, revealed: revealed, score: score };
    }

    function reveal() {
      if (revealed || guess.mean == null || guess.median == null) return;
      var x = round.values, m = U.mean(x), med = U.median(x);
      var rng = Math.max.apply(null, x) - Math.min.apply(null, x);
      revealed = true;
      score.total++;
      if (Math.abs(guess.mean - m) / rng < 0.08 && Math.abs(guess.median - med) / rng < 0.08) score.good++;
      draw();
    }

    function at(e) { return Math.round(U.valueAt(chart, canvas, e, [LO, HI]).x * 10) / 10; }
    function px(v) { return chart.convertToPixel({ gridIndex: 0 }, [v, 0])[0]; }
    function pixelX(e) { return e.clientX - canvas.getBoundingClientRect().left; }
    canvas.style.touchAction = "none";
    canvas.style.cursor = "crosshair";
    Charts.listen(canvas, "pointerdown", function (e) {
      if (revealed) return;
      var x = pixelX(e), v = at(e), near = null, best = 12;
      ["mean", "median"].forEach(function (k) {
        if (guess[k] != null && Math.abs(px(guess[k]) - x) < best) { best = Math.abs(px(guess[k]) - x); near = k; }
      });
      if (near) drag = near;
      else if (guess.mean == null) { guess.mean = v; drag = "mean"; }
      else if (guess.median == null) { guess.median = v; drag = "median"; }
      if (!drag) return;
      canvas.setPointerCapture(e.pointerId);
      draw();
      e.preventDefault();
    });
    Charts.listen(canvas, "pointermove", function (e) {
      if (drag) { guess[drag] = at(e); draw(); return; }
      if (revealed) { canvas.style.cursor = "default"; return; }
      var x = pixelX(e), near = ["mean", "median"].some(function (k) {
        return guess[k] != null && Math.abs(px(guess[k]) - x) < 12;
      });
      canvas.style.cursor = near ? "ew-resize" : (guess.median == null ? "crosshair" : "default");
    });
    ["pointerup", "pointercancel"].forEach(function (t) {
      Charts.listen(canvas, t, function () { drag = null; });
    });

    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-g-act]");
      if (!b || !root.contains(b)) return;
      if (b.dataset.gAct === "new") newRound();
      if (b.dataset.gAct === "reveal") reveal();
    });

    Charts.zoomControl(root);
    paint();
    newRound();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  Charts.onReady(".lc-guess", init);
})();
