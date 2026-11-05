// Rozkłady o różnej kurtozie (Ryc. 5.5): gęstość o średniej 0 i SD 1 na tle N(0, 1)
// oraz powiększenie prawego ogona (x > 2.5). Rodziny jak dawniej w R:
//   kurtoza < −0.01: Beta(a, a) przeskalowana do SD = 1, a = max(1.01, (−6/k − 3)/2);
//   |kurtoza| ≤ 0.01: N(0, 1);
//   kurtoza > 0.01: t Studenta przeskalowany do SD = 1, df = max(4.5, 6/k + 4).
// Kurtozę zmienia przeciąganie szczytu w górę lub w dół (krok 0.2 na 6 px ruchu)
// i przyciski [data-k-set]. Kontener .lc-kurt z <div class="lc-ec" data-role="main">,
// <div class="lc-ec" data-role="tail"> i polami [data-role="type|ek|text"]. ECharts.
(function () {
  var Charts = window.lcCharts;
  var U = window.lc01;
  if (!U) return;
  var KMIN = -1.2, KMAX = 6, STEP = 0.2, PX_PER_STEP = 6, TAIL = 2.5;

  // lgamma: Lanczos (g = 7, 9 współczynników), błąd względny ~1e-15.
  var LG = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
    -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6,
    1.5056327351493116e-7];
  function lgamma(z) {
    if (z < 0.5) return Math.log(Math.PI / Math.abs(Math.sin(Math.PI * z))) - lgamma(1 - z);
    z -= 1;
    var x = LG[0], t = z + 7.5;
    for (var i = 1; i < 9; i++) x += LG[i] / (z + i);
    return 0.5 * Math.log(2 * Math.PI) + (z + 0.5) * Math.log(t) - t + Math.log(x);
  }
  function dnorm(x) { return Math.exp(-x * x / 2) / Math.sqrt(2 * Math.PI); }
  function dt(x, df) {
    return Math.exp(lgamma((df + 1) / 2) - lgamma(df / 2) - 0.5 * Math.log(df * Math.PI) -
      (df + 1) / 2 * Math.log(1 + x * x / df));
  }
  function dbeta(u, a, b) {
    if (u <= 0 || u >= 1) return 0;
    return Math.exp((a - 1) * Math.log(u) + (b - 1) * Math.log(1 - u) - (lgamma(a) + lgamma(b) - lgamma(a + b)));
  }
  // Gęstość o nadwyżkowej kurtozie ek (średnia 0, SD 1).
  function density(ek) {
    if (ek < -0.01) {
      var a = Math.max(1.01, (-6 / ek - 3) / 2), s = Math.sqrt(2 * a + 1);
      var f = function (x) { return Math.abs(x) > s ? 0 : dbeta((x / s + 1) / 2, a, a) / (2 * s); };
      f.support = s;
      return f;
    }
    if (ek <= 0.01) return dnorm;
    var df = Math.max(4.5, 6 / ek + 4), sd = Math.sqrt(df / (df - 2));
    return function (x) { return dt(x * sd, df) * sd; };
  }
  // P(X > c): całka Simpsona po podstawieniu x = c + u/(1 − u), u ∈ [0, 1).
  function upper(f, c) {
    var end = f.support != null ? f.support : Infinity;
    if (end <= c) return 0;
    var n = 4000, h, s = 0, i, x;
    if (isFinite(end)) {
      // x = end − (end − c)·u⁴ wygładza gęstość przy brzegu nośnika beta.
      // Zwykła siatka x zaniżała całkę dla kształtu bliskiego rozkładowi jednostajnemu.
      h = 1 / n;
      for (i = 0; i <= n; i++) {
        var u = i * h, u2 = u * u;
        x = end - (end - c) * u2 * u2;
        s += (i === 0 || i === n ? 1 : i % 2 ? 4 : 2) * f(x) * 4 * (end - c) * u2 * u;
      }
      return s * h / 3;
    }
    var ue = 1 - 1e-6; h = ue / n;
    for (i = 0; i <= n; i++) {
      var u = i * h; x = c + u / (1 - u);
      s += (i === 0 || i === n ? 1 : i % 2 ? 4 : 2) * f(x) / ((1 - u) * (1 - u));
    }
    return s * h / 3;
  }

  function kind(ek) {
    if (ek < -0.1) return { name: "Platykurtyczny", color: "bursztyn" };
    if (ek > 0.1) return { name: "Leptokurtyczny", color: "accent" };
    return { name: "Mezokurtyczny", color: "szalwia" };
  }
  function verdict(ek) {
    if (ek < -0.5) return ["Platykurtyczny", "warning", "Ogony lżejsze niż w rozkładzie normalnym — wartości skrajne są rzadsze."];
    if (ek < 0.5) return ["Mezokurtyczny", "info", "Ogony zbliżone do rozkładu normalnego."];
    return ["Leptokurtyczny", "danger", "Ogony cięższe niż w rozkładzie normalnym — wartości skrajne są częstsze."];
  }

  function init(root) {
    var cMain = root.querySelector('.lc-ec[data-role="main"]');
    var cTail = root.querySelector('.lc-ec[data-role="tail"]');
    if (!cMain || !cTail) return false;
    var read = {};
    ["type", "ek", "text", "swatch"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });
    var ek = parseFloat(root.dataset.ek || "0");
    var XS = [], XT = [], i;
    for (i = 0; i <= 500; i++) XS.push(-5 + i * 0.02);
    for (i = 0; i <= 125; i++) XT.push(TAIL + i * 0.02);
    var NORM_TAIL = upper(dnorm, TAIL);

    var layer = Charts.handleLayer(root);
    var handle = layer.add(cMain, {
      label: "Kurtoza nadwyżkowa", orientation: "vertical",
      hint: "Góra zwiększa kurtozę, dół ją zmniejsza, co 0.2. Shift: krok 1. Home i End: krańce zakresu.",
      onKey: function (event) {
        var value = Charts.keyValue(event, ek, KMIN, KMAX, STEP, 1);
        if (value === null) return false;
        setEk(value); layer.announce("Kurtoza nadwyżkowa " + Charts.fmt(ek, 1) + ".");
        return true;
      },
      onPointerDown: function (event) { startDrag(event); }
    });
    function positionHandle() {
      var q = chMain.convertToPixel({ gridIndex: 0 }, [0, density(ek)(0)]);
      if (q) handle.position(q[0], q[1]);
    }
    function curve(id, extra) {
      return Object.assign({ id: id, type: "line", data: [], symbol: "none", silent: true, animation: false,
                             lineStyle: { width: 2 } }, extra || {});
    }
    function opts(x, y, series, onResize) {
      return [{ animation: false, aria: { enabled: false }, grid: { left: 8, right: 12, top: 6, bottom: 8 },
                xAxis: Object.assign({ type: "value", name: "x" }, x),
                yAxis: Object.assign({ type: "value", min: 0, name: "Gęstość" }, y), series: series },
              { onResize: onResize }];
    }
    var mo = opts({ min: -5, max: 5, interval: 1 }, { max: 0.55, interval: 0.1 }, [
      curve("f", { areaStyle: {}, lineStyle: { width: 2.5 } }),
      curve("norm", { lineStyle: { width: 2, type: [6, 4] } }),
      { id: "dot", type: "scatter", data: [], symbolSize: 16, silent: true, animation: false }
    ], positionHandle);
    var chMain = Charts.chart(cMain, mo[0], mo[1]);
    var to = opts({ min: TAIL, max: 5, interval: 0.5 }, { max: 0.02, interval: 0.005 }, [
      curve("f", { areaStyle: {}, lineStyle: { width: 2.5 } }),
      curve("norm", { areaStyle: {}, lineStyle: { width: 2, type: [6, 4] } })
    ]);
    var chTail = Charts.chart(cTail, to[0], to[1]);

    var P = null;
    function paint() {
      P = Charts.palette();
      var fs = Charts.axisFontSize(root);
      [[chMain, 1], [chTail, 3]].forEach(function (pair) {
        var y = Charts.ecAxis(P, fs, { nameGap: Math.round(fs * (pair[1] === 1 ? 2.6 : 3.6)) });
        y.axisLabel.showMaxLabel = false;
        pair[0].setOption({ xAxis: Charts.ecAxis(P, fs, { nameGap: Math.round(fs * 1.9) }), yAxis: y,
          series: [{ id: "norm", lineStyle: { color: P.ref } }] });
      });
      chTail.setOption({ series: [{ id: "norm", areaStyle: { color: Charts.alpha(P.ref, 0.15) } }] });
    }

    function draw() {
      var f = density(ek), k = kind(ek), col = P[k.color];
      chMain.setOption({ series: [
        { id: "f", data: XS.map(function (x) { return [x, f(x)]; }), lineStyle: { color: col }, areaStyle: { color: Charts.alpha(col, 0.3) } },
        { id: "norm", data: XS.map(function (x) { return [x, dnorm(x)]; }) },
        { id: "dot", data: [[0, f(0)]], itemStyle: { color: col, borderColor: P.surface, borderWidth: 2 } }
      ] });
      chTail.setOption({ series: [
        { id: "f", data: XT.map(function (x) { return [x, f(x)]; }), lineStyle: { color: col }, areaStyle: { color: Charts.alpha(col, 0.3) } },
        { id: "norm", data: XT.map(function (x) { return [x, dnorm(x)]; }) }
      ] });

      var pt = upper(f, TAIL), v = verdict(ek);
      if (read.type) read.type.textContent = k.name;
      if (read.ek) read.ek.textContent = Charts.fmt(ek, 1);
      var box = read.ek && read.ek.closest(".lc-read");
      if (box) box.style.setProperty("--lc-read-color", k.color === "accent" ? "var(--upwr-accent)" : "var(--upwr-cat-" + k.color + ")");
      if (read.text) {
        read.text.innerHTML = "";
        var sp = document.createElement("span"); sp.className = "lc-status-" + v[1];
        var st = document.createElement("strong");
        st.textContent = v[0] + " (nadwyżkowa kurtoza = " + Charts.fmt(ek, 1) + "):";
        sp.appendChild(st);
        read.text.appendChild(sp);
        read.text.appendChild(document.createTextNode(" " + v[2] + " P(X > 2.5) = " + Charts.fmt(100 * pt, 2) +
          "%, w rozkładzie normalnym " + Charts.fmt(100 * NORM_TAIL, 2) + "%."));
      }
      root.querySelectorAll("[data-k-set]").forEach(function (b) {
        b.setAttribute("aria-pressed", String(Math.abs(+b.dataset.kSet - ek) < 1e-9));
      });
      handle.value(ek, KMIN, KMAX, "Kurtoza nadwyżkowa " + Charts.fmt(ek, 1));
      positionHandle();
      root.lcState = { ek: ek, peak: f(0), tail: pt, normTail: NORM_TAIL, f: f };
    }

    function setEk(v) {
      v = Math.round(Math.max(KMIN, Math.min(KMAX, v)) / STEP) * STEP;
      v = Math.round(v * 10) / 10;
      if (v !== ek) { ek = v; draw(); }
    }

    // Przeciąganie: ruch w górę zwiększa kurtozę (wyższy, węższy szczyt i cięższe ogony).
    var drag = null;
    cMain.style.touchAction = "none";
    cMain.style.cursor = "ns-resize";
    function startDrag(e) {
      drag = { y: e.clientY, ek: ek };
      cMain.setPointerCapture(e.pointerId);
      e.preventDefault();
    }
    Charts.listen(cMain, "pointerdown", startDrag);
    Charts.listen(cMain, "pointermove", function (e) {
      if (!drag) return;
      setEk(drag.ek + Math.round((drag.y - e.clientY) / PX_PER_STEP) * STEP);
    });
    ["pointerup", "pointercancel"].forEach(function (tp) {
      Charts.listen(cMain, tp, function () { drag = null; });
    });
    Charts.listen(root, "click", function (e) {
      var b = e.target.closest("[data-k-set]");
      if (!b || !root.contains(b)) return;
      setEk(+b.dataset.kSet);
    });

    Charts.zoomControl(root);
    paint();
    draw();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  window.lc01.kurtDensity = density;
  window.lc01.kurtUpper = upper;
  Charts.onReady(".lc-kurt", init);
})();
