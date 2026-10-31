// Rozkłady o różnej kurtozie (Ryc. 5.5): gęstość o średniej 0 i SD 1 na tle N(0, 1)
// oraz powiększenie prawego ogona (x > 2.5). Rodziny jak dawniej w R:
//   kurtoza < −0.01: Beta(a, a) przeskalowana do SD = 1, a = max(1.01, (−6/k − 3)/2);
//   |kurtoza| ≤ 0.01: N(0, 1);
//   kurtoza > 0.01: t Studenta przeskalowany do SD = 1, df = max(4.5, 6/k + 4).
// Kurtozę zmienia przeciąganie szczytu w górę lub w dół (krok 0.2 na 6 px ruchu)
// i przyciski [data-k-set]. Kontener .lc-kurt z <canvas data-role="main">,
// <canvas data-role="tail"> i polami [data-role="type|ek|text"].
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
      h = (end - c) / n;
      for (i = 0; i <= n; i++) { x = c + i * h; s += (i === 0 || i === n ? 1 : i % 2 ? 4 : 2) * f(x); }
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
    var cMain = root.querySelector('canvas[data-role="main"]');
    var cTail = root.querySelector('canvas[data-role="tail"]');
    if (!cMain || !cTail) return false;
    var read = {};
    ["type", "ek", "text", "swatch"].forEach(function (k) { read[k] = root.querySelector('[data-role="' + k + '"]'); });
    var ek = parseFloat(root.dataset.ek || "0");
    var XS = [], XT = [], i;
    for (i = 0; i <= 500; i++) XS.push(-5 + i * 0.02);
    for (i = 0; i <= 125; i++) XT.push(TAIL + i * 0.02);
    var NORM_TAIL = upper(dnorm, TAIL);

    function ds(extra) {
      return Object.assign({ data: [], showLine: true, pointRadius: 0, borderWidth: 2 }, extra || {});
    }
    var common = {
      responsive: true, maintainAspectRatio: false, animation: false, locale: "en-US", events: [],
      plugins: { legend: { display: false }, tooltip: { enabled: false }, colors: { enabled: false } }
    };
    var chMain = Charts.create(cMain, {
      type: "scatter",
      data: { datasets: [
        ds({ fill: "origin", borderWidth: 2.5 }),
        ds({ borderDash: [6, 4] }),
        { data: [], pointRadius: 8, borderWidth: 2 }
      ] },
      options: Object.assign({}, common, {
        layout: { padding: { top: 6, right: 12 } },
        scales: {
          x: { type: "linear", min: -5, max: 5, ticks: { stepSize: 1 }, title: { display: true, text: "x" } },
          y: { min: 0, max: 0.55, ticks: { stepSize: 0.1, includeBounds: false },
               title: { display: true, text: "Gęstość" } }
        }
      })
    });
    var chTail = Charts.create(cTail, {
      type: "scatter",
      data: { datasets: [ds({ fill: "origin", borderWidth: 2.5 }), ds({ fill: "origin", borderDash: [6, 4] })] },
      options: Object.assign({}, common, {
        layout: { padding: { top: 6, right: 12 } },
        scales: {
          x: { type: "linear", min: TAIL, max: 5, ticks: { stepSize: 0.5 }, title: { display: true, text: "x" } },
          y: { min: 0, max: 0.02, ticks: { stepSize: 0.005 },
               title: { display: true, text: "Gęstość" } }
        }
      })
    });

    var P = null;
    function paint() {
      P = Charts.palette();
      Charts.styleAxes(chMain, P, { maxRotation: 0 }); Charts.styleAxes(chTail, P, { maxRotation: 0 });
      chMain.data.datasets[1].borderColor = P.ref;
      chTail.data.datasets[1].borderColor = P.ref;
      chTail.data.datasets[1].backgroundColor = Charts.alpha(P.ref, 0.15);
      chMain.update(); chTail.update();
    }

    function draw() {
      var f = density(ek), k = kind(ek), col = P[k.color];
      var m = chMain.data.datasets, t = chTail.data.datasets;
      m[0].data = XS.map(function (x) { return { x: x, y: f(x) }; });
      m[1].data = XS.map(function (x) { return { x: x, y: dnorm(x) }; });
      m[2].data = [{ x: 0, y: f(0) }];
      m[0].borderColor = col; m[0].backgroundColor = Charts.alpha(col, 0.3);
      m[2].pointBackgroundColor = col; m[2].pointBorderColor = P.surface;
      t[0].data = XT.map(function (x) { return { x: x, y: f(x) }; });
      t[1].data = XT.map(function (x) { return { x: x, y: dnorm(x) }; });
      t[0].borderColor = col; t[0].backgroundColor = Charts.alpha(col, 0.3);
      chMain.update("none"); chTail.update("none");

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
    Charts.listen(cMain, "pointerdown", function (e) {
      drag = { y: e.clientY, ek: ek };
      cMain.setPointerCapture(e.pointerId);
      e.preventDefault();
    });
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

    paint();
    draw();
    Charts.onTheme(root, function () { paint(); draw(); });
  }

  window.lc01.kurtDensity = density;
  window.lc01.kurtUpper = upper;
  Charts.onReady(".lc-kurt", init);
})();
