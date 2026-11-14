// Wykresy wykładu 06 „Zmienność i próg” liczone w przeglądarce (dawniej ggplot
// na serwerze). Silnik: window.lcSF (shared/lc_stat_figures.js), rozkłady:
// window.lcDist, funkcje kursu: window.lcRisk (R/risk_figures.js).
//
//   z6_hist    — Histogram stabilizuje się wraz z liczebnością (dane z R, ziarno 606)
//   z6_normal  — Przesuń i rozszerz krzywą (μ, σ)
//   z6_tail    — Próg temperatury łożyska: pole ogona N(82, 3)
//   z6_action  — Która interwencja najbardziej zmienia ogon?
//   z6_ls      — Pary obciążenie–wytrzymałość (odchylenia z R, ziarno 607)
//   z6_shapes  — Trzy rozkłady o podobnym centrum: histogram albo wykres kwantylowy (dane z R)
//
// Funkcje liczące są w window.lcZ6Charts (testy: tests/js/z6_wykresy.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist, R = root.lcRisk;

  // Histogram w skali gęstości na stałych granicach co `width` od `from` do `to`;
  // przedziały domknięte z prawej, pierwszy także z lewej (jak geom_histogram(breaks = …)).
  // Gęstość liczona względem obserwacji mieszczących się w zakresie, jak w ggplot.
  function densityBins(values, from, to, width) {
    var k = Math.round((to - from) / width), counts = [], inside = 0, i;
    for (i = 0; i < k; i++) counts.push(0);
    values.forEach(function (v) {
      if (v < from || v > to) return;
      var j = v === from ? 0 : Math.ceil((v - from) / width - 1e-12) - 1;
      counts[Math.max(0, Math.min(k - 1, j))] += 1; inside += 1;
    });
    return counts.map(function (c, j) { return [from + j * width, from + (j + 1) * width, inside ? c / inside / width : 0]; });
  }
  // Odczyty próby: średnia, SD i udział ponad progiem jak w dawnym panelu.
  function sampleStats(values, threshold) {
    var over = values.filter(function (v) { return v > threshold; }).length;
    return { mean: R.rnd(R.mean(values), 2), sd: R.rnd(R.sd(values), 2),
      over: over + " z " + values.length + " (" + (100 * over / values.length).toFixed(1) + "%)" };
  }
  // Pary (L, S) z odchyleń standaryzowanych i podział na awarie (L > S) i rezerwę.
  function loadStrength(cfg, load, strength) {
    var fail = [], ok = [];
    cfg.zl.forEach(function (z, i) {
      var l = load + cfg.sd_load * z, s = strength + cfg.sd_strength * cfg.zs[i];
      (l > s ? fail : ok).push([l, s]);
    });
    return { fail: fail, ok: ok };
  }
  root.lcZ6Charts = { densityBins: densityBins, sampleStats: sampleStats, loadStrength: loadStrength };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S || !D || !R) return;

  var X_NAME = "Temperatura (°C)", figures = {};
  function density(m, s) { return function (x) { return D.dnorm(x, m, s); }; }
  // Pole pod krzywą od progu w prawo.
  function tail(curve, threshold, m, s, color, alpha) {
    return { type: "line", data: R.tailCurve(curve, threshold, density(m, s)), color: color, width: 0, fill: color, fillAlpha: alpha };
  }

  figures.z6_hist = {
    draw: function (v, P, k, state) {
      var n = Number(v), values = state.config.values.slice(0, n);
      return { x: [70, 94], y: [0, 0.22],
        layers: [{ type: "bins", data: densityBins(values, 68, 96, 1), color: P.secondary, stroke: P.surface },
                 { type: "line", data: R.normalCurve(82, 3, 70, 94, 300), color: P.accent, width: 2.2 },
                 { type: "vline", x: 85, color: P.ref, width: 1.4, dash: [6, 5] }],
        xName: X_NAME, yName: "Gęstość", aria: "Histogram temperatur z nałożoną krzywą normalną." };
    },
    update: function (el, state, v) {
      var st = sampleStats(state.config.values.slice(0, Number(v)), 85);
      S.setValue(el, "hist_mean", st.mean); S.setValue(el, "hist_sd", st.sd); S.setValue(el, "hist_over", st.over);
    }
  };

  figures.z6_normal = {
    draw: function (v, P, k, state, vals) {
      var m = Number(vals.z6_mean), s = Number(vals.z6_sd), curve = R.normalCurve(m, s, 65, 105, 400);
      // Stała oś Y (szczyt przy σ = 1.5): zwężenie rozkładu widać jako wyższą krzywą.
      return { x: [65, 105], y: [0, 0.28],
        layers: [tail(curve, 85, m, s, P.accent, 0.35),
                 { type: "line", data: R.normalCurve(82, 3, 65, 105, 400), color: P.ref, width: 1.6, dash: [2, 4] },
                 { type: "line", data: curve, color: P.accent, width: 2.4 },
                 { type: "vline", x: m, color: P.ref, width: 1.4, dash: [6, 5] },
                 { type: "vline", x: 85, color: P.secondary, width: 2.2, dash: [6, 5] }],
        xName: X_NAME, yName: "Gęstość", aria: "Krzywa normalna sterowana średnią i odchyleniem standardowym." };
    },
    update: function (el) {
      var m = Number(el.querySelector("#z6_mean").value), s = Number(el.querySelector("#z6_sd").value);
      S.setValue(el, "normal_z", R.rnd((85 - m) / s, 2));
      S.setValue(el, "normal_p", R.fmtP(R.normalExceedance(85, m, s)));
    }
  };

  // Panel progu ma własny, stały model N(82, 3): liczby w tekście nie zależą od suwaków μ i σ.
  figures.z6_tail = {
    draw: function (v, P) {
      var c = Number(v), curve = R.normalCurve(82, 3, 70, 97, 500);
      return { x: [70, 97], y: [0, 0.14],
        layers: [tail(curve, c, 82, 3, P.accent, 0.55),
                 { type: "line", data: curve, color: P.secondary, width: 2.2 },
                 { type: "vline", x: c, color: P.ref, width: 1.4, dash: [6, 5] }],
        xName: X_NAME, yName: "Gęstość", aria: "Krzywa normalna z zacieniowanym obszarem temperatur powyżej progu." };
    },
    update: function (el, state, v) {
      var p = R.normalExceedance(Number(v), 82, 3);
      S.setValue(el, "tail_p", R.fmtP(p)); S.setValue(el, "tail_freq", R.naturalFrequency(p));
    }
  };

  figures.z6_action = {
    draw: function (v, P, k, state) {
      var pars = state.config[v], curve = R.normalCurve(pars[0], pars[1], 70, 96, 500);
      return { x: [70, 96], y: [0, 0.21],
        layers: [tail(curve, pars[2], pars[0], pars[1], P.accent, 0.45),
                 { type: "line", data: R.normalCurve(82, 3, 70, 96, 500), color: P.ref, width: 1.6, dash: [2, 4] },
                 { type: "vline", x: 85, color: P.ref, width: 1.6, dash: [2, 4] },
                 { type: "line", data: curve, color: P.secondary, width: 2.4 },
                 { type: "vline", x: pars[2], color: P.ref, width: 1.4, dash: [6, 5] }],
        xName: X_NAME, yName: "Gęstość",
        aria: "Krzywa normalna po wybranej interwencji na tle stanu bazowego, z zacieniowanym polem ponad progiem." };
    },
    update: function (el, state, v) {
      var pars = state.config[v];
      S.setValue(el, "action_pars", pars.join(" / "));
      S.setValue(el, "action_p", R.fmtP(R.normalExceedance(pars[2], pars[0], pars[1])));
    }
  };

  figures.z6_ls = {
    draw: function (v, P, k, state, vals) {
      var d = loadStrength(state.config, Number(vals.z6_load), Number(vals.z6_strength));
      return { x: [35, 135], y: [45, 145],
        layers: [{ type: "points", data: d.ok, color: P.secondary, size: 7, alpha: 0.55, symbol: "triangle" },
                 { type: "points", data: d.fail, color: P.accent, size: 6, alpha: 0.55 },
                 { type: "line", data: [[45, 45], [135, 135]], color: P.ink, width: 1.4, dash: [6, 5] }],
        legend: [{ color: P.accent, text: "Awaria: L>S", box: true }, { color: P.secondary, text: "Rezerwa: S≥L", box: true }],
        xName: "Obciążenie L", yName: "Wytrzymałość S", aria: "Punkty obciążenia i wytrzymałości po obu stronach linii równości." };
    },
    update: function (el, state) {
      var load = Number(el.querySelector("#z6_load").value), strength = Number(el.querySelector("#z6_strength").value);
      S.setValue(el, "ls_p", R.fmtP(R.stressStrength(load, state.config.sd_load, strength, state.config.sd_strength)));
    }
  };

  figures.z6_shapes = {
    draw: function (v, P, k, state, vals) {
      var d = state.config[vals.z6_shape];
      if (vals.z6_view === "qq") {
        var r = d.qq_range, padX = (r[1] - r[0]) * 0.05, padY = (r[3] - r[2]) * 0.05;
        var x0 = r[0] - padX, x1 = r[1] + padX, a = d.qq_line[0], b = d.qq_line[1];
        return { x: [x0, x1], y: [r[2] - padY, r[3] + padY],
          layers: [{ type: "points", data: d.qq, color: P.secondary, size: 5, alpha: 0.4 },
                   { type: "line", data: [[x0, a + b * x0], [x1, a + b * x1]], color: P.accent, width: 2.2 }],
          xName: "Kwantyle teoretyczne (normalne)", yName: "Kwantyle próby (°C)",
          aria: "Wykres kwantylowy wybranego rozkładu względem modelu normalnego." };
      }
      var top = Math.max.apply(null, d.bins.map(function (b) { return b[2]; }));
      return { x: [65, 105], y: [0, top * 1.05],
        layers: [{ type: "bins", data: d.bins, color: P.secondary, stroke: P.surface },
                 { type: "vline", x: 85, color: P.accent, width: 2.2, dash: [6, 5] }],
        xName: X_NAME, yName: "Liczba obserwacji", aria: "Histogram wybranego rozkładu z zaznaczonym progiem 85°C." };
    },
    // Odczyty z modelu (nie z próby): te same liczby co w przykładzie 6.10.
    update: function (el, state) {
      var d = state.config[el.querySelector("#z6_shape").value];
      S.setValue(el, "shapes_p85", d.p85); S.setValue(el, "shapes_p91", d.p91);
    }
  };

  S.register(figures);
})();
