// Wykresy wykładu 07 „Czas życia elementu” liczone w przeglądarce (dawniej ggplot
// na serwerze). Silnik: window.lcSF (shared/lc_stat_figures.js), rozkłady:
// window.lcDist, funkcje kursu: window.lcRisk (R/risk_figures.js).
//
//   c7m_timeline   — Oś czasu: awarie i obserwacje cenzorowane (odczyty z R)
//   c7m_functions  — f(t), F(t), R(t), h(t) rozkładu wykładniczego ze wspólną linią czasu
//   c7m_exp        — R(t) przy stałym hazardzie, sterowane MTTF
//   c7m_gamma      — Gęstość gamma o kształcie k
//   c7m_weibull    — R(t) i hazard Weibulla na dwóch wykresach
//   c7m_same_mean  — Trzy modele Weibulla o tym samym MTTF (skale z R)
//   c7m_bathtub    — Krzywa wannowa: trzy składowe i suma (dno wanny z R)
//   c7m_plan       — Termin przeglądu na krzywej R(t)
//
// Przedrostek c7m_ odróżnia je od rycin prototypów punktów stop (modules/proto_*.js),
// które mają już nazwy c7_gamma i c7_weibull.
// Funkcje liczące są w window.lcC7Charts (testy: tests/testthat/test-c7-wykresy.R).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist, R = root.lcRisk;

  // Pozycja suwaka jako indeks w tablicy odczytów policzonych w R.
  function sliderIndex(v, min, step) { return Math.round((Number(v) - min) / step); }
  // Obserwowane czasy: awaria (czas ≤ koniec obserwacji) albo cenzorowanie w końcu obserwacji.
  function observe(times, follow) {
    return times.map(function (t, i) { return { id: i + 1, time: Math.min(t, follow), failed: t <= follow }; });
  }
  // Cztery funkcje rozkładu wykładniczego o średniej mttf w chwili t.
  function exponential(t, mttf) {
    var r = Math.exp(-t / mttf);
    return { density: r / mttf, cdf: 1 - r, reliability: r, hazard: 1 / mttf };
  }
  // Odczyty panelu gamma (skala 500 h).
  function gammaReads(k) {
    return { mean: k * 500 + " h", mode: Math.max(0, (k - 1) * 500) + " h",
             tail: R.fmtP(D.pgamma(1000, k, 500, false)), family: k % 1 === 0 ? "Erlang" : "gamma" };
  }
  // Składowe hazardu wannowego w chwili t.
  function bathtub(t, wear) {
    var early = 1.2 * Math.exp(-t / 350), w = wear * Math.pow(t / 4000, 3);
    return { early: early, stable: 0.12, wear: w, total: early + 0.12 + w };
  }
  root.lcC7Charts = { sliderIndex: sliderIndex, observe: observe, exponential: exponential,
                      gammaReads: gammaReads, bathtub: bathtub };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S || !D || !R) return;

  var DASH = [6, 5], DOT = [2, 4], figures = {};
  function num(el, id) { return Number(el.querySelector("#" + id).value); }

  figures.c7m_timeline = {
    draw: function (v, P, k, state) {
      var follow = Number(v), obs = observe(state.config.times, follow), layers = [], labels = {};
      layers.push({ type: "vline", x: follow, color: P.ref, width: 1.4, dash: DASH });
      obs.forEach(function (o) {
        labels[o.id] = String(o.id);
        layers.push({ type: "segment", from: [0, o.id], to: [o.time, o.id], color: o.failed ? P.accent : P.secondary, width: 5 });
      });
      layers.push({ type: "points", data: obs.filter(function (o) { return o.failed; }).map(function (o) { return [o.time, o.id]; }),
                    color: P.accent, size: 11 });
      layers.push({ type: "points", data: obs.filter(function (o) { return !o.failed; }).map(function (o) { return [o.time, o.id]; }),
                    color: P.secondary, size: 12, symbol: "triangle" });
      return { x: [0, 2500], y: [0.4, 8.6], layers: layers, yLabels: labels, noYGrid: true,
        legend: [{ color: P.accent, text: "Awaria", box: true }, { color: P.secondary, text: "Cenzorowanie", box: true }],
        xName: "Czas (h)", yName: "Element", aria: "Osiem linii czasu zakończonych awarią lub znacznikiem cenzorowania." };
    },
    update: function (el, state, v) {
      var cfg = state.config, st = cfg.stats[sliderIndex(v, cfg.min, cfg.step)];
      ["tl_fail", "tl_cens", "tl_total", "tl_mean", "tl_mttf"].forEach(function (key, i) { S.setValue(el, key, st[i]); });
    }
  };

  // Rozkład wykładniczy o MTTF = 1500 h; gęstość i hazard przeskalowane, żeby zmieściły się na osi 0–2.
  figures.c7m_functions = {
    draw: function (v, P) {
      var t = Number(v), cols = R.cat(4);
      // Kolejność i barwy jak w dawnej legendzie.
      var funs = [{ name: "F(t)", f: function (x) { return exponential(x, 1500).cdf; } },
                  { name: "f(t) × 3000", f: function (x) { return exponential(x, 1500).density * 3000; } },
                  { name: "h(t) × 1500", f: function (x) { return exponential(x, 1500).hazard * 1500; } },
                  { name: "R(t)", f: function (x) { return exponential(x, 1500).reliability; } }];
      var layers = funs.map(function (fn, i) { return { type: "line", data: R.curve(fn.f, 0, 4000, 400), color: cols[i], width: 2.2 }; });
      layers.push({ type: "vline", x: t, color: P.ref, width: 1.4, dash: DASH });
      funs.forEach(function (fn, i) { layers.push({ type: "point", x: t, y: fn.f(t), color: cols[i], size: 10 }); });
      return { x: [0, 4000], y: [0, 2], layers: layers,
        legend: funs.map(function (fn, i) { return { color: cols[i], text: fn.name }; }),
        xName: "Czas (h)", yName: "Wartość przeskalowana", aria: "Cztery zsynchronizowane funkcje czasu życia ze wspólną linią czasu." };
    },
    update: function (el, state, v) {
      var e = exponential(Number(v), 1500);
      S.setValue(el, "fun_cdf", R.fmtP(e.cdf)); S.setValue(el, "fun_rel", R.fmtP(e.reliability));
      // Wartości bez przeskalowania, jak w przykładzie 7.3.
      S.setValue(el, "fun_dens", e.density.toFixed(6)); S.setValue(el, "fun_haz", e.hazard.toFixed(6));
    }
  };

  figures.c7m_exp = {
    draw: function (v, P) {
      var mttf = Number(v), rel = function (t) { return Math.exp(-t / mttf); }, e1 = Math.exp(-1);
      // Kreska misji 1000 h (odczyt R(1000 h)) i punkt (MTTF, e⁻¹), o którym mówi tekst.
      return { x: [0, 5000], y: [0, 1],
        layers: [{ type: "vline", x: 1000, color: P.ref, width: 1.4, dash: DASH },
                 { type: "line", data: R.curve(rel, 0, 5000, 400), color: P.accent, width: 2.4 },
                 { type: "point", x: 1000, y: rel(1000), color: P.accent, size: 11 },
                 { type: "line", data: [[0, e1], [mttf, e1]], color: P.ref, width: 1.4, dash: DOT },
                 { type: "line", data: [[mttf, 0], [mttf, e1]], color: P.ref, width: 1.4, dash: DOT },
                 { type: "point", x: mttf, y: e1, color: P.secondary, size: 11 }],
        xName: "Czas (h)", yName: "R(t)", aria: "Malejąca wykładnicza krzywa niezawodności." };
    },
    update: function (el, state, v) {
      S.setValue(el, "exp_r1000", R.fmtP(Math.exp(-1000 / Number(v)))); S.setValue(el, "exp_rmttf", R.fmtP(Math.exp(-1)));
    }
  };

  figures.c7m_gamma = {
    draw: function (v, P) {
      // Stałe osie: suwak k zmienia kształt, nie skalę. Dla k < 1 gęstość w zerze jest
      // nieskończona, więc siatka zaczyna się tuż za zerem, a krzywa wychodzi ponad oś.
      var k = Number(v), dens = function (t) { return D.dgamma(t, k, 500); };
      var curve = R.curve(dens, 5, 6000, 500);
      return { x: [0, 6000], y: [0, 0.0021],
        layers: [{ type: "line", data: curve.filter(function (p) { return p[0] >= 1000; }), color: P.secondary, width: 0, fill: P.secondary, fillAlpha: 0.25 },
                 { type: "line", data: curve, color: P.secondary, width: 2.4 },
                 { type: "vline", x: 1000, color: P.ref, width: 1.4, dash: DASH },
                 { type: "vline", x: k * 500, color: P.ref, width: 1.8, dash: DOT }],
        xName: "Czas (h)", yName: "Gęstość (1/h)", aria: "Gęstość rozkładu gamma dla wybranego parametru kształtu." };
    },
    update: function (el, state, v) {
      var r = gammaReads(Number(v));
      S.setValue(el, "gamma_mean", r.mean); S.setValue(el, "gamma_mode", r.mode);
      S.setValue(el, "gamma_tail", r.tail); S.setValue(el, "gamma_family", r.family);
    }
  };

  // R(t) u góry, hazard na dole (tak opisuje je tekst). Oś hazardu jest stała
  // (0–0.004 na godzinę): suwaki zmieniają krzywą, nie skalę; krzywa ponad osią jest ucięta.
  figures.c7m_weibull = {
    draw: function (v, P, k, state, vals) {
      var beta = Number(vals.c7_beta), eta = Number(vals.c7_eta), cols = R.cat(2);
      var mission = { type: "vline", x: 1000, color: P.ref, width: 1.4, dash: DASH };
      if (k === 0) {
        return { x: [0, 5000], y: [0, 1], alignLeft: true,
          layers: [mission,
                   { type: "line", data: R.curve(function (t) { return R.weibull(t, beta, eta).reliability; }, 1, 5000, 500), color: cols[0], width: 2.3 },
                   { type: "point", x: 1000, y: R.weibull(1000, beta, eta).reliability, color: cols[0], size: 11 }],
          xName: "", yName: "", aria: "Krzywa niezawodności Weibulla sterowana parametrami beta i eta." };
      }
      return { x: [0, 5000], y: [0, 0.004], alignLeft: true,
        layers: [mission,
                 { type: "line", data: R.curve(function (t) { return R.weibull(t, beta, eta).hazard; }, 1, 5000, 500), color: cols[1], width: 2.3 }],
        xName: "Czas (h)", yName: "", aria: "Krzywa hazardu Weibulla sterowana parametrami beta i eta." };
    },
    update: function (el) {
      var beta = num(el, "c7_beta"), eta = num(el, "c7_eta");
      S.setValue(el, "weib_dir", beta < 1 ? "maleje" : beta > 1 ? "rośnie" : "stały");
      S.setValue(el, "weib_r", R.fmtP(R.weibull(1000, beta, eta).reliability));
    }
  };

  figures.c7m_same_mean = {
    draw: function (v, P, k, state) {
      var t = Number(v), cfg = state.config, cols = R.cat(3), layers = [];
      cfg.shapes.forEach(function (b, i) {
        layers.push({ type: "line", data: R.curve(function (x) { return R.weibull(x, b, cfg.scales[i]).reliability; }, 0, 3500, 400), color: cols[i], width: 2.2 });
      });
      layers.push({ type: "vline", x: t, color: P.ref, width: 1.4, dash: DASH });
      cfg.shapes.forEach(function (b, i) { layers.push({ type: "point", x: t, y: R.weibull(t, b, cfg.scales[i]).reliability, color: cols[i], size: 11 }); });
      return { x: [0, 3500], y: [0, 1], layers: layers,
        legend: cfg.shapes.map(function (b, i) { return { color: cols[i], text: "β=" + b }; }),
        xName: "Czas (h)", yName: "R(t)", aria: "Trzy krzywe Weibulla o tym samym średnim czasie życia i różnych kształtach." };
    },
    // Trzy wartości R(t) na linii misji, w kolorach krzywych (przykład 7.8).
    update: function (el, state, v) {
      var cfg = state.config;
      cfg.shapes.forEach(function (b, i) { S.setValue(el, "same_r" + (i + 1), R.fmtP(R.weibull(Number(v), b, cfg.scales[i]).reliability)); });
    }
  };

  // Stała oś Y: suwak podnosi prawe ramię (przy dużym nasileniu wychodzi ponad oś),
  // lewe stoi w miejscu. Punkt i przerywana kreska: dno wanny.
  figures.c7m_bathtub = {
    draw: function (v, P, k, state) {
      var wear = Number(v), cfg = state.config, cols = R.cat(4), bottom = cfg.bottom[sliderIndex(v, cfg.min, cfg.step)];
      var parts = [["Wczesne defekty", "early"], ["Losowe awarie", "stable"], ["Zużycie", "wear"], ["Suma", "total"]];
      var layers = parts.map(function (p, i) {
        return { type: "line", data: R.curve(function (t) { return bathtub(t, wear)[p[1]]; }, 1, 4000, 500), color: cols[i], width: p[1] === "total" ? 2.9 : 1.6 };
      });
      layers.push({ type: "vline", x: bottom.t, from: 0, to: bottom.h, color: P.ink, width: 1.4, dash: DASH });
      layers.push({ type: "point", x: bottom.t, y: bottom.h, color: P.accent, size: 11 });
      return { x: [0, 4000], y: [0, 1.4], layers: layers,
        legend: parts.map(function (p, i) { return { color: cols[i], text: p[0] }; }),
        xName: "Czas", yName: "Względny hazard", aria: "Krzywa hazardu w kształcie wanny i jej trzy składowe." };
    },
    update: function (el, state, v) {
      var cfg = state.config, bottom = cfg.bottom[sliderIndex(v, cfg.min, cfg.step)];
      S.setValue(el, "bath_t", bottom.t_text); S.setValue(el, "bath_h", bottom.h_text);
    }
  };

  // R(t) modelu Weibulla (β = 2, η = 1700 h), wymaganie R = 0.90 i wybrany termin;
  // odcinek w kolorze akcentu to ryzyko awarii 1 − R(t).
  figures.c7m_plan = {
    draw: function (v, P) {
      var t = Number(v), rel = function (x) { return R.weibull(x, 2, 1700).reliability; }, now = rel(t);
      return { x: [0, 3000], y: [0, 1],
        layers: [{ type: "segment", from: [t, now], to: [t, 1], color: P.accent, width: 6 },
                 { type: "line", data: [[0, 0.9], [3000, 0.9]], color: P.ref, width: 1.8, dash: DOT },
                 { type: "line", data: R.curve(rel, 0, 3000, 400), color: P.secondary, width: 2.4 },
                 { type: "vline", x: t, from: 0, to: now, color: P.ink, width: 1.4, dash: DASH },
                 { type: "point", x: t, y: now, color: P.accent, size: 11 }],
        xName: "Czas do przeglądu (h)", yName: "R(t)",
        aria: "Krzywa niezawodności Weibulla z wybranym terminem przeglądu, wymaganiem 0.90 i odcinkiem ryzyka awarii przed przeglądem." };
    },
    update: function (el, state, v) {
      var r = R.weibull(Number(v), 2, 1700).reliability;
      S.setValue(el, "plan_r", R.fmtP(r)); S.setValue(el, "plan_risk", R.fmtP(1 - r));
    }
  };

  S.register(figures);
})();
