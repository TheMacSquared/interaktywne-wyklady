// Ryciny wykładu 10 „Od modelu do decyzji” liczone w przeglądarce (dawniej wykresy
// ggplot i odczyty z serwera). Silnik: window.lcSF (shared/lc_stat_figures.js),
// funkcje kursu: window.lcRisk (R/risk_figures.js). Przedrostek i10m_ odróżnia je
// od rycin prototypów punktów stop (modules/proto_*.js).
//
// Wszystkie panele liczą na jednym kontrakcie misji (mission(), odpowiednik
// risk_mission_analysis() z R/risk_math.R). Ustawienia — czas misji, model wentylatora,
// R zasilania i sterownika, P(I), czułość, budżet, zakres u — stoją na kartach w różnych
// rozdziałach; settings() czyta je z całego dokumentu, a zmiana dowolnego z nich
// odświeża wszystkie ryciny (zdarzenie lc:sf-redraw).
//
//   i10m_alarm          — Karta 1: P(I | alarm) i P(D | I) (same odczyty)
//   i10m_inspection     — Karta 2: partia zaworów (same odczyty)
//   i10m_life           — Karta 3: oba modele czasu życia wentylatora
//   i10m_system         — Redukcja: elementy systemu w tej samej misji (same odczyty)
//   i10m_fta            — Integracja: P(I), P(D | I), P(S | I) i P(TOP)
//   i10m_interventions  — Opcje: P(TOP) po każdym działaniu, budżet
//   i10m_scenarios      — Niepewność: opcje w trzech scenariuszach
//   i10m_memo           — Notatka: wynik, działanie, kryterium (sam tekst)
//
// Funkcje liczące są w window.lcI10Charts (testy: tests/testthat/test-i10-wykresy.R).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var R = root.lcRisk;

  // risk_parallel_reliability(rep(r, k)).
  function parallel(r, k) { var q = 1; for (var i = 0; i < k; i++) q *= 1 - r; return 1 - q; }
  function lifeCurve(model, t) { var x = t / 1700; return model === "exp" ? Math.exp(-t / 1500) : Math.exp(-(x * x)); }
  // risk_mission_analysis(): jeden kontrakt misji dla kart, interwencji, scenariuszy i rekomendacji.
  // s: {time, model, power, controller, init, sens}; id: "none" albo interwencja; stress: mnożnik scenariusza.
  function mission(s, id, stress) {
    var m = stress == null ? 1 : stress;
    // Scenariusz skaluje skumulowany hazard, zachowując spójność czasu.
    var fan = Math.pow(lifeCurve(s.model, s.time), m);
    var power = Math.pow(s.power, s.time / 1000 * m), controller = Math.pow(s.controller, s.time / 1000 * m);
    var init = Math.min(1, s.init * m), miss = Math.min(1, (1 - s.sens) * m);
    // Założona skuteczność działania: 50% bazowo, słabsza w ostrożnym scenariuszu.
    var efficacy = Math.max(0, Math.min(1, 0.5 * (2 - m))), branches = 2;
    if (id === "detector") miss = miss * (1 - efficacy);
    else if (id === "prevention") init = init * (1 - efficacy);
    else if (id === "power") power = parallel(power, 2);
    else if (id === "fan") branches = 3;
    var par = parallel(fan, branches), system = power * controller * par;
    return { fan_r: fan, power_r: power, controller_r: controller, parallel_r: par, system_r: system,
             initiation: init, miss: miss, cooling_failure: 1 - system,
             top: init * (1 - (1 - miss) * (1 - (1 - system))), efficacy: efficacy };
  }
  // risk_bayes(), risk_at_least_one(), risk_zero_failure_upper(n, 0.95).
  function bayes(prev, sens, fpr) { var den = sens * prev + fpr * (1 - prev); return den === 0 ? NaN : sens * prev / den; }
  function atLeastOne(n, p) { return 1 - Math.pow(1 - p, n); }
  function zeroFailureUpper(n) { return -Math.expm1(Math.log1p(-0.95) / n); }
  // risk_format_probability(): liczba i procent w nawiasie.
  function formatProbability(x) {
    if (typeof x !== "number" || !isFinite(x)) return "—";
    var d = 3;
    if (x > 0 && x < 0.01) d = Math.max(3, Math.min(12, Math.ceil(-Math.log10(x)) + 2));
    return R.fixed(x, d) + " (" + R.fixed(100 * x, Math.max(1, d - 2)) + "%)";
  }
  // Wyniki trzech scenariuszy (optymistyczny, bazowy, ostrożny) dla każdej interwencji.
  function scenarios(s, ids, u) {
    return [1 - u, 1, 1 + u].map(function (m) { return ids.map(function (id) { return mission(s, id, m).top; }); });
  }
  // Najlepsze działania w budżecie w jednym scenariuszu; równe wyniki razem („A / B”).
  function winners(results, options, budget) {
    var ok = options.map(function (o, i) { return i; }).filter(function (i) { return options[i].cost_index <= budget; });
    if (!ok.length) return "";
    var best = Math.min.apply(null, ok.map(function (i) { return results[i]; }));
    return ok.filter(function (i) { return Math.abs(results[i] - best) < 1e-12; }).map(function (i) { return options[i].label; }).join(" / ");
  }
  function settingsLabel(s) {
    return (s.model === "exp" ? "wykładniczy" : "Weibull") + " · " + s.time + " h · P(I) " + R.fmtP(s.init) + " · czułość " + R.fmtP(s.sens);
  }
  root.lcI10Charts = { mission: mission, lifeCurve: lifeCurve, bayes: bayes, atLeastOne: atLeastOne,
    zeroFailureUpper: zeroFailureUpper, formatProbability: formatProbability, scenarios: scenarios,
    winners: winners, settingsLabel: settingsLabel, parallel: parallel };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S || !R) return;

  var DASH = [6, 5], figures = {};
  var DEFAULTS = { i10_time: 1000, i10_life_model: "exp", i10_power: 0.98, i10_controller: 0.95, i10_init: 0.005,
                   i10_sens: 0.95, i10_budget: 2, i10_uncertainty: 0.2 };
  function control(id) {
    var el = document.getElementById(id);
    if (!el) return DEFAULTS[id];
    return el.tagName === "SELECT" ? el.value : Number(el.value);
  }
  // Ustawienia kart z całego dokumentu.
  function settings() {
    return { time: control("i10_time"), model: control("i10_life_model"), power: control("i10_power"),
             controller: control("i10_controller"), init: control("i10_init"), sens: control("i10_sens") };
  }
  // Zmiana ustawienia na dowolnej karcie odświeża wszystkie ryciny wykładu.
  function refresh(e) {
    if (!e.target || !Object.prototype.hasOwnProperty.call(DEFAULTS, e.target.id)) return;
    document.querySelectorAll('.lc-sf[data-fig^="i10m_"]').forEach(function (fig) {
      if (!fig.contains(e.target)) fig.dispatchEvent(new CustomEvent("lc:sf-redraw"));
    });
  }
  document.addEventListener("input", refresh);
  document.addEventListener("change", refresh);

  var MODEL_NAMES = { exp: "Wykładniczy", weibull: "Weibull" };
  function byId(options, id) { return options.filter(function (o) { return o.id === id; })[0]; }
  function ids(options) { return options.map(function (o) { return o.id; }); }

  figures.i10m_alarm = {
    update: function (el) {
      var s = settings();
      S.setValue(el, "alarm_post", R.fmtP(bayes(s.init, s.sens, Number(el.querySelector("#i10_fpr").value))));
      S.setValue(el, "alarm_miss", R.fmtP(1 - s.sens));
    }
  };

  figures.i10m_inspection = {
    update: function (el) {
      var n = Number(el.querySelector("#i10_n").value), p = Number(el.querySelector("#i10_p").value);
      S.setValue(el, "insp_mean", R.rnd(n * p, 2));
      S.setValue(el, "insp_any", R.fmtP(atLeastOne(n, p)));
      S.setValue(el, "insp_upper", R.fmtP(zeroFailureUpper(n)));
    }
  };

  // Oba modele na stałych osiach (0–3000 h, 0–1): wybrany linią ciągłą, drugi
  // przerywaną, żeby było widać przecięcie krzywych z przykładu 10.5.
  figures.i10m_life = {
    draw: function (v, P) {
      var s = settings(), other = s.model === "exp" ? "weibull" : "exp";
      return { x: [0, 3000], y: [0, 1], xTicks: [0, 500, 1000, 1500, 2000, 2500, 3000],
        layers: [{ type: "line", data: R.curve(function (t) { return lifeCurve(other, t); }, 0, 3000, 400), color: P.ref, width: 1.8, dash: DASH },
                 { type: "line", data: R.curve(function (t) { return lifeCurve(s.model, t); }, 0, 3000, 400), color: P.accent, width: 2.2 },
                 { type: "vline", x: s.time, color: P.ref, width: 1.4, dash: DASH },
                 { type: "point", x: s.time, y: lifeCurve(s.model, s.time), color: P.accent, size: 11 }],
        xName: "Czas (h)", yName: "R(t)",
        aria: "Krzywe obu modeli czasu życia wentylatora; wybrany model linią ciągłą, z zaznaczonym czasem misji." };
    },
    update: function (el) {
      var s = settings(), other = s.model === "exp" ? "weibull" : "exp";
      S.setValue(el, "life_r", R.fmtP(mission(s, "none").fan_r));
      S.setValue(el, "life_other_name", MODEL_NAMES[other]);
      S.setValue(el, "life_other", R.fmtP(lifeCurve(other, s.time)));
    }
  };

  figures.i10m_system = {
    update: function (el) {
      var s = settings(), b = mission(s, "none");
      S.setValue(el, "sys_model", MODEL_NAMES[s.model]); S.setValue(el, "sys_time", s.time + " h");
      S.setValue(el, "sys_power", R.fmtP(b.power_r)); S.setValue(el, "sys_controller", R.fmtP(b.controller_r));
      S.setValue(el, "sys_parallel", R.fmtP(b.parallel_r)); S.setValue(el, "sys_system", R.fmtP(b.system_r));
    }
  };

  figures.i10m_fta = {
    draw: function (v, P) {
      var b = mission(settings(), "none"), cols = R.cat(4), layers = [], labels = {};
      // Kolejność jak w drzewie (nie alfabetyczna), stała oś 0–1 i wartości nad słupkami.
      var nodes = [["P(I)", b.initiation], ["P(D | I)", b.miss], ["P(S | I)", b.cooling_failure], ["P(TOP)", b.top]];
      nodes.forEach(function (n, i) {
        labels[i + 1] = n[0];
        layers.push({ type: "bins", data: [[i + 0.55, i + 1.45, n[1]]], color: cols[i], stroke: P.surface });
      });
      nodes.forEach(function (n, i) { layers.push({ type: "text", x: i + 1, y: n[1], text: R.fmtP(n[1]), color: P.ink }); });
      return { x: [0.4, 4.6], y: [0, 1], layers: layers, xLabels: labels, xName: "", yName: "Prawdopodobieństwo",
        aria: "Prawdopodobieństwo inicjacji, warunkowe prawdopodobieństwa niepowodzeń oraz wynik na jedną misję." };
    },
    update: function (el) {
      var s = settings(), top = mission(s, "none").top;
      S.setValue(el, "fta_top", R.fmtP(top)); S.setValue(el, "fta_freq", R.naturalFrequency(top, 10000));
      S.setValue(el, "fta_settings", settingsLabel(s));
    }
  };

  figures.i10m_interventions = {
    draw: function (v, P, k, state, vals) {
      var s = settings(), options = state.config.options, budget = Number(vals.i10_budget), cat = R.cat(2);
      var rows = options.map(function (o) { return { o: o, top: mission(s, o.id).top }; });
      // Najmniejsze ryzyko na dole; równe wyniki w kolejności alfabetycznej (jak reorder() w R).
      rows.sort(function (a, b) { return a.top - b.top || (a.o.label < b.o.label ? -1 : 1); });
      var layers = [], labels = {}, max = 0;
      // Stałe przypisanie kolorów, kreska stanu bazowego i obrys wybranej interwencji.
      rows.forEach(function (r, i) {
        labels[i + 1] = r.o.wrapped; max = Math.max(max, r.top);
        var chosen = r.o.id === vals.i10_intervention;
        layers.push({ type: "hbins", data: [[i + 0.55, i + 1.45, r.top]], color: r.o.cost_index <= budget ? cat[1] : cat[0],
                      stroke: chosen ? P.ink : undefined, lineWidth: 3 });
      });
      layers.push({ type: "vline", x: mission(s, "none").top, color: P.ref, width: 1.4, dash: DASH });
      return { x: [0, max * 1.08], y: [0.4, 4.6], layers: layers, yLabels: labels, noYGrid: true,
        legend: [{ color: cat[1], text: "W budżecie", box: true }, { color: cat[0], text: "Poza budżetem", box: true }],
        xName: "P(TOP) na misję (linia przerywana: bez działania)", yName: "",
        aria: "Porównanie ryzyka po czterech działaniach z oznaczeniem dostępności w budżecie." };
    },
    update: function (el, state) {
      var o = byId(state.config.options, el.querySelector("#i10_intervention").value);
      S.setValue(el, "int_top", R.fmtP(mission(settings(), o.id).top));
      S.setValue(el, "int_cost", String(o.cost_index)); S.setValue(el, "int_feasibility", o.feasibility);
    }
  };

  var SCENARIOS = ["Optymistyczny", "Bazowy", "Ostrożny"];
  figures.i10m_scenarios = {
    draw: function (v, P, k, state) {
      var s = settings(), options = state.config.options, budget = control("i10_budget"), cols = R.cat(3);
      var res = scenarios(s, ids(options), Number(v)), layers = [], labels = {};
      // Stała oś: największy wynik przy u = 0.5 dla bieżących ustawień kart, więc
      // suwak u zmienia długość słupków, a nie skalę.
      var max = Math.max.apply(null, ids(options).map(function (id) { return mission(s, id, 1.5).top; }));
      options.forEach(function (o, i) {
        // Kolejność opcji jak w tabeli interwencji (pierwsza u góry), w grupie optymistyczny u góry.
        var c = options.length - i;
        labels[c] = o.wrapped;
        SCENARIOS.forEach(function (name, j) {
          var lo = c + 0.15 - j * 0.3;
          layers.push({ type: "hbins", data: [[lo, lo + 0.3, res[j][i]]], color: cols[j], alpha: o.cost_index <= budget ? 1 : 0.35 });
        });
      });
      return { x: [0, max * 1.04], y: [0.4, options.length + 0.6], layers: layers, yLabels: labels, noYGrid: true,
        legend: SCENARIOS.map(function (name, j) { return { color: cols[j], text: name, box: true }; }),
        xName: "P(TOP) po działaniu", yName: "", aria: "Trzy scenariusze ryzyka po każdej interwencji." };
    },
    update: function (el, state, v) {
      var s = settings(), options = state.config.options, budget = control("i10_budget");
      var res = scenarios(s, ids(options), Number(v));
      var text = SCENARIOS.map(function (name, j) { return name + ": " + winners(res[j], options, budget); }).join("; ");
      S.setValue(el, "scen_text", text + ". Ranking minimalnego P(TOP) w budżecie " + budget +
        " (opcje poza budżetem są wyblakłe); równe wyniki pokazano razem. To nie jest przedział ufności. Ustawienia z kart: " +
        settingsLabel(s) + ".");
    }
  };

  figures.i10m_memo = {
    update: function (el, state) {
      var s = settings(), options = state.config.options, budget = control("i10_budget"), u = control("i10_uncertainty");
      var o = byId(options, el.querySelector("#i10_recommend").value), target = Number(el.querySelector("#i10_target").value);
      var top = mission(s, "none").top, at = options.indexOf(o);
      var worst = Math.max.apply(null, scenarios(s, ids(options), u).map(function (row) { return row[at]; }));
      var affordable = o.cost_index <= budget, meets = worst <= target;
      S.setValue(el, "memo_1", "Przy misji " + s.time + " h bazowe P utraty ochrony wynosi " + formatProbability(top) + "; to " +
        R.naturalFrequency(top, 10000) + " porównywalnych misji.");
      S.setValue(el, "memo_2", "Rozważamy: " + o.label + "; koszt " + o.cost_index + ", " + (affordable ? "w budżecie" : "poza budżetem") +
        "; P(TOP) po działaniu bazowo " + formatProbability(mission(s, o.id).top) + ", w najgorszym rozpatrzonym scenariuszu " +
        formatProbability(worst) + ".");
      S.setValue(el, "memo_3", "Demonstracyjny limit " + formatProbability(target) +
        (meets ? " jest spełniony w badanych scenariuszach" : " nie jest spełniony") + "; " +
        (affordable && meets ? "wariant można przekazać do oceny skutków i wdrożenia" : "potrzebna jest inna opcja, projekt lub budżet") +
        ". Założenia: brak napraw, niezależność detekcji od chłodzenia i skuteczność działań zgodna ze scenariuszem.");
      S.setValue(el, "memo_settings", "Ustawienia z poprzednich rozdziałów: " + settingsLabel(s) + " · budżet " + budget +
        " · zakres u " + u + ".");
    }
  };

  S.register(figures);
})();
