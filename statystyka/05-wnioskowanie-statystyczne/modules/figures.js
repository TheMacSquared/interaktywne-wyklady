// Ryciny wykładu 05 „Wnioskowanie statystyczne” liczone w przeglądarce (dawniej wykresy
// ggplot i wyjścia serwera). Silnik: window.lcSF (shared/lc_stat_figures.js), rozkłady
// i losowanie: window.lcDist, testy: window.lcStats (shared/lc_stats.js).
// Ryciny na stałych danych i o skończonej liczbie ustawień opisuje R (lc_sf_spec()
// w modules/figures.R); tutaj jest to, co zależy od losowania i suwaków.
//
// Rozdział 1 (logika testu):
//   s05_case   — Ryc. 1.1: eksperyment „telefon w plecaku / na biurku” (losowanie, pudełka)
//   s05_sim    — Ryc. 3.2: powtórzone eksperymenty pod H₀ (histogram różnic)
//   s05_power  — Ryc. 3.1: moc testu i błędy (α, odległość, n)
//   s05_quiz   — Ryc. 3.6: quiz „odrzucić czy nie?” (bez wykresu)
//
// Wynik eksperymentu z Ryc. 1.1 jest wspólny dla Ryc. 3.2 (store.caseData);
// po nowym losowaniu rycina zależna dostaje zdarzenie lc:sf-redraw.
// Funkcje liczące są w window.lcS05 (testy: tests/js/stat05.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist, T = root.lcStats;
  if (!D || !T) return;

  // generate_phone_data(): dwie grupy po n osób, wyniki zaokrąglone do 0.1 pkt.
  function phoneData(r, n) {
    var round1 = function (v) { return Math.round(v * 10) / 10; };
    return { plecak: r.many(n, function () { return round1(r.norm(72, 12)); }),
             biurko: r.many(n, function () { return round1(r.norm(65, 14)); }) };
  }
  // Różnice średnich w k eksperymentach bez efektu: dwie grupy po n z N(70, 13).
  function nullDiffs(r, k, n) {
    return r.many(k, function () {
      var a = 0, b = 0, i;
      for (i = 0; i < n; i++) { a += r.norm(70, 13); b += r.norm(70, 13); }
      return (a - b) / n;
    });
  }
  // Udział różnic co najmniej tak skrajnych jak obserwowana (p z symulacji).
  function simP(diffs, obs) {
    return diffs.filter(function (d) { return Math.abs(d) >= Math.abs(obs); }).length / diffs.length;
  }
  // Moc testu dwustronnego dla średniej: H₀ μ = 70, σ = 13, prawdziwa średnia 70 + odległość.
  function power(alpha, effect, n) {
    var se = 13 / Math.sqrt(n), mu0 = 70, mu1 = mu0 + effect;
    var lo = mu0 + D.qnorm(alpha / 2) * se, hi = mu0 + D.qnorm(1 - alpha / 2) * se;
    return { se: se, mu0: mu0, mu1: mu1, lo: lo, hi: hi, power: D.pnorm(lo, mu1, se) + (1 - D.pnorm(hi, mu1, se)) };
  }
  // Decyzja w quizie: odrzucamy, gdy p < α.
  function quizAnswer(sc) { return sc.p < sc.alpha ? "reject" : "fail_to_reject"; }
  root.lcS05 = { phoneData: phoneData, nullDiffs: nullDiffs, simP: simP, power: power, quizAnswer: quizAnswer };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S) return;

  var rng = D.rng(), store = { caseData: null }, figures = {};
  function notify(names) {
    names.forEach(function (name) {
      document.querySelectorAll('.lc-sf[data-fig="' + name + '"]').forEach(function (el) { el.dispatchEvent(new CustomEvent("lc:sf-redraw")); });
    });
  }
  function round(x, d) { return T.round(x, d); }
  // Eksperyment istnieje od wejścia na stronę (jak na serwerze: dane generowane na starcie).
  function caseData() {
    if (!store.caseData) {
      var d = phoneData(rng, 40);
      store.caseData = { plecak: d.plecak, biurko: d.biurko, jitter: rng.many(80, function () { return rng.unif(-0.15, 0.15); }) };
    }
    return store.caseData;
  }
  function observedDiff() { var d = caseData(); return T.mean(d.plecak) - T.mean(d.biurko); }
  function boxStats(x) {
    var q1 = T.quantile(x, 0.25), q3 = T.quantile(x, 0.75), iqr = q3 - q1;
    var inside = x.filter(function (v) { return v >= q1 - 1.5 * iqr && v <= q3 + 1.5 * iqr; });
    return { q1: q1, med: T.median(x), q3: q3, lo: Math.min.apply(null, inside), hi: Math.max.apply(null, inside) };
  }

  figures.s05_case = {
    act: function () { store.caseData = null; caseData(); notify(["s05_sim"]); },
    draw: function (v, P) {
      var d = caseData(), groups = [d.plecak, d.biurko], cols = [P.szalwia, P.bursztyn], layers = [];
      groups.forEach(function (gr, i) {
        var b = boxStats(gr);
        layers.push({ type: "box", stroke: cols[i], boxes: [{ pos: i + 1, width: 0.5, q1: b.q1, med: b.med, q3: b.q3, lo: b.lo, hi: b.hi, fill: C.alpha(cols[i], 0.6) }] });
      });
      groups.forEach(function (gr, i) {
        layers.push({ type: "points", data: gr.map(function (y, j) { return [i + 1 + d.jitter[i * 40 + j], y]; }), color: cols[i], size: 7, alpha: 0.5 });
      });
      return { x: [0.4, 2.6], y: [20, 100], layers: layers, xLabels: { 1: "Telefon w plecaku", 2: "Telefon na biurku" },
        xName: "", yName: "Wynik (0–100 pkt)", aria: "Wykresy pudełkowe wyników koncentracji w dwóch grupach eksperymentu." };
    },
    update: function (el) {
      var d = caseData(), m1 = round(T.mean(d.plecak), 1), m2 = round(T.mean(d.biurko), 1);
      S.setValue(el, "case_a", m1 + " pkt"); S.setValue(el, "case_b", m2 + " pkt");
      // Różnica zaokrąglonych średnich, jak w dawnym odczycie.
      S.setValue(el, "case_diff", round(Number(m1) - Number(m2), 1) + " pkt");
    }
  };

  figures.s05_sim = {
    init: function (state) { state.diffs = []; },
    act: function (state, action) {
      if (action === "reset") state.diffs = [];
      else state.diffs = state.diffs.concat(nullDiffs(rng, action === "sim200" ? 200 : 10, 40));   // n jak w eksperymencie z rozdziału 01
    },
    draw: function (v, P, k, state) {
      if (!state.diffs.length) return null;
      var obs = observedDiff(), bins = S.binsGgplot(state.diffs, 30), top = 0;
      var all = bins.map(function (b) { top = Math.max(top, b.n); return [b.l, b.r, b.n]; });
      // Różnice co najmniej tak skrajne jak obserwowana: u dołu słupka, w kolorze p-wartości.
      var extreme = bins.map(function (b) {
        return [b.l, b.r, state.diffs.filter(function (d) { return d > b.l - 1e-12 && d <= b.r + 1e-12 && Math.abs(d) >= Math.abs(obs); }).length];
      });
      var lo = Math.min(bins[0].l, -Math.abs(obs)), hi = Math.max(bins[bins.length - 1].r, Math.abs(obs)), padX = (hi - lo) * 0.04;
      return { x: [lo - padX, hi + padX], y: [0, top * 1.05],
        layers: [{ type: "bins", data: all, color: P.niebo, stroke: P.surface },
                 { type: "bins", data: extreme, color: P.bursztyn, stroke: P.surface },
                 { type: "vline", x: obs, color: P.accent, width: 3.4 },
                 { type: "vline", x: -obs, color: P.accent, width: 2.2, dash: [6, 5] }],
        legend: [{ color: P.niebo, text: "bliżej zera", box: true }, { color: P.bursztyn, text: "co najmniej tak skrajne", box: true }],
        xName: "Różnica średnich (grupa A − grupa B)", yName: "Liczba",
        aria: "Histogram różnic średnich z powtórzonych eksperymentów bez efektu, z zaznaczoną różnicą obserwowaną." };
    },
    update: function (el, state) {
      var obs = observedDiff(), n = state.diffs.length;
      S.setValue(el, "sim_n", String(n)); S.setValue(el, "sim_obs", round(obs, 1) + " pkt");
      S.setValue(el, "sim_p", n ? simP(state.diffs, obs).toFixed(3) : "—");
    },
    noteKey: function (vals, state) { return state.diffs.length ? "data" : "empty"; }
  };

  figures.s05_power = {
    draw: function (v, P, k, state, vals) {
      var w = power(Number(vals.ch1_alpha), Number(vals.ch1_effect), Number(vals.ch1_power_n));
      var a = w.mu0 - 4 * w.se, b = Math.max(w.mu1, w.mu0) + 4 * w.se, h0 = [], h1 = [], i, x;
      for (i = 0; i < 500; i++) { x = a + (b - a) * i / 499; h0.push([x, D.dnorm(x, w.mu0, w.se)]); h1.push([x, D.dnorm(x, w.mu1, w.se)]); }
      // Ogony poza wartościami krytycznymi: pod H₀ obszar odrzucenia (α), pod Hₐ moc.
      function tails(curve, mean, color) {
        var left = curve.filter(function (p) { return p[0] <= w.lo; }), right = curve.filter(function (p) { return p[0] >= w.hi; });
        left.push([w.lo, D.dnorm(w.lo, mean, w.se)]); right.unshift([w.hi, D.dnorm(w.hi, mean, w.se)]);
        return [left, right].map(function (part) { return { type: "line", data: part, color: color, width: 0, fill: color, fillAlpha: 0.2 }; });
      }
      var top = D.dnorm(w.mu0, w.mu0, w.se);
      return { x: [a, b], y: [0, top * 1.05],
        layers: tails(h0, w.mu0, P.accent).concat(tails(h1, w.mu1, P.szalwia), [
          { type: "line", data: h0, color: P.niebo, width: 2.6 },
          { type: "line", data: h1, color: P.accent, width: 2.6 },
          { type: "vline", x: w.lo, color: P.secondary, width: 1.4, dash: [6, 5] },
          { type: "vline", x: w.hi, color: P.secondary, width: 1.4, dash: [6, 5] }]),
        legend: [{ color: P.niebo, text: "H₀: μ = 70" }, { color: P.accent, text: "Hₐ: μ = 70 + odległość" }],
        xName: "Średnia koncentracja w próbie", yName: "Gęstość",
        aria: "Rozkłady średniej z próby pod hipotezą zerową i alternatywną z obszarami odrzucenia i mocą testu." };
    },
    update: function (el) {
      var num = function (id) { return Number(el.querySelector("#" + id).value); };
      var w = power(num("ch1_alpha"), num("ch1_effect"), num("ch1_power_n"));
      S.setValue(el, "power_value", round(w.power * 100, 1) + "%"); S.setValue(el, "power_beta", round((1 - w.power) * 100, 1) + "%");
    }
  };

  figures.s05_quiz = {
    init: function (state) { state.current = state.config.scenarios[rng.int(state.config.scenarios.length) - 1]; state.answer = null; },
    act: function (state, action) {
      if (action === "next") { state.current = state.config.scenarios[rng.int(state.config.scenarios.length) - 1]; state.answer = null; }
      else if (!state.answer) state.answer = action;
    },
    update: function (el, state) {
      var sc = state.current, correct = quizAnswer(sc), fb = el.querySelector('[data-role="feedback"]');
      S.setValue(el, "quiz_context", sc.context);
      el.querySelector('[data-role="options"]').hidden = !!state.answer;
      if (!state.answer) { fb.innerHTML = ""; return; }
      var cmp = "p = " + sc.p + " " + (sc.p < sc.alpha ? "<" : "≥") + " α = " + sc.alpha;
      fb.innerHTML = state.answer === correct
        ? S.status('<span class="lc-status-ok"><strong>Poprawnie.</strong></span><p>' + S.esc(cmp) + "</p>")
        : S.status('<span class="lc-status-danger"><strong>Niepoprawnie.</strong></span><p>' +
            S.esc(cmp + ", zatem " + (correct === "reject" ? "odrzucamy H₀" : "nie mamy podstaw do odrzucenia H₀") + ".") + "</p>");
    }
  };

  S.register(figures);
})();
