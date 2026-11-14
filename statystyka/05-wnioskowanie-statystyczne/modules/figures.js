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
  // Test t jednej próby: statystyki opisowe, t i p (dwustronne albo w jedną stronę).
  function tOne(x, mu0, alternative) {
    var n = x.length, m = T.mean(x), s = T.sd(x), se = s / Math.sqrt(n), t = (m - mu0) / se, df = n - 1;
    var p = alternative === "less" ? D.pt(t, df) : alternative === "greater" ? 1 - D.pt(t, df) : 2 * D.pt(-Math.abs(t), df);
    return { n: n, mean: m, sd: s, se: se, t: t, df: df, p: p };
  }
  // Histogram na 15 równych przedziałach od min do max (pierwszy z lewym końcem), jak hist() w R.
  function hist15(x) {
    var lo = Math.min.apply(null, x), hi = Math.max.apply(null, x), w = (hi - lo) / 15, counts = [], i;
    for (i = 0; i < 15; i++) counts.push(0);
    x.forEach(function (v) { counts[Math.max(0, Math.min(14, Math.ceil((v - lo) / w - 1e-9) - 1))] += 1; });
    return counts.map(function (c, j) { return [lo + j * w, lo + (j + 1) * w, c]; });
  }
  // Wartości krytyczne rozkładu statystyki testowej (α = 0.05) i funkcja „w obszarze odrzucenia”.
  function critical(type, df, alternative) {
    if (type === "chisq") { var c = D.qchisq(0.95, df); return { values: [c], reject: function (x) { return x >= c; } }; }
    if (alternative === "greater") { var g = D.qt(0.95, df); return { values: [g], reject: function (x) { return x >= g; } }; }
    if (alternative === "less") { var l = D.qt(0.05, df); return { values: [l], reject: function (x) { return x <= l; } }; }
    var t2 = D.qt(0.975, df);
    return { values: [-t2, t2], reject: function (x) { return Math.abs(x) >= t2; } };
  }
  root.lcS05 = { phoneData: phoneData, nullDiffs: nullDiffs, simP: simP, power: power, quizAnswer: quizAnswer,
                 tOne: tOne, hist15: hist15, critical: critical };

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

  // ---- Wspólne części rycin „test krok po kroku” (rozdziały 4–9) ----------------
  // Role elementów jak STEP_ROLES w shared/theme_upwr.R: nowy element kroku w akcencie
  // i grubszą linią, znany w kolorze pomocniczym, tło w kolorze danych.
  function roleColor(P, role) { return role === "new" ? P.accent : role === "known" ? P.secondary : P.niebo; }
  function roleWidth(role) { return role === "new" ? 1.8 : 1.1; }
  function stepRole(step, from) { return step === from ? "new" : "known"; }
  function b(x) { return "<b>" + S.esc(String(x)) + "</b>"; }
  var fmt = S.fmt;
  // Etykieta przy pionowej linii: po prawej albo po lewej stronie (jak hjust = -0.1 / 1.1).
  function lineLabel(x, y, text, color, right) {
    return { type: "text", x: x, y: y, text: text, color: color, bold: true, align: right ? "left" : "right", valign: "middle" };
  }
  // Werdykt kroku decyzji: step_verdict() z helpers.R.
  function verdictHtml(p) {
    var sig = p < 0.05;
    return '<span class="lc-status-' + (sig ? "danger" : "ok") + '">' + (sig ? "Odrzucamy H₀" : "Brak podstaw do odrzucenia H₀") + "</span>. " +
      (p < 0.001 ? "p < " + b("0.001") : "p = " + b(fmt(p, 3))) + (sig ? " < α = 0.05 — wynik istotny statystycznie." : " ≥ α = 0.05 — wynik nieistotny statystycznie.");
  }
  // Rozkład statystyki testowej pod H₀ ze statystyką z próby (phase "stat") i obszarem
  // odrzucenia (phase "decision"): step_null_plot() z helpers.R.
  function nullPlot(P, stat, df, type, alternative, phase) {
    var chisq = type === "chisq", half = Math.max(4, Math.abs(stat) * 1.1 + 0.5);
    var x0 = chisq ? 0 : -half, x1 = chisq ? Math.max(stat * 2, 15) : half, span = x1 - x0;
    var dens = chisq ? function (x) { return D.dchisq(x, df); } : function (x) { return D.dt(x, df); };
    var crit = critical(type, df, alternative || "two.sided"), curve = [], top = 0, i, x, y;
    for (i = 0; i < 600; i++) { x = x0 + span * i / 599; y = dens(x); curve.push([x, y]); if (isFinite(y) && x >= x0 + span * 0.02) top = Math.max(top, y); }
    top *= 1.25;
    curve = curve.map(function (p) { return [p[0], Math.min(isFinite(p[1]) ? p[1] : top, top)]; });
    var role = phase === "stat" ? "new" : "known", layers = [];
    if (phase === "decision") {
      // Obszar odrzucenia w akcencie, reszta w kolorze tła; granice jako linie pomocnicze.
      var part = [];
      curve.forEach(function (p, j) {
        if (crit.reject(p[0])) part.push(p);
        if ((!crit.reject(p[0]) || j === curve.length - 1) && part.length) {
          layers.push({ type: "line", data: part, color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.3 }); part = [];
        }
      });
      layers.push({ type: "line", data: curve.filter(function (p) { return !crit.reject(p[0]); }), color: P.niebo, width: 0, fill: P.niebo, fillAlpha: 0.22 });
      crit.values.forEach(function (c) { layers.push({ type: "vline", x: c, color: P.accent, width: 1.8, dash: [4, 4] }); });
      if (!chisq && crit.values.length === 2) {
        var mid = (crit.values[1] + x1) / 2;
        layers.push({ type: "text", x: 0, y: top * 0.45, text: "nie odrzucamy H₀", color: P.secondary, bold: true, valign: "middle" });
        layers.push({ type: "text", x: -mid, y: top * 0.25, text: "Hₐ", color: P.accent, bold: true, valign: "middle" });
        layers.push({ type: "text", x: mid, y: top * 0.25, text: "Hₐ", color: P.accent, bold: true, valign: "middle" });
      }
    }
    layers.push({ type: "line", data: curve, color: P.secondary, width: 1.1 });
    layers.push({ type: "vline", x: stat, color: roleColor(P, role), width: roleWidth(role) });
    layers.push(lineLabel(stat, top * 0.93, (chisq ? "χ²" : "t") + " = " + fmt(stat, 3), roleColor(P, role), !(stat > x0 + 0.7 * span)));
    return { x: [x0, x1], y: [0, top], layers: layers, xName: "Statystyka testowa", yName: "Gęstość",
             aria: "Rozkład statystyki testowej pod hipotezą zerową z zaznaczoną statystyką z próby." };
  }

  // ---- Rozdział 4: jedna zmienna ilościowa (test t jednej próby) ----------------
  // Jedna wspólna próba dla testu dwustronnego (s05_t1) i jednostronnego (s05_t1b).
  // Próbka pamięta scenariusz i n: po ich zmianie jest nieaktualna i ryciny wracają do stanu pustego.
  store.t1 = null;
  function t1Controls() {
    var sc = document.getElementById("ch2_scenario"), n = document.getElementById("ch2_n");
    return { scenario: sc ? sc.value : "concentration", n: n ? Number(n.value) : 40 };
  }
  function t1Sample() {
    var c = t1Controls(), s = store.t1;
    return s && s.scenario === c.scenario && s.n === c.n ? s.values : null;
  }
  // Histogram danych (kroki 1–2): rama z danych i μ₀, wspólna dla obu kroków.
  function t1Hist(P, x, mu0, xlab, step) {
    var bins = hist15(x), top = 0, m = T.mean(x);
    bins.forEach(function (r) { top = Math.max(top, r[2]); });
    top *= 1.3;
    var lo = Math.min(Math.min.apply(null, x), mu0), hi = Math.max(Math.max.apply(null, x), mu0), pad = (hi - lo) * 0.06;
    var layers = [{ type: "bins", data: bins, color: P.niebo, alpha: 0.7, stroke: P.ink }];
    if (step >= 2) {
      // μ₀ znamy z hipotezy; średnia z próby jest nowa w kroku 2.
      var right = m >= mu0, role = stepRole(step, 2);
      layers.push({ type: "vline", x: mu0, color: P.secondary, width: 1.1, dash: [4, 4] });
      layers.push({ type: "vline", x: m, color: roleColor(P, role), width: roleWidth(role) });
      layers.push(lineLabel(mu0, top * 0.92, "μ₀ = " + mu0, P.secondary, !right));
      layers.push(lineLabel(m, top * 0.92, "x̄ = " + fmt(m, 2), roleColor(P, role), right));
    }
    return { x: [lo - pad, hi + pad], y: [0, top], layers: layers, xName: xlab, yName: "Liczba", aria: "Histogram wylosowanej próby." };
  }
  function t1Fig(oneSided) {
    return {
      act: function (state, action, el) {
        if (oneSided) return;
        var c = t1Controls(), par = state.config.scenarios[c.scenario];
        store.t1 = { scenario: c.scenario, n: c.n, values: rng.many(c.n, function () { return rng.norm(par.mu_true, par.sd); }) };
      },
      draw: function (v, P, k, state, vals, step) {
        var x = t1Sample();
        if (!x) return null;
        var par = state.config.scenarios[t1Controls().scenario];
        if (step <= 2) return t1Hist(P, x, par.mu0, par.xlab, step);
        var alt = oneSided ? par.alt : "two.sided", r = tOne(x, par.mu0, alt);
        return nullPlot(P, r.t, r.df, "t", alt, step === 3 ? "stat" : "decision");
      },
      text: function (state, v, vals, step) {
        var x = t1Sample();
        if (!x) return "";
        var par = state.config.scenarios[t1Controls().scenario], mu0 = par.mu0;
        var r = tOne(x, mu0, oneSided ? par.alt : "two.sided");
        if (!oneSided) {
          if (step === 1) return "Mamy próbę " + b(r.n) + " obserwacji. Chcemy sprawdzić, czy średnia różni się od μ₀ = " + b(mu0) + ".";
          if (step === 2) return "x̄ = " + b(fmt(r.mean, 2)) + ", s = " + b(fmt(r.sd, 2)) + ", SE = s/√n = " + b(fmt(r.se, 2)) +
            ". Różnica między x̄ a μ₀: " + b(fmt(r.mean - mu0, 2)) + ". Żeby ocenić, czy to dużo, odnosimy ją do SE.";
          if (step === 3) return "t = (" + fmt(r.mean, 2) + " − " + mu0 + ") / " + fmt(r.se, 2) + " = " + b(fmt(r.t, 3)) +
            ". Statystyka t mówi: średnia z próby jest " + b(fmt(Math.abs(r.t), 1)) + " błędów standardowych od μ₀.";
          return verdictHtml(r.p);
        }
        if (step === 1) return "n = " + b(r.n) + " (te same dane co wyżej). Pytamy, czy średnia jest " +
          (par.alt === "less" ? "mniejsza" : "większa") + " niż μ₀ = " + b(mu0) + ".";
        if (step === 2) return "x̄ = " + b(fmt(r.mean, 2)) + ", s = " + b(fmt(r.sd, 2)) + ", SE = s/√n = " + b(fmt(r.se, 2)) +
          ", t = " + b(fmt(r.t, 3)) + " — te same wartości co w teście dwustronnym.";
        if (step === 3) return "t = " + b(fmt(r.t, 3)) + ". W teście jednostronnym patrzymy tylko na " +
          (par.alt === "less" ? "lewy" : "prawy") + " ogon rozkładu.";
        return "Jednostronnie: " + verdictHtml(r.p);
      },
      // Test jednostronny korzysta z próby i kontrolek testu dwustronnego: odświeżamy go przy każdej zmianie.
      update: function () { if (!oneSided) notify(["s05_t1b"]); },
      noteKey: function () { return t1Sample() ? "data" : "empty"; }
    };
  }
  figures.s05_t1 = t1Fig(false);
  figures.s05_t1b = t1Fig(true);

  S.register(figures);
})();
