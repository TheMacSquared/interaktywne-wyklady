// Ryciny wykładu 05 „Wnioskowanie statystyczne” liczone w przeglądarce (dawniej wykresy
// ggplot i wyjścia serwera). Silnik: window.lcSF (shared/lc_stat_figures.js), rozkłady
// i losowanie: window.lcDist, testy: window.lcStats (shared/lc_stats.js).
// Ryciny na stałych danych i o skończonej liczbie ustawień opisuje R (lc_sf_spec()
// w modules/figures.R); tutaj jest to, co zależy od losowania i suwaków.
//
// Rozdział 1 (logika testu):
//   s05_case   — Ryc. 1.2: eksperyment „telefon w plecaku / na biurku” (losowanie, pudełka)
//   s05_sim    — Ryc. 3.3: powtórzone eksperymenty pod H₀ (histogram różnic)
//   s05_power  — Ryc. 3.2: moc testu i błędy dla dwóch grup (α, różnica, n w grupie)
//   s05_quiz   — Ryc. 3.9: quiz „odrzucić czy nie?” (bez wykresu)
//
// Wynik eksperymentu z Ryc. 1.2 jest wspólny dla Ryc. 3.3 (store.caseData);
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
  // Moc testu dwustronnego dla różnicy średnich dwóch grup po n osób (σ = 13 w każdej),
  // jak w eksperymencie z telefonem: H₀ różnica = 0, prawdziwa różnica = effect.
  function power(alpha, effect, n) {
    var se = 13 * Math.sqrt(2 / n), mu0 = 0, mu1 = effect;
    var lo = D.qnorm(alpha / 2) * se, hi = D.qnorm(1 - alpha / 2) * se;
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
  // binom.test(k, n, p0): p dokładne. Dwustronnie: suma prawdopodobieństw wyników nie bardziej
  // prawdopodobnych niż k (z tolerancją 1e-7, jak w R); extreme to te wyniki (słupki p-wartości).
  function binomTest(k, n, p0, alternative) {
    var probs = [], extreme = [], p = 0, i, dk = D.dbinom(k, n, p0);
    for (i = 0; i <= n; i++) probs.push(D.dbinom(i, n, p0));
    for (i = 0; i <= n; i++) {
      var ex = alternative === "greater" ? i >= k : alternative === "less" ? i <= k : probs[i] <= dk * (1 + 1e-7);
      extreme.push(ex); if (ex) p += probs[i];
    }
    return { p: Math.min(1, p), probs: probs, extreme: extreme };
  }
  // prop.test(k, n, p0, correct = TRUE): z z poprawką Yatesa i p z rozkładu χ²(1).
  function propTest(k, n, p0) {
    var diff = k - n * p0, adj = Math.abs(diff) - Math.min(0.5, Math.abs(diff));
    var z = (diff > 0 ? 1 : diff < 0 ? -1 : 0) * adj / Math.sqrt(n * p0 * (1 - p0));
    return { z: z, p: 1 - D.pchisq(z * z, 1) };
  }
  // generate_correlation_data(type = "linear"): x normalne, y = r · z(x) · sd_y + reszta; wartości do 0.1.
  function corData(r, n, par) {
    var round1 = function (v) { return Math.round(v * 10) / 10; };
    // x_digits: miejsca po przecinku wartości x (NDVI potrzebuje dwóch); domyślnie jedno.
    var kx = Math.pow(10, par.x_digits === undefined ? 1 : par.x_digits), roundX = function (v) { return Math.round(v * kx) / kx; };
    var x = r.many(n, function () { return r.norm(par.x_mean, par.x_sd); }), mx = T.mean(x), sx = T.sd(x);
    var y = x.map(function (v) { return par.r_true * (v - mx) / sx * par.y_sd + Math.sqrt(1 - par.r_true * par.r_true) * r.norm() * par.y_sd + par.y_mean; });
    return { x: x.map(roundX), y: y.map(round1) };
  }
  // Test korelacji Pearsona: r, statystyka t = r · √(n − 2) / √(1 − r²), p i prosta MNK.
  function corTest(x, y, alternative) {
    var n = x.length, mx = T.mean(x), my = T.mean(y), sxy = 0, sxx = 0, syy = 0, i;
    for (i = 0; i < n; i++) { sxy += (x[i] - mx) * (y[i] - my); sxx += (x[i] - mx) * (x[i] - mx); syy += (y[i] - my) * (y[i] - my); }
    var r = sxy / Math.sqrt(sxx * syy), t = r * Math.sqrt(n - 2) / Math.sqrt(1 - r * r), df = n - 2;
    var p = alternative === "less" ? D.pt(t, df) : alternative === "greater" ? 1 - D.pt(t, df) : 2 * D.pt(-Math.abs(t), df);
    return { n: n, r: r, t: t, df: df, p: p, slope: sxy / sxx, intercept: my - sxy / sxx * mx };
  }
  // Tabela r × c z losowania: liczebności wierszy z rozkładu wielomianowego o równych szansach,
  // w każdym wierszu kategorie kolumn z prawdopodobieństwami probs[i].
  function tableSample(r, n, probs) {
    var tab = probs.map(function (row) { return row.map(function () { return 0; }); });
    for (var i = 0; i < n; i++) {
      var a = r.int(probs.length) - 1;
      tab[a][r.choose(probs[a]) - 1] += 1;
    }
    return tab;
  }
  function margins(tab) {
    var rows = tab.map(function (r) { return r.reduce(function (s, v) { return s + v; }, 0); });
    var cols = tab[0].map(function (v, j) { return tab.reduce(function (s, r) { return s + r[j]; }, 0); });
    return { rows: rows, cols: cols, n: rows.reduce(function (s, v) { return s + v; }, 0) };
  }
  // chisq.test() dla tabeli większej niż 2 × 2 (bez poprawki): statystyka, df, p i liczebności oczekiwane.
  function chisqTest(tab) {
    var m = margins(tab), stat = 0;
    var expected = tab.map(function (r, i) { return r.map(function (v, j) { return m.rows[i] * m.cols[j] / m.n; }); });
    tab.forEach(function (r, i) { r.forEach(function (v, j) { stat += (v - expected[i][j]) * (v - expected[i][j]) / expected[i][j]; }); });
    var df = (tab.length - 1) * (tab[0].length - 1);
    return { stat: stat, df: df, p: 1 - D.pchisq(stat, df), expected: expected };
  }
  // fisher.test() dla tabeli r × c: suma prawdopodobieństw tabel o tych samych sumach brzegowych
  // nie bardziej prawdopodobnych od obserwowanej. Wyliczenie dokładne, dopóki tabel jest niewiele
  // (limit `cap`); powyżej — losowanie tabel jak fisher.test(simulate.p.value = TRUE, B = 2000).
  function fisherTest(tab, r, cap) {
    var m = margins(tab), R = tab.length, Cn = tab[0].length, lf = D.lgamma;
    var lfact = function (k) { return lf(k + 1); };
    var constant = m.rows.reduce(function (s, v) { return s + lfact(v); }, 0) + m.cols.reduce(function (s, v) { return s + lfact(v); }, 0) - lfact(m.n);
    var logp = function (t) { var s = constant; t.forEach(function (row) { row.forEach(function (v) { s -= lfact(v); }); }); return s; };
    var obs = logp(tab), limit = obs + 1e-7, count = 0, total = 0, aborted = false, max = cap || 4e6;
    // Wyliczenie: komórki wiersz po wierszu; ostatni wiersz i ostatnia kolumna wynikają z sum.
    var cur = tab.map(function (row) { return row.map(function () { return 0; }); }), colLeft = m.cols.slice();
    function fill(i, j, rowLeft, acc) {
      if (aborted) return;
      if (i === R - 1) {                       // ostatni wiersz: reszta kolumn
        var s = acc;
        for (var c = 0; c < Cn; c++) s -= lfact(colLeft[c]);
        if (++count > max) { aborted = true; return; }
        if (s <= limit) total += Math.exp(s);
        return;
      }
      if (j === Cn - 1) {                      // ostatnia komórka wiersza: reszta wiersza
        if (rowLeft > colLeft[j]) return;
        colLeft[j] -= rowLeft;
        fill(i + 1, 0, m.rows[i + 1], acc - lfact(rowLeft));
        colLeft[j] += rowLeft;
        return;
      }
      for (var v = Math.min(rowLeft, colLeft[j]); v >= 0; v--) {
        colLeft[j] -= v;
        fill(i, j + 1, rowLeft - v, acc - lfact(v));
        colLeft[j] += v;
      }
    }
    fill(0, 0, m.rows[0], constant);
    if (!aborted) return { p: Math.min(1, total), exact: true };
    // Symulacja: losowe przyporządkowanie etykiet kolumn (te same sumy brzegowe), B = 2000.
    var labels = [], B = 2000, hits = 0, i, b;
    m.cols.forEach(function (k, c) { for (var q = 0; q < k; q++) labels.push(c); });
    for (b = 0; b < B; b++) {
      for (i = labels.length - 1; i > 0; i--) { var w = r.int(i + 1) - 1, tmp = labels[i]; labels[i] = labels[w]; labels[w] = tmp; }
      var t = tab.map(function (row) { return row.map(function () { return 0; }); }), pos = 0;
      m.rows.forEach(function (k, a) { for (var q = 0; q < k; q++) t[a][labels[pos++]] += 1; });
      if (logp(t) <= limit) hits++;
    }
    return { p: (1 + hits) / (B + 1), exact: false };
  }
  // Wybrana zmienna z generate_student_data() dla n kobiet i n mężczyzn (equal_sex = TRUE).
  function studentVar(r, n, name) {
    var round = function (v, d) { var k = Math.pow(10, d); return Math.round(v * k) / k; };
    var one = function (female) {
      if (name === "wzrost") return round(female ? r.norm(166, 6) : r.norm(178, 7), 1);
      if (name === "waga") return round(female ? r.norm(62, 8) : r.norm(78, 10), 1);
      if (name === "czas_dojazdu") return round(r.gamma(3, 10), 1);
      // Scenariusz kierunkowy (IDKiS): temperatura powierzchni z satelity, tereny zielone (K) i miasto (M).
      if (name === "temp_sat") return round(female ? r.norm(24, 3) : r.norm(27, 3.5), 1);
      // Średnia ocen: poziom zależy od kierunku (losowanego z wagami), ograniczona do 2–5.
      var base = [3.6, 3.4, 3.8, 3.5][r.choose([0.3, 0.25, 0.25, 0.2]) - 1];
      return round(Math.min(5, Math.max(2, base + r.norm(0, 0.4))), 2);
    };
    return { K: r.many(n, function () { return one(true); }), M: r.many(n, function () { return one(false); }) };
  }
  // generate_paired_data(): wynik przed N(50, 12), po = przed + N(efekt, 8); do 0.1 pkt.
  // Scenariusz "ndvi" (IDKiS): zieleń tych samych pól w czerwcu N(0.70, 0.08) i w sierpniu,
  // po = przed − efekt/100 + N(0, 0.04), ograniczone do 0–1; do 0.01.
  function pairedData(r, n, effect, scenario) {
    if (scenario === "ndvi") {
      var r2 = function (v) { return Math.round(Math.min(1, Math.max(0, v)) * 100) / 100; }, june = [], aug = [];
      for (var j = 0; j < n; j++) { var z = r.norm(0.70, 0.08); june.push(r2(z)); aug.push(r2(z - effect / 100 + r.norm(0, 0.04))); }
      return { przed: june, po: aug };
    }
    var round1 = function (v) { return Math.round(v * 10) / 10; }, przed = [], po = [];
    for (var i = 0; i < n; i++) { var a = r.norm(50, 12); przed.push(round1(a)); po.push(round1(a + r.norm(effect, 8))); }
    return { przed: przed, po: po };
  }
  // Test t dla par: różnice po − przed.
  function pairedTest(przed, po) {
    var d = po.map(function (v, i) { return v - przed[i]; }), n = d.length, m = T.mean(d), s = T.sd(d), t = m / (s / Math.sqrt(n));
    return { n: n, mean: m, sd: s, t: t, df: n - 1, p: 2 * D.pt(-Math.abs(t), n - 1) };
  }
  // Dane do ANOVA jak generate_fermentation_data(), generate_workplace_data() i generate_student_data():
  // n obserwacji przydzielonych losowo do grup; zwraca wartości każdej zmiennej w podziale na grupy.
  function anovaData(r, scenario, n) {
    var round = function (v, d) { var k = Math.pow(10, d); return Math.round(v * k) / k; };
    var S3 = { fermentation: { groups: ["20°C", "25°C", "30°C"], vars: { pH: function (g) { return round([4.55, 4.30, 4.05][g] + r.norm(0, 0.14), 2); },
                                                                       kwasowosc_SH: function (g) { return round([28, 33, 38][g] + r.norm(0, 3.5), 1); } } },
               workplace: { groups: ["Budowa", "Magazyn", "Biuro"], vars: { nieobecnosci: function (g) { return Math.max(0, Math.round([9, 5, 2][g] + r.norm(0, 3))); },
                                                                           stres: function (g) { return Math.min(100, Math.max(0, Math.round([62, 58, 48][g] + r.norm(0, 12)))); } } },
               // Scenariusz kierunkowy (IDKiS): zachmurzenie nie zależy od typu terenu.
               landcover: { groups: ["Las", "Pole", "Zabudowa"], vars: { temp_pow: function (g) { return round([22, 26, 30][g] + r.norm(0, 2.5), 1); },
                                                                         ndvi: function (g) { return round(Math.min(1, Math.max(0, [0.80, 0.60, 0.25][g] + r.norm(0, 0.08))), 2); },
                                                                         zachmurzenie: function () { return Math.min(100, Math.max(0, Math.round(r.norm(40, 18)))); } } } };
    var out = { vars: {} }, i, g;
    if (S3[scenario]) {
      var cfg = S3[scenario];
      out.groups = cfg.groups;
      Object.keys(cfg.vars).forEach(function (name) { out.vars[name] = cfg.groups.map(function () { return []; }); });
      for (i = 0; i < n; i++) {
        g = r.int(3) - 1;
        Object.keys(cfg.vars).forEach(function (name) { out.vars[name][g].push(cfg.vars[name](g)); });
      }
      return out;
    }
    // Studenci: grupy to kierunki w kolejności alfabetycznej; wzrost zależy od płci, średnia od kierunku.
    var order = ["Biologia", "Ekonomia", "Informatyka", "Psychologia"], prob = [0.2, 0.25, 0.3, 0.25], gpa = [3.5, 3.4, 3.6, 3.8];
    out.groups = order;
    ["srednia_ocen", "wzrost", "czas_dojazdu"].forEach(function (name) { out.vars[name] = order.map(function () { return []; }); });
    for (i = 0; i < n; i++) {
      g = r.choose(prob) - 1;
      var female = r.u() < 0.55;
      out.vars.wzrost[g].push(round(female ? r.norm(166, 6) : r.norm(178, 7), 1));
      out.vars.srednia_ocen[g].push(round(Math.min(5, Math.max(2, gpa[g] + r.norm(0, 0.4))), 2));
      out.vars.czas_dojazdu[g].push(round(r.gamma(3, 10), 1));
    }
    return out;
  }
  // Ryzyko co najmniej jednego błędu I rodzaju przy wszystkich testach t par k grup (α = 0.05).
  function fwer(k) { var tests = k * (k - 1) / 2; return { tests: tests, risk: 1 - Math.pow(0.95, tests) }; }
  // Odmiana: 1 istotna różnica, 2–4 istotne różnice, 5+ istotnych różnic.
  function pluralDiff(n) {
    if (n === 1) return "istotna różnica";
    return n % 10 >= 2 && n % 10 <= 4 && !(n % 100 >= 12 && n % 100 <= 14) ? "istotne różnice" : "istotnych różnic";
  }
  // Ryc. 10.1: test t dwóch grup o tej samej liczebności n i rozrzucie 1 dla różnicy średnich d.
  function effectTest(d, n) {
    var t = d / Math.sqrt(2 / n), df = 2 * n - 2, a = Math.abs(d);
    return { t: t, df: df, p: 2 * D.pt(-Math.abs(t), df), label: a < 0.2 ? "pomijalna" : a < 0.5 ? "mała" : a < 0.8 ? "średnia" : "duża" };
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
                 tOne: tOne, hist15: hist15, critical: critical, binomTest: binomTest, propTest: propTest,
                 corData: corData, corTest: corTest, tableSample: tableSample, chisqTest: chisqTest, fisherTest: fisherTest,
                 studentVar: studentVar, pairedData: pairedData, pairedTest: pairedTest,
                 anovaData: anovaData, fwer: fwer, pluralDiff: pluralDiff, effectTest: effectTest };

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
      // Stały zakres osi: większa próba widocznie zwęża obie krzywe.
      var a = -16, b = 30, h0 = [], h1 = [], i, x;
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
        legend: [{ color: P.niebo, text: "H₀: telefon nie wpływa (różnica = 0)" }, { color: P.accent, text: "Hₐ: telefon obniża wynik o ustawioną różnicę" }],
        xName: "Różnica średnich w próbie: plecak − biurko (pkt)", yName: "Gęstość",
        aria: "Rozkłady różnicy średnich dwóch grup pod hipotezą zerową i alternatywną z obszarami odrzucenia i mocą testu." };
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

  // ---- Rozdział 5: jedna zmienna jakościowa (test dwumianowy) -------------------
  // Jedna wspólna próba (liczba sukcesów k w n próbach) dla Ryc. 5.1–5.3.
  store.b1 = null;
  function b1Controls() {
    var sc = document.getElementById("ch3_scenario"), n = document.getElementById("ch3_n");
    return { scenario: sc ? sc.value : "water_quality", n: n ? Number(n.value) : 50 };
  }
  function b1Data() {
    var c = b1Controls(), s = store.b1;
    return s && s.scenario === c.scenario && s.n === c.n ? s : null;
  }
  // Krok 1: słupki sukces / porażka z proporcją z próby.
  function b1Counts(P, d, par, label) {
    var top = Math.max(d.k, d.n - d.k), labels = {};
    labels[1] = par.success_label; labels[2] = par.failure_label;
    return { x: [0.4, 2.6], y: [0, top * 1.2], xLabels: labels,
      layers: [{ type: "bins", data: [[0.7, 1.3, d.k]], color: P.niebo, alpha: 0.7, stroke: P.ink },
               { type: "bins", data: [[1.7, 2.3, d.n - d.k]], color: P.bursztyn, alpha: 0.7, stroke: P.ink },
               { type: "text", x: 1, y: d.k, text: String(d.k), color: P.secondary, bold: true, mono: true, size: 1.3 },
               { type: "text", x: 2, y: d.n - d.k, text: String(d.n - d.k), color: P.secondary, bold: true, mono: true, size: 1.3 },
               { type: "text", x: 1.5, y: top * 0.7, text: label, color: P.accent, bold: true, valign: "middle", size: 1.35 }],
      xName: "", yName: "Liczba", aria: "Liczba sukcesów i porażek w wylosowanej próbie." };
  }
  // Kroki 2–3: rozkład dwumianowy pod H₀; w kroku 3 słupki p-wartości w akcencie.
  function b1Binom(P, d, p0, test, step) {
    var max = Math.max.apply(null, test.probs), lo = d.k, hi = d.k, plain = [], hot = [];
    test.probs.forEach(function (pr, i) { if (pr >= max * 1e-3) { lo = Math.min(lo, i); hi = Math.max(hi, i); } });
    test.probs.forEach(function (pr, i) {
      if (i < lo - 1 || i > hi + 1) return;
      (test.extreme[i] && step >= 3 ? hot : plain).push([i - 0.4, i + 0.4, pr]);
    });
    var role = stepRole(step, 2), top = max * 1.15;
    return { x: [lo - 1.5, hi + 1.5], y: [0, top], xInt: true,
      layers: [{ type: "bins", data: plain, color: P.niebo, alpha: 0.7, stroke: P.ink },
               { type: "bins", data: hot, color: P.accent, alpha: 0.7, stroke: P.ink },
               { type: "vline", x: d.k, color: roleColor(P, role), width: roleWidth(role) },
               lineLabel(d.k, top * 0.9, "k = " + d.k, roleColor(P, role), d.k > d.n * p0)],
      xName: "Liczba sukcesów", yName: "Prawdopodobieństwo", aria: "Rozkład dwumianowy liczby sukcesów pod hipotezą zerową z zaznaczonym wynikiem próby." };
  }
  function b1Fig(oneSided) {
    return {
      act: function (state) {
        if (oneSided) return;
        var c = b1Controls(), par = state.config.scenarios[c.scenario];
        store.b1 = { scenario: c.scenario, n: c.n, k: rng.binom(c.n, par.p_true) };
      },
      draw: function (v, P, k, state, vals, step) {
        var d = b1Data();
        if (!d) return null;
        var par = state.config.scenarios[d.scenario];
        if (step === 1) {
          return b1Counts(P, d, par, oneSided ? "p̂ = " + fmt(d.k / d.n, 3) + " (te same dane)"
                                               : "p̂ = " + d.k + "/" + d.n + " = " + fmt(d.k / d.n, 3));
        }
        return b1Binom(P, d, par.p0, binomTest(d.k, d.n, par.p0, oneSided ? par.alt_1s : "two.sided"), step);
      },
      text: function (state, v, vals, step) {
        var d = b1Data();
        if (!d) return "";
        var par = state.config.scenarios[d.scenario], p0 = par.p0, phat = d.k / d.n;
        if (!oneSided) {
          if (step === 1) return "n = " + b(d.n) + ", " + S.esc(par.success_label) + ": " + b(d.k) + ". Proporcja z próby: p̂ = " + d.k + "/" + d.n + " = " +
            b(fmt(phat, 3)) + ". Wartość referencyjna: p₀ = " + b(p0) + ". Różnica: " + b(fmt(phat - p0, 3)) + ". Ale czy to dużo?";
          if (step === 2) return "Rozkład B(" + d.n + "; " + fmt(p0, 2) + ") — liczba sukcesów, jakiej należałoby się spodziewać przy prawdziwej H₀. " +
            "Pionowa linia: nasz wynik k = " + b(d.k) + ".";
          return verdictHtml(binomTest(d.k, d.n, p0, "two.sided").p);
        }
        if (step === 1) return "n = " + b(d.n) + ", p̂ = " + b(fmt(phat, 3)) + " — te same dane co w teście dwustronnym.";
        if (step === 2) return "Ten sam rozkład B(" + d.n + "; " + fmt(p0, 2) + "), ale teraz liczy się tylko " +
          (par.alt_1s === "greater" ? "prawy" : "lewy") + " ogon.";
        return "Jednostronnie: " + verdictHtml(binomTest(d.k, d.n, p0, par.alt_1s).p) + " <em>Porównaj z p-wartością testu dwustronnego wyżej.</em>";
      },
      update: function () { if (!oneSided) notify(["s05_b1b", "s05_bcmp"]); },
      noteKey: function () { return b1Data() ? "data" : "empty"; }
    };
  }
  figures.s05_b1 = b1Fig(false);
  figures.s05_b1b = b1Fig(true);

  // Ryc. 5.3: test dwumianowy a test proporcji (z) na tej samej próbie; bez wykresu.
  figures.s05_bcmp = {
    act: function (state) { state.asked = true; },
    update: function (el, state) {
      var d = b1Data();
      if (!state.asked || !d) return;
      var p0 = state.config.scenarios[d.scenario].p0, bt = binomTest(d.k, d.n, p0, "two.sided"), pt = propTest(d.k, d.n, p0);
      var data = "k = " + d.k + ", n = " + d.n, decision = function (p) { return p < 0.05 ? "Odrzucamy H₀" : "Brak podstaw do odrzucenia H₀"; };
      S.setValue(el, "cmp_b_data", data); S.setValue(el, "cmp_p_data", data);
      S.setValue(el, "cmp_b_stat", "k = " + d.k + " (dokładna)"); S.setValue(el, "cmp_p_stat", "z = " + T.round(pt.z, 3) + " (z poprawką na ciągłość)");
      S.setValue(el, "cmp_b_p", T.fmtPValue(bt.p)); S.setValue(el, "cmp_p_p", T.fmtPValue(pt.p));
      S.setValue(el, "cmp_b_dec", decision(bt.p)); S.setValue(el, "cmp_p_dec", decision(pt.p));
      [["cmp_b_dec", bt.p], ["cmp_p_dec", pt.p]].forEach(function (c) {
        var cell = el.querySelector('[data-v="' + c[0] + '"]');
        if (cell && cell.closest("td")) cell.closest("td").classList.toggle("is-base", c[1] < 0.05);
      });
      S.setValue(el, "cmp_np", T.round(d.n * p0, 1)); S.setValue(el, "cmp_nq", T.round(d.n * (1 - p0), 1));
    },
    // Przybliżenie normalne jest dobre, gdy oczekujemy co najmniej 10 sukcesów i 10 porażek.
    noteKey: function (vals, state) {
      var d = b1Data();
      if (!state.asked) return "none";
      if (!d) return "nodata";
      var p0 = state.config.scenarios[d.scenario].p0;
      return d.n * p0 >= 10 && d.n * (1 - p0) >= 10 ? "ok" : "small";
    }
  };

  // ---- Rozdział 6: dwie zmienne ilościowe (test korelacji) ----------------------
  // Jedna wspólna próba par (x, y) dla testu dwustronnego (s05_r1) i jednostronnego (s05_r1b).
  store.r1 = null;
  function r1Controls() {
    var sc = document.getElementById("ch4_scenario"), n = document.getElementById("ch4_n");
    return { scenario: sc ? sc.value : "sleep_grade", n: n ? Number(n.value) : 40 };
  }
  function r1Data() {
    var c = r1Controls(), s = store.r1;
    return s && s.scenario === c.scenario && s.n === c.n ? s : null;
  }
  // Kroki 1–2: wykres rozrzutu; prosta MNK nowa w kroku 2. Rama z danych.
  function r1Scatter(P, d, par, step) {
    var pad = function (v) { var lo = Math.min.apply(null, v), hi = Math.max.apply(null, v), e = (hi - lo) * 0.06; return [lo - e, hi + e]; };
    var layers = [{ type: "points", data: d.x.map(function (x, i) { return [x, d.y[i]]; }), color: P.niebo, size: 8, alpha: 0.7 }];
    if (step >= 2) {
      var fit = corTest(d.x, d.y), lo = Math.min.apply(null, d.x), hi = Math.max.apply(null, d.x), role = stepRole(step, 2);
      layers.push({ type: "line", data: [[lo, fit.intercept + fit.slope * lo], [hi, fit.intercept + fit.slope * hi]], color: roleColor(P, role), width: roleWidth(role) });
    }
    return { x: pad(d.x), y: pad(d.y), layers: layers, xName: par.xlab, yName: par.ylab, aria: "Wykres rozrzutu wylosowanych par obserwacji." };
  }
  function r1Fig(oneSided) {
    return {
      act: function (state) {
        if (oneSided) return;
        var c = r1Controls(), d = corData(rng, c.n, state.config.scenarios[c.scenario]);
        store.r1 = { scenario: c.scenario, n: c.n, x: d.x, y: d.y };
      },
      draw: function (v, P, k, state, vals, step) {
        var d = r1Data();
        if (!d) return null;
        var par = state.config.scenarios[d.scenario];
        if (step <= 2) return r1Scatter(P, d, par, step);
        var alt = oneSided ? par.alt_1s : "two.sided", r = corTest(d.x, d.y, alt);
        return nullPlot(P, r.t, r.df, "t", alt, step === 3 ? "stat" : "decision");
      },
      text: function (state, v, vals, step) {
        var d = r1Data();
        if (!d) return "";
        var par = state.config.scenarios[d.scenario], r = corTest(d.x, d.y, oneSided ? par.alt_1s : "two.sided");
        if (!oneSided) {
          if (step === 1) return "n = " + b(r.n) + " par obserwacji. Każdy punkt to jedna obserwacja z dwiema wartościami: " +
            S.esc(par.xlab + " i " + par.ylab) + ". Czy widać trend?";
          if (step === 2) return "Korelacja z próby: r = " + b(fmt(r.r, 3)) + ". Ale czy to wystarczająco daleko od zera, by odrzucić H₀?";
          if (step === 3) return "t = " + fmt(r.r, 3) + " · √" + r.df + " / √(1 − " + fmt(r.r * r.r, 3) + ") = " + b(fmt(r.t, 3)) +
            ". Zamieniamy r na statystykę t, żeby móc porównać z rozkładem t(" + r.df + ").";
          return verdictHtml(r.p);
        }
        if (step === 1) return "n = " + b(r.n) + " (te same dane co wyżej). Te same obserwacje, ale pytamy o kierunek związku.";
        if (step === 2) return "r = " + b(fmt(r.r, 3)) + ", tak samo jak w teście dwustronnym. Zmieniło się tylko pytanie.";
        if (step === 3) return "t = " + b(fmt(r.t, 3)) + ", bez zmian. Obszar odrzucenia leży teraz tylko w " +
          (par.alt_1s === "greater" ? "prawym" : "lewym") + " ogonie.";
        return "Jednostronnie: " + verdictHtml(r.p);
      },
      update: function () { if (!oneSided) notify(["s05_r1b"]); },
      noteKey: function () { return r1Data() ? "data" : "empty"; }
    };
  }
  figures.s05_r1 = r1Fig(false);
  figures.s05_r1b = r1Fig(true);

  // ---- Rozdział 7: dwie zmienne jakościowe (test χ² niezależności) ----------------
  // Tabela krzyżowa jak lc_crosstab() w R: liczebności albo procenty wierszowe, z sumami.
  function crosstabHtml(o) {
    var m = margins(o.tab), pct = o.measure === "row", e = S.esc;
    var cell = function (v, total) { return pct ? fmt(100 * v / total, 1) : String(v); };
    var head = '<thead><tr><th scope="col" rowspan="2" class="lc-tbl-corner">' + e(o.rowName) + '</th><th scope="colgroup" colspan="' + o.cols.length +
      '" class="lc-tbl-span">' + e(o.colName) + '</th><th scope="col" rowspan="2" class="n is-total">Razem</th></tr><tr>' +
      o.cols.map(function (c, j) { return '<th scope="col" class="n"><i class="lc-th-swatch" aria-hidden="true" style="--lc-sw:' + o.colours[j] + ';"></i>' + e(c) + "</th>"; }).join("") + "</tr></thead>";
    var body = "<tbody>" + o.tab.map(function (row, i) {
      return '<tr><th scope="row">' + e(o.rows[i]) + "</th>" + row.map(function (v, j) { return '<td class="n" data-label="' + e(o.cols[j]) + '">' + cell(v, m.rows[i]) + "</td>"; }).join("") +
        '<td class="n is-total' + (pct ? " is-base-val" : "") + '">' + (pct ? "100" : m.rows[i]) + "</td></tr>";
    }).join("") + "</tbody>";
    var foot = '<tfoot><tr><th scope="row">Razem</th>' + m.cols.map(function (v) { return '<td class="n">' + cell(v, m.n) + "</td>"; }).join("") +
      '<td class="n is-total">' + (pct ? "100" : m.n) + "</td></tr></tfoot>";
    return '<div class="lc-tbl-block">' + (pct ? '<p class="lc-tbl-lead"><b>% wierszowe:</b> każdy wiersz sumuje się do 100.</p>' : "") +
      '<div class="lc-tbl-scroll" tabindex="0" role="region" aria-label="' + e(o.label) + '"><table class="lc-tbl is-fit">' + head + body + foot + "</table></div></div>";
  }
  // Jedna wspólna tabela dla Ryc. 7.2 (kroki) i 7.3 (χ² a Fisher); po zmianie scenariusza albo n jest nieaktualna.
  store.x2 = null;
  function x2Controls() {
    var sc = document.getElementById("ch5_scenario"), n = document.getElementById("ch5_n");
    return { scenario: sc ? sc.value : "packaging", n: n ? Number(n.value) : 120 };
  }
  function x2Data() {
    var c = x2Controls(), s = store.x2;
    return s && s.scenario === c.scenario && s.n === c.n ? s : null;
  }
  // Kolory kategorii zmiennej w kolumnach (kroki 1–2): dane, grupa, trzecia.
  function catColours(P) { return [P.niebo, P.bursztyn, P.szalwia]; }
  figures.s05_x2 = {
    act: function (state) {
      var c = x2Controls();
      store.x2 = { scenario: c.scenario, n: c.n, tab: tableSample(rng, c.n, state.config.scenarios[c.scenario].probs) };
    },
    draw: function (v, P, k, state, vals, step) {
      var d = x2Data();
      if (!d) return null;
      var par = state.config.scenarios[d.scenario];
      if (step >= 3) { var test = chisqTest(d.tab); return nullPlot(P, test.stat, test.df, "chisq", null, step === 3 ? "stat" : "decision"); }
      // Krok 1: liczności; krok 2: procenty w obrębie wiersza. Słupki kategorii obok siebie.
      var m = margins(d.tab), cols = catColours(P), nc = par.cats2.length, w = 0.9 / nc, layers = [], labels = {}, top = 0;
      d.tab.forEach(function (row, i) {
        labels[i + 1] = par.cats1[i];
        row.forEach(function (count, j) {
          var pct = Math.round(count / m.rows[i] * 1000) / 10, value = step === 1 ? count : pct, x = i + 1 - 0.45 + w * (j + 0.5);
          top = Math.max(top, value);
          layers.push({ type: "bins", data: [[x - w * 0.47, x + w * 0.47, value]], color: cols[j], alpha: 0.7, stroke: P.ink });
          layers.push({ type: "text", x: x, y: value, text: step === 1 ? String(count) : pct + "%", color: P.secondary, mono: true });
        });
      });
      return { x: [0.4, d.tab.length + 0.6], y: [0, step === 1 ? top * 1.15 : 110], layers: layers, xLabels: labels,
        xName: par.lab1, yName: step === 1 ? "Liczebność" : "Procent", aria: "Słupki kategorii drugiej zmiennej w grupach pierwszej zmiennej." };
    },
    // Tabele kroków nad wykresem: 1 liczebności, 2 procenty wierszowe, 3 liczebności oczekiwane.
    table: function (state, v, vals, step) {
      var d = x2Data();
      if (!d || step === 4) return "";
      var par = state.config.scenarios[d.scenario], base = { tab: d.tab, rows: par.cats1, cols: par.cats2, rowName: par.lab1, colName: par.lab2, colours: catColours(C.palette()) };
      if (step === 1) return crosstabHtml(Object.assign({ measure: "n", label: "Tabela krzyżowa: " + par.lab1 + " × " + par.lab2 }, base));
      if (step === 2) return crosstabHtml(Object.assign({ measure: "row", label: "Procenty w każdej grupie (wierszu)" }, base));
      var expected = chisqTest(d.tab).expected;
      return '<p class="lc-tbl-lead">Liczebności oczekiwane (gdyby H₀ była prawdziwa):</p>' +
        S.table([{ label: par.lab1, row: true }].concat(par.cats2.map(function (c) { return { label: c, digits: 1 }; })),
                expected.map(function (row, i) { return [par.cats1[i]].concat(row); }));
    },
    text: function (state, v, vals, step) {
      var d = x2Data();
      if (!d) return "";
      var test = chisqTest(d.tab), low = test.expected.some(function (row) { return row.some(function (e) { return e < 5; }); });
      if (step === 1) return "To liczebności obserwowane. Grupy mogą mieć różne rozmiary, więc same liczby trudno porównać.";
      if (step === 2) return "Przy niezależności procenty w populacji byłyby takie same w każdym wierszu; w próbie różnią się także przez przypadek.";
      if (step === 3) return "χ² = " + b(fmt(test.stat, 3)) + " (df = " + test.df + ") — łączna rozbieżność między tabelą obserwowaną a oczekiwaną." +
        (low ? ' <span class="lc-status-danger">Uwaga: niektóre liczebności oczekiwane są mniejsze od 5</span>' : "");
      return "Wynik testu χ² niezależności: χ²(" + test.df + ") = " + b(fmt(test.stat, 3)) + ". " + verdictHtml(test.p);
    },
    update: function () { notify(["s05_x2cmp"]); },
    noteKey: function () { return x2Data() ? "data" : "empty"; }
  };

  // Ryc. 7.3: test χ² a test dokładny Fishera na tej samej tabeli; bez wykresu.
  figures.s05_x2cmp = {
    act: function (state) { state.asked = true; },
    update: function (el, state) {
      var d = x2Data();
      if (!state.asked || !d) return;
      // Wynik liczony raz na tabelę: przy dużych tabelach test Fishera jest symulowany.
      if (state.tab !== d.tab) { state.tab = d.tab; state.chi = chisqTest(d.tab); state.fisher = fisherTest(d.tab, rng); }
      var decision = function (p) { return p < 0.05 ? "Odrzucamy H₀" : "Brak podstaw do odrzucenia H₀"; };
      S.setValue(el, "x2_chi_p", T.fmtPValue(state.chi.p)); S.setValue(el, "x2_fisher_p", T.fmtPValue(state.fisher.p));
      S.setValue(el, "x2_chi_dec", decision(state.chi.p)); S.setValue(el, "x2_fisher_dec", decision(state.fisher.p));
      [["x2_chi_dec", state.chi.p], ["x2_fisher_dec", state.fisher.p]].forEach(function (c) {
        var cell = el.querySelector('[data-v="' + c[0] + '"]');
        if (cell && cell.closest("td")) cell.closest("td").classList.toggle("is-base", c[1] < 0.05);
      });
      var low = 0;
      state.chi.expected.forEach(function (row) { row.forEach(function (e) { if (e < 5) low++; }); });
      S.setValue(el, "x2_low", String(low));
    },
    noteKey: function (vals, state) {
      var d = x2Data();
      if (!state.asked) return "none";
      if (!d) return "nodata";
      return chisqTest(d.tab).expected.some(function (row) { return row.some(function (e) { return e < 5; }); }) ? "low" : "fine";
    }
  };

  // ---- Rozdział 8: dwie grupy (test t niezależny i dla par) ---------------------
  var IND_LABEL = { wzrost: "Wzrost (cm)", waga: "Waga (kg)", srednia_ocen: "Średnia ocen", czas_dojazdu: "Czas dojazdu (min)", temp_sat: "Temperatura powierzchni (°C)" };
  var IND_NAME = { wzrost: "wzrost", waga: "waga", srednia_ocen: "średnia ocen", czas_dojazdu: "czas dojazdu", temp_sat: "temperatura powierzchni" };
  // Nazwy grup i jednostek obserwacji: studenci według płci albo punkty pomiaru satelitarnego.
  var IND_PEOPLE = { a: "Kobieta", b: "Mężczyzna", axis: "Płeć", unit: "osób", each: "jedna osoba", order: "kobiety − mężczyźni",
                     aria: "Wykresy pudełkowe wybranej zmiennej u kobiet i mężczyzn." };
  var IND_GROUPS = { temp_sat: { a: "Tereny zielone", b: "Miasto", axis: "Strefa", unit: "punktów pomiaru", each: "jeden punkt pomiaru",
                                 order: "tereny zielone − miasto", aria: "Wykresy pudełkowe temperatury powierzchni w terenach zielonych i w mieście." } };
  function indGroups(name) { return IND_GROUPS[name] || IND_PEOPLE; }
  // Zapis p jak ui_p_value() w shared.R: liczba wyróżniona, „p <” przy wartości granicznej.
  function pValueHtml(p) {
    var v = T.fmtPValue(p), bound = v.charAt(0) === "<" || v.charAt(0) === ">";
    return "<p>" + (bound ? "p " + S.esc(v.charAt(0)) + " " : "p = ") + '<strong style="font-size: 1.25em;">' + S.esc(bound ? v.slice(1) : v) + "</strong></p>";
  }
  function decisionHtml(p) {
    return '<p><span class="lc-status-' + (p < 0.05 ? "danger" : "ok") + '"><strong>' + (p < 0.05 ? "Odrzucamy H₀" : "Brak podstaw do odrzucenia H₀") + "</strong></span></p>";
  }
  function boxOf(x) {
    var q1 = T.quantile(x, 0.25), q3 = T.quantile(x, 0.75), iqr = q3 - q1;
    var inside = x.filter(function (v) { return v >= q1 - 1.5 * iqr && v <= q3 + 1.5 * iqr; });
    return { q1: q1, med: T.median(x), q3: q3, lo: Math.min.apply(null, inside), hi: Math.max.apply(null, inside) };
  }
  function indData(el) {
    var s = el._s05ind;
    return s && s.name === el.querySelector("#ch6_ind_var").value && s.n === Number(el.querySelector("#ch6_ind_n").value) ? s : null;
  }
  figures.s05_ind = {
    act: function (state, action, el) {
      var name = el.querySelector("#ch6_ind_var").value, n = Number(el.querySelector("#ch6_ind_n").value), d = studentVar(rng, n, name);
      // Rozsunięcie punktów w poziomie losowane raz dla próby.
      el._s05ind = { name: name, n: n, K: d.K, M: d.M, jitter: rng.many(2 * n, function () { return rng.unif(-0.15, 0.15); }) };
      state.root = el;
    },
    prepare: function (state, v, vals, el) { state.d = indData(el); state.test = state.d ? T.tTest(state.d.K, state.d.M, false) : null; },
    draw: function (v, P, k, state, vals, step) {
      var d = state.d;
      if (!d) return null;
      if (step >= 3) return nullPlot(P, state.test.t, state.test.df, "t", "two.sided", step === 3 ? "stat" : "decision");
      // Pierwsza grupa w roli danych, druga w roli grupy; rama z danych.
      var groups = [d.K, d.M], cols = [P.niebo, P.bursztyn], all = d.K.concat(d.M), layers = [];
      var lo = Math.min.apply(null, all), hi = Math.max.apply(null, all), span = hi - lo;
      groups.forEach(function (gr, i) {
        var bx = boxOf(gr);
        layers.push({ type: "box", stroke: P.ink, boxes: [{ pos: i + 1, width: 0.5, q1: bx.q1, med: bx.med, q3: bx.q3, lo: bx.lo, hi: bx.hi, fill: C.alpha(cols[i], 0.35) }] });
        layers.push({ type: "points", data: gr.map(function (y, j) { return [i + 1 + d.jitter[i * d.n + j], y]; }), color: cols[i], size: 5.5, alpha: 0.5 });
      });
      if (step >= 2) {
        groups.forEach(function (gr, i) {
          var m = T.mean(gr);
          layers.push({ type: "point", x: i + 1, y: m, color: P.accent, symbol: "diamond", size: 17 });
          layers.push({ type: "point", x: i + 1, y: m, color: P.surface, symbol: "diamond", size: 10 });
          layers.push({ type: "text", x: i === 0 ? 0.9 : 2.1, y: m + span * 0.04, text: "x̄ = " + fmt(m, 1), color: P.accent, bold: true, align: i === 0 ? "right" : "left", valign: "middle" });
        });
      }
      var G = indGroups(d.name);
      return { x: [0.4, 2.6], y: [lo - span * 0.08, hi + span * 0.08], layers: layers, xLabels: { 1: G.a, 2: G.b },
        xName: G.axis, yName: IND_LABEL[d.name], aria: G.aria };
    },
    // Tabela średnich pod wykresem (krok 2).
    table: function (state, v, vals, step) {
      var d = state.d;
      if (!d || step !== 2) return "";
      return S.table([{ label: "Grupa", row: true }, { label: "n", digits: 0 }, { label: "x̄", digits: 2 }, { label: "s", digits: 2 }],
        [[indGroups(d.name).a, d.K.length, T.mean(d.K), T.sd(d.K)], [indGroups(d.name).b, d.M.length, T.mean(d.M), T.sd(d.M)]]);
    },
    text: function (state, v, vals, step) {
      var d = state.d;
      if (!d) return "";
      var G = indGroups(d.name);
      var t = state.test, mk = T.mean(d.K), mm = T.mean(d.M), higher = mk >= mm ? G.a : G.b, lower = mk >= mm ? G.b : G.a;
      var diff = fmt(Math.abs(mk - mm), 2), name = IND_NAME[d.name];
      if (step === 1) return "n = " + b(2 * d.n) + " " + G.unit + " łącznie. Każdy punkt to " + G.each + ". Najpierw patrzymy, czy grupy wizualnie wyglądają na przesunięte względem siebie.";
      if (step === 2) return "Różnica średnich w próbie wynosi " + b(diff) + ". Test pyta, czy taka różnica jest duża względem zmienności w grupach.";
      if (step === 3) return "t = " + b(fmt(t.t, 3)) + " (df Welcha = " + fmt(t.df, 1) + "). Tyle błędów standardowych dzieli średnie obu grup (" + G.order + ").";
      return "Wynik testu t niezależnego: t(" + fmt(t.df, 1) + ") = " + b(fmt(t.t, 3)) + ". " + verdictHtml(t.p) + " <strong>Werdykt:</strong>" +
        (t.p < 0.05
          ? " średnia zmiennej „" + name + "” różni się istotnie między grupami — w próbie była wyższa w grupie " + higher + " niż " + lower + " o " + b(diff) + "."
          : " nie ma podstaw, by twierdzić, że średnia zmiennej „" + name + "” różni się między grupami. Obserwowana w próbie różnica " + b(diff) +
            " (na korzyść grupy " + higher + ") mieści się w zakresie wahań losowych.");
    },
    noteKey: function (vals, state) { return state.d ? "data" : "empty"; }
  };

  // Ryc. 8.2: dane sparowane „przed i po”; próba nieaktualna po zmianie suwaków albo scenariusza.
  // Napisy scenariuszy: korepetycje (wynik w punktach) i susza (NDVI tych samych pól, IDKiS).
  var PAIRED = {
    tutoring: { first: "Przed", second: "Po", axis: "Moment", y: "Wynik", diff: "po − przed", unit: " pkt", digits: 2,
      up: "wzrosły", down: "spadły", same: "nie zmieniły się", changed: "wyniki istotnie się zmieniły", nochange: "wyniki się zmieniły",
      aria: "Wyniki tych samych osób przed interwencją i po niej, połączone liniami." },
    ndvi: { first: "Czerwiec", second: "Sierpień", axis: "Miesiąc", y: "Zieleń pola (NDVI)", diff: "sierpień − czerwiec", unit: "", digits: 3,
      up: "wzrosła", down: "spadła", same: "nie zmieniła się", changed: "zieleń pól istotnie się zmieniła", nochange: "zieleń pól się zmieniła",
      aria: "Wskaźnik zieleni tych samych pól w czerwcu i w sierpniu, połączony liniami." }
  };
  function pairedScenario(el) { var sc = el.querySelector("#ch6_paired_scenario"); return sc ? sc.value : "tutoring"; }
  figures.s05_paired = {
    act: function (state, action, el) {
      var n = Number(el.querySelector("#ch6_paired_n").value), effect = Number(el.querySelector("#ch6_paired_effect").value);
      var scenario = pairedScenario(el), d = pairedData(rng, n, effect, scenario);
      state.sample = { n: n, effect: effect, scenario: scenario, przed: d.przed, po: d.po };
    },
    prepare: function (state, v, vals, el) {
      var s = state.sample;
      state.d = s && s.scenario === pairedScenario(el) && s.n === Number(el.querySelector("#ch6_paired_n").value) &&
        s.effect === Number(el.querySelector("#ch6_paired_effect").value) ? s : null;
    },
    draw: function (v, P, k, state) {
      var d = state.d;
      if (!d) return null;
      var all = d.przed.concat(d.po), lo = Math.min.apply(null, all), hi = Math.max.apply(null, all), pad = (hi - lo) * 0.05;
      var L = PAIRED[d.scenario] || PAIRED.tutoring;
      return { x: [0.4, 2.6], y: [lo - pad, hi + pad], xLabels: { 1: L.first, 2: L.second },
        layers: [{ type: "segs", items: d.przed.map(function (a, i) { return { from: [1, a], to: [2, d.po[i]] }; }), color: C.token("--upwr-cat-kurkuma", "#cbb858"), width: 1.4, alpha: 0.3 },
                 { type: "points", data: d.przed.map(function (a) { return [1, a]; }), color: P.niebo, size: 8, alpha: 0.7 },
                 { type: "points", data: d.po.map(function (a) { return [2, a]; }), color: P.accent, size: 8, alpha: 0.7 }],
        xName: L.axis, yName: L.y, aria: L.aria };
    },
    update: function (el, state) {
      var box = el.querySelector('[data-role="result"]'), d = state.d;
      if (!box) return;
      if (!d) { box.innerHTML = ""; return; }
      var r = pairedTest(d.przed, d.po), L = PAIRED[d.scenario] || PAIRED.tutoring;
      var dir = r.mean > 0 ? L.up : r.mean < 0 ? L.down : L.same, val = function (x) { return T.round(x, L.digits) + L.unit; };
      box.innerHTML = S.status("<p><strong>Wynik testu t dla danych sparowanych:</strong></p>" +
        "<p>Średnia różnica (" + L.diff + "): " + val(r.mean) + "</p><p>t(" + r.df + ") = " + T.round(r.t, 3) + "</p>" + pValueHtml(r.p) + decisionHtml(r.p) +
        (r.p < 0.05
          ? "<p><strong>Werdykt:</strong> " + L.changed + " — średnio " + dir + " o " + val(Math.abs(r.mean)) + ".</p>"
          : "<p><strong>Werdykt:</strong> nie ma podstaw, by twierdzić, że " + L.nochange + ". Obserwowana w próbie zmiana (" + val(r.mean) +
            ") mieści się w zakresie wahań losowych.</p>"));
    },
    noteKey: function (vals, state) { return state.d ? "data" : "empty"; }
  };

  // ---- Rozdział 9: ANOVA i porównania par -------------------------------------
  // Ryc. 9.1: rysunki dla 2–8 grup są w dokumencie (R); tu licznik grup i odczyty.
  figures.s05_fwer = {
    init: function (state) { state.k = 2; },
    act: function (state, action) { state.k = action === "reset" ? 2 : Math.min(state.k + 1, 8); },
    update: function (el, state) {
      var f = fwer(state.k), node = el.querySelector('[data-v="fwer_risk"]'), read = node && node.closest(".lc-read");
      S.setValue(el, "fwer_k", String(state.k)); S.setValue(el, "fwer_tests", String(f.tests));
      S.setValue(el, "fwer_risk", T.round(f.risk * 100, 1) + "%");
      if (read) read.style.setProperty("--lc-read-color", f.risk < 0.10 ? "var(--upwr-sage)" : f.risk < 0.30 ? "var(--upwr-warning)" : "var(--upwr-accent)");
    },
    noteKey: function (vals, state) { return String(state.k); }
  };

  // Ryc. 9.2 i 9.3 pracują na jednych danych; po zmianie scenariusza albo n dane są nieaktualne.
  store.anova = null;
  function anovaControls() {
    var sc = document.getElementById("ch7_scenario"), n = document.getElementById("ch7_n"), v = document.getElementById("ch7_var");
    return { scenario: sc ? sc.value : "fermentation", n: n ? Number(n.value) : 160, variable: v ? v.value : null };
  }
  // Grupy wybranej zmiennej zależnej albo null, gdy nie ma aktualnych danych.
  function anovaGroups() {
    var c = anovaControls(), s = store.anova;
    if (!s || s.scenario !== c.scenario || s.n !== c.n || !s.data.vars[c.variable]) return null;
    return { names: s.data.groups, values: s.data.vars[c.variable], variable: c.variable, jitter: s.jitter };
  }
  figures.s05_anova = {
    act: function (state) {
      var c = anovaControls();
      store.anova = { scenario: c.scenario, n: c.n, data: anovaData(rng, c.scenario, c.n), jitter: rng.many(c.n, function () { return rng.unif(-0.15, 0.15); }) };
    },
    // Lista „Zmienna zależna” zależy od scenariusza: przy jego zmianie wymieniamy pozycje.
    prepare: function (state, v, vals, el) {
      var c = anovaControls(), sel = el.querySelector("#ch7_var"), cfg = state.config.scenarios[c.scenario];
      if (!sel || sel.getAttribute("data-scenario") === c.scenario) return;
      sel.textContent = "";
      Object.keys(cfg.vars).forEach(function (label) { var o = document.createElement("option"); o.value = cfg.vars[label]; o.textContent = label; sel.appendChild(o); });
      sel.setAttribute("data-scenario", c.scenario);
    },
    draw: function (v, P, k, state) {
      var d = anovaGroups();
      if (!d) return null;
      var cfg = state.config.scenarios[anovaControls().scenario], cols = [P.ink, P.bursztyn, P.niebo, P.szalwia], layers = [], labels = {}, out = [], pts = [], all = [], pos = 0;
      var boxes = d.values.map(function (gr, i) {
        labels[i + 1] = d.names[i];
        var bx = boxOf(gr);
        gr.forEach(function (y) { all.push(y); pts.push([i + 1 + d.jitter[pos++ % d.jitter.length], y]); if (y < bx.lo || y > bx.hi) out.push([i + 1, y]); });
        return { pos: i + 1, width: 0.75, q1: bx.q1, med: bx.med, q3: bx.q3, lo: bx.lo, hi: bx.hi, fill: C.alpha(cols[i], 0.6) };
      });
      var lo = Math.min.apply(null, all), hi = Math.max.apply(null, all), pad = (hi - lo) * 0.05 || 1;
      return { x: [0.4, d.names.length + 0.6], y: [lo - pad, hi + pad], xLabels: labels,
        layers: [{ type: "box", boxes: boxes, stroke: P.accent }, { type: "points", data: out, color: P.accent, size: 6, alpha: 0.3 },
                 { type: "points", data: pts, color: P.accent, size: 4, alpha: 0.2 }],
        xName: cfg.group_label, yName: cfg.var_labels[d.variable], aria: "Wykresy pudełkowe zmiennej zależnej w grupach." };
    },
    update: function (el) {
      var box = el.querySelector('[data-role="result"]'), d = anovaGroups();
      notify(["s05_gh"]);
      if (!box) return;
      if (!d) { box.innerHTML = ""; return; }
      var a = T.anovaF(d.values);
      box.innerHTML = S.status("<p><strong>Wynik ANOVA jednoczynnikowej:</strong></p><p>F(" + a.df1 + ", " + a.df2 + ") = " + T.round(a.F, 3) + "</p>" + pValueHtml(a.p) + decisionHtml(a.p));
    },
    noteKey: function () { return anovaGroups() ? "data" : "empty"; }
  };

  // Ryc. 9.3: porównania par Gamesa-Howella na danych z Ryc. 9.2 (macierz p, wykres różnic, lista).
  figures.s05_gh = {
    act: function (state) { state.asked = true; },
    prepare: function (state) {
      var d = state.asked ? anovaGroups() : null;
      state.d = d; state.gh = d ? T.gamesHowell(d.values) : null;
    },
    draw: function (v, P, k, state) {
      var d = state.d;
      if (!d) return null;
      var rows = state.gh.map(function (g) { return { label: d.names[g.i] + " — " + d.names[g.j], g: g }; });
      rows.sort(function (a, b) { return a.label < b.label ? -1 : a.label > b.label ? 1 : 0; });   // oś kategorii: alfabetycznie od dołu
      var labels = {}, layers = [], lo = 0, hi = 0, sig = {};
      rows.forEach(function (r, i) {
        var y = i + 1, col = r.g.p < 0.05 ? P.accent : P.szalwia;
        labels[y] = r.label; lo = Math.min(lo, r.g.lo); hi = Math.max(hi, r.g.hi); sig[r.g.p < 0.05] = true;
        layers.push({ type: "segs", color: col, width: 1.8, items: [{ from: [r.g.lo, y], to: [r.g.hi, y] }, { from: [r.g.lo, y - 0.1], to: [r.g.lo, y + 0.1] }, { from: [r.g.hi, y - 0.1], to: [r.g.hi, y + 0.1] }] });
        layers.push({ type: "point", x: r.g.estimate, y: y, color: col, size: 10 });
      });
      layers.push({ type: "vline", x: 0, color: P.secondary, width: 1.4, dash: [6, 5] });
      var legend = [];
      if (sig[false]) legend.push({ color: P.szalwia, text: "p ≥ 0.05", box: true });
      if (sig[true]) legend.push({ color: P.accent, text: "p < 0.05", box: true });
      var pad = (hi - lo) * 0.05 || 1;
      return { x: [lo - pad, hi + pad], y: [0.4, rows.length + 0.6], layers: layers, yLabels: labels, noYGrid: true, legend: legend,
        xName: "Różnica średnich", yName: "", aria: "Różnice średnich między parami grup z 95% przedziałami ufności." };
    },
    update: function (el, state) {
      var matrix = el.querySelector('[data-role="matrix"]'), result = el.querySelector('[data-role="result"]'), d = state.d, e = S.esc;
      el.querySelectorAll("[data-gh-part]").forEach(function (n) { n.hidden = !d; });
      if (!d) { matrix.innerHTML = ""; result.innerHTML = state.asked ? "" : ""; return; }
      // Macierz p-wartości (format jamovi: dolno-trójkątna, z gwiazdkami istotności).
      var k = d.names.length, pOf = function (i, j) { return state.gh.filter(function (g) { return g.i === j && g.j === i; })[0].p; };
      var head = '<th scope="col"></th>' + d.names.map(function (n) { return '<th scope="col" class="n">' + e(n) + "</th>"; }).join("");
      var body = d.names.map(function (name, i) {
        return '<tr><th scope="row">' + e(name) + "</th>" + d.names.map(function (col, j) {
          if (j > i) return '<td class="n" data-label="' + e(col) + '"></td>';
          if (j === i) return '<td class="n is-dim" data-label="' + e(col) + '">—</td>';
          var p = pOf(i, j), stars = p < 0.001 ? " ***" : p < 0.01 ? " **" : p < 0.05 ? " *" : "";
          return '<td class="n' + (p < 0.05 ? " is-base" : "") + '" data-label="' + e(col) + '">' + (p < 0.001 ? "&lt; 0.001" : p.toFixed(3)) + stars + "</td>";
        }).join("") + "</tr>";
      }).join("");
      matrix.innerHTML = '<div class="lc-tbl-block"><table class="lc-tbl is-fit"><thead><tr>' + head + "</tr></thead><tbody>" + body +
        '</tbody></table><div class="lc-tbl-note">p-wartości skorygowane metodą Games-Howella. * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</div></div>';
      var sigPairs = state.gh.filter(function (g) { return g.p < 0.05; });
      result.innerHTML = !sigPairs.length
        ? '<div class="lc-caption" data-tone="info"><div><strong>Żadna para nie różni się istotnie</strong> (po korekcie Games-Howella).</div></div>'
        : S.status('<p><span class="lc-status-ok"><strong>' + sigPairs.length + " " + pluralDiff(sigPairs.length) + ":</strong></span></p><ul>" +
            sigPairs.map(function (g) { return "<li>" + e(d.names[g.i] + " — " + d.names[g.j] + ": Δ = " + T.round(g.estimate, 2) + ", p.adj = " + T.fmtPValue(g.p)) + "</li>"; }).join("") + "</ul>");
    },
    noteKey: function (vals, state) { return !state.asked ? "none" : state.d ? "data" : "nodata"; }
  };

  // ---- Rozdział 10: siła efektu ------------------------------------------------
  // Ryc. 10.1: ten sam efekt d przy różnych n daje różne p. Pozostałe ryciny rozdziału
  // (przykłady w surowych liczbach) opisuje R wariantami (modules/figures.R).
  figures.s05_dist = {
    draw: function (v, P, k, state, vals) {
      var d = Number(vals.ch10_d), a = -4, bb = d + 4, g1 = [], g2 = [], i, x;
      for (i = 0; i < 600; i++) { x = a + (bb - a) * i / 599; g1.push([x, D.dnorm(x, 0, 1)]); g2.push([x, D.dnorm(x, d, 1)]); }
      return { x: [a, bb], y: [0, 0.52],
        layers: [{ type: "line", data: g1, color: P.niebo, width: 1.8, fill: P.niebo, fillAlpha: 0.35 },
                 { type: "line", data: g2, color: P.accent, width: 1.8, fill: P.accent, fillAlpha: 0.35 },
                 { type: "vline", x: 0, color: P.niebo, width: 1.6, dash: [6, 5] },
                 { type: "vline", x: d, color: P.accent, width: 1.6, dash: [6, 5] },
                 { type: "arrows", both: true, items: [{ from: [0, 0.45], to: [d, 0.45] }], color: P.soft, width: 1.4 },
                 { type: "text", x: d / 2, y: 0.465, text: "d = " + d, color: P.ink, bold: true }],
        legend: [{ color: P.niebo, text: "Grupa 1 (średnia = 0)", box: true }, { color: P.accent, text: "Grupa 2 (średnia = d = " + d + ")", box: true }],
        xName: "", yName: "Gęstość", aria: "Dwa rozkłady normalne przesunięte o d odchyleń standardowych." };
    },
    update: function (el) {
      var d = Number(el.querySelector("#ch10_d").value), n = Number(el.querySelector("#ch10_n").value), r = effectTest(d, n);
      S.setValue(el, "dist_n", String(n)); S.setValue(el, "dist_d", d + "  (" + r.label + ")");
      S.setValue(el, "dist_t", T.round(r.t, 2)); S.setValue(el, "dist_p", T.fmtPValue(r.p));
      var box = el.querySelector('[data-role="verdict"]');
      if (box) box.innerHTML = S.status('<span class="lc-status-' + (r.p < 0.05 ? "warning" : "ok") + '"><strong>' +
        (r.p < 0.05 ? "Odrzucamy H₀" : "Brak podstaw do odrzucenia H₀") + "</strong></span>");
    }
  };

  S.register(figures);
})();
