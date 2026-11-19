// Ryciny wykładu 05 (05-wnioskowanie-statystyczne/modules/figures.js): funkcje liczące.
// Wzorce policzone w R (podane przy asercjach).
// Uruchamianie: node --test statystyka/tests/js/stat05.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

globalThis.window = globalThis;
require(path.resolve(__dirname, "../../../shared/lc_dist.js"));
require(path.resolve(__dirname, "../../../shared/lc_stats.js"));
require(path.resolve(__dirname, "../../05-wnioskowanie-statystyczne/modules/figures.js"));
const Z = globalThis.lcS05, T = globalThis.lcStats, D = globalThis.lcDist;
const close = (a, b, tol, label) => assert.ok(Math.abs(a - b) <= tol, `${label}: ${a} vs ${b}`);

test("rozdział 1: eksperyment, powtórzenia pod H₀ i p z symulacji", () => {
  const r = D.rng(505);
  const d = Z.phoneData(r, 40);
  assert.equal(d.plecak.length, 40); assert.equal(d.biurko.length, 40);
  assert.ok(d.plecak.every((v) => Math.abs(v * 10 - Math.round(v * 10)) < 1e-9));   // zaokrąglenie do 0.1 pkt
  const big = Z.phoneData(r, 20000);
  close(T.mean(big.plecak), 72, 0.4, "średnia plecak"); close(T.sd(big.biurko), 14, 0.3, "SD biurko");
  // Różnica średnich dwóch grup po 40 z N(70, 13): SD = 13 · sqrt(2 / 40) ≈ 2.907.
  const diffs = Z.nullDiffs(r, 4000, 40);
  close(T.mean(diffs), 0, 0.15, "średnia różnic"); close(T.sd(diffs), 13 * Math.sqrt(2 / 40), 0.1, "SD różnic");
  assert.equal(Z.simP([-3, -1, 0, 1, 2, 4], 2), 0.5);          // |d| ≥ 2: trzy z sześciu
  assert.equal(Z.simP([-3, -1, 0, 1, 2, 4], -2), 0.5);         // znak obserwowanej różnicy bez znaczenia
});

test("rozdział 1: moc testu jak w R i decyzja w quizie", () => {
  // R: se <- 13 / sqrt(40); lo <- 70 + qnorm(.025) * se; hi <- 70 + qnorm(.975) * se
  //    pnorm(lo, 77, se) + 1 - pnorm(hi, 77, se)  # 0.925850528
  // Dwie grupy po n osób, σ = 13: se = 13·√(2/n). R: pnorm(lo, d, se) + 1 - pnorm(hi, d, se).
  close(Z.power(0.05, 7, 40).power, 0.672969082, 1e-7, "moc 7 pkt, n = 40 w grupie");
  close(Z.power(0.05, 0, 40).power, 0.05, 1e-7, "bez efektu moc = α");
  close(Z.power(0.01, 3, 100).power, 0.172586378, 1e-6, "moc 3 pkt, n = 100 w grupie, α = 0.01");
  assert.equal(Z.quizAnswer({ p: 0.048, alpha: 0.05 }), "reject");
  assert.equal(Z.quizAnswer({ p: 0.06, alpha: 0.01 }), "fail_to_reject");
  assert.equal(Z.quizAnswer({ p: 0.05, alpha: 0.05 }), "fail_to_reject");            // p = α: nie odrzucamy
});

test("rozdział 4: test t jednej próby, histogram kroków i wartości krytyczne", () => {
  // R: x <- c(71.2, 68.4, 75.1, 80.3, 66.7, 73.9, 69.5, 77.8); t.test(x, mu = 70)
  //    t = 1.7056, df = 7, p-value = 0.1319; alternative = "greater": 0.0659; "less": 0.9341
  const x = [71.2, 68.4, 75.1, 80.3, 66.7, 73.9, 69.5, 77.8];
  const two = Z.tOne(x, 70, "two.sided");
  close(two.t, 1.705573, 1e-5, "t"); assert.equal(two.df, 7); close(two.p, 0.1318561, 1e-6, "p dwustronne");
  close(Z.tOne(x, 70, "greater").p, 0.0659281, 1e-6, "p prawostronne");
  close(Z.tOne(x, 70, "less").p, 0.9340719, 1e-6, "p lewostronne");
  const bins = Z.hist15(x);
  assert.equal(bins.length, 15);
  assert.equal(bins.reduce((s, b) => s + b[2], 0), 8);
  assert.equal(bins[0][2], 1); assert.equal(bins[14][2], 1);       // minimum w pierwszym, maksimum w ostatnim przedziale
  // R: qt(.975, 39) = 2.022691; qt(.95, 39) = 1.684875; qchisq(.95, 3) = 7.814728
  close(Z.critical("t", 39, "two.sided").values[1], 2.022691, 1e-5, "qt dwustronne");
  close(Z.critical("t", 39, "greater").values[0], 1.684875, 1e-5, "qt prawostronne");
  close(Z.critical("t", 39, "less").values[0], -1.684875, 1e-5, "qt lewostronne");
  close(Z.critical("chisq", 3).values[0], 7.814728, 1e-5, "qchisq");
  assert.ok(Z.critical("t", 39, "less").reject(-2) && !Z.critical("t", 39, "less").reject(2));
});

test("rozdział 5: test dwumianowy i test proporcji jak binom.test() i prop.test()", () => {
  // R: binom.test(k, n, p)$p.value i prop.test(k, n, p = p, correct = TRUE)$p.value
  const cases = [[45, 50, 0.8, 0.0788306, 0.0480272, 0.1116118], [34, 50, 0.6, 0.3122589, 0.1560906, 0.3123214],
                 [3, 50, 0.03, 0.1892019, 0.1892019, 0.4070893], [70, 80, 0.95, 0.0065343, 0.9979009, 0.0047809],
                 [2, 20, 0.5, 0.0004025, 0.9999800, 0.0007962]];
  for (const [k, n, p0, two, greater, prop] of cases) {
    close(Z.binomTest(k, n, p0, "two.sided").p, two, 2e-7, `binom dwustronny ${k}/${n}`);
    close(Z.binomTest(k, n, p0, "greater").p, greater, 2e-7, `binom prawostronny ${k}/${n}`);
    close(Z.propTest(k, n, p0).p, prop, 2e-7, `prop.test ${k}/${n}`);
  }
  const t = Z.binomTest(45, 50, 0.8, "two.sided");
  assert.equal(t.probs.length, 51);
  assert.ok(t.extreme[45] && t.extreme[50] && !t.extreme[40]);
  // Poprawka Yatesa nie przekracza samej różnicy: k = np₀ daje z = 0.
  assert.equal(Z.propTest(40, 50, 0.8).z, 0);
});

test("rozdział 6: test korelacji jak cor.test() i losowanie par o zadanej korelacji", () => {
  // R: x <- c(6.1, 7.4, 5.2, 8.0, 6.8, 7.9, 5.5, 9.1, 6.3, 7.0); y <- c(62, 75, 58, 71, 66, 80, 61, 84, 70, 64)
  //    cor.test(x, y): r = 0.9033888, t = 5.958572, df = 8, p = 0.00033871; "greater": 0.00016935; lm: 24.937771 + 6.372616 x
  const x = [6.1, 7.4, 5.2, 8.0, 6.8, 7.9, 5.5, 9.1, 6.3, 7.0], y = [62, 75, 58, 71, 66, 80, 61, 84, 70, 64];
  const r = Z.corTest(x, y, "two.sided");
  close(r.r, 0.9033888, 1e-7, "r"); close(r.t, 5.958572, 1e-5, "t"); assert.equal(r.df, 8);
  close(r.p, 0.00033871, 1e-7, "p"); close(Z.corTest(x, y, "greater").p, 0.00016935, 1e-7, "p prawostronne");
  close(Z.corTest(x, y, "less").p, 1 - 0.00016935, 1e-7, "p lewostronne");
  close(r.slope, 6.372616, 1e-5, "nachylenie"); close(r.intercept, 24.937771, 1e-4, "wyraz wolny");
  // Duża próba: korelacja blisko zadanej, średnie i rozrzut jak w scenariuszu.
  const d = Z.corData(D.rng(64), 5000, { r_true: -0.55, x_mean: 20, x_sd: 7, y_mean: 8, y_sd: 3 });
  close(Z.corTest(d.x, d.y).r, -0.55, 0.03, "korelacja próby");
  close(T.mean(d.y), 8, 0.15, "średnia y"); close(T.sd(d.x), 7, 0.25, "SD x");
});

test("rozdział 7: test χ² i test Fishera dla tabel r × c jak chisq.test() i fisher.test()", () => {
  // R: chisq.test(m) i fisher.test(m) dla macierzy wierszami.
  const cases = [
    { tab: [[3, 37], [6, 34], [9, 31]], stat: 3.529412, df: 2, p: 0.17123714, fisher: 0.19043885, e11: 6 },
    { tab: [[6, 16, 18], [22, 14, 4], [28, 10, 2]], stat: 34.257143, df: 4, p: 0.00000066, fisher: 0.00000042, e11: 18.66667 },
    { tab: [[18, 14, 8], [48, 10, 2]], stat: 14.482323, df: 2, p: 0.00071648, fisher: 0.00063524, e11: 26.4 },
    { tab: [[1, 9], [0, 12], [5, 3]], stat: 12.65625, df: 2, p: 0.00178538, fisher: 0.00134394, e11: 2 }];
  const r = D.rng(7);
  for (const c of cases) {
    const chi = Z.chisqTest(c.tab);
    close(chi.stat, c.stat, 1e-5, "χ²"); assert.equal(chi.df, c.df); close(chi.p, c.p, 2e-8, "p χ²"); close(chi.expected[0][0], c.e11, 1e-4, "oczekiwana");
    const f = Z.fisherTest(c.tab, r);
    assert.ok(f.exact);
    close(f.p, c.fisher, 2e-8, "p Fishera");
  }
  // Powyżej limitu wyliczenia: symulacja (B = 2000), wynik blisko dokładnego.
  const sim = Z.fisherTest(cases[0].tab, r, 10);
  assert.ok(!sim.exact);
  close(sim.p, 0.19043885, 0.04, "p Fishera z symulacji");
  // Losowanie tabeli: suma n, kształt jak macierz prawdopodobieństw.
  const tab = Z.tableSample(r, 120, [[0.05, 0.95], [0.12, 0.88], [0.2, 0.8]]);
  assert.equal(tab.length, 3);
  assert.equal(tab.flat().reduce((s, v) => s + v, 0), 120);
});

test("rozdział 8: test t dla par jak t.test(paired = TRUE) i losowanie grup", () => {
  // R: a <- c(48.2, 55.1, 39.7, 61.3, 52.8, 44.0, 58.6, 47.3); b <- c(55.0, 57.4, 46.2, 60.1, 61.9, 49.5, 66.3, 50.8)
  //    t.test(b, a, paired = TRUE): średnia różnic 5.025, t = 4.262834, df = 7, p = 0.00373352
  const PAIRED = [5.025, 4.262834, 7, 0.00373352];
  const a = [48.2, 55.1, 39.7, 61.3, 52.8, 44.0, 58.6, 47.3], b = [55.0, 57.4, 46.2, 60.1, 61.9, 49.5, 66.3, 50.8];
  const r = Z.pairedTest(a, b);
  close(r.mean, PAIRED[0], 1e-6, "średnia różnica"); close(r.t, PAIRED[1], 1e-5, "t"); assert.equal(r.df, PAIRED[2]); close(r.p, PAIRED[3], 1e-7, "p");
  const g = D.rng(8), s = Z.studentVar(g, 4000, "wzrost");
  close(T.mean(s.K), 166, 0.4, "wzrost kobiet"); close(T.mean(s.M), 178, 0.4, "wzrost mężczyzn"); close(T.sd(s.M), 7, 0.3, "SD wzrostu mężczyzn");
  const gpa = Z.studentVar(g, 4000, "srednia_ocen");
  assert.ok(Math.min(...gpa.K, ...gpa.M) >= 2 && Math.max(...gpa.K, ...gpa.M) <= 5);
  close(T.mean(gpa.K), 0.3 * 3.6 + 0.25 * 3.4 + 0.25 * 3.8 + 0.2 * 3.5, 0.05, "średnia ocen");
  close(T.mean(Z.studentVar(g, 4000, "czas_dojazdu").M), 30, 1.2, "czas dojazdu: gamma(3, 10)");
  const pd = Z.pairedData(g, 5000, 5);
  close(Z.pairedTest(pd.przed, pd.po).mean, 5, 0.4, "średnia zmiana"); close(T.sd(pd.przed), 12, 0.4, "SD przed");
});
