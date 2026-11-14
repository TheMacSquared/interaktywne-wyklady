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
  close(Z.power(0.05, 7, 40).power, 0.925850528, 1e-7, "moc 7 pkt, n = 40");
  close(Z.power(0.05, 0, 40).power, 0.05, 1e-7, "bez efektu moc = α");
  close(Z.power(0.01, 3, 100).power, 0.3942975, 1e-6, "moc 3 pkt, n = 100, α = 0.01");   // R: 0.3942975
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
