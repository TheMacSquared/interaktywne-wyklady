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
