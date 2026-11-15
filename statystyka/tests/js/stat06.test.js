// Ryciny wykładu 06 (06-zalozenia-testow/modules/figures.js): funkcje liczące.
// Wyniki testów względem R sprawdza lc_stats.test.js; tutaj losowanie i zliczanie.
// Uruchamianie: node --test statystyka/tests/js/stat06.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

globalThis.window = globalThis;
require(path.resolve(__dirname, "../../../shared/lc_dist.js"));
require(path.resolve(__dirname, "../../../shared/lc_stats.js"));
require(path.resolve(__dirname, "../../06-zalozenia-testow/modules/figures.js"));
const Z = globalThis.lcS06, T = globalThis.lcStats, D = globalThis.lcDist;

test("losowanie prób: liczebność, środek i rozrzut pięciu rozkładów", () => {
  const r = D.rng(606);
  for (const dist of ["normal", "skewed", "heavy_tail", "bimodal", "uniform"]) {
    assert.equal(Z.sample(r, 50, dist).length, 50, dist);
    const x = Z.sample(r, 20000, dist), m = T.mean(x);
    // R: średnie teoretyczne 170, 160 (gamma 2·5 + 150), 170, 170, 170.
    assert.ok(Math.abs(m - (dist === "skewed" ? 160 : 170)) < 0.6, `${dist}: średnia ${m}`);
  }
  const u = Z.sample(r, 5000, "uniform");
  assert.ok(Math.min(...u) >= 150 && Math.max(...u) <= 190);
});

test("odsetek odrzuceń Shapiro–Wilka jak na Ryc. 1.3 (5000 prób w R)", () => {
  const r = D.rng(2026);
  const rate = (dist, n, reps) => {
    let k = 0;
    for (let i = 0; i < reps; i++) if (T.shapiroWilk(Z.sample(r, n, dist)).p < 0.05) k++;
    return 100 * k / reps;
  };
  // Wartości z ryciny: normalny 5%, prawoskośny 95% przy n = 50, ciężkie ogony 65%, jednostajny 74%.
  const normal = rate("normal", 50, 2000);
  assert.ok(normal > 3.5 && normal < 6.5, `normalny: ${normal}%`);
  assert.ok(Math.abs(rate("skewed", 50, 600) - 95) < 4);
  assert.ok(Math.abs(rate("heavy_tail", 50, 800) - 65) < 6);
  assert.ok(Math.abs(rate("uniform", 50, 800) - 74) < 6);
  assert.ok(Math.abs(rate("skewed", 10, 800) - 25) < 5);
});

test("liczba przedziałów, wykres pudełkowy i przedziały p-wartości", () => {
  // R: nclass.Sturges(rnorm(50)) = 7, dla n = 200: 9.
  assert.equal(Z.histBins(50, "normal"), 7);
  assert.equal(Z.histBins(200, "normal"), 9);
  assert.equal(Z.histBins(50, "bimodal"), 14);
  // R: boxplot.stats(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 30)) — wąsy 1 i 9, odstająca 30;
  // zawiasy geom_boxplot to kwartyle typu 7: 3.25, 5.5, 7.75.
  const b = Z.boxStats([1, 2, 3, 4, 5, 6, 7, 8, 9, 30]);
  assert.deepEqual([b.q1, b.med, b.q3, b.lo, b.hi], [3.25, 5.5, 7.75, 1, 9]);
  assert.deepEqual(b.out, [30]);
  const bins = Z.pBins([0, 0.05, 0.050001, 0.5, 1, 0.999]);
  assert.equal(bins.length, 20);
  assert.deepEqual([bins[0][2], bins[1][2], bins[9][2], bins[19][2]], [2, 1, 1, 2]);   // (a, b], pierwszy z zerem
  assert.equal(Z.alarmRate([0.01, 0.04, 0.05, 0.5]), 50);
});

test("symulacja χ² i Fishera przy prawdziwej H₀", () => {
  const r = D.rng(31);
  const tab = Z.simTable(r, 40, 0.5);
  assert.equal(tab[0][0] + tab[0][1] + tab[1][0] + tab[1][1], 40);
  // Duże n, równe kategorie: oba testy blisko α (Fisher nieco poniżej).
  const big = Z.simulate(r, 200, 0.5, 1500);
  assert.equal(big.fisher.length, 1500);
  assert.ok(Math.abs(Z.alarmRate(big.chi) - 5) < 2.2, `chi ${Z.alarmRate(big.chi)}`);
  assert.ok(Z.alarmRate(big.fisher) <= Z.alarmRate(big.chi));
  // Małe n i rzadkie kategorie: część tabel bez χ², Fisher konserwatywny.
  const rare = Z.simulate(r, 20, 0.1, 1500);
  assert.ok(rare.chi.length < 1500);
  assert.ok(Z.alarmRate(rare.fisher) < 3, `fisher ${Z.alarmRate(rare.fisher)}`);
});
