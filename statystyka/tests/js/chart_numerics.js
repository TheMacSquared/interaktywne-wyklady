// Wykonuje funkcje produkcyjne JS na danych dostarczonych przez testthat.
// Wartości referencyjne i tolerancje są obliczane niezależnie w R.
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const statRoot = path.resolve(__dirname, "../..");
const input = JSON.parse(fs.readFileSync(process.argv[2], "utf8"));
const document = { documentElement: {}, addEventListener() {}, querySelectorAll: () => [] };
const window = { addEventListener() {} };
const context = vm.createContext({
  window, document,
  MutationObserver: class { observe() {} }
});
for (const file of ["R/lc_charts.js", "01-typy-danych/modules/chart_utils.js", "01-typy-danych/modules/kurtosis_chart.js"]) {
  vm.runInContext(fs.readFileSync(path.join(statRoot, file), "utf8"), context, { filename: file });
}
const C = window.lcCharts, U = window.lc01;
// Stały strumień wejściowy eliminuje losową niestabilność testów rozkładów.
// Nie porównujemy sekwencji z set.seed() w R: generatory mają inne algorytmy.
vm.runInContext(`Math.random = (function () {
  var state = 20261031;
  return function () {
    state = (Math.imul(1664525, state) + 1013904223) >>> 0;
    return (state + 0.5) / 4294967296;
  };
})();`, context);
const output = {
  statistics: input.statistics.map(({ x, p, trim }) => ({
    mean: U.mean(x), median: U.median(x),
    quantiles: p.map(q => U.quantile7(x, q)),
    sortedQuantiles: p.map(q => U.quantile7(U.sorted(x), q, true)),
    trimmedMeans: trim.map(t => U.trimMean(x, t))
  })),
  probabilities: input.z.map(z => C.normalCdf(z)),
  intervals: input.intervals.map(({ a, b }) => C.normalCdf(b) - C.normalCdf(a)),
  quantiles: input.quantiles.map(({ p, mu, sd, lo, hi }) => C.normalQuantile(p, mu, sd, lo, hi)),
  fits: input.fits.map(({ x, y }) => C.linearFit(x, y)),
  kurtosis: input.kurtosis.map(({ ek, x, cutoffs }) => {
    const density = U.kurtDensity(ek);
    return { density: x.map(density), upper: cutoffs.map(c => U.kurtUpper(density, c)) };
  }),
  sampling: input.sampling.map(({ kind, n, a, b }) => {
    const generate = {
      normal: () => U.rnorm(a, b), gamma: () => U.rgammaInt(a, b),
      uniform: () => U.runif(a, b), integer: () => U.sampleInt(a, b)
    }[kind];
    const values = Array.from({ length: n }, generate), mean = U.mean(values);
    return {
      mean, variance: values.reduce((s, x) => s + (x - mean) ** 2, 0) / (n - 1),
      quantiles: [0.1, 0.5, 0.9].map(p => U.quantile7(values, p)),
      counts: kind === "integer" ? Array.from({ length: b - a + 1 }, (_, i) => values.filter(x => x === a + i).length) : [],
      min: Math.min(...values), max: Math.max(...values)
    };
  })
};
fs.writeFileSync(process.argv[3], JSON.stringify(output));
