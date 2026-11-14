// Wspólne funkcje rycin analizy ryzyka (R/risk_figures.js) względem R.
// Wzorce policzone w R: risk_fmt_p(), risk_normal_exceedance(),
// risk_stress_strength_normal(), risk_natural_frequency().
// Uruchamianie: node --test analiza-ryzyka/tests/js/risk_figures.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

globalThis.window = globalThis;
require(path.resolve(__dirname, "../../../shared/lc_dist.js"));
require(path.resolve(__dirname, "../../R/risk_figures.js"));
const R = globalThis.lcRisk;
const close = (a, b, tol, label) => assert.ok(Math.abs(a - b) <= tol, `${label}: ${a} vs ${b}`);

test("fmtP jak risk_fmt_p() w R", () => {
  const p = [0.5, 0.159, 0.0123, 0.00456, 0.000789, 1e-7, 0, 1, 0.0099, 0.01];
  const r = ["0.500", "0.159", "0.012", "0.00456", "0.000789", "0.000000100", "0.000", "1.000", "0.00990", "0.010"];
  assert.deepEqual(p.map(R.fmtP), r);
  assert.equal(R.fmtP(NaN), "—");
  assert.equal(R.fmtP(Infinity), "—");
});

test("normalExceedance jak risk_normal_exceedance() w R", () => {
  const cases = [[85, 82, 3, 0.158655253931457], [87, 82, 3, 0.0477903522728147], [85, 80, 3, 0.0477903522728147],
                 [85, 82, 2, 0.06680720126885807], [78, 82, 3, 0.9087887802741321]];
  for (const [c, m, s, r] of cases) close(R.normalExceedance(c, m, s), r, 2e-7, `P(X > ${c}) dla N(${m}, ${s})`);
});

test("stressStrength jak risk_stress_strength_normal() w R", () => {
  close(R.stressStrength(85, 8, 95, 7), 0.1734239609325736, 2e-7, "L 85 / S 95");
  close(R.stressStrength(110, 8, 70, 7), 0.9999160172620531, 2e-7, "L 110 / S 70");
  close(R.stressStrength(60, 8, 120, 7), 8.29151780797539e-09, 2e-9, "L 60 / S 120");
  close(R.stressStrength(85, 8, 95, 7, 0.5), 0.09266315209928981, 2e-7, "rho = 0.5");
  assert.equal(R.stressStrength(90, 5, 80, 5, 1), 1);          // różnica bez rozrzutu: pewna awaria
});

test("częstość naturalna, zaokrąglenie i statystyki próby", () => {
  assert.equal(R.naturalFrequency(0.158655), "około 159 na 1 000");   // risk_natural_frequency(0.158655)
  assert.equal(R.naturalFrequency(0.0477, 10000), "około 477 na 10 000");
  assert.equal(R.naturalFrequency(NaN), "—");
  assert.equal(R.big(1234567), "1 234 567");
  assert.equal(R.rnd(82.1, 2), "82.1"); assert.equal(R.rnd(82, 2), "82"); assert.equal(R.rnd(1.005, 1), "1");
  close(R.mean([5, 3, 6, 4, 7]), 5, 1e-12, "mean");
  close(R.sd([5, 3, 6, 4, 7]), 1.58113883008419, 1e-12, "sd");   // R: sd(c(5, 3, 6, 4, 7))
});

test("krzywa normalna i pole ogona zaczynające się w progu", () => {
  const curve = R.normalCurve(82, 3, 70, 94, 25);
  assert.equal(curve.length, 25);
  assert.deepEqual([curve[0][0], curve[24][0]], [70, 94]);
  close(curve[12][1], 0.132980760133811, 1e-9, "dnorm(82, 82, 3)");
  const tail = R.tailCurve(curve, 85.5, (x) => globalThis.lcDist.dnorm(x, 82, 3));
  assert.equal(tail[0][0], 85.5);
  assert.ok(tail.slice(1).every((p) => p[0] > 85.5));
});

test("weibull jak risk_weibull() w R, siatka jak seq() i barwy palety", () => {
  // R: risk_weibull(1000, 2, 1700); risk_weibull(1000, 0.7, 1500 / gamma(1 + 1 / 0.7))
  close(R.weibull(1000, 2, 1700).reliability, 0.7074977984184172, 1e-12, "R(1000), beta 2");
  close(R.weibull(1000, 2, 1700).hazard, 0.0006920415224913495, 1e-15, "h(1000), beta 2");
  close(R.weibull(1000, 0.7, 1500 / 1.265823506057284).reliability, 0.411491042909168, 1e-9, "R(1000), beta 0.7");
  const c = R.curve((x) => 2 * x, 0, 4000, 400);
  assert.equal(c.length, 400);
  assert.deepEqual([c[0], c[399]], [[0, 0], [4000, 8000]]);
  assert.deepEqual(R.cat(3), ["#2d3b3d", "#c08540", "#6a9dc4"]);   // upwr_cat_n(3)
});

test("dokładne połówki zaokrąglane do parzystej, jak sprintf() w R", () => {
  // R: sprintf("%.3f", c(0.0625, 0.1875, 0.3125, 0.4375, 0.5625, 0.9375))
  assert.deepEqual([0.0625, 0.1875, 0.3125, 0.4375, 0.5625, 0.9375].map(R.fmtP),
                   ["0.062", "0.188", "0.312", "0.438", "0.562", "0.938"]);
  // R: sprintf("%.0f", c(0.5, 1.5, 2.5)); sprintf("%.1f", c(0.25, 0.75)); sprintf("%.4f", 0.01865)
  assert.deepEqual([0.5, 1.5, 2.5].map((x) => R.fixed(x, 0)), ["0", "2", "2"]);
  assert.deepEqual([0.25, 0.75].map((x) => R.fixed(x, 1)), ["0.2", "0.8"]);
  assert.equal(R.fixed(0.01865, 4), (0.01865).toFixed(4));       // nie połówka: jak toFixed
  assert.equal(R.fixed(0.5525, 3), (0.5525).toFixed(3));
});

test("fmtPd jak risk_fmt_p(x, 6) w R", () => {
  // R: vapply(c(0.00063, 0.000674, 0.0000698, 0.126, 0), risk_fmt_p, "", digits = 6)
  assert.deepEqual([0.00063, 0.000674, 0.0000698, 0.126, 0].map((x) => R.fmtPd(x, 6)),
                   ["0.000630", "0.000674", "0.0000698", "0.126000", "0.000000"]);
});
