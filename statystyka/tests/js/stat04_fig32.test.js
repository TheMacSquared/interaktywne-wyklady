// Ryc. 3.2 wykładu 04 (modules/fig_32.js): liczby przedziału dla średniej i tekst kroków.
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

function load() {
  const noop = () => {};
  const window = {};
  const document = { addEventListener: noop, querySelectorAll: () => [], documentElement: {} };
  class MutationObserver { observe() {} }
  const src = fs.readFileSync(path.join(__dirname, "../../04-przedzialy-ufnosci/modules/fig_32.js"), "utf8");
  vm.runInNewContext(src, { window, document, MutationObserver });
  return window.lcStat04Fig32;
}
const near = (a, b, tol = 1e-4) => assert.ok(Math.abs(a - b) < tol, a + " ≠ " + b);

test("Ryc. 3.2: średnia, SE, margines i granice jak t.test() w R", () => {
  const F = load(), x = [];
  for (let i = 0; i < 25; i++) x.push(160 + i);   // 160, 161, …, 184
  const s = F.stats(x);
  // R: x <- 160:184; mean = 172; sd = 7.359801; se = 1.47196; t.test(x)$conf.int = 168.9620 175.0380
  near(s.m, 172); near(s.s, 7.359801); near(s.se, 1.47196);
  near(s.lo, 168.9620, 1e-3); near(s.hi, 175.0380, 1e-3);
});

test("Ryc. 3.2: tekst ostatniego kroku mówi, czy przedział zawiera μ = 170", () => {
  const F = load(), x = [];
  for (let i = 0; i < 25; i++) x.push(160 + i);
  assert.ok(F.text(F.stats(x), 4).includes("zawiera μ = 170 cm"));
  assert.ok(F.text(F.stats(x.map(v => v + 20)), 4).includes("nie zawiera μ = 170 cm"));
});

test("Ryc. 3.3: różnica średnich, SE i liczba stopni swobody Welcha jak w R", () => {
  const F = load(), x = [], y = [];
  for (let i = 0; i < 25; i++) { x.push(170 + i); y.push(160 + 0.5 * i); }
  const s = F.diffStats(x, y);
  // R: x <- 170:194; y <- 160 + 0.5 * (0:24); t.test(x, y): różnica 16, se = 1.645701, df = 35.29412.
  near(s.d, 16); near(s.se, 1.645701); near(s.df, 35.29412, 1e-3);
  // Bez lcDist kwantyl jest stały (2.063899); granice sprawdzamy względem niego.
  near(s.lo, 16 - s.t * s.se); near(s.hi, 16 + s.t * s.se);
  assert.ok(F.textDiff(s, 5).includes("Przedział nie obejmuje 0"));
});
