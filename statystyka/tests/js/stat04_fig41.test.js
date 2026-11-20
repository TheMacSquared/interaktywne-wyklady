// Ryc. 4.1 wykładu 04 (modules/fig_41.js): liczby przedziału dla proporcji i tekst kroków.
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
  const src = fs.readFileSync(path.join(__dirname, "../../04-przedzialy-ufnosci/modules/fig_41.js"), "utf8");
  vm.runInNewContext(src, { window, document, MutationObserver });
  return window.lcStat04Fig41;
}
const near = (a, b, tol = 1e-6) => assert.ok(Math.abs(a - b) < tol, a + " ≠ " + b);

test("Ryc. 4.1: p̂, SE, margines i granice jak w R (30 × TAK na 50)", () => {
  const F = load(), s = F.stats(30);
  // R: p = 0.6; SE = sqrt(0.6 * 0.4 / 50) = 0.06928203; ME = qnorm(0.975) * SE = 0.1357903.
  near(s.p, 0.6); near(s.se, 0.06928203); near(s.me, 0.1357903);
  near(s.lo, 0.6 - 0.1357903); near(s.hi, 0.6 + 0.1357903);
});

test("Ryc. 4.1: tekst ostatniego kroku podaje przedział ułamkiem i w procentach", () => {
  const F = load(), txt = F.text(F.stats(27), 4);
  assert.ok(txt.includes("[0.402, 0.678]"), txt);
  assert.ok(txt.includes("od 40.2% do 67.8%"), txt);
});

test("Ryc. 4.3: różnica proporcji, SE i granice jak w R (45 i 29 × TAK na 60)", () => {
  const F = load(), s = F.diffStats(45, 29);
  // R: p1 = 0.75, p2 = 29/60; se = sqrt(p1*(1-p1)/60 + p2*(1-p2)/60) = 0.08536414;
  // ME = qnorm(0.975) * se = 0.1673106; granice 0.09935602 i 0.4339773.
  near(s.d, 0.2666667); near(s.se, 0.08536414); near(s.me, 0.1673106);
  near(s.lo, 0.09935602); near(s.hi, 0.4339773);
  const txt = F.textDiff(s, 5);
  assert.ok(txt.includes("[0.099, 0.434]"), txt);
  assert.ok(txt.includes("w grupie 1 odsetek TAK jest wyższy"), txt);
});
