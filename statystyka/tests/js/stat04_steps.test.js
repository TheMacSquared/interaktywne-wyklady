// Wykład 04 — ryciny liczone w przeglądarce w sf_steps.js: Ryc. 3.1 i Przykłady A–C.
// Ryc. 3.2–3.3 i 4.1, 4.3 mają własne testy: stat04_fig32.test.js, stat04_fig41.test.js.
const test = require("node:test");
const assert = require("node:assert/strict");
const { environment, Element, root } = require("./harness");

function load() {
  const env = environment();
  env.load("../shared/lc_dist.js"); env.load("../shared/lc_stat_figures.js");
  env.load("04-przedzialy-ufnosci/modules/ci_look.js");
  env.load("04-przedzialy-ufnosci/modules/sf_steps.js");
  const F = env.window.lcSF.figures, P = env.window.lcCharts.palette();
  const run = (name, config) => {
    const fig = typeof F[name] === "function" ? { draw: F[name] } : F[name], state = { config };
    if (fig.init) fig.init(state);
    return { fig, state, spec: (k, step) => fig.draw(null, P, k || 0, state, {}, step) };
  };
  return { env, F, P, run };
}
const near = (a, b, eps = 1e-6) => assert.ok(Math.abs(a - b) < eps, `${a} ≠ ${b}`);
const types = spec => Array.from(spec.layers, l => l.type);
const texts = spec => Array.from(spec.layers.filter(l => l.type === "text"), l => l.text);
const plain = o => JSON.parse(JSON.stringify(o));  // obiekt z kontekstu vm

test("Ryc. 3.1: gęstości N(0, 1) i t(4), ogony poza ±t*, podziałki ±1.96 i ±2.78", () => {
  const { run } = load();
  const s = run("s04-tnorm").spec();
  assert.deepEqual(Array.from(s.xTicks), [-2.78, -1.96, 0, 1.96, 2.78]);
  assert.deepEqual([...s.x, ...s.y], [-4.5, 4.5, 0, 0.43]);
  assert.deepEqual(types(s), ["line", "line", "line", "line", "segment", "segment", "segment", "segment", "text", "text"]);
  // Ogony: same punkty |x| ≥ t* = 2.776445 (R: qt(0.975, 4)).
  assert.ok(s.layers[0].data.every(p => p[0] >= 2.776445));
  assert.ok(s.layers[1].data.every(p => p[0] <= -2.776445));
  assert.ok(s.layers[0].fill && s.layers[0].fillAlpha === 0.3);
  near(s.layers[6].to[1], 0.02558082 + 0.06);   // dt(t*, 4) + 0.06
  assert.deepEqual(texts(s), ["N(0, 1)", "t, df = 4"]);
});

test("Przykłady A–C: CI grup z pasem nakładania i CI różnicy z zerem", () => {
  const { run } = load();
  const A = { g1_name: "Dostawca A", g2_name: "Dostawca B", unit: "zawartość białka (%)", m1: 11.96, m2: 10.97,
              ci1: [11.76, 12.16], ci2: [10.80, 11.15], md: 0.99, ci_d: [0.72, 1.25] };
  let r = run("s04-comp", A), top = r.spec(0), bot = r.spec(1);
  assert.ok(!types(top).includes("band"), "rozłączne CI: bez pasa nakładania");
  assert.deepEqual(plain(top.yLabels), { 1: "Dostawca B", 2: "Dostawca A" });
  // Średnie nad kreskami, granice przedziałów po bokach klamer (wygląd Ryc. 3.2).
  assert.deepEqual(texts(top), ["11.96", "11.76", "12.16", "10.97", "10.80", "11.15"]);
  near(top.x[0], 10.80 - 0.2 * 1.36); near(top.x[1], 12.16 + 0.2 * 1.36);
  assert.equal(top.xName, "zawartość białka (%)");
  assert.equal(types(top).filter(t => t === "bracket").length, 2);
  // Dolny: oś obejmuje zero (linia odniesienia), CI różnicy w wierszu 1.
  near(bot.x[0], -0.2 * 1.25); near(bot.x[1], 1.25 * 1.2);
  assert.equal(bot.yLabels[1], "Dostawca A − Dostawca B");
  assert.equal(bot.xName, "różnica — zawartość białka (%)");
  assert.deepEqual(texts(bot), ["brak różnicy", "0.99", "0.72", "1.25"]);
  const br = bot.layers.find(l => l.type === "bracket");
  assert.deepEqual([br.lo, br.hi, br.y], [0.72, 1.25, 1]);
  // B: CI grup się nakrywają — żółty pas na odcinku wspólnym.
  r = run("s04-comp", { ...A, ci1: [3.03, 3.17], ci2: [3.11, 3.28], m1: 3.10, m2: 3.20, md: -0.10, ci_d: [-0.21, 0.01] });
  const band = r.spec(0).layers.find(l => l.type === "band");
  assert.deepEqual([band.lo, band.hi], [3.11, 3.17]);
  assert.equal(band.color, "#f1c40f");
});

