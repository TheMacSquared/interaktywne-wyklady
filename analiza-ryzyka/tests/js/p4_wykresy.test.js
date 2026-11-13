// Wykresy wykładu 04 (modules/wykresy.js) bez przeglądarki: liczby zgodne z R
// i opisy rycin lcSF (zakresy, warstwy, odczyty). ECharts nie jest potrzebny —
// atrapa lcSF zbiera zarejestrowane ryciny, lcDist jest prawdziwy.
// Uruchamianie: node --test analiza-ryzyka/tests/js/p4_wykresy.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

globalThis.window = globalThis;
require(path.resolve(__dirname, "../../R/lc_dist.js"));
const figures = {};
const values = {};
globalThis.lcCharts = { token: name => name };
globalThis.lcSF = {
  register: f => Object.assign(figures, f),
  setValue: (root, key, text) => { values[key] = text; },
  esc: s => String(s)
};
require(path.resolve(__dirname, "../../04-wiele-prob/modules/wykresy.js"));
const M = globalThis.lcP4Charts;
const P = { ink: "ink", surface: "surface", accent: "accent", secondary: "secondary", ref: "ref", rule: "rule", szalwia: "szalwia" };
const near = (a, b, tol = 1e-12) => assert.ok(Math.abs(a - b) < tol, `${a} ≠ ${b}`);
const byType = (spec, type) => spec.layers.filter(l => l.type === type);

// Atrapa kontenera ryciny: elementy po id i odczyty [data-v].
function fakeRoot(inputs = {}) {
  const els = {};
  Object.entries(inputs).forEach(([id, value]) => { els[id] = { value: String(value), max: "", style: {} }; });
  return {
    querySelector(sel) {
      const m = sel.match(/^#([\w]+)$/);
      if (m) return els[m[1]] || null;
      return { textContent: "", style: { setProperty() {} }, closest: () => ({ style: { setProperty() {} } }), innerHTML: "" };
    }
  };
}

test("funkcje dwumianowe zgodne z R", () => {
  near(M.pbinom(1, 50, 0.02), 0.735771394461726);
  near(M.pbinom(1, 50, 0.08), 0.0827120229268151);
  near(M.pbinom(2, 100, 0.02), 0.676685622351779);
  assert.deepEqual([[100, 0.02], [10, 0.5], [20, 0.25], [10, 0.8]].map(([n, p]) => M.qbinom(0.9999, n, p)), [9, 10, 13, 10]);
  assert.equal(M.fmtP(0.0004321), "0.000432");  // jak risk_fmt_p() w R
  assert.equal(M.fmtP(0.302), "0.302");
  assert.deepEqual(M.share([0, 0, 2, 15], 9), [0.5, 0, 0.25, 0, 0, 0, 0, 0, 0, 0.25]);
});

test("Rozkład: odpowiedź jak w tekście wykładu i k przycięte do n", () => {
  const fig = figures.p4_binom, state = {};
  const spec = fig.draw(null, P, 0, state, { p4_n: "10", p4_p: "0.2", p4_k: "2", p4_query: "exactly" });
  fig.update(fakeRoot(), state);
  assert.equal(values.bi1, "0.302");
  assert.equal(byType(spec, "bins")[0].colors.filter(c => c === "accent").length, 1);
  fig.draw(null, P, 0, state, { p4_n: "10", p4_p: "0.2", p4_k: "3", p4_query: "at_least" });
  fig.update(fakeRoot(), state);
  assert.equal(values.bi1, "0.322");
  const root = fakeRoot({ p4_k: 20 });
  fig.prepare(state, null, { p4_n: "8" }, root);
  assert.equal(root.querySelector("#p4_k").value, "8");
  assert.equal(root.querySelector("#p4_k").max, "8");
});

test("Trzy partie: widok razem i osobno", () => {
  const cfg = {
    ref: Array(11).fill(0.05), obs: { stable: Array(11).fill(0.1), mixture: Array(11).fill(0.1), dependent: Array(11).fill(0.1) },
    reads: { stable: { mean: "2.01", p0: "0.133", p5: "0.056" } },
    split: { obsA: Array(11).fill(0.1), obsB: Array(11).fill(0.1), refA: Array(11).fill(0), refB: Array(11).fill(0), mean: "1.00 · 3.01", p0A: "0.366", p0B: "0.051" }
  };
  const fig = figures.p4_assume, state = { config: cfg };
  fig.prepare(state, "stable", { p4p_split: "apart" });
  assert.equal(state.split, false);
  assert.equal(fig.noteKey({}, state), "joint");
  assert.deepEqual(fig.draw("stable", P, 0, state).x, [-0.6, 10.6]);
  fig.prepare(state, "mixture", { p4p_split: "apart" });
  const spec = fig.draw("mixture", P, 0, state);
  assert.equal(fig.noteKey({}, state), "split");
  assert.equal(spec.xLabels[22], "10+");
  assert.equal(byType(spec, "text").length, 2);
  fig.update(fakeRoot(), state, "mixture");
  assert.equal(values.as1, "1.00 · 3.01");
  assert.equal(values.as3_l, "P(X = 0) w B");
});

test("Galeria B(n, p): oś do 0.9999 kwantyla, pusta bez wyboru", () => {
  const defs = { b1: { label: "Moneta", n: 10, p: 0.5, mean: "5", sd: "1.58" }, b4: { label: "Zawory", n: 100, p: 0.02, mean: "2", sd: "1.40" } };
  const fig = figures.p4_bg, state = { config: { defs } };
  fig.prepare(state, ["b4"]);
  assert.deepEqual(fig.draw(null, P, 0, state).x, [-0.6, 10.6]);
  fig.prepare(state, ["b1", "b4"]);
  const spec = fig.draw(null, P, 0, state);
  assert.equal(spec.legend.length, 2);
  // kropki rozsunięte jak position_dodge(0.5)
  assert.deepEqual(byType(spec, "points").map(l => l.data[0][0]), [-0.125, 0.125]);
  fig.prepare(state, []);
  assert.equal(fig.draw(null, P, 0, state), null);
});

test("Partia po partii i Powtórzenia: losowanie, średnia i odczyty", () => {
  const bt = figures.p4_bt, s = {};
  bt.init(s);
  bt.update(fakeRoot(), s);
  assert.equal(values.bt2, "—");
  bt.act(s, "thou"); bt.act(s, "ten");
  assert.equal(s.xs.length, 1010);
  const m = s.xs.reduce((a, b) => a + b, 0) / s.xs.length;
  assert.ok(m > 1.7 && m < 2.3, `średnia ${m}`);
  assert.equal(byType(bt.draw(null, P, 0, s), "vline").length, 1);
  bt.act(s, "reset");
  assert.equal(s.xs.length, 0);

  const ba = figures.p4_batches, b = { config: { first: [0, 2, 2, 5] } };
  ba.init(b);
  ba.update(fakeRoot(), b);
  assert.deepEqual([values.ba2, values.ba3, values.ba4], ["2.25", "50%", "0–5"]);
  ba.act(b);
  assert.equal(b.xs.length, 500);
});

test("Krzywa, skrót n · p i krzywa operacyjna: odczyty jak w R", () => {
  figures.p4_one.update(fakeRoot(), {}, "0.02");
  assert.equal(values.on1, "0.867");
  figures.p4_np.update(fakeRoot(), {}, "0.005");
  assert.deepEqual([values.np1, values.np2, values.np3], ["0.394 vs 0.500", "200", "0.633"]);
  figures.p4_np.update(fakeRoot(), {}, "0.003");
  assert.equal(values.np2, "334");
  const oc = { config: { good: 0.02, bad: 0.08 } };
  figures.p4_oc.update(fakeRoot({ p4p_oc_n: 50, p4p_oc_c: 1 }), oc);
  assert.deepEqual([values.oc1, values.oc2], ["0.264", "0.083"]);
  figures.p4_oc.update(fakeRoot({ p4p_oc_n: 100, p4p_oc_c: 2 }), oc);
  assert.equal(values.oc1, "0.323");
});

test("Trzech dostawców i domknięcie: stałe ryciny", () => {
  const zero = figures.p4_zero(null, P, 0, { config: { n: [30, 100, 300] } });
  assert.deepEqual(byType(zero, "text").map(t => t.text),
    ["30 kontroli: p ≤ 0.095", "100 kontroli: p ≤ 0.030", "300 kontroli: p ≤ 0.010"]);
  const closing = figures.p4_closing(null, P);
  near(byType(closing, "bins")[0].data[0][2], Math.pow(0.98, 100));
  assert.equal(byType(closing, "text").length, 5);
});
