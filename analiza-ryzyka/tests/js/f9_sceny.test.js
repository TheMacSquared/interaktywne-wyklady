// Silnik scen wykładu 09 (modules/sceny.js) bez przeglądarki i bez serwera R.
// Uruchamianie: node --test analiza-ryzyka/tests/js/f9_sceny.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../09-drzewo-bledow/modules/sceny.js"));
const F = globalThis.lcF9, U = F.util;

const near = (a, b, tol) => assert.ok(Math.abs(a - b) <= tol, `${a} ≉ ${b}`);
const leaf = (id, ev) => ({ id, label: [id], sym: id, ev: ev || id });
const gate = (id, kind, ...children) => ({ id, label: [id], gate: kind, children });

// Drzewo Bananpolu: TOP = I ∩ (D ∪ S).
const bananpol = gate("TOP", "and", leaf("I"), gate("M", "or", leaf("D"), leaf("S")));
// Przykład 9.6: (Z ∪ B1) ∩ (Z ∪ B2), Z narysowane dwa razy.
const channels = gate("ND", "and", gate("K1", "or", leaf("Z1", "Z"), leaf("B1")), gate("K2", "or", leaf("Z2", "Z"), leaf("B2")));

test("evalTree: TOP zapalają dokładnie trzy z ośmiu kombinacji", () => {
  const hits = U.subsets(U.events(bananpol)).filter(s => U.evalTree(bananpol, s).TOP);
  assert.deepEqual(hits, [["I", "S"], ["I", "D"], ["I", "D", "S"]]);
  assert.equal(U.subsets(["I", "D", "S"]).length, 8);
  assert.equal(U.evalTree(bananpol, ["D", "S"]).M, true);
  assert.equal(U.evalTree(bananpol, ["D", "S"]).TOP, false);
});

test("probTree: rachunek bramka po bramce jak risk_fta_top()", () => {
  const p = U.probTree(bananpol, { I: 0.005, D: 0.05, S: 0.08 });
  near(p.M, 0.126, 1e-12);
  near(p.TOP, 0.00063, 1e-12);
  // Powtórzony liść liczony naiwnie: 0.118² (przykład 9.6a).
  near(U.probTree(channels, { Z: 0.02, B1: 0.1, B2: 0.1 }).ND, 0.013924, 1e-9);
});

test("cutSets i probCuts: minimalne przekroje i wynik dokładny", () => {
  assert.deepEqual(U.cutSets(bananpol), [["D", "I"], ["I", "S"]]);
  near(U.probCuts(U.cutSets(bananpol), { I: 0.005, D: 0.05, S: 0.08 }), 0.00063, 1e-12);
  // Pochłanianie: (Z ∪ B1) ∩ (Z ∪ B2) = Z ∪ (B1 ∩ B2); P = 0.0298 (przykład 9.6b).
  assert.deepEqual(U.cutSets(channels), [["Z"], ["B1", "B2"]]);
  near(U.probCuts(U.cutSets(channels), { Z: 0.02, B1: 0.1, B2: 0.1 }), 0.0298, 1e-12);
  assert.deepEqual(U.events(channels), ["Z", "B1", "B2"]);
});

test("fmtP jak risk_fmt_p(), num bez końcowych zer", () => {
  assert.equal(U.fmtP(0.126), "0.126");
  assert.equal(U.fmtP(0.00063), "0.000630");  // jak odczyt panelu rachunku
  assert.equal(U.num(0.14348), "0.14348");
  assert.equal(U.num(0.0050, 3), "0.005");
  assert.equal(U.num(6.3, 1), "6.3");
});

test("layout i tree: pozycje, zapalone gałęzie, klikalne liście", () => {
  const L = U.layout(bananpol);
  const at = id => L.nodes.find(n => n.n.id === id);
  assert.equal(at("I").col, 0);
  assert.equal(at("M").col, 1.5);
  assert.equal(at("TOP").col, 0.75);
  assert.ok(U.layout(bananpol, "tight").w < L.w);
  const svg = U.tree(bananpol, { lit: ["I", "D"], values: { M: "0.126" }, p: { I: "0.005" } }, { click: "leaf" });
  assert.ok(svg.startsWith("<svg"));
  assert.equal((svg.match(/f9p-node is-on/g) || []).length, 4);   // I, D, M, TOP
  assert.equal((svg.match(/data-f9p="leaf"/g) || []).length, 3);
  assert.match(svg, />0\.126</);
  assert.match(svg, />AND</);
  // Ukryty węzeł znika z rysunku, ale układ zostaje.
  assert.ok(!U.tree(bananpol, { hide: ["S"] }).includes(">S<"));
});

test("scena drzewo przełącza kadry i widoki", () => {
  const out = { head: "", stage: "", body: "" }, btn = { prev: false, next: false };
  const ui = { set: (n, h) => { out[n] = h; return true; }, ends: (f, n) => { btn.prev = f === 0; btn.next = f === n - 1; } };
  const frames = [{ name: "A", body: "a", panels: [{ tree: "main" }] },
                  { name: "B", body: "b", panels: [{ tree: "main", lit: ["I", "S"] }, { svg: "<svg>x</svg>", title: "T" }] }];
  const scene = F.scenes.drzewo({ trees: { main: bananpol }, frames, nav: "kadry" }, ui);
  scene.render();
  assert.match(out.head, /A.*Kadr 1 z 2/);
  assert.ok(!out.stage.includes("is-on"));
  assert.equal(btn.prev, true);
  scene.on.next(); scene.render();
  assert.ok(out.stage.includes("is-on") && out.stage.includes("<svg>x</svg>") && out.stage.includes("<h4>T</h4>"));
  assert.equal(out.body, "b");
  assert.equal(btn.next, true);
});
