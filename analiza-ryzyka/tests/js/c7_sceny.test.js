// Silnik scen wykładu 07 (modules/sceny.js) bez przeglądarki i bez serwera R.
// Uruchamianie: node --test analiza-ryzyka/tests/js/c7_sceny.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../07-czas-zycia/modules/sceny.js"));
const Z = globalThis.lcC7, U = Z.util;

const near = (a, b, tol) => assert.ok(Math.abs(a - b) <= tol, `${a} ≉ ${b}`);

test("Weibull i wykładniczy zgadzają się z przykładami wykładu", () => {
  near(U.wR(1000, 1, 1500), 0.5134171, 1e-6);       // przykład 7.1, oferta A
  near(U.wR(1000, 2, 1700), 0.7075, 1e-4);          // oferta B
  near(U.wR(3000, 2, 1700), 0.0444, 1e-4);
  near(U.wh(1000, 2, 1700), 0.000692, 1e-6);        // przykład 7.4
  near(U.wh(1000, 1, 1500), 1 / 1500, 1e-12);
  near(U.wf(1000, 1, 1500), 0.000342, 1e-6);        // przykład 7.3
  near(U.wH(1000, 2, 1700), 0.346, 1e-3);
  near(U.wF(1500, 1, 1500), 1 - Math.exp(-1), 1e-12);
  assert.equal(U.wR(0, 0.5, 1700), 1);
});

test("termin Bq, kwantyl i MTTF (wzory 7.14 i 7.16)", () => {
  near(U.wTime(0.90, 2, 1700), 551.8, 0.1);         // przykład 7.10: B10 ≈ 552 h
  near(U.wTime(0.95, 2, 1700), 385.0, 0.1);
  near(U.wTime(0.90, 1, 1500), 158.0, 0.1);
  near(U.wq(0.5, 1, 1500), 1500 * Math.LN2, 1e-9);  // mediana ≈ 1040 h
  near(U.gammaFn(1.5), Math.sqrt(Math.PI) / 2, 1e-12);
  near(U.gammaFn(5), 24, 1e-9);
  near(U.wMean(2, 1700), 1506.6, 0.1);              // przykład 7.7: MTTF ≈ 1507 h
  near(1500 / U.gammaFn(1 + 1 / 0.7), 1185.0, 0.1); // przykład 7.8
  near(1500 / U.gammaFn(1 + 1 / 2.5), 1690.6, 0.1);
});

test("park kwantyli daje liczby z tekstu", () => {
  const alive = (p, t) => p.filter(x => x > t).length;
  const a = U.park(100, 1, 1500), b = U.park(100, 2, 1700);
  assert.equal(a.length, 100);
  assert.deepEqual([alive(a, 1000), alive(b, 1000)], [51, 71]);
  assert.deepEqual([alive(a, 3000), alive(b, 3000)], [14, 4]);
  const a2 = U.park(200, 1, 1500);
  // P7: okno 300 h przy t = 1000 h i t = 3000 h
  assert.equal(alive(a2, 1000), 103);
  assert.equal(alive(a2, 1000) - alive(a2, 1300), 19);
  assert.equal(alive(a2, 3000), 27);
  assert.equal(alive(a2, 3000) - alive(a2, 3300), 5);
  for (let i = 1; i < a.length; i++) assert.ok(a[i] > a[i - 1]);
});

test("fmtP, fmtH i fcurve", () => {
  assert.equal(U.fmtP(0.5134171), "0.513");
  assert.equal(U.fmtH(1039.7), "1040 h");
  assert.equal(U.fmtH(1812.5, 1), "1812.5 h");
  const c = U.fcurve(t => U.wR(t, 1, 1500), 0, 5000, 50);
  assert.equal(c.length, 51);
  assert.deepEqual(c[0], [0, 1]);
  near(c[50][1], Math.exp(-5000 / 1500), 1e-12);
});

test("mulberry32 jest powtarzalny", () => {
  const a = U.mulberry32(7), b = U.mulberry32(7);
  for (let i = 0; i < 5; i++) assert.equal(a(), b());
});

test("scena kadry przełącza kadry i blokuje krańce", () => {
  const out = { head: { innerHTML: "" }, stage: { innerHTML: "" }, body: { innerHTML: "" } };
  const btn = { prev: { disabled: false }, next: { disabled: false } };
  const ui = {
    set: (name, html) => { out[name].innerHTML = html; return true; },
    ends: (f, n) => { btn.prev.disabled = f === 0; btn.next.disabled = f === n - 1; }
  };
  const frames = [1, 2, 3].map(i => ({ name: "K" + i, svg: "<svg>" + i + "</svg>", body: "b" + i }));
  const scene = Z.scenes.kadry({ frames }, ui);
  scene.render();
  assert.match(out.head.innerHTML, /K1.*Kadr 1 z 3/);
  assert.equal(btn.prev.disabled, true);
  scene.on.next(); scene.on.next(); scene.on.next(); scene.render();
  assert.equal(out.stage.innerHTML, "<svg>3</svg>");
  assert.equal(btn.next.disabled, true);
  scene.on.prev(); scene.render();
  assert.equal(out.body.innerHTML, "b2");
});
