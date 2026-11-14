// Silnik scen wykładu 08 (modules/sceny.js) bez przeglądarki i bez serwera R.
// Uruchamianie: node --test analiza-ryzyka/tests/js/s8_sceny.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../08-niezawodnosc-systemu/modules/sceny.js"));
const Z = globalThis.lcS8, U = Z.util;

const near = (a, b, tol) => assert.ok(Math.abs(a - b) <= tol, `${a} ≉ ${b}`);

test("rachunek niezawodności zgadza się z przykładami wykładu", () => {
  near(U.series([0.92, 0.95, 0.98]), 0.85652, 1e-12);       // ćwiczenie 2
  near(U.parallel([0.92, 0.95]), 0.996, 1e-12);             // przykład 8.4
  near(0.98 * U.parallel([0.92, 0.95]), 0.97608, 1e-12);    // przykład 8.5
  near(U.common(U.parallel([0.92, 0.95]), 0.01), 0.98604, 1e-12);  // przykład 8.8
  near(U.expR(1000, 1800), 0.5737534, 1e-6);                // przykład 8.6
  near(U.kOfN(2, 3, 0.9), 0.972, 1e-12);                    // układ 2 z 3
  near(U.kOfN(1, 3, 0.9), 0.999, 1e-12);
  near(U.kOfN(3, 3, 0.9), 0.729, 1e-12);
});

test("beta-factor: przykład 8.9 i ćwiczenie 5", () => {
  const lt = -Math.log(0.92);
  near(U.betaR(0, lt), 0.9936, 1e-12);
  near(1 - U.betaR(0.1, lt), 0.01348692, 1e-7);
  near(1 - U.betaR(0.05, lt), 0.009936076, 1e-7);
});

test("fmtP jak risk_fmt_p() i naturalna częstość", () => {
  assert.equal(U.fmtP(0.97608), "0.976");
  assert.equal(U.fmtP(0.01396), "0.014");
  assert.equal(U.fmtP(0.0064), "0.00640");
  assert.equal(U.per1000(0.01396), "14 na 1000");
});

test("toggleBit zmienia stan jednego bloku", () => {
  assert.equal(U.toggleBit(7, 2), 5);
  assert.equal(U.toggleBit(5, 2), 7);
  assert.equal(U.toggleBit(0, 1), 1);
});

function fakeUi() {
  const out = { head: "", stage: "", body: "" };
  const btn = { prev: { disabled: false }, next: { disabled: false }, reset: { disabled: false } };
  return { out, btn, ui: {
    root: { querySelector: sel => (sel.indexOf("reset") >= 0 ? btn.reset : null), querySelectorAll: () => [] },
    set: (name, html) => { out[name] = html; return true; },
    ends: (f, n) => { btn.prev.disabled = f === 0; btn.next.disabled = f === n - 1; }
  } };
}

test("scena kadry przełącza kadry i blokuje krańce", () => {
  const { out, btn, ui } = fakeUi();
  const frames = [1, 2, 3].map(i => ({ name: "K" + i, svg: "<svg>" + i + "</svg>", body: "b" + i }));
  const scene = Z.scenes.kadry({ frames }, ui);
  scene.render();
  assert.match(out.head, /K1.*Kadr 1 z 3/);
  assert.equal(btn.prev.disabled, true);
  scene.on.next(); scene.on.next(); scene.on.next(); scene.render();
  assert.equal(out.stage, "<svg>3</svg>");
  assert.equal(btn.next.disabled, true);
  scene.on.prev(); scene.render();
  assert.equal(out.body, "b2");
});

test("scena uklad: klik bloku zmienia maskę, reset ją przywraca", () => {
  const { out, btn, ui } = fakeUi();
  const views = [0, 1, 2, 3].map(m => ({ svg: "s" + m, body: "b" + m }));
  const scene = Z.scenes.uklad({ n: 2, views }, ui);
  const blk = i => ({ getAttribute: () => String(i) });
  scene.render();
  assert.equal(out.stage, "s3");
  assert.equal(btn.reset.disabled, true);
  scene.on.blk(blk(1)); scene.render();
  assert.equal(out.stage, "s2");                 // blok 1 zepsuty, blok 2 działa
  assert.equal(btn.reset.disabled, false);
  scene.on.blk(blk(2)); scene.render();
  assert.equal(out.body, "b0");
  scene.on.blk(blk(1)); scene.render();
  assert.equal(out.stage, "s1");
  scene.on.reset(); scene.render();
  assert.equal(scene.mask(), 3);
});

test("scena widoki pokazuje widok wciśniętych przycisków", () => {
  const { out, ui } = fakeUi();
  const seg = v => ({ querySelector: () => ({ getAttribute: () => v }) });
  let segs = [seg("same"), seg("fire")];
  ui.root.querySelectorAll = () => segs;
  const views = { "same|fire": { svg: "A", body: "a" }, "rule|fire": { svg: "B", body: "b" } };
  const scene = Z.scenes.widoki({ views }, ui);
  scene.render();
  assert.equal(out.stage, "A");
  segs = [seg("rule"), seg("fire")];
  scene.render();
  assert.equal(out.body, "b");
});

test("mulberry32 jest powtarzalny", () => {
  const a = U.mulberry32(81), b = U.mulberry32(81);
  for (let i = 0; i < 5; i++) assert.equal(a(), b());
});
