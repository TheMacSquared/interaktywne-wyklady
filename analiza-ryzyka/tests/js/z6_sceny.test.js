// Silnik scen wykładu 06 (modules/sceny.js) bez przeglądarki i bez serwera R.
// Uruchamianie: node --test analiza-ryzyka/tests/js/z6_sceny.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../06-zmiennosc-i-prog/modules/sceny.js"));
const Z = globalThis.lcZ6, U = Z.util;

const near = (a, b, tol) => assert.ok(Math.abs(a - b) <= tol, `${a} ≉ ${b}`);

test("pnorm i dnorm zgadzają się z R", () => {
  near(U.pnorm(85, 82, 3), 0.8413447, 1e-6);      // pnorm(85, 82, 3)
  near(1 - U.pnorm(91, 82, 3), 0.001349898, 1e-7);
  near(U.pnorm(0, 10, Math.sqrt(113)), 0.1734, 1e-4);  // P(L > S) dla zawiesia
  near(U.pnorm(-1), 0.1586553, 1e-6);
  near(U.dnorm(82, 82, 3), 0.1329808, 1e-7);
});

test("fmtP jak risk_fmt_p() i naturalna częstość", () => {
  assert.equal(U.fmtP(0.1586553), "0.159");
  assert.equal(U.fmtP(0.001349898), "0.00135");  // sprintf("%.5f")
  assert.equal(U.fmtP(0.0062097), "0.00621");
  assert.equal(U.per1000(0.1586553), "159 na 1000");
  assert.equal(U.per1000(0.0062097), "6 na 1000");
  assert.equal(U.per1000(0.00077), "0.8 na 1000");
});

test("curve i normPath: krańce i liczba punktów", () => {
  const c = U.curve(82, 3, 68, 98, 0.1);
  assert.equal(c.length, 301);
  assert.deepEqual(c[0][0], 68);
  near(c[140][1], U.dnorm(82, 82, 3), 1e-12);
  const d = U.normPath(82, 3, x => x, y => 100 - 100 * y, 70, 94, true, 5);
  assert.ok(d.startsWith("M70.0,100.0 L70.0,"));
  assert.ok(d.endsWith("L94.0,100.0 Z"));
});

test("mulberry32 jest powtarzalny, rnorm ma rozsądne momenty", () => {
  const a = U.mulberry32(7), b = U.mulberry32(7);
  for (let i = 0; i < 5; i++) assert.equal(a(), b());
  const rnd = U.mulberry32(11), xs = [];
  for (let i = 0; i < 20000; i++) xs.push(U.rnorm(rnd));
  near(U.mean(xs), 0, 0.03);
  near(U.mean(xs.map(x => x * x)), 1, 0.05);
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
