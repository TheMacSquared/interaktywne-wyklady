// Seria kontroli zaworów wykładu 04 (modules/seria.js) bez przeglądarki.
// Uruchamianie: node --test analiza-ryzyka/tests/js/p4_seria.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../04-wiele-prob/modules/seria.js"));
const S = globalThis.lcP4Seria;

function mulberry32(a) {
  return function () {
    a |= 0; a = a + 0x6D2B79F5 | 0;
    let t = Math.imul(a ^ a >>> 15, 1 | a);
    t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}

test("seria ma n prób o wynikach 0 albo 1", () => {
  for (const n of [0, 10, 100, 200]) {
    const s = S.draw(n, 0.02, mulberry32(n + 1));
    assert.equal(s.length, n);
    assert.ok(s.every((x) => x === 0 || x === 1));
  }
});

test("próg losowania: sukces, gdy liczba losowa jest mniejsza od p", () => {
  const values = [0.019, 0.02, 0.5, 0.0];
  let i = 0;
  assert.deepEqual(S.draw(4, 0.02, () => values[i++]), [1, 0, 0, 1]);
});

test("częstość sukcesów w długiej serii jest bliska p", () => {
  const s = S.draw(200000, 0.02, mulberry32(42));
  const share = s.reduce((a, b) => a + b, 0) / s.length;
  assert.ok(Math.abs(share - 0.02) < 0.002, String(share));
});

test("zapis serii i wartość oczekiwana jak w dawnej wersji serwerowej", () => {
  assert.equal(S.sequenceText([0, 1, 0, 0]), "· × · ·");
  assert.equal(S.sequenceText([]), "");
  // R: round(n * 0.02, 1)
  assert.equal(S.expectedText(100, 0.02), "2");
  assert.equal(S.expectedText(10, 0.02), "0.2");
  assert.equal(S.expectedText(130, 0.02), "2.6");
});
