// Wykład 05: rachunek piaskownic P7 / „Limit, średnia i rezerwa” (nb_chart.js)
// i P11 (mix_chart.js) bez przeglądarki. Liczby jak w tekście wykładu (r = 3, p = 0.10).
// Uruchamianie: node --test analiza-ryzyka/tests/js/d5_charts.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../05-do-zdarzenia/modules/nb_chart.js"));
require(path.resolve(__dirname, "../../05-do-zdarzenia/modules/mix_chart.js"));
const NB = globalThis.lcD5Nb, MIX = globalThis.lcD5Mix;

function nbExact(x, r, p) {  // C(x−1, r−1) p^r (1−p)^(x−r)
  let c = 1;
  for (let i = 1; i <= r - 1; i++) c = c * (x - 1 - (r - 1) + i) / i;
  return c * Math.pow(p, r) * Math.pow(1 - p, x - r);
}

test("ujemny dwumianowy: rozkład, dystrybuanta i kwantyle", () => {
  const d = NB.pmf(3, 0.1, 400);
  assert.equal(d[0][0], 3);
  for (const [x, y] of d.slice(0, 60)) assert.ok(Math.abs(y - nbExact(x, 3, 0.1)) < 1e-12);
  assert.ok(Math.abs(d.reduce((s, v) => s + v[1], 0) - 1) < 1e-9);
  // Plan bananów: limit 61, P(X ≤ 61) ≈ 0.951 (wzór 5.7); mediana 27 (odczyt startowy P7).
  assert.equal(NB.quantile(3, 0.1, 0.95), 61);
  assert.equal(NB.cdf(3, 0.1, 61).toFixed(3), "0.951");
  assert.equal(NB.quantile(3, 0.1, 0.5), 27);
  // r = 1 to rozkład geometryczny
  assert.ok(Math.abs(NB.cdf(1, 0.1, 10) - (1 - Math.pow(0.9, 10))) < 1e-12);
});

test("dwaj dostawcy: mieszanka obniża szansę planu 61", () => {
  const m = MIX.model(3, 0.1, 0.05, 3000), base = MIX.pmfArr(3, 0.1, 3000);
  assert.equal(MIX.cdf(base, 61).toFixed(3), "0.951");
  assert.equal(MIX.cdf(m.mix, 61).toFixed(2), "0.80");  // odczyt startowy w panelu
  assert.ok(m.q95 > 61);
  assert.ok(Math.abs(MIX.cdf(m.mix, 61) - (MIX.cdf(m.A, 61) + MIX.cdf(m.B, 61)) / 2) < 1e-12);
  assert.equal(MIX.model(3, 0.1, 0, 3000).q95, 61);
});
