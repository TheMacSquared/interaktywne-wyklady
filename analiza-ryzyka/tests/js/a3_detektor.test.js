// Detektor wykładu 03 (modules/detektor.js) bez przeglądarki: rachunek jak risk_math.R
// i rysunki jak w R (tablica 2×2, drzewo). Ryciny lcSF wymagają strony; tu liczby.
// Uruchamianie: node --test analiza-ryzyka/tests/js/a3_detektor.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../03-alarm-i-prawda/modules/detektor.js"));
const A = globalThis.lcA3;
const C = { accent: "#6b1a2a", alt: "#b48a2a" };

test("tablica 2×2 i posterior jak w tekście wykładu", () => {
  const d = A.detectorCounts(10000, 0.01, 0.95, 0.05);
  assert.deepEqual(d, { alarm: [95, 495], noAlarm: [5, 9405], total: [100, 9900] });
  assert.equal(A.fmtP(A.bayes(0.01, 0.95, 0.05)), "0.161");
  const e = A.detectorCounts(10000, 0.05, 0.95, 0.05);
  assert.deepEqual(e.alarm, [475, 475]);
  // round() z R: 25 · 0.5 = 12.5 → 12
  assert.equal(A.detectorCounts(10000, 0.0025, 0.5, 0.005).alarm[0], 12);
  const html = A.crosstab(d);
  assert.match(html, /<td class="n" data-label="Brak alarmu"><span class="lc-pad" aria-hidden="true">0000<\/span>5<\/td>/);
  assert.match(html, /<td class="n is-total">10 000<\/td>/);
});

test("dwa alarmy: kopiowanie zjada zysk z drugiego czujnika", () => {
  assert.deepEqual([0, 0.25, 0.5, 1].map(d => A.fmtP(A.twoAlarm(0.01, 0.95, 0.05, d))), ["0.785", "0.391", "0.263", "0.161"]);
  const c = A.doubleCounts(0.01, 0.95, 0.05, 0);
  assert.deepEqual(c.count.map(Math.round), [90, 25]);
  assert.deepEqual(c.ceiling.map(Math.round), [95, 495]);
});

test("drzewo: atrybuty dla drzewo.js i szerokości gałęzi", () => {
  const svg = A.treeSvg(A.detectorCounts(10000, 0.01, 0.95, 0.05), 0.01, 0.95, 0.05, C);
  assert.match(svg, /data-prev="0.01" data-sens="0.95" data-fpr="0.05" data-tp="95" data-fn="5" data-fp="495" data-tn="9405"/);
  assert.match(svg, /data-k="e-fa" d="M395 230 C440 230 440 190 485 190" style="stroke-width:24.0;stroke:#b48a2a"/);
  assert.match(svg, />590<\/text>/);
  assert.match(svg, />Zmiany · 10 000</);
  // risk_fmt_p(p, 2) przy małych p
  assert.match(svg, /data-k="p-a" x="186" y="96" text-anchor="middle">0.01</);
});

test("Serpuchów: łańcuch szans, mnożniki i oś", () => {
  const ch = A.spChain(5, 3, 0.9);
  assert.deepEqual([A.fmtP(ch.p[1]), A.fmtP(ch.p[2])], ["0.00990", "0.000999"]);
  assert.deepEqual([A.spFactor(1000), A.spFactor(0.1), A.spFactor(Math.pow(10, 5.5)), A.spFactor(Math.pow(10, 0.5))],
    ["× 1 000", "× 0.1", "× 320 000", "× 3.2"]);
  assert.deepEqual([-8, -4, -2, 0, 2, 4].map(A.spAxisLabel), ["10⁻⁸", "10⁻⁴", "0.01", "0.5", "0.99", "0.9999"]);
  // s ograniczone do 0.99 jak w R
  assert.ok(Math.abs(A.spChain(5, 3, 1).logOdds[2] + 4) < 1e-12);
});
