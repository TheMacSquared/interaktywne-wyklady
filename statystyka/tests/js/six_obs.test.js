// Ryc. 3.5 „Szósta obserwacja” (01-typy-danych/modules/six_obs.js) bez przeglądarki.
// Wzorce liczb policzone w R: mean(), median(), lc_fmt(x, 1, big_mark = " ").
// Uruchamianie: node --test statystyka/tests/js/six_obs.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../01-typy-danych/modules/six_obs.js"));
const S = globalThis.lcS01Six;
const base = [5, 3, 6, 4, 7];

test("liczby jak lc_fmt(x, 1, big_mark = \" \") w R", () => {
  const x = [0.25, 0.35, 5.25, 5.35, 4.35, 2.5, 0.05, 0.15, 1234.56, 1000, 0, -0.04, 12.449999, 7.75, 7.85];
  // R: lc_fmt(x, 1, big_mark = " ") — dokładne połówki do cyfry parzystej
  const r = ["0.2", "0.3", "5.2", "5.3", "4.3", "2.5", "0.1", "0.1", "1 234.6", "1 000", "0", "0", "12.4", "7.8", "7.8"];
  assert.deepEqual(x.map(S.fmt), r);
  assert.equal(S.fmt(-2.5), "-2.5");
});

test("pole x6: liczba albo brak wartości", () => {
  assert.equal(S.parse(""), null);
  assert.equal(S.parse("  "), null);
  assert.equal(S.parse("abc"), null);
  assert.equal(S.parse("60"), 60);
  assert.equal(S.parse("5.5"), 5.5);
  assert.equal(S.parse("0"), 0);
});

test("pięć pensji: środkowa pozycja i brak zmiany", () => {
  const r = S.compute(base, null);
  assert.deepEqual([r.n, r.hasNew, r.mean, r.median, r.sum], [5, false, 5, 5, 25]);
  assert.deepEqual(r.sorted, [3, 4, 5, 6, 7]);
  assert.deepEqual(r.mid, [3]);
  assert.equal(r.meanExact, true);
  assert.match(S.html(r), /Pole x6 jest puste/);
  assert.match(S.html(r), /nieparzyste/);
});

test("szósta pensja: średnia, mediana i zmiana względem pięciu jak w R", () => {
  // R: x <- c(5, 3, 6, 4, 7, v); round(mean(x), 1); round(median(x), 1)
  const cases = [
    [1, "4.3", "4.5", "−0.7 tys. zł", "−0.5 tys. zł"],
    [60, "14.2", "5.5", "+9.2 tys. zł", "+0.5 tys. zł"],
    [5.5, "5.1", "5.2", "+0.1 tys. zł", "+0.2 tys. zł"],
    [5, "5", "5", "0 tys. zł", "0 tys. zł"],
    [1234.56, "209.9", "5.5", "+204.9 tys. zł", "+0.5 tys. zł"]
  ];
  for (const [v, mean, median, dMean, dMedian] of cases) {
    const r = S.compute(base, v);
    assert.deepEqual([S.fmt(r.mean), S.fmt(r.median), S.signed(r.dMean), S.signed(r.dMedian)],
      [mean, median, dMean, dMedian], "x6 = " + v);
    assert.deepEqual(r.mid, [3, 4]);
    assert.equal(r.n, 6);
  }
});

test("posortowane komórki: środek i nowa wartość", () => {
  const r = S.compute(base, 60);
  assert.deepEqual(r.sorted, [3, 4, 5, 6, 7, 60]);
  assert.equal(r.order[5], 5, "szósta wartość jest ostatnia po sortowaniu");
  const html = S.html(r);
  assert.equal((html.match(/ch3-six-cell is-mid/g) || []).length, 2);
  assert.equal((html.match(/is-new/g) || []).length, 1);
  assert.match(html, /<span class="ch3-six-new">60<\/span>/);
  assert.match(html, /≈ 14\.2/);
  assert.match(html, /parzyste/);
  // wartość równa jednej z pięciu: wyróżniona jest nowa, nie stara
  const tie = S.compute(base, 5);
  assert.deepEqual(tie.sorted, [3, 4, 5, 5, 6, 7]);
  assert.equal(tie.order.indexOf(5), 3);
  assert.equal(tie.meanExact, true);
  assert.match(S.html(tie), /= 5<\/span>/);
});
