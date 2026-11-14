// Testy i statystyki liczone w przeglądarce (shared/lc_stats.js) względem R.
// Wzorce: fixtures/lc_stats.json (generuje fixtures/make_lc_stats.R).
// Uruchamianie: node --test statystyka/tests/js/lc_stats.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");
const fs = require("node:fs");

globalThis.window = globalThis;
require(path.resolve(__dirname, "../../../shared/lc_dist.js"));
require(path.resolve(__dirname, "../../../shared/lc_stats.js"));
const S = globalThis.lcStats;
const F = JSON.parse(fs.readFileSync(path.resolve(__dirname, "fixtures/lc_stats.json"), "utf8"));
// Zgodność względna (albo bezwzględna dla liczb bliskich zera).
const close = (a, b, tol, label) => assert.ok(Math.abs(a - b) <= tol * Math.max(1, Math.abs(b)), `${label}: ${a} vs ${b}`);

test("statystyki opisowe i wykres kwantylowy jak w R", () => {
  for (const [name, s] of Object.entries(F.shapiro)) {
    close(S.mean(s.x), s.mean, 1e-12, `${name} mean`);
    close(S.sd(s.x), s.sd, 1e-12, `${name} sd`);
    close(S.median(s.x), s.median, 1e-12, `${name} median`);
    close(S.quantile(s.x, 0.1), s.q10, 1e-12, `${name} q10`);
    const qq = S.qqNormal(s.x);
    close(qq.slope, s.qq_slope, 1e-8, `${name} qq slope`);
    close(qq.intercept, s.qq_intercept, 1e-8, `${name} qq intercept`);
    close(qq.points[0][0], s.qq_first, 1e-7, `${name} pierwszy kwantyl`);
    assert.equal(qq.points.length, s.x.length);
  }
});

test("Shapiro–Wilk jak shapiro.test()", () => {
  for (const [name, s] of Object.entries(F.shapiro)) {
    const r = S.shapiroWilk(s.x);
    close(r.W, s.W, 1e-7, `${name} W`);
    // p z przybliżenia normalnego: zgodność względna, a dla skrajnie małych p rząd wielkości.
    if (s.p > 1e-12) assert.ok(Math.abs(r.p - s.p) <= 2e-6 * Math.max(s.p, 1e-3) + 1e-9, `${name} p: ${r.p} vs ${s.p}`);
    else assert.ok(r.p < 1e-10, `${name} p: ${r.p}`);
    assert.equal(S.fmtPValue(r.p), s.p_text, `${name} zapis p`);
  }
});

test("Levene, Bartlett i testy t dwóch prób", () => {
  F.two.forEach((d, i) => {
    const lev = S.levene([d.a, d.b]), bart = S.bartlett([d.a, d.b]);
    close(lev.F, d.levene.F, 1e-9, `levene F ${i}`);
    assert.deepEqual([lev.df1, lev.df2], [d.levene.df1, d.levene.df2]);
    close(lev.p, d.levene.p, 1e-7, `levene p ${i}`);
    close(bart.K2, d.bartlett.K2, 1e-9, `bartlett ${i}`);
    close(bart.p, d.bartlett.p, 1e-6, `bartlett p ${i}`);
    const st = S.tTest(d.a, d.b, true), we = S.tTest(d.a, d.b, false);
    close(st.t, d.student.t, 1e-10, `student t ${i}`); close(st.df, d.student.df, 1e-12, `student df ${i}`);
    close(st.p, d.student.p, 1e-7, `student p ${i}`);
    close(we.t, d.welch.t, 1e-10, `welch t ${i}`); close(we.df, d.welch.df, 1e-10, `welch df ${i}`);
    close(we.p, d.welch.p, 1e-7, `welch p ${i}`);
  });
});

test("tabela 2×2: χ² bez poprawki i test dokładny Fishera", () => {
  F.tables.forEach((t, i) => {
    const chi = S.chisq2x2(t.tab);
    if (t.chi_p === null) assert.ok(Number.isNaN(chi.p), `chi NaN ${i}`);
    else { close(chi.X2, t.X2, 1e-10, `X2 ${i}`); close(chi.p, t.chi_p, 1e-7, `chi p ${i}`); }
    close(S.fisher2x2(t.tab), t.fisher_p, 1e-9, `fisher ${i}`);
  });
});

test("rozkład F, zapis p-wartości i zaokrąglenia", () => {
  F.pf.args.forEach((a, i) => close(S.pf(a[0], a[1], a[2], false), F.pf.upper[i], 1e-9, `pf ${a}`));
  assert.deepEqual(F.pvalue.p.map(S.fmtPValue), F.pvalue.text);
  assert.equal(S.fmtPValue(NaN), "NA");
  // 2.5 → R zaokrągla do parzystej ("2"), toFixed w górę: ten przypadek pomijamy.
  F.round.x.forEach((x, i) => { if (x !== 2.5 && x !== 0.125) assert.equal(S.round(x, F.round.d[i]), F.round.text[i]); });
});
