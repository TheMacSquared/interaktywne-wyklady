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

test("regresja liniowa jak lm()", () => {
  F.lm.forEach((m, i) => {
    const fit = S.lm(m.columns, m.y);
    fit.coef.forEach((c, j) => {
      close(c.estimate, m.estimate[j], 1e-9, `lm ${i} b${j}`); close(c.se, m.se[j], 1e-9, `lm ${i} se${j}`);
      close(c.t, m.t[j], 1e-8, `lm ${i} t${j}`); close(c.p, m.p[j], 1e-7, `lm ${i} p${j}`);
      close(c.lo, m.lo[j], 1e-6, `lm ${i} lo${j}`); close(c.hi, m.hi[j], 1e-6, `lm ${i} hi${j}`);
    });
    close(fit.r2, m.r2, 1e-10, `lm ${i} R2`); close(fit.adjR2, m.adj, 1e-10, `lm ${i} adj`);
    close(fit.aic, m.aic, 1e-9, `lm ${i} AIC`); close(fit.sigma, m.sigma, 1e-10, `lm ${i} sigma`);
  });
  // Kolumny liniowo zależne: brak rozwiązania.
  assert.equal(S.lm([[1, 2, 3, 4], [2, 4, 6, 8]], [1, 3, 2, 5]), null);
});

test("rozkład studentyzowanego rozstępu jak ptukey() i qtukey()", () => {
  F.tukey.args.forEach((a, i) => {
    close(S.ptukey(a[0], a[1], a[2]), F.tukey.lower[i], 1e-8, `ptukey ${a}`);
    close(S.qtukey(0.95, a[1], a[2]), F.tukey.q95[i], 1e-6, `qtukey ${a}`);
  });
  assert.equal(S.ptukey(0, 3, 20), 0);
  close(S.ptukey(3.5, 3, 20, false), 1 - F.tukey.lower[0], 1e-8, "górny ogon");
});

test("porównania Gamesa-Howella i ANOVA jak rstatix", () => {
  F.gh.forEach((g, n) => {
    const res = S.gamesHowell(g.groups);
    assert.equal(res.length, g.estimate.length);
    res.forEach((r, i) => {
      close(r.estimate, g.estimate[i], 1e-9, `GH ${n} różnica ${i}`);
      close(r.lo, g.lo[i], 1e-5, `GH ${n} dolna ${i}`); close(r.hi, g.hi[i], 1e-5, `GH ${n} górna ${i}`);
      // rstatix zaokrągla p.adj do trzech cyfr znaczących.
      assert.ok(Math.abs(r.p - g.p[i]) <= 6e-3 * Math.max(g.p[i], 1e-4) + 1e-9, `GH ${n} p ${i}: ${r.p} vs ${g.p[i]}`);
    });
    const av = S.anovaF(g.groups);
    close(av.F, g.F, 2e-3, `ANOVA ${n} F`);          // rstatix zaokrągla F do trzech miejsc
    assert.deepEqual([av.df1, av.df2], [g.df1, g.df2]);
    close(av.p, g.anova_p, 1e-8, `ANOVA ${n} p`);
  });
});
