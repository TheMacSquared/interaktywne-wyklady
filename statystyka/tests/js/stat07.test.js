// Ryciny wykładu 07 (07-regresja/modules/figures.js): funkcje liczące.
// Wzorce policzone w R: fixtures/make_stat07.R → fixtures/stat07.json.
// Uruchamianie: node --test statystyka/tests/js/stat07.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

globalThis.window = globalThis;
require(path.resolve(__dirname, "../../../shared/lc_dist.js"));
require(path.resolve(__dirname, "../../../shared/lc_stats.js"));
require(path.resolve(__dirname, "../../07-regresja/modules/figures.js"));
const Z = globalThis.lcS07, T = globalThis.lcStats, D = globalThis.lcDist;
const R = require("./fixtures/stat07.json");
const close = (a, b, tol, label) => assert.ok(Math.abs(a - b) <= tol, `${label}: ${a} vs ${b}`);

test("regresja prosta: współczynniki, r i test nachylenia jak lm()", () => {
  const f = Z.simpleFit(R.x, R.y);
  close(f.b0, R.simple.b0, 1e-10, "b0"); close(f.b1, R.simple.b1, 1e-10, "b1");
  close(f.r, R.simple.r, 1e-12, "r"); close(f.se1, R.simple.se1, 1e-10, "SE(b1)"); close(f.p1, R.simple.p1, 1e-9, "p");
});

test("wielomian ortogonalny jak lm(y ~ poly(x, k)): miary i przewidywania", () => {
  for (const ref of R.poly) {
    const f = Z.polyFit(R.x, R.y, ref.k);
    close(f.r2, ref.r2, 1e-9, `R² k=${ref.k}`); close(f.adjR2, ref.adj, 1e-9, `adj R² k=${ref.k}`);
    close(f.aic, ref.aic, 1e-7, `AIC k=${ref.k}`); close(f.bic, ref.bic, 1e-7, `BIC k=${ref.k}`);
    R.xt.forEach((x, i) => close(f.predict(x), ref.pred[i], 1e-6, `Ŷ(${x}) k=${ref.k}`));
  }
  assert.equal(Z.polyFit([1, 2, 3], [1, 4, 9], 3), null);   // stopień ≥ liczba punktów
});

test("miary modelu wielorakiego jak compute_model_metrics()", () => {
  const m = Z.lmMetrics([R.x, R.lm.b], R.y);
  close(m.r2, R.lm.r2, 1e-10, "R²"); close(m.adjR2, R.lm.adj, 1e-10, "adj R²");
  close(m.aic, R.lm.aic, 1e-8, "AIC"); close(m.bic, R.lm.bic, 1e-8, "BIC"); close(m.rmse, R.lm.rmse, 1e-10, "RMSE");
});

test("generatory danych: zakresy i zaokrąglenia jak w helpers.R", () => {
  const r = D.rng(707);
  const d = Z.multiData(400, r);
  assert.ok(d.ocena.every((v) => v >= 2 && v <= 5));
  assert.ok(d.godziny_nauki.every((v) => v >= 0 && v <= 40 && Math.abs(v * 10 - Math.round(v * 10)) < 1e-9));
  const tt = Z.trainTest(r);
  assert.equal(tt.train.x.length, 35); assert.equal(tt.test.x.length, 180);
  assert.ok(tt.train.x.every((v, i, a) => i === 0 || a[i - 1] <= v));   // posortowane
  close(Z.rmseOn(Z.polyFit(tt.test.x, tt.test.y, 6), tt.test.x, tt.test.y), 1, 0.15, "RMSE ≈ σ szumu");
});

test("regresja logistyczna jak glm(binomial) i confint()", () => {
  const L = R.logit, X = [L.z.map(() => 1), L.g, L.s];
  const f = Z.logitFit(X, L.z);
  L.est.forEach((b, j) => close(f.coef[j].estimate, b, 1e-7, `β${j}`));
  L.se.forEach((se, j) => close(f.coef[j].se, se, 1e-6, `SE${j}`));
  L.p.forEach((p, j) => close(f.coef[j].p, p, 1e-6, `p${j}`));
  close(f.aic, L.aic, 1e-6, "AIC"); close(f.bic, L.bic, 1e-6, "BIC");
  L.fitted.forEach((v, i) => close(f.fitted[i], v, 1e-8, `p̂${i}`));
  // confint() interpoluje profil splajnem: zgodność do ~1e-3.
  [1, 2].forEach((j) => {
    const ci = Z.logitProfileCI(X, L.z, f, j);
    close(ci[0], L.lo[j], 2e-3, `CI dół ${j}`); close(ci[1], L.hi[j], 2e-3, `CI góra ${j}`);
  });
});
