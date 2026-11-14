// Silnik scen wykładu 10 (modules/sceny.js) bez przeglądarki i bez serwera R.
// Uruchamianie: node --test analiza-ryzyka/tests/js/i10_sceny.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../10-model-do-decyzji/modules/sceny.js"));
const Z = globalThis.lcI10, U = Z.util;

const near = (a, b, tol) => assert.ok(Math.abs(a - b) <= tol, `${a} ≉ ${b}`);

// Parametry jak i10p_cfg w stop_proto_common.R (bananpol$integration, bananpol$lifetime).
const cfg = { time: 1000, base_time: 1000, mttf: 1500, beta: 2, eta: 1700, power_r: 0.98,
  controller_r: 0.95, init: 0.005, sens: 0.95, efficacy: 0.5 };

test("mission zgadza się z risk_mission_analysis() w R", () => {
  const b = U.mission(cfg);
  near(b.fan_r, 0.5134171, 1e-6);
  near(b.parallel_r, 0.7632371, 1e-6);
  near(b.system_r, 0.7105737, 1e-6);
  near(b.top, 0.001624775, 1e-8);
  const w = U.mission(cfg, { model: "weibull" });
  near(w.fan_r, 0.7074978, 1e-6);
  near(w.top, 0.000956, 1e-6);
  const t2 = U.mission(cfg, { time: 2000 });
  near(t2.power_r, 0.9604, 1e-6);
  near(t2.controller_r, 0.9025, 1e-6);
  near(t2.system_r, 0.397, 1e-3);
  near(U.mission(cfg, { sens: 1 }).top, 0.001447131, 1e-8);
});

test("interwencje i scenariusze jak w przykładach 10.10 i 10.11", () => {
  const top = (id, m, model) => U.mission(cfg, { intervention: id, stress: m, model: model }).top;
  near(top("detector", 1), 0.001536, 1e-6);
  near(top("prevention", 1), 0.000812, 1e-6);
  near(top("power", 1), 0.001557, 1e-6);
  near(top("fan", 1), 0.001087, 1e-6);
  near(top("detector", 1.2), 0.002301, 1e-6);
  near(top("prevention", 0.8), 0.000397, 1e-6);
  near(top("prevention", 1.2), 0.001436, 1e-6);
  // Odwrócenie rankingu: Weibull, u = 0.5, scenariusz ostrożny.
  near(top("prevention", 1.5, "weibull"), 0.001717, 1e-6);
  near(top("fan", 1.5, "weibull"), 0.001682, 1e-6);
  assert.ok(top("prevention", 1.4, "weibull") < top("fan", 1.4, "weibull"));
});

test("fanR, zeroUpper i bayes", () => {
  near(U.fanR(cfg, 3000, "exp"), 0.1353353, 1e-6);
  near(U.fanR(cfg, 3000, "weibull"), 0.04441461, 1e-7);
  near(U.zeroUpper(100), 0.02951305, 1e-7);
  near(U.zeroUpper(1000), 0.00299125, 1e-7);
  near(U.bayes(0.005, 0.95, 0.05), 0.08715596, 1e-7);
  assert.ok(Number.isNaN(U.bayes(0, 0.95, 0)));
});

test("fmtP jak risk_fmt_p() i naturalna częstość", () => {
  assert.equal(U.fmtP(0.5134171), "0.513");
  assert.equal(U.fmtP(0.001624775), "0.00162");
  assert.equal(U.fmtP(0.05), "0.050");
  assert.equal(U.per10000(0.001624775), "16 na 10 000");
  assert.equal(U.per10000(0.00025), "2.5 na 10 000");
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
