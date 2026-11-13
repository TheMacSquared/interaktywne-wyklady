// Wykład 04 — ryciny liczone w przeglądarce: Ryc. 3.1, Przykłady A–C, Ryc. 4.1, 4.3
// (sf_steps.js) oraz próby i tekst kroku Ryc. 3.2–3.3 (ci_construction.js).
const test = require("node:test");
const assert = require("node:assert/strict");
const { environment, Element, root } = require("./harness");

function load() {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("04-przedzialy-ufnosci/modules/ci_construction.js");
  env.load("04-przedzialy-ufnosci/modules/sf_steps.js");
  const F = env.window.lcSF.figures, P = env.window.lcCharts.palette();
  const run = (name, config) => {
    const fig = typeof F[name] === "function" ? { draw: F[name] } : F[name], state = { config };
    if (fig.init) fig.init(state);
    return { fig, state, spec: (k, step) => fig.draw(null, P, k || 0, state, {}, step) };
  };
  return { env, F, P, run };
}
const near = (a, b, eps = 1e-6) => assert.ok(Math.abs(a - b) < eps, `${a} ≠ ${b}`);
const types = spec => Array.from(spec.layers, l => l.type);
const texts = spec => Array.from(spec.layers.filter(l => l.type === "text"), l => l.text);
const plain = o => JSON.parse(JSON.stringify(o));  // obiekt z kontekstu vm

test("Ryc. 3.1: gęstości N(0, 1) i t(4), ogony poza ±t*, podziałki ±1.96 i ±2.78", () => {
  const { run } = load();
  const s = run("s04-tnorm").spec();
  assert.deepEqual(Array.from(s.xTicks), [-2.78, -1.96, 0, 1.96, 2.78]);
  assert.deepEqual([...s.x, ...s.y], [-4.5, 4.5, 0, 0.43]);
  assert.deepEqual(types(s), ["line", "line", "line", "line", "segment", "segment", "segment", "segment", "text", "text"]);
  // Ogony: same punkty |x| ≥ t* = 2.776445 (R: qt(0.975, 4)).
  assert.ok(s.layers[0].data.every(p => p[0] >= 2.776445));
  assert.ok(s.layers[1].data.every(p => p[0] <= -2.776445));
  assert.ok(s.layers[0].fill && s.layers[0].fillAlpha === 0.3);
  near(s.layers[6].to[1], 0.02558082 + 0.06);   // dt(t*, 4) + 0.06
  assert.deepEqual(texts(s), ["N(0, 1)", "t, df = 4"]);
});

test("Przykłady A–C: CI grup z pasem nakładania i CI różnicy z zerem", () => {
  const { run } = load();
  const A = { g1_name: "Dostawca A", g2_name: "Dostawca B", unit: "zawartość białka (%)", m1: 11.96, m2: 10.97,
              ci1: [11.76, 12.16], ci2: [10.80, 11.15], md: 0.99, ci_d: [0.72, 1.25] };
  let r = run("s04-comp", A), top = r.spec(0), bot = r.spec(1);
  assert.ok(!types(top).includes("band"), "rozłączne CI: bez pasa nakładania");
  assert.deepEqual(plain(top.yLabels), { 1: "Dostawca B", 2: "Dostawca A" });
  assert.deepEqual(texts(top), ["11.96", "10.97"]);
  near(top.x[0], 10.80 - 0.05 * 1.36); near(top.x[1], 12.16 + 0.05 * 1.36);
  assert.equal(top.xName, "zawartość białka (%)");
  // Dolny: oś obejmuje zero (linia odniesienia), CI różnicy w wierszu 1.
  near(bot.x[0], -0.05 * 1.25); near(bot.x[1], 1.25 * 1.05);
  assert.equal(bot.yLabels[1], "Dostawca A − Dostawca B");
  assert.equal(bot.xName, "różnica — zawartość białka (%)");
  assert.deepEqual(texts(bot), ["0", "0.99"]);
  const bar = bot.layers.find(l => l.type === "segs");
  assert.deepEqual(Array.from(bar.items[0].from), [0.72, 1]); assert.deepEqual(Array.from(bar.items[0].to), [1.25, 1]);
  // B: CI grup się nakrywają — żółty pas na odcinku wspólnym.
  r = run("s04-comp", { ...A, ci1: [3.03, 3.17], ci2: [3.11, 3.28], m1: 3.10, m2: 3.20, md: -0.10, ci_d: [-0.21, 0.01] });
  const band = r.spec(0).layers.find(l => l.type === "band");
  assert.deepEqual([band.lo, band.hi], [3.11, 3.17]);
  assert.equal(band.color, "#f1c40f");
});

test("Ryc. 4.1: słupki TAK/NIE, kroki p̂, ± SE, przedział i tekst jak w R", () => {
  const { run } = load();
  const r = run("s04-pstep");
  assert.ok(r.state.x >= 0 && r.state.x <= 50);
  r.state.x = 30;
  const bars = r.spec(0, 1);
  assert.deepEqual(Array.from(bars.layers[0].data, b => b[2]), [20, 30]);
  near(bars.y[1], 30 * 1.15);
  assert.deepEqual(plain(bars.xLabels), { 1: "NIE", 2: "TAK" });
  assert.equal(r.spec(1, 1).layers.length, 0, "krok 1: pusta oś proporcji");
  assert.deepEqual(texts(r.spec(1, 2)), ["p̂"]);
  assert.deepEqual(texts(r.spec(1, 3)), ["p̂", "± SE"]);
  const s4 = r.spec(1, 4), ci = s4.layers.filter(l => l.type === "segs").pop();
  // R: SE = 0.06928203, ME = 0.1357903.
  near(ci.items[0].from[0], 0.6 - 0.1357903); near(ci.items[0].to[0], 0.6 + 0.1357903);
  assert.deepEqual(texts(s4), ["p̂", "± SE", "95% CI"]);
  const P = load().P;
  assert.equal(s4.layers.find(l => l.type === "point").color, P.secondary, "p̂ już znane");
  assert.equal(r.spec(1, 2).layers.find(l => l.type === "point").color, P.accent, "p̂ nowe w kroku 2");
  assert.equal(r.fig.text(r.state, null, null, 1), "<p>50 odpowiedzi: 30 razy TAK, 20 razy NIE.</p>");
  assert.match(r.fig.text(r.state, null, null, 2), /= 30\/50 = 0\.6<\/p>/);
  assert.match(r.fig.text(r.state, null, null, 3), /√\(0\.6 · 0\.4\/50\) = 0\.069<\/p>/);
  assert.match(r.fig.text(r.state, null, null, 4), /= 0\.136<\/p><p>95% CI: <b>\[0\.464 ; 0\.736\]<\/b>/);
  r.fig.act(r.state, "new");
  assert.ok(Number.isInteger(r.state.x));
});

test("Ryc. 4.3: dwie grupy, p̂ obu grup, skala różnicy i werdykt jak w R", () => {
  const { run, P } = load();
  const r = run("s04-pdstep");
  r.state.x1 = 42; r.state.x2 = 30;
  const left = r.spec(0, 1);
  assert.deepEqual(Array.from(left.layers.find(l => l.type === "bins").data, b => b[2]), [18, 42, 30, 30]);
  assert.deepEqual(texts(left).slice(-2), ["Grupa 1", "Grupa 2"]);
  assert.equal(r.spec(1, 1).layers.length, 0);
  assert.deepEqual(texts(r.spec(1, 2)), ["p̂₁ = 0.7", "p̂₂ = 0.5"]);
  assert.deepEqual(texts(r.spec(2, 3)), ["p̂₁ − p̂₂ = 0.2"]);
  assert.deepEqual(texts(r.spec(2, 4)), ["± SE = ±0.088"]);
  const s5 = r.spec(2, 5);
  assert.deepEqual(texts(s5), ["95% CI: [0.028 ; 0.372]"]);
  // Oś różnicy jak w R: range(−0.5, 0.5, d ± 1.3·ME) z marginesem 8%.
  near(s5.x[0], -0.5 - 0.08); near(s5.x[1], 0.5 + 0.08);
  assert.equal(s5.layers.filter(l => l.type === "segs").pop().alpha, 0.6);
  assert.equal(s5.layers.find(l => l.type === "point").color, P.secondary);
  assert.match(r.fig.text(r.state, null, null, 1), /Grupa 1: 42 TAK i 18 NIE\. Grupa 2: 30 TAK i 30 NIE\./);
  assert.match(r.fig.text(r.state, null, null, 5), /\[0\.028 ; 0\.372\].*w grupie 1 odsetek TAK jest wyższy, z 95% ufnością o co najmniej 0\.028\./);
  r.state.x1 = 31;
  assert.match(r.fig.text(r.state, null, null, 5), /Przedział obejmuje 0/);
  r.state.x1 = 10;
  assert.match(r.fig.text(r.state, null, null, 5), /w grupie 2 odsetek TAK jest wyższy/);
});

test("Ryc. 3.2–3.3: statystyki jak ci_construction_stats() w R i tekst kroku", () => {
  const { env } = load();
  const K = env.window.lcCiConstruction;
  const x = [165.2, 171.4, 180.3, 158.9, 175, 169.1, 172.8, 162.4, 177.7, 168.3];
  const y = [160.1, 166.4, 158.2, 171.9, 163.5, 159.8, 167.2, 164.4];
  let s = K.stats(x);
  near(s.mean, 170.11); near(s.se, 2.12855976347); near(s.t, 2.26215716280, 1e-5); near(s.lower, 165.29486328462, 1e-4);
  s = K.stats(x, y);
  near(s.mean, 6.1725); near(s.se, 2.669275193667); near(s.df, 15.657278413783, 1e-6); near(s.upper, 11.841193866049, 1e-4);
  const m = K.stats(x);
  assert.match(K.stepText(m, 1, false), /^10 pomiarów wzrostu.*<i>x̄<\/i> = 170\.11 cm/);
  assert.match(K.stepText(m, 4, false), /<i>t<\/i>\* = 2\.262 \(df = 9\).*95% CI: \[165\.29 ; 174\.93\]\. Ten przedział zawiera μ = 170 cm\.$/);
  assert.match(K.stepText(s, 5, true), /df Welcha ≈ 15\.7.*95% CI: \[0\.5 ; 11\.84\] cm\. Przedział nie obejmuje 0: mężczyźni są średnio wyżsi o co najmniej 0\.5 cm\.$/);
  assert.match(K.stepText(s, 4, true), /mniej niż suma SE obu średnich/);
});

test("Ryc. 3.2: widget losuje nową próbę i pisze tekst kroku bez serwera", () => {
  const { env } = load();
  const bank = [{ x: [170, 172, 168], y: null, mean: 170, se: 1.1547, df: 2, t: 4.3027, lower: 165.03, upper: 174.97 }];
  const holder = new Element("div", { class: "ci-construction", "data-kind": "mean", "data-config": JSON.stringify(bank) },
                             [new Element("div", { class: "lc-ec" })]);
  const out = new Element("span", { id: "ch3_step_text", class: "shiny-html-output" });
  const btn = new Element("button", { id: "ch3_step_new_sample" });
  const host = root("lc-stepper", [btn, holder, out], { id: "ch3_step", "data-lc-step": "1" });
  host.id = "ch3_step";  // atrapa DOM nie ma własności id
  env.mount(host);
  assert.equal(out.classList.contains("shiny-html-output"), false, "tekst kroku nie jest wyjściem Shiny");
  assert.match(out.innerHTML, /^3 pomiarów wzrostu/);
  host.setAttribute("data-lc-step", "4");
  env.observers.filter(o => o.options.attributes).forEach(o => o.fn([{ target: host }]));
  assert.match(out.innerHTML, /^<i>t<\/i>\* = /);
  btn.emit("click");
  assert.match(out.innerHTML, /\(df = 24\)/, "nowa próba: 25 pomiarów");
  assert.deepEqual(env.errors, []);
});
