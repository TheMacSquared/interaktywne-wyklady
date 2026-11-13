// Ryciny wykładu 04, rozdziały 1–2, liczone w przeglądarce (sf_ch12.js):
// Ryc. 1.1 (tarcza), 1.2 (cztery miarki), 1.3 (stos średnich), 1.4 (dwie pizzerie), 2.3 (quiz).
const test = require("node:test");
const assert = require("node:assert/strict");
const { environment, Element, root, ecbox } = require("./harness");

function load() {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("04-przedzialy-ufnosci/modules/sf_ch12.js");
  const F = env.window.lcSF.figures, P = env.window.lcCharts.palette(), X = env.window.lc04Ch12;
  const run = (name, config) => {
    const fig = F[name], state = { config };
    if (typeof fig !== "function" && fig.init) fig.init(state);
    const draw = typeof fig === "function" ? fig : fig.draw;
    return { fig, state, spec: k => { if (fig.prepare) fig.prepare(state, null, {}); return draw(null, P, k || 0, state, {}, null); } };
  };
  return { env, F, P, X, run };
}
const types = spec => Array.from(spec.layers, l => l.type);
const mean = v => v.reduce((a, b) => a + b, 0) / v.length;
const MIARKI_CFG = { mu: 170, sigma: 10, n: 25 };
const PIZZA_CFG = { init: 4.4, prob: [0.03, 0.04, 0.08, 0.22, 0.63],
  rows: [{ who: "U Gośki: 5 opinii dziennie", n: 5, y: 2 }, { who: "Napoli: 200 opinii dziennie", n: 200, y: 1 }] };

test("pomocniki: sd() i median() jak w R", () => {
  const { X } = load();
  assert.ok(Math.abs(X.sd([1, 2, 3, 4, 10]) - 3.535534) < 1e-6);
  assert.ok(Number.isNaN(X.sd([4.4])));
  assert.equal(X.median([5, 1, 3]), 3);
  assert.equal(X.median([4, 1, 3, 2]), 2.5);
});

test("Ryc. 1.2: pusta rycina, +1000 grupek, histogramy na wspólnej osi i tabela", () => {
  const { run, P } = load();
  const r = run("d04-miarki", MIARKI_CFG);
  let s = r.spec(0);
  assert.deepEqual(types(s), ["bins", "vline"]);
  assert.deepEqual(Array.from(s.x), [155, 185]);
  assert.equal(s.y[1], 1.05, "pusta rycina: oś 0–1 jak w R");
  assert.equal(s.layers[1].x, 170);
  assert.equal(r.fig.table(r.state).match(/–/g).length, 8, "bez grupek same kreski");
  r.fig.act(r.state, "add1000");
  r.fig.act(r.state, "add1");
  assert.equal(r.state.est[0].length, 1001);
  s = r.spec(0);
  const top = Math.max(...r.state.bins.flat().map(b => b.n));
  assert.ok(Math.abs(s.y[1] - 1.05 * top) < 1e-9, "wspólna oś y czterech paneli");
  assert.equal(s.layers[0].data.reduce((a, b) => a + b[2], 0), 1001);
  assert.ok(s.layers[0].data.every(b => Math.abs(b[1] - b[0] - 0.5) < 1e-9 && Math.abs(b[0] / 0.5 - Math.round(b[0] / 0.5)) < 1e-9),
    "binwidth = 0.5, boundary = 0");
  assert.equal(s.layers[0].color, P.bursztyn);
  assert.equal(r.spec(3).layers[0].color, P.terakota, "uszkodzona miarka w kolorze chybienia");
  // Statystycznie jak w R: E ≈ 170 (uszkodzona 172), SD średniej 2, mediany ≈ 2.5, środka rozstępu ≈ 3.5.
  const [m, md, mr, b] = r.state.est;
  assert.ok(Math.abs(mean(m) - 170) < 0.3 && Math.abs(mean(md) - 170) < 0.4 && Math.abs(mean(mr) - 170) < 0.5);
  assert.ok(Math.abs(mean(b) - mean(m) - 2) < 1e-9);
  const X = load().X;
  assert.ok(Math.abs(X.sd(m) - 2) < 0.2 && X.sd(md) > X.sd(m) && X.sd(mr) > X.sd(md));
  const rows = r.fig.table(r.state);
  assert.match(rows, /Uszkodzona miarka/);
  assert.match(rows, /17[01]\.\d</);
  r.fig.act(r.state, "reset");
  assert.equal(r.state.est[0].length, 0);
});

test("Ryc. 1.1: cztery tarcze z config, krzyżyk tylko w obciążonych", () => {
  const { run } = load();
  const shots = [[0.1, 0.2], [-0.3, 0.4]];
  const cfg = { labels: ["a", "b", "c", "d"], panels: [{ shots }, { shots }, { shots, mean: [1.6, 0.1] }, { shots, mean: [1.4, 0] }] };
  const r = run("d04-tarcza", cfg);
  let s = r.spec(0);
  assert.deepEqual(types(s), ["line", "line", "line", "line", "rect", "point", "points"]);
  assert.deepEqual(Array.from(s.x), [-4.2, 4.2]);
  const ring = s.layers[3].data;
  assert.equal(ring.length, 181);
  assert.ok(ring.every(p => Math.abs(Math.hypot(p[0], p[1]) - 4) < 1e-9), "okrąg o promieniu 4");
  s = r.spec(2);
  assert.equal(types(s).at(-1), "segs");
  assert.ok(s.layers.at(-1).items.every(it => Math.abs((it.from[0] + it.to[0]) / 2 - 1.6) < 1e-9));
});

test("Ryc. 1.3: słupki gęstości z R, klamra ±2·SD, nazwa osi x tylko pod ostatnim", () => {
  const { run } = load();
  const cfg = { mu: 170, panels: [{ n: 25, sd: 2, bins: [[169.8, 170, 0.2]] }, { n: 100, sd: 1, bins: [] }, { n: 400, sd: 0.5, bins: [] }] };
  const r = run("d04-stos", cfg);
  const s = r.spec(0);
  assert.deepEqual(types(s), ["bins", "vline", "segs"]);
  assert.deepEqual(Array.from(s.layers[2].items[0].from), [166, -0.06]);
  assert.deepEqual(Array.from(s.layers[2].items[0].to), [174, -0.06]);
  assert.equal(s.xName, "");
  assert.equal(r.spec(2).xName, "Średnia z próby x̄ (cm)");
  assert.deepEqual(Array.from(r.spec(2).layers[2].items[0].from), [169, -0.06]);
});

test("Ryc. 1.4: kolejne dni, SD średnich jak sd(c(4.4, …)) w R, rozrzut ≈ σ/√n", () => {
  const { run, X } = load();
  const r = run("d04-pizzerie", PIZZA_CFG);
  let s = r.spec(0);
  assert.deepEqual(types(s), ["segment", "segment", "point", "text", "point", "text"]);
  assert.equal(s.yLabels[2], "U Gośki: 5 opinii dziennie");
  assert.equal(s.layers[3].text, "4.4 ★");
  assert.equal(r.fig.noteKey({}, r.state), "empty");
  assert.ok(Number.isNaN(X.pizzaSd(r.state, 0)));
  r.state.days[0].push([5, 2]);
  assert.ok(Math.abs(X.pizzaSd(r.state, 0) - 0.4242641) < 1e-6, "sd(c(4.4, 5))");
  r.fig.act(r.state, "reset");
  for (let i = 0; i < 100; i++) r.fig.act(r.state, "rep20");
  r.fig.act(r.state, "rep1");
  assert.equal(r.state.days[0].length, 2001);
  assert.equal(r.fig.noteKey({}, r.state), "reps");
  s = r.spec(0);
  assert.equal(types(s)[2], "points");
  assert.equal(s.layers[2].data.length, 4002);
  // Gwiazdki: średnio 4.38, SD 0.998 → SD średniej z 5 opinii ≈ 0.446, z 200 ≈ 0.071.
  assert.ok(Math.abs(X.pizzaSd(r.state, 0) - 0.446) < 0.03);
  assert.ok(Math.abs(X.pizzaSd(r.state, 1) - 0.0706) < 0.006);
  assert.ok(r.state.days[1].every(d => Math.abs(d[1] - 1) <= 0.22), "rozrzut pionowy ±0.22");
});

test("Ryc. 1.2 i 1.4 w silniku: przyciski zmieniają wykres, odczyty i podpis", () => {
  const { env } = load();
  const act = name => new Element("button", { "data-stat-act": name });
  const vals = keys => keys.map(k => new Element("span", { "data-v": k }));
  const btns = [act("add1000"), act("reset")], table = new Element("div", { "data-role": "table" });
  const miarki = root("lc-sf", [...btns, ...vals(["groups"]), ...[0, 1, 2, 3].map(k => ecbox("p", { "data-chart": k })), table],
    { "data-fig": "d04-miarki", "data-config": JSON.stringify(MIARKI_CFG) });
  env.mount(miarki);
  assert.equal(miarki.querySelector('[data-v="groups"]').textContent, "0");
  miarki.emit("click", { target: btns[0] });
  assert.equal(miarki.querySelector('[data-v="groups"]').textContent, "1 000");
  assert.match(table.innerHTML, /Środek rozstępu/);
  const pBtns = [act("rep20")], notes = ["empty", "reps"].map(w => new Element("div", { "data-when": w }));
  const pizza = root("lc-sf", [...pBtns, ...vals(["days", "sd1", "sd2"]), ecbox("p", { "data-chart": 0 }), ...notes],
    { "data-fig": "d04-pizzerie", "data-config": JSON.stringify(PIZZA_CFG) });
  env.mount(pizza);
  assert.equal(pizza.querySelector('[data-v="sd1"]').textContent, "–");
  assert.deepEqual(notes.map(n => n.hidden), [false, true]);
  pizza.emit("click", { target: pBtns[0] });
  assert.equal(pizza.querySelector('[data-v="days"]').textContent, "20");
  assert.match(pizza.querySelector('[data-v="sd1"]').textContent, /^\d\.\d\d ★$/);
  assert.deepEqual(notes.map(n => n.hidden), [true, false]);
  assert.deepEqual(env.errors, []);
});

test("Ryc. 2.3: quiz bez serwera — jedna odpowiedź, kafelki znikają, komentarz do litery", () => {
  const { env } = load();
  const tiles = ["A", "B", "C", "D"].map(v => new Element("button", { "data-quiz-choice": v }));
  const grid = new Element("div", { class: "quiz-tiles" }, tiles);
  const fb = ["A", "B", "C", "D"].map(v => { const n = new Element("div", { "data-quiz-feedback": v }); n.hidden = true; return n; });
  const box = root("c04-quiz", [grid, ...fb], { "data-correct": "C" });
  env.mount(box);
  env.document.emit("click", { target: tiles[0] });
  assert.equal(box.getAttribute("data-answered"), "wrong");
  assert.equal(grid.hidden, true);
  assert.deepEqual(fb.map(n => n.hidden), [false, true, true, true]);
  env.document.emit("click", { target: tiles[2] });
  assert.equal(box.getAttribute("data-answered"), "wrong", "druga odpowiedź nie zmienia wyniku");
  assert.deepEqual(fb.map(n => n.hidden), [false, true, true, true]);
});
