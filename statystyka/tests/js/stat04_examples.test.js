// Przykłady z hipotezą wykładu 04 (rozdz. 3 i 4) liczone w przeglądarce: rycina s4-case (sf_examples.js).
const test = require("node:test");
const assert = require("node:assert/strict");
const { environment, Element, root } = require("./harness");

function load() {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("04-przedzialy-ufnosci/modules/sf_examples.js");
  const fig = env.window.lcSF.figures["s4-case"], P = env.window.lcCharts.palette();
  const run = config => {
    const state = { config };
    fig.init(state);
    return { state, spec: (k, step) => fig.draw(null, P, k, state, {}, step), key: () => fig.noteKey({}, state) };
  };
  return { env, fig, run };
}
const types = spec => Array.from(spec.layers, l => l.type);
const texts = spec => Array.from(spec.layers.filter(l => l.type === "text"), l => l.text);
const lims = (raw, x) => ({ raw, x });

// Wartości jak z ch3_case_config(ch3_cases$A1) (próba skrócona do 3 punktów).
const A1 = {
  type: "single_mean", n: 4, xlab: "Wzrost (cm)", text: "Przedział gotowy.",
  hyps: [{ bound: 168, dir: "gt", label: "≥ 168" }, { bound: 180, dir: "gt", label: "≥ 180" }],
  xbar: 173.4, se: 1.4971, lo: 170.338069678584, hi: 176.461930321416,
  pts: [[181.9, 0.55], [169.3, 0.31], [160, 0.4]], y: [-0.615, 0.815],
  lims: { 0: lims([154, 189.5], [152.2, 191.3]), 1: lims([154, 189.5], [152.2, 191.3]), 2: lims([154, 189.5], [152.2, 191.3]) },
  lab_mean: "x̄ = 173.4", lab_se: "± SE = ±1.5", lab_ci: "95% CI: [170.34 ; 176.46]"
};

test("Przykład A1 (średnia): kroki budują przedział, hipoteza tylko na ostatnim kroku", () => {
  const { fig, run } = load();
  const r = run(A1);
  let s = r.spec(0, 1);
  assert.deepEqual(types(s), ["points"]);
  assert.deepEqual(Array.from(s.x), [152.2, 191.3]);
  assert.equal(s.xName, "Wzrost (cm)");
  s = r.spec(0, 2);
  assert.deepEqual(texts(s), ["x̄ = 173.4"]);
  assert.equal(s.layers.find(l => l.type === "vline").dash, "dotted");
  s = r.spec(0, 3);
  assert.deepEqual(texts(s), ["± SE = ±1.5"], "napis średniej znika, SE z napisem");
  const se = s.layers.find(l => l.type === "segs");
  assert.deepEqual(Array.from(se.items[0].from), [173.4 - 1.4971, 0]);
  s = r.spec(0, 4);
  assert.deepEqual(texts(s), ["95% CI: [170.34 ; 176.46]"]);
  const ci = s.layers.filter(l => l.type === "segs")[1];
  assert.deepEqual([ci.items[0].from[0], ci.items[0].to[0]], [A1.lo, A1.hi], "granice CI z R");
  assert.equal(fig.text(r.state, null, {}, 4), "Przedział gotowy.");
  assert.equal(fig.text(r.state, null, {}, 3), "");

  // Hipoteza: przełącznik, obszar od granicy w prawo, werdykt po kliknięciu.
  assert.equal(r.key(), "");
  fig.act(r.state, "hyp1");
  s = r.spec(0, 4);
  const band = s.layers.find(l => l.type === "band");
  assert.deepEqual([band.lo, band.hi], [168, 191.3]);
  assert.ok(texts(s).includes("≥ 168"));
  assert.equal(r.key(), "h1");
  fig.act(r.state, "reveal");
  assert.equal(r.key(), "h1r");
  // Inny krok: hipoteza nie działa (obszar i opis znikają), wybór zostaje.
  s = r.spec(0, 3);
  assert.ok(!types(s).includes("band"));
  assert.equal(r.key(), "");
  r.spec(0, 4);
  assert.equal(r.key(), "h1r");
  // Zmiana hipotezy chowa werdykt; ponowne kliknięcie wyłącza hipotezę.
  fig.act(r.state, "hyp2");
  assert.equal(r.key(), "h2");
  fig.act(r.state, "hyp2");
  assert.equal(r.key(), "");
  assert.ok(!types(r.spec(0, 4)).includes("band"));
});

test("Przykład B (różnica średnich): dolny wykres od kroku 3, linia zera bez hipotezy, obszar „<”", () => {
  const { fig, run } = load();
  const cfg = {
    type: "diff_means", n: 5, xlab: "Czas reakcji (ms)", text: "x",
    hyps: [{ bound: 0, dir: "lt", label: "≤ 0" }, { bound: -25, dir: "lt", label: "≤ -25" }],
    x1: 350, x2: 362, label1: "Grupa A", label2: "Grupa B",
    pts1: [[340, 1.8]], pts2: [[370, 1.0]], top: lims([211, 463], [199, 476]), y: [0.255, 2.345],
    diff: -12, se: 5.196, lo: -22.2258, hi: -1.7742, y_bot: [-0.61, 0.71],
    lims: { 0: lims([-28, 4], [-29.5, 5.5]), 1: lims([-28, 4], [-29.5, 5.5]), 2: lims([-28, 4], [-29.5, 5.5]) },
    xlab_bot: "Różnica (ms)  —  A − B", lab_x1: "x̄₁ = 350", lab_x2: "x̄₂ = 362",
    lab_diff: "x̄₁ − x̄₂ = -12", lab_se: "± SE = ±5.2", lab_ci: "95% CI: [-22.23 ; -1.77]"
  };
  const r = run(cfg);
  assert.equal(r.spec(1, 2), null, "różnica ukryta przed krokiem 3");
  assert.deepEqual(texts(r.spec(0, 1)), ["Grupa A", "Grupa B"]);
  assert.deepEqual(texts(r.spec(0, 2)), ["Grupa A", "Grupa B", "x̄₁ = 350", "x̄₂ = 362"]);
  let s = r.spec(1, 3);
  assert.equal(s.xName, "Różnica (ms)  —  A − B");
  assert.equal(s.layers[0].type, "vline"); assert.equal(s.layers[0].x, 0);
  assert.deepEqual(texts(s), ["x̄₁ − x̄₂ = -12"]);
  fig.act(r.state, "hyp1");
  s = r.spec(1, 5);
  const band = s.layers.find(l => l.type === "band");
  assert.deepEqual([band.lo, band.hi], [-29.5, 0], "obszar „< 0” od lewej krawędzi do granicy");
  assert.ok(!s.layers.some(l => l.type === "vline" && l.dash === "dashed"), "z hipotezą bez linii zera");
  assert.deepEqual(texts(s), ["≤ 0", "95% CI: [-22.23 ; -1.77]"]);
});

test("Przykład A2 (trzy n): krok k pokazuje k przedziałów z etykietami", () => {
  const { run } = load();
  const cfg = {
    type: "compare_n", n: 3, xlab: "Stężenie", text: "x", hyps: [{ bound: 25, dir: "gt", label: "≥ 25" }],
    xbar: 32, lo: [26.28, 29.73, 30.88], hi: [37.72, 34.27, 33.12], y: [0.13, 3.87],
    lims: { 0: lims([25.1, 38.9], [24.4, 39.6]), 1: lims([23.7, 39], [23, 39.8]) },
    lab_n: ["n = 10", "n = 50", "n = 200"], lab_ci: ["[26.28 ; 37.72]", "[29.73 ; 34.27]", "[30.88 ; 33.12]"]
  };
  const r = run(cfg);
  assert.deepEqual(texts(r.spec(0, 1)), ["n = 10", "[26.28 ; 37.72]"]);
  const s = r.spec(0, 3);
  assert.equal(texts(s).length, 6);
  assert.equal(s.layers.find(l => l.type === "segs").items.length, 9, "trzy odcinki z wąsami");
  assert.equal(s.layers.find(l => l.type === "text" && l.text === "n = 200").x, 25.1, "etykieta n przy lewej krawędzi z R");
});

test("Przykład C (forest): hipoteza pairwise bez obszaru, werdykt z R", () => {
  const { fig, run } = load();
  const cfg = {
    type: "forest", n: 3, xlab: "Wynik", text: "x", hyps: [{ pairwise: true }],
    groups: ["A", "B"], means: [28, 33], lo: [26, 31], hi: [30, 35], pts: [[27, 1.1], [34, 2]], y: [0.08, 2.92],
    lims: { 0: lims([25, 36], [24.5, 36.5]), 1: lims([25, 36], [24.5, 36.5]) }
  };
  const r = run(cfg);
  assert.deepEqual(types(r.spec(0, 1)), ["text", "text", "points"]);
  fig.act(r.state, "hyp1");
  const s = r.spec(0, 3);
  assert.ok(!types(s).includes("band"));
  assert.deepEqual(types(s).slice(-2), ["points", "segs"]);
  assert.equal(r.key(), "h1");
});

test("Przykłady proporcji: słupki TAK/NIE, dwie p̂, różnica z linią zera i obszarem", () => {
  const { fig, run } = load();
  const A = run({
    type: "single_prop", n: 4, xlab: "Poparcie", text: "x",
    hyps: [{ bound: 0.5, dir: "gt", label: "≥ 0.5" }, { bound: 0.6, dir: "gt", label: "≥ 0.6" }],
    counts: [188, 212], phat: 0.53, se: 0.025, lo: 0.4811, hi: 0.5789, y: [-0.66, 0.66],
    lims: { 0: lims([0.457, 0.624], [0.449, 0.632]), 1: lims([0.457, 0.624], [0.449, 0.632]), 2: lims([0.457, 0.624], [0.449, 0.632]) },
    lab_phat: "p̂ = 0.53", lab_se: "± SE = ±0.025", lab_ci: "95% CI: [0.481 ; 0.579]"
  });
  let s = A.spec(0, 1);
  assert.deepEqual(Array.from(s.layers[0].data, b => b[2]), [188, 212]);
  assert.equal(s.xLabels[2], "TAK");
  assert.deepEqual(texts(s), ["188", "212"]);
  assert.deepEqual(types(A.spec(1, 1)), [], "oś proporcji pusta na kroku 1");
  assert.deepEqual(texts(A.spec(1, 3)), ["± SE = ±0.025"]);
  assert.equal(A.spec(1, 4).layers.filter(l => l.type === "segs").length, 1, "w kroku 4 przedział zastępuje SE");

  const B = run({
    type: "diff_props", n: 5, xlab: "Odsetek", text: "x",
    hyps: [{ bound: 0, dir: "gt", label: "≥ 0" }, { bound: 0.25, dir: "gt", label: "≥ 0.25" }],
    label1: "Lek", label2: "Placebo", counts: [76, 124, 116, 84], p1: 0.62, p2: 0.42, diff: 0.2, se: 0.049,
    lo: 0.104, hi: 0.296, y_top: [0.29, 2.71], y_bot: [-0.605, 0.605],
    lims: { 0: lims([-0.35, 0.37], [-0.39, 0.41]), 1: lims([-0.35, 0.37], [-0.39, 0.41]), 2: lims([-0.35, 0.37], [-0.39, 0.41]) },
    xlab_bot: "Różnica proporcji  —  Lek − Placebo", lab_p1: "p̂₁ = 0.62", lab_p2: "p̂₂ = 0.42",
    lab_diff: "p̂₁ − p̂₂ = 0.2", lab_se: "± SE = ±0.049", lab_ci: "95% CI: [0.104 ; 0.296]"
  });
  s = B.spec(0, 1);
  assert.equal(s.layers[0].data.length, 4);
  assert.equal(s.xLabels[1], "Lek");
  assert.deepEqual(types(B.spec(1, 1)), []);
  assert.deepEqual(texts(B.spec(1, 2)), ["p̂₁ = 0.62", "p̂₂ = 0.42"]);
  assert.equal(B.spec(1, 2).yLabels[2], "Placebo");
  s = B.spec(2, 1);
  assert.deepEqual(types(s), ["vline"], "linia zera od początku");
  fig.act(B.state, "hyp2");
  s = B.spec(2, 5);
  assert.ok(texts(s).includes("≥ 0.25"));
  assert.ok(texts(s).includes("95% CI: [0.104 ; 0.296]"));
  assert.equal(B.key(), "h2");

  const C = run({
    type: "forest_prop", n: 3, xlab: "Powikłania", text: "x", hyps: [{ pairwise: true }],
    phat: [0.08, 0.2], lo: [0.04, 0.15], hi: [0.12, 0.27], y: [0.08, 2.92],
    lims: { 0: lims([0, 0.39], [-0.02, 0.41]), 1: lims([0, 0.39], [-0.02, 0.41]) },
    lab_groups: ["Szpital A · 12/150", "Szpital D · 35/170"]
  });
  s = C.spec(0, 1);
  assert.deepEqual(texts(s), ["Szpital A · 12/150", "Szpital D · 35/170"]);
  assert.equal(s.layers[0].x, 0.39); assert.equal(s.layers[0].align, "right");
  assert.deepEqual(types(C.spec(0, 3)).slice(-2), ["points", "segs"]);
});

test("Rycina w DOM: przełącznik i „Pokaż werdykt” działają bez serwera", () => {
  const { env } = load();
  const stepper = new Element("div", { class: "lc-stepper", id: "ch3_caseA1", "data-lc-step": "4" });
  const out = new Element("span", { id: "ch3_caseA1_text" });
  const chip = new Element("button", { class: "lc-chip", "data-stat-act": "hyp1", "data-value": "1" });
  const reveal = new Element("button", { "data-stat-act": "reveal" });
  const notes = ["h1", "h1r", "h2", "h2r"].map(k => new Element("div", { "data-when": k, hidden: "" }));
  notes.forEach(n => Object.defineProperty(n, "hidden", { value: true, writable: true }));
  stepper.appendChild(chip); stepper.appendChild(out);
  const fig = root("lc-sf", [stepper, new Element("div", { class: "lc-ec", "data-chart": "0" }), ...notes, reveal],
    { "data-fig": "s4-case", "data-config": JSON.stringify(A1) });
  env.mount(fig);
  assert.equal(env.errors.length, 0);
  assert.ok(notes.every(n => n.hidden));
  fig.emit("click", { target: chip });
  assert.equal(notes[0].hidden, false, "opis hipotezy 1 przed werdyktem");
  fig.emit("click", { target: reveal });
  assert.equal(notes[0].hidden, true);
  assert.equal(notes[1].hidden, false, "werdykt po kliknięciu");
  assert.equal(out.innerHTML, "Przedział gotowy.");
});
