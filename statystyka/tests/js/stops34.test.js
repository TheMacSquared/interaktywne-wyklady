// Punkty stop wykładu 03 liczone w przeglądarce: Ryc. 3.6, 3.9, 4.10, 5.13 (stops34_charts.js).
const test = require("node:test");
const assert = require("node:assert/strict");
const { environment, Element, root } = require("./harness");

function load() {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("03-rozklady-prawdopodobienstwa/modules/sim_charts.js");
  env.load("03-rozklady-prawdopodobienstwa/modules/stops34_charts.js");
  const F = env.window.lcSF.figures, P = env.window.lcCharts.palette();
  const run = (name, config, vals, v) => {
    const fig = F[name], state = { config };
    if (fig.init) fig.init(state);
    if (fig.prepare) fig.prepare(state, v, vals || {});
    return { fig, state, spec: (k, step) => fig.draw(v, P, k || 0, state, vals || {}, step) };
  };
  return { env, F, P, run };
}
const types = spec => Array.from(spec.layers, l => l.type);
const values = keys => keys.map(k => new Element("span", { "data-v": k }));

test("Ryc. 3.6 (V-1): mapa z ziarnem, siatka z kolorami, słupki i kółka oczekiwanych", () => {
  const { run } = load();
  const cfg = { obs: [229, 211, 93, 35, 7, 1], exp: [226.7, 211.4, 98.5, 30.6, 7.1, 1.6], texts: ["a", "b", "c", "d"] };
  const r = run("s3p-v1", cfg), r2 = run("s3p-v1", cfg);
  assert.equal(r.state.hits.length, 537);
  assert.deepEqual(Array.from(r.state.hits[0]), Array.from(r2.state.hits[0]), "stan startowy z ziarnem 1944");
  assert.ok(r.state.hits.every(h => h[0] >= 0 && h[0] < 24 && h[1] >= 0 && h[1] < 24));
  let s = r.spec(0, 1);
  assert.deepEqual(types(s), ["points"]);
  assert.deepEqual([s.x, s.y].map(a => Array.from(a)), [[0, 24], [0, 24]]);
  assert.equal(s.legend, null);
  s = r.spec(0, 2);
  assert.deepEqual(types(s), ["cells", "segs", "points"]);
  assert.equal(s.legend.title, "Trafienia"); assert.equal(s.legend.to, "4+");
  // Komórki z trafieniami: tyle, ile niepustych kwadratów; suma trafień 537.
  const cells = new Set(r.state.hits.map(h => Math.floor(h[0]) + "," + Math.floor(h[1])));
  assert.equal(s.layers[0].items.length, cells.size);
  const first = r.state.hits;
  r.fig.act(r.state, "shuffle");
  assert.notEqual(r.state.hits, first, "przycisk rozrzuca od nowa");
  s = r.spec(0, 3);
  assert.deepEqual(Array.from(s.layers[0].data, b => b[2]), cfg.obs);
  assert.equal(s.xLabels[5], "5+");
  assert.equal(types(s).filter(t => t === "text").length, 6);
  s = r.spec(0, 4);
  assert.equal(types(s).filter(t => t === "points").length, 2, "pierścień z dwóch kółek");
  const note = values(["v1_note"]);
  r.fig.update(root("lc-sf", note), r.state);
  assert.match(note[0].textContent, /^Słupki: Londyn 1944 · kółka/);
  r.spec(0, 3); r.fig.update(root("lc-sf", note), r.state);
  assert.equal(note[0].textContent, "");
  r.spec(0, 1); r.fig.update(root("lc-sf", note), r.state);
  assert.match(note[0].textContent, /^Południowy Londyn, 1944/);
  assert.equal(r.fig.text(r.state, null, null, 2), "b");
});

test("Ryc. 3.9 (Bortkiewicz): słupki, średnia z wariancją, kółka Pois", () => {
  const { run } = load();
  const cfg = { k: [0, 1, 2, 3, 4], obs: [109, 65, 22, 3, 1], mean: 0.61, exp: [108.67, 66.29, 20.22, 4.11, 0.63],
                label: "średnia 0.61, wariancja 0.61", texts: ["x", "y", "z"] };
  const r = run("s3p-horses", cfg);
  assert.deepEqual(types(r.spec(0, 1)), ["bins", "text", "text", "text", "text", "text"]);
  const s2 = r.spec(0, 2);
  const vl = s2.layers.find(l => l.type === "vline");
  assert.equal(vl.x, 0.61); assert.ok(vl.dash);
  assert.ok(s2.layers.some(l => l.type === "text" && l.text === cfg.label));
  const s3 = r.spec(0, 3);
  const labels = s3.layers.filter(l => l.type === "text" && l.size).map(l => l.text);
  assert.deepEqual(Array.from(labels), ["108.7", "66.3", "20.2", "4.1", "0.6"]);
  assert.equal(r.fig.text(r.state, null, null, 3), "z");
});

test("Ryc. 4.10 (autobus): odjazdy, czekanie, histogram, odczyty i zmiana trybu", () => {
  const { run } = load();
  const r = run("s3p-bus", null, {}, "regular");
  assert.equal(r.state.buses.length, 400);
  assert.equal(r.state.buses[399], 4000);
  assert.match(r.spec(1).aria, /Wpuść pasażerów/);
  assert.deepEqual(Array.from(r.spec(0).x), [0, 120]);
  r.fig.act(r.state, "p1000");
  assert.equal(r.state.pax.length, 1000);
  assert.ok(r.state.pax.every(p => p.wait > 0 && p.wait <= 10 && p.gap === 10));
  const out = values(["bus_n", "bus_wait", "bus_gap"]);
  r.fig.update(root("lc-sf", out), r.state);
  assert.equal(out[0].textContent, "1000");
  assert.ok(Math.abs(parseFloat(out[1].textContent) - 5) < 0.5, out[1].textContent);
  assert.equal(out[2].textContent, "10 min", "lc_fmt bez końcowych zer");
  const h = r.spec(1);
  assert.deepEqual(types(h), ["bins", "vline", "text"]);
  assert.equal(h.layers[0].data.reduce((s, b) => s + b[2], 0), 1000);
  const line = r.spec(0);
  assert.ok(types(line).includes("arrow"));
  const last = r.state.pax[999];
  assert.ok(line.x[0] <= last.t && last.t <= line.x[1]);
  // Zmiana trybu: nowe odjazdy, pasażerowie od zera; średnio czeka ok. 10 min.
  r.fig.prepare(r.state, "random", {});
  assert.equal(r.state.pax.length, 0);
  assert.notEqual(r.state.buses[399], 4000);
  for (let i = 0; i < 5; i++) r.fig.act(r.state, "p1000");
  r.fig.update(root("lc-sf", out), r.state);
  assert.ok(Math.abs(parseFloat(out[1].textContent) - 10) < 1.5, out[1].textContent);
  assert.ok(Math.abs(parseFloat(out[2].textContent) - 20) < 3, out[2].textContent);
  r.fig.act(r.state, "reset");
  assert.equal(r.state.pax.length, 0);
});

test("Ryc. 5.13 (podwyżki): lata, histogram 40 binów, średnia i mediana, oś log, podpisy", () => {
  const { run, P } = load();
  const r = run("s3p-raises", null, { s3p_4f_scale: "raw" }, "raw");
  let s = r.spec(0);
  assert.deepEqual(types(s), ["vline"]);
  assert.equal(s.layers[0].x, 5000);
  assert.equal(r.fig.noteKey({ s3p_4f_scale: "raw" }, r.state), "start");
  r.fig.act(r.state, "y10");
  assert.equal(r.state.pay.year, 10);
  assert.ok(r.state.pay.x.every(v => v >= 100));
  s = r.spec(0);
  assert.deepEqual(types(s), ["bins", "vline", "vline"]);
  assert.equal(s.layers[0].data.length, 40);
  assert.ok(s.xBig);
  assert.equal(s.legend.length, 2);
  const out = values(["pay_year", "pay_mean", "pay_median", "pay_below"]);
  r.fig.update(root("lc-sf", out), r.state);
  assert.equal(out[0].textContent, "10");
  assert.match(out[1].textContent, /^\d{1,2} \d{3} zł$/);
  assert.match(out[3].textContent, /^\d+%$/);
  assert.ok(parseInt(out[3].textContent) > 50, "prawy ogon: większość poniżej średniej");
  assert.equal(r.fig.noteKey({ s3p_4f_scale: "log" }, r.state), "log");
  const lg = r.fig.draw("log", P, 0, r.state, {}, null);
  assert.ok(!lg.xBig);
  assert.ok(lg.x[1] < 20, "oś w logarytmach");
  r.fig.act(r.state, "y1");
  assert.equal(r.state.pay.year, 11);
  r.fig.act(r.state, "reset");
  assert.equal(r.state.pay.year, 0);
  assert.ok(r.state.pay.x.every(v => v === 5000));
});
