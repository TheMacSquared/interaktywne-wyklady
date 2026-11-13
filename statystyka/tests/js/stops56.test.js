// Ryciny punktów stop rozdziałów 5–6 wykładu 03 (stops56_charts.js) bez przeglądarki:
// Ryc. 5.9 „Normalny czy nie?” i Ryc. 6.6 „Grupka przyjaciół”.
// Uruchamianie: node --test statystyka/tests/js/stops56.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const { environment, Element, root } = require("./harness");

function load() {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("03-rozklady-prawdopodobienstwa/modules/sim_charts.js");
  env.load("03-rozklady-prawdopodobienstwa/modules/stops56_charts.js");
  return env;
}

test("Ryc. 5.9: histogram 30 binów, krzywa N(m, s), odczyty jak w R", () => {
  const env = load(), S = env.window.lcSF, F = S.figures, P = env.window.lcCharts.palette();
  const fig = F["s3p-normal"];
  // Wartości z R: v <- c(1:9, 40); mean(abs(v - m) > 2 * sd(v)) = 0.1, skośność 2.0599472942.
  const v = [1, 2, 3, 4, 5, 6, 7, 8, 9, 40];
  const st = env.window.lc03Stops56.stats5d(v);
  assert.equal(st.tail, 0.1);
  assert.ok(Math.abs(st.skew - 2.0599472942) < 1e-9, `skośność ${st.skew}`);

  const config = { vars: { hk: { lab: "Wzrost kobiet (cm)", x: [160, 165, 170, 175, 180, 168, 172, null] },
                           com: { lab: "Czas dojazdu (min)", x: v } } };
  const state = { config };
  fig.prepare(state, "com", { s3p_5d_var: "com" });
  const sp = fig.draw("com", P, 0, state, {});
  assert.deepEqual(Array.from(sp.layers, l => l.type), ["bins", "line"]);
  assert.equal(sp.layers[0].data.length, 30);
  assert.equal(sp.xName, "Czas dojazdu (min)");
  assert.equal(sp.yName, "Gęstość");
  // Pole histogramu gęstości = 1.
  const area = sp.layers[0].data.reduce((s, r) => s + (r[1] - r[0]) * r[2], 0);
  assert.ok(Math.abs(area - 1) < 1e-9, `pole ${area}`);
  // Krzywa na zakresie min(v, m − 3.5s)…max(v, m + 3.5s), 400 punktów.
  const curve = sp.layers[1].data, m = st.m, s = st.s;
  assert.equal(curve.length, 400);
  assert.ok(Math.abs(curve[0][0] - Math.min(1, m - 3.5 * s)) < 1e-9);
  assert.ok(Math.abs(curve[399][0] - Math.max(40, m + 3.5 * s)) < 1e-9);
  assert.ok(sp.x[0] <= curve[0][0] && sp.x[1] >= curve[399][0]);
  assert.equal(sp.legend.length, 2);
  assert.match(sp.legend[1].text, /tej samej średniej i SD/);
  // Odczyty: s3p_pct(x, 1) i lc_fmt(x, 2).
  const tail = new Element("span", { "data-v": "tail" }), skew = new Element("span", { "data-v": "skew" });
  fig.update(root("lc-sf", [tail, skew]), state);
  assert.equal(tail.textContent, "10%");
  assert.equal(skew.textContent, "2.06");
  // Braki danych odfiltrowane; zmiana zmiennej przelicza statystyki.
  fig.prepare(state, "hk", { s3p_5d_var: "hk" });
  assert.equal(state.x.length, 7);
  fig.update(root("lc-sf", [tail, skew]), state);
  assert.equal(tail.textContent, "0%");
  assert.equal(env.errors.length, 0);
});

test("Ryc. 6.6: symulacja z ziarnem, SE ≈ σ/√n przy ρ = 0, odczyty i podpis", () => {
  const env = load(), S = env.window.lcSF, F = S.figures, P = env.window.lcCharts.palette();
  const fig = F["s3p-friends"], X = env.window.lc03Stops56;
  // Szerokość binu 0.1 jak geom_histogram(binwidth = 0.1) (layer_data z ggplot2).
  const b = env.window.lcSF.binsWidth([5.93, 6.04, 6.06, 6.15, 5.95], 0.1);
  assert.deepEqual(Array.from(b, r => [Number(r.l.toFixed(2)), Number(r.r.toFixed(2)), r.n]),
                   [[5.85, 5.95, 2], [5.95, 6.05, 1], [6.05, 6.15, 2]]);

  const run = (rho, n) => {
    const state = {}, vals = { s3p_6b_rho: String(rho), s3p_6b_n: String(n) };
    fig.prepare(state, String(rho), vals);
    const out = ["se_sim", "se_ctg", "n_eff"].map(k => new Element("span", { "data-v": k }));
    fig.update(root("lc-sf", out), state);
    return { state, vals, spec: fig.draw(null, P, 0, state, vals), read: out.map(o => o.textContent) };
  };
  for (const n of [5, 10, 30]) {
    const r = run(0, n), se = 2 / Math.sqrt(n);
    assert.equal(r.state.data.length, 3000);
    assert.ok(Math.abs(Number(r.read[0]) / se - 1) < 0.06, `n = ${n}: SE z symulacji ${r.read[0]} vs ${se}`);
    assert.equal(r.read[1], S.fmt(se, 2));
    assert.equal(r.read[2], String(n));
    assert.equal(fig.noteKey(r.vals), "indep");
  }
  // ρ > 0: SE ≈ σ·√(ρ + (1 − ρ)/n), efektywna liczba osób n / (1 + (n − 1)ρ).
  const r = run(0.3, 10);
  const seDep = 2 * Math.sqrt(0.3 + 0.7 / 10);
  assert.ok(Math.abs(Number(r.read[0]) / seDep - 1) < 0.06, `SE przy ρ = 0.3: ${r.read[0]} vs ${seDep}`);
  assert.equal(r.read[1], "0.63");
  assert.equal(r.read[2], "2.7");
  assert.equal(fig.noteKey(r.vals), "dep");
  // Stałe ziarno: te same ustawienia dają te same średnie.
  assert.deepEqual(Array.from(run(0.3, 10).state.data), Array.from(r.state.data));
  // Rysunek: słupki przycięte do osi 2–10, krzywa N(6, 2/√n).
  const sp = r.spec;
  assert.deepEqual(Array.from(sp.x), [2, 10]);
  assert.deepEqual(Array.from(sp.layers, l => l.type), ["bins", "line"]);
  assert.ok(sp.layers[0].data.every(q => q[0] >= 2 && q[1] <= 10 && q[1] - q[0] <= 0.1 + 1e-9));
  const peak = Math.max(...sp.layers[1].data.map(p => p[1]));
  assert.ok(Math.abs(peak - 1 / (Math.sqrt(2 * Math.PI) * 2 / Math.sqrt(10))) < 1e-3);
  assert.equal(sp.legend.length, 2);
  assert.match(sp.legend[0].text, /3000 grupek/);
  // Zmiana n losuje od nowa.
  const state = r.state, before = state.data;
  fig.prepare(state, "0.3", { s3p_6b_rho: "0.3", s3p_6b_n: "30" });
  assert.notEqual(state.data, before);
  assert.equal(env.errors.length, 0);
});
