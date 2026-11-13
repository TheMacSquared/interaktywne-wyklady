// Ryciny rozdziału 5 wykładu 04 liczone w przeglądarce (sf_ch5.js):
// Ryc. 5.1 (c5-poll), 5.2 (c5-factors), 5.3 (c5-plan), 5.4 (c5-cmp), Przykład 1–3 (c5-edge), Ryc. 5.5 (c5-anatomy).
const test = require("node:test");
const assert = require("node:assert/strict");
const { environment, Element, root } = require("./harness");

function load() {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("04-przedzialy-ufnosci/modules/sf_ch5.js");
  const F = env.window.lcSF.figures, P = env.window.lcCharts.palette(), H = env.window.lc04Ch5;
  const run = (name, config, rootEl) => {
    const fig = F[name], state = { config };
    if (fig.init) fig.init(state, rootEl);
    return { fig, state, spec: (k, step) => fig.draw(null, P, k || 0, state, {}, step) };
  };
  return { env, F, P, H, run };
}
const types = spec => Array.from(spec.layers, l => l.type);
const texts = spec => Array.from(spec.layers).filter(l => l.type === "text").map(l => l.text);
const values = keys => keys.map(k => new Element("span", { "data-v": k }));
const input = (id, value) => new Element("input", { id, value: String(value) });
const near = (a, b, eps = 1e-3) => assert.ok(Math.abs(a - b) < eps, `${a} ≠ ${b}`);

test("Ryc. 5.1: dokup, podwój, reset; ślady, ścieżka i odczyty", () => {
  const { run, H } = load();
  const r = run("c5-poll", null);
  assert.deepEqual(Array.from(r.state.ns), [100]);
  let s = r.spec(0);
  assert.deepEqual(types(s), ["vline", "segment", "point"], "bez śladów na starcie");
  r.fig.act(r.state, "dokup"); r.fig.act(r.state, "double");
  assert.deepEqual(Array.from(r.state.ns), [100, 200, 400]);
  s = r.spec(0);
  assert.equal(s.layers[1].items.length, 2, "dwa poprzednie przedziały jako ślady");
  near(s.layers[2].to[0] - 50, 100 * 1.96 * Math.sqrt(0.25 / 400));
  const p = r.spec(1);
  const path = p.layers.find(l => l.type === "points");
  assert.deepEqual(Array.from(path.data, d => d[0]), [2, 4, 8], "koszt w tys. zł");
  near(H.pollMe(100) * 100, 9.8, 1e-9);
  const out = values(["poll_n", "poll_me", "poll_cost"]);
  r.fig.update(root("lc-sf", out), r.state);
  assert.deepEqual(out.map(o => o.textContent), ["400", "± 4.9 pkt proc.", "8 000 zł"]);
  for (let i = 0; i < 15; i++) r.fig.act(r.state, "dokup");
  assert.equal(r.spec(0).layers[1].items.length, 12, "najwyżej 12 śladów");
  r.fig.act(r.state, "reset");
  assert.deepEqual(Array.from(r.state.ns), [100]);
});

test("Ryc. 5.2: t*, ME i szerokość jak w R; suwaki zmieniają wykres", () => {
  const { run } = load();
  const n = input("ch5_n", 30), conf = input("ch5_conf", 0.95), sd = input("ch5_s", 8);
  const el = root("lc-sf", [n, conf, sd, ...values(["f_me", "f_width", "f_t"])]);
  const r = run("c5-factors", null, el);
  r.fig.update(el, r.state);
  const read = () => ["f_me", "f_width", "f_t"].map(k => el.querySelector(`[data-v="${k}"]`).textContent);
  assert.deepEqual(read(), ["2.99", "5.97", "2.045"]);
  let top = r.spec(0), bot = r.spec(1);
  assert.equal(top.layers[0].data.length, 96, "krzywa dla n = 5…100");
  const bar = bot.layers.find(l => l.type === "segs").items[0];
  near(bar.to[0] - 170, 2.98727, 1e-4);
  assert.ok(texts(bot).includes("95% CI"));
  const xFixed = Array.from(bot.x);
  n.value = "10"; conf.value = "0.99";
  r.fig.update(el, r.state);
  assert.deepEqual(read(), ["8.22", "16.44", "3.25"]);
  bot = r.spec(1);
  assert.deepEqual(Array.from(bot.x), xFixed, "stała oś dolnego wykresu");
  assert.ok(texts(bot).includes("99% CI"));
  sd.value = "4"; n.value = "30"; conf.value = "0.95";
  r.fig.update(el, r.state);
  assert.equal(read()[0], "1.49");
});

test("Ryc. 5.3: n = ⌈(z*·s/ME)²⌉, wzór i pasek osiągniętego ME", () => {
  const { run } = load();
  const me = input("ch5_plan_me", 2), sd = input("ch5_plan_s", 10), conf = input("ch5_plan_conf", 0.95);
  const res = new Element("div", { "data-role": "plan_result" });
  const el = root("lc-sf", [me, sd, conf, res]);
  const r = run("c5-plan", null, el);
  r.fig.update(el, r.state);
  assert.match(res.innerHTML, /Wymagana wielkość próby/);
  assert.match(res.innerHTML, /\(1\.96 · 10 \/ 2\)² = 96 ≈ <b>97<\/b>/);
  let top = r.spec(0);
  assert.ok(texts(top).includes("n = 97"));
  const bot = r.spec(1);
  assert.ok(texts(bot).includes("Osiągnięte ME ≤ wymagane ✓"));
  me.value = "1"; r.fig.update(el, r.state);
  assert.ok(texts(r.spec(0)).includes("n = 385"));
  me.value = "2"; conf.value = "0.9"; r.fig.update(el, r.state);
  assert.ok(texts(r.spec(0)).includes("n = 68"));
  conf.value = "0.99";
  assert.ok(texts(r.spec(0)).includes("n = 166"));
  me.value = ""; r.fig.update(el, r.state);
  assert.equal(r.spec(0), null, "puste pole: bez wykresu");
  assert.equal(res.innerHTML, "");
});

test("Ryc. 5.4: przed kliknięciem zachęta; po kliknięciu trzy przedziały jak w R", () => {
  const { run } = load();
  const sel = new Element("select", { id: "ch5_cmp_data", value: "height" });
  const stats = new Element("div", { "data-role": "cmp_stats" });
  const el = root("lc-sf", [sel, stats]);
  const cfg = { data: { height: { xbar: 170.6859, s: 12.55028, n: 30 }, commute: { xbar: 30, s: 15, n: 50 } } };
  const r = run("c5-cmp", cfg, el);
  assert.deepEqual(texts(r.spec(0)), ["Kliknij „Oblicz 3 przedziały”"]);
  r.fig.update(el, r.state); assert.equal(stats.innerHTML, "");
  r.fig.act(r.state, "calc", el);
  const s = r.spec(0);
  assert.deepEqual(texts(s), ["[166.79 ; 174.58]", "[166 ; 175.37]", "[164.37 ; 177]"]);
  assert.deepEqual(Object.values(s.yLabels), ["99%", "95%", "90%"]);
  r.fig.update(el, r.state);
  assert.match(stats.innerHTML, /90%.*±3\.89.*95%.*±4\.69.*99%.*±6\.32/);
  sel.value = "commute";
  assert.deepEqual(texts(r.spec(0))[0], "[166.79 ; 174.58]", "zmiana danych dopiero po kliknięciu");
  r.fig.act(r.state, "calc", el);
  assert.notEqual(texts(r.spec(0))[0], "[166.79 ; 174.58]");
});

test("Przykład 1–3: poziom ufności, odsłonięcie werdyktu, granice jak w R", () => {
  const { run, H } = load();
  const cfg1 = { kind: "mean", xbar: 28.5, s: 8, n: 40, text: "Średni czas dojazdu przekracza 26 min", bound: 26, dir: "gt", xlab: "Średni czas dojazdu (min)" };
  const btn = a => new Element("button", { "data-stat-act": a, class: "lc-action is-outline" });
  const bs = ["c90", "c95", "c99", "reveal"].map(btn), ex = new Element("div", { "data-role": "edge_explain" });
  const el = root("lc-sf", [...bs, ex]);
  const r = run("c5-edge", cfg1, el);
  r.fig.update(el, r.state);
  assert.equal(bs[3].hidden, true, "werdykt ukryty przed wyborem poziomu");
  assert.match(ex.innerHTML, /Wybierz poziom ufności \(90%, 95% lub 99%\)/);
  let s = r.spec(0);
  assert.ok(texts(s).includes("Wybierz poziom ufności powyżej"));
  assert.ok(texts(s).includes("x̄ = 28.5") && texts(s).includes("≥ 26"));
  near(s.x[0], 23.22508, 1e-4); near(s.x[1], 33.77492, 1e-4);
  r.fig.act(r.state, "c90"); r.fig.update(el, r.state);
  assert.equal(bs[0].getAttribute("aria-pressed"), "true");
  assert.ok(bs[0].classList.contains("is-solid") && bs[1].classList.contains("is-outline"));
  assert.equal(bs[3].hidden, false);
  assert.match(ex.innerHTML, /Poziom ufności:<\/strong> 90%/);
  assert.ok(texts(r.spec(0)).includes("90% CI: [26.369 ; 30.631]"));
  r.fig.act(r.state, "reveal"); r.fig.update(el, r.state);
  assert.equal(bs[3].hidden, true);
  assert.match(ex.innerHTML, /lc-status-ok.*Werdykt przy 90% ufności: TAK/);
  assert.match(ex.innerHTML, /możemy stwierdzić: średni czas dojazdu przekracza 26 min\./);
  r.fig.act(r.state, "c95"); r.fig.update(el, r.state);
  assert.equal(r.state.revealed, false, "zmiana poziomu chowa werdykt");
  r.fig.act(r.state, "reveal"); r.fig.update(el, r.state);
  assert.match(ex.innerHTML, /lc-status-warning.*NIEPEWNE/);
  assert.match(ex.innerHTML, /przecina granicę hipotezy \(26\)/);
  // Przykład 2 (proporcja) i 3.
  const ci2 = H.edgeCi({ kind: "prop", x: 540, n: 1000 }, 0.99);
  near(ci2.lower, 0.4994031, 1e-6); near(ci2.upper, 0.5805969, 1e-6);
  assert.equal(H.edgeVerdict(ci2.lower, ci2.upper, 0.5, "gt"), "maybe");
  const ci2b = H.edgeCi({ kind: "prop", x: 540, n: 1000 }, 0.95);
  assert.equal(H.edgeVerdict(ci2b.lower, ci2b.upper, 0.5, "gt"), "yes");
  const ci3 = H.edgeCi({ kind: "mean", xbar: 68, s: 10, n: 20 }, 0.90);
  near(ci3.lower, 64.13354, 1e-4);
  assert.equal(H.edgeVerdict(ci3.lower, ci3.upper, 65, "gt"), "maybe");
  assert.equal(H.edgeVerdict(66, 70, 65, "lt"), "no");
});

test("Ryc. 5.5: warstwy kroków i tekst kroku z config", () => {
  const { run } = load();
  const r = run("c5-anatomy", { texts: ["a", "b", "c", "d", "e"] });
  assert.deepEqual(types(r.spec(0, 1)), ["point"]);
  assert.deepEqual(types(r.spec(0, 2)), ["segs", "point"]);
  const s3 = r.spec(0, 3);
  assert.deepEqual(types(s3), ["segs", "segs", "text", "point"]);
  near(s3.layers[1].items[0].from[0], 25.94148, 1e-4);
  near(s3.layers[1].items[0].to[0], 31.05852, 1e-4);
  const s5 = r.spec(0, 5);
  assert.equal(s5.layers[0].type, "rect");
  assert.ok(texts(s5).includes("próg 26 min"));
  assert.deepEqual(Array.from(s5.xTicks), [22, 24, 26, 28, 30, 32, 34]);
  assert.equal(r.fig.text(r.state, null, null, 4), "d");
});
