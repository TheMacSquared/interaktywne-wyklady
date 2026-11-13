// Punkty stop rozdziałów 1–2 wykładu 03 (stops12_charts.js) bez serwera R:
// Ryc. 1.1 „Stos kostek”, 2.2 „Ile wraca z 1 zł”, 2.3 „Ubezpieczenie telefonu”,
// 2.4 „Dwie trasy na egzamin”, 2.5 „Pasy wypłat”.
// Uruchamianie: node --test statystyka/tests/js/stops12.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const { environment, Element, root } = require("./harness");

function setup() {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("03-rozklady-prawdopodobienstwa/modules/sim_charts.js");
  env.load("03-rozklady-prawdopodobienstwa/modules/stops12_charts.js");
  const S = env.window.lcSF, P = env.window.lcCharts.palette();
  // Rycina jak w mount(): init, prepare, act, draw, update; opis przechodzi przez option().
  const run = (name, config, v, vals) => {
    const fig = S.figures[name], state = { config };
    if (fig.init) fig.init(state);
    const prep = (v2, vals2) => { if (fig.prepare) fig.prepare(state, v2, vals2 || {}); };
    prep(v, vals);
    const spec = k => {
      const sp = fig.draw(v, P, k || 0, state, vals || {});
      S.option(sp, P, 12, 640);  // silnik przyjmuje opis bez błędu
      return sp;
    };
    return { fig, state, spec, prep, act: a => fig.act(state, a) };
  };
  const host = keys => {
    const els = {};
    keys.forEach(k => { els[k] = new Element("span", { "data-v": k }); });
    return { els, root: root("lc-sf", Object.values(els)) };
  };
  return { env, S, run, host };
}

const types = sp => Array.from(sp.layers, l => l.type);

test("Ryc. 1.1 stos kostek: kafelki do 60 rzutów, potem słupki częstości", () => {
  const { env, run, host } = setup();
  const cfg = { tiles_max: 60, captions: { few: "FEW", tiles: "TILES", bars: "BARS" } };
  const r = run("d03-dice", cfg);
  assert.match(r.spec().aria, /Rzuć kostką/);
  const cap = new Element("div", { "data-role": "caption" }, [new Element("span", { "data-v": "cap" })]);
  const h = host(["n", "last"]); h.root.appendChild(cap);
  r.fig.update(h.root, r.state);
  assert.equal(h.els.n.textContent, "0");
  assert.equal(h.els.last.textContent, "–");
  assert.equal(cap.hidden, true, "bez rzutów nie ma podpisu");

  r.act("r6");
  let sp = r.spec();
  const rects = sp.layers.filter(l => l.type === "rects");
  assert.equal(rects[0].items.length + rects[1].items.length, 6, "kafelek na każdy rzut");
  assert.equal(rects[1].items.length, 1, "ostatni rzut wyróżniony");
  assert.equal(sp.layers.filter(l => l.type === "text").length, 7, "cyfry na kafelkach i etykieta n/6");
  assert.equal(sp.layers[2].data[0][1], 6 / 6 + 0.5, "linia n/6 + 0.5");
  assert.equal(sp.y[1], 10);
  r.fig.update(h.root, r.state);
  assert.equal(h.els.n.textContent, "6");
  assert.equal(h.els.last.textContent, String(r.state.rolls[5]));
  assert.equal(cap.hidden, false);
  assert.equal(cap.children[0].textContent, "FEW");
  r.act("r6"); r.act("r1"); r.fig.update(h.root, r.state);
  assert.equal(cap.children[0].textContent, "TILES", "13 rzutów: drugi próg");

  r.act("r60");
  sp = r.spec();
  assert.deepEqual(types(sp).slice(0, 3), ["bins", "line", "text"]);
  assert.equal(sp.layers[0].data.length, 6);
  assert.ok(Math.abs(sp.layers[0].data.reduce((s, b) => s + b[2], 0) - 1) < 1e-12, "odsetki sumują się do 1");
  assert.deepEqual(Array.from(sp.y), [0, 0.3]);
  assert.match(sp.layers.at(-1).text, /^0(\.\d{1,3})?$/, "etykieta częstości z 3 miejscami");
  r.fig.update(h.root, r.state);
  assert.equal(cap.children[0].textContent, "BARS");
  r.act("reset");
  assert.equal(r.state.rolls.length, 0);
  assert.equal(env.errors.length, 0);
});

test("Ryc. 2.2 ile wraca z 1 zł: gry, oś log., zestawienie i wzory", () => {
  const { env, run } = setup();
  const lp = [3, 4, 5, 6].map(k => {
    const ch = (n, r) => { let s = 1; for (let i = 0; i < r; i++) s = s * (n - i) / (i + 1); return s; };
    return ch(6, k) * ch(43, 6 - k) / ch(49, 6);
  });
  const ev = (x, p) => x.reduce((s, v, i) => s + v * p[i], 0);
  const games = {
    lotto: { name: "Lotto", price: 3, x: [0, 24, 170, 6000, 5e6], p: [1 - lp.reduce((a, b) => a + b), ...lp] },
    roulette: { name: "Ruletka", price: 1, x: [0, 2], p: [19 / 37, 18 / 37] },
    scratch: { name: "Zdrapka", price: 5, x: [0, 4, 10, 100], p: [0.65, 0.25, 0.08, 0.02] }
  };
  Object.values(games).forEach(g => { g.back = ev(g.x, g.p) / g.price; });
  const cfg = { games, order: ["roulette", "scratch", "lotto"],
    tex: { lotto: "TL", roulette: "TR", scratch: "TS", all: "TA" },
    captions: { start: "START", hint: "HINT", roulette: "ROUL", scratch: "SCR", lotto_few: "LF", lotto: "LOTTO", all: "ALL" } };
  const r = run("d03-back", cfg, "roulette", { s3p_2a_game: "roulette", s3p_2a_ev: "off" });
  let sp = r.spec();
  assert.deepEqual(Array.from(sp.x), [0, 1]);
  assert.deepEqual(types(sp), ["line", "text"], "bez gier: tylko linia „na zero”");

  const els = {};
  const mk = (tag, attrs) => (els[attrs["data-role"] || attrs["data-v"]] = new Element(tag, attrs));
  const host = root("lc-sf", [mk("div", { "data-role": "play" }), mk("button", { "data-role": "reset" }),
    mk("div", { "data-role": "reads" }), mk("div", { "data-role": "calc" }),
    ...["n", "spent", "won", "back", "cap"].map(k => mk("span", { "data-v": k }))]);
  r.fig.update(host, r.state);
  assert.equal(els.cap.textContent, "START");
  assert.equal(els.calc.hidden, true);

  r.act("p10000");
  assert.equal(r.state.wins.length, 10000);
  assert.ok(r.state.wins.every(w => w === 0 || w === 2));
  sp = r.spec();
  assert.equal(sp.x[1], 4, "oś do log10(10 000)");
  assert.deepEqual({ ...sp.xLabels }, { 0: "1", 1: "10", 2: "100", 3: "1 000", 4: "10 000" });
  const line = sp.layers.find(l => l.type === "line" && l.data.length > 2);
  assert.ok(line.data.length <= 400 && line.data[0][0] === 0, "punkty na skali log od n = 1");
  const lastM = line.data.at(-1)[1];
  assert.ok(Math.abs(lastM - 36 / 37) < 0.06, `średnia ${lastM}`);
  r.fig.update(host, r.state);
  assert.equal(els.n.textContent, "10 000");
  assert.equal(els.spent.textContent, "10 000 zł");
  assert.equal(els.cap.textContent, "HINT");

  // Wartość oczekiwana: linia, wzór i podpis gry.
  r.prep("roulette", { s3p_2a_ev: "on" });
  sp = r.spec();
  assert.ok(sp.layers.some(l => l.type === "text" && /^E = 0\.97 z 1 zł$/.test(l.text)));
  r.fig.update(host, r.state);
  assert.equal(els.calc.hidden, false);
  assert.equal(els.calc.innerHTML, "$$TR$$");
  assert.equal(els.cap.textContent, "ROUL");

  // Zmiana gry czyści wyniki; Lotto losuje z wagami ~1e-7 bez błędu.
  r.prep("lotto", { s3p_2a_ev: "on" });
  assert.equal(r.state.wins.length, 0);
  r.act("p1000");
  assert.ok(r.state.wins.every(w => games.lotto.x.includes(w)));
  r.fig.update(host, r.state);
  assert.equal(els.cap.textContent, "LOTTO");
  assert.equal(els.calc.innerHTML, "$$TL$$");

  // Zestawienie: paski, ukryte przyciski i odczyty, wzór zawsze widoczny.
  r.prep("all", { s3p_2a_ev: "off" });
  sp = r.spec();
  assert.deepEqual({ ...sp.yLabels }, { 1: "Ruletka", 2: "Zdrapka", 3: "Lotto" });
  const dark = sp.layers.filter(l => l.type === "rects")[0].items;
  assert.ok(Math.abs(dark[0][1] - 36 / 37) < 1e-12 && Math.abs(dark[1][1] - 0.76) < 1e-12);
  assert.ok(sp.layers.some(l => l.text === "wraca 0.35"));
  r.fig.update(host, r.state);
  for (const k of ["play", "reset", "reads"]) assert.equal(els[k].hidden, true, k);
  assert.equal(els.calc.hidden, false);
  assert.equal(els.calc.innerHTML, "$$TA$$");
  assert.equal(els.cap.textContent, "ALL");
  r.prep("scratch", { s3p_2a_ev: "off" });
  r.fig.update(host, r.state);
  assert.equal(els.play.hidden, false);
  assert.equal(els.calc.hidden, true);
  assert.equal(env.errors.length, 0);
});

test("Ryc. 2.3 ubezpieczenie: dwa wykresy, odczyt i próg 150 zł", () => {
  const { env, run, host } = setup();
  const r = run("d03-phone", { phone: 3000, risk: 0.05 }, "250");
  const a = r.spec(0), b = r.spec(1);
  assert.deepEqual(Array.from(a.layers[0].data, d => d[2]), [0.95, 0.05]);
  assert.deepEqual(Array.from(b.layers[0].data, d => d[2]), [1]);
  assert.equal(b.xLabels[1], "−250 zł\n(składka)");
  assert.deepEqual(Array.from(a.layers.slice(1), l => l.text), ["0.95", "0.05"]);
  const h = host(["e2", "withp", "cap"]);
  r.fig.update(h.root, r.state);
  assert.equal(h.els.e2.textContent, "-250");
  assert.match(h.els.cap.textContent, /z polisą pewne 250 zł/);
  r.prep("150"); r.fig.update(h.root, r.state);
  assert.match(h.els.cap.textContent, /^Przy składce do 150 zł/);
  assert.equal(env.errors.length, 0);
});

test("Ryc. 2.4 dwie trasy: gęstości gamma, pola spóźnienia i odczyty", () => {
  const { env, run, host } = setup();
  const routes = { A: { name: "Trasa A: obwodnica", mean: 30, sd: 2, shape: 225, scale: 4 / 30 },
                   B: { name: "Trasa B: przez centrum", mean: 25, sd: 12, shape: (25 / 12) ** 2, scale: 144 / 25 } };
  const r = run("d03-routes", { routes }, "35");
  const sp = r.spec();
  assert.deepEqual(types(sp), ["line", "line", "line", "line", "vline", "text"]);
  assert.equal(sp.layers[0].data[0][0], 35, "pole od czasu egzaminu");
  assert.ok(sp.layers[2].data.every(p => p[1] < 0.21), "gęstość A mieści się w osi");
  assert.equal(sp.legend.length, 2);
  const h = host(["lateA", "lateB", "cap"]);
  r.fig.update(h.root, r.state);
  // pgamma(35, 225, scale = 2/15, lower = FALSE) = 0.0083…, dla B 0.1844… (R)
  assert.equal(h.els.lateA.textContent, "0.8%");
  assert.equal(h.els.lateB.textContent, "18.4%");
  assert.match(h.els.cap.textContent, /obwodnicą \(A\)/);
  r.prep("28"); r.fig.update(h.root, r.state);
  assert.match(h.els.cap.textContent, /przez centrum \(B\)/);
  assert.equal(env.errors.length, 0);
});

test("Ryc. 2.5 pasy wypłat: chmury z jitterem stałym dla n, klamry ±SD", () => {
  const { env, run, host } = setup();
  const cfg = { lots: { A: { name: "A: pewne 50 zł", x: [50], p: [1] }, B: { name: "B: 0 albo 100 zł", x: [0, 100], p: [0.5, 0.5] },
                        C: { name: "C: kostka × 20 zł", x: [0, 20, 40, 60, 80, 100], p: Array(6).fill(1 / 6) } },
                sd: { A: 0, B: 50, C: Math.sqrt(3500 / 3) }, captions: { start: "START", played: "PLAYED" } };
  const r = run("d03-pays", cfg, "off");
  assert.equal(r.spec().layers.filter(l => l.type === "points").length, 0);
  r.prep("on");
  let sp = r.spec();
  assert.deepEqual(Array.from(sp.layers.filter(l => l.type === "text"), l => l.text).slice(1), ["SD ≈ 0 zł", "SD ≈ 50 zł", "SD ≈ 34 zł"],
    "bez gier klamry teoretyczne");
  r.act("g100");
  assert.ok(r.state.pays.A.every(x => x === 50));
  assert.ok(r.state.pays.C.every(x => x % 20 === 0 && x >= 0 && x <= 100));
  sp = r.spec();
  const pts = sp.layers.filter(l => l.type === "points");
  assert.equal(pts.length, 3);
  assert.equal(pts[0].alpha, 0.35, "przy n > 50 przezroczyste");
  assert.deepEqual(Array.from(r.spec().layers.filter(l => l.type === "points")[1].data, p => p[1]),
                   Array.from(pts[1].data, p => p[1]), "jitter stały między przerysowaniami");
  assert.ok(pts[0].data.every(p => Math.abs(p[0] - 50) <= 2.5 && Math.abs(p[1] - 3) <= 0.22));
  const h = host(["n", "means", "cap"]);
  r.fig.update(h.root, r.state);
  assert.equal(h.els.n.textContent, "100");
  assert.match(h.els.means.textContent, /^50 \/ \d+ \/ \d+$/);
  assert.equal(h.els.cap.textContent, "PLAYED");
  r.act("reset"); r.fig.update(h.root, r.state);
  assert.equal(h.els.means.textContent, "–");
  assert.equal(env.errors.length, 0);
});
