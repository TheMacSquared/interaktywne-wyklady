// Gra Monty Halla wykładu 02 (modules/monty.js) bez przeglądarki.
// Uruchamianie: node --test analiza-ryzyka/tests/js/w2_monty.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../02-warunki/modules/monty.js"));
const M = globalThis.lcW2Monty;

function mulberry32(a) {
  return function () {
    a |= 0; a = a + 0x6D2B79F5 | 0;
    let t = Math.imul(a ^ a >>> 15, 1 | a);
    t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}

test("prowadzący odsłania Zonka, zmiana prowadzi do trzeciej bramki", () => {
  const rnd = mulberry32(7);
  for (let i = 0; i < 300; i++) {
    const g = M.newGame(rnd), door = 1 + (i % 3);
    M.choose(g, door, rnd);
    assert.notEqual(g.opened, g.prize);
    assert.notEqual(g.opened, g.chosen);
    M.change(g);
    assert.notEqual(g.fin, g.chosen);
    assert.notEqual(g.fin, g.opened);
    assert.equal(g.strategy, "zmiana");
  }
  const g = M.newGame(rnd);
  M.stay(g);  // bez wyboru nic się nie dzieje (jak req(monty$opened))
  assert.equal(g.fin, null);
});

test("symulacja sumuje gry i daje około 1/3 i 2/3", () => {
  const sim = { n: 0, stay: 0, sw: 0 }, rnd = mulberry32(11);
  M.addGames(sim, 10, rnd); M.addGames(sim, 10, rnd);
  assert.equal(sim.n, 20);
  assert.equal(sim.stay + sim.sw, 20);
  M.addGames(sim, 30000, rnd);
  assert.ok(Math.abs(sim.stay / sim.n - 1 / 3) < 0.01);
  assert.equal(M.pct(1 / 3), "33.3%");
});

test("kroki gry: przyciski, karty bramek i komunikaty", () => {
  const rnd = mulberry32(3), g = M.newGame(rnd);
  assert.match(M.controlsHtml(g), /Krok 1 z 3.*data-mt-act="door1".*data-mt-act="door3"/s);
  assert.match(M.doorsHtml(g), /--lc-stat-cols:3/);
  assert.equal(M.feedbackHtml(g), "");
  M.choose(g, 1, rnd);
  assert.match(M.controlsHtml(g), new RegExp("Zonka za bramką " + g.opened + "\\.</p>"));
  assert.match(M.feedbackHtml(g), /Nowa informacja:/);
  M.stay(g);
  assert.match(M.controlsHtml(g), /data-mt-act="new"/);
  assert.match(M.feedbackHtml(g), g.fin === g.prize ? /lc-status-ok.*Nagroda!/ : /lc-status-warning.*Zonk\./);
  assert.match(M.doorsHtml(g), /Twój ostateczny wybór/);
});
