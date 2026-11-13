// Sceny wykładu 04 (modules/sceny.js) bez przeglądarki i bez serwera R.
// Uruchamianie: node --test analiza-ryzyka/tests/js/p4_sceny.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../04-wiele-prob/modules/sceny.js"));
const S = globalThis.lcP4Scenes;

// Mikroatrapa kontenera sceny: sloty [data-p4s-out] i przyciski [data-p4s].
function mount(scene, config, slots, acts) {
  const out = {}, btn = {};
  let onClick = null;
  slots.forEach(s => { out[s] = { innerHTML: "" }; });
  acts.forEach(a => {
    btn[a] = { disabled: false, attr: { "data-p4s": a } };
    btn[a].getAttribute = k => btn[a].attr[k] ?? null;
    btn[a].closest = () => btn[a];
  });
  const root = {
    dataset: {},
    getAttribute: k => ({ "data-scene": scene, "data-config": JSON.stringify(config) })[k] ?? null,
    querySelector(sel) {
      let m = sel.match(/data-p4s-out="([a-z]+)"/);
      if (m) return out[m[1]] || null;
      m = sel.match(/data-p4s="([a-z]+)"/);
      return m ? btn[m[1]] || null : null;
    },
    addEventListener(type, fn) { onClick = fn; },
    contains: () => true
  };
  S.init(root);
  const click = (a, extra) => {
    const target = extra || btn[a];
    if (extra) { extra.closest = () => extra; extra.getAttribute = k => extra.attr[k] ?? null; }
    onClick({ target });
  };
  return { out, btn, click, root };
}

const phobia = [{ button: "arachnofobia", who: "z pająkiem" }, { button: "koulrofobia", who: "z klaunem" }, { button: {}, who: "z zagrożeniem" }];

test("Pająk w kiści: kadry, wynik 0/1 i żart z fobiami", () => {
  const frames = [{ name: "A", text: "a" }, { name: "B", text: "b" }, { name: "C", text: "c" }];
  const spider = Array.from({ length: 12 }, (_, i) => i === 3 || i === 8);
  const s = mount("banany", { frames, phobia, spider }, ["head", "stage", "caption", "phobia"], ["prev", "next", "phobia"]);
  assert.equal(s.btn.prev.disabled, true);
  assert.equal(s.out.phobia.innerHTML, "");
  s.click("next");
  assert.match(s.out.phobia.innerHTML, /arachnofobia/);
  s.click("next");
  assert.equal(s.btn.next.disabled, true);
  assert.match(s.out.stage.innerHTML, /X = liczba kiści z pająkiem = 2/);
  s.click("phobia"); s.click("phobia");
  assert.match(s.out.stage.innerHTML, /z zagrożeniem = 2/);
  // NULL z R przychodzi jako {}: brak przycisku, nie „[object Object]”
  assert.equal(s.out.phobia.innerHTML, "");
  assert.match(s.out.head.innerHTML, /Kadr 3 z 3/);
});

test("Kontroler i próg: poszerzona tolerancja usuwa wady z protokołu", () => {
  const frames = [1, 2, 3, 4].map(i => ({ name: "K" + i, text: "t" + i }));
  const cfg = { frames, pressure: [3.05, 2.93, 3.12, 2.75, 3.01, 2.88, 3.24, 2.97, 3.08, 2.95], tol: [2.8, 3.2], tolLate: [2.7, 3.3] };
  assert.match(S.thresholdSvg(3, cfg), /X = liczba jedynek = 2/);
  assert.match(S.thresholdSvg(4, cfg), /X = liczba jedynek = 0/);
  // kadr 4 zaznacza przerywanym okręgiem dwa zawory, które były poza normą
  assert.equal((S.thresholdSvg(4, cfg).match(/stroke-dasharray="4 3"/g) || []).length, 2);
});

test("Partia na palecie: losowanie, stos i przejście na słupki", () => {
  const cfg = { title: "Kontrola przyjęcia", size: 100, p: 0.02, seed: 7, captions: ["c0", "c1", "c2"] };
  const s = mount("paleta", cfg, ["head", "stage", "caption"], ["one", "ten", "hund", "reset"]);
  assert.match(s.out.head.innerHTML, /Paleta czeka/);
  s.click("one");
  assert.match(s.out.head.innerHTML, /Partia nr 1 · X = \d+/);
  assert.match(s.out.caption.innerHTML, /c1/);
  s.click("ten");
  assert.match(s.out.stage.innerHTML, /Wszystkie partie \(11\)/);
  assert.equal((s.out.stage.innerHTML.match(/p4s-layer/g) || []).length, 10);
  s.click("hund");
  assert.match(s.out.stage.innerHTML, /Wszystkie partie \(111\)/);
  assert.match(s.out.stage.innerHTML, /liczba partii nad słupkiem/);
  assert.doesNotMatch(s.out.stage.innerHTML, /p4s-layer/);
  s.click("reset");
  assert.match(s.out.head.innerHTML, /Paleta czeka/);
});

test("Hala, 100 kierowców: rachunek (4.6) i licznik w hali", () => {
  const calc = S.driversCalc(10, [1, 2500, 2501, 99999]);
  assert.match(calc, /n = 250 · 10 = 2 500 przejazdów/);
  assert.match(calc, /0\.9999²⁵⁰⁰ ≈ 0\.779/);
  assert.match(calc, /1 - 0\.779 ≈ 0\.221/);
  assert.match(calc, /2 na 100 kierowców/);
  const s = mount("kierowcy", { first: [1, 300, 2500, 99999], captions: ["c0", "c1", "po {k}"] },
    ["head", "stage", "caption", "calc"], ["next", "reset"]);
  for (let i = 0; i < 12; i++) s.click("next");
  assert.equal(s.btn.next.disabled, true);
  assert.match(s.out.head.innerHTML, /Rok 10 · 2 500 przejazdów na kierowcę/);
  assert.match(s.out.caption.innerHTML, /po 3/);
  assert.match(s.out.stage.innerHTML, />r2</);
  s.click("reset");
  assert.match(s.out.calc.innerHTML, /n = 0 przejazdów/);
});

test("Odbiór dostawy: tabela decyzji sumuje się do liczby dostaw", () => {
  const cfg = { title: "Magazyn", n: 50, c: 1, quality: { good: 0.02, weak: 0.08 }, shareGood: 0.7, seed: 11, captions: ["c0", "c1"] };
  const s = mount("odbior", cfg, ["head", "stage", "caption"], ["one", "ten", "hund", "reset"]);
  s.click("one"); s.click("ten"); s.click("hund");
  assert.match(s.out.head.innerHTML, /Dostaw: 121/);
  const cells = [...s.out.caption.innerHTML.matchAll(/<td[^>]*>(\d+)<\/td>/g)].map(m => +m[1]);
  assert.equal(cells.reduce((a, b) => a + b, 0), 121);
  assert.match(s.out.stage.innerHTML, /PRZYJĘTA|ODRZUCONA/);
  // decyzja: przyjęcie wtedy i tylko wtedy, gdy najwyżej c niesprawnych
  const rnd = S.mulberry32(3);
  for (let i = 0; i < 200; i++) {
    const d = S.receiptDraw(cfg, rnd);
    assert.equal(d.accepted, d.sample.filter(Boolean).length <= 1);
  }
});

test("Tabliczka magazyniera: przegródki 1-3-3-1 i przełącznik historii w kadrze 5", () => {
  const story = (p, item, label) => ({ label, p, item, unit: ["wynik", "wyniki"], frames: ["f1", "f2", "f3", "f4"].map(f => label + f) });
  const cfg = {
    stories: { banany: story(0.1, "banana", "Banany"), zawory: story(0.02, "valve", "Zawory"), test: story(0.25, "answer", "Test") },
    storyOrder: ["banany", "zawory", "test"], frameNames: ["1", "2", "3", "4", "5"], frame5: "piąty", phobia,
    outcomes: [[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1], [1, 1, 0], [1, 0, 1], [0, 1, 1], [1, 1, 1]]
  };
  const bars = S.tallySvg(4, cfg.stories.zawory, 1, cfg.outcomes);
  ["0.941", "0.058", "0.001", "0.000008"].forEach(v => assert.match(bars, new RegExp(">" + v.replace(".", "\\.") + "<")));
  assert.match(bars, /3 · 0\.0192/);
  const s = mount("tabliczka", cfg, ["head", "stage", "caption", "phobia", "story"], ["prev", "next", "phobia"]);
  assert.match(s.out.phobia.innerHTML, /arachnofobia/);
  for (let i = 0; i < 4; i++) s.click("next");
  assert.match(s.out.caption.innerHTML, /piąty/);
  assert.match(s.out.story.innerHTML, /aria-pressed="true">Banany/);
  s.click(null, { attr: { "data-p4s": "story", "data-story": "zawory" } });
  assert.match(s.out.stage.innerHTML, /p = 0\.02\./);
  assert.equal(s.out.phobia.innerHTML, "");
  s.click("prev");
  assert.match(s.out.caption.innerHTML, /Zaworyf4/);
  assert.equal(s.out.story.innerHTML, "");
});
