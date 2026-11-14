// Sceny wykładu 05 (modules/sceny.js) bez przeglądarki i bez serwera R.
// Uruchamianie: node --test analiza-ryzyka/tests/js/d5_sceny.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../05-do-zdarzenia/modules/sceny.js"));
const S = globalThis.lcD5Scenes;

// Mikroatrapa kontenera: sloty [data-d5p-out], przyciski [data-d5p], suwaki i radia po id/name.
function mount(scene, config, slots, acts, inputs = {}) {
  const out = {}, btn = {}, listeners = {};
  slots.forEach(s => { out[s] = { innerHTML: "" }; });
  acts.forEach(a => { btn[a] = { disabled: false, getAttribute: k => (k === "data-d5p" ? a : null) }; btn[a].closest = () => btn[a]; });
  const radios = inputs.radios || [];
  const root = {
    dataset: {},
    getAttribute: k => ({ "data-scene": scene, "data-config": JSON.stringify(config) })[k] ?? null,
    querySelector(sel) {
      let m = sel.match(/data-d5p-out="([a-z]+)"/);
      if (m) return out[m[1]] || null;
      m = sel.match(/data-d5p="([a-z]+)"/);
      if (m) return btn[m[1]] || null;
      m = sel.match(/^#([\w]+)$/);
      if (m) return (inputs.sliders || {})[m[1]] || null;
      m = sel.match(/output\[data-value="([\w]+)"\]/);
      if (m) return { textContent: "" };
      if (/:checked/.test(sel)) return radios.find(r => r.checked) || null;
      return null;
    },
    addEventListener(type, fn) { listeners[type] = fn; },
    contains: () => true
  };
  S.init(root);
  return { out, btn, root, click: a => listeners.click({ target: btn[a] }), fire: (type, target) => listeners[type]({ target }) };
}

test("pnbinom i round() zgodne z R", () => {
  // pnbinom(27, 3, 0.1) = 0.5886… (1 - 0.411), pnbinom(17, 3, 0.1) = P(X ≤ 20)
  assert.equal((1 - S.pnbinom(27, 3, 0.1)).toFixed(2), "0.41");
  assert.equal(S.pnbinom(17, 3, 0.1).toFixed(3), "0.323");
  assert.deepEqual([S.rround(12.5), S.rround(13.5), S.rround(40)], [12, 14, 40]);
});

test("kadry: przełączanie stałych kadrów z R", () => {
  const frames = [1, 2, 3].map(i => ({ name: "K" + i, svg: "<svg>" + i + "</svg>", body: "<p>b" + i + "</p>" }));
  const s = mount("kadry", { frames }, ["head", "stage", "body"], ["prev", "next"]);
  assert.equal(s.btn.prev.disabled, true);
  s.click("next"); s.click("next"); s.click("next");
  assert.match(s.out.head.innerHTML, /K3.*Kadr 3 z 3/);
  assert.equal(s.out.stage.innerHTML, "<svg>3</svg>");
  assert.equal(s.btn.next.disabled, true);
  s.click("prev");
  assert.equal(s.out.body.innerHTML, "<p>b2</p>");
});

test("seria za serią: losowanie, cięcie i reset przy zmianie p", () => {
  const radios = [{ value: "0.10", checked: true }, { value: "0.33", checked: false }];
  const sliders = { d5p_geo_cut: { value: "10" } };
  const s = mount("geo", { seed: 5, captions: ["c0", "c1", "c2", "c3"] }, ["head", "stage", "read", "caption", "cut"],
    ["one", "ten", "hund", "reset"], { radios, sliders });
  assert.match(s.out.read.innerHTML, /Serii: 0/);
  s.click("hund"); s.click("ten");
  assert.match(s.out.read.innerHTML, /Serii: 110/);
  assert.match(s.out.caption.innerHTML, /c3/);
  sliders.d5p_geo_cut.value = "30";
  s.fire("input", { type: "range" });
  assert.match(s.out.cut.innerHTML, /X &gt; 30: \d+ z 110 serii/);
  radios[0].checked = false; radios[1].checked = true;
  s.fire("change", { name: "d5p_geo_p" });
  assert.match(s.out.head.innerHTML, /p = 0\.33/);
  assert.match(s.out.read.innerHTML, /Serii: 0/);
  // serie kończą się pierwszą wadą
  const t = S.series(1, 0.3, S.mulberry32(9));
  assert.equal(t[t.length - 1], true);
  assert.equal(t.filter(Boolean).length, 1);
});

test("oddziały: 20 audytów z R, potem sezony i model", () => {
  const seedX = [24, 41, 12, 30, 31, 55, 18, 27, 33, 9, 44, 26, 61, 15, 29, 38, 22, 35, 19, 47];
  const caps = ["c0", "c1", "zawiódł w {k} z 20", "c3", "c4"];
  const s = mount("oddzialy", { seed: 3, seedX, model: "0.41", captions: caps }, ["head", "stage", "read", "caption"],
    ["one", "all", "sez", "reset"]);
  s.click("one");
  assert.match(s.out.head.innerHTML, /Audytów: 1/);
  s.click("all"); s.click("all");
  const over = seedX.filter(x => x > 30).length;
  assert.match(s.out.read.innerHTML, new RegExp("Zabrakło: " + over + " z 20"));
  assert.match(s.out.caption.innerHTML, new RegExp("zawiódł w " + over + " z 20"));
  s.click("sez");
  assert.match(s.out.read.innerHTML, /z 2020 \(\d+%\) · model: 0\.41/);
  assert.match(s.out.stage.innerHTML, /budżet 30/);
  s.click("reset");
  assert.match(s.out.caption.innerHTML, /c0/);
});

test("taśma przecięta: X i Y_n opisują to samo zdarzenie", () => {
  const tape = Array.from({ length: 80 }, (_, i) => [6, 13, 23, 50].includes(i));  // wady w 7, 14, 24, 51
  const sliders = { d5p_cut_n: { value: "30" } };
  const s = mount("ciecie", { start: tape, len: 80, seed: 1 }, ["head", "stage", "read"], ["shuffle"], { sliders });
  assert.match(s.out.read.innerHTML, /X = 24 ≤ 30/);
  assert.match(s.out.read.innerHTML, /Y₃₀ = 3 ≥ 3/);
  assert.match(s.out.read.innerHTML, / = 0\.589 = /);
  assert.doesNotMatch(s.out.read.innerHTML, /\\\(/);  // bez TeX: offline nie ma MathJax
  sliders.d5p_cut_n.value = "20";
  s.fire("input", { type: "range" });
  assert.match(s.out.read.innerHTML, /X = 24 &gt; 20/);
  assert.match(s.out.read.innerHTML, /zabraknie ×/);
  assert.match(s.out.head.innerHTML, /Cięcie po kontroli 20/);
  s.click("shuffle");
  assert.match(s.out.stage.innerHTML, /<svg class="d5p-svg"/);
});
