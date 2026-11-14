// Quiz kafelkowy (shared/lc_quiz.js) bez przeglądarki: losowanie, odpowiedzi, wynik.
// Uruchamianie: node --test statystyka/tests/js/lc_quiz.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../../shared/lc_quiz.js"));
const Q = globalThis.lcTileQuiz;

function mulberry32(a) {
  return function () {
    a |= 0; a = a + 0x6D2B79F5 | 0;
    let t = Math.imul(a ^ a >>> 15, 1 | a);
    t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
    return ((t ^ t >>> 14) >>> 0) / 4294967296;
  };
}
const questions = Array.from({ length: 20 }, (_, i) => ({
  scenario: "Scenariusz " + i, question: "Pytanie " + i,
  options: ["a" + i, "b" + i, "c" + i, "d" + i], correct: 1 + (i % 4), explanation: "Bo " + i
}));
const cfg = {
  n: 10, questions,
  summary: [{ min: 90, text: "świetnie" }, { min: 70, text: "dobrze" }, { min: 50, text: "nieźle" }, { min: 0, text: "wróć" }]
};
// Pozycja kafelka z poprawną odpowiedzią w bieżącej kolejności.
const rightTile = (s) => s.order.indexOf(s.questions[s.index].correct - 1);

test("tasowanie zachowuje elementy i nie zmienia wejścia", () => {
  const input = [1, 2, 3, 4, 5, 6];
  const out = Q.shuffle(input, mulberry32(3));
  assert.deepEqual(input, [1, 2, 3, 4, 5, 6]);
  assert.deepEqual(out.slice().sort(), input);
});

test("start losuje n różnych pytań i kolejność odpowiedzi", () => {
  const s = Q.start(cfg, mulberry32(1));
  assert.equal(s.total, 10);
  assert.equal(new Set(s.questions.map((q) => q.question)).size, 10);
  assert.deepEqual(s.order.slice().sort(), [0, 1, 2, 3]);
  assert.deepEqual([s.index, s.correct, s.wrong, s.answered, s.finished], [0, 0, 0, false, false]);
  // pula mniejsza niż n: wszystkie pytania
  assert.equal(Q.start({ n: 10, questions: questions.slice(0, 3) }, mulberry32(1)).total, 3);
});

test("odpowiedź liczy się raz i zapamiętuje omówienie", () => {
  const s = Q.start(cfg, mulberry32(2));
  const q = s.questions[0];
  Q.answer(s, rightTile(s));
  assert.deepEqual([s.correct, s.wrong, s.answered], [1, 0, true]);
  assert.deepEqual(s.last, { ok: true, label: q.options[q.correct - 1], explanation: q.explanation });
  Q.answer(s, (rightTile(s) + 1) % 4);          // drugie kliknięcie nic nie zmienia
  assert.deepEqual([s.correct, s.wrong], [1, 0]);
  Q.next(s, mulberry32(5));
  assert.deepEqual([s.index, s.answered], [1, false]);
  Q.answer(s, (rightTile(s) + 1) % 4);
  assert.deepEqual([s.correct, s.wrong, s.last.ok], [1, 1, false]);
  assert.equal(s.last.label, s.questions[1].options[s.questions[1].correct - 1]);
});

test("pełna gra: po ostatnim pytaniu wynik i progi podsumowania", () => {
  const rnd = mulberry32(9);
  const s = Q.start(cfg, rnd);
  for (let i = 0; i < 10; i++) {
    Q.answer(s, i < 7 ? rightTile(s) : (rightTile(s) + 1) % 4);
    if (i < 9) Q.next(s, rnd);
  }
  assert.equal(s.index, 9);
  Q.finish(s);
  assert.deepEqual([s.finished, s.correct, s.wrong], [true, 7, 3]);
  assert.deepEqual(Q.summary(s, cfg), { pct: 70, type: "ok", text: "dobrze" });
  Q.answer(s, 0);                                  // po zakończeniu odpowiedzi są ignorowane
  assert.deepEqual([s.correct, s.wrong], [7, 3]);
  const at = (c) => Q.summary({ correct: c, total: 10 }, cfg);
  assert.deepEqual(at(10), { pct: 100, type: "ok", text: "świetnie" });
  assert.deepEqual(at(9), { pct: 90, type: "ok", text: "świetnie" });
  assert.deepEqual(at(5), { pct: 50, type: "warning", text: "nieźle" });
  assert.deepEqual(at(4), { pct: 40, type: "danger", text: "wróć" });
  assert.deepEqual(at(0), { pct: 0, type: "danger", text: "wróć" });
});

test("next na ostatnim pytaniu kończy grę", () => {
  const s = Q.start({ n: 1, questions: questions.slice(0, 1) }, mulberry32(4));
  Q.answer(s, rightTile(s));
  Q.next(s, mulberry32(4));
  assert.equal(s.finished, true);
});

test("kolor wyniku w trakcie gry: stały albo z progów", () => {
  const levels = { start: "s", ok: "o", mid: "m", low: "l" };
  assert.equal(Q.scoreColor({ correct: 0, wrong: 0 }, { score: "x" }), "x");
  assert.equal(Q.scoreColor({ correct: 0, wrong: 0 }, { score_levels: levels }), "s");
  assert.equal(Q.scoreColor({ correct: 7, wrong: 3 }, { score_levels: levels }), "o");
  assert.equal(Q.scoreColor({ correct: 5, wrong: 5 }, { score_levels: levels }), "m");
  assert.equal(Q.scoreColor({ correct: 4, wrong: 6 }, { score_levels: levels }), "l");
});

test("część odpowiedzi w pytaniu: poprawna i losowe błędne", () => {
  const six = { n: 5, option_count: 3, questions: Array.from({ length: 8 }, (_, i) => ({
    question: "P" + i, options: ["a", "b", "c", "d", "e", "f"], correct: 1 + (i % 6), explanation: "" })) };
  const rnd = mulberry32(21);
  const s = Q.start(six, rnd);
  const seen = new Set();
  for (let i = 0; i < 5; i++) {
    assert.equal(s.order.length, 3);
    assert.equal(new Set(s.order).size, 3, "odpowiedzi się nie powtarzają");
    assert.ok(s.order.includes(s.questions[s.index].correct - 1), "poprawna jest wśród kafelków");
    s.order.forEach((k) => seen.add(k));
    Q.answer(s, rightTile(s));
    assert.equal(s.last.ok, true);
    if (i < 4) Q.next(s, rnd);
  }
  assert.ok(seen.size > 3, "błędne odpowiedzi są losowane z całej puli");
});
