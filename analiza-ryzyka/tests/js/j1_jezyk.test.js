// Wykład 01 (modules/jezyk.js) bez przeglądarki: ocena klasyfikacji jak
// score_risk_classification w helpers.R, siatka zdarzeń i skumulowana częstość.
// Uruchamianie: node --test analiza-ryzyka/tests/js/j1_jezyk.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const path = require("node:path");

require(path.resolve(__dirname, "../../01-jezyk-ryzyka/modules/jezyk.js"));
const J = globalThis.lcJ1;
const labels = { hazard: "Zagrożenie", exposure: "Ekspozycja", event: "Zdarzenie", consequence: "Skutek", safeguard: "Zabezpieczenie" };
const items = [
  { id: "peel", text: "Skórka na posadzce.", correct: "hazard", explanation: "To stan otoczenia." },
  { id: "fall", text: "Upadek.", correct: "event", explanation: "To zaszło." }
];

test("klasyfikacja: pola-role (1.1) i pola-zdania (1.2)", () => {
  const ok = J.classify({ hazard: "peel", event: "fall" }, items, labels, false);
  assert.deepEqual([ok.score, ok.total], [2, 2]);
  const half = J.classify({ event: "peel" }, items, labels, false);
  assert.deepEqual(half.answers, { peel: "event", fall: "" });
  assert.equal(half.score, 0);
  // nieznane pole albo karta są pomijane (jak w assignment_to_answers)
  assert.equal(J.classify({ nope: "peel", hazard: "ghost" }, items, labels, false).answers.peel, "");
  const rows = J.classify({ peel: "hazard", fall: "consequence" }, items, labels, true);
  assert.deepEqual([rows.score, rows.answers.fall], [1, "consequence"]);
  const html = J.classificationHtml(rows, { items, labels, rows: true, ok: " Brawo." });
  assert.match(html, /lc-status-warning.*Wynik: 1\/2\./);
  assert.match(html, /<strong>Zdarzenie: <\/strong>Upadek\. Przypisana rola: Skutek\. To zaszło\./);
  assert.match(J.classificationHtml(ok, { items, labels, rows: false, ok: " Brawo." }), /lc-status-ok.*Brawo\./);
});

test("siatka zdarzeń: część wspólna w dozwolonym zakresie, liczby z tekstu wykładu", () => {
  const e = J.eventCounts(30, 20, 8);
  assert.deepEqual(e.groups, [8, 22, 12, 58]);
  assert.deepEqual([J.fmtP(e.groups[0] / 100), J.fmtP(42 / 100), J.fmtP(0.7), J.fmtP(0.58)], ["0.080", "0.420", "0.700", "0.580"]);
  const f = J.eventCounts(80, 40, 0);
  assert.deepEqual([f.lo, f.hi, f.overlap], [20, 40, 20]);
  const layers = J.gridLayers(e.groups, ["a", "b", "c", "d"]);
  assert.deepEqual(layers.map(l => l.items.length), [8, 22, 12, 58]);
  // pierwsze pole w lewym górnym rogu, z odstępem
  assert.deepEqual(layers[0].items[0].map(v => Math.round(v * 100) / 100), [0.56, 1.44, -1.44, -0.56]);
});

test("skumulowana częstość i formatowanie", () => {
  assert.deepEqual(J.cumulative([1, 0, 0, 1]), [[1, 1], [2, 0.5], [3, 1 / 3], [4, 0.5]]);
  assert.equal(J.big(1100), "1 100");
  assert.equal(J.fmtP(0.0042), "0.00420");  // jak risk_fmt_p() w R
});

// Ćwiczenie 1: notatka dla dyrektora (dawniej output$ch8_feedback w jezyk_cwiczenia_server).
const reportCfg = {
  required: ["definition", "exposure", "period", "consequence"], correct: "insufficient",
  labels: { definition: "definicja zdarzenia", exposure: "mianownik ekspozycji",
            period: "wspólny okres i warunki", consequence: "rodzaj skutków" }
};

test("notatka dla dyrektora: braki, wniosek i komplet", () => {
  const none = J.reportFeedback([], "", reportCfg);
  assert.equal(none.ok, false);
  assert.equal(none.verdict, "Wstrzymaj decyzję.");
  assert.equal(none.text, " Brakuje: definicja zdarzenia, mianownik ekspozycji, wspólny okres i warunki, rodzaj skutków." +
    " Same liczniki 3 i 5 nie pozwalają jeszcze ustalić, który magazyn jest bezpieczniejszy.");
  // wszystkie pola, zły wniosek: zostaje tylko uwaga o licznikach
  const wrong = J.reportFeedback(reportCfg.required, "safer", reportCfg);
  assert.equal(wrong.ok, false);
  assert.equal(wrong.text, " Same liczniki 3 i 5 nie pozwalają jeszcze ustalić, który magazyn jest bezpieczniejszy.");
  // dobry wniosek, brak dwóch pól: lista braków w kolejności pól
  const partial = J.reportFeedback(["consequence", "definition"], "insufficient", reportCfg);
  assert.equal(partial.ok, false);
  assert.equal(partial.text, " Brakuje: mianownik ekspozycji, wspólny okres i warunki.");
  const full = J.reportFeedback(["period", "definition", "consequence", "exposure"], "insufficient", reportCfg);
  assert.equal(full.ok, true);
  assert.equal(full.verdict, "Rekomendacja jest kompletna.");
  assert.equal(full.text, " Najpierw ujednolicamy definicje i ekspozycję, potem porównujemy częstości i skutki.");
  assert.match(J.reportHtml(full), /^<div class="lc-status" role="status" aria-live="polite"><span class="lc-status-ok"><strong>Rekomendacja jest kompletna\.<\/strong><\/span> Najpierw/);
  assert.match(J.reportHtml(none), /lc-status-warning/);
});

test("punkt startu: wynik liczy trafienia na swoich pozycjach", () => {
  const correct = ["classical", "empirical", "model"];
  assert.deepEqual(J.modelsScore(["", "", ""], correct), { score: 0, total: 3, ok: false, verdict: "Wynik: 0/3." });
  assert.equal(J.modelsScore(["classical", "model", "empirical"], correct).score, 1);
  assert.deepEqual(J.modelsScore(correct, correct), { score: 3, total: 3, ok: true, verdict: "Wynik: 3/3." });
});
