// Ryciny wykładu 02 liczone w przeglądarce (survey_charts.js): Ryc. 2.2 (tabela
// krzyżowa, słupki i mapa ciepła) i Ryc. 3.2 (pudełka w grupach z tabelą).
// Wartości oczekiwane policzone w R (table, prop.table, quantile type 7, sd).
const test = require("node:test");
const assert = require("node:assert/strict");
const { environment, Element, root, ecbox } = require("./harness");

// Mała ankieta: dwie płcie, trzy kierunki (C pusty), wzrost z jedną wartością
// odstającą (200 cm u K).
const SURVEY = { n: 10, columns: {
  plec: { label: "Płeć", type: "nom", levels: ["K", "M"], values: [1, 1, 1, 1, 2, 2, 2, 1, 2, 1] },
  kierunek: { label: "Kierunek", type: "nom", levels: ["A", "B", "C"], values: [1, 2, 1, 2, 1, 1, 2, 2, 1, 1] },
  wzrost: { label: "Wzrost", type: "cont", values: [160, 165, 170, 150, 180, 175, 185, 200, 178, 168] }
} };

function setup() {
  const env = environment();
  env.window.lc01Survey = SURVEY;
  env.load("01-typy-danych/modules/chart_utils.js");
  env.load("R/lc_stat_figures.js");
  env.load("02-zwiazki-zmiennych/modules/survey_charts.js");
  return { env, S: env.window.lcSF, X: env.window.lc02Survey, P: env.window.lcCharts.palette() };
}
const text = html => html.replace(/<span class="lc-pad"[^>]*>[^<]*<\/span>/g, "").replace(/<[^>]+>/g, "|")
  .split("|").map(s => s.trim()).filter(Boolean);

test("Ryc. 2.2: tabela krzyżowa jak table() i prop.table() w R", () => {
  const { X } = setup();
  const t = X.crossTab("plec", "kierunek");
  assert.deepEqual(Array.from(t.n, r => Array.from(r)), [[3, 3, 0], [3, 1, 0]]);
  const row = X.crossValues(t, "row_pct"), col = X.crossValues(t, "col_pct");
  assert.deepEqual(Array.from(row.value, r => Array.from(r)), [[50, 50, 0], [75, 25, 0]]);
  assert.deepEqual(Array.from(col.value[1]).slice(0, 2), [50, 25]);
  assert.ok(Number.isNaN(col.value[0][2]), "pusta kolumna: 0/0 jak NaN w R");
  assert.deepEqual(Array.from(row.bottom), [60, 40, 0]);
  assert.deepEqual(Array.from(col.right), [60, 40]);
  // HTML jak lc_crosstab(): liczebności z sumami, procenty z 1 miejscem, pusta komórka „–”.
  let html = X.crossTable(t, "counts", [0, 0], "Płeć", "Kierunek", null);
  assert.deepEqual(text(html), ["Płeć", "Kierunek", "Razem", "A", "B", "C",
    "K", "3", "3", "0", "6", "M", "3", "1", "0", "4", "Razem", "6", "4", "0", "10"]);
  assert.match(html, /class="n is-target"[^>]*><button[^>]*data-stat-act="cell:0:0"[^>]*aria-pressed="true"/);
  html = X.crossTable(t, "col_pct", [1, 1], "Płeć", "Kierunek", ["Aa", "B", "C"]);
  assert.deepEqual(text(html).slice(0, 9), ["Aa", "A", "B", "B", "C", "C", "Płeć", "Kierunek", "Razem"]);
  assert.ok(text(html).includes("–"));
  assert.match(html, /<th scope="col" class="n is-base">/, "kolumna bazowa przy % kolumnowych");
  assert.match(html, /50<span class="lc-pad" aria-hidden="true">\.0<\/span>/, "dopełnienie ułamka jak lc_num()");
  // Podpisy odczytu komórki.
  assert.equal(X.crossCaption(t, "counts", [0, 1]), "„K” i „B”: <b>3</b> osób, czyli 30% całej próby.");
  assert.equal(X.crossCaption(t, "row_pct", [1, 0]), "W wierszu „M” <b>75%</b> osób to „A” (3 z 4).");
  assert.equal(X.crossCaption(t, "col_pct", [1, 1]), "W kolumnie „B” <b>25%</b> osób to „M” (1 z 4).");
});

test("Ryc. 2.2: słupki dodge, mapa ciepła, zamiana i wybór tej samej zmiennej", () => {
  const { S, P } = setup();
  const F = S.figures["d02-xt"];
  const sel = (id, value) => { const s = new Element("select", { id }); s.value = value; return s; };
  const row = sel("ch2_cross_row", "plec"), col = sel("ch2_cross_col", "kierunek");
  const host = root("lc-sf", [row, col, new Element("div", { "data-role": "table" }),
    new Element("div", { "data-role": "caption" }), new Element("div", { "data-role": "legend" })]);
  const state = { config: { labels: { plec: "Płeć", kierunek: "Kierunek" } } };
  F.init(state, host);
  const vals = { ch2_cross_row: "plec", ch2_cross_col: "kierunek", ch2_cross_type: "counts", ch2_cross_chart: "bar" };
  // Słupki: puste komórki pomijane, pozostałe dzielą szerokość 0.9 (position_dodge).
  let spec = F.draw(null, P, 0, state, vals);
  const bars = Array.from(spec.layers[0].data, r => Array.from(r));
  assert.equal(bars.length, 4);
  assert.deepEqual(bars.map(r => r[2]), [3, 3, 3, 1]);
  assert.ok(Math.abs(bars[0][1] - bars[0][0] - 0.45) < 1e-12);
  assert.equal(spec.xName, "Płeć");
  assert.ok(Math.abs(spec.y[1] - 3.15) < 1e-12);
  // Mapa ciepła: wiersz 1 na dole, wartość największa najciemniejsza, napisy w komórkach.
  spec = F.draw(null, P, 0, state, { ...vals, ch2_cross_type: "row_pct", ch2_cross_chart: "heatmap" });
  const cells = spec.layers[0].items;
  assert.equal(cells.length, 6);
  assert.deepEqual(Array.from(cells[0].r), [0.5, 1.5, 0.5, 1.5]);
  assert.equal(cells[3].color, "#4a0e1e");   // 75% (M, A)
  assert.equal(cells[2].color, "#f5f0e6");   // 0% (K, C)
  assert.deepEqual(Array.from(spec.layers.filter(l => l.type === "text"), l => l.text), ["50%", "50%", "0%", "75%", "25%", "0%"]);
  assert.equal(spec.yLabels[1], "K");
  // Tabela, podpis i legenda w update(); kliknięcie komórki zmienia odczyt.
  F.update(host, state);
  assert.match(host.querySelector('[data-role="caption"]').innerHTML, /„K” i „A”: <b>3<\/b>/);
  F.act(state, "cell:1:1");
  F.update(host, state);
  assert.match(host.querySelector('[data-role="caption"]').innerHTML, /„M” i „B”: <b>1<\/b> osób, czyli 10%/);
  assert.match(host.querySelector('[data-role="legend"]').innerHTML, /lc-legend-t">Kierunek<.*>A<.*>B<.*>C</);
  // Zamiana wierszy z kolumnami.
  F.act(state, "swap", host);
  assert.equal(row.value, "kierunek"); assert.equal(col.value, "plec");
  const v2 = { ...vals, ch2_cross_row: row.value, ch2_cross_col: col.value };
  F.prepare(state, null, v2, host);
  assert.deepEqual([v2.ch2_cross_row, v2.ch2_cross_col], ["kierunek", "plec"]);
  // Ta sama zmienna w obu listach: druga dostaje poprzednią wartość pierwszej.
  row.value = "plec";
  const v3 = { ...vals, ch2_cross_row: "plec", ch2_cross_col: "plec" };
  F.prepare(state, null, v3, host);
  assert.deepEqual([row.value, col.value], ["plec", "kierunek"]);
  assert.deepEqual([v3.ch2_cross_row, v3.ch2_cross_col], ["plec", "kierunek"]);
  // Komórka spoza mniejszej tabeli wraca do (1, 1).
  F.act(state, "cell:2:2");
  assert.deepEqual(Array.from(F.model(state, vals).target), [0, 0]);
});

test("Ryc. 3.2: pudełka (quantile type 7, wąsy 1.5 IQR), tabela i podpis jak w R", () => {
  const { S, X, P } = setup();
  const d = X.groupData("wzrost", "plec");
  assert.deepEqual(Array.from(d.levels), ["K", "M"]);
  const near = (a, b) => assert.ok(Math.abs(a - b) < 1e-4, `${a} ≠ ${b}`);
  // R: K n = 6, średnia 168.8333, mediana 166.5, SD 16.85724; M n = 4, 179.5, 179, 4.203173.
  near(d.stats[0].mean, 168.8333); near(d.stats[0].med, 166.5); near(d.stats[0].sd, 16.85724);
  near(d.stats[1].mean, 179.5); near(d.stats[1].med, 179); near(d.stats[1].sd, 4.203173);
  const F = S.figures["d02-box"], state = { config: { num: { wzrost: "Wzrost (cm)" }, grp: { plec: "Płeć" } } };
  const spec = F.draw(null, P, 0, state, { ch3_num: "wzrost", ch3_grp: "plec" });
  const boxes = spec.layers[0].boxes;
  // R: K kwartyle 161.25 / 166.5 / 169.5, wąsy 150 i 170 (200 poza 1.5 IQR); M 177.25 / 179 / 181.25, wąsy 175 i 185.
  assert.deepEqual([boxes[0].pos, boxes[0].q1, boxes[0].med, boxes[0].q3, boxes[0].lo, boxes[0].hi], [2, 161.25, 166.5, 169.5, 150, 170]);
  assert.deepEqual([boxes[1].pos, boxes[1].q1, boxes[1].med, boxes[1].q3, boxes[1].lo, boxes[1].hi], [1, 177.25, 179, 181.25, 175, 185]);
  assert.equal(spec.layers[1].data.length, 10);
  assert.ok(spec.layers[1].data.every((p, i) => Math.abs(p[1] - (SURVEY.columns.plec.values[i] === 1 ? 2 : 1)) <= 0.18));
  assert.deepEqual(Array.from(spec.x), [147.5, 202.5]);
  assert.equal(spec.xName, "Wzrost (cm)"); assert.equal(spec.yName, "Płeć");
  assert.equal(spec.yLabels[2], "K");
  // Punkty rozsunięte ze stałym ziarnem: ten sam rysunek przy każdym wywołaniu.
  assert.deepEqual(Array.from(F.draw(null, P, 0, state, { ch3_num: "wzrost", ch3_grp: "plec" }).layers[1].data, p => p[1]),
                   Array.from(spec.layers[1].data, p => p[1]));
  // Tabela jak lc_table(): skróty w nagłówku i legenda skrótów, 1 miejsce po kropce.
  const html = X.groupTable(d, "Płeć", 1);
  assert.match(html, /<div class="lc-tbl-key"><span><b>x̄<\/b> średnia<\/span>/);
  assert.match(html, /<abbr title="Odchylenie standardowe">SD<\/abbr>/);
  assert.deepEqual(text(html).filter(s => /^[\d.]+$/.test(s)), ["6", "168.8", "166.5", "16.9", "4", "179.5", "179", "4.2"]);
  // Podpis: tylko grupy od 20 osób; w małej ankiecie żadna, więc pusty.
  assert.equal(X.groupCaption(d, 1), "");
  const big = { levels: ["K", "M"], stats: [{ n: 99, med: 166 }, { n: 82, med: 177.1 }] };
  assert.equal(X.groupCaption(big, 1), "Największa różnica median w grupach od 20 osób: M − K = <b>11.1</b>.");
});

test("Ryc. 2.2 i 3.2 montują się w kontenerze lcSF", () => {
  const { env } = setup();
  const sel = (id, value) => { const s = new Element("select", { id }); s.value = value; return new Element("div", { class: "lc-select lc-sf-ctl" }, [s]); };
  const seg = (id, value) => {
    const r = new Element("input", { type: "radio", name: id, value }); r.checked = true;
    return new Element("div", { id, class: "lc-grp lc-sf-ctl" }, [r]);
  };
  const xt = root("lc-sf", [sel("ch2_cross_row", "plec"), sel("ch2_cross_col", "kierunek"), seg("ch2_cross_type", "counts"),
    seg("ch2_cross_chart", "bar"), new Element("div", { "data-role": "table" }), new Element("div", { "data-role": "caption" }),
    new Element("div", { "data-role": "legend" }), ecbox("plot")],
    { "data-fig": "d02-xt", "data-config": JSON.stringify({ labels: { plec: "Płeć", kierunek: "Kierunek" } }) });
  const box = root("lc-sf", [sel("ch3_num", "wzrost"), sel("ch3_grp", "plec"), new Element("div", { "data-role": "table" }),
    new Element("div", { "data-role": "caption" }), ecbox("plot")],
    { "data-fig": "d02-box", "data-config": JSON.stringify({ num: { wzrost: "Wzrost (cm)" }, grp: { plec: "Płeć" } }) });
  env.mount(xt); env.mount(box);
  assert.equal(env.errors.length, 0, JSON.stringify(env.errors));
  assert.match(xt.querySelector('[data-role="table"]').innerHTML, /Razem/);
  assert.match(xt.querySelector(".lc-ec").getAttribute("aria-label"), /^Wykres słupkowy liczebności: Płeć/);
  assert.match(box.querySelector(".lc-ec").getAttribute("aria-label"), /^Wykresy pudełkowe zmiennej Wzrost \(cm\) w grupach \(Płeć\)/);
  assert.match(box.querySelector('[data-role="table"]').innerHTML, /168\.8/);
});
