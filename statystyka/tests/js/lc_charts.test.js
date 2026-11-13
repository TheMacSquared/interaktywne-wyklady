// Testy cyklu życia bez przeglądarki: prawdziwe skrypty widgetów, kontrolowany DOM
// i atrapa renderera Chart.js. Uruchamianie: node statystyka/tests/js/lc_charts.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const { Element, environment, canvas, ecbox, root, press, statRoot } = require("./harness");

test("usunięcie i powrót zwalniają wykresy, zdarzenia i callbacki motywu", () => {
  const env = environment(), C = env.window.lcCharts;
  let paints = 0, clicks = 0;
  C.onReady(".widget", el => {
    for (const cv of el.querySelectorAll("canvas")) C.create(cv, { data: {}, options: {} });
    C.onTheme(el, () => paints++);
    C.listen(el, "click", () => clicks++);
  });
  const el = root("widget", [canvas("a"), canvas("b")]);
  for (let i = 0; i < 20; i++) {
    env.mount(el);
    env.mutations();
    assert.equal(env.live.size, 2);
    env.theme(); el.emit("click");
    assert.equal(paints, i + 1); assert.equal(clicks, i + 1);
    env.unmount(el);
    env.theme(); el.emit("click");
    assert.equal(env.live.size, 0);
    assert.equal(paints, i + 1); assert.equal(clicks, i + 1);
  }
  assert.equal(env.observers.length, 2, "obserwatory nie przyrastają przy powrotach");
  assert.equal(env.errors.length, 0);
});

test("stylowanie osi edytuje konfigurację, zachowując funkcje skali logarytmicznej", () => {
  const env = environment(), C = env.window.lcCharts;
  const callback = value => String(value), afterBuildTicks = axis => axis;
  const scales = { x: { type: "logarithmic", ticks: { callback }, afterBuildTicks }, y: { min: 0 } };
  const chart = { config: { options: { scales } }, options: new Proxy({}, {
    get() { throw new Error("Nie przypisuj resolvera Chart.js do surowej konfiguracji"); }
  }) };
  C.styleAxes(chart, null, { fontSize: 21 });
  C.styleAxes(chart, null, { fontSize: 14 });
  assert.equal(scales.x.ticks.callback, callback);
  assert.equal(scales.x.afterBuildTicks, afterBuildTicks);
  assert.equal(scales.x.type, "logarithmic");
  assert.equal(scales.x.ticks.font.size, 14);
});

test("niekompletny lub błędny widget można zainicjować ponownie", () => {
  const env = environment(), C = env.window.lcCharts;
  const el = root("widget", [canvas("a")]);
  let attempt = 0;
  C.onReady(".widget", node => {
    C.create(node.querySelector("canvas"), { data: {}, options: {} });
    if (++attempt === 1) throw new Error("inicjalizacja przerwana");
  });
  env.mount(el);
  assert.equal(env.live.size, 0);
  assert.equal(el.dataset.ready, undefined);
  env.mutations();
  assert.equal(env.live.size, 1);
  assert.equal(attempt, 2);
  assert.equal(env.errors.length, 1);
});

test("nowa animacja zastępuje starą, a usunięcie widgetu anuluje klatki", () => {
  const env = environment(), C = env.window.lcCharts;
  const el = root("widget", [canvas("a")]);
  C.onReady(".widget", node => { C.create(node.querySelector("canvas"), { data: {}, options: {} }); });
  env.mount(el);
  let value = 0, completed = 0;
  C.tween(el, 0, 1, v => { value = v; }, () => completed++);
  C.tween(el, 0, 2, v => { value = v; }, () => completed++);
  assert.equal(env.frames.size, 1);
  env.frame(0); env.frame(400);
  assert.equal(value, 2); assert.equal(completed, 1);
  C.tween(el, 2, 3, v => { value = v; });
  env.unmount(el);
  assert.equal(env.frames.size, 0);
  env.frame(800); assert.equal(value, 2);
});

test("kanały wiadomości czekają na Shiny i rejestrują się tylko raz", () => {
  const env = environment(false);
  let received;
  env.window.lcCharts.onMessage("data", msg => { received = msg; });
  assert.equal(env.handlers.size, 0);
  env.connect(); env.connect();
  assert.equal(env.registrations, 1);
  env.handlers.get("data")({ x: 42 });
  assert.equal(received.x, 42);
});

test("lejek odtwarza serie po powrocie, także po wiadomości bez obecnego DOM", () => {
  const env = environment();
  env.load("03-rozklady-prawdopodobienstwa/modules/lejek_chart.js");
  const message = env.handlers.get("lejek_series");
  message({ series: [[0, 0.5, 0.33]] });
  for (let i = 0; i < 10; i++) {
    const el = root("lc-lejek", [ecbox("lejek")]);
    env.mount(el);
    assert.equal(env.live.size, 1);
    const ch = [...env.live][0];
    assert.equal(ch.element, el.children[0]);
    assert.equal(ch.opts.renderer, "canvas", "lejek rysuje setki serii na canvas");
    assert.equal(ch.series.get("old").data.length, 0);
    assert.equal(ch.series.get("last").data.length, 3);
    message({ series: [[0, 0.5, 0.33], [1, 0.5, 0.33]] });
    assert.equal(env.timers.size, 1, "nowa seria jest animowana");
    env.unmount(el);
    assert.equal(env.timers.size, 0);
    assert.equal(env.live.size, 0);
    message({ series: [[0, 0.5, 0.33]] });
  }
  message({ series: [] });
  env.mount(root("lc-lejek", [ecbox("lejek")]));
  assert.equal([...env.live][0].series.get("last").data.length, 0, "reset działa także poza rozdziałem");
  assert.equal(env.errors.length, 0);
});

test("scenariusz koło/słupki obsługuje obie kolejności wiadomości i DOM", () => {
  for (const beforeDom of [false, true]) {
    const env = environment();
    env.load("01-typy-danych/modules/scenario_chart.js");
    const message = env.handlers.get("render_scenario");
    const data = { labels: ["A", "B"], data: [60, 40], colors: ["#aaa", "#bbb"], reveal: true };
    if (beforeDom) message(data);
    const makeRoot = () => root("lc-scenario", [ecbox("pie", { id: "ch2_pie_canvas" }), ecbox("bar", { id: "ch2_bar_canvas" })]);
    const bars = () => [...env.live][1].series.get("bar").data.length;
    let el = makeRoot(); env.mount(el);
    if (!beforeDom) message(data);
    assert.equal(env.live.size, 2); assert.equal(bars(), 2);
    message({ ...data, reveal: false }); assert.equal(bars(), 0, "słupki dopiero po odsłonięciu");
    message(data); assert.equal(bars(), 2);
    env.unmount(el); assert.equal(env.live.size, 0);
    el = makeRoot(); env.mount(el);
    assert.equal(bars(), 2, "dane wracają bez kolejnej wiadomości z R");
    env.unmount(el);
    assert.equal(env.errors.length, 0);
  }
});

test("pole pod krzywą niszczy obsługę przeciągania i poprawnie wraca", () => {
  const env = environment();
  env.load("03-rozklady-prawdopodobienstwa/modules/area_chart.js");
  const el = root("lc-area", [ecbox("pdf")]);
  env.mount(el);
  const first = [...env.live][0];
  el.children[0].emit("pointerdown", { clientX: 100, pointerId: 1, preventDefault() {} });
  assert.ok(first.updates > 2);
  env.unmount(el);
  assert.equal(el.children[0].listeners.get("pointermove").size, 0);
  env.mount(el); env.theme();
  assert.equal(env.live.size, 1);
  assert.notEqual([...env.live][0], first);
  assert.equal(env.errors.length, 0);
});

test("granice obsługują klawiaturę, a lupa powiększa napisy bez zmiany przedziału", () => {
  const env = environment();
  env.load("03-rozklady-prawdopodobienstwa/modules/area_chart.js");
  const zoom = new Element("button", { "data-lc-chart-zoom": "true", "aria-pressed": "false" });
  const el = root("lc-area", [zoom, ecbox("pdf")], { "data-a": 165, "data-b": 175, "data-mu": 170, "data-sd": 8 });
  env.mount(el);
  const a = el.querySelector("button[data-boundary=\"a\"]"), b = el.querySelector("button[data-boundary=\"b\"]");
  const key = (handle, value, shiftKey = false) => handle.emit("keydown", { key: value, shiftKey, preventDefault() {} });
  key(a, "ArrowRight"); assert.equal(a.getAttribute("aria-valuenow"), "165.5");
  key(a, "ArrowRight", true); assert.equal(a.getAttribute("aria-valuenow"), "170.5");
  key(a, "End"); assert.equal(a.getAttribute("aria-valuenow"), "175");
  key(b, "ArrowLeft"); assert.equal(b.getAttribute("aria-valuenow"), "175", "granice nie zamieniają się rolami");
  key(a, "Home"); assert.equal(a.getAttribute("aria-valuenow"), "138");
  const ch = [...env.live][0], fontBefore = ch.xAxis.axisLabel.fontSize;
  zoom.emit("click"); env.theme(el);
  assert.equal(zoom.getAttribute("aria-pressed"), "true");
  assert.ok(ch.xAxis.axisLabel.fontSize > fontBefore);
  assert.equal(a.getAttribute("aria-valuenow"), "138");
  zoom.emit("click"); env.theme(el);
  assert.equal(ch.xAxis.axisLabel.fontSize, fontBefore);
  assert.equal(el.querySelectorAll("input").length, 0);
  env.unmount(el);
  assert.equal(el.querySelectorAll("button[data-boundary]").length, 0);
  assert.equal(env.errors.length, 0);
});

test("dystrybuanta łączy wykresy i chroni wybór klawiaturowy przed hoverem", () => {
  const env = environment();
  env.load("03-rozklady-prawdopodobienstwa/modules/cdf_chart.js");
  const x = new Element("span", { "data-role": "x" });
  const el = root("lc-cdf", [ecbox("pdf"), ecbox("cdf"), x]);
  env.mount(el);
  const handle = el.querySelector(".lc-chart-handle");
  press(handle, "ArrowRight"); assert.equal(x.textContent, "1.05");
  const pdf = [...env.live][0], cdf = [...env.live][1];
  pdf.element.emit("pointermove", { clientX: 200, clientY: 0 }); assert.equal(x.textContent, "1.05");
  assert.equal(cdf.series.get("dot").data[0][0], 1.05);
  pdf.element.emit("pointerdown", { clientX: 200, clientY: 0 }); assert.equal(x.textContent, "2.00");
  pdf.element.emit("pointermove", { clientX: 150, clientY: 0 }); assert.equal(x.textContent, "1.50", "po kliknięciu kursor znów prowadzi x");
  press(handle, "ArrowLeft", true); assert.equal(x.textContent, "1.00");
  env.unmount(el); assert.equal(env.live.size, 0);
  assert.equal(env.errors.length, 0);
});

test("kwantyl ukrywa odpowiedź po zmianie i wyłącza uchwyt w trybie p", () => {
  const env = environment();
  env.load("03-rozklady-prawdopodobienstwa/modules/quantile_chart.js");
  const modeX = new Element("button", { "data-q-mode": "x" });
  const modeP = new Element("button", { "data-q-mode": "p" });
  const preset = new Element("button", { "data-q-p": "0.5", "aria-pressed": "true" });
  const reveal = new Element("button", { "data-q-reveal": "true" });
  const x = new Element("span", { "data-role": "x" }), p = new Element("span", { "data-role": "p" });
  const el = root("lc-quant", [ecbox("pdf"), modeX, modeP, preset, reveal, x, p]);
  env.mount(el);
  const handle = el.querySelector(".lc-chart-handle");
  el.emit("click", { target: reveal }); assert.notEqual(p.textContent, "?");
  press(handle, "ArrowRight"); assert.equal(p.textContent, "?");
  assert.equal(x.textContent, "1.5");
  assert.equal(handle.getAttribute("aria-valuetext"), "x = 1.5 cm");
  el.emit("click", { target: modeP });
  assert.equal(handle.hidden, true); assert.equal(x.textContent, "?");
  const old = handle.getAttribute("aria-valuenow");
  press(handle, "ArrowRight"); assert.equal(handle.getAttribute("aria-valuenow"), old);
  el.emit("click", { target: modeX }); assert.equal(handle.hidden, false);
  assert.equal(env.errors.length, 0);
});

test("percentyl przesuwa się według rzędu, z obiema wizualizacjami", () => {
  const env = environment();
  env.load("01-typy-danych/modules/chart_utils.js");
  env.load("01-typy-danych/modules/percentile_chart.js");
  const el = root("lc-pctl", [ecbox("hist"), ecbox("box")], {
    "data-config": JSON.stringify({ values: [150, 160, 170, 180, 190], p: 50 })
  });
  env.mount(el);
  const handle = el.querySelector(".lc-chart-handle");
  press(handle, "ArrowRight"); assert.equal(el.lcState.p, 51);
  press(handle, "ArrowLeft", true); assert.equal(el.lcState.p, 41);
  press(handle, "Home"); assert.equal(el.lcState.p, 0);
  press(handle, "End"); assert.equal(el.lcState.p, 100);
  assert.equal([...env.live][0].series.get("dot").data[0][0], 190);
  assert.equal(el.lcState.q, 190);
  assert.ok([...env.live][1].series.get("box").renderItem, "pudełko przerysowane z nowym znacznikiem");
  assert.equal(env.errors.length, 0);
});

test("odpowiedzi średnia/mediana są niezależne i odsłaniane tylko przyciskiem", () => {
  const env = environment();
  env.load("01-typy-danych/modules/chart_utils.js");
  env.load("01-typy-danych/modules/guess_chart.js");
  const reveal = new Element("button", { "data-g-act": "reveal" });
  const fresh = new Element("button", { "data-g-act": "new" });
  const zoom = new Element("button", { "data-lc-chart-zoom": "true" });
  const el = root("lc-guess", [ecbox("hist"), reveal, fresh, zoom]);
  env.mount(el);
  const mean = el.querySelector("[data-guess=\"mean\"]"), median = el.querySelector("[data-guess=\"median\"]");
  assert.equal(el.lcState.guess.mean, null); assert.equal(el.lcState.guess.median, null);
  assert.ok(mean.getAttribute("aria-valuetext").includes("Nie wskazano"));
  press(mean, "End"); press(median, "Home");
  assert.ok(el.lcState.guess.mean > el.lcState.guess.median, "brak ograniczenia a ≤ b dla typów");
  assert.equal(el.lcState.revealed, false); assert.equal(reveal.disabled, false);
  assert.equal([...env.live][0].series.get("tMean").data.length, 0);
  const before = JSON.stringify(el.lcState);
  zoom.emit("click"); env.theme(el); assert.equal(JSON.stringify(el.lcState), before);
  el.emit("click", { target: reveal });
  assert.equal(el.lcState.revealed, true); assert.equal(mean.hidden, true); assert.equal(median.hidden, true);
  assert.ok([...env.live][0].series.get("tMean").data.length > 0);
  for (let i = 0; i < 5; i++) el.emit("click", { target: fresh });
  assert.equal(el.querySelectorAll(".lc-chart-handle").length, 2);
  assert.equal(el.lcState.guess.mean, null); assert.equal(el.lcState.guess.median, null);
  assert.equal(mean.hidden, false);
  assert.equal(env.errors.length, 0);
});

test("punkty odstające mają osobne uchwyty 2D, zwalniane przy nowych danych", () => {
  const env = environment();
  env.load("02-zwiazki-zmiennych/modules/scatter_charts.js");
  const add = new Element("button", { "data-o-add": "true" }), fresh = new Element("button", { "data-o-new": "true" });
  const el = root("lc-scat lc-outlier", [ecbox("plot"), add, fresh]);
  env.mount(el);
  assert.equal(el.querySelectorAll(".lc-chart-handle").length, 0);
  el.emit("click", { target: add });
  const ch = [...env.live][0], handle = el.querySelector(".lc-chart-handle");
  const out = () => ch.series.get("outs").data[0], old = [...out()];
  press(handle, "ArrowRight");
  assert.ok(out()[0] > old[0]); assert.equal(out()[1], old[1]);
  press(handle, "ArrowUp", true); assert.ok(out()[1] > old[1]);
  el.emit("click", { target: add }); assert.equal(el.querySelectorAll(".lc-chart-handle").length, 2);
  el.emit("click", { target: fresh });
  assert.equal(el.querySelectorAll(".lc-chart-handle").length, 0);
  assert.equal(handle.listeners.get("keydown").size, 0);
  assert.equal(ch.series.get("base").data.length, 50);
  assert.equal(env.errors.length, 0);
});

test("wszystkie migrowane widgety inicjują się i wracają z nową instancją", () => {
  const json = value => ({ "data-config": JSON.stringify(value) });
  const normal = { mu: 170, sd: 8, x: 178, step: 0.5, grp: "Grupa", unit: "cm" };
  const zInputs = () => ["mu", "sd", "x"].map((role, i) => new Element("input", { "data-role": role, value: [170, 8, 178][i] }));
  const pair = { a: normal, b: { ...normal, mu: 180 } };
  const fixtures = [
    ["01-typy-danych", "salary_chart", () => root("lc-salary", [ecbox("hist"), ecbox("strip")],
      json({ n: 20, shape: 3, scale: 100, shift: 1000, hi: 5000, bin: 100 })), 2],
    ["01-typy-danych", "percentile_chart", () => root("lc-pctl", [ecbox("hist"), ecbox("box")],
      json({ values: [150, 160, 165, 170, 175, 180, 190], p: 50, bin: 2 })), 2],
    ["01-typy-danych", "guess_chart", () => root("lc-guess", [ecbox("hist")]), 1],
    ["01-typy-danych", "kurtosis_chart", () => root("lc-kurt", [ecbox("main"), ecbox("tail")]), 2],
    ["02-zwiazki-zmiennych", "scatter_charts", () => root("lc-scat lc-strength", [ecbox("plot")],
      json({ x: [-1, 0, 1], e: [1, -2, 1], sigma: [0.5, 1, 2] })), 1],
    ["02-zwiazki-zmiennych", "scatter_charts", () => root("lc-scat lc-slope", [ecbox("plot")],
      json({ x: [-1, 0, 1], e: [1, -2, 1], rel: 0.33, slopes: [0.5, 1, 2] })), 1],
    ["02-zwiazki-zmiennych", "scatter_charts", () => root("lc-scat lc-outlier", [ecbox("plot")]), 1],
    ["02-zwiazki-zmiennych", "scatter_charts", () => root("lc-scat lc-simpson", [ecbox("plot")],
      json({ x: [1, 2, 3, 4, 5, 6], y: [3, 2, 1, 6, 5, 4], g: [0, 0, 0, 1, 1, 1],
        groups: ["A", "B"], colors: ["--upwr-accent", "--upwr-reference"], xlim: [0, 10], ylim: [0, 10] })), 1],
    ["03-rozklady-prawdopodobienstwa", "cdf_chart", () => root("lc-cdf", [ecbox("pdf"), ecbox("cdf")]), 2],
    ["03-rozklady-prawdopodobienstwa", "quantile_chart", () => root("lc-quant", [ecbox("pdf")]), 1],
    ["03-rozklady-prawdopodobienstwa", "normals_chart", () => root("lc-normals", [ecbox("pdf")],
      { "data-mu1": 170, "data-sd1": 8, "data-mu2": 180, "data-sd2": 10 }), 1],
    ["03-rozklady-prawdopodobienstwa", "cases_chart", () => root("lc-cases", [ecbox("pdf")],
      json([{ ...normal, t: 178, tail: "upper" }])), 1],
    ["03-rozklady-prawdopodobienstwa", "duel_chart", () => root("lc-duel", [ecbox("a"), ecbox("b"), ecbox("z")],
      json([pair])), 3],
    ["03-rozklady-prawdopodobienstwa", "zscore_chart", () => root("lc-zcalc", [ecbox("x"), ecbox("z"), ...zInputs()]), 2],
    ["03-rozklady-prawdopodobienstwa", "zscore_chart", () => root("lc-zcalc", [ecbox("x"), ...zInputs()], { "data-mode": "dual" }), 1]
  ];
  for (const [course, script, makeRoot, expected] of fixtures) {
    const env = environment();
    if (course.startsWith("01")) env.load(course + "/modules/chart_utils.js");
    env.load(course + "/modules/" + script + ".js");
    for (let i = 0; i < 3; i++) {
      const el = makeRoot();
      env.mount(el); env.theme();
      assert.equal(env.errors.length, 0, course + "/" + script + ": " + env.errors.map(e => e.join(" ")).join("\n"));
      assert.equal(env.live.size, expected, course + "/" + script);
      env.unmount(el); env.theme();
      assert.equal(env.live.size, 0);
    }
  }
});

test("pensja prezesa: uchwyt pojawia się po dodaniu i znika po nowym losowaniu", () => {
  const env = environment(); env.load("01-typy-danych/modules/chart_utils.js"); env.load("01-typy-danych/modules/salary_chart.js");
  const add = new Element("button", {"data-s-act":"ceo"}), reset = new Element("button", {"data-s-act":"reset"});
  const el = root("lc-salary", [ecbox("hist"),ecbox("strip"),add,reset], {"data-config":JSON.stringify({n:30,shape:3,scale:1000,shift:2000,hi:25000,hi_wide:60000,bin:1000,bin_wide:2000,ceo:true,ceo_value:50000,snap:100})});
  env.mount(el); const h = el.querySelector(".lc-chart-handle"); assert.equal(h.hidden,true);
  el.emit("click",{target:add}); assert.equal(h.hidden,false); const oldMean=el.lcState.mean;
  press(h,"ArrowRight",true); assert.equal(Number(h.getAttribute("aria-valuenow")),51000); assert.ok(el.lcState.mean>oldMean);
  press(h,"Home"); assert.equal(el.lcState.axisMax,25000);
  press(h,"End"); assert.equal(el.lcState.axisMax,60000);
  el.emit("click",{target:reset}); assert.equal(h.hidden,true); assert.equal(el.lcState.values.length,30);
  assert.equal(env.errors.length,0);
});

test("kurtoza: krok klawiatury i preset aktualizują jednocześnie szczyt i ogon", () => {
  const env=environment(); env.load("01-typy-danych/modules/chart_utils.js");env.load("01-typy-danych/modules/kurtosis_chart.js");
  const preset=new Element("button",{"data-k-set":"0"}); const el=root("lc-kurt",[ecbox("main"),ecbox("tail"),preset]); env.mount(el);
  const h=el.querySelector(".lc-chart-handle"), tail=el.lcState.tail;
  press(h,"ArrowUp"); assert.equal(el.lcState.ek,.2); assert.ok(el.lcState.tail>tail);
  press(h,"ArrowUp",true);assert.equal(el.lcState.ek,1.2);
  press(h,"Home");assert.equal(el.lcState.ek,-1.2);
  el.emit("click",{target:preset});assert.equal(el.lcState.ek,0);assert.equal(Number(h.getAttribute("aria-valuenow")),0);
  assert.equal(env.errors.length,0);
});

for (const [kind,config,start,end] of [
  ["strength",{sigma:[.5,1,2]},.5,3.5], ["slope",{slopes:[.5,1,2],rel:.33},.5,2]
]) test(kind+": klawiatura przerywa preset i zachowuje model korelacji",()=>{
  const env=environment();env.load("02-zwiazki-zmiennych/modules/scatter_charts.js");
  const preset=new Element("button",{"data-s-preset":"2"}),r=new Element("span",{"data-role":"r"});
  const el=root("lc-scat lc-"+kind,[ecbox("plot"),preset,r],{"data-config":JSON.stringify({x:[-1,0,1],e:[1,-2,1],...config})});env.mount(el);
  const h=el.querySelector(".lc-chart-handle"),oldR=r.textContent;
  assert.equal(Number(h.getAttribute("aria-valuenow")),start);
  el.emit("click",{target:preset});assert.ok(env.frames.size>0);
  press(h,"ArrowUp");assert.equal(env.frames.size,0);assert.equal(Number(h.getAttribute("aria-valuenow")),.55);
  if(kind==="slope")assert.equal(r.textContent,oldR);else assert.notEqual(r.textContent,oldR);
  press(h,"End");assert.equal(Number(h.getAttribute("aria-valuenow")),end);assert.equal(env.errors.length,0);
});

test("dwie krzywe: niezależne średnie, odwrotny kierunek odchylenia i reset",()=>{
 const env=environment();env.load("03-rozklady-prawdopodobienstwa/modules/normals_chart.js");const reset=new Element("button",{"data-n-reset":"true"});
 const el=root("lc-normals",[ecbox("pdf"),reset],{"data-mu1":166,"data-sd1":6,"data-mu2":178,"data-sd2":7});env.mount(el);
 const [a,b]=el.querySelectorAll(".lc-chart-handle"),ch=[...env.live][0],dots=()=>ch.series.get("dots").data,original=dots().map(p=>[...p]);
 press(a,"ArrowRight",true);assert.equal(dots()[0][0],171);assert.equal(dots()[1][0],178);
 press(a,"ArrowUp");assert.ok(dots()[0][1]>original[0][1]);
 assert.equal(b.getAttribute("role"),"button");el.emit("click",{target:reset});assert.equal(dots()[0][0],166);assert.equal(dots()[0][1],original[0][1]);assert.equal(env.errors.length,0);
});

test("ryciny ECharts (03): mapa wybiera n klawiaturą, przedział liczy dane i model", () => {
  const env = environment();
  env.load("03-rozklady-prawdopodobienstwa/modules/ec_charts.js");
  const rows = [1, 2, 3].map(n => [0.1 * n, 0.5 * n, 0.1 * n]);
  const base = { key: "a", short: "A", unit: "g", axis: "Oś", story: "", one: "jedna", many: "wielu", mu: 0.5, sd: 0.3, skew: 1, xlo: 0, xhi: 1, rows, tv: [0.3, 0.2, 0.1],
    sims: 10, bins: 2, counts: [[6, 4], [5, 5], [4, 6]] };
  const n = new Element("span", { "data-role": "n" }), tv = new Element("span", { "data-role": "tv" });
  const map = root("lc-ecmap", [ecbox("map"), n, tv], { "data-config": JSON.stringify([base]) });
  env.mount(map);
  const h = map.querySelector(".lc-chart-handle");
  press(h, "ArrowDown"); assert.equal(n.textContent, "2"); assert.equal(tv.textContent, "20.0%");
  press(h, "End"); assert.equal(n.textContent, "3");
  [...env.live][0].events.mouseover({ seriesId: "map", value: [1, 0, 1] }); assert.equal(n.textContent, "1");
  env.unmount(map); assert.equal(env.live.size, 0);
  const data = new Element("span", { "data-role": "data" }), model = new Element("span", { "data-role": "model" });
  const big = new Element("button", { "data-b-preset": "big" });
  const values = Array.from({ length: 100 }, (_, i) => 150 + i);
  const brush = root("lc-brushx", [ecbox("hist"), data, model, big],
    { "data-config": JSON.stringify({ values, mean: 199.5, sd: 29, lo: 90, hi: 300, bin: 5, a: 160, b: 200 }) });
  env.mount(brush);
  assert.equal(data.textContent, "39.0% (39)");
  brush.emit("click", { target: big });
  assert.equal(data.textContent, "0.00% (0)", "kraniec osi to przedział otwarty");
  const ch = [...env.live][0];
  ch.events.brushSelected({ batch: [{ areas: [{ coordRange: [200, 230.4] }] }] });
  assert.equal(data.textContent, "29.0% (29)", "granice zaokrąglone do 1 g, przedział otwarty");
  assert.ok(ch.actions.some(a => a.type === "takeGlobalCursor"));
  env.unmount(brush); assert.equal(env.live.size, 0);
  assert.equal(env.errors.length, 0);
});

test("pojedynek: kroki w jednostkach, ukryte uchwyty w z i reset przy zmianie pary",()=>{
 const env=environment();env.load("03-rozklady-prawdopodobienstwa/modules/duel_chart.js");
 const group={mu:170,sd:8,x:178,step:.5,unit:"cm",grp:"Grupa",lab:"Osoba"};
 const raw=new Element("button",{"data-d-view":"raw"}),z=new Element("button",{"data-d-view":"z"}),pair=new Element("button",{"data-d-pair":"1"});
 const el=root("lc-duel",[root("lc-duel-top",[ecbox("a"),ecbox("b")]),root("lc-duel-z",[ecbox("z")]),raw,z,pair],{"data-config":JSON.stringify([{a:group,b:group},{a:{...group,x:38,mu:36.6,sd:.5,step:.1,unit:"°C"},b:group}])});env.mount(el);
 const [a,b]=el.querySelectorAll(".lc-chart-handle");press(a,"ArrowRight",true);assert.equal(Number(a.getAttribute("aria-valuenow")),183);assert.equal(Number(b.getAttribute("aria-valuenow")),178);
 el.emit("click",{target:z});assert.equal(a.hidden,true);press(a,"ArrowLeft");assert.equal(Number(a.getAttribute("aria-valuenow")),183);
 el.emit("click",{target:raw});assert.equal(a.hidden,false);assert.equal(Number(a.getAttribute("aria-valuenow")),183);
 el.emit("click",{target:pair});press(a,"ArrowRight");assert.equal(a.getAttribute("aria-valuetext"),"38.1 °C");assert.equal(env.errors.length,0);
});

test("galeria: krok zależy od przypadku, prawdopodobieństwo dwóch ogonów jest symetryczne",()=>{
 const env=environment();env.load("03-rozklady-prawdopodobienstwa/modules/cases_chart.js");
 const item={mu:0,sd:1,t:1,step:.1,unit:"cm",axis:"Długa nazwa osi wymagająca zawijania",tail:"outside"},other=new Element("button",{"data-c-case":"1"}),p=new Element("span",{"data-role":"p"});
 const el=root("lc-cases",[ecbox("pdf"),other,p],{"data-config":JSON.stringify([item,{...item,t:0,step:5,mu:0,sd:10,unit:"pkt",tail:"lower"}])});env.mount(el);
 const h=el.querySelector(".lc-chart-handle"),old=p.textContent;press(h,"ArrowLeft",true);assert.equal(p.textContent,"100.0%");press(h,"ArrowLeft",true);assert.equal(p.textContent,old);
 el.emit("click",{target:other});press(h,"ArrowRight");assert.equal(h.getAttribute("aria-valuetext"),"5 pkt");press(h,"End");assert.equal(Number(h.getAttribute("aria-valuenow")),40);
 const lines=env.window.lcCharts.axisTitle(item.axis,180,21);assert.ok(lines.length>1);assert.equal(lines.join(" "),item.axis);assert.equal(env.errors.length,0);
});

// Lupy paneli, w których interakcję zapewniają istniejące przyciski lub pola.
function zoomWithoutChangingData(env, el) {
  const charts = [...env.live];
  const before = JSON.stringify(charts.map(ch => ch.data));
  const font = ch => ch.options?.scales ? ch.options.scales.x.ticks.font.size : ch.axisFont;
  const sized = charts.find(ch => font(ch));
  const size = sized && font(sized);
  el.querySelector("[data-lc-chart-zoom]").emit("click");
  env.theme(el);
  assert.equal(JSON.stringify(charts.map(ch => ch.data)), before);
  assert.deepEqual([...env.live], charts, "lupa nie wymienia instancji wykresów");
  if (size) {
    const after = font(sized);
    assert.ok(el.dataset.lcChartLarge === "true" ? after > size : after < size);
  }
  assert.equal(env.errors.length, 0);
}
const zoomButton = () => new Element("button", { "data-lc-chart-zoom": "true" });

test("koło/słupki: lupa zachowuje ukrycie, odsłonięcie i paletę scenariusza", () => {
  const env = environment();
  env.load("01-typy-danych/modules/scenario_chart.js");
  const el = root("lc-scenario", [zoomButton(), ecbox("pie", {id:"ch2_pie_canvas"}), ecbox("bar", {id:"ch2_bar_canvas"})]);
  env.mount(el);
  const message = env.handlers.get("render_scenario");
  const data = {labels:["A","B"],data:[45,25],colors:["#aaa","#bbb"],reveal:false};
  message(data);
  zoomWithoutChangingData(env, el);
  const bar = [...env.live][1];
  assert.equal(bar.series.get("bar").data.length, 0, "lupa nie odsłania słupków");
  message({...data,reveal:true});
  const font = bar.axisFont;
  zoomWithoutChangingData(env, el);
  assert.ok(bar.axisFont < font, "wyłączenie lupy przywraca mniejszą czcionkę");
  assert.equal(bar.series.get("bar").data.map(d => d.itemStyle.color).join(), data.colors.join());
});

test("odporność miar: lupa zachowuje pensje i średnią uciętą", () => {
  const env=environment();
  env.load("01-typy-danych/modules/chart_utils.js"); env.load("01-typy-danych/modules/salary_chart.js");
  const add=new Element("button",{"data-s-act":"out5"});
  const el=root("lc-salary",[zoomButton(),ecbox("hist"),ecbox("strip"),add],{"data-config":JSON.stringify({n:40,shape:3,scale:1000,shift:2000,trim:.1,hi:60000,bin:1500})});
  env.mount(el); el.emit("click",{target:add});
  const before=JSON.stringify(el.lcState);
  zoomWithoutChangingData(env,el);
  assert.equal(JSON.stringify(el.lcState),before);
  assert.equal(el.lcState.values.length,45);
  assert.equal(el.querySelectorAll(".lc-chart-handle").length,0);
});

test("Simpson: lupa zachowuje widok osobnych branż", () => {
  const env=environment();env.load("02-zwiazki-zmiennych/modules/scatter_charts.js");
  const split=new Element("button",{"data-s-view":"split"});
  const el=root("lc-scat lc-simpson",[zoomButton(),ecbox("plot"),split],{"data-config":JSON.stringify({x:[1,2,3,4,5,6],y:[3,2,1,6,5,4],g:[0,0,0,1,1,1],groups:["A","B"],colors:["--upwr-accent","--upwr-reference"],xlim:[0,10],ylim:[0,10]})});
  env.mount(el);el.emit("click",{target:split});
  const ch=[...env.live][0];assert.ok(ch.series.get("g0").data.length);
  zoomWithoutChangingData(env,el);
  assert.ok(ch.series.get("g0").data.length);assert.equal(ch.series.get("all").lineStyle.type.join(),"6,4");
});

test("lejek: lupa nie kończy animacji, nie zmienia serii i zachowuje skalę log", () => {
  const env=environment();env.load("03-rozklady-prawdopodobienstwa/modules/lejek_chart.js");
  const el=root("lc-lejek",[zoomButton(),ecbox("lejek")]);env.mount(el);
  env.handlers.get("lejek_series")({series:[[.1,.2,.15]]});
  assert.equal(env.timers.size,1);
  zoomWithoutChangingData(env,el);
  assert.equal(env.timers.size,1);
  const ch=[...env.live][0];assert.equal(ch.xAxis.type,"log");
  assert.equal(ch.series.get("last").data.length,1);
  env.handlers.get("lejek_series")({series:[]});assert.equal(env.timers.size,0);
  assert.equal(ch.series.get("last").data.length,0);assert.equal(ch.series.get("old").data.length,0);
});

test("kalkulator z: pola i wynik zostają po powiększeniu, niepoprawne σ nie psuje danych", () => {
  const env=environment();env.load("03-rozklady-prawdopodobienstwa/modules/zscore_chart.js");
  const fields=["mu","sd","x"].map((role,i)=>new Element("input",{"data-role":role,value:[65,10,80][i]}));
  const z=new Element("span",{"data-role":"zval"});
  const el=root("lc-zcalc",[zoomButton(),ecbox("x"),ecbox("z"),...fields,z]);env.mount(el);
  fields[2].value="90";fields[2].emit("input");assert.equal(z.textContent,"2.50");
  zoomWithoutChangingData(env,el);assert.equal(z.textContent,"2.50");assert.equal(fields[2].value,"90");
  const before=JSON.stringify([...env.live].map(ch=>ch.data));
  fields[1].value="0";fields[1].emit("input");
  assert.equal(JSON.stringify([...env.live].map(ch=>ch.data)),before);
});


test("konstruktor CI podpina lupę poza wykresem bez pętli inicjalizacji", () => {
  const env = environment();
  const axis = {range:[0,1],breaks:[0,1],labels:["0","1"],name:"",show:true};
  const spec = {panels:[{box:[0,0,1,1],x:axis,y:axis,label:"",elements:[
    {type:"line",coords:[[.2,.5],[.8,.5]],color:"#222",fill:"transparent",alpha:1,width:2,size:3,shape:"16",dash:"solid"},
    {type:"point",coords:[.5,.5],color:"#222",fill:"transparent",alpha:1,width:1,size:5,shape:"18",dash:"solid"}
  ]}],color_keys:{}};
  const source = new Element("span", {"data-ec-spec":JSON.stringify(spec)});
  const config = root("lc-ec-plot-config", [source]);
  const plot = root("lc-ec-plot", [ecbox("ci"),config]);
  const button = new Element("button", {"data-lc-chart-zoom":"true"});
  const stepper = root("lc-stepper", [button,plot]);
  env.load("R/lc_ggplot_echarts.js");
  env.mount(stepper);
  assert.equal(env.errors.length,0,"lupa poza korzeniem wykresu nie może przerwać inicjalizacji");
  assert.equal(env.live.size,1);
  const series = [...env.live][0].series.get("panel-0");
  for (let i=0;i<2;i++) {
    const rendered = series.renderItem({dataIndex:i},{coord:p=>[p[0]*1000,p[1]*500]});
    assert.ok(rendered.clipPath.shape.width>500,"obszar przycięcia musi mieć szerokość panelu, nie grubość kreski");
    assert.ok(rendered.children.length>0);
    // Element o tym samym indeksie mógł wcześniej być napisem z przesunięciem x/y.
    assert.equal(rendered.x,0,"grupa zeruje przesunięcie po poprzednim napisie");
    assert.equal(rendered.y,0);
    assert.equal(rendered.children[0].x,0,"kształt zeruje przesunięcie po poprzednim napisie");
  }
  for(let i=0;i<5;i++) env.mutations();
  assert.equal(env.charts.length,1,"kolejne mutacje DOM nie tworzą ponownie wykresu");
  button.emit("click");
  assert.equal(stepper.dataset.lcChartLarge,"true");
  env.unmount(stepper);
  assert.equal(env.live.size,0);
  assert.equal(button.listeners.get("click").size,0,"sprzątanie usuwa obsługę zewnętrznego przycisku");
  assert.equal(env.errors.length,0);
});

test("eksploratory CI (Ryc. 2.2, 3.4, 3.5): ECharts, klawiatura, presety i sprzątanie", () => {
  const env = environment();
  env.load("04-przedzialy-ufnosci/modules/ci_explorers.js");
  const explorer = (kind, config, inputs, buttons = []) => {
    const read = new Element("div", { class: "ci-explorer-read" });
    const fields = Object.entries(inputs).map(([k, v]) => new Element("input", { "data-ex-input": k, value: String(v) }));
    const btns = buttons.map(a => new Element("button", { "data-ex-action": a }));
    const box = new Element("div", { class: "ci-explorer-chart" }, [new Element("div", { class: "lc-ec" })]);
    const el = root("ci-explorer", [...fields, ...btns, box, read], { "data-kind": kind, "data-config": JSON.stringify(config) });
    return { el, read, fields, btns };
  };
  // Ryc. 3.4: uchwyt średniej linii 1 zmienia różnicę z klawiatury.
  const diff = explorer("difference", { base: 9.08, margins: [0.18, 0.17], diff_margin: 0.25 }, { d: 0.34 }, ["trap"]);
  env.mount(diff.el);
  assert.equal(env.errors.length, 0);
  assert.equal(env.live.size, 1);
  assert.match(diff.read.textContent, /^Różnica 0.34/);
  const handle = diff.el.querySelector(".lc-chart-handle");
  press(handle, "ArrowRight", true);
  assert.equal(String(diff.fields[0].value), "0.44", "Shift + strzałka przesuwa o 0.1");
  assert.match(diff.read.textContent, /^Różnica 0.44/);
  diff.el.emit("click", { target: diff.btns[0] });
  assert.equal(String(diff.fields[0].value), "0.3", "preset „Pułapka”");
  // Ryc. 3.5: preset ustawia efekt i n, odczyt mówi o kierunku i użyteczności.
  const tq = Array.from({ length: 1981 }, () => 1.96);
  const useful = explorer("useful", { tq }, { effect: 0.4, n: 1000 }, ["useful"]);
  env.mount(useful.el);
  useful.el.emit("click", { target: useful.btns[0] });
  assert.deepEqual(useful.fields.map(f => String(f.value)), ["4", "400"]);
  assert.match(useful.read.textContent, /kierunek dodatni · oszczędność przekracza 2 min/);
  // Ryc. 2.2 bez mapy: wykres i odczyt działają, nowa seria przechodzi do kolejnych prób.
  const groups = [[{ xbar: 170, s: 10 }], [{ xbar: 190, s: 10 }]];
  const cov = explorer("coverage", { groups, tq: Array(20).fill(2.064) }, { level: 95 }, ["new"]);
  env.mount(cov.el);
  assert.match(cov.read.textContent, /trafia 1 \/ 100/);
  cov.el.emit("click", { target: cov.btns[0] });
  assert.match(cov.read.textContent, /trafia 0 \/ 100/);
  [diff, useful, cov].forEach(x => env.unmount(x.el));
  assert.equal(env.live.size, 0, "zmiana rozdziału zwalnia wykresy");
  assert.equal(diff.el.querySelectorAll(".lc-chart-handle").length, 0, "uchwyt znika razem z widgetem");
  assert.equal(env.errors.length, 0);
});

test("ryciny statystyk z ankiety (01, rozdz. 3): kontrolka, opisy dla opcji i sprzątanie", () => {
  const env = environment();
  env.window.lc01Survey = { n: 6, columns: {
    wzrost: { label: "Wzrost (cm)", type: "ilosciowa_ciagla", values: [160, 165, 170, 171, 175, 190] },
    waga: { label: "Waga (kg)", type: "ilosciowa_ciagla", values: [50, 60, 61, 70, 80, 95] },
    liczba_kursow: { label: "Liczba kursów", values: [3, 4, 4, 5, 5, 5] }
  } };
  env.load("01-typy-danych/modules/chart_utils.js");
  env.load("R/lc_stat_figures.js"); env.load("01-typy-danych/modules/stat_charts.js");
  const select = new Element("select", { value: "wzrost" });
  const notes = ["wzrost", "waga"].map(v => new Element("div", { "data-when": v }));
  const fig = root("lc-sf", [root("lc-sf-ctl", [select]), ecbox("0"), ...notes], { "data-fig": "mean" });
  env.mount(fig);
  assert.equal(env.errors.length, 0);
  assert.equal(env.live.size, 1);
  assert.deepEqual(notes.map(n => n.hidden), [false, true], "widoczny opis wybranej zmiennej");
  const chart = [...env.live][0];
  const before = chart.updates;
  select.value = "waga";
  fig.querySelector(".lc-sf-ctl").emit("change");
  assert.ok(chart.updates > before, "zmiana zmiennej przerysowuje wykres");
  assert.deepEqual(notes.map(n => n.hidden), [true, false]);
  // Dwa wykresy w jednej rycinie (Ryc. 3.7): słupki i histogram 15 przedziałów.
  const radio = new Element("input", { type: "radio", value: "liczba_kursow", checked: "" });
  const disc = root("lc-sf", [root("lc-sf-ctl", [radio]), ecbox("0"), ecbox("1")], { "data-fig": "disc" });
  env.mount(disc);
  assert.equal(env.live.size, 3);
  env.unmount(fig); env.unmount(disc);
  assert.equal(env.live.size, 0, "zmiana rozdziału zwalnia wykresy");
  assert.equal(env.errors.length, 0);
  // Reguła ggplot dla bins = k: 15 przedziałów niezgranych z liczbami całkowitymi.
  const bins = env.window.lc01Stat.binsGgplot([0, 1, 2, 3, 9], 15);
  assert.equal(bins.reduce((s, b) => s + b.n, 0), 5, "każda obserwacja trafia do jednego przedziału");
  assert.ok(bins.some(b => b.n === 0), "są puste przedziały mimo braku luk w danych");
});

test("ryciny statystyk ze stanem (01): przyciski, pole wyboru i wartości w tabeli", () => {
  const env = environment();
  env.window.lc01Survey = { n: 8, columns: {
    wzrost: { label: "Wzrost (cm)", values: [160, 165, 170, 171, 172, 175, 180, 190] },
    kierunek: { label: "Kierunek studiów", levels: ["Biologia", "Ekonomia", "Informatyka", "Psychologia"],
                values: [1, 2, 3, 3, 4, 3, 2, 1] },
    zadowolenie: { label: "Zadowolenie", levels: ["Bardzo niezadowolony", "Niezadowolony", "Neutralny", "Zadowolony", "Bardzo zadowolony"],
                   values: [1, 2, 3, 4, 5, 3, 3, 4] }
  } };
  env.load("01-typy-danych/modules/chart_utils.js");
  env.load("R/lc_stat_figures.js"); env.load("01-typy-danych/modules/stat_charts.js");
  // Ryc. 4.7: dopisanie wartości odstającej aktualizuje n i komórki tabeli.
  const add = new Element("button", { "data-stat-act": "add1" }), reset = new Element("button", { "data-stat-act": "reset" });
  const n = new Element("span", { "data-v": "n" }), range = new Element("span", { "data-v": "range" });
  const comp = root("lc-sf", [add, reset, ecbox("0"), n, range], { "data-fig": "comp" });
  env.mount(comp);
  assert.equal(n.textContent, "8"); assert.equal(range.textContent, "30.0 cm");
  comp.emit("click", { target: add });
  assert.equal(n.textContent, "9");
  assert.ok(parseFloat(range.textContent) > 55, "nowa wartość leży ok. 30 cm nad maksimum");
  comp.emit("click", { target: reset });
  assert.equal(n.textContent, "8");
  // Ryc. 2.5: losowanie daje 200 obserwacji i opis dominanty.
  const text = new Element("div", { "data-role": "mode-text" }), again = new Element("button", { "data-stat-act": "resample" });
  const mode = root("lc-sf", [again, ecbox("0"), text], { "data-fig": "mode" });
  env.mount(mode);
  assert.match(text.innerHTML, /Dominanta:<\/b> Informatyka/);
  mode.emit("click", { target: again });
  assert.match(text.innerHTML, /z 200 obserwacji/);
  // Ryc. 2.2: pole wyboru przełącza opis i kolejność.
  const box = new Element("input", { type: "checkbox" });
  const off = new Element("div", { "data-when": "off" }), on = new Element("div", { "data-when": "on" });
  const order = root("lc-sf", [root("lc-sf-ctl", [box]), ecbox("0"), ecbox("1"), off, on], { "data-fig": "order" });
  env.mount(order);
  assert.deepEqual([off.hidden, on.hidden], [false, true]);
  box.checked = true; order.querySelector(".lc-sf-ctl").emit("change");
  assert.deepEqual([off.hidden, on.hidden], [true, false]);
  [comp, mode, order].forEach(x => env.unmount(x));
  assert.equal(env.live.size, 0);
  assert.equal(env.errors.length, 0);
});

test("pudełka w grupach (01, Ryc. 4.6): kilka kontrolek wybiera opis pary zmienna × grupy", () => {
  const env = environment();
  env.window.lc01Survey = { n: 6, columns: {
    wzrost: { label: "Wzrost (cm)", values: [160, 165, 170, 175, 180, 150] },
    waga: { label: "Waga (kg)", values: [50, 60, 70, 80, 90, 55] },
    plec: { label: "Płeć", levels: ["Kobieta", "Mężczyzna"], values: [1, 1, 1, 2, 2, 1] },
    kierunek: { label: "Kierunek", levels: ["A", "B"], values: [1, 2, 1, 2, 1, 2] }
  } };
  env.load("01-typy-danych/modules/chart_utils.js");
  env.load("R/lc_stat_figures.js"); env.load("01-typy-danych/modules/stat_charts.js");
  const select = new Element("select", { id: "ch4_grp_var", value: "wzrost" });
  const by = new Element("input", { type: "radio", name: "ch4_grp_by", value: "plec", checked: "" });
  const violin = new Element("input", { type: "checkbox", id: "ch4_grp_violin" });
  const notes = ["wzrost|plec", "waga|plec"].map(k => new Element("div", { "data-when": k }));
  const ctl = (child, attrs = {}) => root("lc-sf-ctl", [child], attrs);
  const fig = root("lc-sf", [ctl(select), ctl(by, { id: "ch4_grp_by" }), ctl(violin), ecbox("0"), ...notes], { "data-fig": "groups" });
  env.mount(fig);
  assert.equal(env.errors.length, 0, String(env.errors[0] && env.errors[0][1] && env.errors[0][1].stack));
  assert.deepEqual(notes.map(n => n.hidden), [false, true]);
  select.value = "waga"; fig.querySelectorAll(".lc-sf-ctl")[0].emit("change");
  assert.deepEqual(notes.map(n => n.hidden), [true, false], "opis zależy od zmiennej i podziału");
  violin.checked = true; fig.querySelectorAll(".lc-sf-ctl")[2].emit("change");
  assert.equal(env.errors.length, 0, "skrzypce rysują się bez błędu");
  env.unmount(fig);
  assert.equal(env.live.size, 0);
});

test("ryciny krokowe (01, Ryc. 4.2 i 4.5): krok zmienia wykres, tekst i tabelę; zbędny wykres znika", () => {
  const env = environment();
  env.window.lc01Survey = { n: 0, columns: {} };
  env.load("01-typy-danych/modules/chart_utils.js");
  env.load("R/lc_stat_figures.js"); env.load("01-typy-danych/modules/stat_charts.js");
  const stepper = (id, charts, extra = []) => {
    const text = new Element("div", { id: id + "_text", class: "shiny-html-output shiny-bound-output recalculating" });
    const boxes = charts.map(k => root("lc-sf-chart", [ecbox(String(k))], { "data-k": String(k) }));
    const st = root("lc-stepper", [...boxes, text, ...extra], { id, "data-lc-step": "1" });
    return { st, text, boxes };
  };
  const table = new Element("div", { "data-role": "table" }), again = new Element("button", { "data-stat-act": "new" });
  const sd = stepper("ch4_sd", [0], [table, again]);
  const fig = root("lc-sf", [sd.st], { "data-fig": "sd" });
  env.mount(fig);
  assert.equal(env.errors.length, 0);
  assert.match(sd.text.innerHTML, /Dziesięć pomiarów/);
  assert.equal(sd.text.getAttribute("class"), "", "tekst kroku przestaje być wyjściem Shiny (bez wiecznego .recalculating)");
  assert.equal(table.innerHTML, "", "tabela dopiero od kroku 2");
  sd.st.setAttribute("data-lc-step", "3"); env.theme(sd.st);
  assert.match(sd.text.innerHTML, /Wariancja/);
  assert.match(table.innerHTML, /SUMA/);
  const before = table.innerHTML;
  fig.emit("click", { target: again });
  assert.notEqual(table.innerHTML, before, "nowy zestaw zmienia tabelę");
  // Ryc. 4.5: drugi wykres (histogram) widoczny tylko w kroku 6.
  const bp = stepper("ch4_bp", [0, 1]);
  const fig2 = root("lc-sf", [bp.st], { "data-fig": "bp" });
  env.mount(fig2);
  assert.equal(bp.boxes[1].hidden, true);
  bp.st.setAttribute("data-lc-step", "6"); env.theme(bp.st);
  assert.equal(bp.boxes[1].hidden, false);
  assert.match(bp.text.innerHTML, /Gotowy wykres pudełkowy/);
  [fig, fig2].forEach(f => env.unmount(f));
  assert.equal(env.live.size, 0);
  assert.equal(env.errors.length, 0);
});

test("palety kolorów (01, Ryc. 2.4): losowanie i zmiana palety przełączają opis i mapę", () => {
  const env = environment();
  env.window.lc01Survey = { n: 6, columns: {
    kierunek: { label: "Kierunek", levels: ["Biologia", "Ekonomia", "Informatyka", "Psychologia"], values: [1, 2, 3, 3, 4, 1] },
    wzrost: { label: "Wzrost (cm)", values: [160, 165, 170, 175, 180, 185] },
    waga: { label: "Waga (kg)", values: [55, 60, 66, 70, 80, 90] }
  } };
  env.load("01-typy-danych/modules/chart_utils.js");
  env.load("R/lc_stat_figures.js"); env.load("01-typy-danych/modules/stat_charts.js");
  const select = new Element("select", { id: "ch2_color_palette", value: "neutral" });
  const random = new Element("button", { "data-stat-act": "random" });
  const legend = new Element("div", { class: "lc-sf-legend" });
  const notes = ["neutral", "viridis", "random"].map(k => new Element("div", { "data-when": k }));
  const fig = root("lc-sf", [root("lc-sf-ctl", [select]), random, legend, ecbox("0"), ecbox("1"), ...notes], { "data-fig": "colors" });
  env.mount(fig);
  assert.equal(env.errors.length, 0);
  assert.deepEqual(notes.map(n => n.hidden), [false, true, true]);
  assert.equal(legend.hidden, false, "skala barw mapy jest widoczna");
  fig.emit("click", { target: random });
  assert.deepEqual(notes.map(n => n.hidden), [true, true, false], "losowe kolory mają własny opis");
  select.value = "viridis"; fig.querySelector(".lc-sf-ctl").emit("change");
  assert.deepEqual(notes.map(n => n.hidden), [true, false, true], "zmiana palety kasuje losowe kolory");
  env.unmount(fig);
  assert.equal(env.live.size, 0);
});

test("ryciny rozkładów (03): scenariusze z pól wyboru, podpisy, presety i pole pod N(0, 1)", () => {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("03-rozklady-prawdopodobienstwa/modules/dist_charts.js");
  // Ryc. 3.4: grupa pól wyboru daje listę zaznaczonych scenariuszy.
  const cfg = { dist: "binom", xName: "k", yName: "P", defs: {
    b1: { label: "B(10, 0.5)", n: 10, p: 0.5, color: "#2d3b3d", stats: "B(10, 0.5): E(X) = 5" },
    b2: { label: "B(20, 0.25)", n: 20, p: 0.25, color: "#c08540", stats: "B(20, 0.25): E(X) = 5" } } };
  const boxes = ["b1", "b2"].map(v => { const b = new Element("input", { type: "checkbox", value: v }); b.checked = v === "b1"; return b; });
  const stats = new Element("ul", { "data-role": "stats" }), legend = new Element("div", { class: "lc-sf-legend" });
  const fig = root("lc-sf", [root("lc-sf-ctl", boxes, { id: "sc" }), legend, ecbox("0"), stats],
                   { "data-fig": "d03-pmf", "data-config": JSON.stringify(cfg) });
  env.mount(fig);
  assert.equal(env.errors.length, 0);
  assert.equal(env.live.size, 1);
  assert.match(stats.innerHTML, /B\(10, 0.5\): E\(X\) = 5/);
  assert.doesNotMatch(stats.innerHTML, /B\(20/);
  const chart = [...env.live][0], before = chart.updates;
  boxes[1].checked = true; fig.querySelector(".lc-sf-ctl").emit("change");
  assert.ok(chart.updates > before, "zaznaczenie scenariusza przerysowuje wykres");
  assert.equal((stats.innerHTML.match(/<li>/g) || []).length, 2);
  // Bez scenariuszy wykres znika zamiast rzucać błąd (dawne req()).
  boxes.forEach(b => { b.checked = false; }); fig.querySelector(".lc-sf-ctl").emit("change");
  assert.equal(stats.hidden, true);
  assert.equal(env.errors.length, 0);

  // Ryc. 5.2: preset ustawia zakres, krok i wartość suwaków bez serwera.
  const slider = (id, min, max, value, step) => root("lc-sf-ctl", [
    new Element("output", { "data-value": id }), new Element("input", { id, type: "range", min, max, value, step })]);
  const mu = slider("ch5_mu", -10, 10, 0, 0.5), sigma = slider("ch5_sigma", 0.5, 5, 1, 0.1);
  const emp = new Element("input", { type: "checkbox", id: "ch5_show_empirical" }); emp.checked = true;
  const iq = new Element("button", { "data-stat-act": "iq" });
  const read = ["mu", "sigma", "i68"].map(k => new Element("span", { "data-v": k }));
  const presets = { iq: { mu: [50, 150, 100, 1], sigma: [1, 30, 15, 0.5] } };
  const normal = root("lc-sf", [mu, sigma, root("lc-sf-ctl", [emp]), iq, ...read, ecbox("0")],
                      { "data-fig": "d03-normal", "data-config": JSON.stringify({ presets }) });
  env.mount(normal);
  assert.equal(read[2].textContent, "[-1, 1]");
  normal.emit("click", { target: iq });
  assert.equal(normal.querySelector("#ch5_mu").value, "100");
  assert.equal(normal.querySelector("#ch5_sigma").getAttribute("max"), "30");
  assert.deepEqual(read.map(r => r.textContent), ["100", "15", "[85, 115]"]);

  // Ryc. 5.6: pole P(a < Z < b), suwak b tylko dla przedziału.
  // Atrapa DOM nie zna :checked, więc zaznaczona opcja stoi pierwsza.
  const seg = root("lc-sf-ctl", ["between", "less"].map(v => new Element("input", { type: "radio", value: v })));
  const a = slider("ch5_prob_a", -4, 4, -1, 0.05), b = slider("ch5_prob_b", -4, 4, 1, 0.05);
  b.setAttribute("data-role", "b");
  const out = ["plabel", "p", "pct"].map(k => new Element("span", { "data-v": k }));
  const zfig = root("lc-sf", [seg, a, b, ...out, ecbox("0")], { "data-fig": "d03-zprob" });
  env.mount(zfig);
  assert.deepEqual(out.map(o => o.textContent), ["P(-1 < Z < 1)", "0.6827", "68.27%"]);
  assert.equal(b.hidden, false);
  env.unmount(fig); env.unmount(normal); env.unmount(zfig);
  assert.equal(env.live.size, 0);
  assert.equal(env.errors.length, 0);
});

test("symulacje (03): losowanie lcDist, histogram ggplot i ryciny Ryc. 1.3–6.4 bez serwera", () => {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  env.load("03-rozklady-prawdopodobienstwa/modules/sim_charts.js");
  const D = env.window.lcDist, S = env.window.lcSF, F = S.figures;
  // Generatory z ziarnem: średnie i SD zgodne z rozkładami.
  const g = D.rng(7), N = 20000;
  const avg = a => a.reduce((s, v) => s + v, 0) / a.length;
  const sdv = a => { const m = avg(a); return Math.sqrt(a.reduce((s, v) => s + (v - m) ** 2, 0) / (a.length - 1)); };
  for (const [name, draw, mu, sd] of [
    ["norm", () => g.norm(170, 8), 170, 8], ["exp", () => g.exp(0.5), 2, 2],
    ["gamma", () => g.gamma(3, 10), 30, Math.sqrt(300)], ["beta", () => g.beta(0.5, 0.5), 0.5, Math.sqrt(1 / 8)],
    ["pois", () => g.pois(3), 3, Math.sqrt(3)], ["binom", () => g.binom(10, 0.6), 6, Math.sqrt(2.4)],
    ["choose", () => g.choose([0.1, 0.1, 0.1, 0.1, 0.1, 0.5]), 4.5, Math.sqrt(3.25)]]) {
    const x = g.many(N, draw);
    assert.ok(Math.abs(avg(x) - mu) < 4 * sd / Math.sqrt(N), `${name}: średnia ${avg(x)}`);
    assert.ok(Math.abs(sdv(x) / sd - 1) < 0.04, `${name}: SD ${sdv(x)}`);
  }
  const g1 = D.rng(1), g2 = D.rng(1);
  assert.deepEqual(Array.from(g1.many(3, g1.u)), Array.from(g2.many(3, g2.u)), "ziarno daje powtarzalny ciąg");
  // Gęstość jądrowa z odbiciem: pole ≈ 1, przy brzegu bez zapaści do połowy.
  const ux = g.many(4000, () => g.unif(0, 10)), k = D.kde(ux, 0, 10, 501, null, [0, 10]);
  const area = k.slice(1).reduce((s, p, i) => s + (p[0] - k[i][0]) * (p[1] + k[i][1]) / 2, 0);
  assert.ok(Math.abs(area - 1) < 0.02, `pole KDE ${area}`);
  assert.ok(k[0][1] > 0.08, "odbicie podnosi gęstość przy brzegu");
  // Histogram ggplot: wszystkie obserwacje w przedziałach, k przedziałów.
  const b = S.binsGgplot([0, 1, 2, 3, 9], 5);
  assert.equal(b.length, 5);
  assert.equal(b.reduce((s, r) => s + r.n, 0), 5);

  const P = env.window.lcCharts.palette();
  const run = (name, vals, step) => {
    const fig = F[name], state = {};
    if (fig.init) fig.init(state);
    fig.prepare(state, null, vals);
    return { fig, state, spec: k => fig.draw(null, P, k || 0, state, vals, step) };
  };
  // Ryc. 1.3: próba ciągła i dyskretna, warstwy włączane polami wyboru.
  let r = run("d03-emp", { ch1_emp_dist: "normal", ch1_emp_n: "200", ch1_show_hist: "on", ch1_show_density: "on" });
  assert.equal(r.state.data.length, 200);
  assert.deepEqual(Array.from(r.spec().layers, l => l.type), ["bins", "line"]);
  const first = r.state.data;
  r.fig.prepare(r.state, null, { ch1_emp_dist: "normal", ch1_emp_n: "200" });
  assert.equal(r.state.data, first, "te same ustawienia nie losują od nowa");
  r.fig.act(r.state, "new"); r.fig.prepare(r.state, null, { ch1_emp_dist: "normal", ch1_emp_n: "200" });
  assert.notEqual(r.state.data, first, "przycisk losuje nową próbę");
  r = run("d03-emp", { ch1_emp_dist: "poisson", ch1_emp_n: "400", ch1_show_hist: "off", ch1_show_density: "off" });
  assert.match(r.spec().aria, /Włącz/);
  // Ryc. 1.4: częstości sumują się do 1, podpis z największą różnicą.
  r = run("d03-freq", { ch1_scenario: "loaded", ch1_n_obs: "1000" });
  const bars = r.spec().layers[0].data;
  assert.equal(bars.length, 6);
  assert.ok(Math.abs(bars.reduce((s, q) => s + q[2], 0) - 1) < 1e-12);
  const diff = new Element("span", { "data-v": "diff" }), host = root("lc-sf", [diff]);
  r.fig.update(host, r.state);
  assert.match(diff.textContent, /^Maksymalna różnica między częstością a prawdopodobieństwem: 0\.\d{3}/);
  // Ryc. 3.1: k słupków dla monety i kostki 12-ściennej.
  assert.equal(run("d03-unif", { ch3_unif_type: "d12", ch3_unif_n: "500" }).spec().layers[0].data.length, 12);
  // Ryc. 4.1: kroki 1–7, stała oś X, krzywa w krokach 6–7.
  const steps = [1, 2, 3, 4, 5, 6, 7].map(s => run("d03-hist", { ch4_step_dist: "exp", ch4_step_n: "500" }, s));
  steps.forEach((h, i) => {
    const sp = h.spec();
    assert.ok(sp.layers.length > 0, `krok ${i + 1}`);
    if (i >= 1 && i <= 3) assert.equal(sp.layers[0].data.length, [5, 15, 30][i - 1]);
  });
  const kd = steps[6].spec().layers[0].data;
  assert.ok(kd.every(p => p[0] >= steps[6].state.frame.xlim[0] && p[1] >= 0));
  assert.match(F["d03-hist"].text(null, null, null, 5), /łączne pole słupków wynosi 1/);
  // Ryc. 6.4: zbieranie średnich, czyszczenie po zmianie n, odczyty.
  r = run("d03-ctg", { ch6_pop_dist: "exponential", ch6_sample_size: "5" });
  assert.match(r.spec(1).aria, /Pobierz/);
  r.fig.act(r.state, "take1000");
  assert.equal(r.state.means.length, 1000);
  const ctgLayers = Array.from(r.spec(1).layers, l => l.type);
  assert.deepEqual(ctgLayers, ["bins", "line", "vline"]);
  const sdOut = new Element("span", { "data-v": "sd" }), seOut = new Element("span", { "data-v": "se" });
  r.fig.update(root("lc-sf", [sdOut, seOut]), r.state);
  assert.equal(seOut.textContent, "0.894");
  assert.ok(Math.abs(Number(sdOut.textContent) - 0.894) < 0.08, `SD średnich ${sdOut.textContent}`);
  r.fig.act(r.state, "take1"); assert.equal(r.state.means.length, 1001);
  r.fig.prepare(r.state, null, { ch6_pop_dist: "exponential", ch6_sample_size: "10" });
  assert.equal(r.state.means.length, 0, "zmiana n czyści średnie");
  r.fig.act(r.state, "take1"); r.fig.act(r.state, "take1");
  assert.equal(r.spec(1).layers[0].type, "points", "kilka średnich jako kropki");
  for (const dist of ["uniform", "bimodal", "u_shape", "die"]) {
    const c = run("d03-ctg", { ch6_pop_dist: dist, ch6_sample_size: "5" });
    const sp = c.spec(0);
    assert.ok(sp.layers[0].data.every(p => p.every(Number.isFinite)), `${dist}: skończone punkty`);
  }
  assert.equal(env.errors.length, 0);
});

test("silnik lcSF: etykiety yLabels przy krańcach, kolejność warstw, tabela ze skrótami, binsWidth, akcja z kontenerem", () => {
  const env = environment();
  env.load("R/lc_dist.js"); env.load("R/lc_stat_figures.js");
  const S = env.window.lcSF, P = env.window.lcCharts.palette();
  const opt = S.option({ x: [0, 1], y: [0.5, 3.5], yLabels: { 1: "A", 2: "B", 3: "C" },
    layers: [{ type: "points", data: [[0.5, 1]], color: "#000", size: 15 },
             { type: "points", data: [[0.5, 1]], color: "#fff", size: 9 }] }, P, 12, 600);
  assert.equal(opt.yAxis.axisLabel.showMinLabel, true, "dolna kategoria ma etykietę");
  assert.ok(opt.series[1].z > opt.series[0].z, "późniejsza warstwa na wierzchu");
  assert.equal(opt.series[0].progressive, 0);
  const html = S.table([{ label: "Grupa", row: true }, { label: "Średnia", short: "x̄", desc: "średnia", digits: 1 }], [["A", 1.25]]);
  assert.match(html, /<div class="lc-tbl-block"><div class="lc-tbl-key"><span><b>x̄<\/b> średnia<\/span><\/div><table/);
  assert.match(html, /<abbr title="Średnia">x̄<\/abbr>/);
  // binsWidth jak geom_histogram(binwidth): domyślnie granice w połowie, z boundary = 0 na wielokrotnościach.
  const half = Array.from(S.binsWidth([5.93, 6.04, 6.06, 6.15], 0.1), b => [+b.l.toFixed(2), b.n]);
  assert.deepEqual(half, [[5.85, 1], [5.95, 1], [6.05, 2]], "granice 5.85, 5.95, 6.05, 6.15 (boundary w/2)");
  assert.equal(Array.from(S.binsWidth([5.93, 6.04, 6.06, 6.15], 0.1)).reduce((s, b) => s + b.n, 0), 4);
  const zero = Array.from(S.binsWidth([0.2, 1, 1.5, 3], 1, 0), b => [b.l, b.r, b.n]);
  assert.deepEqual(zero, [[0, 1, 2], [1, 2, 1], [2, 3, 1]], "przedziały (a, b], pierwszy z lewym końcem");
  // Akcja dostaje kontener ryciny.
  let got = null;
  S.register({ demo_act: { act: (state, a, rootEl) => { got = [a, rootEl]; }, draw: () => null } });
  const btn = new Element("button", { "data-stat-act": "go" });
  const fig = root("lc-sf", [btn, ecbox("0")], { "data-fig": "demo_act" });
  env.mount(fig);
  fig.emit("click", { target: btn });
  assert.equal(got[0], "go"); assert.equal(got[1], fig);
  env.unmount(fig);
  assert.equal(env.errors.length, 0);
});
