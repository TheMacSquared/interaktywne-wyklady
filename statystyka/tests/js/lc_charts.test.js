// Testy cyklu życia bez przeglądarki: prawdziwe skrypty widgetów, kontrolowany DOM
// i atrapa renderera Chart.js. Uruchamianie: node statystyka/tests/js/lc_charts.test.js
const assert = require("node:assert/strict");
const { test } = require("node:test");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const statRoot = path.resolve(__dirname, "../..");

class Element {
  constructor(tag = "div", attrs = {}, children = []) {
    this.tagName = tag;
    this.attrs = { ...attrs };
    this.dataset = {};
    this.listeners = new Map();
    this.children = [];
    this.parent = null;
    this.connected = false;
    this.style = { setProperty() {} };
    this.value = attrs.value || "";
    this.classList = { contains: (cls) => (this.attrs.class || "").split(" ").includes(cls) };
    for (const [key, value] of Object.entries(attrs)) this.setAttribute(key, value);
    children.forEach(child => this.appendChild(child));
  }
  get isConnected() { return this.parent ? this.parent.isConnected : this.connected; }
  contains(node) { return this === node || this.children.some(child => child.contains(node)); }
  appendChild(child) { child.parent = this; this.children.push(child); return child; }
  setAttribute(key, value) {
    this.attrs[key] = String(value);
    if (key.startsWith("data-")) {
      const name = key.slice(5).replace(/-([a-z])/g, (_, c) => c.toUpperCase());
      this.dataset[name] = String(value);
    }
  }
  getAttribute(key) { return this.attrs[key] ?? null; }
  matches(selector) {
    const tag = selector.match(/^[a-z]+/);
    if (tag && tag[0] !== this.tagName) return false;
    const id = selector.match(/#([\w-]+)/);
    if (id && this.attrs.id !== id[1]) return false;
    for (const cls of selector.matchAll(/\.([\w-]+)/g)) if (!this.classList.contains(cls[1])) return false;
    for (const attr of selector.matchAll(/\[([\w-]+)(?:="([^"]*)")?\]/g)) {
      if (!(attr[1] in this.attrs)) return false;
      if (attr[2] !== undefined && this.attrs[attr[1]] !== attr[2]) return false;
    }
    return true;
  }
  querySelectorAll(selector) {
    const found = [];
    for (const child of this.children) {
      if (child.matches(selector)) found.push(child);
      found.push(...child.querySelectorAll(selector));
    }
    return found;
  }
  querySelector(selector) { return this.querySelectorAll(selector)[0] || null; }
  closest(selector) { return this.matches(selector) ? this : this.parent?.closest(selector) || null; }
  addEventListener(type, fn) {
    if (!this.listeners.has(type)) this.listeners.set(type, new Set());
    this.listeners.get(type).add(fn);
  }
  removeEventListener(type, fn) { this.listeners.get(type)?.delete(fn); }
  emit(type, event = {}) { for (const fn of this.listeners.get(type) || []) fn(event); }
  getBoundingClientRect() { return { left: 0, top: 0, width: 1000, height: 500 }; }
  setPointerCapture() {}
}

function environment(shiny = true) {
  const document = new Element("document"), window = new Element("window");
  const documentElement = new Element("html");
  document.appendChild(documentElement);
  document.connected = true;
  document.documentElement = documentElement;
  document.createElement = tag => new Element(tag);
  document.createTextNode = text => { const el = new Element("text"); el.textContent = text; return el; };
  const observers = [], charts = [], live = new Set(), errors = [], handlers = new Map();
  const frames = new Map(), timers = new Map();
  let sequence = 0, registrations = 0;
  class Chart {
    constructor(canvas, config) {
      this.canvas = canvas;
      this.data = config.data;
      this.options = config.options;
      this.chartArea = { left: 0, right: 1000, top: 0, bottom: 500 };
      this.updates = 0;
      this.scales = {};
      for (const key of Object.keys(this.options.scales || {})) {
        const options = this.options.scales[key];
        options.ticks ||= {};
        this.scales[key] = {
          get min() { return options.min ?? 0; }, get max() { return options.max ?? 1; },
          getPixelForValue: value => value * 100,
          getValueForPixel: pixel => pixel / 100
        };
      }
      charts.push(this);
      live.add(this);
    }
    update() { assert.ok(!this.destroyed, "aktualizacja zniszczonego wykresu"); this.updates++; }
    resize() { assert.ok(!this.destroyed); }
    destroy() { assert.ok(!this.destroyed, "powtórne destroy()"); this.destroyed = true; live.delete(this); }
  }
  window.Chart = Chart;
  function connect() {
    window.Shiny = { addCustomMessageHandler(name, fn) { registrations++; handlers.set(name, fn); } };
    document.emit("shiny:connected");
  }
  const context = vm.createContext({
    document, window, Chart,
    console: { error: (...args) => errors.push(args) },
    getComputedStyle: () => ({ getPropertyValue: () => "" }),
    MutationObserver: class {
      constructor(fn) { this.fn = fn; }
      observe(node, options) { observers.push({ fn: this.fn, options }); }
    },
    requestAnimationFrame(fn) { const id = ++sequence; frames.set(id, fn); return id; },
    cancelAnimationFrame(id) { frames.delete(id); },
    setTimeout(fn) { const id = ++sequence; timers.set(id, fn); return id; },
    clearTimeout(id) { timers.delete(id); }
  });
  function load(file) { vm.runInContext(fs.readFileSync(path.join(statRoot, file), "utf8"), context, { filename: file }); }
  function mutations() { observers.filter(o => o.options.childList).forEach(o => o.fn()); }
  function theme() { observers.filter(o => o.options.attributes).forEach(o => o.fn()); }
  function mount(root) { documentElement.appendChild(root); mutations(); }
  function unmount(root) {
    documentElement.children = documentElement.children.filter(child => child !== root);
    root.parent = null;
    mutations();
  }
  function frame(ts) { const pending = [...frames.values()]; frames.clear(); pending.forEach(fn => fn(ts)); }
  if (shiny) connect();
  load("R/lc_charts.js");
  return { load, mount, unmount, mutations, theme, frame, connect, document, window, charts, live,
    errors, frames, timers, handlers, observers, get registrations() { return registrations; } };
}

const canvas = (role, attrs = {}) => new Element("canvas", { "data-role": role, ...attrs });
const root = (cls, children, attrs = {}) => new Element("div", { class: cls, ...attrs }, children);

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
    const el = root("lc-lejek", [canvas("lejek")]);
    env.mount(el);
    assert.equal(env.live.size, 1);
    const ch = [...env.live][0];
    assert.equal(ch.canvas, el.children[0]);
    assert.equal(ch.data.datasets.length, 4);
    assert.equal(ch.data.datasets[3].data.length, 3);
    message({ series: [[0, 0.5, 0.33], [1, 0.5, 0.33]] });
    assert.equal(env.timers.size, 1, "nowa seria jest animowana");
    env.unmount(el);
    assert.equal(env.timers.size, 0);
    assert.equal(env.live.size, 0);
    message({ series: [[0, 0.5, 0.33]] });
  }
  message({ series: [] });
  env.mount(root("lc-lejek", [canvas("lejek")]));
  assert.equal([...env.live][0].data.datasets.length, 3, "reset działa także poza rozdziałem");
  assert.equal(env.errors.length, 0);
});

test("scenariusz koło/słupki obsługuje obie kolejności wiadomości i DOM", () => {
  for (const beforeDom of [false, true]) {
    const env = environment();
    env.load("01-typy-danych/modules/scenario_chart.js");
    const message = env.handlers.get("render_scenario");
    const data = { labels: ["A", "B"], data: [60, 40], colors: ["#aaa", "#bbb"], reveal: true };
    if (beforeDom) message(data);
    const makeRoot = () => root("lc-scenario", [canvas("pie", { id: "ch2_pie_canvas" }), canvas("bar", { id: "ch2_bar_canvas" })]);
    let el = makeRoot(); env.mount(el);
    if (!beforeDom) message(data);
    assert.equal(env.live.size, 2);
    message({ ...data, reveal: false }); assert.equal(env.live.size, 1);
    message(data); assert.equal(env.live.size, 2);
    env.unmount(el); assert.equal(env.live.size, 0);
    el = makeRoot(); env.mount(el);
    assert.equal(env.live.size, 2, "dane wracają bez kolejnej wiadomości z R");
    env.unmount(el);
    assert.equal(env.errors.length, 0);
  }
});

test("pole pod krzywą niszczy obsługę przeciągania i poprawnie wraca", () => {
  const env = environment();
  env.load("03-rozklady-prawdopodobienstwa/modules/area_chart.js");
  const el = root("lc-area", [canvas("pdf")]);
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

test("wszystkie migrowane widgety inicjują się i wracają z nową instancją", () => {
  const json = value => ({ "data-config": JSON.stringify(value) });
  const normal = { mu: 170, sd: 8, x: 178, step: 0.5, grp: "Grupa", unit: "cm" };
  const zInputs = () => ["mu", "sd", "x"].map((role, i) => new Element("input", { "data-role": role, value: [170, 8, 178][i] }));
  const pair = { a: normal, b: { ...normal, mu: 180 } };
  const fixtures = [
    ["01-typy-danych", "salary_chart", () => root("lc-salary", [canvas("hist"), canvas("strip")],
      json({ n: 20, shape: 3, scale: 100, shift: 1000, hi: 5000, bin: 100 })), 2],
    ["01-typy-danych", "percentile_chart", () => root("lc-pctl", [canvas("hist"), canvas("box")],
      json({ values: [150, 160, 165, 170, 175, 180, 190], p: 50, bin: 2 })), 2],
    ["01-typy-danych", "guess_chart", () => root("lc-guess", [canvas("hist")]), 1],
    ["01-typy-danych", "kurtosis_chart", () => root("lc-kurt", [canvas("main"), canvas("tail")]), 2],
    ["02-zwiazki-zmiennych", "scatter_charts", () => root("lc-scat lc-strength", [canvas("plot")],
      json({ x: [-1, 0, 1], e: [1, -2, 1], sigma: [0.5, 1, 2] })), 1],
    ["02-zwiazki-zmiennych", "scatter_charts", () => root("lc-scat lc-slope", [canvas("plot")],
      json({ x: [-1, 0, 1], e: [1, -2, 1], rel: 0.33, slopes: [0.5, 1, 2] })), 1],
    ["02-zwiazki-zmiennych", "scatter_charts", () => root("lc-scat lc-outlier", [canvas("plot")]), 1],
    ["02-zwiazki-zmiennych", "scatter_charts", () => root("lc-scat lc-simpson", [canvas("plot")],
      json({ x: [1, 2, 3, 4, 5, 6], y: [3, 2, 1, 6, 5, 4], g: [0, 0, 0, 1, 1, 1],
        groups: ["A", "B"], colors: ["--upwr-accent", "--upwr-reference"], xlim: [0, 10], ylim: [0, 10] })), 1],
    ["03-rozklady-prawdopodobienstwa", "cdf_chart", () => root("lc-cdf", [canvas("pdf"), canvas("cdf")]), 2],
    ["03-rozklady-prawdopodobienstwa", "quantile_chart", () => root("lc-quant", [canvas("pdf")]), 1],
    ["03-rozklady-prawdopodobienstwa", "normals_chart", () => root("lc-normals", [canvas("pdf")],
      { "data-mu1": 170, "data-sd1": 8, "data-mu2": 180, "data-sd2": 10 }), 1],
    ["03-rozklady-prawdopodobienstwa", "cases_chart", () => root("lc-cases", [canvas("pdf")],
      json([{ ...normal, t: 178, tail: "upper" }])), 1],
    ["03-rozklady-prawdopodobienstwa", "duel_chart", () => root("lc-duel", [canvas("a"), canvas("b"), canvas("z")],
      json([pair])), 3],
    ["03-rozklady-prawdopodobienstwa", "zscore_chart", () => root("lc-zcalc", [canvas("x"), canvas("z"), ...zInputs()]), 2],
    ["03-rozklady-prawdopodobienstwa", "zscore_chart", () => root("lc-zcalc", [canvas("x"), ...zInputs()], { "data-mode": "dual" }), 1]
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
