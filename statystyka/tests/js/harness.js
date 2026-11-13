// Atrapa DOM, Chart.js i ECharts dla testów widgetów bez przeglądarki.
// Użycie: const { environment, Element, ecbox, root, canvas, press, statRoot } = require("./harness");
const assert = require("node:assert/strict");
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
  get parentElement() { return this.parent; }
  remove() {
    if (this.parent) this.parent.children = this.parent.children.filter(child => child !== this);
    this.parent = null;
  }
  focus() {}
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
      this.config = config;
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
    stop() { assert.ok(!this.destroyed); }
    destroy() { assert.ok(!this.destroyed, "powtórne destroy()"); this.destroyed = true; live.delete(this); }
  }
  window.Chart = Chart;
  // Atrapa ECharts: współrzędne piksel = wartość × 100, jak w atrapie Chart.js.
  class EChart {
    constructor(element, opts) {
      this.element = element; this.opts = opts; this.updates = 0;
      this.xAxis = {}; this.yAxis = {}; this.series = new Map();
      charts.push(this); live.add(this);
    }
    setOption(option) {
      assert.ok(!this.disposed, "setOption() na zwolnionym wykresie");
      this.updates++;
      const merge = (old, add) => Array.isArray(add)
        ? add.map((a, i) => ({ ...((Array.isArray(old) ? old[i] : i ? {} : old) || {}), ...a }))
        : Array.isArray(old) ? [{ ...old[0], ...add }, ...old.slice(1)] : { ...old, ...add };
      if (option.xAxis) this.xAxis = merge(this.xAxis, option.xAxis);
      if (option.yAxis) this.yAxis = merge(this.yAxis, option.yAxis);
      (option.series || []).forEach(s => this.series.set(s.id, { ...(this.series.get(s.id) || {}), ...s }));
    }
    convertToPixel(finder, value) { assert.ok(!this.disposed); return [value[0] * 100, value[1] * 100]; }
    convertFromPixel(finder, pixel) { assert.ok(!this.disposed); return [pixel[0] / 100, pixel[1] / 100]; }
    resize() { assert.ok(!this.disposed); }
    on(name, fn) { (this.events ||= {})[name] = fn; }
    dispatchAction(action) { assert.ok(!this.disposed); (this.actions ||= []).push(action); }
    getWidth() { return 1000; }
    getHeight() { return 500; }
    get data() { return Object.fromEntries([...this.series].map(([id, s]) => [id, s.data])); }
    get axisFont() { return (Array.isArray(this.xAxis) ? this.xAxis[0] : this.xAxis).axisLabel?.fontSize; }
    isDisposed() { return !!this.disposed; }
    dispose() { assert.ok(!this.disposed, "powtórne dispose()"); this.disposed = true; live.delete(this); }
  }
  const echarts = { init: (element, theme, opts) => new EChart(element, opts) };
  window.echarts = echarts;
  function connect() {
    window.Shiny = { addCustomMessageHandler(name, fn) { registrations++; handlers.set(name, fn); } };
    document.emit("shiny:connected");
  }
  const context = vm.createContext({
    document, window, Chart, echarts,
    console: { error: (...args) => errors.push(args) },
    getComputedStyle: () => ({ getPropertyValue: () => "" }),
    MutationObserver: class {
      constructor(fn) { this.fn = fn; }
      observe(node, options) { observers.push({ fn: this.fn, options }); }
      disconnect() {}
    },
    requestAnimationFrame(fn) { const id = ++sequence; frames.set(id, fn); return id; },
    cancelAnimationFrame(id) { frames.delete(id); },
    setTimeout(fn) { const id = ++sequence; timers.set(id, fn); return id; },
    clearTimeout(id) { timers.delete(id); }
  });
  function load(file) { vm.runInContext(fs.readFileSync(path.join(statRoot, file), "utf8"), context, { filename: file }); }
  function mutations() { observers.filter(o => o.options.childList).forEach(o => o.fn()); }
  function theme(target = documentElement) { observers.filter(o => o.options.attributes).forEach(o => o.fn([{ target }])); }
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
const ecbox = (role, attrs = {}) => new Element("div", { class: "lc-ec", "data-role": role, ...attrs });
const root = (cls, children, attrs = {}) => new Element("div", { class: cls, ...attrs }, children);
const press = (handle, key, shiftKey = false) => handle.emit("keydown", {
  key, shiftKey, preventDefault() {}, stopPropagation() {}
});

module.exports = { Element, environment, canvas, ecbox, root, press, statRoot };
