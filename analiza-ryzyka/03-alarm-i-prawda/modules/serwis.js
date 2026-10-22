// Scena „Serwis szuka winnego” (sekcja „Więcej niż dwa stany”, przed wzorem 3.6).
//
// Kontener .lc-sw[data-config]: cfg.groups to podejrzane podzespoły
// ({ name, n, sym, color, prior, lik, post }) — n dawnych awarii w rejestrze,
// sym z nich z dzisiejszym objawem. Kadry (cfg.frames) mają pole step:
//   1 — rejestr (pola w grupach), 2 — udziały przed obserwacją,
//   3 — pola z objawem pełne, reszta przygaszona,
//   4 — pola bez objawu zostają jako blade kontury (ile było), pola z objawem
//       układają się od podstawy w tej samej siatce (ile zostało), udziały po obserwacji.
// Przyciski [data-sw="prev|next"], odczyt kadru w .lc-sw-readout.
//
// Rozmieszczenie objawów w grupie losuje deterministyczny generator, więc
// rejestr wygląda tak samo przy każdym otwarciu. Działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var VW = 760, VH = 222;
  var COLS = 10, PITCH = 22, SIZE = 18;   // siatka grupy
  var GX0 = 20, GW = COLS * PITCH, GAP = 30;
  var BASE = 130;                         // wspólna podstawa grup i słupków

  function mulberry32(a) {
    return function () {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      var t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }
  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var groups = cfg.groups || [], frames = cfg.frames || [];
    var rnd = mulberry32(cfg.seed || 20261021);

    var s = svg("svg", { viewBox: "0 0 " + VW + " " + VH, class: "lc-sw-svg", "aria-hidden": "true" });
    var cells = [], gEls = [];

    groups.forEach(function (G, gi) {
      var gx = GX0 + gi * (GW + GAP);
      var g = svg("g", { class: "lc-sw-g" }, s);
      g.style.setProperty("--sw-c", "var(--upwr-cat-" + G.color + ")");
      var rows = Math.ceil(G.n / COLS);
      var box = svg("rect", { x: gx - 7, y: BASE - rows * PITCH - 5, width: GW + 10, height: rows * PITCH + 10,
        rx: 6, class: "lc-sw-box" }, g);

      // Które pola mają objaw: losowanie bez zwracania w obrębie grupy.
      var order = [];
      for (var i = 0; i < G.n; i++) order.push(i);
      for (i = G.n - 1; i > 0; i--) { var j = Math.floor(rnd() * (i + 1)), t = order[i]; order[i] = order[j]; order[j] = t; }
      var sym = new Array(G.n).fill(false);
      order.slice(0, G.sym).forEach(function (k) { sym[k] = true; });

      // Pełne kontury grupy (kadr 4): ile było awarii, zanim zostały tylko te z objawem.
      for (i = 0; i < G.n; i++) {
        svg("rect", { x: gx + (i % COLS) * PITCH, y: BASE - (Math.floor(i / COLS) + 1) * PITCH + (PITCH - SIZE),
          width: SIZE, height: SIZE, rx: 3, class: "lc-sw-ghost" }, g);
      }
      var k = 0;
      for (i = 0; i < G.n; i++) {
        // Pola rosną od podstawy w górę, wiersz po wierszu.
        var x = gx + (i % COLS) * PITCH, y = BASE - (Math.floor(i / COLS) + 1) * PITCH + (PITCH - SIZE);
        var r = svg("rect", { x: x, y: y, width: SIZE, height: SIZE, rx: 3, class: "lc-sw-cell" }, g);
        var c = { el: r, sym: sym[i], x: x, y: y };
        if (c.sym) {
          // Ta sama siatka co rejestr, żeby wysokości po obserwacji porównywać z grupą.
          c.px = gx + (k % COLS) * PITCH;
          c.py = BASE - (Math.floor(k / COLS) + 1) * PITCH + (PITCH - SIZE);
          k++;
        }
        cells.push(c);
      }
      cells.forEach(function (c) { if (c.sym && c.el.parentNode === g) g.appendChild(c.el); });

      var cx = gx + GW / 2 - 2;
      svg("text", { x: cx, y: BASE + 26, class: "lc-sw-name" }, g, G.name);
      var num = svg("text", { x: cx, y: BASE + 70, class: "lc-sw-num" }, g, "");
      gEls.push({ G: G, box: box, num: num });
    });
    svg("line", { x1: GX0 - 10, x2: VW - GX0 + 6, y1: BASE + 2, y2: BASE + 2, class: "lc-sw-base" }, s);
    root.querySelector(".lc-sw-stage").appendChild(s);

    var caption = root.querySelector(".lc-sw-caption"), readout = root.querySelector(".lc-sw-readout");
    var stage = root.querySelector(".lc-sw-stage");
    var nameEl = root.querySelector('[data-sw-out="name"]'), stepEl = root.querySelector('[data-sw-out="step"]');
    var prevBtn = root.querySelector('[data-sw="prev"]'), nextBtn = root.querySelector('[data-sw="next"]');
    var topPost = Math.max.apply(null, groups.map(function (G) { return G.sym / (cfg.symTotal || 1); }));
    var current = -1;

    function show(f) {
      var F = frames[f], step = F.step;
      cells.forEach(function (c) {
        var cls = "lc-sw-cell";
        if (step >= 3 && !c.sym) cls += step >= 4 ? " is-gone" : " is-dim";
        c.el.setAttribute("class", cls);
        c.el.style.transform = step >= 4 && c.sym ? "translate(" + (c.px - c.x) + "px," + (c.py - c.y) + "px)" : "";
      });
      s.classList.toggle("is-final", step >= 4);
      gEls.forEach(function (E) {
        E.box.setAttribute("class", "lc-sw-box" + (step === 2 ? " is-on" : ""));
        var txt = "", cls = "lc-sw-num";
        if (step === 2) txt = E.G.prior;
        if (step === 3) { txt = E.G.lik; cls += " is-lik"; }
        if (step >= 4) { txt = E.G.post; if (E.G.sym / (cfg.symTotal || 1) === topPost) cls += " is-top"; }
        E.num.textContent = txt;
        E.num.setAttribute("class", cls);
      });
      if (nameEl) nameEl.textContent = F.name;
      if (stepEl) stepEl.textContent = "Kadr " + (f + 1) + " z " + frames.length;
      caption.textContent = F.text;
      readout.textContent = F.read || "";
      stage.setAttribute("aria-label", F.name + ". " + F.text);
      prevBtn.disabled = f === 0;
      nextBtn.disabled = f === frames.length - 1;
      current = f;
    }

    prevBtn.addEventListener("click", function () { if (current > 0) show(current - 1); });
    nextBtn.addEventListener("click", function () { if (current < frames.length - 1) show(current + 1); });
    show(0);
  }

  function scan() { document.querySelectorAll(".lc-sw:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
