// Scena „Dziennik dyżurnego” (rozdział „Kierunek warunku”, sekcja „Alarm na zmianie”).
//
// Kontener .lc-dz[data-config]: dziennik cfg.cols × cfg.rows zmian, w nim cfg.events
// awarii, z których cfg.hits kończy się alarmem, oraz cfg.false fałszywych alarmów
// wśród zmian bez awarii. Kadry (cfg.frames) mają pole step:
//   1 — sam dziennik, 2 — awarie, 3 — alarmy przy awarii,
//   4 — alarmy bez awarii, 5 — telefony zjeżdżają w dwa stosy.
// Przyciski [data-dz="prev|next"], odczyt kadru w .lc-dz-readout.
//
// Rozmieszczenie zmian losuje deterministyczny generator, więc dziennik
// wygląda tak samo przy każdym otwarciu. Działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var VW = 720, VH = 344;
  var GX = 16, GY = 14, PITCH = 8, SIZE = 6.4;      // siatka dziennika
  var PX = 488, PITCH_P = 21, SIZE_P = 17, PCOLS = 10; // stosy telefonów

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
    var cols = cfg.cols, rows = cfg.rows, n = cols * rows, frames = cfg.frames || [];

    // Losowanie bez zwracania: awarie, potem fałszywe alarmy wśród pozostałych.
    var rnd = mulberry32(cfg.seed || 20261021), order = [];
    for (var i = 0; i < n; i++) order.push(i);
    for (i = n - 1; i > 0; i--) { var j = Math.floor(rnd() * (i + 1)), t = order[i]; order[i] = order[j]; order[j] = t; }
    var kind = new Array(n).fill("calm");
    order.slice(0, cfg.events).forEach(function (k, idx) { kind[k] = idx < cfg.hits ? "hit" : "miss"; });
    order.slice(cfg.events, cfg.events + cfg.false).forEach(function (k) { kind[k] = "false"; });

    var s = svg("svg", { viewBox: "0 0 " + VW + " " + VH, class: "lc-dz-svg", "aria-hidden": "true" });
    svg("rect", { x: GX - 8, y: GY - 6, width: cols * PITCH + 14, height: rows * PITCH + 12, rx: 6, class: "lc-dz-book" }, s);
    svg("text", { x: PX, y: 34, class: "lc-dz-pile is-true" }, s, "prawdziwe alarmy · " + cfg.hits);
    svg("text", { x: PX, y: 102, class: "lc-dz-pile is-false" }, s, "fałszywe alarmy · " + cfg.false);

    var cells = [], hitIdx = 0, falseIdx = 0;
    for (i = 0; i < n; i++) {
      var x = GX + (i % cols) * PITCH, y = GY + Math.floor(i / cols) * PITCH;
      var r = svg("rect", { x: x, y: y, width: SIZE, height: SIZE, rx: 1.2, class: "lc-dz-cell" }, s);
      var c = { el: r, kind: kind[i], x: x, y: y };
      // Miejsce w stosie: prawdziwe u góry, fałszywe pod nimi.
      if (c.kind === "hit") { c.px = PX + (hitIdx % PCOLS) * PITCH_P; c.py = 44 + Math.floor(hitIdx / PCOLS) * PITCH_P; hitIdx++; }
      if (c.kind === "false") { c.px = PX + (falseIdx % PCOLS) * PITCH_P; c.py = 112 + Math.floor(falseIdx / PCOLS) * PITCH_P; falseIdx++; }
      r.style.transformOrigin = x + "px " + y + "px";
      cells.push(c);
    }
    // Komórki alarmów na wierzchu, żeby w locie nie chowały się pod innymi.
    cells.forEach(function (c) { if (c.px !== undefined) s.appendChild(c.el); });
    root.querySelector(".lc-dz-stage").appendChild(s);

    var caption = root.querySelector(".lc-dz-caption"), readout = root.querySelector(".lc-dz-readout");
    var stage = root.querySelector(".lc-dz-stage");
    var nameEl = root.querySelector('[data-dz-out="name"]'), stepEl = root.querySelector('[data-dz-out="step"]');
    var prevBtn = root.querySelector('[data-dz="prev"]'), nextBtn = root.querySelector('[data-dz="next"]');
    var current = -1;

    function show(f) {
      var F = frames[f], step = F.step;
      cells.forEach(function (c) {
        var cls = "lc-dz-cell";
        if (step >= 2 && (c.kind === "hit" || c.kind === "miss")) cls += " is-event";
        if (step >= 3 && c.kind === "hit") cls += " is-hit";
        if (step >= 4 && c.kind === "false") cls += " is-false";
        if (step >= 5 && c.px === undefined) cls += " is-faded";
        c.el.setAttribute("class", cls);
        c.el.style.transform = step >= 5 && c.px !== undefined
          ? "translate(" + (c.px - c.x) + "px," + (c.py - c.y) + "px) scale(" + (SIZE_P / SIZE) + ")"
          : "";
      });
      s.classList.toggle("is-piles", step >= 5);
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

  function scan() { document.querySelectorAll(".lc-dz:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
