// Scena „Dwie hale, ten sam czujnik” (rozdział „Częstość bazowa”, sekcja „Pułapka
// częstości bazowej”, przed krzywą).
//
// Kontener .lc-dh[data-config]: dwa dzienniki cfg.cols × cfg.rows zmian obok siebie,
// w tej samej skali. Dla każdej hali (cfg.halls) events awarii, z których hits kończy
// się alarmem, oraz false fałszywych alarmów wśród zmian bez awarii. Kadry (cfg.frames)
// mają pole step:
//   1 — dwa szare dzienniki, 2 — awarie, 3 — alarmy prawdziwe i fałszywe,
//   4 — reszta gaśnie, alarmy zjeżdżają w stosy pod każdą halą, przy hali
//       jedna liczba: wiarygodność alarmu P(A | 🚨).
// Przyciski [data-dh="prev|next"], odczyt kadru w .lc-dh-readout.
//
// Rozmieszczenie losuje deterministyczny generator (ziarno na halę), więc dzienniki
// wyglądają tak samo przy każdym otwarciu. Działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var VW = 860;
  var HX = [24, 456], GY = 40, PITCH = 7.6, SIZE = 6;   // dzienniki
  var PY = 392, PITCH_P = 12, SIZE_P = 10, PCOLS = 20;  // stosy telefonów

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
  function pileRows(k) { return Math.ceil(k / PCOLS); }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var cols = cfg.cols, rows = cfg.rows, n = cols * rows;
    var halls = cfg.halls || [], frames = cfg.frames || [];

    // Wysokość stosów: najwyższy stos wyznacza dół rysunku.
    var pileH = 0;
    halls.forEach(function (h) {
      pileH = Math.max(pileH, (pileRows(h.hits) + pileRows(h.false)) * PITCH_P + 26);
    });
    var VH = PY + pileH + 8;
    var s = svg("svg", { viewBox: "0 0 " + VW + " " + VH, class: "lc-dh-svg", "aria-hidden": "true" });
    var cells = [], movers = [];

    halls.forEach(function (h, hi) {
      var gx = HX[hi];
      svg("text", { x: gx - 4, y: GY - 14, class: "lc-dh-hall" }, s, h.name);
      svg("rect", { x: gx - 8, y: GY - 6, width: cols * PITCH + 14, height: rows * PITCH + 12, rx: 6, class: "lc-dh-book" }, s);

      // Losowanie bez zwracania: awarie, potem fałszywe alarmy wśród pozostałych.
      var rnd = mulberry32((cfg.seed || 20261021) + 7919 * (hi + 1)), order = [];
      for (var i = 0; i < n; i++) order.push(i);
      for (i = n - 1; i > 0; i--) { var j = Math.floor(rnd() * (i + 1)), t = order[i]; order[i] = order[j]; order[j] = t; }
      var kind = new Array(n).fill("calm");
      order.slice(0, h.events).forEach(function (k, idx) { kind[k] = idx < h.hits ? "hit" : "miss"; });
      order.slice(h.events, h.events + h.false).forEach(function (k) { kind[k] = "false"; });

      // Stosy pod halą: prawdziwe u góry, fałszywe pod nimi; obok jedna liczba.
      var yTrue = PY, yFalse = PY + pileRows(h.hits) * PITCH_P + 22;
      svg("text", { x: gx - 4, y: yTrue - 6, class: "lc-dh-late lc-dh-pile is-true" }, s, "🚨 prawdziwe · " + h.hits);
      svg("text", { x: gx - 4, y: yFalse - 6, class: "lc-dh-late lc-dh-pile is-false" }, s, "🚨 fałszywe · " + h.false);
      var cx = gx + PCOLS * PITCH_P + (cols * PITCH - PCOLS * PITCH_P) / 2 + 2;
      svg("text", { x: cx, y: PY + 14, class: "lc-dh-late lc-dh-ppv-label" }, s, "P(A | 🚨)");
      svg("text", { x: cx, y: PY + 56, class: "lc-dh-late lc-dh-ppv" }, s, h.ppv);

      var hitIdx = 0, falseIdx = 0;
      for (i = 0; i < n; i++) {
        var x = gx + (i % cols) * PITCH, y = GY + Math.floor(i / cols) * PITCH;
        var r = svg("rect", { x: x, y: y, width: SIZE, height: SIZE, rx: 1.1, class: "lc-dh-cell" }, s);
        var c = { el: r, kind: kind[i], x: x, y: y };
        if (c.kind === "hit") { c.px = gx + (hitIdx % PCOLS) * PITCH_P; c.py = yTrue + Math.floor(hitIdx / PCOLS) * PITCH_P; hitIdx++; }
        if (c.kind === "false") { c.px = gx + (falseIdx % PCOLS) * PITCH_P; c.py = yFalse + Math.floor(falseIdx / PCOLS) * PITCH_P; falseIdx++; }
        r.style.transformOrigin = x + "px " + y + "px";
        cells.push(c);
        if (c.px !== undefined) movers.push(c);
      }
    });
    // Komórki alarmów na wierzchu, żeby w locie nie chowały się pod innymi.
    movers.forEach(function (c) { s.appendChild(c.el); });

    var stage = root.querySelector(".lc-dh-stage");
    stage.appendChild(s);
    var caption = root.querySelector(".lc-dh-caption"), readout = root.querySelector(".lc-dh-readout");
    var nameEl = root.querySelector('[data-dh-out="name"]'), stepEl = root.querySelector('[data-dh-out="step"]');
    var prevBtn = root.querySelector('[data-dh="prev"]'), nextBtn = root.querySelector('[data-dh="next"]');
    var current = -1;

    function show(f) {
      var F = frames[f], step = F.step;
      cells.forEach(function (c) {
        var cls = "lc-dh-cell";
        if (step >= 2 && (c.kind === "hit" || c.kind === "miss")) cls += " is-event";
        if (step >= 3 && c.kind === "hit") cls += " is-hit";
        if (step >= 3 && c.kind === "false") cls += " is-false";
        if (step >= 4 && c.px === undefined) cls += " is-faded";
        c.el.setAttribute("class", cls);
        c.el.style.transform = step >= 4 && c.px !== undefined
          ? "translate(" + (c.px - c.x) + "px," + (c.py - c.y) + "px) scale(" + (SIZE_P / SIZE) + ")"
          : "";
      });
      s.classList.toggle("is-piles", step >= 4);
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

  function scan() { document.querySelectorAll(".lc-dh:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
