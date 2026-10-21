// Scena „Drugi telefon” (rozdział „Baza”, sekcja „Druga informacja”).
//
// Kontener .lc-dt[data-config]: dwa stosy alarmów pierwszego czujnika
// (cfg.tp1 prawdziwych, cfg.fp1 fałszywych), jedno pole = cfg.scale alarmów.
// Drugi czujnik („sito”) przepuszcza cfg.tp2 prawdziwych i cfg.fp2 fałszywych.
// Kadry (cfg.frames) mają pole step:
//   1 — stosy po pierwszym alarmie, 2 — pojawia się sito,
//   3 — przesiew: potwierdzone pola przechodzą na prawo, reszta gaśnie,
//   4 — po dwóch alarmach: zostają stosy po prawej i wiarygodność alarmu;
// oraz flagi: events — obok stosów lista zdarzeń, które wywołują fałszywe
// alarmy (cfg.events), missed — wyróżnione awarie odrzucone przez drugi czujnik.
// Przyciski [data-dt="prev|next"], odczyt kadru w .lc-dt-readout.
//
// To, które pola przechodzą przez sito, losuje deterministyczny generator
// (cfg.seed), więc scena wygląda tak samo przy każdym otwarciu.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var VW = 720, VH = 340;
  var PITCH = 16, SIZE = 13, COLS = 10;
  var LX = 24, RX = 506;              // lewe i prawe stosy
  var SX = 330, SW = 46;              // sito
  var TY = 50, FY = 132;              // górny wiersz stosu prawdziwych i fałszywych

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
  function pick(n, k, rnd) {
    var order = [];
    for (var i = 0; i < n; i++) order.push(i);
    for (i = n - 1; i > 0; i--) { var j = Math.floor(rnd() * (i + 1)), t = order[i]; order[i] = order[j]; order[j] = t; }
    var keep = new Array(n).fill(false);
    order.slice(0, k).forEach(function (i) { keep[i] = true; });
    return keep;
  }
  function fmt(x) { return String(x).replace(/\B(?=(\d{3})+(?!\d))/g, " "); }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var frames = cfg.frames || [], sc = cfg.scale || 1;
    var nT1 = Math.round(cfg.tp1 / sc), nF1 = Math.round(cfg.fp1 / sc);
    var nT2 = Math.round(cfg.tp2 / sc), nF2 = Math.round(cfg.fp2 / sc);
    var rnd = mulberry32(cfg.seed || 20261021);
    var keepT = pick(nT1, nT2, rnd), keepF = pick(nF1, nF2, rnd);

    var s = svg("svg", { viewBox: "0 0 " + VW + " " + VH, class: "lc-dt-svg", "aria-hidden": "true" });

    // Etykiety stosów: lewe (po pierwszym alarmie) i prawe (po dwóch).
    var lT = svg("text", { x: LX, y: TY - 12, class: "lc-dt-pile is-true is-left" }, s, "🚨 prawdziwe · " + fmt(cfg.tp1));
    var lF = svg("text", { x: LX, y: FY - 12, class: "lc-dt-pile is-false is-left" }, s, "🚨 fałszywe · " + fmt(cfg.fp1));
    svg("text", { x: RX, y: TY - 12, class: "lc-dt-pile is-true is-right" }, s, "🚨🚨 prawdziwe · " + fmt(cfg.tp2));
    svg("text", { x: RX, y: FY - 12, class: "lc-dt-pile is-false is-right" }, s, "🚨🚨 fałszywe · " + fmt(cfg.fp2));

    // Sito: drugi czujnik w ramie z kratką, przepustowość przy każdym stosie.
    var sieve = svg("g", { class: "lc-dt-sieve" }, s);
    svg("text", { x: SX + SW / 2, y: 18, class: "lc-dt-sieve-title", "text-anchor": "middle" }, sieve, "drugi czujnik");
    svg("text", { x: SX + SW / 2, y: 34, class: "lc-dt-sieve-sub", "text-anchor": "middle" }, sieve, cfg.sieve || "");
    svg("rect", { x: SX, y: 42, width: SW, height: FY + 10 * PITCH - 42 + 6, rx: 5, class: "lc-dt-frame" }, sieve);
    for (var gy = 50; gy < FY + 10 * PITCH; gy += 9) {
      svg("line", { x1: SX + 5, x2: SX + SW - 5, y1: gy, y2: gy, class: "lc-dt-mesh" }, sieve);
    }
    svg("text", { x: SX + SW + 10, y: TY + 20, class: "lc-dt-rate is-true" }, sieve, Math.round(cfg.sens * 100) + "%");
    svg("text", { x: SX + SW + 10, y: FY + 84, class: "lc-dt-rate is-false" }, sieve, Math.round(cfg.fpr * 100) + "%");

    // Skąd fałszywe alarmy: karty zdarzeń po prawej (kadr z flagą events).
    var ev = svg("g", { class: "lc-dt-events" }, s);
    svg("text", { x: 400, y: 40, class: "lc-dt-ev-title" }, ev, "fałszywy alarm czujnika 1, gdy:");
    (cfg.events || []).forEach(function (t, i) {
      var g = svg("g", { transform: "translate(400 " + (54 + i * 46) + ")" }, ev);
      svg("rect", { x: 0, y: 0, width: 300, height: 36, rx: 6, class: "lc-dt-ev-card" }, g);
      svg("text", { x: 14, y: 23, class: "lc-dt-ev-text" }, g, t);
    });

    // Awarie odrzucone przez drugi czujnik (kadr z flagą missed).
    svg("text", { x: LX, y: TY + 2 * PITCH + 14, class: "lc-dt-miss-lab" }, s,
      "obrysowane: " + fmt(cfg.tp1 - cfg.tp2) + " awarii odrzuconych");

    // Wiarygodność alarmu pod stosami (kadr 4).
    var yHit = FY + 10 * PITCH + 34;
    svg("text", { x: LX, y: yHit, class: "lc-dt-hit is-before" }, s,
      "P(A | 🚨) = " + cfg.ppv1);
    svg("text", { x: RX, y: yHit + 4, class: "lc-dt-hit is-after" }, s,
      "P(A | 🚨🚨) = " + cfg.ppv2);

    // Pola: lewe miejsce, a dla przepuszczonych miejsce w prawym stosie.
    var cells = [];
    function addPile(n, keep, y0, kind) {
      var k = 0;
      for (var i = 0; i < n; i++) {
        var x = LX + (i % COLS) * PITCH, y = y0 + Math.floor(i / COLS) * PITCH;
        var r = svg("rect", { x: x, y: y, width: SIZE, height: SIZE, rx: 2.5, class: "lc-dt-cell is-" + kind }, s);
        var c = { el: r, kind: kind, keep: keep[i], x: x, y: y };
        if (keep[i]) {
          c.px = RX + (k % COLS) * PITCH; c.py = y0 + Math.floor(k / COLS) * PITCH;
          c.delay = k; k++;
        }
        cells.push(c);
      }
    }
    addPile(nT1, keepT, TY, "true");
    addPile(nF1, keepF, FY, "false");
    // Przepuszczane pola na wierzchu, żeby w locie nie chowały się pod sitem.
    cells.forEach(function (c) { if (c.keep) s.appendChild(c.el); });
    root.querySelector(".lc-dt-stage").appendChild(s);

    var caption = root.querySelector(".lc-dt-caption"), readout = root.querySelector(".lc-dt-readout");
    var stage = root.querySelector(".lc-dt-stage");
    var nameEl = root.querySelector('[data-dt-out="name"]'), stepEl = root.querySelector('[data-dt-out="step"]');
    var prevBtn = root.querySelector('[data-dt="prev"]'), nextBtn = root.querySelector('[data-dt="next"]');
    var current = -1;

    function show(f) {
      var F = frames[f], step = F.step;
      cells.forEach(function (c) {
        var cls = "lc-dt-cell is-" + c.kind;
        if (step >= 3 && !c.keep) cls += " is-gone";
        if (step >= 4 && !c.keep) cls += F.missed && c.kind === "true" ? " is-missed" : " is-out";
        c.el.setAttribute("class", cls);
        var moved = step >= 3 && c.keep;
        c.el.style.transitionDelay = moved && f > current ? (c.delay * 0.04) + "s" : "0s";
        c.el.style.transform = moved ? "translate(" + (c.px - c.x) + "px," + (c.py - c.y) + "px)" : "";
      });
      s.classList.toggle("is-sieve", step >= 2);
      s.classList.toggle("is-passed", step >= 3);
      s.classList.toggle("is-final", step >= 4);
      s.classList.toggle("is-events", !!F.events);
      s.classList.toggle("is-missed", !!F.missed);
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

  function scan() { document.querySelectorAll(".lc-dt:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
