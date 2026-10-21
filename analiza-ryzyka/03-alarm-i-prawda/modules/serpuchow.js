// Scena „Serpuchów, 1983” (rozdział „Kierunek warunku”).
//
// Kontener .lc-sp[data-config] z listą kadrów (cfg.frames). Każdy kadr podaje liczbę
// sygnałów startu na ekranie (missiles), napis statusu (status, tone: "alarm" albo "calm"),
// karty widoczne po prawej (cards: klucze z CARDS), przygaszone karty (dim) i to,
// czy ekran pokazuje prawdziwą przyczynę: słońce odbite od chmur (reveal).
// Przyciski [data-sp="prev|next"].
//
// Całość działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var VW = 720, VH = 340;
  var BASES = [128, 180, 232, 284, 336];
  var CARDS = {
    proc: "Procedura: zameldować atak",
    nowy: "System działa od niedawna",
    setki: "Atak to setki rakiet, nie pięć",
    radar: "Radary naziemne: cisza",
    decyzja: "Melduje: awaria systemu"
  };
  var CARD_Y = { proc: 108, nowy: 152, setki: 196, radar: 240, decyzja: 288 };

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }

  function draw(root) {
    var s = svg("svg", { viewBox: "0 0 " + VW + " " + VH, class: "lc-sp-svg", "aria-hidden": "true" });

    // Ekran systemu Oko
    svg("rect", { x: 16, y: 16, width: 440, height: 290, rx: 10, class: "lc-sp-screen" }, s);
    svg("text", { x: 34, y: 42, class: "lc-sp-mono" }, s, "OKO");
    var status = svg("text", { x: 438, y: 42, "text-anchor": "end", class: "lc-sp-status" }, s, "");
    svg("line", { x1: 30, y1: 52, x2: 442, y2: 52, class: "lc-sp-scanline" }, s);

    // Horyzont: krzywizna Ziemi z bazami w USA
    var clip = svg("clipPath", { id: "lc-sp-clip" }, svg("defs", {}, s));
    svg("rect", { x: 16, y: 16, width: 440, height: 290, rx: 10 }, clip);
    var view = svg("g", { "clip-path": "url(#lc-sp-clip)" }, s);
    svg("circle", { cx: 232, cy: 640, r: 400, class: "lc-sp-earth" }, view);
    svg("text", { x: 232, y: 290, "text-anchor": "middle", class: "lc-sp-mono" }, view, "USA");

    // Satelita
    var sat = svg("g", { transform: "translate(92 86)" }, view);
    svg("rect", { x: -26, y: -5, width: 16, height: 10, class: "lc-sp-panel" }, sat);
    svg("rect", { x: 10, y: -5, width: 16, height: 10, class: "lc-sp-panel" }, sat);
    svg("rect", { x: -8, y: -8, width: 16, height: 16, rx: 2, class: "lc-sp-sat" }, sat);

    // Słońce, chmury i odbite promienie (tylko w ostatnim kadrze)
    var reveal = svg("g", { class: "lc-sp-reveal" }, view);
    svg("circle", { cx: 404, cy: 92, r: 18, class: "lc-sp-sun" }, reveal);

    var missiles = [];
    BASES.forEach(function (x, i) {
      var top = 168 - (i % 2) * 34;
      var g = svg("g", { class: "lc-sp-missile" }, view);
      svg("line", { x1: x, y1: 240, x2: x, y2: top + 14, class: "lc-sp-trail" }, g);
      svg("path", { d: "M" + x + " " + top + " l-7 17 h14 z", class: "lc-sp-tip" }, g);
      missiles.push(g);
      svg("ellipse", { cx: x, cy: top + 6, rx: 25, ry: 11, class: "lc-sp-cloud" }, reveal);
      svg("line", { x1: 404, y1: 92, x2: x, y2: top + 6, class: "lc-sp-ray" }, reveal);
      svg("line", { x1: x, y1: top + 6, x2: 92, y2: 86, class: "lc-sp-ray" }, reveal);
    });

    var count = svg("text", { x: 236, y: 330, "text-anchor": "middle", class: "lc-sp-count" }, s, "");

    // Dyżurny i jego karty
    var duty = svg("g", { transform: "translate(585 54)" }, s);
    svg("circle", { cx: 0, cy: -14, r: 13, class: "lc-sp-face" }, duty);
    svg("path", { d: "M-22 30 q0 -26 22 -26 q22 0 22 26 z", class: "lc-sp-body" }, duty);
    svg("text", { x: 0, y: 48, "text-anchor": "middle", class: "lc-sp-who" }, duty, "dyżurny oficer");

    var cards = {};
    Object.keys(CARDS).forEach(function (k) {
      var g = svg("g", { class: "lc-sp-card is-" + k, transform: "translate(472 " + CARD_Y[k] + ")" }, s);
      svg("rect", { x: 0, y: 0, width: 236, height: 34, rx: 6 }, g);
      svg("text", { x: 118, y: 22, "text-anchor": "middle" }, g, CARDS[k]);
      cards[k] = g;
    });

    root.querySelector(".lc-sp-stage").appendChild(s);
    return { status: status, missiles: missiles, reveal: reveal, count: count, cards: cards, screen: s };
  }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var frames = cfg.frames || [];
    var ui = draw(root);
    var caption = root.querySelector(".lc-sp-caption");
    var stage = root.querySelector(".lc-sp-stage");
    var nameEl = root.querySelector('[data-sp-out="name"]');
    var stepEl = root.querySelector('[data-sp-out="step"]');
    var prevBtn = root.querySelector('[data-sp="prev"]'), nextBtn = root.querySelector('[data-sp="next"]');
    var current = -1;

    function show(f) {
      var F = frames[f];
      ui.missiles.forEach(function (m, i) {
        m.classList.toggle("is-on", i < (F.missiles || 0));
        m.classList.toggle("is-ghost", !!F.reveal);
      });
      ui.reveal.classList.toggle("is-on", !!F.reveal);
      ui.status.textContent = F.status || "";
      ui.status.setAttribute("class", "lc-sp-status is-" + (F.tone || "alarm"));
      ui.count.textContent = F.missiles ? "Sygnały startu: " + F.missiles : "";
      Object.keys(ui.cards).forEach(function (k) {
        var on = (F.cards || []).indexOf(k) >= 0;
        ui.cards[k].classList.toggle("is-on", on);
        ui.cards[k].classList.toggle("is-dim", on && (F.dim || []).indexOf(k) >= 0);
      });
      if (nameEl) nameEl.textContent = F.name;
      if (stepEl) stepEl.textContent = "Kadr " + (f + 1) + " z " + frames.length;
      caption.textContent = F.text;
      stage.setAttribute("aria-label", F.name + ". " + F.text);
      prevBtn.disabled = f === 0;
      nextBtn.disabled = f === frames.length - 1;
      current = f;
    }

    prevBtn.addEventListener("click", function () { if (current > 0) show(current - 1); });
    nextBtn.addEventListener("click", function () { if (current < frames.length - 1) show(current + 1); });
    show(0);
  }

  function scan() { document.querySelectorAll(".lc-sp:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
