// Scena „Telefon o trzeciej” (sekcja „Alarm na zmianie”), bliźniacza z „Serpuchów, 1983”:
// po lewej panel czujnika w dojrzewalni, po prawej dyżurny i karty z założeniami.
//
// Kontener .lc-tf[data-config]: cfg.cards to karty {key, num, unit, text, term, tone},
// gdzie tone to "base" (częstość bazowa), "sens" (czułość), "fpr" (fałszywe alarmy)
// albo "ask" (pytanie). Kadry (cfg.frames) podają nazwę, tekst i listę widocznych kart.
// Przyciski [data-tf="prev|next"].
//
// Całość działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var VW = 720, VH = 340;
  var CX = 462, CW = 250, CH = 46, CY0 = 90, CDY = 58;
  // Temperatura w dojrzewalni: spokojny przebieg, pod koniec skok nad próg.
  var TEMP = [14.1, 14.0, 14.2, 14.1, 13.9, 14.0, 14.2, 14.3, 14.1, 14.2, 14.4, 14.6, 15.1, 15.9, 16.8];

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }

  function draw(root, cards) {
    var s = svg("svg", { viewBox: "0 0 " + VW + " " + VH, class: "lc-sp-svg", "aria-hidden": "true" });

    // Panel czujnika
    svg("rect", { x: 16, y: 16, width: 420, height: 290, rx: 10, class: "lc-sp-screen" }, s);
    svg("text", { x: 34, y: 42, class: "lc-sp-mono" }, s, "DOJRZEWALNIA 3");
    var status = svg("text", { x: 418, y: 42, "text-anchor": "end", class: "lc-sp-status is-alarm" }, s, "ALARM");
    svg("line", { x1: 30, y1: 52, x2: 422, y2: 52, class: "lc-sp-scanline" }, s);
    svg("text", { x: 226, y: 104, "text-anchor": "middle", class: "lc-tf-clock" }, s, "03:00");

    // Wykres temperatury z progiem alarmu
    var x0 = 50, x1 = 410, yTop = 140, yBot = 280, tMin = 13, tMax = 17.5;
    function ty(t) { return yBot - (t - tMin) / (tMax - tMin) * (yBot - yTop); }
    svg("line", { x1: x0, y1: ty(15.5), x2: x1, y2: ty(15.5), class: "lc-tf-limit" }, s);
    svg("text", { x: x0, y: ty(15.5) - 6, class: "lc-tf-axis" }, s, "próg 15.5 °C");
    var pts = TEMP.map(function (t, i) { return (x0 + i * (x1 - x0) / (TEMP.length - 1)) + "," + ty(t); }).join(" ");
    svg("polyline", { points: pts, class: "lc-tf-line" }, s);
    svg("circle", { cx: x1, cy: ty(TEMP[TEMP.length - 1]), r: 5, class: "lc-tf-dot" }, s);

    // Dyżurny z telefonem
    var duty = svg("g", { transform: "translate(587 40)" }, s);
    svg("circle", { cx: 0, cy: -8, r: 12, class: "lc-sp-face" }, duty);
    svg("path", { d: "M-20 30 q0 -24 20 -24 q20 0 20 24 z", class: "lc-sp-body" }, duty);
    svg("rect", { x: 10, y: -16, width: 7, height: 15, rx: 2, class: "lc-tf-phone" }, duty);

    var els = {};
    cards.forEach(function (c, i) {
      var g = svg("g", { class: "lc-tf-card is-" + c.tone, transform: "translate(" + CX + " " + (CY0 + i * CDY) + ")" }, s);
      svg("rect", { x: 0, y: 0, width: CW, height: CH, rx: 6 }, g);
      svg("text", { x: 34, y: 27, "text-anchor": "middle", class: "lc-tf-num" }, g, c.num);
      if (c.unit) svg("text", { x: 34, y: 40, "text-anchor": "middle", class: "lc-tf-unit" }, g, c.unit);
      svg("text", { x: 68, y: 21, class: "lc-tf-text" }, g, c.text);
      if (c.term) svg("text", { x: 68, y: 37, class: "lc-tf-term" }, g, c.term);
      els[c.key] = g;
    });

    root.querySelector(".lc-tf-stage").appendChild(s);
    return { status: status, cards: els };
  }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var frames = cfg.frames || [];
    var ui = draw(root, cfg.cards || []);
    var caption = root.querySelector(".lc-tf-caption"), stage = root.querySelector(".lc-tf-stage");
    var nameEl = root.querySelector('[data-tf-out="name"]'), stepEl = root.querySelector('[data-tf-out="step"]');
    var prevBtn = root.querySelector('[data-tf="prev"]'), nextBtn = root.querySelector('[data-tf="next"]');
    var current = -1;

    function show(f) {
      var F = frames[f], on = F.cards || [];
      Object.keys(ui.cards).forEach(function (k) { ui.cards[k].classList.toggle("is-on", on.indexOf(k) >= 0); });
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

  function scan() { document.querySelectorAll(".lc-tf:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
