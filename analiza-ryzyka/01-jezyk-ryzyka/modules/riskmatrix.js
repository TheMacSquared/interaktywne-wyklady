// Schemat 1.2: macierz ryzyka 5 × 5 z dwoma problemami Bananpolu.
// Wiersze to kategorie prawdopodobieństwa z jawnymi granicami, kolumny to kategorie
// skutku. Poślizgnięcie (A) i kolizja z wózkiem (B) mają skutek jako zakres pól.
// Przełącznik horyzontu (zmiana / rok) przesuwa problemy w pionie; przełącznik
// „iloczyn pól” pokazuje numery p · s i dwa pola o tym samym iloczynie 10.
// Kontener: .lc-rm, przyciski [data-rm-h] i [data-rm-prod], opis .lc-rm-status.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 660, H = 440, GX = 210, GY = 24, CW = 84, CH = 66, N = 5;
  var P_LABELS = [
    ["rzadkie", "poniżej 0.01"], ["mało prawdopodobne", "0.01–0.05"],
    ["możliwe", "0.05–0.2"], ["prawdopodobne", "0.2–0.6"], ["prawie pewne", "powyżej 0.6"]
  ];
  var S_LABELS = ["nieistotny", "drobny", "poważny", "ciężki", "katastrofalny"];
  // Problemy: kategoria P w obu horyzontach, skutek jako zakres [od, do] i typowy.
  var ITEMS = [
    { key: "A", name: "poślizgnięcie", p: { shift: 0.08, year: 1 - Math.pow(0.92, 250) },
      s: [1, 3], typ: 2, cls: "is-a" },
    { key: "B", name: "kolizja z wózkiem", p: { shift: 0.002, year: 1 - Math.pow(0.998, 250) },
      s: [4, 5], typ: 4, cls: "is-b" }
  ];

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function pCat(p) { return p < 0.01 ? 1 : p < 0.05 ? 2 : p < 0.2 ? 3 : p < 0.6 ? 4 : 5; }
  function colX(s) { return GX + (s - 0.5) * CW; }
  function rowY(p) { return GY + (N - p + 0.5) * CH; }
  function zone(p, s) { var t = p + s; return t <= 4 ? "z1" : t <= 6 ? "z2" : t <= 8 ? "z3" : "z4"; }
  function fmtP(p) { return p > 0.995 ? "≈ 1" : p < 0.01 ? p.toFixed(3) : p.toFixed(2); }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var stage = root.querySelector(".lc-rm-stage"), status = root.querySelector(".lc-rm-status");
    var horizon = "shift", product = false;

    var s = svg("svg", { viewBox: "0 0 " + W + " " + H, class: "lc-rm-svg", role: "img",
      "aria-label": "Macierz ryzyka 5 na 5 z poślizgnięciem i kolizją z wózkiem" }, stage);
    var gGrid = svg("g", {}, s), gProd = svg("g", {}, s), gItems = svg("g", {}, s);

    // siatka i opisy osi (stałe)
    for (var p = 1; p <= N; p++) {
      for (var c = 1; c <= N; c++) {
        svg("rect", { x: GX + (c - 1) * CW, y: GY + (N - p) * CH, width: CW, height: CH,
          class: "lc-rm-cell " + zone(p, c) }, gGrid);
      }
      var y = GY + (N - p + 0.5) * CH;
      svg("text", { x: GX - 10, y: y - 2, "text-anchor": "end", class: "lc-rm-lab" }, gGrid,
        p + " · " + P_LABELS[p - 1][0]);
      svg("text", { x: GX - 10, y: y + 14, "text-anchor": "end", class: "lc-rm-range" }, gGrid,
        P_LABELS[p - 1][1]);
    }
    for (var c2 = 1; c2 <= N; c2++) {
      svg("text", { x: colX(c2), y: GY + N * CH + 18, "text-anchor": "middle", class: "lc-rm-lab" }, gGrid,
        String(c2));
      svg("text", { x: colX(c2), y: GY + N * CH + 34, "text-anchor": "middle", class: "lc-rm-range" }, gGrid,
        S_LABELS[c2 - 1]);
    }
    svg("text", { x: GX + N * CW / 2, y: H - 8, "text-anchor": "middle", class: "lc-rm-axis" }, gGrid, "Skutek");
    svg("text", { x: 14, y: GY + N * CH / 2, "text-anchor": "middle", class: "lc-rm-axis",
      transform: "rotate(-90 14 " + (GY + N * CH / 2) + ")" }, gGrid, "Prawdopodobieństwo w horyzoncie");

    // problemy: grupa przesuwana w pionie (CSS transition), zakres skutku jako odcinek
    var groups = ITEMS.map(function (it) {
      var g = svg("g", { class: "lc-rm-item " + it.cls }, gItems);
      var x1 = colX(it.s[0]) - 18, x2 = colX(it.s[1]) + 18, xt = colX(it.typ);
      svg("line", { x1: x1, x2: x2, y1: 0, y2: 0, class: "lc-rm-span" }, g);
      svg("line", { x1: x1, x2: x1, y1: -7, y2: 7, class: "lc-rm-span" }, g);
      svg("line", { x1: x2, x2: x2, y1: -7, y2: 7, class: "lc-rm-span" }, g);
      svg("circle", { cx: xt, cy: 0, r: 15, class: "lc-rm-dot" }, g);
      svg("text", { x: xt, y: 5, "text-anchor": "middle", class: "lc-rm-key" }, g, it.key);
      svg("text", { x: (x1 + x2) / 2, y: -22, "text-anchor": "middle", class: "lc-rm-name" }, g, it.name);
      return g;
    });

    function drawProduct() {
      gProd.textContent = "";
      if (!product) return;
      for (var p = 1; p <= N; p++) {
        for (var c = 1; c <= N; c++) {
          var x = GX + (c - 1) * CW, y = GY + (N - p) * CH, ten = p * c === 10;
          if (ten) svg("rect", { x: x + 2, y: y + 2, width: CW - 4, height: CH - 4, class: "lc-rm-ten" }, gProd);
          svg("text", { x: x + CW - 8, y: y + 16, "text-anchor": "end",
            class: "lc-rm-prod" + (ten ? " is-ten" : "") }, gProd, String(p * c));
        }
      }
    }

    function update() {
      ITEMS.forEach(function (it, i) {
        groups[i].style.transform = "translate(0px, " + rowY(pCat(it.p[horizon])) + "px)";
      });
      drawProduct();
      var a = ITEMS[0], b = ITEMS[1];
      var txt = horizon === "shift"
        ? "Jedna zmiana: poślizgnięcie P = " + fmtP(a.p.shift) + " (" + P_LABELS[pCat(a.p.shift) - 1][0] +
          "), kolizja P = " + fmtP(b.p.shift) + " (" + P_LABELS[pCat(b.p.shift) - 1][0] + ")."
        : "Rok, 250 niezależnych zmian: poślizgnięcie P " + fmtP(a.p.year) + " (" + P_LABELS[pCat(a.p.year) - 1][0] +
          "), kolizja P = " + fmtP(b.p.year) + " (" + P_LABELS[pCat(b.p.year) - 1][0] +
          "). Te same problemy przesuwają się w górę macierzy.";
      if (product) txt += " Pola (5, 2) i (2, 5) mają ten sam iloczyn 10, choć opisują zupełnie różne sytuacje.";
      status.textContent = txt;
    }

    root.addEventListener("click", function (e) {
      var h = e.target.closest("[data-rm-h]");
      if (h) {
        horizon = h.getAttribute("data-rm-h");
        root.querySelectorAll("[data-rm-h]").forEach(function (b) {
          b.setAttribute("aria-pressed", b === h ? "true" : "false");
        });
        update();
        return;
      }
      var t = e.target.closest("[data-rm-prod]");
      if (t) {
        product = !product;
        t.setAttribute("aria-pressed", product ? "true" : "false");
        update();
      }
    });
    update();
  }

  function scan() { document.querySelectorAll(".lc-rm:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
