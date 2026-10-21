// Widget „Wzory na tablicy 2×2” (rozdział „Czułość i swoistość”).
//
// Kontener .lc-wz[data-config]: cfg.cells to komórki tablicy {key, abbr, n} w kolejności
// TP, FN, FP, TN; cfg.steps to kolejne wzory {name, symbol, num, den, value}, gdzie
// num i den to listy kluczy komórek. Komórki licznika są niebieskie, komórki mianownika
// mają żółte tło lub obrys; te same kolory mają części wzoru obok tablicy.
// Przyciski [data-wz="prev|next"].
//
// Działa w przeglądarce, bez serwera.
(function () {
  function el(tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined) e.textContent = text;
    return e;
  }
  function fmt(n) { return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, " "); }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var cells = {}, steps = cfg.steps || [];
    cfg.cells.forEach(function (c) { cells[c.key] = c; });

    var grid = root.querySelector(".lc-wz-grid");
    var cellEls = {};
    root.querySelectorAll("[data-wz-cell]").forEach(function (e) { cellEls[e.dataset.wzCell] = e; });
    var formula = root.querySelector(".lc-wz-formula");
    var nameEl = root.querySelector('[data-wz-out="name"]'), stepEl = root.querySelector('[data-wz-out="step"]');
    var prevBtn = root.querySelector('[data-wz="prev"]'), nextBtn = root.querySelector('[data-wz="next"]');
    var current = -1;

    // Suma komórek jako ciąg kolorowych żetonów: „TP + FN” albo „95 + 5”.
    function chips(keys, role, field) {
      var box = el("span", "lc-wz-sum");
      keys.forEach(function (k, i) {
        if (i > 0) box.appendChild(el("span", "lc-wz-op", "+"));
        var v = field === "n" ? fmt(cells[k].n) : cells[k].abbr;
        box.appendChild(el("span", "lc-wz-chip is-" + role, v));
      });
      return box;
    }
    function fraction(S, field) {
      var f = el("span", "lc-wz-frac");
      f.appendChild(chips(S.num, "num", field));
      f.appendChild(el("span", "lc-wz-bar"));
      f.appendChild(chips(S.den, "den", field));
      return f;
    }

    function show(i) {
      var S = steps[i];
      Object.keys(cellEls).forEach(function (k) {
        var e = cellEls[k];
        e.classList.toggle("is-num", S.num.indexOf(k) >= 0);
        e.classList.toggle("is-den", S.den.indexOf(k) >= 0);
        e.classList.toggle("is-off", S.num.indexOf(k) < 0 && S.den.indexOf(k) < 0);
      });
      grid.dataset.dir = S.dir || "";
      formula.innerHTML = "";
      var row1 = el("div", "lc-wz-row");
      row1.appendChild(el("span", "lc-wz-symbol", S.symbol));
      row1.appendChild(el("span", "lc-wz-eq", "="));
      row1.appendChild(fraction(S, "abbr"));
      var row2 = el("div", "lc-wz-row");
      row2.appendChild(el("span", "lc-wz-eq", "="));
      row2.appendChild(fraction(S, "n"));
      row2.appendChild(el("span", "lc-wz-eq", "≈"));
      row2.appendChild(el("span", "lc-wz-value", S.value));
      formula.appendChild(row1);
      formula.appendChild(row2);
      if (S.note) formula.appendChild(el("p", "lc-wz-note", S.note));
      if (nameEl) nameEl.textContent = S.name;
      if (stepEl) stepEl.textContent = "Wzór " + (i + 1) + " z " + steps.length;
      prevBtn.disabled = i === 0;
      nextBtn.disabled = i === steps.length - 1;
      current = i;
    }

    prevBtn.addEventListener("click", function () { if (current > 0) show(current - 1); });
    nextBtn.addEventListener("click", function () { if (current < steps.length - 1) show(current + 1); });
    show(0);
  }

  function scan() { document.querySelectorAll(".lc-wz:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
