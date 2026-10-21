// Przykład „Prawdopodobieństwo alarmu w trzech sytuacjach” (rozdział
// „Wzór Bayesa”, sekcja „Od drzewa do Bayesa”, wzór 3.3).
//
// Kontener .lc-pc[data-config]: cfg.cases to sytuacje {key, short, name, text,
// sens, pA, fpr, pAc, termTp, termFp, pAlarm, countTp, countFp, countAlarm,
// population}; wszystkie liczby policzone i sformatowane w R (stop_calkowite.R).
// Każda sytuacja: drzewo z podświetlonymi drogami do alarmu [data-pc-tree]
// (gotowe z R) i wzór rozpisany w trzech linijkach [data-pc-out].
// Przyciski [data-pc-go="i"] (segment), [data-pc="prev|next"] i strzałki ← →.
//
// Działa w przeglądarce, bez serwera.
(function () {
  function reduced() {
    return window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var cases = cfg.cases || [], n = cases.length, last = n - 1;
    var prevBtn = root.querySelector('[data-pc="prev"]'), nextBtn = root.querySelector('[data-pc="next"]');
    var segBtns = root.querySelectorAll("[data-pc-go]");
    var current = -1;

    function setAll(sel, attr, src) {
      root.querySelectorAll(sel).forEach(function (e) {
        var v = src[e.getAttribute(attr)];
        if (v !== undefined) e.textContent = v;
      });
    }

    function show(i, animate) {
      var C = cases[i];
      if (!C) return;
      segBtns.forEach(function (b) { b.setAttribute("aria-pressed", String(+b.dataset.pcGo === i)); });
      var extra = { step: "Sytuacja " + (i + 1) + " z " + n };
      Object.keys(extra).forEach(function (k) { C[k] = extra[k]; });
      setAll("[data-pc-out]", "data-pc-out", C);
      root.querySelectorAll("[data-pc-tree]").forEach(function (t) { t.hidden = +t.dataset.pcTree !== i; });
      if (animate && !reduced()) {
        root.dataset.swap = "1";
        root.getBoundingClientRect();
        requestAnimationFrame(function () { root.dataset.swap = "0"; });
      }
      prevBtn.disabled = i === 0;
      nextBtn.disabled = i === last;
      current = i;
    }

    prevBtn.addEventListener("click", function () { if (current > 0) show(current - 1, true); });
    nextBtn.addEventListener("click", function () { if (current < last) show(current + 1, true); });
    segBtns.forEach(function (b) {
      b.addEventListener("click", function () { var i = +b.dataset.pcGo; if (i !== current) show(i, true); });
    });
    root.addEventListener("keydown", function (e) {
      if (e.target && /INPUT|TEXTAREA|SELECT/.test(e.target.tagName)) return;
      if (e.key === "ArrowLeft" && current > 0) { show(current - 1, true); e.preventDefault(); }
      if (e.key === "ArrowRight" && current < last) { show(current + 1, true); e.preventDefault(); }
    });
    show(0, false);
  }

  function scan() { document.querySelectorAll(".lc-pc:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
