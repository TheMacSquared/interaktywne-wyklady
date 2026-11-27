// ============================================================================
// Tryb zimowy — ozdoby (śnieżynki, choinka, prezenty, bombki).
// Widoczność ozdób steruje CSS (html[data-lc-theme=winter]), daty: season.js.
// ============================================================================
(function () {
  "use strict";

  function arm(angle) {
    return '<g transform="rotate(' + angle + ' 50 50)"><path d="M50 50V8M50 20L41 11M50 20L59 11M50 33L43 26M50 33L57 26"/></g>';
  }
  var SNOWFLAKE =
    '<svg viewBox="0 0 100 100" aria-hidden="true"><g fill="none" stroke="currentColor" stroke-width="5" stroke-linecap="round">' +
    arm(0) + arm(60) + arm(120) + arm(180) + arm(240) + arm(300) + '</g></svg>';
  var TREE =
    '<svg viewBox="0 0 100 124" aria-hidden="true">' +
    '<rect x="43" y="100" width="14" height="20" fill="#6b4423"/>' +
    '<path d="M50 14L24 50H76Z" fill="#1f7a54"/><path d="M50 36L16 78H84Z" fill="#1b6b49"/>' +
    '<path d="M50 60L8 106H92Z" fill="#175c3f"/>' +
    '<path d="M36 50C42 46 46 54 50 50C54 46 60 54 64 50L76 50H24Z" fill="#ffffff" opacity=".85"/>' +
    '<path d="M28 78C36 73 42 83 50 78C58 73 64 83 72 78L84 78H16Z" fill="#ffffff" opacity=".85"/>' +
    '<path d="M50 2L53 11L62 11L55 17L58 26L50 20L42 26L45 17L38 11L47 11Z" fill="#e0b13a"/>' +
    '<circle cx="40" cy="64" r="4" fill="#c0283a"/><circle cx="62" cy="68" r="4" fill="#e0b13a"/>' +
    '<circle cx="30" cy="94" r="4" fill="#e0b13a"/><circle cx="52" cy="90" r="4" fill="#c0283a"/>' +
    '<circle cx="72" cy="96" r="4" fill="#2a72aa"/><circle cx="50" cy="42" r="3.5" fill="#c0283a"/></svg>';
  function gift(box, ribbon) {
    return '<svg viewBox="0 0 100 92" aria-hidden="true">' +
      '<rect x="10" y="38" width="80" height="52" fill="' + box + '"/><rect x="5" y="26" width="90" height="16" fill="' + box + '"/>' +
      '<rect x="5" y="40" width="90" height="3" fill="#000000" opacity=".15"/>' +
      '<rect x="43" y="26" width="14" height="64" fill="' + ribbon + '"/>' +
      '<path d="M50 26C40 4 16 8 24 20C28 26 42 26 50 26ZM50 26C60 4 84 8 76 20C72 26 58 26 50 26Z" fill="' + ribbon + '"/></svg>';
  }
  function bauble(k) {
    return '<svg viewBox="0 0 60 86" aria-hidden="true">' +
      '<path d="M30 0V14" stroke="#5b6e7a" stroke-width="2"/><rect x="23" y="14" width="14" height="9" rx="2" fill="#e0b13a"/>' +
      '<circle cx="30" cy="52" r="28" fill="' + k + '"/>' +
      '<path d="M4 46C20 54 40 54 56 46" stroke="#ffffff" stroke-width="4" fill="none" opacity=".7"/>' +
      '<ellipse cx="20" cy="40" rx="5" ry="8" fill="#ffffff" opacity=".45" transform="rotate(28 20 40)"/></svg>';
  }

  function insertOrnaments() {
    if (document.querySelector(".lc-winter")) return;
    var el = function (cls, svg) { return '<span class="lc-winter-el ' + cls + '">' + svg + '</span>'; };
    var div = document.createElement("div");
    div.className = "lc-winter";
    div.setAttribute("aria-hidden", "true");
    div.innerHTML = el("s1", SNOWFLAKE) + el("s2", SNOWFLAKE) + el("s3", SNOWFLAKE) +
      el("t1", TREE) + el("g1", gift("#c0283a", "#e0b13a")) + el("g2", gift("#1f7a54", "#f9fcfe")) +
      el("b1", bauble("#c0283a")) + el("b2", bauble("#e0b13a"));
    document.body.appendChild(div);
  }

  // Żart: co kilkanaście minut postać wychyla się zza prawej krawędzi, zostaje
  // 2 sekundy i chowa się. Tylko w trybie zimowym, przy widocznej karcie
  // i gdy strona ma obrazek (zmienna --lc-winter-peek z lecture_page()).
  var PEEK_FIRST = [120, 300];    // sekundy do pierwszego wychylenia
  var PEEK_EVERY = [600, 900];    // sekundy między kolejnymi (10–15 min)
  var PEEK_HOLD = 2000;           // ms na ekranie
  function between(range) { return (range[0] + Math.random() * (range[1] - range[0])) * 1000; }

  function startPeek() {
    var root = document.documentElement;
    if (!getComputedStyle(root).getPropertyValue("--lc-winter-peek").trim()) return;
    var el = document.createElement("div");
    el.className = "lc-winter-peek";
    el.setAttribute("aria-hidden", "true");
    document.body.appendChild(el);

    function show() {
      if (root.getAttribute("data-lc-theme") !== "winter" || document.hidden) return;
      el.classList.add("is-out");
      setTimeout(function () { el.classList.remove("is-out"); }, 700 + PEEK_HOLD);
    }
    function peek() {
      show();
      setTimeout(peek, between(PEEK_EVERY));
    }
    window.lcWinterPeek = show;   // do ręcznego wywołania z konsoli
    setTimeout(peek, between(PEEK_FIRST));
  }

  function init() { insertOrnaments(); startPeek(); }
  if (document.readyState === "complete" || document.readyState === "interactive") {
    init();
  } else {
    document.addEventListener("DOMContentLoaded", init);
  }
})();
