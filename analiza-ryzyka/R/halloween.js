// ============================================================================
// Tryb halloweenowy — ozdoby (nietoperze, dynie, cukierki) i domyślny tryb
// od 20.10 do 2.11. Widoczność ozdób steruje CSS (html[data-lc-theme=halloween]).
// ============================================================================
(function () {
  "use strict";

  // Domyślny tryb, gdy nikt nie wybrał trybu przyciskiem (używa shared_toc.js
  // i skrypt startowy w lecture_page()).
  window.lcDomyslnyTryb = function () {
    var d = new Date();
    var md = (d.getMonth() + 1) * 100 + d.getDate();
    return (md >= 1020 && md <= 1102) ? "halloween" : "light";
  };

  var NIETOPERZ =
    '<svg viewBox="0 0 100 50" aria-hidden="true"><path fill="currentColor" d="M50 18C46 10 42 12 40 16C34 6 20 4 4 10' +
    'C12 14 14 22 12 30C18 26 24 28 28 34C32 28 38 28 42 32C44 30 46 30 50 34C54 30 56 30 58 32C62 28 68 28 72 34' +
    'C76 28 82 26 88 30C86 22 88 14 96 10C80 4 66 6 60 16C58 12 54 10 50 18Z"/>' +
    '<path fill="currentColor" d="M45 15L46 7L49 13ZM55 15L54 7L51 13Z"/>' +
    '<circle cx="47" cy="19" r="1.6" fill="#ffb02e"/><circle cx="53" cy="19" r="1.6" fill="#ffb02e"/></svg>';
  var DYNIA =
    '<svg viewBox="0 0 100 90" aria-hidden="true">' +
    '<path d="M50 18C52 8 56 4 62 2" stroke="#4d7c0f" stroke-width="6" fill="none" stroke-linecap="round"/>' +
    '<ellipse cx="30" cy="54" rx="26" ry="32" fill="#ea580c"/><ellipse cx="70" cy="54" rx="26" ry="32" fill="#ea580c"/>' +
    '<ellipse cx="50" cy="54" rx="24" ry="34" fill="#f97316"/>' +
    '<path d="M50 22V86M30 24C22 40 22 70 30 84M70 24C78 40 78 70 70 84" stroke="#c2410c" stroke-width="2" fill="none"/>' +
    '<path d="M30 44L40 40L38 52ZM70 44L60 40L62 52Z" fill="#3b1d06"/>' +
    '<path d="M26 62L34 66L40 62L46 68L54 68L60 62L66 66L74 62C70 76 30 76 26 62Z" fill="#3b1d06"/></svg>';
  function cukierek(k) {
    return '<svg viewBox="0 0 80 40" aria-hidden="true"><path d="M22 20L4 6L8 20L4 34Z" fill="' + k + '"/>' +
      '<path d="M58 20L76 6L72 20L76 34Z" fill="' + k + '"/><circle cx="40" cy="20" r="15" fill="' + k + '"/>' +
      '<path d="M30 9L48 33M38 5L54 27M26 16L40 35" stroke="#ffffff" stroke-width="3" opacity=".75"/></svg>';
  }
  var KUKURYDZA =
    '<svg viewBox="0 0 40 50" aria-hidden="true"><path d="M20 2C26 2 38 40 36 46C32 49 8 49 4 46C2 40 14 2 20 2Z" fill="#fde047"/>' +
    '<path d="M10 25C16 23 24 23 30 25L34 38C26 36 14 36 6 38Z" fill="#f97316"/>' +
    '<path d="M16 8C18 6 22 6 24 8L27 16C22 15 18 15 13 16Z" fill="#ffffff"/></svg>';

  function wstawOzdoby() {
    if (document.querySelector(".lc-deko")) return;
    var el = function (kl, svg) { return '<span class="lc-deko-el ' + kl + '">' + svg + '</span>'; };
    var div = document.createElement("div");
    div.className = "lc-deko";
    div.setAttribute("aria-hidden", "true");
    div.innerHTML = el("n1", NIETOPERZ) + el("n2", NIETOPERZ) + el("n3", NIETOPERZ) +
      el("d1", DYNIA) + el("d2", DYNIA) +
      el("c1", cukierek("#a855f7")) + el("c2", cukierek("#16a34a")) + el("k1", KUKURYDZA);
    document.body.appendChild(div);
  }

  if (document.readyState === "complete" || document.readyState === "interactive") {
    wstawOzdoby();
  } else {
    document.addEventListener("DOMContentLoaded", wstawOzdoby);
  }
})();
