// ============================================================================
// Tryby sezonowe — kalendarz. Jedno miejsce, w którym zapisane są daty.
//   window.lcSeason()        tryb sezonowy aktywny dziś ("winter") albo null
//   window.lcDomyslnyTryb()  tryb startowy, gdy nikt nie wybrał trybu przyciskiem
// Używają tego skrypt startowy w lecture_page() i przełącznik w shared_toc.js.
// Wygląd trybu jest w osobnych plikach (winter.css + winter.js, halloween.*).
// ============================================================================
(function () {
  "use strict";

  // from/to: miesiąc * 100 + dzień, oba końce włącznie. from > to oznacza
  // sezon przechodzący przez przełom roku.
  var SEASONS = [
    { theme: "winter", from: 1201, to: 106 }
    // Halloween wyłączone; żeby włączyć, odkomentuj wiersz i dodaj przycisk
    // w theme_control (lecture_layout.R):
    // , { theme: "halloween", from: 1020, to: 1102 }
  ];

  window.lcSeason = function (date) {
    var d = date || new Date();
    var md = (d.getMonth() + 1) * 100 + d.getDate();
    for (var i = 0; i < SEASONS.length; i++) {
      var s = SEASONS[i];
      var active = s.from <= s.to ? (md >= s.from && md <= s.to) : (md >= s.from || md <= s.to);
      if (active) return s.theme;
    }
    return null;
  };

  window.lcDomyslnyTryb = function () {
    return window.lcSeason() || "light";
  };
})();
