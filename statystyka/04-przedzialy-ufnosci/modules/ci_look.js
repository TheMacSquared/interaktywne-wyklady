// Wspólny wygląd przedziałów ufności w rycinach lcSF wykładu 04 (2026-11-20), wzorowany
// na Ryc. 3.2–4.3 (fig_32.js, fig_41.js): pomiary jako stos kropek, estymata jako gruba
// czarna kreska, ± SE jako czarna klamra (tylko w swoim kroku), przedział jako burgundowa
// klamra z granicami po bokach. Funkcje zwracają warstwy silnika lcSF (bracket, tick,
// dotstack, text — shared/lc_stat_figures.js). Ładowany przed sf_*.js.
(function () {
  "use strict";
  function num(v, d) { return Number(v).toFixed(d == null ? 2 : d); }
  // Liczba przy klamrze: pogrubiona, monospace, z tłem pod napisem.
  function value(x, y, text, color, o) {
    o = o || {};
    return { type: "text", x: x, y: y, text: text, color: color, mono: true, bold: true, halo: true,
             size: o.size || 1.25, align: o.align, valign: o.valign || "middle", minW: o.minW, fit: o.fit };
  }
  var L = {
    num: num,
    value: value,
    // Klamra przedziału: belka na y, nóżki w dół, granice po bokach (o.labels === false: bez nich).
    ci: function (lo, hi, y, P, o) {
      o = o || {};
      var color = o.color || P.accent, d = o.d == null ? 2 : o.d, out = [
        { type: "bracket", lo: lo, hi: hi, y: y, color: color, width: o.width || 4, leg: o.leg, alpha: o.alpha, z: o.z }];
      if (o.labels !== false) {
        var f = o.fmt || function (v) { return num(v, d); };
        // Granica przy krawędzi wykresu staje nad końcem klamry (fit). Na telefonie (wykres
        // węższy niż 340 px) zostaje sama klamra: liczby są w tekście i odczytach.
        var minW = o.minW == null ? 340 : o.minW;
        out.push(value(lo, y, f(lo), color, { align: "right", size: o.size, minW: minW, fit: true }),
                 value(hi, y, f(hi), color, { align: "left", size: o.size, minW: minW, fit: true }));
      }
      return out;
    },
    // Klamra ± SE: czarna, nóżki w obie strony; napis nad środkiem klamry (po prawej
    // ucinał się przy krawędzi wykresu), o.side = "right": obok prawej nóżki.
    se: function (lo, hi, y, P, label, o) {
      o = o || {};
      var out = [{ type: "bracket", lo: lo, hi: hi, y: y, color: P.ink, width: 3, leg: o.leg || 16, sym: true }];
      if (label) out.push(o.side === "right"
        ? value(hi, y, label, P.ink, { align: "left", size: o.size || 1.1 })
        : value((lo + hi) / 2, y + (o.lift || 0), label, P.ink, { valign: "bottom", size: o.size || 1.1 }));
      return out;
    },
    // Estymata: gruba czarna kreska od y0 do y1 (jednostki danych).
    est: function (x, y0, y1, P) {
      return { type: "vline", x: x, from: y0, to: y1, color: P.ink, width: 3.5 };
    },
    // Estymata w wierszu (np. forest): kreska o stałej długości w px wokół y.
    mark: function (x, y, P, up, down) {
      return { type: "tick", x: x, y: y, up: up == null ? 10 : up, down: down == null ? 18 : down, color: P.ink, width: 3.5 };
    },
    // Napis estymaty nad kreską („x̄ = 171.04”).
    label: function (x, y, text, P, o) {
      o = o || {};
      return value(x, y, text, o.color || P.ink, { valign: "bottom", size: o.size || 1.25, align: o.align });
    },
    dots: function (xs, y, color, o) {
      o = o || {};
      return { type: "dotstack", data: xs, y: y, r: o.r || 5, color: color, colors: o.colors, alpha: o.alpha };
    }
  };
  window.lc04Look = L;
})();
