// Prototypy punktów stop wykładu 09, grupa wy (P10, P12, P16A, P16B, P18). Ryciny lcSF
// liczone w przeglądarce; kontenery w R: lc_sf_figure() w stop_proto_wy.R.
//
//   f9_suma        — P10 „Suma liczy narożnik dwa razy”: kwadrat d × s, pasy i narożnik.
//   f9_powtorzenie — P12 „Dwa wystąpienia, jedno źródło”: q, błędne OR i AND na jednej osi.
//   f9_ranking     — P16A „Ta sama redukcja każdego liścia”: trzy słupki spadku, oś 0–100%.
//   f9_pole        — P16B „Ryzyko jako pole prostokąta”: pole P(TOP), usunięty pas.
//   f9_przedzial   — P18 „Przedział zamiast punktu”: krzywa y(d) i pas d od 0.02 do 0.10.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, F = window.lcF9;
  if (!C || !S || !F) return;
  var U = F.util;

  // Stałe Bananpolu (jak f9p_pi, f9p_d, f9p_s, f9p_top w R); P16 bierze je z config.
  var DEF = { pi: 0.005, d: 0.05, s: 0.08, top: 0.00063 };

  // Krzywa z n+1 punktów na [a, b].
  function curve(f, a, b, n) {
    var out = [];
    for (var i = 0; i <= n; i++) { var x = a + (b - a) * i / n; out.push([x, f(x)]); }
    return out;
  }

  // Wynik z sześcioma miejscami po kropce (jak w tekstach wykładu).
  function f6(x) { return Number(x).toFixed(6); }
  // Procent całości, zaokrąglony do całości.
  function pct(part, whole) { return Math.round(100 * part / whole); }
  // Różnica z jawnym znakiem minus (U+2212), jak w tekstach wykładu.
  function minus6(x) { return "−" + f6(x); }

  S.register({
    // ---- P10: suma d + s liczy część wspólną d · s dwa razy ------------------------
    f9_suma: {
      draw: function (v, P, k, state, vals) {
        var d = Number(vals.f9p10_d), s = Number(vals.f9p10_s), top = 0.6;
        var sum = d + s, ex = 1 - (1 - d) * (1 - s), cor = d * s, err = (sum / ex - 1) * 100;
        state.sum = sum; state.ex = ex; state.cor = cor; state.err = err;
        return {
          x: [0, top], y: [0, top],
          xName: "Detekcja zawodzi (część przypadków)",
          yName: "Tłumienie zawodzi (część przypadków)",
          layers: [
            { type: "rect", x0: 0, x1: d, y0: 0, y1: top, fill: P.niebo, alpha: 0.3 },
            { type: "rect", x0: 0, x1: top, y0: 0, y1: s, fill: P.bursztyn, alpha: 0.3 },
            { type: "rect", x0: 0, x1: d, y0: 0, y1: s, fill: P.accent, alpha: 0.75 },
            { type: "text", x: d, y: s, text: "d · s = " + U.num(cor, 4), color: P.ink,
              align: "left", mono: true, halo: true }
          ],
          legend: [
            { text: "zawodzi detekcja", color: P.niebo, box: true },
            { text: "zawodzi tłumienie", color: P.bursztyn, box: true },
            { text: "zawodzą obie (liczone dwa razy)", color: P.accent, box: true }
          ],
          aria: "Kwadrat prawdopodobieństw dla d " + d + " i s " + s + ": pas detekcji, pas tłumienia i ich wspólny narożnik d · s."
        };
      },
      update: function (root, state) {
        S.setValue(root, "sum", U.num(state.sum, 4));
        S.setValue(root, "ex", U.num(state.ex, 4));
        S.setValue(root, "cor", U.num(state.cor, 4));
        S.setValue(root, "err", (state.err >= 0 ? "+" : "−") + Math.abs(state.err).toFixed(1) + "%");
      }
    },

    // ---- P12: powtórzony liść pod OR i pod AND -------------------------------------
    f9_powtorzenie: {
      draw: function (v, P, k, state, vals) {
        var q = Number(vals.f9p12_q), top = 0.2;
        var orv = 1 - (1 - q) * (1 - q), andv = q * q;
        state.q = q; state.orv = orv; state.andv = andv;
        return {
          x: [0, top], y: [0, 0.4],
          xName: "q = P(utraty wspólnego zasilania)",
          yName: "Wynik rachunku",
          layers: [
            { type: "vline", x: q, color: P.subtle, width: 1.4, dash: [3, 3] },
            { type: "line", data: curve(function (x) { return x; }, 0, top, 40), color: P.ink, width: 2.6 },
            { type: "line", data: curve(function (x) { return 1 - (1 - x) * (1 - x); }, 0, top, 40), color: P.bursztyn, width: 2.2 },
            { type: "line", data: curve(function (x) { return x * x; }, 0, top, 40), color: P.accent, width: 2.2 },
            { type: "point", x: q, y: q, color: P.ink, size: 9 },
            { type: "point", x: q, y: orv, color: P.bursztyn, size: 9 },
            { type: "point", x: q, y: andv, color: P.accent, size: 9 }
          ],
          legend: [
            { text: "poprawnie: q", color: P.ink },
            { text: "błędne OR: 1 − (1 − q)²", color: P.bursztyn },
            { text: "błędne AND: q²", color: P.accent }
          ],
          aria: "Trzy krzywe wyniku względem q: poprawna wartość q, błędne OR 1 − (1 − q)² i błędne AND q². Dla q = " + q + " wynik poprawny " + U.num(q, 4) + ", OR " + U.num(orv, 4) + ", AND " + U.num(andv, 4) + "."
        };
      },
      update: function (root, state) {
        var q = state.q;
        S.setValue(root, "q1", U.num(q, 2));
        S.setValue(root, "or", U.num(state.orv, 6) + " · ×" + U.num(state.orv / q, 2));
        S.setValue(root, "and", U.num(state.andv, 6) + " · " + Math.round(1 / q) + " razy za mało");
      }
    },

    // ---- P16A: ta sama względna redukcja r każdego liścia, słupki ------------------
    f9_ranking: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config || DEF, r = Number(vals.f9p16_r);
        var I = cfg.pi, d = cfg.d, s = cfg.s, top = cfg.top;
        // P(TOP) = I · (1 − (1 − d)(1 − s)); redukcja liścia o r zmniejsza jego parametr o r.
        function tp(d1, s1) { return I * (1 - (1 - d1) * (1 - s1)); }
        // Inicjacja: P(I) zmniejszone o r, więc spadek to r · top.
        var dI = top * r;
        var dS = top - tp(d, s * (1 - r));
        var dD = top - tp(d * (1 - r), s);
        state.dI = dI; state.dS = dS; state.dD = dD; state.top = top;
        var y = [dI, dS, dD].map(function (x) { return 100 * x / top; });
        state.y = y;
        return {
          x: [0.4, 3.6], y: [0, 100],
          xTicks: [1, 2, 3], xLabels: { 1: "Inicjacja", 2: "Tłumienie", 3: "Detekcja" },
          xName: "", yName: "Spadek P(TOP) (% wyjściowego 0.00063)",
          layers: [
            { type: "bins", data: [[0.6, 1.4, y[0]], [1.6, 2.4, y[1]], [2.6, 3.4, y[2]]],
              colors: [P.secondary, P.secondary, P.secondary], alpha: 0.9 },
            { type: "text", x: 1, y: y[0], text: f6(dI) + " · " + pct(dI, top) + "%", color: P.ink, align: "center", mono: true },
            { type: "text", x: 2, y: y[1], text: f6(dS) + " · " + pct(dS, top) + "%", color: P.ink, align: "center", mono: true },
            { type: "text", x: 3, y: y[2], text: f6(dD) + " · " + pct(dD, top) + "%", color: P.ink, align: "center", mono: true }
          ],
          aria: "Słupki spadku P(TOP) dla redukcji " + r + ": inicjacja " + pct(dI, top) + "%, tłumienie " + pct(dS, top) + "%, detekcja " + pct(dD, top) + "% wyjściowego P(TOP)."
        };
      },
      update: function (root, state) {
        S.setValue(root, "ri", f6(state.dI));
        S.setValue(root, "rs", f6(state.dS));
        S.setValue(root, "rd", f6(state.dD));
      }
    },

    // ---- P16B: P(TOP) jako pole prostokąta, usunięty pas po poprawie liścia --------
    f9_pole: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config || DEF, r = Number(vals.f9p16b_r), leaf = v;
        var I = cfg.pi, d = cfg.d, s = cfg.s;
        var W0 = 1 - (1 - d) * (1 - s), H0 = I, top = I * W0;
        // Pole zostające (w1 × h1) i usunięty obszar zależnie od liścia.
        var w1 = W0, h1 = H0;
        if (leaf === "I") { h1 = I * (1 - r); }
        else if (leaf === "S") { w1 = 1 - (1 - d) * (1 - s * (1 - r)); }
        else { w1 = 1 - (1 - d * (1 - r)) * (1 - s); }
        var after = w1 * h1, drop = top - after;
        state.after = after; state.drop = drop; state.top = top;
        var layers = [
          // Stan wyjściowy: obrys przerywany (cztery odcinki, bo prostokąt bez dash).
          { type: "line", data: [[0, H0], [W0, H0]], color: P.ref, width: 1.4, dash: [5, 4] },
          { type: "line", data: [[W0, 0], [W0, H0]], color: P.ref, width: 1.4, dash: [5, 4] },
          { type: "line", data: [[0, 0], [W0, 0]], color: P.ref, width: 1.4, dash: [5, 4] },
          { type: "line", data: [[0, 0], [0, H0]], color: P.ref, width: 1.4, dash: [5, 4] },
          // Pole, które zostaje.
          { type: "rect", x0: 0, x1: w1, y0: 0, y1: h1, fill: P.secondary, alpha: 0.35 }
        ];
        // Pole usunięte: pas u góry (inicjacja) albo pas po prawej (tłumienie, detekcja).
        if (leaf === "I") {
          layers.push({ type: "rect", x0: 0, x1: W0, y0: h1, y1: H0, fill: P.accent, alpha: 0.55 });
        } else {
          layers.push({ type: "rect", x0: w1, x1: W0, y0: 0, y1: H0, fill: P.accent, alpha: 0.55 });
        }
        if (r > 0) {
          var cx = leaf === "I" ? W0 / 2 : (w1 + W0) / 2;
          var cy = leaf === "I" ? (h1 + H0) / 2 : H0 / 2;
          layers.push({ type: "text", x: cx, y: cy, text: minus6(drop), color: P.ink, align: "center",
            valign: "middle", mono: true, halo: true });
        }
        return {
          x: [0, 0.14], y: [0, 0.006],
          xName: "P(D ∪ S | I): bariery zawodzą przy inicjacji",
          yName: "P(I): inicjacja w roku",
          layers: layers,
          legend: [
            { text: "zostaje", color: P.secondary, box: true },
            { text: "usunięte", color: P.accent, box: true },
            { text: "stan wyjściowy", color: P.ref, dash: true }
          ],
          aria: "Prostokąt P(TOP) " + f6(top) + " z usuniętym obszarem " + f6(drop) + " po poprawie liścia " + leaf + "; pozostaje " + f6(after) + "."
        };
      },
      update: function (root, state) {
        S.setValue(root, "after", f6(state.after));
        S.setValue(root, "drop", f6(state.drop));
        S.setValue(root, "part", pct(state.drop, state.top) + "%");
      }
    },

    // ---- P18: przedział P(TOP) z niepewnego d (statyczna) --------------------------
    f9_przedzial: {
      draw: function (v, P) {
        // y(d) = 10000 · 0.005 · (1 − (1 − d) · 0.92), na 10 000 magazyno-lat.
        function y(d) { return 10000 * 0.005 * (1 - (1 - d) * 0.92); }
        var lo = 0.02, hi = 0.10, mid = 0.05;
        return {
          x: [0, 0.15], y: [0, 12],
          xName: "d = P(braku detekcji | I)",
          yName: "P(TOP) na 10 000 magazyno-lat",
          layers: [
            { type: "band", lo: lo, hi: hi, color: P.bursztyn, alpha: 0.15 },
            { type: "line", data: [[0, y(0)], [0.15, y(0.15)]], color: P.secondary, width: 2.6 },
            { type: "line", data: [[0, y(lo)], [lo, y(lo)]], color: P.accent, width: 1, dash: [3, 3] },
            { type: "line", data: [[0, y(hi)], [hi, y(hi)]], color: P.accent, width: 1, dash: [3, 3] },
            { type: "point", x: lo, y: y(lo), color: P.accent, size: 10 },
            { type: "point", x: mid, y: y(mid), color: P.ink, size: 10 },
            { type: "point", x: hi, y: y(hi), color: P.accent, size: 10 },
            { type: "text", x: lo, y: y(lo), text: "0.000492", color: P.ink, align: "right", mono: true },
            { type: "text", x: mid, y: y(mid), text: "0.00063", color: P.ink, align: "right", mono: true },
            { type: "text", x: hi, y: y(hi), text: "0.00086", color: P.ink, align: "right", mono: true }
          ],
          aria: "Krzywa P(TOP) względem d: dla d od 0.02 do 0.10 wynik od 4.9 do 8.6 na 10 000 magazyno-lat, punkt nominalny 6.3 dla d = 0.05."
        };
      }
    }
  });
})();
