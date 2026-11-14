// Prototypy punktów stop wykładu 08, grupa RE (ryciny lcSF) — ECharts liczony w przeglądarce.
// Kontenery w R: lc_sf_figure() w stop_proto_re.R.
//
//   s8_horyzont     — Piaskownica „Wspólny czas misji” (P4B): wentylator, czujnik i system w czasie.
//   s8_kwadrat      — Piaskownica „Kwadrat dwóch elementów” (P9A): pola R₁ × R₂ dla szeregu i równoległego.
//   s8_architektury — Piaskownica „Te same R, dwa systemy” (P9B): elementy, szereg i układ równoległy.
//   s8_czas         — Piaskownica „System pod najsłabszym szeregiem” (P11A): wentylatory A, B i sterownik C.
//   s8_przewaga     — Piaskownica „Ile daje druga gałąź” (P11B): ryzyko bloku wentylatorów z drugą gałęzią.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcS8;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Krzywa jako [[x, f(x)]…] co krok (stała siatka, bez wygładzania).
  function curve(f, a, b, step) {
    var out = [];
    for (var x = a; x <= b + 1e-9; x += step) out.push([x, f(x)]);
    return out;
  }
  // Punkt na krzywej w chwili t.
  function dot(x, y, color, size) { return { type: "point", x: x, y: y, color: color, size: size || 8 }; }

  // Modele P4B: wentylator 0.92 na 1000 h, czujnik 0.95 na 8760 h (wykres pokazuje liczby z karty).
  function fanR(t) { return Math.pow(0.92, t / 1000); }
  function sensR(t) { return Math.pow(0.95, t / 8760); }

  // Modele P11 (przykład 8.6): MTTF 1800, 2000 i 2500 h.
  function ra(t) { return U.expR(t, 1800); }
  function rb(t) { return U.expR(t, 2000); }
  function rc(t) { return U.expR(t, 2500); }
  function rsys(t) { return rc(t) * (1 - (1 - ra(t)) * (1 - rb(t))); }

  S.register({
    // P4B — wspólny czas misji. Dwa stałe punkty z karty w różnych chwilach.
    s8_horyzont: {
      draw: function (v, P, k, state, vals) {
        var t = Number(vals.s8p4b_t);
        state.t = t;
        var fan = fanR(t), sens = sensR(t), sys = fan * sens;
        return {
          x: [0, 9000], y: [0.4, 1.02], xLabelEvery: 1000,
          layers: [
            { type: "line", data: curve(fanR, 0, 9000, 50), color: P.secondary, width: 2 },
            { type: "line", data: curve(sensR, 0, 9000, 50), color: P.szalwia, width: 2 },
            { type: "line", data: curve(function (x) { return fanR(x) * sensR(x); }, 0, 9000, 50), color: P.accent, width: 3 },
            { type: "point", x: 1000, y: 0.92, color: P.ink, size: 11 },
            { type: "point", x: 8760, y: 0.95, color: P.ink, size: 11 },
            { type: "text", x: 1000, y: 0.92, text: "karta: 0.92", color: P.ink, mono: true, halo: true, align: "left", valign: "middle" },
            { type: "text", x: 8760, y: 0.95, text: "karta: 0.95", color: P.ink, mono: true, halo: true, align: "right", valign: "middle" },
            { type: "vline", x: t, color: P.ink, width: 1.5, dash: "dashed" },
            dot(t, fan, P.secondary, 8),
            dot(t, sens, P.szalwia, 8),
            dot(t, sys, P.accent, 9)
          ],
          legend: [
            { text: "wentylator", color: P.secondary },
            { text: "czujnik", color: P.szalwia },
            { text: "system (wymaga obu)", color: P.accent }
          ],
          xName: "Czas misji (h)", yName: "R(t)",
          aria: "Wentylator " + U.fmt(fan, 3) + ", czujnik " + U.fmt(sens, 3) + ", system " + U.fmt(sys, 3) + " przy t = " + t + " h."
        };
      },
      update: function (root, state) {
        var t = state.t, fan = fanR(t), sens = sensR(t);
        S.setValue(root, "fan", U.fmtP(fan));
        S.setValue(root, "sens", U.fmtP(sens));
        S.setValue(root, "sys", U.fmtP(fan * sens));
      }
    },

    // P9A — kwadrat dwóch elementów: pole = prawdopodobieństwo, układ przesuwa tylko działanie systemu.
    s8_kwadrat: {
      draw: function (v, P, k, state, vals) {
        var r1 = Number(vals.s8p9a_r1), r2 = Number(vals.s8p9a_r2), par = v === "parallel";
        state.r1 = r1; state.r2 = r2; state.par = par;
        // [x0, x1, y0, y1]: oba działają, tylko E₂, tylko E₁, oba zawodzą
        var both = [0, r1, 0, r2], onlyE2 = [r1, 1, 0, r2], onlyE1 = [0, r1, r2, 1], none = [r1, 1, r2, 1];
        var works = par ? [both, onlyE2, onlyE1] : [both];
        var fails = par ? [none] : [onlyE2, onlyE1, none];
        return {
          x: [0, 1], y: [0, 1],
          layers: [
            { type: "rects", items: works, fill: P.szalwia, alpha: 0.45 },
            { type: "rects", items: fails, fill: P.accent, alpha: 0.55 },
            { type: "vline", x: r1, color: P.ink, width: 1.5 },
            { type: "line", data: [[0, r2], [1, r2]], color: P.ink, width: 1.5 },
            { type: "text", x: r1 / 2, y: r2 / 2, text: "oba działają " + U.fmtP(r1 * r2), color: P.ink, mono: true, halo: true, valign: "middle" }
          ],
          legend: [
            { text: "system działa", color: P.szalwia, box: true },
            { text: "system zawodzi", color: P.accent, box: true }
          ],
          xName: "Element 1: działa na szerokości R₁",
          yName: "Element 2: działa na wysokości R₂",
          aria: "Kwadrat jednostkowy podzielony liniami R₁ = " + r1 + " i R₂ = " + r2 + "; " +
            (par ? "układ równoległy" : "układ szeregowy") + "."
        };
      },
      update: function (root, state) {
        var r1 = state.r1, r2 = state.r2;
        S.setValue(root, "both", U.fmtP(r1 * r2));
        S.setValue(root, "one", U.fmtP(r1 * (1 - r2) + (1 - r1) * r2));
        S.setValue(root, "none", U.fmtP((1 - r1) * (1 - r2)));
        S.setValue(root, "sys", U.fmtP(state.par ? 1 - (1 - r1) * (1 - r2) : r1 * r2));
      }
    },

    // P9B — te same R w dwóch systemach: słupki elementów, szeregu i równoległego.
    s8_architektury: {
      draw: function (v, P, k, state, vals) {
        var r1 = Number(vals.s8p9b_r1), r2 = Number(vals.s8p9b_r2);
        var ser = r1 * r2, par = 1 - (1 - r1) * (1 - r2);
        var lo = Math.min(r1, r2), hi = Math.max(r1, r2), w = 0.32;
        state.ser = ser; state.par = par;
        return {
          x: [0.4, 4.6], y: [0, 1.12], noXGrid: true,
          xLabels: { 1: "Element 1", 2: "Element 2", 3: "Szereg", 4: "Równoległy" },
          layers: [
            { type: "bins", data: [[1 - w, 1 + w, r1], [2 - w, 2 + w, r2]], color: P.secondary, alpha: 0.75 },
            { type: "bins", data: [[3 - w, 3 + w, ser]], color: P.accent },
            { type: "bins", data: [[4 - w, 4 + w, par]], color: P.szalwia },
            { type: "line", data: [[0.4, lo], [4.6, lo]], color: P.ref, width: 1.4, dash: [6, 4] },
            { type: "line", data: [[0.4, hi], [4.6, hi]], color: P.ref, width: 1.4, dash: [6, 4] },
            { type: "text", x: 1, y: r1, text: U.fmt(r1, 3), color: P.ink, mono: true, halo: true },
            { type: "text", x: 2, y: r2, text: U.fmt(r2, 3), color: P.ink, mono: true, halo: true },
            { type: "text", x: 3, y: ser, text: U.fmt(ser, 3), color: P.ink, mono: true, halo: true },
            { type: "text", x: 4, y: par, text: U.fmt(par, 3), color: P.ink, mono: true, halo: true }
          ],
          yName: "Niezawodność",
          aria: "Elementy " + U.fmt(r1, 3) + " i " + U.fmt(r2, 3) + "; szereg " + U.fmt(ser, 3) + ", układ równoległy " + U.fmt(par, 3) + "."
        };
      },
      update: function (root, state) {
        S.setValue(root, "r_ser", U.fmtP(state.ser));
        S.setValue(root, "k_ser", U.fmtP(1 - state.ser));
        S.setValue(root, "r_par", U.fmtP(state.par));
        S.setValue(root, "k_par", U.fmtP(1 - state.par));
      }
    },

    // P11A — system mieszany: sterownik C leży na każdej drodze sukcesu.
    s8_czas: {
      draw: function (v, P, k, state, vals) {
        var t = Number(vals.s8p11a_t);
        state.t = t;
        var b = 3000, step = 50;
        // pole między krzywą sterownika (w przód) a krzywą systemu (w tył)
        var upper = curve(rc, 0, b, step), lower = curve(rsys, 0, b, step).reverse();
        return {
          x: [0, b], y: [0, 1.05], xLabelEvery: 500,
          layers: [
            { type: "polygon", points: upper.concat(lower), fill: C.alpha(P.accent, 0.12) },
            { type: "line", data: curve(ra, 0, b, step), color: P.niebo, width: 1.8 },
            { type: "line", data: curve(rb, 0, b, step), color: P.szalwia, width: 1.8 },
            { type: "line", data: curve(rc, 0, b, step), color: P.secondary, width: 2.2 },
            { type: "line", data: curve(rsys, 0, b, step), color: P.accent, width: 3.2 },
            { type: "vline", x: t, color: P.ink, width: 1.5, dash: "dashed" },
            dot(t, ra(t), P.niebo, 8),
            dot(t, rb(t), P.szalwia, 8),
            dot(t, rc(t), P.secondary, 8),
            dot(t, rsys(t), P.accent, 9)
          ],
          legend: [
            { text: "wentylator A", color: P.niebo },
            { text: "wentylator B", color: P.szalwia },
            { text: "sterownik C", color: P.secondary },
            { text: "system", color: P.accent }
          ],
          xName: "Czas (h)", yName: "R(t)",
          aria: "Niezawodność w chwili " + t + " h: A " + U.fmt(ra(t), 3) + ", B " + U.fmt(rb(t), 3) +
            ", C " + U.fmt(rc(t), 3) + ", system " + U.fmt(rsys(t), 3) + "."
        };
      },
      update: function (root, state) {
        var t = state.t;
        S.setValue(root, "ra", U.fmtP(ra(t)));
        S.setValue(root, "rb", U.fmtP(rb(t)));
        S.setValue(root, "rc", U.fmtP(rc(t)));
        S.setValue(root, "rs", U.fmtP(rsys(t)));
      }
    },

    // P11B — ile daje druga gałąź: ryzyko wentylatora A i bloku A/B w czasie.
    s8_przewaga: {
      draw: function (v, P, k, state, vals) {
        var t = Number(vals.s8p11b_t);
        state.t = t;
        var single = 1 - ra(t), pair = (1 - ra(t)) * (1 - rb(t));
        var b = 3000, step = 50;
        return {
          x: [0, b], y: [0, 1], xLabelEvery: 500,
          layers: [
            { type: "line", data: curve(function (x) { return 1 - ra(x); }, 0, b, step), color: P.secondary, width: 2.2, dash: [5, 4] },
            { type: "line", data: curve(function (x) { return (1 - ra(x)) * (1 - rb(x)); }, 0, b, step), color: P.accent, width: 3 },
            { type: "segment", from: [t, pair], to: [t, single], color: P.szalwia, width: 4 },
            { type: "vline", x: t, color: P.ink, width: 1.5, dash: "dashed" },
            dot(t, single, P.secondary, 8),
            dot(t, pair, P.accent, 8)
          ],
          legend: [
            { text: "sam wentylator A", color: P.secondary, dash: true },
            { text: "wentylatory A i B równolegle", color: P.accent },
            { text: "ryzyko usunięte przez drugą gałąź", color: P.szalwia, box: true }
          ],
          xName: "Czas (h)", yName: "Ryzyko bloku wentylatorów",
          aria: "Ryzyko wentylatora A " + U.fmt(single, 3) + ", bloku A i B " + U.fmtP(pair) + " przy t = " + t + " h."
        };
      },
      update: function (root, state) {
        var t = state.t;
        S.setValue(root, "risk_a", U.fmtP(1 - ra(t)));
        S.setValue(root, "risk_ab", U.fmtP((1 - ra(t)) * (1 - rb(t))));
        S.setValue(root, "mult", U.fmt(1 - rb(t), 3));
      }
    }
  });
})();
