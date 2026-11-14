// Prototypy punktów stop wykładu 08, grupa rf (P16A, P16B, P17). Ryciny lcSF liczone
// w przeglądarce; kontenery w R: lc_sf_figure() w stop_proto_rf.R.
//
//   s8_wspolna — piaskownica „Ile zostaje z obietnicy 0.996” (P16A): R(q) = (1 − q)·0.996
//                (wzór 8.10), trójkąt straty między obietnicą a linią R(q).
//   s8_sklad   — piaskownica „Z czego składa się ryzyko” (P16B): ryzyko w polach
//                skumulowanych; na dole awaria obu wentylatorów, na górze wspólne zasilanie.
//   s8_beta    — piaskownica „β: awarie własne i wspólne” (P17): beta-factor (wzór 8.12),
//                R(β) = U.betaR(β, λt), λt = −ln 0.92; część wspólna 1 − exp(−β·λt).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcS8;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Dwa wentylatory równolegle, R = 0.92 i 0.95: bez wspólnej przyczyny 0.996 (ryzyko 0.004).
  var R_IND = U.parallel([0.92, 0.95]);
  var RISK_IND = 1 - R_IND;
  var Q_MAX = 0.15;
  // Beta-factor: dwa jednakowe wentylatory o R = 0.92 na 1000 h.
  var LT = -Math.log(0.92);
  var BETA_MAX = 0.2;
  var BASE = 1 - U.betaR(0, LT);       // pełna niezależność: 0.0064

  // Zaokrąglenie połówek w górę (13.45 → 13.5), jak w tekście wykładu.
  function fx(x, d) { var k = Math.pow(10, d); return (Math.round(x * k + 1e-9) / k).toFixed(d); }
  function pct(x) { return Math.round(100 * x) + "%"; }
  function times(x, d) { return fx(x, d) + "×"; }
  // Dwa punkty wystarcza dla funkcji liniowych w q.
  function line2(f, a, b) { return [[a, f(a)], [b, f(b)]]; }
  function curve(f, a, b, n) {
    var out = [];
    for (var i = 0; i <= n; i++) { var x = a + (b - a) * i / n; out.push([x, f(x)]); }
    return out;
  }

  // Rachunek P16: q = P(utraty wspólnego zasilania); R = (1 − q)·0.996.
  function p16(q) {
    var r = U.common(R_IND, q), risk = 1 - r;
    return { r: r, risk: risk, both: U.common(RISK_IND, q), share: q / risk };
  }

  // Rachunek P17: β; ryzyko 1 − R(β), część wspólna 1 − e^(−β·λt).
  function p17(beta) {
    var r = U.betaR(beta, LT), risk = 1 - r, common = 1 - Math.exp(-beta * LT);
    return { r: r, risk: risk, common: common, indep: risk - common };
  }

  S.register({
    // P16A: linia R(q) i obietnica bez wspólnej przyczyny.
    s8_wspolna: {
      draw: function (v, P, k, state, vals) {
        var q = Number(vals.s8p16a_q), o = p16(q);
        state.q = q; state.o = o;
        return {
          x: [0, Q_MAX], y: [0.84, 1], xLabelEvery: 0.05,
          xName: "q", yName: "R układu",
          layers: [
            { type: "polygon", fill: C.alpha(P.accent, 0.13),
              points: [[0, R_IND], [Q_MAX, R_IND], [Q_MAX, (1 - Q_MAX) * R_IND]] },
            { type: "line", data: [[0, R_IND], [Q_MAX, R_IND]], color: P.ref, width: 1.6, dash: [6, 4] },
            { type: "text", x: Q_MAX, y: R_IND, text: "bez wspólnej przyczyny: 0.996",
              color: P.ink, align: "right", mono: true, halo: true },
            { type: "line", data: line2(function (x) { return (1 - x) * R_IND; }, 0, Q_MAX),
              color: P.accent, width: 2.8 },
            { type: "segment", from: [q, o.r], to: [q, R_IND], color: P.ink, width: 2 },
            { type: "point", x: q, y: o.r, color: P.accent, size: 12 }
          ],
          aria: "Niezawodność układu przy wspólnym zasilaniu q = " + U.fmtP(q) + ": R = " + U.fmtP(o.r) +
            ", obietnica bez wspólnej przyczyny 0.996."
        };
      },
      update: function (root, state) {
        var o = state.o;
        S.setValue(root, "s8p16a_out_r", U.fmtP(o.r));
        S.setValue(root, "s8p16a_out_risk", U.fmtP(o.risk));
        S.setValue(root, "s8p16a_out_share", pct(o.share));
        S.setValue(root, "s8p16a_out_ratio", times(o.risk / RISK_IND, 1));
      }
    },

    // P16B: ryzyko w dwóch polach skumulowanych, awaria obu wentylatorów na dole.
    s8_sklad: {
      draw: function (v, P, k, state, vals) {
        var q = Number(vals.s8p16b_q), o = p16(q);
        state.q = q; state.o = o;
        var riskAt = function (x) { return p16(x).risk; };
        var bothAt = function (x) { return p16(x).both; };
        return {
          x: [0, Q_MAX], y: [0, 0.16], xLabelEvery: 0.05,
          xName: "q", yName: "Ryzyko układu",
          layers: [
            { type: "line", data: line2(riskAt, 0, Q_MAX), color: P.accent, width: 2.4,
              fill: P.accent, fillAlpha: 0.4 },
            { type: "line", data: line2(bothAt, 0, Q_MAX), color: P.secondary, width: 2,
              fill: P.secondary, fillAlpha: 0.85 },
            { type: "vline", x: q, color: P.ink, width: 1.4, dash: [5, 4] },
            { type: "point", x: q, y: o.risk, color: P.accent, size: 10 }
          ],
          legend: [{ text: "wspólne zasilanie", color: P.accent, box: true },
                   { text: "awaria obu wentylatorów", color: P.secondary, box: true }],
          aria: "Ryzyko układu przy q = " + U.fmtP(q) + ": razem " + U.fmtP(o.risk) +
            ", z tego awaria obu wentylatorów " + U.fmtP(o.both) + "."
        };
      },
      update: function (root, state) {
        var o = state.o;
        S.setValue(root, "s8p16b_out_q", U.fmtP(state.q));
        S.setValue(root, "s8p16b_out_both", U.fmtP(o.both));
        S.setValue(root, "s8p16b_out_risk", U.fmtP(o.risk));
        S.setValue(root, "s8p16b_out_share", pct(o.share));
      }
    },

    // P17: beta-factor; pole całego ryzyka i pole części niezależnej (dolne).
    s8_beta: {
      draw: function (v, P, k, state, vals) {
        var beta = Number(vals.s8p17_beta), o = p17(beta);
        state.o = o;
        var riskAt = function (x) { return p17(x).risk; };
        var indepAt = function (x) { return p17(x).indep; };
        return {
          x: [0, BETA_MAX], y: [0, 0.024], xLabelEvery: 0.05,
          xName: "β", yName: "Ryzyko układu na 1000 h",
          layers: [
            { type: "line", data: curve(riskAt, 0, BETA_MAX, 40), color: P.accent, width: 2.6,
              fill: P.accent, fillAlpha: 0.35 },
            { type: "line", data: curve(indepAt, 0, BETA_MAX, 40), color: P.secondary, width: 1.8,
              fill: P.secondary, fillAlpha: 0.8 },
            { type: "line", data: [[0, BASE], [BETA_MAX, BASE]], color: P.ref, width: 1.6, dash: [6, 4] },
            { type: "text", x: BETA_MAX, y: BASE, text: "pełna niezależność: 0.0064",
              color: P.ink, align: "right", mono: true, halo: true },
            { type: "vline", x: beta, color: P.ink, width: 1.4, dash: [5, 4] },
            { type: "point", x: beta, y: o.risk, color: P.accent, size: 10 }
          ],
          legend: [{ text: "awarie wspólne", color: P.accent, box: true },
                   { text: "niezależna awaria obu gałęzi", color: P.secondary, box: true }],
          aria: "Ryzyko układu przy β = " + U.fmtP(beta) + ": " + U.fmtP(o.risk) +
            ", z tego część wspólna " + U.fmtP(o.common) + "."
        };
      },
      update: function (root, state) {
        var o = state.o;
        S.setValue(root, "s8p17_out_r", U.fmtP(o.r));
        S.setValue(root, "s8p17_out_risk", U.fmtP(o.risk));
        S.setValue(root, "s8p17_out_common", U.fmtP(o.common));
        S.setValue(root, "s8p17_out_ratio", times(o.risk / BASE, 2));
      }
    }
  });
})();
