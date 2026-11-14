// Prototypy punktów stop wykładu 07, plik wzorcowy (P12). Ryciny lcSF liczone
// w przeglądarce; kontener w R: lc_sf_figure() w stop_proto_ra.R.
//
//   c7_staly — Piaskownica „Stały hazard” (P12): R(t) = exp(−t/MTTF) z suwaka na
//              tle oferty A, kreska misji 1000 h, punkt (MTTF, e⁻¹), stałe osie.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcC7;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Wspólna oś czasu krzywych R(t) w całym wykładzie.
  var X = [0, 5000];

  S.register({
    c7_staly: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, m = Number(vals.c7p12_mttf), t0 = cfg.mission;
        state.m = m;
        var r0 = U.wR(t0, 1, m), e1 = Math.exp(-1);
        return { x: X, y: [0, 1], xLabelEvery: 1000,
          layers: [
            { type: "line", data: U.fcurve(function (t) { return U.wR(t, 1, cfg.mttf); }, X[0], X[1]), color: P.ref, width: 1.6, dash: [5, 4] },
            { type: "line", data: U.fcurve(function (t) { return U.wR(t, 1, m); }, X[0], X[1]), color: P.niebo, width: 2.6 },
            { type: "segs", items: [{ from: [0, e1], to: [m, e1] }, { from: [m, 0], to: [m, e1] }], color: P.subtle, width: 1.4, dash: [3, 3] },
            { type: "vline", x: t0, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "point", x: m, y: e1, color: P.niebo, size: 12 },
            { type: "point", x: t0, y: r0, color: P.accent, size: 12 },
            { type: "text", x: t0, y: "top", text: "misja " + t0 + " h", color: P.ink, align: "left", mono: true },
            { type: "text", x: m, y: e1, text: "t = MTTF", color: P.soft, align: "left", halo: true }
          ],
          legend: [{ text: "R(t) dla MTTF z suwaka", color: P.niebo }, { text: "oferta A (MTTF 1500 h)", color: P.ref, dash: true }],
          xName: "Czas pracy (h)", yName: "R(t)",
          aria: "Krzywa niezawodności rozkładu wykładniczego o średniej " + m + " godzin z kreską misji " + t0 + " godzin." };
      },
      update: function (root, state) {
        var m = state.m;
        S.setValue(root, "r", U.fmtP(U.wR(state.config.mission, 1, m)));
        S.setValue(root, "med", U.fmtH(m * Math.LN2));
        S.setValue(root, "fm", "63.2%");
      }
    }
  });
})();
