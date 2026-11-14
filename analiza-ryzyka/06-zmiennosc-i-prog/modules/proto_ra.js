// Prototypy punktów stop wykładu 06, plik wzorcowy (P4). Ryciny lcSF liczone
// w przeglądarce; kontener w R: lc_sf_figure() w stop_proto_ra.R.
//
//   z6_musigma — Piaskownica „Przesuń i rozszerz krzywą” (P4): N(μ, σ) z suwaków
//                na tle stanu bazowego, stały próg, pole ogona, stałe osie.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcZ6;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Wspólne osie krzywych temperatury w całym wykładzie.
  var X = [68, 98], YMAX = 0.28;

  S.register({
    z6_musigma: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, mu = Number(vals.z6p4_mu), sd = Number(vals.z6p4_sd), c = cfg.c;
        state.mu = mu; state.sd = sd;
        var tail = U.curve(mu, sd, c, X[1]);
        return { x: X, y: [0, YMAX], xLabelEvery: 5,
          layers: [
            { type: "line", data: tail, color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.4 },
            { type: "line", data: U.curve(cfg.mu, cfg.sd, X[0], X[1]), color: P.ref, width: 1.6, dash: [5, 4] },
            { type: "line", data: U.curve(mu, sd, X[0], X[1]), color: P.secondary, width: 2.6 },
            { type: "vline", x: mu, color: P.subtle, width: 1.4, dash: [3, 3], from: 0, to: U.dnorm(mu, mu, sd) },
            { type: "vline", x: c, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "text", x: c, y: "top", text: "próg " + c + "°C", color: P.ink, align: "left", mono: true }
          ],
          legend: [{ text: "rozkład po zmianie", color: P.secondary }, { text: "stan bazowy N(82, 3)", color: P.ref, dash: true },
                   { text: "pomiary ponad progiem", color: P.accent, box: true }],
          xName: "Temperatura łożyska (°C)", yName: "Gęstość",
          aria: "Krzywa normalna o średniej " + mu + " i odchyleniu " + sd + " stopni na tle stanu bazowego, z polem ponad progiem " + c + " stopni." };
      },
      update: function (root, state) {
        var c = state.config.c, p = 1 - U.pnorm(c, state.mu, state.sd);
        S.setValue(root, "z", U.fmt((c - state.mu) / state.sd, 2));
        S.setValue(root, "p", U.fmtP(p));
        S.setValue(root, "f", U.per1000(p));
      }
    }
  });
})();
