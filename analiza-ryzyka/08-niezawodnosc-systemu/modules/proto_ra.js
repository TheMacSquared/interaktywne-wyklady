// Prototypy punktów stop wykładu 08, plik wzorcowy (P7). Ryciny lcSF liczone
// w przeglądarce; kontener w R: lc_sf_figure() w stop_proto_ra.R.
//
//   s8_lancuch — Piaskownica „Długi łańcuch” (P7): R szeregu n jednakowych ogniw
//                jako słupki Rᵏ dla k = 1…20, próg 0.90, przybliżenie sumą ryzyk.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcS8;
  if (!C || !S || !Z) return;
  var U = Z.util;

  S.register({
    s8_lancuch: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, n = Number(vals.s8p7_n), r = Number(vals.s8p7_r), N = cfg.nmax;
        state.n = n; state.r = r;
        var before = [], now = [], after = [], approx = [];
        for (var i = 1; i <= N; i++) {
          var bar = [i - 0.36, i + 0.36, Math.pow(r, i)];
          (i < n ? before : i === n ? now : after).push(bar);
          approx.push([i, Math.max(0, 1 - i * (1 - r))]);
        }
        var ticks = {};
        for (var t = 1; t <= N; t++) ticks[t] = String(t);
        return { x: [0.3, N + 0.7], y: [0, 1.05], xLabels: ticks, noXGrid: true,
          layers: [
            { type: "bins", data: after, color: P.secondary, alpha: 0.18 },
            { type: "bins", data: before, color: P.secondary, alpha: 0.75 },
            { type: "bins", data: now, color: P.accent },
            { type: "line", data: approx, color: P.ref, width: 1.6, dash: [5, 4] },
            { type: "line", data: [[0.3, cfg.limit], [N + 0.7, cfg.limit]], color: P.ink, width: 1.6, dash: [6, 4] },
            { type: "text", x: N + 0.6, y: cfg.limit, text: "próg " + U.fmt(cfg.limit, 2), color: P.ink, align: "right", mono: true, halo: true },
            { type: "text", x: n, y: Math.pow(r, n), text: U.fmt(Math.pow(r, n), 3), color: P.accent, mono: true, bold: true, halo: true }
          ],
          legend: [{ text: "R szeregu o n ogniwach", color: P.accent, box: true },
                   { text: "krótsze szeregi", color: P.secondary, box: true },
                   { text: "przybliżenie 1 − n(1 − R)", color: P.ref, dash: true }],
          xName: "Liczba ogniw w szeregu", yName: "R systemu",
          aria: "Niezawodność szeregu " + n + " ogniw o niezawodności " + r + " każde: " + U.fmt(Math.pow(r, n), 3) + "." };
      },
      update: function (root, state) {
        var rs = Math.pow(state.r, state.n), lim = state.config.limit;
        S.setValue(root, "rs", U.fmtP(rs));
        S.setValue(root, "risk", U.fmtP(1 - rs));
        S.setValue(root, "sum", U.fmtP(state.n * (1 - state.r)));
        // największe n, dla którego Rⁿ ≥ próg (z zapasem na zaokrąglenia)
        S.setValue(root, "max", String(Math.floor(Math.log(lim) / Math.log(state.r) + 1e-9)));
      }
    }
  });
})();
