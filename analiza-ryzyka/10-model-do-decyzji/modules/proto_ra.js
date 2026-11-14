// Prototypy punktów stop wykładu 10, plik wzorcowy (P7). Ryciny lcSF liczone
// w przeglądarce; kontener w R: lc_sf_figure() w stop_proto_ra.R. Parametry modelu
// przychodzą z R w data-config (i10p_cfg) — w tym pliku nie ma liczb modelu.
//
//   i10_modele — Galeria „Ta sama średnia, inne ryzyko” (P7): R(t) modelu
//                wykładniczego i Weibulla na stałych osiach, czas misji z suwaka.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcI10;
  if (!C || !S || !Z) return;
  var U = Z.util;

  function lifeCurve(cfg, model, n) {
    var pts = [];
    for (var i = 0; i <= n; i++) { var t = cfg.time_max * i / n; pts.push([t, U.fanR(cfg, t, model)]); }
    return pts;
  }

  S.register({
    i10_modele: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, t = Number(vals.i10p7_t);
        var re = U.fanR(cfg, t, "exp"), rw = U.fanR(cfg, t, "weibull");
        state.re = re; state.rw = rw;
        var left = t > cfg.time_max * 0.8;  // napisy przy prawej krawędzi stają po lewej stronie kreski
        return { x: [0, cfg.time_max], y: [0, 1], xLabelEvery: 500,
          layers: [
            { type: "vline", x: cfg.cross, color: P.subtle, width: 1.2, dash: [3, 3] },
            { type: "text", x: cfg.cross, y: "top", text: "przecięcie " + Math.round(cfg.cross) + " " + cfg.unit, color: P.subtle, align: "left", mono: true },
            { type: "line", data: lifeCurve(cfg, "exp", 200), color: P.secondary, width: 2.4 },
            { type: "line", data: lifeCurve(cfg, "weibull", 200), color: P.accent, width: 2.4 },
            { type: "vline", x: t, color: P.ink, width: 1.6, dash: [6, 4] },
            { type: "point", x: t, y: re, color: P.secondary, size: 11 },
            { type: "point", x: t, y: rw, color: P.accent, size: 11 },
            { type: "text", x: t, y: re, text: U.fmtP(re), color: P.secondary, align: left ? "right" : "left", valign: "middle", mono: true, bold: true, halo: true },
            { type: "text", x: t, y: rw, text: U.fmtP(rw), color: P.accent, align: left ? "right" : "left", valign: "middle", mono: true, bold: true, halo: true }
          ],
          legend: [{ text: "model wykładniczy (stały hazard)", color: P.secondary },
                   { text: "Weibull (zużycie)", color: P.accent }],
          xName: "Czas misji (" + cfg.unit + ")", yName: "R(t)",
          aria: "Krzywe niezawodności wentylatora w modelu wykładniczym i Weibulla; przy czasie misji " + t +
                " wartości " + U.fmtP(re) + " i " + U.fmtP(rw) + "." };
      },
      update: function (root, state) {
        S.setValue(root, "re", U.fmtP(state.re));
        S.setValue(root, "rw", U.fmtP(state.rw));
        S.setValue(root, "d", (state.rw >= state.re ? "+" : "−") + U.fmtP(Math.abs(state.rw - state.re)));
      }
    }
  });
})();
