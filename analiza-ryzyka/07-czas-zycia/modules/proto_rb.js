// Prototypy punktów stop wykładu 07, grupa rb (P2A, P2B, P8, P10). Ryciny lcSF
// liczone w przeglądarce; kontenery w R: lc_sf_figure() w stop_proto_rb.R.
//
//   c7_gestosci      — „Dwie gęstości, jedna średnia” (P2A): gęstości A i B na jednym
//                      wykresie, pola za kreską misji, stałe osie.
//   c7_gestosci_b    — P2A osobno (P2B): dwa wykresy, pole przed kreską w akcencie.
//   c7_cztery        — „Cztery funkcje, jedna chwila” (P8): F i R na górze, h i f na dole.
//   c7_pole_hazardu  — „Pole pod hazardem” (P10): pole H(t) pod hazardem, R(t) = exp(−H(t)).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcC7;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Wspólne osie gęstości i czasu w rycinach P2A/P2B.
  var X_DENS = [0, 5000], Y_DENS = [0, 0.0008];

  // Gęstości obu ofert (1/h): A wykładnicza z MTTF, B Weibull z β i η.
  function dA(cfg) { return function (t) { return U.wf(t, 1, cfg.mttf); }; }
  function dB(cfg) { return function (t) { return U.wf(t, cfg.beta, cfg.eta); }; }

  // Warstwy: pole pod krzywą f na przedziale [from, to] (bez linii, z wypełnieniem).
  function area(f, from, to, color, alpha) {
    return { type: "line", data: U.fcurve(f, from, to, 120), color: color, width: 0, fill: color, fillAlpha: alpha };
  }
  // Warstwy: krzywa f na przedziale [from, to].
  function curve(f, from, to, color, width) {
    return { type: "line", data: U.fcurve(f, from, to, 200), color: color, width: width };
  }
  // Kreska MTTF (przerywana, szara); napis tuż za kreską, niżej niż napis misji u góry,
  // żeby przy misji 1000 h oba napisy nie nachodziły na siebie.
  function mttfLayers(cfg, P, yLabel) {
    return [
      { type: "vline", x: cfg.mttf, color: P.subtle, width: 1.4, dash: [3, 3] },
      { type: "text", x: cfg.mttf, y: yLabel, text: "MTTF", color: P.soft, align: "left" }
    ];
  }
  // Kreska misji z napisem u góry (ink, przerywana).
  function missionLayers(t, P) {
    return [
      { type: "vline", x: t, color: P.ink, width: 1.8, dash: [6, 4] },
      { type: "text", x: t, y: "top", text: "misja " + t + " h", color: P.ink, align: "left", mono: true }
    ];
  }

  S.register({
    c7_gestosci: {
      draw: function (v, P, k, state) {
        var cfg = state.config, t = Number(v), fA = dA(cfg), fB = dB(cfg);
        state.t = t;
        return {
          x: X_DENS, y: Y_DENS, xLabelEvery: 1000,
          layers: [].concat(
            [area(fA, t, X_DENS[1], P.niebo, 0.3), area(fB, t, X_DENS[1], P.bursztyn, 0.3),
             curve(fA, X_DENS[0], X_DENS[1], P.niebo, 2.6), curve(fB, X_DENS[0], X_DENS[1], P.bursztyn, 2.6)],
            mttfLayers(cfg, P, 0.0007),
            missionLayers(t, P)),
          legend: [{ text: "oferta A (wykładnicza)", color: P.niebo }, { text: "oferta B (Weibull)", color: P.bursztyn }],
          xName: "Czas pracy (h)", yName: "Gęstość (1/h)",
          aria: "Gęstości obu ofert z kreską misji " + t + " godzin: pole za kreską to niezawodność R(t)."
        };
      },
      update: function (root, state, v) {
        var cfg = state.config, t = Number(v);
        S.setValue(root, "ra", U.fmtP(U.wR(t, 1, cfg.mttf)));
        S.setValue(root, "rb", U.fmtP(U.wR(t, cfg.beta, cfg.eta)));
      }
    },

    // ---- P2B: jak P2A, dwa wykresy, pole przed kreską w akcencie ----------------
    c7_gestosci_b: {
      draw: function (v, P, k, state) {
        var cfg = state.config, t = Number(v), isA = k === 0;
        var f = isA ? dA(cfg) : dB(cfg), col = isA ? P.niebo : P.bursztyn;
        state.t = t;
        return {
          x: X_DENS, y: Y_DENS, xLabelEvery: 1000, alignLeft: true,
          layers: [].concat(
            [area(f, 0, t, P.accent, 0.3), area(f, t, X_DENS[1], col, 0.3),
             curve(f, X_DENS[0], X_DENS[1], col, 2.6)],
            mttfLayers(cfg, P, 0.00055),
            missionLayers(t, P)),
          xName: "Czas pracy (h)", yName: "",
          aria: (isA ? "Gęstość oferty A" : "Gęstość oferty B") + " z kreską misji " + t +
            " godzin: pole przed kreską to wentylatory, które zawiodły."
        };
      },
      update: function (root, state, v) {
        var cfg = state.config, t = Number(v);
        S.setValue(root, "ra", U.fmtP(U.wR(t, 1, cfg.mttf)));
        S.setValue(root, "rb", U.fmtP(U.wR(t, cfg.beta, cfg.eta)));
      }
    },

    // ---- P8: cztery funkcje jednej chwili ---------------------------------------
    c7_cztery: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, t = Number(vals.c7p8_t);
        var isB = vals.c7p8_model === "b";
        var beta = isB ? cfg.beta : 1, eta = isB ? cfg.eta : cfg.mttf;
        var col = isB ? P.bursztyn : P.niebo;
        var F = function (x) { return U.wF(x, beta, eta); }, R = function (x) { return U.wR(x, beta, eta); };
        var h = function (x) { return U.wh(x, beta, eta); }, f = function (x) { return U.wf(x, beta, eta); };
        state.t = t; state.beta = beta; state.eta = eta;
        if (k === 0) {
          return {
            x: [0, 4000], y: [0, 1], xLabelEvery: 1000, alignLeft: true,
            layers: [
              curve(F, 0, 4000, P.accent, 2.4),
              curve(R, 0, 4000, P.szalwia, 2.4),
              { type: "vline", x: t, color: P.ink, width: 1.8, dash: [6, 4] },
              { type: "point", x: t, y: F(t), color: P.accent, size: 11 },
              { type: "point", x: t, y: R(t), color: P.szalwia, size: 11 },
              { type: "text", x: t, y: t > 2000 ? 0.5 : "top", text: "t = " + t + " h", color: P.ink, align: t > 2000 ? "right" : "left", mono: true }
            ],
            legend: [
              { text: "F(t): zawiodło do chwili t", color: P.accent },
              { text: "R(t): działa po chwili t", color: P.szalwia },
              { text: "h(t): hazard", color: P.ink },
              { text: "f(t): gęstość", color: col }
            ],
            yName: "Prawdopodobieństwo",
            aria: "Dystrybuanta F(t) i niezawodność R(t) przy t = " + t + " godzin."
          };
        }
        return {
          x: [0, 4000], y: [0, 0.003], xLabelEvery: 1000, alignLeft: true,
          layers: [
            curve(h, 0, 4000, P.ink, 2.6),
            curve(f, 0, 4000, col, 2.6),
            { type: "vline", x: t, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "point", x: t, y: h(t), color: P.ink, size: 11 },
            { type: "point", x: t, y: f(t), color: col, size: 11 }
          ],
          xName: "Czas pracy (h)", yName: "1/h",
          aria: "Hazard h(t) i gęstość f(t) przy t = " + t + " godzin."
        };
      },
      update: function (root, state) {
        var t = state.t, beta = state.beta, eta = state.eta;
        S.setValue(root, "F", U.fmtP(U.wF(t, beta, eta)));
        S.setValue(root, "R", U.fmtP(U.wR(t, beta, eta)));
        S.setValue(root, "f", U.fmt(U.wf(t, beta, eta), 6));
        S.setValue(root, "h", U.fmt(U.wh(t, beta, eta), 6));
      }
    },

    // ---- P10: pole pod hazardem --------------------------------------------------
    c7_pole_hazardu: {
      draw: function (v, P, k, state) {
        var cfg = state.config, t = Number(v);
        var hA = function (x) { return U.wh(x, 1, cfg.mttf); }, hB = function (x) { return U.wh(x, cfg.beta, cfg.eta); };
        var RA = function (x) { return U.wR(x, 1, cfg.mttf); }, RB = function (x) { return U.wR(x, cfg.beta, cfg.eta); };
        state.t = t;
        if (k === 0) {
          return {
            x: [0, 3000], y: [0, 0.0025], xLabelEvery: 500, alignLeft: true,
            layers: [
              { type: "line", data: U.fcurve(hA, 0, t, 60), color: P.niebo, width: 0, fill: P.niebo, fillAlpha: 0.25 },
              { type: "line", data: U.fcurve(hB, 0, t, 120), color: P.bursztyn, width: 0, fill: P.bursztyn, fillAlpha: 0.25 },
              curve(hA, 0, 3000, P.niebo, 2.6),
              curve(hB, 0, 3000, P.bursztyn, 2.6),
              { type: "vline", x: t, color: P.ink, width: 1.8, dash: [6, 4] },
              { type: "point", x: t, y: hA(t), color: P.niebo, size: 12 },
              { type: "point", x: t, y: hB(t), color: P.bursztyn, size: 12 },
              { type: "text", x: t, y: "top", text: "t = " + t + " h", color: P.ink, align: t > 1500 ? "right" : "left", mono: true }
            ],
            legend: [{ text: "oferta A: hazard stały", color: P.niebo }, { text: "oferta B: hazard rośnie", color: P.bursztyn }],
            xName: "Czas pracy (h)", yName: "1/h",
            aria: "Hazardy obu ofert z zacienionym polem do chwili " + t + " godzin."
          };
        }
        return {
          x: [0, 3000], y: [0, 1], xLabelEvery: 500, alignLeft: true,
          layers: [
            curve(RA, 0, 3000, P.niebo, 2.6),
            curve(RB, 0, 3000, P.bursztyn, 2.6),
            { type: "vline", x: t, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "point", x: t, y: RA(t), color: P.niebo, size: 12 },
            { type: "point", x: t, y: RB(t), color: P.bursztyn, size: 12 }
          ],
          xName: "Czas pracy (h)", yName: "R(t)",
          aria: "Niezawodność obu ofert przy t = " + t + " godzin."
        };
      },
      update: function (root, state) {
        var cfg = state.config, t = state.t;
        S.setValue(root, "ha", U.fmt(U.wH(t, 1, cfg.mttf), 3));
        S.setValue(root, "hb", U.fmt(U.wH(t, cfg.beta, cfg.eta), 3));
        S.setValue(root, "ra", U.fmt(U.wR(t, 1, cfg.mttf), 3));
        S.setValue(root, "rb", U.fmt(U.wR(t, cfg.beta, cfg.eta), 3));
      }
    }
  });
})();
