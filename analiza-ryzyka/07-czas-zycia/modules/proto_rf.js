// Prototypy punktów stop wykładu 07, grupa rf: ryciny lcSF dla P14, P16A, P16B, P17A.
// Liczby: window.lcDist (gamma) i lcC7.util (Weibull). Kontenery w R: lc_sf_figure()
// w stop_proto_rf.R.
//
//   c7_gamma   — P14: gęstość gamma(k, skala 500 h) z suwaka, pole od 1000 h, kreska
//                k = 1 (wykładniczy), kreska średniej 500·k, stałe osie.
//   c7_weibull — P16A: R(t) (wykres 1) i h(t) (wykres 2) Weibulla z suwaków β i η.
//   c7_rodzina — P16B: cztery kształty β przy η = 1700 h, R(t) i h(t), bez kontrolek.
//   c7_ranking — P17A: trzy modele o MTTF 1500 h, pasek „kto prowadzi”, suwak misji.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcC7, D = window.lcDist;
  if (!C || !S || !Z || !D) return;
  var U = Z.util;

  // Wartości ponad górną granicą osi przycinamy (krzywe rosną do nieskończoności przy t → 0).
  function capped(f, top) { return function (t) { return Math.min(f(t), top); }; }

  // Model o najwyższym R(t) w chwili t (indeks w tablicy modeli).
  function leader(models, t) {
    var best = 0, bv = -1;
    models.forEach(function (m, i) {
      var r = U.wR(t, m.beta, m.eta);
      if (r > bv) { bv = r; best = i; }
    });
    return best;
  }
  // Odcinki [from, to) z prowadzącym modelem; siatka co 1 h, granice w pełnych godzinach.
  function leadRuns(models, tmax) {
    var runs = [], cur = leader(models, 1), from = 0;
    for (var t = 2; t <= tmax; t++) {
      var l = leader(models, t);
      if (l !== cur) { runs.push({ i: cur, from: from, to: t }); cur = l; from = t; }
    }
    runs.push({ i: cur, from: from, to: tmax });
    return runs;
  }

  S.register({
    c7_gamma: {
      draw: function (v, P, k, state, vals) {
        var kk = Number(vals.c7p14_k), top = 0.0025, m = 500 * kk;
        state.k = kk;
        var dens = capped(function (t) { return D.dgamma(t, kk, 500); }, top);
        var ym = dens(m);
        return {
          x: [0, 6000], y: [0, 0.0021], xLabelEvery: 1000,
          xName: "Czas do k-tej awarii (h)", yName: "Gęstość (1/h)",
          layers: [
            { type: "line", data: U.fcurve(dens, 1000, 6000, 250), color: P.secondary, width: 0, fill: P.secondary, fillAlpha: 0.25 },
            { type: "line", data: U.fcurve(function (t) { return D.dgamma(t, 1, 500); }, 5, 6000, 250), color: P.ref, width: 1.6, dash: [5, 4] },
            { type: "line", data: U.fcurve(dens, 5, 6000, 300), color: P.secondary, width: 2.6 },
            { type: "segs", items: [{ from: [m, 0], to: [m, ym] }], color: P.subtle, width: 1.4, dash: [3, 3] },
            { type: "vline", x: 1000, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "text", x: 1000, y: "top", text: "1000 h", color: P.ink, align: "left", mono: true }
          ],
          legend: [
            { text: "gęstość dla k z suwaka", color: P.secondary },
            { text: "k = 1 (wykładniczy)", color: P.ref, dash: true },
            { text: "zapas wystarczy na 1000 h", color: P.secondary, box: true }
          ],
          aria: "Gęstość czasu do k-tej awarii dla k = " + kk + " z kreską 1000 godzin."
        };
      },
      update: function (root, state) {
        var kk = state.k;
        S.setValue(root, "mean", U.fmtH(500 * kk, 0));
        S.setValue(root, "mode", U.fmtH(Math.max(0, (kk - 1) * 500), 0));
        S.setValue(root, "p", U.fmtP(D.pgamma(1000, kk, 500, false)));
        S.setValue(root, "fam", Math.abs(kk - Math.round(kk)) < 1e-9 ? "Erlang" : "gamma");
      }
    },

    c7_weibull: {
      draw: function (v, P, k, state, vals) {
        var beta = Number(vals.c7p16_beta), eta = Number(vals.c7p16_eta), e1 = Math.exp(-1);
        state.beta = beta; state.eta = eta;
        if (k === 0) {
          var X = [0, 5000];
          return {
            x: X, y: [0, 1], xLabelEvery: 1000, alignLeft: true,
            xName: "Czas pracy (h)", yName: "R(t)",
            layers: [
              { type: "line", data: U.fcurve(function (t) { return U.wR(t, 1, eta); }, X[0], X[1]), color: P.ref, width: 1.6, dash: [5, 4] },
              { type: "line", data: U.fcurve(function (t) { return U.wR(t, beta, eta); }, X[0], X[1]), color: P.bursztyn, width: 2.6 },
              { type: "segs", items: [{ from: [0, e1], to: [eta, e1] }, { from: [eta, 0], to: [eta, e1] }], color: P.subtle, width: 1.4, dash: [3, 3] },
              { type: "point", x: eta, y: e1, color: P.bursztyn, size: 12 },
              { type: "text", x: eta, y: e1, text: "t = η", color: P.soft, align: "left", halo: true },
              { type: "vline", x: 1000, color: P.ink, width: 1.8, dash: [6, 4] },
              { type: "text", x: 1000, y: "top", text: "misja 1000 h", color: P.ink, align: "left", mono: true },
              { type: "point", x: 1000, y: U.wR(1000, beta, eta), color: P.accent, size: 12 }
            ],
            legend: [
              { text: "Weibull z suwaków", color: P.bursztyn },
              { text: "wykładniczy o tej samej skali (β = 1)", color: P.ref, dash: true }
            ],
            aria: "Niezawodność Weibulla z β = " + beta + " i η = " + eta + " godzin oraz wykładnicza krzywa o tej samej skali."
          };
        }
        var top = 0.0048, h1 = U.wh(1000, beta, eta);
        var layers = [
          { type: "line", data: [[0, 1 / eta], [5000, 1 / eta]], color: P.ref, width: 1.6, dash: [5, 4] },
          { type: "line", data: U.fcurve(capped(function (t) { return U.wh(t, beta, eta); }, top), 5, 5000, 300), color: P.bursztyn, width: 2.6 },
          { type: "vline", x: 1000, color: P.ink, width: 1.8, dash: [6, 4] }
        ];
        if (h1 <= 0.004) layers.push({ type: "point", x: 1000, y: h1, color: P.accent, size: 12 });
        return {
          x: [0, 5000], y: [0, 0.004], xLabelEvery: 1000, alignLeft: true,
          xName: "Czas pracy (h)", yName: "h(t) (1/h)",
          layers: layers,
          aria: "Hazard Weibulla z β = " + beta + " i η = " + eta + " godzin z kreską misji 1000 godzin."
        };
      },
      update: function (root, state) {
        var b = state.beta, eta = state.eta, dir;
        if (Math.abs(b - 1) <= 1e-9) dir = "stały";
        else if (b < 1) dir = "maleje";
        else dir = "rośnie";
        S.setValue(root, "dir", dir);
        S.setValue(root, "r", U.fmtP(U.wR(1000, b, eta)));
        S.setValue(root, "h", U.fmt(U.wh(1000, b, eta), 6));
        S.setValue(root, "m", U.fmtH(U.wMean(b, eta), 0));
      }
    },

    c7_rodzina: {
      draw: function (v, P, k) {
        var eta = 1700, e1 = Math.exp(-1), X = [0, 5000], top = 0.0036;
        var models = [
          { beta: 0.5, color: P.terakota, text: "β = 0.5: hazard maleje (wczesne defekty)" },
          { beta: 1, color: P.niebo, text: "β = 1: hazard stały" },
          { beta: 2, color: P.bursztyn, text: "β = 2: hazard rośnie liniowo" },
          { beta: 4, color: P.secondary, text: "β = 4: hazard rośnie coraz szybciej" }
        ];
        if (k === 0) {
          var rl = models.map(function (m) {
            return { type: "line", data: U.fcurve(function (t) { return U.wR(t, m.beta, eta); }, X[0], X[1]), color: m.color, width: 2.4 };
          });
          rl.push({ type: "point", x: eta, y: e1, color: P.ink, size: 12 });
          rl.push({ type: "text", x: eta, y: e1, text: "t = η: R = 0.37", color: P.soft, align: "left", halo: true });
          return {
            x: X, y: [0, 1], xLabelEvery: 1000, alignLeft: true,
            xName: "Czas pracy (h)", yName: "R(t)",
            layers: rl,
            legend: models.map(function (m) { return { text: m.text, color: m.color }; }),
            aria: "Cztery krzywe niezawodności Weibulla przy skali 1700 godzin, przecinające się w punkcie 0.37."
          };
        }
        return {
          x: X, y: [0, 0.003], xLabelEvery: 1000, alignLeft: true,
          xName: "Czas pracy (h)", yName: "h(t) (1/h)",
          layers: models.map(function (m) {
            return { type: "line", data: U.fcurve(capped(function (t) { return U.wh(t, m.beta, eta); }, top), 5, X[1], 300), color: m.color, width: 2.4 };
          }),
          aria: "Cztery funkcje hazardu Weibulla przy skali 1700 godzin."
        };
      }
    },

    c7_ranking: {
      draw: function (v, P, k, state, vals) {
        var t = Number(vals.c7p17_t), X = [0, 3500];
        var models = [
          { beta: 0.7, color: P.terakota, name: "β = 0.7", text: "β = 0.7 (hazard maleje)" },
          { beta: 1, color: P.niebo, name: "β = 1", text: "β = 1 (hazard stały)" },
          { beta: 2.5, color: P.bursztyn, name: "β = 2.5", text: "β = 2.5 (hazard rośnie)" }
        ];
        models.forEach(function (m) { m.eta = 1500 / U.gammaFn(1 + 1 / m.beta); });
        state.t = t; state.models = models;
        var layers = leadRuns(models, X[1]).filter(function (r) { return r.to > r.from; }).map(function (r) {
          return { type: "rect", x0: r.from, x1: r.to, y0: 0, y1: 0.035, fill: models[r.i].color, alpha: 0.9 };
        });
        models.forEach(function (m) {
          layers.push({ type: "line", data: U.fcurve(function (x) { return U.wR(x, m.beta, m.eta); }, X[0], X[1], 350), color: m.color, width: 2.6 });
        });
        layers.push({ type: "vline", x: t, color: P.ink, width: 1.8, dash: [6, 4] });
        layers.push({ type: "text", x: t, y: "top", text: "misja " + t + " h", color: P.ink, align: "left", mono: true });
        models.forEach(function (m) {
          layers.push({ type: "point", x: t, y: U.wR(t, m.beta, m.eta), color: m.color, size: 11 });
        });
        return {
          x: X, y: [0, 1], xLabelEvery: 500,
          xName: "Czas pracy (h)", yName: "R(t)",
          layers: layers,
          legend: [
            { text: models[0].text, color: models[0].color },
            { text: models[1].text, color: models[1].color },
            { text: models[2].text, color: models[2].color },
            { text: "pasek przy osi: model o najwyższym R(t)", color: P.subtle, box: true }
          ],
          aria: "Trzy krzywe niezawodności o tym samym MTTF 1500 godzin, z kreską misji " + t + " godzin."
        };
      },
      update: function (root, state) {
        var t = state.t, ms = state.models;
        S.setValue(root, "r1", U.fmtP(U.wR(t, ms[0].beta, ms[0].eta)));
        S.setValue(root, "r2", U.fmtP(U.wR(t, ms[1].beta, ms[1].eta)));
        S.setValue(root, "r3", U.fmtP(U.wR(t, ms[2].beta, ms[2].eta)));
        S.setValue(root, "best", ms[leader(ms, t)].name);
      }
    }
  });
})();
