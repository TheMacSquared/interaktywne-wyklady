// Prototypy punktów stop wykładu 07, grupa rh: P19, P20, P21A, P21B, P22. Ryciny lcSF
// liczone w przeglądarce; kontenery w R: lc_sf_figure() w stop_proto_rh.R.
//
//   c7_wanna          — Piaskownica „Wanna: suma trzech mechanizmów” (P19): hazard
//                       h1 + h2 + h3 z suwakiem nasilenia zużycia w, pasy tła wskazują
//                       dominującego sprawcę, stałe osie 0–4000 h i 0–1.4.
//   c7_jeden_weibull  — Przypadek brzegowy „Jeden Weibull nie zrobi wanny” (P20): wanna
//                       dla w = 1 i jeden hazard Weibulla przez dno wanny, suwak β.
//   c7_termin         — Piaskownica „Termin przeglądu na krzywej R(t)” (P21A): R Weibulla
//                       i wykładniczego, wymaganie R* z przełącznika, terminy i ryzyko.
//   c7_termin_pole    — Piaskownica „Ryzyko jako pole przed kreską przeglądu” (P21B):
//                       gęstość Weibulla, pole przed przeglądem i pole po nim.
//   c7_stary_nowy     — Piaskownica „Używany czy nowy na kolejne 500 h?” (P22): warunkowa
//                       niezawodność używanego R(s + u) / R(s) względem nowego R(u).
// Wymaga: stop_proto_common.R, R/lc_stat_figures.R, modules/sceny.js (lcC7.util).

(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcC7;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // ---- P19 / P20: wanna jako suma trzech hazardów (jednostki względne) -------
  var WANNA_X = [0, 4000], WANNA_Y = [0, 1.4], WANNA_CLIP = 1.7;
  function h1(t) { return 1.2 * Math.exp(-t / 350); }          // wczesne defekty
  function h2() { return 0.12; }                               // awarie losowe
  function h3(t, w) { return w * Math.pow(t / 4000, 3); }      // zużycie
  function hSum(t, w) { return h1(t) + h2() + h3(t, w); }
  function clipH(v) { return Math.min(v, WANNA_CLIP); }

  // Który składnik jest największy: przedziały [lo, hi) na osi 0–4000 (siatka co 1 h).
  function dominanceRuns(w) {
    var out = [], cur = null;
    for (var t = 0; t <= 4000; t++) {
      var a = h1(t), b = h2(), c = h3(t, w), k;
      if (a >= b) k = a >= c ? 0 : 2; else k = b >= c ? 1 : 2;
      if (!cur || cur.k !== k) {
        if (cur) cur.hi = t;
        cur = { k: k, lo: t, hi: 4000 };
        out.push(cur);
      }
    }
    return out;
  }
  // Minimum sumy na siatce co 1 h (dno wanny).
  function bathMinimum(w) {
    var bt = 1, bh = Infinity;
    for (var t = 1; t <= 4000; t++) {
      var v = hSum(t, w);
      if (v < bh) { bh = v; bt = t; }
    }
    return { t: bt, h: bh };
  }
  function widestName(runs, names) {
    var width = [0, 0, 0];
    runs.forEach(function (r) { width[r.k] += r.hi - r.lo; });
    var best = 0;
    for (var i = 1; i < 3; i++) if (width[i] > width[best]) best = i;
    return names[best];
  }

  // ---- P21A / P21B / P22: niezawodność i gęstość -----------------------------
  // Liczby z tekstu wykładu: wykładniczy MTTF 1500 h, Weibull β = 2, η = 1700 h.
  var WEIB = { beta: 2, eta: 1700 }, EXP = { beta: 1, eta: 1500 };

  S.register({
    c7_wanna: {
      draw: function (v, P, k, state, vals) {
        var w = Number(vals.c7p19_w);
        var runs = dominanceRuns(w), m = bathMinimum(w);
        var names = ["wczesne defekty", "awarie losowe", "zużycie"];
        var colors = [P.terakota, P.niebo, P.bursztyn];
        var labels = ["kontrola odbiorcza", "redundancja", "wymiana"];
        state.min = m;
        state.dom = widestName(runs, names);

        var layers = [];
        runs.forEach(function (r) {
          layers.push({ type: "band", lo: r.lo, hi: r.hi, color: colors[r.k], alpha: 0.10 });
        });
        runs.forEach(function (r) {
          layers.push({ type: "text", x: (r.lo + r.hi) / 2, y: "top", text: labels[r.k], color: P.soft, size: 0.9 });
        });
        layers.push(
          { type: "line", data: U.fcurve(function (t) { return h1(t); }, WANNA_X[0], WANNA_X[1], 400),
            color: P.terakota, width: 1.6 },
          { type: "line", data: U.fcurve(function () { return h2(); }, WANNA_X[0], WANNA_X[1], 400),
            color: P.niebo, width: 1.6 },
          { type: "line", data: U.fcurve(function (t) { return h3(t, w); }, WANNA_X[0], WANNA_X[1], 400),
            color: P.bursztyn, width: 1.6 },
          { type: "line", data: U.fcurve(function (t) { return hSum(t, 1); }, WANNA_X[0], WANNA_X[1], 400),
            color: P.ref, width: 1.6, dash: [5, 4] },
          { type: "line", data: U.fcurve(function (t) { return clipH(hSum(t, w)); }, WANNA_X[0], WANNA_X[1], 800),
            color: P.ink, width: 3 },
          { type: "vline", x: m.t, from: 0, to: m.h, color: P.accent, width: 1.4, dash: [3, 3] },
          { type: "point", x: m.t, y: m.h, color: P.accent, size: 12 }
        );
        return {
          x: WANNA_X, y: WANNA_Y, xLabelEvery: 1000,
          layers: layers,
          legend: [
            { text: "wczesne defekty", color: P.terakota },
            { text: "awarie losowe", color: P.niebo },
            { text: "zużycie", color: P.bursztyn },
            { text: "suma", color: P.ink },
            { text: "suma przy nasileniu 1", color: P.ref, dash: true }
          ],
          xName: "Czas pracy", yName: "Hazard (jednostki względne)",
          aria: "Hazard wanny: suma trzech składowych przy nasileniu zużycia " + w + ", dno przy " + m.t + " godzinach."
        };
      },
      update: function (root, state) {
        var m = state.min;
        S.setValue(root, "t", "t ≈ " + (Math.round(m.t / 10) * 10));
        S.setValue(root, "h", U.fmt(m.h, 3));
        S.setValue(root, "dom", state.dom);
      }
    },

    c7_jeden_weibull: {
      draw: function (v, P, k, state, vals) {
        var b = Number(vals.c7p20_beta);
        var T0 = 1310, H0 = 0.184;
        var g = function (t) { return clipH(H0 * Math.pow(t / T0, b - 1)); };
        var dir = Math.abs(b - 1) < 1e-9 ? "stały" : (b < 1 ? "maleje" : "rośnie");
        var fit = Math.abs(b - 1) < 1e-9 ? "dna wanny" : (b < 1 ? "lewego ramienia" : "prawego ramienia");
        state.dir = dir; state.fit = fit;
        return {
          x: WANNA_X, y: WANNA_Y, xLabelEvery: 1000,
          layers: [
            { type: "line", data: U.fcurve(function (t) { return hSum(t, 1); }, WANNA_X[0], WANNA_X[1], 400),
              color: P.ref, width: 2.6 },
            { type: "line", data: U.fcurve(g, 5, WANNA_X[1], 400), color: P.bursztyn, width: 2.6 },
            { type: "point", x: T0, y: H0, color: P.ink, size: 11 },
            { type: "text", x: T0, y: H0, text: "punkt obrotu", color: P.ink, align: "left", halo: true }
          ],
          legend: [
            { text: "krzywa wannowa (suma trzech mechanizmów)", color: P.ref },
            { text: "hazard jednego Weibulla", color: P.bursztyn }
          ],
          xName: "Czas pracy", yName: "Hazard (jednostki względne)",
          aria: "Hazard jednego rozkładu Weibulla z kształtem " + b + " przechodzący przez dno wanny, na tle krzywej wannowej."
        };
      },
      update: function (root, state) {
        S.setValue(root, "dir", state.dir);
        S.setValue(root, "fit", state.fit);
      }
    },

    c7_termin: {
      draw: function (v, P, k, state, vals) {
        var t = Number(vals.c7p21a_t), rs = Number(vals.c7p21a_req);
        var rW = U.wR(t, WEIB.beta, WEIB.eta);
        var tW = U.wTime(rs, WEIB.beta, WEIB.eta), tE = U.wTime(rs, EXP.beta, EXP.eta);
        state.t = t; state.rW = rW; state.tW = tW; state.tE = tE;
        var X = [0, 3000];
        return {
          x: X, y: [0, 1], xLabelEvery: 500,
          layers: [
            { type: "line", data: U.fcurve(function (x) { return U.wR(x, EXP.beta, EXP.eta); }, X[0], X[1], 300),
              color: P.niebo, width: 1.8, dash: [5, 4] },
            { type: "line", data: U.fcurve(function (x) { return U.wR(x, WEIB.beta, WEIB.eta); }, X[0], X[1], 300),
              color: P.bursztyn, width: 2.6 },
            { type: "segment", from: [X[0], rs], to: [X[1], rs], color: P.subtle, width: 1.4 },
            { type: "text", x: X[1], y: rs, text: "wymaganie R* = " + rs.toFixed(2), color: P.soft, align: "right", halo: true },
            { type: "vline", x: tW, from: 0, to: rs, color: P.bursztyn, width: 1.4, dash: [3, 3] },
            { type: "point", x: tW, y: rs, color: P.bursztyn, size: 12 },
            { type: "text", x: tW, y: rs, text: U.fmtH(tW, 0), color: P.ink, align: "left", mono: true, halo: true },
            { type: "vline", x: tE, from: 0, to: rs, color: P.niebo, width: 1.4, dash: [3, 3] },
            { type: "point", x: tE, y: rs, color: P.niebo, size: 10 },
            { type: "vline", x: t, from: 0, to: rW, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "segment", from: [t, rW], to: [t, 1], color: P.accent, width: 5 },
            { type: "point", x: t, y: rW, color: P.accent, size: 12 }
          ],
          legend: [
            { text: "Weibull β = 2, η = 1700 h", color: P.bursztyn },
            { text: "wykładniczy, MTTF 1500 h", color: P.niebo, dash: true },
            { text: "ryzyko awarii przed przeglądem: 1 − R(t)", color: P.accent }
          ],
          xName: "Czas do przeglądu (h)", yName: "R(t)",
          aria: "Krzywe niezawodności: Weibull i wykładniczy. Wymaganie " + rs.toFixed(2) + " przecinają się z krzywą przy " + Math.round(tW) + " i " + Math.round(tE) + " godzinach. Przegląd po " + t + " godzinach."
        };
      },
      update: function (root, state) {
        S.setValue(root, "r", U.fmtP(state.rW));
        S.setValue(root, "risk", U.fmtP(1 - state.rW));
        S.setValue(root, "tw", U.fmtH(state.tW, 0));
        S.setValue(root, "te", U.fmtH(state.tE, 0));
      }
    },

    c7_termin_pole: {
      draw: function (v, P, k, state, vals) {
        var t = Number(vals.c7p21b_t);
        var f = function (x) { return U.wf(x, WEIB.beta, WEIB.eta); };
        var X = [0, 4000];
        var B10 = Math.round(U.wTime(0.9, WEIB.beta, WEIB.eta));
        state.t = t; state.F = U.wF(t, WEIB.beta, WEIB.eta); state.R = U.wR(t, WEIB.beta, WEIB.eta);
        return {
          x: X, y: [0, 0.0006], xLabelEvery: 500,
          layers: [
            { type: "line", data: U.fcurve(f, 0, t, 200), fill: P.accent, fillAlpha: 0.4, width: 0 },
            { type: "line", data: U.fcurve(f, t, X[1], 400), fill: P.szalwia, fillAlpha: 0.25, width: 0 },
            { type: "line", data: U.fcurve(f, X[0], X[1], 400), color: P.bursztyn, width: 2.6 },
            { type: "vline", x: B10, from: 0, to: f(B10), color: P.subtle, width: 1.4, dash: [3, 3] },
            { type: "text", x: B10, y: f(B10), text: "B10 = " + B10 + " h", color: P.soft, align: "left", halo: true },
            { type: "vline", x: t, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "text", x: t, y: "top", text: "przegląd po " + t + " h", color: P.ink, align: "left", mono: true }
          ],
          legend: [
            { text: "zawiedzie przed przeglądem", color: P.accent, box: true },
            { text: "dotrwa do przeglądu", color: P.szalwia, box: true },
            { text: "gęstość czasu życia (Weibull β = 2, η = 1700 h)", color: P.bursztyn }
          ],
          xName: "Czas do awarii (h)", yName: "Gęstość (1/h)",
          aria: "Gęstość czasu życia Weibulla z kreską przeglądu po " + t + " godzinach. Pole przed kreską to ryzyko awarii."
        };
      },
      update: function (root, state) {
        S.setValue(root, "risk", U.fmtP(state.F));
        S.setValue(root, "r", U.fmtP(state.R));
        S.setValue(root, "n", Math.round(100 * state.F) + " zawiedzie");
      }
    },

    c7_stary_nowy: {
      draw: function (v, P, k, state, vals) {
        var s = Number(vals.c7p22_s);
        var models = {
          wear: { beta: 2, eta: 1700, color: P.bursztyn },
          "const": { beta: 1, eta: 1500, color: P.niebo },
          early: { beta: 0.7, eta: 1500 / U.gammaFn(1 + 1 / 0.7), color: P.terakota }
        };
        var mk = models[vals.c7p22_model] || models.wear;
        var U500 = 500, X = [0, 1500];
        var used = U.wR(s + U500, mk.beta, mk.eta) / U.wR(s, mk.beta, mk.eta);
        var fresh = U.wR(U500, mk.beta, mk.eta);
        var diff = fresh - used;
        var verdict = diff > 0.0005 ? "pomaga" : (-diff > 0.0005 ? "szkodzi" : "nic nie zmienia");
        state.used = used; state.fresh = fresh; state.verdict = verdict;
        return {
          x: X, y: [0, 1], xLabelEvery: 250,
          layers: [
            { type: "line", data: U.fcurve(function (u) { return U.wR(s + u, mk.beta, mk.eta) / U.wR(s, mk.beta, mk.eta); }, X[0], X[1], 300),
              color: mk.color, width: 2.8 },
            // Krzywa nowego rysowana na wierzchu, żeby przerywana linia była widoczna także
            // wtedy, gdy obie krzywe się pokrywają (stały hazard).
            { type: "line", data: U.fcurve(function (u) { return U.wR(u, mk.beta, mk.eta); }, X[0], X[1], 300),
              color: P.ref, width: 2, dash: [5, 4] },
            { type: "vline", x: U500, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "text", x: U500, y: "top", text: "+500 h", color: P.ink, align: "left", mono: true },
            { type: "point", x: U500, y: used, color: mk.color, size: 12 },
            { type: "point", x: U500, y: fresh, color: P.ref, size: 11 }
          ],
          legend: [
            { text: "używany, wiek s", color: mk.color },
            { text: "nowy", color: P.ref, dash: true }
          ],
          xName: "Dalszy czas pracy (h)", yName: "Szansa dalszej pracy bez awarii",
          aria: "Niezawodność warunkowa używanego wentylatora w wieku " + s + " godzin i nowego, na kolejne 500 godzin."
        };
      },
      update: function (root, state) {
        S.setValue(root, "used", U.fmtP(state.used));
        S.setValue(root, "new", U.fmtP(state.fresh));
        S.setValue(root, "verdict", state.verdict);
      }
    }
  });
})();
