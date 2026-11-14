// Prototypy punktów stop wykładu 10, grupa rg (karta 2 i odnowa). Ryciny lcSF liczone
// w przeglądarce; kontenery w R: stop_proto_rg.R. Parametry przychodzą z R w data-config
// (i10p_cfg i pola podane w R); w tym pliku nie ma liczb modelu.
//
//   i10_zero   — Piaskownica „Zero wad w n kontrolach” (P6A): (1 − p)ⁿ, poziom 1 − conf,
//                górna granica p przy zerze wad.
//   i10_zero_n — Pojęcie „Dziesięć razy niżej, dziesięć razy więcej” (P6B): granica
//                jako funkcja n na osiach logarytmicznych (x = log10 n, y = log10 p + const).
//   i10_odnowa — Galeria „Trzy polityki odnowy” (P8): G(t) = R(T)^⌊t/T⌋ · R(t − T⌊t/T⌋).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcI10;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Reguła trzech: p ≈ 3/n (przybliżenie −ln(0.05)/n dla małych p).
  var RULE_OF_THREE = 3;
  // Granica przy zerze wad: trzy cyfry znaczące (0.259, 0.0295, 0.00299). U.fmtP zaokrągla
  // 0.0295 do 0.030, a tu granica ma być czytelna w tym samym rzędzie wielkości co wzór.
  function fmtSig(x) { return x.toPrecision(3); }

  // ---- P6A i P6B: granica przy zerze wad ---------------------------------------
  // Wspólne odczyty obu rycin: górna granica dla bieżącego n i reguła trzech.
  function readZero(root, state) {
    S.setValue(root, "up", fmtSig(state.up));
    S.setValue(root, "r3", fmtSig(RULE_OF_THREE / state.n));
  }

  // ---- P6B: pomocnicze przekształcenia osi logarytmicznych ---------------------
  function logN(n) { return Math.log10(n); }
  function logP(p, cfg) { return Math.log10(p / cfg.p_axis[0]); }  // oś y od 0 (p_axis[0])

  S.register({
    i10_zero: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, n = Number(vals.i10p6a_n);
        var alpha = 1 - cfg.conf, up = U.zeroUpper(n, cfg.conf);
        var clip = Math.min(up, cfg.p_max);  // rysunek przycięty do osi p
        var pts = [], N = 300, i, p;
        for (i = 0; i <= N; i++) { p = cfg.p_max * i / N; pts.push([p, Math.pow(1 - p, n)]); }
        state.n = n; state.up = up;
        return {
          x: [0, cfg.p_max], y: [0, 1],
          xName: "Prawdziwe p (udział wad)", yName: "P(zero wad w n kontrolach)",
          layers: [
            { type: "band", lo: 0, hi: clip, color: P.szalwia, alpha: 0.18 },
            { type: "line", data: pts, color: P.secondary, width: 2.4 },
            { type: "line", data: [[0, alpha], [cfg.p_max, alpha]], color: P.ink, width: 1.4, dash: [6, 4] },
            { type: "text", x: cfg.p_max, y: alpha, text: "poziom " + S.fmt(alpha, 3), color: P.ink, align: "right", mono: true },
            { type: "vline", x: clip, from: 0, to: alpha, color: P.accent, width: 2 },
            { type: "point", x: clip, y: alpha, color: P.accent, size: 9 },
            // Przy prawej krawędzi napis staje po lewej stronie kreski, żeby nie wyszedł poza wykres.
            // Kotwica napisu tuż nad kreską poziomu (przesunięcie w jednostkach osi y).
            { type: "text", x: clip, y: alpha + 0.06, text: "p górne = " + fmtSig(up), color: P.accent,
              align: clip > cfg.p_max * 0.8 ? "right" : "left", mono: true, bold: true, halo: true }
          ],
          legend: [{ text: "P(zero wad) przy danym p", color: P.secondary },
                   { text: "p zgodne z wynikiem", color: P.szalwia, box: true },
                   { text: "górna granica", color: P.accent }],
          aria: "Zero wad w " + n + " kontrolach: górna granica " + fmtSig(up) +
                ". Krzywa (1 − p) do potęgi n przecina poziom " + S.fmt(alpha, 3) + " w tym punkcie."
        };
      },
      update: function (root, state) { readZero(root, state); }
    },

    i10_zero_n: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, n = Number(vals.i10p6b_n);
        var lo = Math.min.apply(null, cfg.marks), hi = Math.max.apply(null, cfg.marks);
        var up = U.zeroUpper(n, cfg.conf);
        var pLo = cfg.p_axis[0], pHi = cfg.p_axis[1];
        var yMax = logP(pHi, cfg), xs = [logN(lo), logN(hi)];
        var curve = [], rule = [], N = 200, i, nn;
        for (i = 0; i <= N; i++) {
          nn = Math.pow(10, xs[0] + (xs[1] - xs[0]) * i / N);
          curve.push([logN(nn), logP(U.zeroUpper(nn, cfg.conf), cfg)]);
          rule.push([logN(nn), logP(RULE_OF_THREE / nn, cfg)]);
        }
        // Etykiety osi: podziałki co dekadę od p_axis[0] do p_axis[1].
        var yLabels = {}, d;
        for (d = 0; d <= yMax + 1e-9; d++) yLabels[d] = S.fmt(pLo * Math.pow(10, d), 3);
        var xLabels = {};
        cfg.marks.forEach(function (m) { xLabels[logN(m)] = String(m); });
        // Punkty znaczników: napisy z prawej strony, a przy prawej krawędzi z lewej.
        var marks = cfg.marks.map(function (m) {
          var p = U.zeroUpper(m, cfg.conf);
          return { x: logN(m), y: logP(p, cfg), text: fmtSig(p) };
        });
        var layers = [
          { type: "line", data: rule, color: P.ref, width: 1.6, dash: [5, 4] },
          { type: "line", data: curve, color: P.secondary, width: 2.4 },
          { type: "points", data: marks.map(function (m) { return [m.x, m.y]; }), color: P.secondary, size: 6 }
        ];
        marks.forEach(function (m, j) {
          layers.push({ type: "text", x: m.x, y: m.y, text: m.text, color: P.secondary,
            align: j === marks.length - 1 ? "right" : "left", mono: true, bold: true, halo: true });
        });
        layers.push(
          { type: "vline", x: logN(n), from: 0, to: logP(up, cfg), color: P.accent, width: 1.2, dash: [4, 3] },
          { type: "point", x: logN(n), y: logP(up, cfg), color: P.accent, size: 12 });
        state.n = n; state.up = up;
        return {
          x: xs, y: [0, yMax], xLabels: xLabels, yLabels: yLabels,
          xName: "Liczba kontroli bez wady n", yName: "Górna granica dla p",
          layers: layers,
          legend: [{ text: "granica " + Math.round(cfg.conf * 100) + "% przy zerze wad", color: P.secondary },
                   { text: "reguła trzech 3/n", color: P.ref, dash: true }],
          aria: "Górna granica dla p przy zerze wad na osiach logarytmicznych; dla n = " + n +
                " granica " + fmtSig(up) + "."
        };
      },
      update: function (root, state) { readZero(root, state); }
    },

    i10_odnowa: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, model = v === "exp" ? "exp" : "weibull";
        var T = cfg.time_max, ps = cfg.periods, step = 10;
        // Polityka z okresem wymiany T (null = bez wymiany).
        function G(t, per) {
          if (!per) return U.fanR(cfg, t, model);
          var j = Math.floor(t / per);
          return Math.pow(U.fanR(cfg, per, model), j) * U.fanR(cfg, t - per * j, model);
        }
        function curve(per) {
          var pts = [], t;
          for (t = 0; t <= T; t += step) pts.push([t, G(t, per)]);
          return pts;
        }
        var ends = [G(T, null), G(T, ps[0]), G(T, ps[1])];
        var cols = [P.secondary, P.niebo, P.szalwia];
        var layers = [], x;
        // Pionowe kreski przy wymianach co ps[1].
        for (x = ps[1]; x < T; x += ps[1]) layers.push({ type: "vline", x: x, color: P.subtle, width: 1, dash: [2, 3] });
        layers.push(
          { type: "line", data: curve(null), color: cols[0], width: 2.4 },
          { type: "line", data: curve(ps[0]), color: cols[1], width: 2.4 },
          { type: "line", data: curve(ps[1]), color: cols[2], width: 2.4 });
        // Napisy na prawym końcu; dla modelu wykładniczego krzywe się pokrywają: jeden napis.
        var endIdx = model === "exp" ? [2] : [0, 1, 2];
        endIdx.forEach(function (i) {
          layers.push({ type: "text", x: T, y: ends[i], text: U.fmtP(ends[i]), color: cols[i],
            align: "right", mono: true, bold: true, halo: true });
        });
        state.ends = ends;
        return {
          x: [0, T], y: [0, 1], xLabelEvery: ps[1],
          xName: "Łączny czas pracy (" + cfg.unit + ")", yName: "P(bez awarii do tej chwili)",
          layers: layers,
          legend: [{ text: "bez wymiany", color: cols[0] },
                   { text: "wymiana co " + ps[0] + " " + cfg.unit, color: cols[1] },
                   { text: "wymiana co " + ps[1] + " " + cfg.unit, color: cols[2] }],
          aria: "Trzy polityki odnowy, model " + (model === "exp" ? "wykładniczy" : "Weibulla") +
                ", czas pracy " + T + " " + cfg.unit + ": bez wymiany " + U.fmtP(ends[0]) +
                ", wymiana co " + ps[0] + " " + cfg.unit + " " + U.fmtP(ends[1]) +
                ", wymiana co " + ps[1] + " " + cfg.unit + " " + U.fmtP(ends[2]) + "."
        };
      },
      update: function (root, state) {
        S.setValue(root, "a", U.fmtP(state.ends[0]));
        S.setValue(root, "b", U.fmtP(state.ends[1]));
        S.setValue(root, "c", U.fmtP(state.ends[2]));
      }
    }
  });
})();
