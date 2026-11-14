// Prototypy punktów stop wykładu 06, grupa rd (P9A, P9B, P10A, P10B, P13). Ryciny lcSF
// liczone w przeglądarce; kontenery w R: lc_sf_figure() w stop_proto_rd.R.
//
//   z6_interwencje   — galeria „Trzy interwencje” (P9A) i próg 80°C (P10B): warianty
//                      z konfiguracji, wybór kluczem v (pierwsza kontrolka), odczyty.
//   z6_interwencje_b — cztery małe wykresy obok siebie (P9B): wariant k z konfiguracji,
//                      wynik zapisany nad każdym wykresem.
//   z6_prog_ponizej  — przypadek brzegowy „Próg poniżej średniej” (P10A): próg z wyboru,
//                      σ z suwaka, stała średnia 82.
//   z6_nakladanie    — „Pole nakładania to nie ryzyko” (P13): gęstości L i S oraz
//                      margines D = S − L, układ a lub zamieniony układ b.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcZ6;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Wspólne osie w całym wykładzie: temperatura 68–98°C.
  var X = [68, 98];

  // Wariant o kluczu key; gdy brak, pierwszy z listy.
  function pick(cfg, key) {
    var list = cfg.variants || [];
    for (var i = 0; i < list.length; i++) if (list[i].key === key) return list[i];
    return list[0];
  }

  // Warstwy galerii interwencji: stały kontur bazowy, pole i krzywa wariantu, strzałki zmian.
  // grid = true: mały wykres z wynikiem nad nim, bez napisu progu.
  // W galerii (grid) oś y ma zapas nad ymax na napis wyniku; kreski progu kończą się pod napisem.
  function interventionLayers(cfg, w, P, grid) {
    var b = cfg.base, p = 1 - U.pnorm(w.c, w.mu, w.sd), top = cfg.ymax, out = [];
    var lineTop = grid ? top + 0.01 : top;
    out.push({ type: "line", data: U.curve(b.mu, b.sd, X[0], X[1]), color: P.ref, width: 1.6, dash: [5, 4] });
    out.push({ type: "vline", x: b.c, color: P.ref, width: 1.2, dash: [5, 4], to: lineTop });
    out.push({ type: "line", data: U.curve(w.mu, w.sd, w.c, X[1]), color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.4 });
    out.push({ type: "line", data: U.curve(w.mu, w.sd, X[0], X[1]), color: P.secondary, width: 2.6 });
    // Przy zmianie μ: pozioma strzałka od starej do nowej średniej na wysokości szczytu.
    if (w.mu !== b.mu) {
      var peak = U.dnorm(w.mu, w.mu, w.sd);
      out.push({ type: "arrow", from: [b.mu, peak], to: [w.mu, peak], color: P.ink, width: 2 });
    }
    // Przy zmianie progu: pozioma strzałka od starego do nowego progu wysoko nad osią.
    if (w.c !== b.c) {
      var yArrow = top * 0.9;
      out.push({ type: "arrow", from: [b.c, yArrow], to: [w.c, yArrow], color: P.ink, width: 2 });
    }
    out.push({ type: "vline", x: w.c, color: P.ink, width: 1.8, dash: [6, 4], to: lineTop });
    if (grid) {
      out.push({ type: "text", x: X[1], y: "top", text: "P = " + U.fmtP(p) + " · " + U.per1000(p),
        color: P.ink, align: "right", mono: true, halo: true, size: 0.9 });
    } else {
      out.push({ type: "text", x: w.c, y: "top", text: "próg " + w.c + "°C", color: P.ink, align: w.c < w.mu ? "right" : "left", mono: true });
    }
    return out;
  }

  function interventionSpec(cfg, w, P, grid) {
    return {
      x: X, y: [0, grid ? cfg.ymax + 0.05 : cfg.ymax], xLabelEvery: grid ? 10 : 5, alignLeft: grid,
      layers: interventionLayers(cfg, w, P, grid),
      xName: grid ? "Temperatura (°C)" : "Temperatura łożyska (°C)",
      yName: grid ? "" : "Gęstość",
      aria: "Krzywa normalna N(" + w.mu + ", " + w.sd + ") wariantu " + w.label + " z polem ponad progiem " +
        w.c + " stopni, na tle stanu bazowego N(" + cfg.base.mu + ", " + cfg.base.sd + ")."
    };
  }

  // Odczyty galerii: μ / σ / próg, z progu, P(T > c), częstość i co się ruszyło.
  function interventionReadouts(root, w) {
    var p = 1 - U.pnorm(w.c, w.mu, w.sd);
    S.setValue(root, "musp", S.fmt(w.mu, 2) + " / " + S.fmt(w.sd, 2) + " / " + S.fmt(w.c, 2));
    S.setValue(root, "z", U.fmt((w.c - w.mu) / w.sd, 2));
    S.setValue(root, "p", U.fmtP(p));
    S.setValue(root, "f", U.per1000(p));
    S.setValue(root, "moved", w.moved);
  }

  // ---- P10A: próg poniżej średniej ----------------------------------------------
  var MU_10A = 82, SD_BASE_10A = 3, YMAX_10A = 0.28;

  // ---- P13: nakładanie gęstości i margines ------------------------------------
  // Układ a: L ~ N(85, 8), S ~ N(95, 7); układ b: średnie zamienione.
  function layoutOf(v) {
    return v === "b" ? { mL: 95, mS: 85 } : { mL: 85, mS: 95 };
  }

  // Pole wspólne min(f_L, f_S): całka numeryczna po przedziale 50–130 kN.
  function overlap(a, b) {
    var s = 0, i, x, h = 0.05;
    for (i = 0; i <= 1600; i++) {
      x = 50 + i * h;
      s += Math.min(U.dnorm(x, a.m, a.s), U.dnorm(x, b.m, b.s)) * h;
    }
    return s;
  }

  // Wielobok pod min obu gęstości (pole wspólne), wypełniany szaro.
  function overlapPoints(a, b) {
    var pts = [[50, 0]], i, x;
    for (i = 0; i <= 160; i++) {
      x = 50 + i * 0.5;
      pts.push([x, Math.min(U.dnorm(x, a.m, a.s), U.dnorm(x, b.m, b.s))]);
    }
    pts.push([130, 0]);
    return pts;
  }

  S.register({
    z6_interwencje: {
      draw: function (v, P, k, state) {
        var w = pick(state.config, v);
        return interventionSpec(state.config, w, P, false);
      },
      update: function (root, state, v) {
        interventionReadouts(root, pick(state.config, v));
      }
    },

    z6_interwencje_b: {
      draw: function (v, P, k, state) {
        var w = state.config.variants[k];
        return interventionSpec(state.config, w, P, true);
      }
    },

    z6_prog_ponizej: {
      draw: function (v, P, k, state, vals) {
        var c = Number(v), sd = Number(vals.z6p10_sd);
        state.sd = sd; state.c = c;
        return {
          x: X, y: [0, YMAX_10A], xLabelEvery: 5,
          layers: [
            { type: "line", data: U.curve(MU_10A, SD_BASE_10A, X[0], X[1]), color: P.ref, width: 1.6, dash: [5, 4] },
            { type: "line", data: U.curve(MU_10A, sd, c, X[1]), color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.4 },
            { type: "line", data: U.curve(MU_10A, sd, X[0], X[1]), color: P.secondary, width: 2.6 },
            { type: "vline", x: c, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "text", x: c, y: "top", text: "próg " + c + "°C", color: P.ink, align: c < MU_10A ? "right" : "left", mono: true }
          ],
          xName: "Temperatura łożyska (°C)", yName: "Gęstość",
          aria: "Krzywa normalna N(82, " + sd + ") z polem ponad progiem " + c + " stopni, na tle stanu bazowego N(82, 3)."
        };
      },
      update: function (root, state, v) {
        var c = Number(v), sd = state.sd, p = 1 - U.pnorm(c, MU_10A, sd);
        var verdict = c > MU_10A ? "zmniejsza ryzyko" : c < MU_10A ? "zwiększa ryzyko" : "nic nie zmienia: 0.5";
        S.setValue(root, "z", U.fmt((c - MU_10A) / sd, 2));
        S.setValue(root, "p", U.fmtP(p));
        S.setValue(root, "zw", verdict);
      }
    },

    z6_nakladanie: {
      draw: function (v, P, k, state) {
        var L = layoutOf(v), sdL = state.config.sdL, sdS = state.config.sdS;
        if (k === 0) {
          return {
            x: [50, 130], y: [0, 0.065], xName: "Siła (kN)", xLabelEvery: 10,
            layers: [
              { type: "polygon", points: overlapPoints({ m: L.mL, s: sdL }, { m: L.mS, s: sdS }), fill: C.alpha(P.subtle, 0.35) },
              { type: "line", data: U.curve(L.mL, sdL, 50, 130), color: P.bursztyn, width: 2.4 },
              { type: "line", data: U.curve(L.mS, sdS, 50, 130), color: P.niebo, width: 2.4 },
              { type: "text", x: L.mL, y: U.dnorm(L.mL, L.mL, sdL), text: "L", color: P.bursztyn, bold: true },
              { type: "text", x: L.mS, y: U.dnorm(L.mS, L.mS, sdS), text: "S", color: P.niebo, bold: true }
            ],
            legend: [
              { text: "obciążenie L", color: P.bursztyn },
              { text: "wytrzymałość S", color: P.niebo },
              { text: "pole wspólne", color: P.subtle, box: true }
            ],
            aria: "Gęstości obciążenia L o średniej " + L.mL + " i wytrzymałości S o średniej " + L.mS +
              " kN, z zaznaczonym polem wspólnym."
          };
        }
        // Margines D = S − L: normalny o średniej μ_S − μ_L i odchyleniu √(σ_S² + σ_L²).
        var mD = L.mS - L.mL, sdD = Math.sqrt(sdL * sdL + sdS * sdS);
        return {
          x: [-45, 45], y: [0, 0.045], xName: "Margines D (kN)", xLabelEvery: 10,
          layers: [
            { type: "line", data: U.curve(mD, sdD, -45, 0), color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.45 },
            { type: "line", data: U.curve(mD, sdD, -45, 45), color: P.secondary, width: 2.4 },
            { type: "vline", x: 0, color: P.ink, width: 1.8, dash: [6, 4] },
            { type: "text", x: 0, y: "top", text: "D = 0", color: P.ink, align: "left", mono: true }
          ],
          aria: "Gęstość marginesu D = S − L o średniej " + mD + " kN; pole na lewo od zera to awarie."
        };
      },
      update: function (root, state, v) {
        var L = layoutOf(v), sdL = state.config.sdL, sdS = state.config.sdS;
        var mD = L.mS - L.mL, sdD = Math.sqrt(sdL * sdL + sdS * sdS);
        var pl = U.pnorm(0, mD, sdD);
        S.setValue(root, "ov", overlap({ m: L.mL, s: sdL }, { m: L.mS, s: sdS }).toFixed(3));
        S.setValue(root, "pl", U.fmtP(pl));
        S.setValue(root, "f", U.per1000(pl));
      }
    }
  });
})();
