// Ryciny wykładu 04 liczone w przeglądarce (silnik lcSF): sf_examples
//
//   s4-case — przykłady z hipotezą (rozdz. 3: A1, B1–C2, rozdz. 4: A1, B1–C1). Rycina krokowa
//             (lc_step_widget): kolejne kroki budują przedział, na ostatnim kroku
//             przełącznik „Hipoteza 1/2” zacienia obszar hipotezy, a „Pokaż werdykt”
//             odsłania opis policzony w R (notes, data-when = "h<i>" / "h<i>r").
//
// Dane przykładów są stałe, więc R liczy wszystko z góry (ch3_case_config(),
// ch4_case_config()): punkty prób, przedziały, zakresy osi dla każdej hipotezy
// i napisy. Tu tylko składamy warstwy według kroku i wybranej hipotezy.
// Typy: single_mean, diff_means, forest (średnie) oraz single_prop,
// diff_props, forest_prop (proporcje).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF;
  if (!C || !S) return;

  // Kolory ról jak w helpers.R (col_ci, col_miss, col_hit, col_estimate) i obszar
  // hipotezy (#8e44ad w R, w motywie indygo — tak mapował go most ggplot→ECharts).
  function roles(P) {
    return { ci: P.niebo, miss: P.terakota, hit: P.szalwia, est: P.bursztyn, sec: P.secondary,
             hyp: C.token("--upwr-cat-indygo", "#3a5a8a"),
             // Napisy grup jak Ryc. 3.3: ciemniejsze odcienie kolorów kropek.
             grp1: C.token("--upwr-cat-indygo", "#3a5a8a"), grp2: C.token("--upwr-cat-terakota", "#b25838"),
             // Zero na osi różnicy: przerywana szara linia jak „brak różnicy” na Ryc. 3.3.
             zeroLine: function (y) { return { type: "vline", x: 0, from: y[0], to: y[1], color: P.ref, width: 1.3, dash: [4, 4] }; } };
  }
  // Rozmiary jak w moście ggplot→ECharts: punkt o średnicy 2.1 · size,
  // napis fs · size / 4.5 (nie mniejszy niż fs), linia 2 · linewidth. Napisy zwykłą
  // czcionką, jak rysował je most (fontface z ggplot nie przechodził).
  function txt(x, y, text, color, size, o) {
    o = o || {};
    return { type: "text", x: x, y: y, text: text, color: color, size: Math.max(1, (size || 3.9) / 4.5),
             align: o.align, valign: o.valign || "middle" };
  }
  // Obszar hipotezy: pas od granicy w stronę hipotezy, linia i napis „≥ 168” przy krawędzi
  // wykresu, po stronie obszaru: domyślnie tuż nad osią (u góry wchodził na napis estymaty),
  // edge = "top" tam, gdzie przy osi stoją kropki próby.
  function hypLayers(h, x, edge, R, size) {
    if (!h) return [];
    var lab = txt(h.bound, edge === "top" ? "top" : "bottom", h.label, R.hyp, size || 4.5, { align: h.dir === "gt" ? "left" : "right" });
    lab.bold = true; lab.halo = true;
    return [{ type: "band", lo: h.dir === "gt" ? h.bound : x[0], hi: h.dir === "gt" ? x[1] : h.bound,
              color: R.hyp, alpha: 0.15 },
            { type: "vline", x: h.bound, color: R.hyp, width: 2 },
            lab];
  }
  function axis(x, y, xName, layers, aria) {
    return { x: x, y: y, xName: xName, hideY: true, noYGrid: true, layers: layers, aria: aria };
  }
  // Oś z nazwami wierszy po lewej (grupy, liczebności): napisy nie wchodzą na przedziały.
  function rowAxis(x, y, xName, names, layers, aria) {
    var yl = {};
    names.forEach(function (t, i) { yl[i + 1] = t; });
    return { x: x, y: y, xName: xName, noYGrid: true, yLabels: yl, layers: layers, aria: aria };
  }
  // Słupki kategorii: centra cx, szerokość w, wysokości ys; liczby nad słupkami.
  function bars(cx, w, ys, colors, R) {
    var layers = [{ type: "bins", data: cx.map(function (c, i) { return [c - w / 2, c + w / 2, ys[i]]; }), colors: colors }];
    cx.forEach(function (c, i) { layers.push(txt(c, ys[i], String(ys[i]), R.sec, 4.6, { valign: "bottom" })); });
    return layers;
  }
  function barAxis(layers, ys, labels, aria) {
    var top = Math.max.apply(null, ys) * 1.15;
    return { x: [0.4, 2.6], y: [0, top], xLabels: { 1: labels[0], 2: labels[1] }, noXGrid: true,
             yName: "Liczebność", layers: layers, aria: aria };
  }
  function range(n) { var out = []; for (var i = 1; i <= n; i++) out.push(i); return out; }

  // ---- Wygląd przedziałów (ci_look.js, jak Ryc. 3.2–4.3) ---------------------------
  // Pomiary jako stos kropek na osi, estymata jako gruba czarna kreska, ± SE jako
  // czarna klamra (tylko w swoim kroku), przedział jako burgundowa klamra z granicami.
  var K = window.lc04Look;
  function xs(pts) { return pts.map(function (p) { return p[0]; }); }
  // Oś jednej estymaty (średnia, proporcja, różnica): kreska od dołu osi do belki, napis
  // estymaty w kroku est, ± SE w kroku se, przedział od kroku ci. y0 — dół osi, yb — belka.
  function oneAxis(layers, c, P, o) {
    var step = o.step, x = o.x, yb = o.yb, d = o.d;
    if (step < o.estStep) return layers;
    layers.push(K.est(x, o.y0, yb, P));
    if (step >= o.ciStep) layers = layers.concat(K.ci(c.lo, c.hi, yb, P, { d: d }));
    else if (step === o.seStep) layers = layers.concat(K.se(x - c.se, x + c.se, yb, P, "± SE = " + K.num(c.se, d), { lift: o.lift }));
    if (step < o.seStep || step >= o.ciStep) layers.push(K.label(x, step >= o.ciStep ? yb + o.lift : yb, o.lab, P));
    return layers;
  }

  // ---- Rozdział 3: średnie ----------------------------------------------------
  var TYPES = {
    // Krok 1 punkty, 2 średnia, 3 ± SE, 4 przedział.
    single_mean: function (c, k, step, h, L, R, P) {
      var y0 = c.y[0], yb = 0.42, layers = hypLayers(h, L.x, "top", R), aria = "Wzrost " + c.pts.length + " osób na osi";
      layers.push(K.dots(xs(c.pts), y0, R.ci));
      layers = oneAxis(layers, c, P, { step: step, x: c.xbar, y0: y0, yb: yb, d: 2, lift: 0.08,
        estStep: 2, seStep: 3, ciStep: 4, lab: c.lab_mean });
      if (step >= 2) aria += "; " + c.lab_mean;
      if (step >= 4) aria += "; " + c.lab_ci;
      // Wolny pas nad napisem x̄ na napis hipotezy (przy osi stoją kropki).
      return axis(L.x, [c.y[0], c.y[1] + 0.25], c.xlab, layers, aria);
    },
    // Wykres 0: obie próby (krok 1) i średnie (krok 2+); wykres 1 od kroku 3: różnica,
    // ± SE (4) i przedział (5).
    diff_means: function (c, k, step, h, L, R, P) {
      if (k === 0) {
        var x0 = c.top.raw[0], b1 = 1.32, b2 = c.y[0], layers = [
          K.value(x0, 2.12, c.label1, R.grp1, { align: "left", size: 1.05 }),
          K.value(x0, 1.08, c.label2, R.grp2, { align: "left", size: 1.05 }),
          K.dots(xs(c.pts1), b1, R.ci, { r: 4 }), K.dots(xs(c.pts2), b2, R.est, { r: 4 })];
        if (step >= 2) {
          layers.push(K.est(c.x1, b1, b1 + 0.62, P), K.est(c.x2, b2, b2 + 0.62, P),
                      K.label(c.x1, b1 + 0.62, c.lab_x1, P, { size: 1.1 }), K.label(c.x2, b2 + 0.62, c.lab_x2, P, { size: 1.1 }));
        }
        return axis(c.top.x, c.y, c.xlab, layers, "Dwie próby: " + c.label1 + " i " + c.label2 +
          (step >= 2 ? "; " + c.lab_x1 + ", " + c.lab_x2 : ""));
      }
      if (step < 3) return null;
      var lb = hypLayers(h, L.x, 0.5, R);
      if (!h) lb.push(R.zeroLine(c.y_bot));
      lb = oneAxis(lb, { lo: c.lo, hi: c.hi, se: c.se }, P, { step: step, x: c.diff, y0: c.y_bot[0], yb: 0.12, d: 2,
        lift: 0.1, estStep: 3, seStep: 4, ciStep: 5, lab: c.lab_diff });
      return axis(L.x, c.y_bot, c.xlab_bot, lb, c.lab_diff + (step >= 5 ? "; " + c.lab_ci : ""));
    },
    // Krok 1 punkty grup, 2 średnie, 3 przedziały.
    forest: function (c, k, step, h, L, R, P) {
      var g = c.groups.length, ys = range(g);
      var layers = hypLayers(h, L.x, g + 0.45, R);
      // Wiersz grupy: kropki na dole wiersza, średnia jako kreska do belki przedziału.
      ys.forEach(function (y) {
        layers.push(K.dots(c.pts.filter(function (p) { return Math.round(p[1]) === y; }).map(function (p) { return p[0]; }),
                           y - 0.4, R.ci, { r: 3.5, alpha: step >= 3 ? 0.55 : 0.85 }));
        if (step >= 2) layers.push(K.est(c.means[y - 1], y - 0.4, y + 0.14, P));
        if (step >= 3) layers = layers.concat(K.ci(c.lo[y - 1], c.hi[y - 1], y + 0.14, P, { size: 1.05, leg: 10 }));
      });
      return rowAxis(L.x, c.y, c.xlab, c.groups, layers, "Grupy: " + c.groups.join(", ") +
        (step >= 3 ? "; przedziały 95% dla średnich" : step >= 2 ? "; średnie grup" : "; punkty pomiarów"));
    },

    // ---- Rozdział 4: proporcje ------------------------------------------------
    // Wykres 0: słupki TAK/NIE; wykres 1: p̂ (krok 2+), ± SE (3), przedział (4).
    single_prop: function (c, k, step, h, L, R, P) {
      if (k === 0) {
        return barAxis(bars([1, 2], 0.6, c.counts, [R.miss, R.ci], R), c.counts, ["NIE", "TAK"],
          "Liczba odpowiedzi NIE: " + c.counts[0] + ", TAK: " + c.counts[1]);
      }
      var layers = hypLayers(h, L.x, 0.5, R), aria = "Oś proporcji";
      layers = oneAxis(layers, c, P, { step: step, x: c.phat, y0: c.y[0], yb: 0.05, d: 3, lift: 0.12,
        estStep: 2, seStep: 3, ciStep: 4, lab: c.lab_phat });
      if (step >= 2) aria += "; " + c.lab_phat;
      if (step >= 4) aria += "; " + c.lab_ci;
      return axis(L.x, c.y, c.xlab, layers, aria);
    },
    // Wykres 0: słupki dwóch grup; 1: dwie p̂ (krok 2+); 2: różnica (3), ± SE (4), przedział (5).
    diff_props: function (c, k, step, h, L, R, P) {
      if (k === 0) {
        var d = 0.1875;
        return barAxis(bars([1 - d, 1 + d, 2 - d, 2 + d], 0.325, c.counts, [R.miss, R.ci, R.miss, R.ci], R),
          c.counts, [c.label1, c.label2],
          c.label1 + ": NIE " + c.counts[0] + ", TAK " + c.counts[1] + "; " + c.label2 + ": NIE " + c.counts[2] + ", TAK " + c.counts[3]);
      }
      if (k === 1) {
        var top = [];
        if (step >= 2) {
          top.push(K.mark(c.p1, 1, P, 14, 14), K.value(c.p1, 1.42, c.lab_p1, P.ink, { size: 1.05 }),
                   K.mark(c.p2, 2, P, 14, 14), K.value(c.p2, 2.42, c.lab_p2, P.ink, { size: 1.05 }));
        }
        return { x: [-0.05, 1.05], y: c.y_top, xName: c.xlab, noYGrid: true,
                 yLabels: { 1: c.label1, 2: c.label2 }, layers: top,
                 aria: step >= 2 ? c.lab_p1 + ", " + c.lab_p2 : "Oś proporcji dla dwóch grup" };
      }
      var lb = [R.zeroLine(c.y_bot)].concat(hypLayers(h, L.x, 0.42, R, 4));
      lb = oneAxis(lb, { lo: c.lo, hi: c.hi, se: c.se }, P, { step: step, x: c.diff, y0: c.y_bot[0], yb: -0.02, d: 3,
        lift: 0.26, estStep: 3, seStep: 4, ciStep: 5, lab: c.lab_diff });
      return axis(L.x, c.y_bot, c.xlab_bot, lb,
        step >= 5 ? c.lab_ci : step >= 3 ? c.lab_diff : "Oś różnicy proporcji z linią zera");
    },
    // Krok 1 nazwy i liczby, 2 proporcje, 3 przedziały.
    forest_prop: function (c, k, step, h, L, R, P) {
      var g = c.lo.length, ys = range(g);
      var layers = hypLayers(h, L.x, g + 0.45, R);
      ys.forEach(function (y) {
        if (step >= 2) layers.push(K.mark(c.phat[y - 1], y, P));
        if (step >= 3) layers = layers.concat(K.ci(c.lo[y - 1], c.hi[y - 1], y, P, { d: 3, size: 1, leg: 12 }));
      });
      return rowAxis(L.x, c.y, c.xlab, c.lab_groups, layers, c.lab_groups.join("; ") +
        (step >= 3 ? "; przedziały 95% dla proporcji" : ""));
    }
  };

  // Hipoteza liczy się tylko na ostatnim kroku (pełny przedział).
  function activeKey(state, step) {
    return step === state.config.n && state.hyp ? state.hyp : null;
  }

  var CASE = {
    init: function (state) { state.hyp = null; state.revealed = false; state.step = 1; },
    // Przełącznik hipotezy (hyp1, hyp2): jeden aktywny albo żaden; zmiana chowa werdykt.
    act: function (state, name) {
      if (name.indexOf("hyp") === 0) {
        var v = name.slice(3);
        state.hyp = state.hyp === v ? null : v;
        state.revealed = false;
      } else if (name === "reveal") {
        state.revealed = true;
      }
    },
    draw: function (v, P, k, state, vals, step) {
      var c = state.config, key = activeKey(state, step), h = key ? c.hyps[Number(key) - 1] : null;
      state.step = step;
      // Hipoteza „które grupy się różnią” (pairwise) nie ma obszaru na wykresie.
      if (h && h.pairwise) h = null;
      return TYPES[c.type](c, k, step, h, c.lims[key || "0"], roles(P), P);
    },
    text: function (state, v, vals, step) {
      return step === state.config.n ? S.esc(state.config.text) : "";
    },
    noteKey: function (vals, state) {
      var key = activeKey(state, state.step);
      return key ? "h" + key + (state.revealed ? "r" : "") : "";
    }
  };

  S.register({ "s4-case": CASE });
})();
