// Ryciny wykładu 04 liczone w przeglądarce (silnik lcSF): sf_examples
//
//   s4-case — przykłady z hipotezą (rozdz. 3: A1–C2, rozdz. 4: A1–C1). Rycina krokowa
//             (lc_step_widget): kolejne kroki budują przedział, na ostatnim kroku
//             przełącznik „Hipoteza 1/2” zacienia obszar hipotezy, a „Pokaż werdykt”
//             odsłania opis policzony w R (notes, data-when = "h<i>" / "h<i>r").
//
// Dane przykładów są stałe, więc R liczy wszystko z góry (ch3_case_config(),
// ch4_case_config()): punkty prób, przedziały, zakresy osi dla każdej hipotezy
// i napisy. Tu tylko składamy warstwy według kroku i wybranej hipotezy.
// Typy: single_mean, compare_n, diff_means, forest (średnie) oraz single_prop,
// compare_n_prop, diff_props, forest_prop (proporcje).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF;
  if (!C || !S) return;

  // Kolory ról jak w helpers.R (col_ci, col_miss, col_hit, col_estimate) i obszar
  // hipotezy (#8e44ad w R, w motywie indygo — tak mapował go most ggplot→ECharts).
  function roles(P) {
    return { ci: P.niebo, miss: P.terakota, hit: P.szalwia, est: P.bursztyn, sec: P.secondary,
             hyp: C.token("--upwr-cat-indygo", "#3a5a8a"), zero: "#9b59b6" };
  }
  // Rozmiary jak w moście ggplot→ECharts: punkt o średnicy 2.1 · size,
  // napis fs · size / 4.5 (nie mniejszy niż fs), linia 2 · linewidth. Napisy zwykłą
  // czcionką, jak rysował je most (fontface z ggplot nie przechodził).
  function txt(x, y, text, color, size, o) {
    o = o || {};
    return { type: "text", x: x, y: y, text: text, color: color, size: Math.max(1, (size || 3.9) / 4.5),
             align: o.align, valign: o.valign || "middle" };
  }
  function dot(x, y, color, size, diamond) {
    return { type: "point", x: x, y: y, color: color, size: 2.1 * size, symbol: diamond ? "diamond" : "circle" };
  }
  function dots(data, color, size, alpha) {
    return { type: "points", data: data, color: color, size: 2.1 * size, alpha: alpha };
  }
  // geom_errorbar(orientation = "y"): odcinek i dwa wąsy o łącznej wysokości w.
  function errbar(rows, w, color, lw, alpha) {
    var items = [];
    rows.forEach(function (r) {
      var lo = r[0], hi = r[1], y = r[2];
      items.push({ from: [lo, y], to: [hi, y] },
                 { from: [lo, y - w / 2], to: [lo, y + w / 2] }, { from: [hi, y - w / 2], to: [hi, y + w / 2] });
    });
    return { type: "segs", items: items, color: color, width: 2 * lw, alpha: alpha, z: 4 };
  }
  // Obszar hipotezy: pas od granicy w stronę hipotezy, linia i napis „≥ 168”.
  function hypLayers(h, x, yLabel, R, size) {
    if (!h) return [];
    return [{ type: "band", lo: h.dir === "gt" ? h.bound : x[0], hi: h.dir === "gt" ? x[1] : h.bound,
              color: R.hyp, alpha: 0.15 },
            { type: "vline", x: h.bound, color: R.hyp, width: 2 },
            txt(h.bound, yLabel, h.label, R.hyp, size || 4.5, { align: "left" })];
  }
  function axis(x, y, xName, layers, aria) {
    return { x: x, y: y, xName: xName, hideY: true, noYGrid: true, layers: layers, aria: aria };
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

  // ---- Rozdział 3: średnie ----------------------------------------------------
  var TYPES = {
    // Krok 1 punkty, 2 średnia, 3 ± SE, 4 przedział.
    single_mean: function (c, k, step, h, L, R) {
      var x = L.x, layers = hypLayers(h, x, 0.68, R), aria = "Wzrost " + c.pts.length + " osób na osi";
      layers.push(dots(c.pts, R.ci, 3, 0.7));
      if (step >= 2) {
        layers.push({ type: "vline", x: c.xbar, color: R.est, width: 2, dash: "dotted" }, dot(c.xbar, 0, R.est, 7, true));
        if (step === 2) layers.push(txt(c.xbar, -0.18, c.lab_mean, R.est, 5));
        aria += "; " + c.lab_mean;
      }
      if (step >= 3) {
        layers.push(errbar([[c.xbar - c.se, c.xbar + c.se, 0]], 0.06, R.hit, 1.8));
        if (step === 3) layers.push(txt(c.xbar, 0.14, c.lab_se, R.hit, 4));
      }
      if (step >= 4) {
        layers.push(errbar([[c.lo, c.hi, 0]], 0.12, R.ci, 2.2, 0.6), txt(c.xbar, -0.22, c.lab_ci, R.ci, 4.8));
        aria += "; " + c.lab_ci;
      }
      return axis(x, c.y, c.xlab, layers, aria);
    },
    // Krok i: przedziały dla pierwszych i liczebności.
    compare_n: function (c, k, step, h, L, R) {
      var n = Math.min(step, c.lo.length), ys = range(n);
      var layers = hypLayers(h, L.x, c.lo.length + 0.5, R);
      layers.push(errbar(ys.map(function (y) { return [c.lo[y - 1], c.hi[y - 1], y]; }), 0.12, R.ci, 1.8),
                  dots(ys.map(function (y) { return [c.xbar, y]; }), R.est, 5));
      layers[layers.length - 1].symbol = "diamond";
      ys.forEach(function (y) {
        layers.push(txt(L.raw[0], y, c.lab_n[y - 1], R.sec, 4.5, { align: "left" }),
                    txt(c.hi[y - 1], y + 0.22, c.lab_ci[y - 1], R.ci, 3.8, { align: "right" }));
      });
      return axis(L.x, c.y, c.xlab, layers, "Przedziały 95% dla " +
        ys.map(function (y) { return c.lab_n[y - 1] + ": " + c.lab_ci[y - 1]; }).join("; "));
    },
    // Wykres 0: obie próby (krok 1) i średnie (krok 2+); wykres 1 od kroku 3: różnica,
    // ± SE (4) i przedział (5).
    diff_means: function (c, k, step, h, L, R) {
      if (k === 0) {
        var x0 = c.top.raw[0], layers = [
          txt(x0, 1.8, c.label1, R.ci, 4.5, { align: "left" }), txt(x0, 1.0, c.label2, R.miss, 4.5, { align: "left" }),
          dots(c.pts1, R.ci, 3, 0.7), dots(c.pts2, R.miss, 3, 0.7)];
        if (step >= 2) {
          layers.push({ type: "vline", x: c.x1, from: 0.6, to: 2.1, color: R.ci, width: 1.6, dash: "dotted" },
                      { type: "vline", x: c.x2, from: 0.6, to: 2.1, color: R.miss, width: 1.6, dash: "dotted" },
                      dot(c.x1, 1.8, R.ci, 7, true), dot(c.x2, 1.0, R.miss, 7, true),
                      txt(c.x1, 2.15, c.lab_x1, R.ci, 4.5),
                      // x̄₂ pod grupą B: przy bliskich średnich etykiety nie nachodzą na siebie.
                      txt(c.x2, 0.47, c.lab_x2, R.miss, 4.5));
        }
        return axis(c.top.x, c.y, c.xlab, layers, "Dwie próby: " + c.label1 + " i " + c.label2 +
          (step >= 2 ? "; " + c.lab_x1 + ", " + c.lab_x2 : ""));
      }
      if (step < 3) return null;
      var lb = hypLayers(h, L.x, 0.55, R);
      if (!h) lb.push({ type: "vline", x: 0, color: C.token("--upwr-cat-wrzos", "#a07894"), width: 2, dash: "dashed" });
      lb.push(dot(c.diff, 0, R.est, 7, true));
      if (step === 3) lb.push(txt(c.diff, -0.22, c.lab_diff, R.est, 4.5));
      if (step >= 4) {
        lb.push(errbar([[c.diff - c.se, c.diff + c.se, 0]], 0.08, R.hit, 1.8));
        if (step === 4) lb.push(txt(c.diff, 0.17, c.lab_se, R.hit, 4));
      }
      if (step >= 5) lb.push(errbar([[c.lo, c.hi, 0]], 0.14, R.ci, 2.2, 0.6), txt(c.diff, 0.2, c.lab_ci, R.ci, 4.8));
      return axis(L.x, c.y_bot, c.xlab_bot, lb, c.lab_diff + (step >= 5 ? "; " + c.lab_ci : ""));
    },
    // Krok 1 punkty grup, 2 średnie, 3 przedziały.
    forest: function (c, k, step, h, L, R) {
      var g = c.groups.length, ys = range(g);
      var layers = ys.map(function (y) { return txt(L.raw[0], y - 0.38, c.groups[y - 1], R.sec, 4.5, { align: "left" }); });
      layers = layers.concat(hypLayers(h, L.x, g + 0.45, R));
      layers.push(dots(c.pts, R.ci, 2.3, 0.55));
      if (step >= 2) {
        var m = dots(ys.map(function (y) { return [c.means[y - 1], y]; }), R.est, 6);
        m.symbol = "diamond"; m.z = 6;
        layers.push(m);
      }
      if (step >= 3) layers.push(errbar(ys.map(function (y) { return [c.lo[y - 1], c.hi[y - 1], y]; }), 0.18, R.ci, 1.8));
      return axis(L.x, c.y, c.xlab, layers, "Grupy: " + c.groups.join(", ") +
        (step >= 3 ? "; przedziały 95% dla średnich" : step >= 2 ? "; średnie grup" : "; punkty pomiarów"));
    },

    // ---- Rozdział 4: proporcje ------------------------------------------------
    // Wykres 0: słupki TAK/NIE; wykres 1: p̂ (krok 2+), ± SE (3), przedział (4).
    single_prop: function (c, k, step, h, L, R) {
      if (k === 0) {
        return barAxis(bars([1, 2], 0.6, c.counts, [R.miss, R.ci], R), c.counts, ["NIE", "TAK"],
          "Liczba odpowiedzi NIE: " + c.counts[0] + ", TAK: " + c.counts[1]);
      }
      var layers = hypLayers(h, L.x, 0.5, R), aria = "Oś proporcji";
      if (step >= 2) {
        layers.push(dot(c.phat, 0, R.est, 7, true));
        if (step === 2) layers.push(txt(c.phat, -0.22, c.lab_phat, R.est, 4.8));
        aria += "; " + c.lab_phat;
      }
      // Krok 3: SE; pełny przedział zastępuje ten pasek w kroku 4.
      if (step === 3) layers.push(errbar([[c.phat - c.se, c.phat + c.se, 0]], 0.08, R.hit, 1.8), txt(c.phat, 0.2, c.lab_se, R.hit, 4));
      if (step >= 4) {
        layers.push(errbar([[c.lo, c.hi, 0]], 0.14, R.ci, 2.2, 0.6), txt(c.phat, 0.2, c.lab_ci, R.ci, 4.8));
        aria += "; " + c.lab_ci;
      }
      return axis(L.x, c.y, c.xlab, layers, aria);
    },
    compare_n_prop: function (c, k, step, h, L, R) {
      var g = c.lo.length, n = Math.min(step, g), ys = range(n);
      var layers = range(g).map(function (y) { return txt(L.raw[0], y, c.lab_n[y - 1], R.sec, 4.5, { align: "left" }); });
      layers = layers.concat(hypLayers(h, L.x, g + 0.45, R));
      var m = dots(ys.map(function (y) { return [c.phat[y - 1], y]; }), R.est, 5);
      m.symbol = "diamond";
      layers.push(m, errbar(ys.map(function (y) { return [c.lo[y - 1], c.hi[y - 1], y]; }), 0.18, R.ci, 1.8, 0.7));
      ys.forEach(function (y) {
        layers.push(txt((c.lo[y - 1] + c.hi[y - 1]) / 2, y - 0.22, c.lab_ci[y - 1], R.ci, 3.8));
      });
      return axis(L.x, c.y, c.xlab, layers, "Przedziały 95% dla " +
        ys.map(function (y) { return c.lab_n[y - 1] + ": " + c.lab_ci[y - 1]; }).join("; "));
    },
    // Wykres 0: słupki dwóch grup; 1: dwie p̂ (krok 2+); 2: różnica (3), ± SE (4), przedział (5).
    diff_props: function (c, k, step, h, L, R) {
      if (k === 0) {
        var d = 0.1875;
        return barAxis(bars([1 - d, 1 + d, 2 - d, 2 + d], 0.325, c.counts, [R.miss, R.ci, R.miss, R.ci], R),
          c.counts, [c.label1, c.label2],
          c.label1 + ": NIE " + c.counts[0] + ", TAK " + c.counts[1] + "; " + c.label2 + ": NIE " + c.counts[2] + ", TAK " + c.counts[3]);
      }
      if (k === 1) {
        var top = [];
        if (step >= 2) {
          top.push(dot(c.p1, 1, R.est, 7, true), txt(c.p1, 1.45, c.lab_p1, R.est, 4.2),
                   dot(c.p2, 2, R.est, 7, true), txt(c.p2, 2.45, c.lab_p2, R.est, 4.2));
        }
        return { x: [-0.05, 1.05], y: c.y_top, xName: c.xlab, noYGrid: true,
                 yLabels: { 1: c.label1, 2: c.label2 }, layers: top,
                 aria: step >= 2 ? c.lab_p1 + ", " + c.lab_p2 : "Oś proporcji dla dwóch grup" };
      }
      var lb = [{ type: "vline", x: 0, color: R.zero, width: 2, dash: "dashed" }].concat(hypLayers(h, L.x, 0.45, R, 4));
      if (step >= 3) {
        lb.push(dot(c.diff, 0, R.est, 7, true));
        if (step === 3) lb.push(txt(c.diff, -0.22, c.lab_diff, R.est, 4.5));
      }
      if (step >= 4) {
        lb.push(errbar([[c.diff - c.se, c.diff + c.se, 0]], 0.08, R.hit, 1.8));
        if (step === 4) lb.push(txt(c.diff, 0.17, c.lab_se, R.hit, 4));
      }
      if (step >= 5) lb.push(errbar([[c.lo, c.hi, 0]], 0.14, R.ci, 2.2, 0.6), txt(c.diff, 0.2, c.lab_ci, R.ci, 4.8));
      return axis(L.x, c.y_bot, c.xlab_bot, lb,
        step >= 5 ? c.lab_ci : step >= 3 ? c.lab_diff : "Oś różnicy proporcji z linią zera");
    },
    // Krok 1 nazwy i liczby, 2 proporcje, 3 przedziały.
    forest_prop: function (c, k, step, h, L, R) {
      var g = c.lo.length, ys = range(g);
      var layers = ys.map(function (y) {
        return txt(L.raw[1], y, c.lab_groups[y - 1], R.sec, 4, { align: "right" });
      });
      layers = layers.concat(hypLayers(h, L.x, g + 0.45, R));
      if (step >= 2) {
        var m = dots(ys.map(function (y) { return [c.phat[y - 1], y]; }), R.est, 6);
        m.symbol = "diamond"; m.z = 6;
        layers.push(m);
      }
      if (step >= 3) layers.push(errbar(ys.map(function (y) { return [c.lo[y - 1], c.hi[y - 1], y]; }), 0.18, R.ci, 1.8));
      return axis(L.x, c.y, c.xlab, layers, c.lab_groups.join("; ") +
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
      return TYPES[c.type](c, k, step, h, c.lims[key || "0"], roles(P));
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
