// Ryciny statystyk opisowych liczone w przeglądarce (ECharts SVG), dawniej
// wykresy ggplot rysowane na serwerze. Dane: ankieta (window.lc01Survey)
// albo losowanie z ziarnem; rachunki: window.lc01 (chart_utils.js).
//
// Kontener (R: lc01_stat_figure() w stat_figures.R):
//   .lc01-stat[data-fig][data-config]
//     .lc01-stat-ctl        — kontrolka (natywny <select> albo radia lc_segmented)
//     .lc-ec[data-chart=k]  — jeden lub więcej wykresów
//     .lc01-legend          — legenda (opcjonalnie)
//     [data-when="wartość"] — opisy policzone w R dla każdej opcji; JS pokazuje właściwy
//
// Figura zwraca opis rysunku: zakresy osi, nazwy osi i warstwy:
//   bins {data:[[l,r,y]], color, alpha}     — słupki histogramu / kategorii
//   band {lo, hi, color, alpha}              — pionowy pas na całą wysokość
//   vline {x, color, width, dash}            — pionowa linia na całą wysokość
//   line {data:[[x,y]], color, width, dash}  — krzywa
//   arrow {from:[x,y], to:[x,y], color, width}
//   text {x, y, text, color, align, bold, mono, size}  (y = "top": przy górnej krawędzi)
(function () {
  "use strict";
  var C = window.lcCharts, U = window.lc01;
  if (!C || !C.chart || !U) return;

  // Okrągłe szerokości przedziałów; granice to wielokrotności szerokości, przedziały [a, b).
  var BIN = { wzrost: 2, waga: 3, srednia_ocen: 0.1, czas_dojazdu: 4, wydatki: 250,
              liczba_nieobecnosci: 1, liczba_kursow: 1 };
  function roundTo(v, w) { return Math.round(v / w * 1e6) / 1e6 * w; }
  function binsBy(values, width) {
    var lo = roundTo(Math.floor(Math.min.apply(null, values) / width + 1e-9) * width, width);
    var hi = roundTo((Math.floor(Math.max.apply(null, values) / width + 1e-9) + 1) * width, width);
    return U.bins(values, lo, hi, width).map(function (b) {
      return { l: roundTo(b.l, width), r: roundTo(b.r, width), n: b.n };
    });
  }
  // Reguła ggplot dla bins = k (szerokość = rozstęp / (k − 1), przedziały domknięte
  // z prawej). Tylko tam, gdzie niezgranie przedziałów z danymi jest treścią ryciny.
  function binsGgplot(values, k) {
    var lo = Math.min.apply(null, values), hi = Math.max.apply(null, values);
    var w = (hi - lo) / (k - 1), boundary = w / 2;
    var origin = boundary + Math.floor((lo - boundary) / w) * w, edges = [];
    for (var e = origin; e <= hi + (1 - 1e-8) * w; e += w) edges.push(e);
    var out = [];
    for (var i = 0; i + 1 < edges.length; i++) out.push({ l: edges[i], r: edges[i + 1], n: 0 });
    values.forEach(function (v) {
      for (var j = 0; j < out.length; j++) {
        if (v <= out[j].r + 1e-9 * w && (v > out[j].l + 1e-9 * w || j === 0)) { out[j].n++; break; }
      }
    });
    return out;
  }
  function density(bins, n) {
    return bins.map(function (b) { return [b.l, b.r, b.n / (n * (b.r - b.l))]; });
  }
  function counts(bins) { return bins.map(function (b) { return [b.l, b.r, b.n]; }); }
  function maxY(rows) { return Math.max.apply(null, rows.map(function (r) { return r[2]; })); }
  // lc_fmt z R: stała liczba cyfr, bez końcowych zer.
  function lcFmt(v, d) {
    var s = C.fmt(v, d);
    return s.indexOf(".") < 0 ? s : s.replace(/0+$/, "").replace(/\.$/, "");
  }


  // ---- Pomocniki rycin ------------------------------------------------------
  // Słupki kategorii na pozycjach 1..k: [[l, r, n]].
  function catRows(n, width) {
    return n.map(function (c, i) { return [i + 1 - width / 2, i + 1 + width / 2, c]; });
  }
  function catLabels(levels, map) {
    var out = {};
    levels.forEach(function (lv, i) { out[i + 1] = map ? map(lv) : lv; });
    return out;
  }
  // Zawijanie etykiety po słowach do n znaków (jak scales::label_wrap).
  function wrap(text, n) {
    var lines = [], line = "";
    String(text).split(/\s+/).forEach(function (w) {
      if (line && line.length + w.length + 1 > n) { lines.push(line); line = ""; }
      line += (line ? " " : "") + w;
    });
    if (line) lines.push(line);
    return lines.join("\n");
  }
  function countTexts(rows, top, P, opts) {
    return rows.map(function (r) {
      return { type: "text", x: (r[0] + r[1]) / 2, y: r[2] + top * 0.03, text: String(r[2]),
               color: P.secondary, mono: true, size: (opts && opts.size) || 1 };
    });
  }
  // Słupki dla każdej obserwowanej wartości liczbowej (zmienna dyskretna).
  function valueBars(x, color, width, P, labels) {
    var values = U.sorted(x).filter(function (t, i, a) { return i === 0 || t !== a[i - 1]; });
    var rows = values.map(function (t) {
      return [t - width / 2, t + width / 2, x.filter(function (s) { return s === t; }).length];
    });
    var top = maxY(rows) * 1.15, layers = [{ type: "bins", data: rows, color: color, alpha: 0.9 }];
    return { rows: rows, top: top, values: values,
             layers: labels === false ? layers : layers.concat(countTexts(rows, top, P)) };
  }
  // Sekwencyjna skala bordowa (upwr_seq_burgundy) od wartości najmniejszej do największej.
  var SEQ = ["#f5f0e6", "#e8d5c8", "#d9a89a", "#c27570", "#a04550", "#7a2535", "#4a0e1e"];
  function seqColor(t) {
    var p = Math.max(0, Math.min(1, t)) * (SEQ.length - 1), i = Math.min(SEQ.length - 2, Math.floor(p)), f = p - i;
    var a = parseInt(SEQ[i].slice(1), 16), b = parseInt(SEQ[i + 1].slice(1), 16);
    var mix = [16, 8, 0].map(function (sh) { return Math.round((a >> sh & 255) * (1 - f) + (b >> sh & 255) * f); });
    return "#" + mix.map(function (c) { return (c < 16 ? "0" : "") + c.toString(16); }).join("");
  }
  function status(html) { return '<div class="lc-status" role="status" aria-live="polite">' + html + "</div>"; }
  function setValue(root, key, text) {
    root.querySelectorAll('[data-v="' + key + '"]').forEach(function (n) { n.textContent = text; });
  }
  function shuffle(a) {
    var out = a.slice();
    for (var i = out.length - 1; i > 0; i--) { var j = Math.floor(Math.random() * (i + 1)), t = out[i]; out[i] = out[j]; out[j] = t; }
    return out;
  }
  // Histogram gęstości z krzywą, średnią (linia ciągła) i medianą (przerywana).
  function meanMedianHist(v, P, alpha, rug) {
    var x = U.survey(v), m = U.mean(x), med = U.median(x), b = density(binsBy(x, BIN[v]), x.length);
    var lo = b[0][0], hi = b[b.length - 1][1], kd = U.kde(x, lo, hi, 256);
    var top = Math.max(maxY(b), Math.max.apply(null, kd.map(function (p) { return p[1]; }))) * 1.12;
    var layers = [{ type: "bins", data: b, color: P.niebo, alpha: alpha },
                  { type: "line", data: kd, color: P.secondary, width: 2 }];
    if (rug) layers.push({ type: "rug", values: x, color: P.secondary, alpha: 0.5 });
    layers.push({ type: "vline", x: m, color: P.accent, width: 2.4 },
                { type: "vline", x: med, color: P.niebo, width: 2.4, dash: [6, 4] });
    return { x: [lo, hi], y: [0, top], xName: U.surveyMeta(v).label, yName: "Gęstość", layers: layers,
             legend: [{ text: "Średnia", color: P.accent }, { text: "Mediana", color: P.niebo, dash: true }],
             aria: "Histogram gęstości zmiennej " + U.surveyMeta(v).label + " ze średnią " + lcFmt(m, 2) + " i medianą " + lcFmt(med, 2) };
  }
  // Pudełko jak w ggplot: kwartyle typu 7, wąsy do skrajnych wartości w granicach 1.5 IQR.
  function boxStats(x) {
    var s = U.sorted(x), q1 = U.quantile7(s, 0.25, true), q3 = U.quantile7(s, 0.75, true), iqr = q3 - q1;
    var lo = q1 - 1.5 * iqr, hi = q3 + 1.5 * iqr, inside = s.filter(function (t) { return t >= lo && t <= hi; });
    return { q1: q1, med: U.quantile7(s, 0.5, true), q3: q3, lo: inside[0], hi: inside[inside.length - 1],
             out: s.filter(function (t) { return t < lo || t > hi; }) };
  }
  var TYPE_COLOR = { nom: "terakota", ord: "bursztyn", disc: "niebo", cont: "szalwia" };

  // ---- Figury -------------------------------------------------------------
  var FIGURES = {
    // Ryc. 3.2 — średnia jako punkt równowagi.
    mean: function (v, P) {
      var x = U.survey(v), m = U.mean(x), b = counts(binsBy(x, BIN[v])), top = maxY(b) * 1.18;
      var lo = Math.min.apply(null, x), hi = Math.max.apply(null, x), ya = -top * 0.09;
      return {
        x: [b[0][0], b[b.length - 1][1]], y: [-top * 0.16, top],
        xName: U.surveyMeta(v).label, yName: "Liczebność",
        layers: [
          { type: "bins", data: b, color: P.rule, alpha: 0.9 },
          { type: "vline", x: m, color: P.accent, width: 3, from: 0 },
          { type: "text", x: m, y: "top", text: "Średnia = " + C.fmt(m, 2), color: P.accent, align: "left", bold: true },
          { type: "arrow", from: [lo, ya], to: [m, ya], color: P.niebo, width: 3.5 },
          { type: "arrow", from: [hi, ya], to: [m, ya], color: P.niebo, width: 3.5 }
        ],
        aria: "Histogram zmiennej " + U.surveyMeta(v).label + " ze średnią " + C.fmt(m, 2) +
              "; strzałki od wartości najmniejszej i największej wskazują punkt równowagi"
      };
    },
    // Ryc. 3.3 — mediana dzieli dane na pół.
    median: function (v, P) {
      var x = U.survey(v), med = U.median(x), b = counts(binsBy(x, BIN[v])), top = maxY(b) * 1.3;
      var lo = b[0][0], hi = b[b.length - 1][1], indygo = C.token("--upwr-cat-indygo", "#3a5a8a");
      var below = x.filter(function (t) { return t < med; }).length, above = x.filter(function (t) { return t > med; }).length;
      return {
        x: [lo, hi], y: [0, top], xName: U.surveyMeta(v).label, yName: "Liczebność",
        layers: [
          { type: "band", lo: lo, hi: med, color: P.niebo, alpha: 0.12 },
          { type: "band", lo: med, hi: hi, color: P.accent, alpha: 0.08 },
          { type: "bins", data: b, color: P.rule, alpha: 0.95 },
          { type: "vline", x: med, color: indygo, width: 3 },
          { type: "text", x: (lo + med) / 2, y: "top", text: "50% (" + below + " obs.)", color: P.secondary, bold: true },
          { type: "text", x: (med + hi) / 2, y: "top", text: "50% (" + above + " obs.)", color: P.secondary, bold: true },
          { type: "text", x: med, y: top * 0.8, text: "Me = " + lcFmt(med, 1), color: indygo, align: "left", bold: true }
        ],
        aria: "Histogram zmiennej " + U.surveyMeta(v).label + " z medianą " + lcFmt(med, 1) +
              "; po " + below + " i " + above + " obserwacji po obu stronach"
      };
    },
    // Ryc. 3.7 — zmienna dyskretna: słupki (k = 0) i histogram 15 przedziałów (k = 1).
    disc: function (v, P, k) {
      var x = U.survey(v), lab = U.surveyMeta(v).label;
      if (k === 0) {
        var values = U.sorted(x).filter(function (t, i, a) { return i === 0 || t !== a[i - 1]; });
        var rows = values.map(function (t) {
          return [t - 0.35, t + 0.35, x.filter(function (s) { return s === t; }).length];
        });
        var top = maxY(rows) * 1.15;
        return {
          x: [values[0] - 0.7, values[values.length - 1] + 0.7], y: [0, top], xName: lab, yName: "Liczebność",
          xTicks: values,
          layers: [{ type: "bins", data: rows, color: P.niebo, alpha: 0.9 }].concat(rows.map(function (r) {
            return { type: "text", x: (r[0] + r[1]) / 2, y: r[2] + top * 0.04, text: String(r[2]), color: P.secondary, mono: true };
          })),
          aria: "Wykres słupkowy zmiennej " + lab + ": osobny słupek dla każdej wartości"
        };
      }
      var h = counts(binsGgplot(x, 15)), top2 = maxY(h) * 1.12;
      return {
        x: [h[0][0], h[h.length - 1][1]], y: [0, top2], xName: lab, yName: "Liczebność",
        layers: [{ type: "bins", data: h, color: P.accent, alpha: 0.65 }],
        aria: "Histogram zmiennej " + lab + " z 15 przedziałami, których granice nie pokrywają się z liczbami całkowitymi"
      };
    },
    // Ryc. 3.8 — modalność. Dane z ziarnem (stałe), gęstości grup względem własnej grupy.
    modal: function (v, P) {
      var g = U.seeded(42), groups, xName, width, note = null;
      if (v === "unimodal") {
        groups = [{ name: null, x: g.rnorm(500, 165, 6) }]; xName = "Wzrost kobiet (cm)"; width = 1.5;
      } else if (v === "bimodal") {
        groups = [{ name: "Kobiety", color: P.accent, x: g.rnorm(250, 162, 5) },
                  { name: "Mężczyźni", color: P.niebo, x: g.rnorm(250, 182, 5) }];
        xName = "Wzrost (cm)"; width = 1.5;
      } else {
        groups = [{ name: "Rower", color: P.szalwia, x: g.rnorm(150, 12, 3) },
                  { name: "Autobus", color: P.bursztyn, x: g.rnorm(120, 25, 4) },
                  { name: "Auto", color: P.accent, x: g.rnorm(130, 40, 5) }];
        xName = "Czas dojazdu (min)"; width = 1.5;
      }
      var all = [].concat.apply([], groups.map(function (t) { return t.x; }));
      var b = density(binsBy(all, width), all.length), lo = b[0][0], hi = b[b.length - 1][1];
      var bw = U.bwNrd0(all), total = U.kde(all, lo, hi, 256, bw), layers = [
        { type: "bins", data: b, color: groups.length > 1 ? P.ref : P.niebo, alpha: groups.length > 1 ? 0.45 : 0.7 },
        { type: "line", data: total, color: P.secondary, width: 2.4 }
      ];
      // Krzywa grupy ważona jej udziałem i z pasmem całości: krzywe grup sumują się
      // dokładnie do krzywej całości (w ggplot każda grupa miała własną skalę i pasmo).
      groups.forEach(function (t) {
        if (!t.name) return;
        var share = t.x.length / all.length;
        layers.push({ type: "line", color: t.color, width: 1.8, dash: [6, 4],
                      data: U.kde(t.x, lo, hi, 256, bw).map(function (p) { return [p[0], p[1] * share]; }) });
      });
      if (v === "unimodal") {
        var m = U.mean(all);
        layers.push({ type: "vline", x: m, color: P.accent, width: 2, dash: [6, 4] },
                    { type: "text", x: m + 1, y: "top", text: "moda ≈ średnia ≈ mediana", color: P.accent, align: "left", bold: true });
      }
      var top = Math.max(maxY(b), Math.max.apply(null, total.map(function (p) { return p[1]; }))) * 1.12;
      return {
        x: [lo, hi], y: [0, top], xName: xName, yName: "Gęstość",
        legend: groups.filter(function (t) { return t.name; }).map(function (t) {
          return { text: t.name, color: t.color, dash: true };
        }),
        layers: layers,
        aria: "Histogram gęstości z krzywą gęstości: " + (v === "unimodal" ? "jeden szczyt" :
              v === "bimodal" ? "dwa szczyty (kobiety i mężczyźni)" : "trzy szczyty (rower, autobus, auto)")
      };
    },

    // Ryc. 1.1 — „średni kierunek” z kodów 1–4.
    kierunek: function (v, P) {
      var lv = ["Biologia", "Ekonomia", "Informatyka", "Psychologia"], x = U.survey("kierunek");
      var n = U.counts(x, lv), m = U.mean(x.map(function (c) { return lv.indexOf(c) + 1; }));
      var rows = catRows(n, 0.42), top = Math.max.apply(null, n) * 1.25;
      return {
        x: [0.5, 4.5], y: [0, top], xName: "", yName: "Liczba studentów", noXGrid: true,
        xLabels: catLabels(lv, function (t) { return (lv.indexOf(t) + 1) + "\n" + t; }),
        layers: [{ type: "bins", data: rows, color: P.terakota, alpha: 0.85 }].concat(countTexts(rows, top * 0.8, P), [
          { type: "vline", x: m, from: 0, to: top * 0.88, color: P.accent, width: 2.2, dash: [5, 4] },
          { type: "point", x: m, y: 0, color: P.accent, symbol: "triangle", size: 14 },
          { type: "text", x: m, y: top * 0.9, text: "średnia = " + (Math.round(m * 100 + 1e-6) / 100).toFixed(2), color: P.accent, mono: true, bold: true }
        ]),
        aria: "Liczebności kierunków zakodowanych liczbami 1–4 i średnia kodów " + (Math.round(m * 100 + 1e-6) / 100).toFixed(2) +
              ", wypadająca między Ekonomią a Informatyką"
      };
    },
    // Ryc. 1.3 — cztery typy zmiennych; v = "on": wykresy źle dobrane do typu.
    types: function (v, P, k) {
      var spec = [["plec", "Płeć", "nom"], ["zadowolenie", "Zadowolenie", "ord"],
                  ["liczba_kursow", "Liczba kursów", "disc"], ["wzrost", "Wzrost (cm)", "cont"]][k];
      var name = spec[0], lab = spec[1], type = spec[2], col = P[TYPE_COLOR[type]], x = U.survey(name), bad = v === "on";
      var meta = U.surveyMeta(name);
      if (type === "nom" || type === "ord") {
        if (bad) {
          var codes = x.map(function (c) { return meta.levels.indexOf(c) + 1; });
          var h = counts(binsGgplot(codes, 10));
          return { x: [h[0][0], h[h.length - 1][1]], y: [0, maxY(h) * 1.1], xName: lab + " (zakodowane jako liczby)",
                   yName: "Liczebność", layers: [{ type: "bins", data: h, color: P.ref }],
                   aria: "Histogram kodów liczbowych kategorii zmiennej " + lab };
        }
        var n = U.counts(x, meta.levels), rows = catRows(n, 0.9), top = Math.max.apply(null, n) * 1.15;
        return { x: [0.4, meta.levels.length + 0.6], y: [0, top], xName: lab, yName: "Liczebność", noXGrid: true,
                 xLabels: catLabels(meta.levels, function (t) { return wrap(t, 12); }),
                 layers: [{ type: "bins", data: rows, color: col }].concat(countTexts(rows, top, P)),
                 aria: "Wykres słupkowy liczebności kategorii zmiennej " + lab };
      }
      if (type === "disc") {
        if (bad) {
          var lo = Math.min.apply(null, x), hi = Math.max.apply(null, x), d = U.kde(x, lo, hi, 256);
          return { x: [lo, hi], y: [0, Math.max.apply(null, d.map(function (p) { return p[1]; })) * 1.1],
                   xName: lab, yName: "Gęstość", layers: [{ type: "line", data: d, color: P.ref, width: 0, fill: P.ref, fillAlpha: 0.8 }],
                   aria: "Wygładzona krzywa gęstości zmiennej dyskretnej " + lab };
        }
        var vb = valueBars(x, col, 0.9, P);
        return { x: [vb.values[0] - 0.7, vb.values[vb.values.length - 1] + 0.7], y: [0, vb.top], xName: lab,
                 yName: "Liczebność", xTicks: vb.values, layers: vb.layers,
                 aria: "Wykres słupkowy liczebności wartości zmiennej " + lab };
      }
      if (bad) {
        var u = valueBars(x, P.ref, 0.3, P, false);
        return { x: [u.values[0] - 1, u.values[u.values.length - 1] + 1], y: [0, maxY(u.rows) * 1.15], xName: lab,
                 yName: "Liczebność", layers: u.layers,
                 aria: "Osobny wąski słupek dla każdej zmierzonej wartości wzrostu" };
      }
      var b = density(binsBy(x, BIN[name]), x.length), lo2 = b[0][0], hi2 = b[b.length - 1][1];
      var kd = U.kde(x, lo2, hi2, 256);
      return { x: [lo2, hi2], y: [0, Math.max(maxY(b), Math.max.apply(null, kd.map(function (p) { return p[1]; }))) * 1.12],
               xName: lab, yName: "Gęstość",
               layers: [{ type: "bins", data: b, color: col, alpha: 0.7 }, { type: "line", data: kd, color: col, width: 2.4 }],
               aria: "Histogram gęstości wzrostu z wygładzoną krzywą" };
    },
    // Ryc. 2.2 — kolejność kategorii; v = "on": losowa kolejność (nowa przy każdym włączeniu).
    order: {
      prepare: function (state, v) {
        if (v === "on" && state.last !== "on") state.perm = [0, 1].map(function () { return shuffle([0, 1, 2, 3, 4]); });
        state.last = v;
      },
      draw: function (v, P, k, state) {
        var name = k === 0 ? "kierunek" : "zadowolenie", meta = U.surveyMeta(name), x = U.survey(name);
        var short = { "Bardzo niezadowolony": "B. niezad.", "Niezadowolony": "Niezad.", "Neutralny": "Neutr.",
                      "Zadowolony": "Zad.", "Bardzo zadowolony": "B. zad." };
        var levels = meta.levels.slice();
        if (v === "on") levels = state.perm[k].filter(function (i) { return i < levels.length; }).map(function (i) { return levels[i]; });
        var n = U.counts(x, levels), rows = catRows(n, 0.9), top = Math.max.apply(null, n) * 1.15;
        return { x: [0.4, levels.length + 0.6], y: [0, top], xName: k === 0 ? "Kierunek" : "Zadowolenie", yName: "Liczebność",
                 noXGrid: true, xLabels: catLabels(levels, function (t) { return short[t] || wrap(t, 12); }),
                 layers: [{ type: "bins", data: rows, color: k === 0 ? P.terakota : P.bursztyn, alpha: 0.85 }].concat(countTexts(rows, top, P)),
                 aria: "Liczebności zmiennej " + (k === 0 ? "kierunek" : "zadowolenie") + " w kolejności: " + levels.join(", ") };
      }
    },
    // Ryc. 2.5 — dominanta; przycisk losuje 200 obserwacji z przypadkowymi proporcjami.
    mode: {
      init: function (state) {
        state.levels = U.surveyMeta("kierunek").levels.slice();
        state.data = U.survey("kierunek");
      },
      act: function (state) {
        var lv = ["Informatyka", "Biologia", "Psychologia", "Ekonomia"];
        var p = lv.map(function () { return Math.random(); }), total = p.reduce(function (a, b) { return a + b; }, 0);
        state.levels = lv;
        state.data = [];
        for (var i = 0; i < 200; i++) {
          var r = Math.random() * total, j = 0;
          while (j < 3 && r >= p[j]) { r -= p[j]; j++; }
          state.data.push(lv[j]);
        }
      },
      draw: function (v, P, k, state) {
        var n = U.counts(state.data, state.levels), mode = n.indexOf(Math.max.apply(null, n));
        var rows = catRows(n, 0.7), top = Math.max.apply(null, n) * 1.15;
        return { x: [0.4, state.levels.length + 0.6], y: [0, top], xName: "Kierunek", yName: "Liczebność", noXGrid: true,
                 xLabels: catLabels(state.levels, null),
                 layers: [{ type: "bins", data: rows, colors: n.map(function (c, i) { return i === mode ? P.terakota : P.rule; }), alpha: 0.9 }],
                 aria: "Liczebności kierunków; dominanta: " + state.levels[mode] };
      },
      update: function (root, state) {
        var n = U.counts(state.data, state.levels), max = Math.max.apply(null, n), i = n.indexOf(max);
        var el = root.querySelector('[data-role="mode-text"]');
        if (el) el.innerHTML = status("<b>Dominanta:</b> " + state.levels[i] + "<br>Występuje " + max + " razy (" +
          lcFmt(100 * max / state.data.length, 1) + "% z " + state.data.length + " obserwacji).");
      }
    },
    // Ryc. 4.4 — reguła 68–95–99.7 na danych ankiety.
    emp: function (v, P) {
      var x = U.survey(v), m = U.mean(x), s = U.sd(x), b = density(binsBy(x, BIN[v]), x.length);
      var lo = Math.min(b[0][0], m - 3 * s), hi = Math.max(b[b.length - 1][1], m + 3 * s), top = maxY(b) * 1.18;
      var layers = [3, 2, 1].map(function (k) {
        return { type: "band", lo: m - k * s, hi: m + k * s, color: P.accent, alpha: [0, 0.22, 0.14, 0.07][k] };
      });
      layers.push({ type: "bins", data: b, color: P.niebo, alpha: 0.85 },
                  { type: "vline", x: m, color: P.secondary, width: 2.4 },
                  { type: "text", x: m, y: "top", text: "x̄ = " + lcFmt(m, 1), color: P.secondary, align: "left", bold: true });
      [1, 2, 3].forEach(function (k) {
        layers.push({ type: "text", x: m - k * s, y: top * 0.04, text: "−" + k + " SD", color: P.secondary, align: "right", size: 0.8, italic: true },
                    { type: "text", x: m + k * s, y: top * 0.04, text: "+" + k + " SD", color: P.secondary, align: "left", size: 0.8, italic: true });
      });
      return { x: [lo, hi], y: [0, top], xName: U.surveyMeta(v).label, yName: "Gęstość", layers: layers,
               aria: "Histogram gęstości zmiennej " + U.surveyMeta(v).label + " z pasami ±1, ±2 i ±3 odchylenia standardowego wokół średniej" };
    },
    // Ryc. 4.7 — odporność miar rozrzutu; przyciski dopisują wartości odstające.
    comp: {
      init: function (state) { state.data = U.survey("wzrost").slice(); },
      act: function (state, a) {
        if (a === "reset") { state.data = U.survey("wzrost").slice(); return; }
        var max = Math.max.apply(null, state.data), k = a === "add5" ? 5 : 1;
        for (var i = 0; i < k; i++) state.data.push(Math.round((max + 30 + (Math.random() * 10 - 5)) * 10) / 10);
      },
      draw: function (v, P, k, state) {
        var x = state.data, lo = Math.min.apply(null, x), hi = Math.max.apply(null, x);
        var q1 = U.quantile7(x, 0.25), q3 = U.quantile7(x, 0.75), b = counts(binsBy(x, BIN.wzrost)), top = maxY(b) * 1.12;
        var szalwia = P.szalwia;
        return { x: [b[0][0], b[b.length - 1][1]], y: [-top * 0.42, top], xName: "Wzrost (cm)", yName: "Liczebność",
                 layers: [
                   { type: "bins", data: b, color: P.niebo, alpha: 0.75 },
                   { type: "segment", from: [lo, -top * 0.1], to: [hi, -top * 0.1], color: P.accent, width: 4 },
                   { type: "text", x: (lo + hi) / 2, y: -top * 0.2, text: "Rozstęp = " + lcFmt(hi - lo, 1), color: P.accent, bold: true, valign: "middle" },
                   { type: "segment", from: [q1, -top * 0.28], to: [q3, -top * 0.28], color: szalwia, width: 4 },
                   { type: "text", x: (q1 + q3) / 2, y: -top * 0.37, text: "IQR = " + lcFmt(q3 - q1, 1), color: szalwia, bold: true, valign: "middle" }
                 ],
                 aria: "Histogram wzrostu " + x.length + " osób; rozstęp " + lcFmt(hi - lo, 1) + " cm, IQR " + lcFmt(q3 - q1, 1) + " cm" };
      },
      update: function (root, state) {
        var x = state.data, s = U.sd(x);
        setValue(root, "n", String(x.length));
        setValue(root, "range", C.fmt(Math.max.apply(null, x) - Math.min.apply(null, x), 1) + " cm");
        setValue(root, "iqr", C.fmt(U.quantile7(x, 0.75) - U.quantile7(x, 0.25), 1) + " cm");
        setValue(root, "sd", C.fmt(s, 2) + " cm");
        setValue(root, "cv", C.fmt(100 * s / U.mean(x), 1) + "%");
      }
    },
    // Ryc. 4.8 — SD (k = 0) i współczynnik zmienności (k = 1) czterech zmiennych.
    spread: function (v, P, k) {
      var vars = ["wzrost", "waga", "czas_dojazdu", "srednia_ocen"];
      var labels = ["Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen"];
      var val = vars.map(function (name) {
        var x = U.survey(name), s = U.sd(x);
        return k === 0 ? s : 100 * s / U.mean(x);
      });
      var lo = Math.min.apply(null, val), hi = Math.max.apply(null, val), yl = {};
      labels.forEach(function (t, i) { yl[4 - i] = t; });
      return { x: [0, hi * (k === 0 ? 1.2 : 1.15)], y: [0.4, 4.6], xName: k === 0 ? "Odchylenie standardowe (oryg. jednostki)" : "Współczynnik zmienności (%)",
               yName: "", yLabels: yl, noYGrid: true,
               layers: [{ type: "hbins", data: val.map(function (t, i) { return [4 - i - 0.3, 4 - i + 0.3, t]; }),
                          colors: val.map(function (t) { return seqColor(0.2 + 0.8 * (t - lo) / (hi - lo)); }), alpha: 0.85 }],
               aria: (k === 0 ? "Odchylenia standardowe: " : "Współczynniki zmienności: ") +
                     labels.map(function (t, i) { return t + " " + lcFmt(val[i], k === 0 ? 2 : 1); }).join(", ") };
    },
    // Ryc. 5.2 — trzy typy skośności; lewy rozkład to dokładne lustro prawego.
    skewcmp: {
      init: function (state) {
        var g = U.seeded(42), right = g.rgamma(5000, 4, 1);
        state.sets = [right.map(function (t) { return -t; }), g.rnorm(5000, 0, 2), right];
      },
      draw: function (v, P, k, state) {
        var x = state.sets[k], lo = Math.min.apply(null, x), hi = Math.max.apply(null, x), d = U.kde(x, lo, hi, 256);
        var fill = [P.accent, P.szalwia, P.niebo][k];
        return { x: [lo, hi], y: [0, Math.max.apply(null, d.map(function (p) { return p[1]; })) * 1.1], xName: "Wartość", yName: k === 0 ? "Gęstość" : "",
                 layers: [{ type: "line", data: d, color: P.secondary, width: 1.6, fill: fill, fillAlpha: 0.5 }],
                 aria: ["Lewostronnie skośny", "Symetryczny", "Prawostronnie skośny"][k] + " rozkład: krzywa gęstości" };
      },
      update: function (root, state) {
        var sk = state.sets.map(U.skewness), names = ["lewostronnie skośny", "symetryczny", "prawostronnie skośny"];
        var el = root.querySelector('[data-role="caption"]');
        if (el) el.textContent = "Skośność: " + names.map(function (t, i) { return t + " " + lcFmt(sk[i], 2); }).join(", ") + ".";
      }
    },
    // Ryc. 5.3 — skośność w danych ankiety: średnia i mediana.
    skew: function (v, P) { return meanMedianHist(v, P, 0.6, false); },
    // Ryc. 5.7 — pełna charakterystyka: histogram jak 5.3 z kreskami obserwacji (k = 0)
    // i poziome pudełko na tej samej osi (k = 1).
    full: function (v, P, k) {
      var h = meanMedianHist(v, P, 0.5, true);
      h.alignLeft = true;
      if (k === 0) return h;
      var x = U.survey(v), b = boxStats(x);
      // alignLeft: ten sam lewy margines co histogram, żeby osie x się pokrywały.
      return { x: h.x, y: [0.45, 1.55], xName: h.xName, yName: "", hideY: true, noYGrid: true, alignLeft: true,
               layers: [{ type: "box", horizontal: true, stroke: P.secondary,
                          boxes: [{ pos: 1, width: 0.75, fill: C.alpha(P.niebo, 0.4), q1: b.q1, med: b.med, q3: b.q3, lo: b.lo, hi: b.hi }] },
                        { type: "points", data: b.out.map(function (t) { return [t, 1]; }), color: P.accent, size: 9 }],
               aria: "Wykres pudełkowy zmiennej " + h.xName + ": mediana " + lcFmt(b.med, 2) + ", kwartyle " +
                     lcFmt(b.q1, 2) + " i " + lcFmt(b.q3, 2) + ", " + b.out.length + " wartości odstających" };
    },
    // Ryc. 4.6 — pudełka w grupach; skrzypce i punkty z pól wyboru.
    groups: {
      noteKey: function (vals) { return vals.ch4_grp_var + "|" + vals.ch4_grp_by; },
      draw: function (v, P, k, state, vals) {
        var name = vals.ch4_grp_var, by = vals.ch4_grp_by, x = U.survey(name), g = U.survey(by);
        var levels = U.surveyMeta(by).levels, fills = [P.secondary, P.bursztyn, P.niebo, P.szalwia];
        var violin = vals.ch4_grp_violin === "on", points = vals.ch4_grp_points === "on";
        var lo = Math.min.apply(null, x), hi = Math.max.apply(null, x), pad = (hi - lo) * 0.05;
        var groups = levels.map(function (lv) { return x.filter(function (t, i) { return g[i] === lv; }); });
        var layers = [], boxes = [], outliers = [];
        if (violin) {
          var dens = groups.map(function (gx) { return gx.length > 1 ? U.kde(gx, Math.min.apply(null, gx), Math.max.apply(null, gx), 128) : []; });
          var top = Math.max.apply(null, dens.map(function (d) { return Math.max.apply(null, d.map(function (p) { return p[1]; }).concat([0])); }));
          dens.forEach(function (d, i) {
            if (!d.length) return;
            var left = d.map(function (p) { return [i + 1 - 0.45 * p[1] / top, p[0]]; });
            var right = d.map(function (p) { return [i + 1 + 0.45 * p[1] / top, p[0]]; }).reverse();
            layers.push({ type: "polygon", points: left.concat(right), fill: C.alpha(fills[i], 0.4) });
          });
        }
        groups.forEach(function (gx, i) {
          if (!gx.length) return;
          var b = boxStats(gx);
          boxes.push({ pos: i + 1, width: violin ? 0.2 : 0.75, fill: C.alpha(fills[i], violin ? 0.8 : 0.7),
                       q1: b.q1, med: b.med, q3: b.q3, lo: b.lo, hi: b.hi });
          if (!violin) b.out.forEach(function (t) { outliers.push([i + 1, t]); });
        });
        layers.push({ type: "box", stroke: P.accent, boxes: boxes });
        if (outliers.length) layers.push({ type: "points", data: outliers, color: P.accent, size: 9 });
        if (points) {
          var j = U.seeded(1);
          layers.push({ type: "points", color: P.accent, alpha: 0.3, size: 5,
                        data: x.map(function (t, i) { return [levels.indexOf(g[i]) + 1 + j.runif(-0.15, 0.15), t]; }) });
        }
        var meta = U.surveyMeta(name);
        return { x: [0.4, levels.length + 0.6], y: [lo - pad, hi + pad], xName: by === "plec" ? "Płeć" : "Kierunek",
                 yName: meta.label, noXGrid: true, xLabels: catLabels(levels, function (t) { return wrap(t, 12); }),
                 layers: layers,
                 aria: "Wykresy pudełkowe zmiennej " + meta.label + " w grupach: " + levels.map(function (lv, i) {
                   return lv + " (mediana " + (groups[i].length ? lcFmt(U.median(groups[i]), 2) : "–") + ")";
                 }).join(", ") };
      }
    },
    // Ryc. 5.4 — wkład obserwacji do sumy kwadratów i czwartych potęg.
    powers: function (v, P) {
      var z = [1, 2, 3], layers = [];
      z.forEach(function (t) {
        layers.push({ type: "bins", data: [[t - 0.32, t - 0.02, t * t]], color: P.ref },
                    { type: "bins", data: [[t + 0.02, t + 0.32, Math.pow(t, 4)]], color: P.terakota },
                    { type: "text", x: t - 0.17, y: t * t + 81 * 0.03, text: String(t * t), color: P.secondary, mono: true },
                    { type: "text", x: t + 0.17, y: Math.pow(t, 4) + 81 * 0.03, text: String(Math.pow(t, 4)), color: P.secondary, mono: true });
      });
      return { x: [0.4, 3.6], y: [0, 81 * 1.14], xName: "Odległość od średniej", yName: "Wkład do sumy", noXGrid: true,
               xLabels: { 1: "1 SD", 2: "2 SD", 3: "3 SD" }, layers: layers,
               legend: [{ text: "z²", color: P.ref, box: true }, { text: "z⁴", color: P.terakota, box: true }],
               aria: "Kwadraty 1, 4, 9 i czwarte potęgi 1, 16, 81 dla odległości 1, 2 i 3 SD od średniej" };
    },
  };

  // ---- Rysunek opisu --------------------------------------------------------
  function rectSeries(L, horizontal) {
    return { type: "custom", silent: true, z: 2, data: L.data.map(function (r, i) { return r.concat([i]); }),
      encode: horizontal ? { y: [0, 1], x: 2 } : { x: [0, 1], y: 2 },
      renderItem: function (params, api) {
        var i = api.value(3), color = L.colors ? L.colors[i] : L.color, a, b, shape;
        if (horizontal) {
          a = api.coord([0, api.value(1)]); b = api.coord([api.value(2), api.value(0)]);
          shape = { x: a[0], y: a[1], width: b[0] - a[0], height: b[1] - a[1] };
        } else {
          a = api.coord([api.value(0), api.value(2)]); b = api.coord([api.value(1), 0]);
          shape = { x: a[0] + 0.5, y: a[1], width: Math.max(0, b[0] - a[0] - 1), height: b[1] - a[1] };
        }
        return { type: "rect", x: 0, y: 0, shape: shape, style: { fill: color, opacity: L.alpha == null ? 1 : L.alpha } };
      } };
  }

  function option(spec, P, fs, W) {
    var series = [], ymin = spec.y[0], ymax = spec.y[1], sans = C.token("--upwr-sans", "sans-serif");
    spec.layers.forEach(function (L) {
      if (L.type === "bins") series.push(rectSeries(L, false));
      else if (L.type === "hbins") series.push(rectSeries(L, true));
      else if (L.type === "band") {
        series.push({ type: "line", silent: true, z: 1, symbol: "none", data: [[L.lo, ymax], [L.hi, ymax]],
          lineStyle: { width: 0 }, areaStyle: { color: C.alpha(L.color, L.alpha), origin: "start" } });
      } else if (L.type === "vline") {
        series.push({ type: "line", silent: true, z: 4, symbol: "none",
          data: [[L.x, L.from == null ? ymin : L.from], [L.x, L.to == null ? ymax : L.to]],
          lineStyle: { color: L.color, width: L.width || 2, type: L.dash || "solid" } });
      } else if (L.type === "segment") {
        series.push({ type: "line", silent: true, z: 4, symbol: "none", data: [L.from, L.to],
          lineStyle: { color: L.color, width: L.width || 2, cap: "butt" } });
      } else if (L.type === "line") {
        series.push({ type: "line", silent: true, z: 3, symbol: "none", data: L.data,
          lineStyle: { color: L.color, width: L.width == null ? 2 : L.width, type: L.dash || "solid" },
          areaStyle: L.fill ? { color: C.alpha(L.fill, L.fillAlpha == null ? 0.5 : L.fillAlpha), origin: "start" } : undefined });
      } else if (L.type === "arrow") {
        series.push({ type: "lines", coordinateSystem: "cartesian2d", silent: true, z: 4,
          symbol: ["none", "arrow"], symbolSize: Math.round(fs * 0.9),
          data: [{ coords: [L.from, L.to] }], lineStyle: { color: L.color, width: L.width || 2, opacity: 1 } });
      } else if (L.type === "point") {
        series.push({ type: "scatter", silent: true, z: 5, symbol: L.symbol || "circle", symbolSize: L.size || 10,
          data: [[L.x, L.y]], itemStyle: { color: L.color } });
      } else if (L.type === "points") {
        series.push({ type: "scatter", silent: true, z: 5, symbolSize: L.size || 6, data: L.data,
          itemStyle: { color: L.color, opacity: L.alpha == null ? 1 : L.alpha } });
      } else if (L.type === "polygon") {
        series.push({ type: "custom", silent: true, z: 1, data: [[0]],
          renderItem: function (params, api) {
            return { type: "polygon", x: 0, y: 0, shape: { points: L.points.map(function (p) { return api.coord(p); }) },
                     style: { fill: L.fill, stroke: null } };
          } });
      } else if (L.type === "box") {
        // Każde pudełko to ta sama grupa kształtów: prostokąt, mediana i dwa wąsy.
        series.push({ type: "custom", silent: true, z: 4, data: L.boxes.map(function (b, i) { return [b.pos, i]; }),
          encode: L.horizontal ? { y: 0 } : { x: 0 },
          renderItem: function (params, api) {
            var b = L.boxes[api.value(1)], hw = b.width / 2, w = Math.max(1.2, fs / 10);
            var pt = L.horizontal ? function (val, off) { return api.coord([val, b.pos + off]); }
                                  : function (val, off) { return api.coord([b.pos + off, val]); };
            var a = pt(b.q1, -hw), c = pt(b.q3, hw);
            function line(p, q, lw) {
              return { type: "line", x: 0, y: 0, shape: { x1: p[0], y1: p[1], x2: q[0], y2: q[1] },
                       style: { stroke: L.stroke, lineWidth: lw || w } };
            }
            return { type: "group", x: 0, y: 0, children: [
              { type: "rect", x: 0, y: 0, shape: { x: Math.min(a[0], c[0]), y: Math.min(a[1], c[1]),
                  width: Math.abs(c[0] - a[0]), height: Math.abs(c[1] - a[1]) },
                style: { fill: b.fill, stroke: L.stroke, lineWidth: w } },
              line(pt(b.med, -hw), pt(b.med, hw), w * 2),
              line(pt(b.lo, 0), pt(b.q1, 0)),
              line(pt(b.q3, 0), pt(b.hi, 0))
            ] };
          } });
      } else if (L.type === "rug") {
        // Kreski obserwacji pod osią: jedna seria custom zamiast serii na każdą wartość.
        series.push({ type: "custom", silent: true, z: 3, data: L.values.map(function (t) { return [t]; }),
          encode: { x: 0 }, clip: true,
          renderItem: function (params, api) {
            var p = api.coord([api.value(0), ymin]), h = Math.max(6, fs * 0.6);
            return { type: "line", x: 0, y: 0, shape: { x1: p[0], y1: p[1], x2: p[0], y2: p[1] - h },
                     style: { stroke: L.color, lineWidth: 1, opacity: L.alpha == null ? 1 : L.alpha } };
          } });
      }
    });
    spec.layers.forEach(function (T) {
      if (T.type !== "text") return;
      var top = T.y === "top", size = Math.round(fs * (T.size || 1));
      series.push({ type: "scatter", silent: true, z: 6, symbolSize: 0,
        data: [[T.x, top ? ymax : T.y]],
        label: { show: true, formatter: T.text, color: T.color,
          position: top ? (T.align === "left" ? "insideTopLeft" : T.align === "right" ? "insideTopRight" : "insideTop") :
                    T.valign === "middle" ? (T.align === "left" ? "right" : T.align === "right" ? "left" : "inside") :
                    (T.align === "left" ? "insideBottomLeft" : T.align === "right" ? "insideBottomRight" : "top"),
          distance: top ? 0 : 4, offset: top ? [T.align === "left" ? 6 : T.align === "right" ? -6 : 0, 6] :
                    (T.align === "left" ? [4, 0] : T.align === "right" ? [-4, 0] : [0, 0]),
          fontSize: size, fontWeight: T.bold ? 600 : 400, fontStyle: T.italic ? "italic" : "normal",
          fontFamily: T.mono ? P.mono : sans } });
    });
    var xa = C.ecAxis(P, fs, { type: "value", min: spec.x[0], max: spec.x[1],
      name: spec.xName ? C.ecName(spec.xName, Math.max(250, W - 40), fs) : "", nameGap: Math.round(fs * 2.2) });
    xa.axisLabel.formatter = function (v) { return lcFmt(v, 6); };
    xa.axisLine.onZero = false;
    // Krańce osi to granice przedziałów, zwykle nie podziałki: bez etykiet przy krawędzi.
    xa.axisLabel.showMinLabel = !!spec.xTicks; xa.axisLabel.showMaxLabel = !!spec.xTicks;
    var ticks = spec.xTicks || (spec.xLabels && Object.keys(spec.xLabels).map(Number));
    if (ticks) {
      xa.axisLabel.customValues = ticks; xa.axisTick.customValues = ticks;
      xa.axisLabel.showMinLabel = true; xa.axisLabel.showMaxLabel = true; xa.axisLabel.hideOverlap = false;
      xa.splitLine = { show: false };
      if (spec.xLabels) {
        xa.axisLabel.formatter = function (v) { return spec.xLabels[Math.round(v)] || ""; };
        xa.axisLabel.fontFamily = sans;
        xa.axisLabel.lineHeight = Math.round(fs * 1.25);
        if (spec.xRotate) { xa.axisLabel.rotate = spec.xRotate; }
        var lines = Math.max.apply(null, Object.keys(spec.xLabels).map(function (k) { return String(spec.xLabels[k]).split("\n").length; }));
        xa.nameGap = Math.round(fs * (1.4 + 1.25 * lines) + (spec.xRotate ? fs * 2.5 : 0));
      }
    }
    if (spec.noXGrid) xa.splitLine = { show: false };
    // Odstęp nazwy liczy się od etykiet osi; przy stałym marginesie (alignLeft) mniejszy.
    var ya = C.ecAxis(P, fs, { type: "value", min: ymin, max: ymax, name: spec.yName || "",
      nameGap: Math.round(fs * (spec.alignLeft ? 3.8 : 2.8)) });
    if (spec.alignLeft) ya.nameMoveOverlap = false;
    ya.axisLabel.formatter = function (v) { return v < 0 ? "" : lcFmt(v, 6); };
    ya.axisLabel.showMaxLabel = false;
    ya.axisLabel.showMinLabel = ymin === 0;  // dół osi bez nieokrągłej wartości (np. 147.94)
    ya.axisLine.onZero = false;
    if (spec.yLabels) {
      var yt = Object.keys(spec.yLabels).map(Number);
      ya.axisLabel.customValues = yt; ya.axisTick.customValues = yt;
      ya.axisLabel.showMaxLabel = true; ya.axisLabel.hideOverlap = false; ya.axisLabel.fontFamily = sans;
      ya.axisLabel.formatter = function (v) { return spec.yLabels[Math.round(v)] || ""; };
    }
    if (spec.noYGrid) ya.splitLine = { show: false };
    if (spec.hideY) { ya.axisLabel.show = false; ya.axisTick.show = false; ya.axisLine.show = false; }
    // Wykresy porównywane ze sobą (alignLeft) mają stałe marginesy bez dopasowania do
    // etykiet (outerBoundsMode: "none"), żeby ich osie x się pokrywały.
    var grid = spec.alignLeft
      ? { left: Math.round(fs * 6), right: 16, top: 12, bottom: Math.round(fs * 3.4), outerBoundsMode: "none" }
      : { left: spec.yLabels ? 12 : Math.round(fs * (spec.yName ? 3.2 : 1.6)), right: 16, top: 12, bottom: Math.round(fs * 0.6) };
    return { animation: false, aria: { enabled: false }, tooltip: { show: false }, grid: grid,
             xAxis: xa, yAxis: ya, series: series };
  }

  function legendItems(legend, items) {
    legend.textContent = "";
    (items || []).forEach(function (it) {
      var item = document.createElement("span"), sw = document.createElement("span");
      item.className = "lc01-legend-item";
      sw.className = it.box ? "lc01-box-sw" : "lc01-line-sw" + (it.dash ? " is-dashed" : "");
      sw.style.setProperty("--lc-sw", it.color);
      item.appendChild(sw); item.appendChild(document.createTextNode(it.text));
      legend.appendChild(item);
    });
    legend.hidden = !(items && items.length);
  }

  // Figura: funkcja draw(v, P, k) albo obiekt {init, prepare, act, draw, update}.
  C.onReady(".lc01-stat", function (root) {
    var def = FIGURES[root.dataset.fig], els = root.querySelectorAll(".lc-ec");
    if (!def || !els.length) return false;
    var fig = typeof def === "function" ? { draw: def } : def, state = {};
    var ctls = root.querySelectorAll(".lc01-stat-ctl"), ctl = ctls[0] || null;
    var legend = root.querySelector(".lc01-legend"), charts = [];
    if (fig.init) fig.init(state);
    function value(ctl) {
      if (!ctl) return null;
      var box = ctl.matches && ctl.matches('input[type="checkbox"]') ? ctl : ctl.querySelector('input[type="checkbox"]');
      if (box) return box.checked ? "on" : "off";
      var sel = ctl.matches && ctl.matches("select") ? ctl : ctl.querySelector("select");
      if (sel) return sel.value;
      var radio = ctl.querySelector('input[type="radio"]:checked') || ctl.querySelector('input[type="radio"]');
      return radio ? radio.value : null;
    }
    // Wartości wszystkich kontrolek według identyfikatora (np. ch4_grp_var); v to pierwsza.
    function values() {
      var out = {};
      ctls.forEach(function (c) {
        var input = c.querySelector("select") || c.querySelector("input");
        var key = c.getAttribute("id") ||
          (input && (input.getAttribute("id") || input.getAttribute("name"))) || "";
        out[key] = value(c);
      });
      return out;
    }
    function draw() {
      var v = value(ctl), vals = values(), P = C.palette(), fs = C.axisFontSize(root);
      if (fig.prepare) fig.prepare(state, v);
      charts.forEach(function (chart, k) {
        if (chart.isDisposed && chart.isDisposed()) return;
        var spec = fig.draw(v, P, k, state, vals);
        chart.setOption(option(spec, P, fs, els[k].clientWidth || 640), true);
        els[k].setAttribute("aria-label", spec.aria);
        if (k === 0 && legend) legendItems(legend, spec.legend);
      });
      if (fig.update) fig.update(root, state, v);
      var key = fig.noteKey ? fig.noteKey(vals) : v;
      root.querySelectorAll("[data-when]").forEach(function (n) { n.hidden = n.getAttribute("data-when") !== key; });
    }
    for (var k = 0; k < els.length; k++) {
      charts.push(C.chart(els[k], { animation: false }, { onResize: draw }));
    }
    ctls.forEach(function (c) { C.listen(c, "change", draw); });
    if (fig.act) C.listen(root, "click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-stat-act]");
      if (!btn) return;
      fig.act(state, btn.getAttribute("data-stat-act"));
      draw();
    });
    C.zoomControl(root);
    C.onTheme(root, draw);
    draw();
  });

  window.lc01Stat = { binsBy: binsBy, binsGgplot: binsGgplot, figures: FIGURES, option: option, BIN: BIN };
})();
