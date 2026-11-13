// Ryciny statystyk opisowych liczone w przeglądarce (ECharts SVG), dawniej
// wykresy ggplot rysowane na serwerze. Dane: ankieta (window.lc01Survey)
// albo losowanie z ziarnem; rachunki: window.lc01 (chart_utils.js).
//
// Kontener i rysunek: wspólny silnik window.lcSF (statystyka/R/lc_stat_figures.js,
// R: lc_sf_figure()). Tu tylko ryciny wykładu 01, rejestrowane w lcSF.
(function () {
  "use strict";
  var C = window.lcCharts, U = window.lc01, S = window.lcSF;
  if (!C || !C.chart || !U || !S) return;

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
  var lcFmt = S.fmt;


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
  // Kolor z gradientu: stops = kolory, at = położenia 0..1 (domyślnie równe odstępy).
  function gradient(stops, at, t) {
    var pos = at || stops.map(function (c, i) { return i / (stops.length - 1); });
    t = Math.max(0, Math.min(1, t));
    var i = 0; while (i < pos.length - 2 && t > pos[i + 1]) i++;
    var f = pos[i + 1] > pos[i] ? (t - pos[i]) / (pos[i + 1] - pos[i]) : 0;
    var a = parseInt(stops[i].slice(1), 16), b = parseInt(stops[i + 1].slice(1), 16);
    return "#" + [16, 8, 0].map(function (sh) {
      var c = Math.round((a >> sh & 255) * (1 - f) + (b >> sh & 255) * f);
      return (c < 16 ? "0" : "") + c.toString(16);
    }).join("");
  }
  var status = S.status, setValue = S.setValue;
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
  // Tabela jak lc_table() w R: lcSF.table (lc_stat_figures.js).
  var esc = S.esc, lcTable = S.table;
  // round() z R: zaokrąglenie do d miejsc, wypisane bez końcowych zer.
  function rround(v, d) { return lcFmt(v, d); }

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
    // Ryc. 4.3 — reguła 68–95–99.7 na krzywej normalnej (statyczna).
    regula: function (v, P) {
      var pk = 1 / Math.sqrt(2 * Math.PI), layers = [], pts = [];
      for (var i = 0; i <= 600; i++) { var t = -3.6 + 7.2 * i / 600; pts.push([t, pk * Math.exp(-t * t / 2)]); }
      [3, 2, 1].forEach(function (k) {
        layers.push({ type: "line", width: 0, color: P.accent, fill: P.accent, fillAlpha: [0, 0.40, 0.25, 0.12][k],
                      data: pts.filter(function (p) { return Math.abs(p[0]) <= k + 1e-9; }) });
      });
      layers.push({ type: "line", data: pts, color: P.secondary, width: 1.8 });
      [[1, 1.12, "68%"], [2, 1.28, "95%"], [3, 1.44, "99.7%"]].forEach(function (b) {
        var y = pk * b[1];
        layers.push({ type: "segment", from: [-b[0], y], to: [b[0], y], color: P.accent, width: 1.3 },
                    { type: "segment", from: [-b[0], y], to: [-b[0], y - pk * 0.04], color: P.accent, width: 1.3 },
                    { type: "segment", from: [b[0], y], to: [b[0], y - pk * 0.04], color: P.accent, width: 1.3 },
                    { type: "text", x: 0, y: y, text: b[2], color: P.accent, mono: true, bold: true });
      });
      return { x: [-3.6, 3.6], y: [0, pk * 1.58], xName: "", yName: "", hideY: true, noYGrid: true,
               xLabels: { "-3": "x̄ − 3s", "-2": "x̄ − 2s", "-1": "x̄ − s", "0": "x̄", "1": "x̄ + s", "2": "x̄ + 2s", "3": "x̄ + 3s" },
               layers: layers,
               aria: "Krzywa normalna z pasami ±1, ±2 i ±3 odchylenia standardowego: 68%, 95% i 99.7% obserwacji" };
    },
    // Ryc. 4.2 — obliczanie SD krok po kroku; przycisk losuje nowe 10 pomiarów.
    sd: {
      init: function (state) { this.act(state); },
      act: function (state) { state.x = []; for (var i = 0; i < 10; i++) state.x.push(Math.round(U.rnorm(170, 8) * 10) / 10); },
      draw: function (v, P, k, state, vals, step) {
        var x = state.x, n = x.length, m = U.mean(x), s = U.sd(x);
        var lo = Math.min(Math.min.apply(null, x), m - s), hi = Math.max(Math.max.apply(null, x), m + s), pad = (hi - lo) * 0.08;
        var spec = { x: [lo - pad, hi + pad], y: [0, n + 1.2], xName: "Wzrost (cm)", yName: "", hideY: true, noYGrid: true, layers: [],
                     aria: "Dziesięć pomiarów wzrostu, średnia " + lcFmt(m, 2) + " cm, SD " + lcFmt(s, 2) + " cm" };
        if (step === 1) {
          spec.layers.push({ type: "points", data: x.map(function (t) { return [t, (n + 1) / 2]; }), color: P.niebo, size: 13 });
          return spec;
        }
        var role = step === 2 ? P.accent : P.secondary;
        var ord = x.map(function (t, i) { return i; }).sort(function (a, b) { return Math.abs(x[b] - m) - Math.abs(x[a] - m); });
        if (step >= 3) {
          spec.layers.push({ type: "rect", x0: m - s, x1: m + s, y0: 0, y1: n + 0.3, fill: P.accent, alpha: 0.08, z: 1 },
            { type: "vline", x: m - s, color: P.accent, width: 1.5, dash: [5, 4] },
            { type: "vline", x: m + s, color: P.accent, width: 1.5, dash: [5, 4] },
            { type: "text", x: m - s, y: 0.3, text: "śr. − SD\n" + lcFmt(m - s, 1), color: P.accent, bold: true, size: 0.85, valign: "middle" },
            { type: "text", x: m + s, y: 0.3, text: "śr. + SD\n" + lcFmt(m + s, 1), color: P.accent, bold: true, size: 0.85, valign: "middle" },
            { type: "text", x: m, y: 0.5, text: "SD = " + lcFmt(s, 2) + " cm", color: P.accent, bold: true, size: 1.1, valign: "middle" });
        }
        spec.layers.push({ type: "vline", x: m, color: role, width: 1.5, dash: [5, 4] },
          { type: "arrows", color: role, width: 1.6, items: ord.map(function (i, r) { return { from: [m, n - r], to: [x[i], n - r] }; }) },
          { type: "points", data: ord.map(function (i, r) { return [x[i], n - r]; }), color: P.niebo, size: 13 },
          { type: "text", x: m, y: n + 0.8, text: "średnia = " + lcFmt(m, 2), color: role, bold: true, valign: "middle" });
        return spec;
      },
      text: function (state, v, vals, step) {
        var x = state.x, n = x.length, m = U.mean(x);
        if (step === 1) return "Dziesięć pomiarów wzrostu na osi liczbowej. Każdy punkt to jedna obserwacja.";
        if (step === 2) return "Średnia wynosi " + lcFmt(m, 2) + " cm. Strzałki to odchylenia od średniej, tabela podaje ich kwadraty.";
        var ss = x.reduce(function (a, t) { return a + (t - m) * (t - m); }, 0), variance = ss / (n - 1);
        return "Wariancja \\(s^2\\) = " + lcFmt(ss, 2) + " / " + (n - 1) + " = " + lcFmt(variance, 2) + " cm². " +
               "Odchylenie standardowe \\(s = \\sqrt{" + lcFmt(variance, 2) + "} = " + lcFmt(Math.sqrt(variance), 2) +
               "\\) cm. Pas na wykresie to \\(\\bar{x} \\pm s\\).";
      },
      table: function (state, v, vals, step) {
        if (step < 2) return "";
        var x = state.x, m = U.mean(x), ss = 0;
        var rows = x.map(function (t, i) { var d = t - m; ss += d * d; return [String(i + 1), t, C.round(d, 2), C.round(d * d, 2)]; });
        return lcTable([{ label: "i", row: true }, { label: "xᵢ", digits: 1 }, { label: "xᵢ − x̄", digits: 2 }, { label: "(xᵢ − x̄)²", digits: 2 }],
                       rows, step >= 3 ? ["SUMA", "", "", C.round(ss, 2)] : null);
      }
    },
    // Ryc. 4.5 — wykres pudełkowy krok po kroku; krok 6: pudełko (k = 0) i histogram (k = 1).
    bp: {
      init: function (state) { this.act(state); },
      act: function (state) {
        state.x = [];
        for (var i = 0; i < 27; i++) state.x.push(Math.round(U.rnorm(170, 8) * 10) / 10);
        state.x.push(145, 198, 200);
      },
      stats: function (x) {
        var b = boxStats(x), iqr = b.q3 - b.q1;
        b.fenceLo = b.q1 - 1.5 * iqr; b.fenceHi = b.q3 + 1.5 * iqr; b.iqr = iqr;
        return b;
      },
      draw: function (v, P, k, state, vals, step) {
        var x = state.x, b = this.stats(x), ink = P.ink;
        var lo = Math.min(Math.min.apply(null, x), b.fenceLo), hi = Math.max(Math.max.apply(null, x), b.fenceHi), pad = (hi - lo) * 0.06;
        var frame = { x: [lo - pad, hi + pad], xName: "Wzrost (cm)", yName: "", xLabelEvery: 20, layers: [] };
        if (step === 6) {
          if (k === 0) {
            var j = U.seeded(3);
            return Object.assign(frame, { y: [0.5, 1.5], hideY: true, noYGrid: true, alignLeft: true, xName: "",
              layers: [{ type: "box", horizontal: true, stroke: ink, boxes: [{ pos: 1, width: 0.4, fill: C.alpha(P.niebo, 0.7),
                         q1: b.q1, med: b.med, q3: b.q3, lo: b.lo, hi: b.hi }] },
                       { type: "points", data: b.out.map(function (t) { return [t, 1]; }), color: P.secondary, size: 9 },
                       { type: "points", data: x.map(function (t) { return [t, 1 + j.runif(-0.05, 0.05)]; }), color: P.secondary, alpha: 0.4, size: 6 }],
              aria: "Gotowy wykres pudełkowy: mediana " + lcFmt(b.med, 1) + " cm, kwartyle " + lcFmt(b.q1, 1) + " i " + lcFmt(b.q3, 1) + " cm" });
          }
          var h = counts(binsGgplot(x, 15)), top = maxY(h) * 1.08;
          return Object.assign(frame, { y: [0, top], yName: "Liczebność", alignLeft: true,
            layers: [{ type: "bins", data: h, color: P.niebo, alpha: 0.7 },
                     { type: "vline", x: b.med, color: P.secondary, width: 1.6 },
                     { type: "vline", x: b.q1, color: P.secondary, width: 1.2, dash: [5, 4] },
                     { type: "vline", x: b.q3, color: P.secondary, width: 1.2, dash: [5, 4] }],
            aria: "Histogram tych samych 30 pomiarów z medianą i kwartylami" });
        }
        if (k === 1) return null;
        var jit = U.seeded(42), spread = step <= 3 ? 0.3 : 0.2;
        var jy = x.map(function () { return jit.runif(-spread, spread); });
        var spec = Object.assign(frame, { y: [-0.9, 0.9], hideY: true, noYGrid: true,
          aria: "Krok " + step + " budowy wykresu pudełkowego dla 30 pomiarów wzrostu" });
        var L = spec.layers, dataPts = function (size, alpha, filter) {
          var d = []; x.forEach(function (t, i) { if (!filter || filter(t)) d.push([t, jy[i]]); });
          L.push({ type: "points", data: d, color: P.niebo, alpha: alpha, size: size });
        };
        var label = function (xx, y, text, color) { L.push({ type: "text", x: xx, y: y, text: text, color: color, bold: true, size: 0.9, valign: "middle" }); };
        if (step <= 2) {
          dataPts(9, 0.7);
          if (step === 2) { L.push({ type: "vline", x: b.med, color: P.accent, width: 2 }); label(b.med, 0.65, "Mediana", P.accent); }
          return spec;
        }
        if (step === 3) {
          L.push({ type: "rect", x0: b.q1, x1: b.q3, y0: -0.5, y1: 0.5, fill: P.niebo, alpha: 0.2, stroke: ink, lineWidth: 0.8, z: 1 });
          dataPts(9, 0.7);
          L.push({ type: "vline", x: b.med, color: P.secondary, width: 1.6 },
                 { type: "vline", x: b.q1, color: P.accent, width: 1.5, dash: [5, 4] },
                 { type: "vline", x: b.q3, color: P.accent, width: 1.5, dash: [5, 4] });
          label(b.med, 0.7, "Mediana", P.secondary); label(b.q1, -0.65, "Q1", P.accent); label(b.q3, -0.65, "Q3", P.accent);
          var mid = (b.q1 + b.q3) / 2;
          // „IQR” tuż nad pudełkiem, żeby nie nachodził na „Mediana” przy medianie w środku.
          L.push({ type: "text", x: mid, y: 0.58, text: "IQR", color: P.accent, bold: true, size: 0.9, valign: "middle" });
          return spec;
        }
        L.push({ type: "rect", x0: b.q1, x1: b.q3, y0: -0.4, y1: 0.4, fill: P.niebo, alpha: 0.2, stroke: ink, lineWidth: 0.8, z: 1 },
               { type: "segment", from: [b.med, -0.4], to: [b.med, 0.4], color: P.secondary, width: 2.4 });
        var fence = step === 4 ? P.accent : P.secondary;
        L.push({ type: "vline", x: b.fenceLo, color: fence, width: 1.4, dash: [5, 4] },
               { type: "vline", x: b.fenceHi, color: fence, width: 1.4, dash: [5, 4] });
        if (step === 4) {
          L.push({ type: "arrows", color: P.accent, width: 1.6, items: [{ from: [b.q1, -0.6], to: [b.fenceLo, -0.6] }, { from: [b.q3, -0.6], to: [b.fenceHi, -0.6] }] });
          label((b.q1 + b.fenceLo) / 2, -0.75, "1.5 IQR", P.accent); label((b.q3 + b.fenceHi) / 2, -0.75, "1.5 IQR", P.accent);
          dataPts(8, 0.5);
          return spec;
        }
        L.push({ type: "segs", color: P.accent, width: 2, items: [
          { from: [b.lo, 0], to: [b.q1, 0] }, { from: [b.lo, -0.2], to: [b.lo, 0.2] },
          { from: [b.q3, 0], to: [b.hi, 0] }, { from: [b.hi, -0.2], to: [b.hi, 0.2] }] });
        dataPts(8, 0.5, function (t) { return t >= b.fenceLo && t <= b.fenceHi; });
        var outPts = []; x.forEach(function (t, i) { if (t < b.fenceLo || t > b.fenceHi) outPts.push([t, jy[i]]); });
        if (outPts.length) {
          L.push({ type: "points", data: outPts, color: P.accent, symbol: "diamond", size: 14 });
          label(U.mean(outPts.map(function (p) { return p[0]; })), 0.55, "wartości odstające: " + outPts.length, P.accent);
        }
        return spec;
      },
      text: function (state, v, vals, step) {
        var x = state.x, b = this.stats(x);
        if (step === 1) return "30 pomiarów wzrostu na osi liczbowej. Widać zakres, ale trudno coś szybko odczytać.";
        if (step === 2) return "Mediana = " + lcFmt(b.med, 1) + " cm dzieli posortowane dane na dwie połowy.";
        if (step === 3) return "Q1 = " + lcFmt(b.q1, 1) + " cm, Q3 = " + lcFmt(b.q3, 1) + " cm. Pudełko obejmuje środkowe 50% danych, IQR = " + lcFmt(b.iqr, 1) + " cm.";
        if (step === 4) return "Granice: Q1 − 1.5 IQR = " + lcFmt(b.fenceLo, 1) + " cm i Q3 + 1.5 IQR = " + lcFmt(b.fenceHi, 1) +
                               " cm. Poza nimi leży obserwacji: " + b.out.length + ".";
        if (step === 5) return "Wąsy kończą się na ostatnich obserwacjach przed granicami: " + lcFmt(b.lo, 1) + " i " + lcFmt(b.hi, 1) + " cm. " +
          (b.out.length ? "Wartości odstające: " + b.out.map(function (t) { return lcFmt(t, 1); }).join(", ") + " cm." : "Brak wartości odstających.");
        return "Gotowy wykres pudełkowy (u góry) obok histogramu tych samych danych (u dołu).";
      }
    },
    // Ryc. 5.1 — skośność krok po kroku: dane, standaryzacja, trzecie potęgi, bilans.
    sk: {
      data: function (state, name) {
        if (state.name === name) return state.d;
        var x = U.survey(name), m = U.mean(x), s = U.sd(x), j = U.seeded(7);
        var z = x.map(function (t) { return (t - m) / s; });
        state.name = name;
        state.d = { x: x, m: m, s: s, z: z, z3: z.map(function (t) { return t * t * t; }),
                    jit: x.map(function () { return j.runif(-0.35, 0.35); }), label: U.surveyMeta(name).label };
        return state.d;
      },
      draw: function (v, P, k, state, vals, step) {
        var d = this.data(state, v), side = d.z.map(function (t) { return t < 0 ? P.niebo : P.terakota; });
        if (step <= 2) {
          var xv = step === 1 ? d.x : d.z, ctr = step === 1 ? d.m : 0;
          var lo = Math.min.apply(null, xv), hi = Math.max.apply(null, xv), pad = (hi - lo) * 0.05, layers = [];
          if (step === 2) [-3, -2, -1, 0, 1, 2, 3].forEach(function (t) { layers.push({ type: "vline", x: t, color: P.ref, width: 0.8, dash: [4, 4] }); });
          layers.push({ type: "points", data: xv.map(function (t, i) { return [t, d.jit[i]]; }),
                        colors: step === 2 ? side : null, color: P.niebo, alpha: 0.7, size: 7 },
                      { type: "vline", x: ctr, color: step === 1 ? P.accent : P.secondary, width: 2 },
                      { type: "text", x: ctr + (hi - lo) * 0.01, y: 0.5, align: "left", bold: true, size: 0.9,
                        text: step === 1 ? "x̄ = " + d.m.toFixed(1) : "x̄", color: step === 1 ? P.accent : P.secondary });
          return { x: [lo - pad, hi + pad], y: [-0.5, 0.62], hideY: true, noYGrid: true, yName: "",
                   xName: step === 1 ? d.label : "Odległość od średniej (w SD)", xTicks: step === 2 ? [-3, -2, -1, 0, 1, 2, 3, 4].filter(function (t) { return t >= lo - pad && t <= hi + pad; }) : null,
                   layers: layers, aria: step === 1 ? "Obserwacje zmiennej " + d.label + " ze średnią" : "Te same obserwacje w jednostkach SD" };
        }
        if (step === 3) {
          var zlo = Math.min.apply(null, d.z), zhi = Math.max.apply(null, d.z), ylo = Math.min(0, Math.min.apply(null, d.z3)), yhi = Math.max(0, Math.max.apply(null, d.z3));
          var py = (yhi - ylo) * 0.05, pz = (zhi - zlo) * 0.05;
          return { x: [zlo - pz, zhi + pz], y: [ylo - py, yhi + py], xName: "Odległość od średniej (w SD)", yName: "z³",
                   layers: [{ type: "segment", from: [zlo - pz, 0], to: [zhi + pz, 0], color: P.secondary, width: 1 },
                            { type: "segs", width: 1.2, alpha: 0.7, items: d.z.map(function (t, i) { return { from: [t, 0], to: [t, d.z3[i]], color: side[i] }; }) },
                            { type: "points", data: d.z.map(function (t, i) { return [t, d.z3[i]]; }), colors: side, size: 5 }],
                   aria: "Trzecie potęgi odległości od średniej: dalekie obserwacje rosną najmocniej" };
        }
        var l = 0, r = 0; d.z3.forEach(function (t) { if (t < 0) l += t; else r += t; });
        var tot = l + r, lim = Math.max(Math.abs(l), Math.abs(r), Math.abs(tot)) * 1.25, indygo = C.token("--upwr-cat-indygo", "#3a5a8a");
        var vals3 = [l, r, tot], cols = [P.niebo, P.terakota, indygo];
        return { x: [-lim, lim], y: [0.4, 3.6], xName: "Suma z³", yName: "", yLabels: { 3: "Lewa strona", 2: "Prawa strona", 1: "Suma" }, noYGrid: true,
                 layers: [{ type: "vline", x: 0, color: P.secondary, width: 1 },
                          { type: "hbins", data: vals3.map(function (t, i) { return [3 - i - 0.275, 3 - i + 0.275, t]; }), colors: cols, alpha: 0.9 }]
                   .concat(vals3.map(function (t, i) {
                     return { type: "text", x: t, y: 3 - i, text: t.toFixed(1), color: P.secondary, mono: true, valign: "middle", align: t < 0 ? "right" : "left" };
                   })),
                 aria: "Suma trzecich potęg: lewa strona " + l.toFixed(1) + ", prawa " + r.toFixed(1) + ", razem " + tot.toFixed(1) };
      },
      text: function (state, v, vals, step) {
        var d = this.data(state, v), n = d.x.length;
        if (step === 1) return n + " obserwacji i ich średnia.";
        if (step === 2) return "Ta sama oś w jednostkach SD: średnia to 0, odchylenie standardowe to 1.";
        if (step === 3) return "Trzecia potęga zachowuje znak i wydłuża dalekie obserwacje.";
        var tot = d.z3.reduce(function (a, t) { return a + t; }, 0);
        return "Suma " + lcFmt(tot, 1) + " podzielona przez " + n + " daje skośność " + lcFmt(tot / n, 2) + ".";
      }
    },
    // Ryc. 3.1 — budowa histogramu w 7 krokach; suwak szerokości od kroku 3.
    hist: {
      DEF: { wzrost: [1, 15, 5, 1, "cm"], waga: [2, 20, 5, 1, "kg"], czas_dojazdu: [2, 20, 5, 1, "min"], srednia_ocen: [0.1, 1, 0.3, 0.05, "pkt"] },
      // Zmiana zmiennej przywraca domyślną szerokość i zakres suwaka (jak dawny renderUI w R).
      prepare: function (state, v, vals, root) {
        var slider = root.querySelector("#ch3_hist_bin_width"), out = root.querySelector('[data-value="ch3_hist_bin_width"]');
        var name = vals.ch3_hist_var, d = this.DEF[name];
        if (slider && state.name !== name) {
          slider.setAttribute("min", d[0]); slider.setAttribute("max", d[1]); slider.setAttribute("step", d[3]);
          slider.value = String(d[2]);
          state.name = name;
        }
        state.w = slider ? Number(slider.value) : d[2];
        if (out) out.textContent = lcFmt(state.w, 2) + " " + d[4];
      },
      model: function (name, w) {
        var x = U.survey(name), wr = function (t) { return roundTo(t, w); };
        var start = wr(Math.floor(Math.min.apply(null, x) / w + 1e-9) * w), end = wr(Math.ceil(Math.max.apply(null, x) / w - 1e-9) * w + w), breaks = [];
        for (var e = start; e <= end + w * 1e-9; e = wr(e + w)) breaks.push(e);
        var bin = x.map(function (t) {
          var i = Math.floor((t - start) / w + 1e-9);
          return Math.min(i, breaks.length - 2);
        });
        var count = breaks.slice(1).map(function () { return 0; });
        bin.forEach(function (i) { count[i]++; });
        var nz = count.map(function (c, i) { return c ? i : -1; }).filter(function (i) { return i >= 0; });
        var first = Math.max(0, nz[0] - 1), last = Math.min(count.length - 1, nz[nz.length - 1] + 1), stats = [];
        for (var i = first; i <= last; i++) stats.push({ i: i, a: breaks[i], b: breaks[i + 1], n: count[i] });
        var order = x.map(function (t, i) { return i; }).sort(function (p, q) { return bin[p] - bin[q] || x[p] - x[q]; }), seen = {}, k = [];
        order.forEach(function (i) { seen[bin[i]] = (seen[bin[i]] || 0) + 1; k[i] = seen[bin[i]]; });
        return { x: x, breaks: breaks, bin: bin, stats: stats, k: k };
      },
      draw: function (v, P, kk, state, vals, step) {
        var name = vals.ch3_hist_var, d = this.DEF[name], w = state.w, md = this.model(name, w), x = md.x, n = x.length;
        var lab = U.surveyMeta(name).label, lo0 = Math.min.apply(null, x), hi0 = Math.max.apply(null, x), xr = [lo0 - (hi0 - lo0) * 0.05, hi0 + (hi0 - lo0) * 0.05];
        var parity = function (i) { return (i + 1) % 2 === 1 ? P.niebo : P.bursztyn; };
        var edgeTicks = md.breaks.filter(function (e, i) { return i % 2 === 0 && e >= xr[0] && e <= xr[1]; });
        if (step === 7) {
          var widths = [d[0], w, d[1]].filter(function (t, i, a) { return a.indexOf(t) === i; });
          if (kk >= widths.length) return null;
          var wi = widths[kk], note = ["najwęższa", "z suwaka", "najszersza"][[d[0], w, d[1]].indexOf(wi)];
          var bnd = Math.floor(lo0 / w + 1e-9) * w, origin = bnd - Math.ceil((bnd - lo0) / wi - 1e-9) * wi, h = [];
          for (var e = origin; e <= hi0; e += wi) h.push([e, e + wi, 0]);
          x.forEach(function (t) { var i = Math.floor((t - origin) / wi + 1e-9); if (h[i]) h[i][2]++; });
          var col = wi === w ? P.accent : P.soft;
          return { x: xr, y: [0, maxY(h) * 1.25], xName: kk === widths.length - 1 ? lab : "", yName: "", alignLeft: true,
                   layers: [{ type: "bins", data: h, color: col, alpha: 0.75 },
                            { type: "text", x: xr[0], y: "top", align: "left", bold: true, color: col,
                              text: "szerokość " + lcFmt(wi, 2) + " " + d[4] + " (" + note + ")" }],
                   aria: "Histogram przy szerokości przedziału " + lcFmt(wi, 2) + " " + d[4] };
        }
        if (kk > 0) return null;
        var spec = { x: xr, xName: lab, yName: "", layers: [], aria: "Krok " + step + " budowy histogramu zmiennej " + lab };
        var L = spec.layers;
        if (step === 1) {
          var j1 = U.seeded(5);
          L.push({ type: "points", data: x.map(function (t) { return [t, j1.runif(-0.3, 0.3)]; }), color: P.niebo, alpha: 0.7, size: 9 });
          return Object.assign(spec, { y: [-0.5, 0.5], hideY: true, noYGrid: true });
        }
        if (step === 2) {
          var sx = U.sorted(x), pad = (hi0 - lo0) * 0.05;
          return { x: [0, n + 1], y: [lo0 - pad, hi0 + pad], xName: "Numer obserwacji po posortowaniu", yName: lab,
                   layers: [{ type: "points", data: sx.map(function (t, i) { return [i + 1, t]; }), color: P.niebo, alpha: 0.7, size: 6 }],
                   aria: "Wartości zmiennej " + lab + " posortowane od najmniejszej do największej" };
        }
        if (step === 3 || step === 4) {
          var bands = [];
          for (var i = 0; i + 1 < md.breaks.length; i++) if (md.breaks[i + 1] > xr[0] && md.breaks[i] < xr[1]) bands.push(i);
          bands.forEach(function (i) { L.push({ type: "band", lo: md.breaks[i], hi: md.breaks[i + 1], color: parity(i), alpha: step === 3 ? 0.18 : 0.12 }); });
          md.breaks.forEach(function (e) { if (e >= xr[0] && e <= xr[1]) L.push({ type: "vline", x: e, color: P.soft, width: 0.6 }); });
          var j2 = U.seeded(11), pts = x.map(function (t) { return [t, j2.runif(-0.3, 0.3)]; });
          L.push({ type: "points", data: pts, size: 7, alpha: step === 3 ? 0.55 : 0.9,
                   color: P.soft, colors: step === 4 ? md.bin.map(parity) : null });
          var top3 = step === 3 ? 0.8 : 0.45;
          if (step === 3) {
            var mb = bands[Math.ceil(bands.length / 2) - 1], a = md.breaks[mb], bb = md.breaks[mb + 1];
            L.push({ type: "arrows", both: true, head: 0.5, color: P.accent, width: 1.6, items: [{ from: [a, 0.55], to: [bb, 0.55] }] },
                   { type: "text", x: (a + bb) / 2, y: 0.7, text: "szerokość " + lcFmt(w, 2) + " " + d[4], color: P.accent, bold: true, valign: "middle" });
          }
          return Object.assign(spec, { y: [-0.45, top3], hideY: true, noYGrid: true, xTicks: edgeTicks });
        }
        var max = Math.max.apply(null, md.stats.map(function (b) { return b.n; })), top = max * 1.15;
        Object.assign(spec, { y: [0, top], yName: "Liczba obserwacji", xTicks: edgeTicks, noXGrid: true });
        var labels = md.stats.filter(function (b) { return b.n; }).map(function (b) {
          return { type: "text", x: (b.a + b.b) / 2, y: b.n + max * 0.03, text: String(b.n), color: P.secondary, mono: true, bold: true, size: 0.9 };
        });
        if (step === 5) {
          md.breaks.forEach(function (e) { if (e >= xr[0] && e <= xr[1]) L.push({ type: "vline", x: e, color: P.rule, width: 0.6 }); });
          var size = Math.max(4, Math.min(10, 300 / max));
          L.push({ type: "points", size: size, colors: md.bin.map(parity),
                   data: x.map(function (t, i) { var b0 = md.breaks[md.bin[i]], b1 = md.breaks[md.bin[i] + 1]; return [(b0 + b1) / 2, md.k[i] - 0.5]; }) });
        } else {
          L.push({ type: "rects", fill: P.niebo, alpha: 0.55, stroke: "#ffffff", lineWidth: 0.6,
                   items: x.map(function (t, i) { return [md.breaks[md.bin[i]], md.breaks[md.bin[i] + 1], md.k[i] - 1, md.k[i]]; }) },
                 { type: "rects", stroke: P.ink, lineWidth: 1, z: 3,
                   items: md.stats.filter(function (b) { return b.n; }).map(function (b) { return [b.a, b.b, 0, b.n]; }) });
        }
        spec.layers = L.concat(labels);
        return spec;
      },
      text: function (state, v, vals, step) {
        var name = vals.ch3_hist_var, x = U.survey(name), unit = this.DEF[name][4];
        return {
          1: "Mamy " + x.length + " obserwacji — każdy punkt to jedna wartość. Trudno z tego odczytać rozkład, prawda?",
          2: "Sortujemy wartości od najmniejszej (" + lcFmt(Math.min.apply(null, x), 1) + " " + unit + ") do największej (" +
             lcFmt(Math.max.apply(null, x), 1) + " " + unit + ") i rysujemy je po kolei. Tam, gdzie wykres jest płaski, wartości leżą gęsto; strome końce to rzadkie wartości skrajne.",
          3: "Dzielimy oś na równe przedziały (biny) o szerokości " + lcFmt(state.w, 2) + " " + unit + ". Każdy bin to „koszyk” na obserwacje.",
          4: "Każda obserwacja trafia do swojego binu i przyjmuje jego kolor.",
          5: "Każda obserwacja ląduje w kolumnie swojego binu i staje na poprzedniej. Wysokość stosu to liczba obserwacji w binie.",
          6: "Stos zamieniamy w słupek tej samej wysokości. Każdy kafelek to jedna obserwacja, a słupek to ich liczba. To już jest histogram.",
          7: "Te same dane przy trzech szerokościach binu: najwęższej, wybranej suwakiem i najszerszej. Za wąskie biny dają poszarpany wykres, za szerokie ukrywają kształt."
        }[step];
      },
      table: function (state, v, vals, step) {
        if (step < 5) return "";
        var md = this.model(vals.ch3_hist_var, state.w), n = md.x.length;
        return lcTable([{ label: "Przedział", row: true }, { label: "Liczba obs.", digits: 0 }, { label: "Procent (%)", digits: 1 }],
          md.stats.filter(function (b) { return b.n; }).map(function (b) {
            return ["[" + lcFmt(b.a, 6) + ", " + lcFmt(b.b, 6) + ")", b.n, C.round(100 * b.n / n, 1)];
          }));
      }
    },
    // Ryc. 2.4 — te same dane w różnych paletach: słupki kierunków (k = 0)
    // i mapa gęstości wzrost × waga (k = 1, jak stat_density_2d z ggplot2).
    colors: {
      SAFE: ["#e6194B", "#3cb44b", "#4363d8", "#f58231", "#911eb4", "#42d4f4", "#f032e6", "#bfef45", "#fabed4", "#469990",
             "#dcbeff", "#9A6324", "#800000", "#aaffc3", "#808000", "#000075", "#a9a9a9", "#e6beff", "#ffd8b1", "#fffac8"],
      palette: function (name, P, random) {
        if (random) return { cat: random, seq: random.slice(0, 3) };
        var lv = U.surveyMeta("kierunek").levels;
        return {
          neutral: { cat: [P.ref, P.ref, P.ref, P.ref], seq: ["#f6f3ee", P.rule, P.ref, "#3d3833"] },
          highlight: { cat: lv.map(function (t) { return t === "Informatyka" ? P.accent : P.rule; }),
                       seq: ["#f6f3ee", P.rule, P.rule, P.accent], at: [0, 0.5, 0.8, 1] },
          viridis: { cat: ["#440154", "#31688e", "#35b779", "#fde725"], seq: ["#440154", "#3b528b", "#21918c", "#5ec962", "#fde725"] },
          brewer: { cat: ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"], seq: ["#ffffd9", "#c7e9b4", "#41b6c4", "#225ea8", "#081d58"] },
          okabe_ito: { cat: ["#E69F00", "#56B4E9", "#009E73", "#CC79A7"], seq: ["#eef5fa", "#0072B2"] },
          jupwr: { cat: ["#9c3b4a", "#d99a5b", "#3f6f9e", "#7a9b8e"], seq: ["#fbe3e5", "#eec79a", "#d99a5b", "#c85264", "#9c3b4a", "#6e2632"] }
        }[name];
      },
      init: function (state) {
        var x = U.survey("wzrost"), y = U.survey("waga");
        var lims = [Math.min.apply(null, x), Math.max.apply(null, x), Math.min.apply(null, y), Math.max.apply(null, y)];
        state.grid = U.kde2d(x, y, 60, lims);
        state.zmax = Math.max.apply(null, state.grid.z.map(function (r) { return Math.max.apply(null, r); }));
      },
      // Zmiana palety kasuje losowe kolory (jak dawniej w R).
      prepare: function (state, v) { if (state.pal !== v) { state.random = null; state.pal = v; } },
      act: function (state) { state.random = shuffle(this.SAFE).slice(0, 4); },
      noteKey: function (vals, state) { return state.random ? "random" : vals.ch2_color_palette; },
      draw: function (v, P, k, state) {
        var pal = this.palette(v, P, state.random);
        if (k === 0) {
          var lv = U.surveyMeta("kierunek").levels, n = U.counts(U.survey("kierunek"), lv);
          var rows = catRows(n, 0.7), top = Math.max.apply(null, n) * 1.15;
          var short = { Biologia: "Biol.", Ekonomia: "Ekon.", Informatyka: "Inf.", Psychologia: "Psych." };
          return { x: [0.4, 4.6], y: [0, top], xName: "Kierunek", yName: "Liczebność", noXGrid: true,
                   xLabels: catLabels(lv, function (t) { return short[t]; }),
                   layers: [{ type: "bins", data: rows, colors: pal.cat }].concat(countTexts(rows, top, P)),
                   aria: "Liczebności kierunków w wybranej palecie kolorów" };
        }
        var g = state.grid, dx = (g.x[1] - g.x[0]) / 2, dy = (g.y[1] - g.y[0]) / 2, items = [];
        g.x.forEach(function (gx, i) { g.y.forEach(function (gy, j) {
          items.push({ r: [gx - dx, gx + dx, gy - dy, gy + dy], color: gradient(pal.seq, pal.at, g.z[i][j] / state.zmax) });
        }); });
        return { x: [g.x[0] - dx, g.x[g.x.length - 1] + dx], y: [g.y[0] - dy, g.y[g.y.length - 1] + dy],
                 xName: "Wzrost (cm)", yName: "Waga (kg)", noXGrid: true, noYGrid: true,
                 layers: [{ type: "cells", items: items }],
                 legend: { title: "Liczba osób", from: "mało", to: "dużo", stops: pal.seq, at: pal.at },
                 aria: "Mapa gęstości wzrostu i wagi w wybranej palecie: najwięcej osób około 170 cm i 65 kg" };
      }
    },
  };

  window.lcSF.register(FIGURES);
  window.lc01Stat = { binsBy: binsBy, binsGgplot: binsGgplot, figures: FIGURES, option: S.option, BIN: BIN };
})();
