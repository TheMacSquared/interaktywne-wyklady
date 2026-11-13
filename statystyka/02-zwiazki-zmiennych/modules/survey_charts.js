// Ryciny wykładu 02 na danych ankiety, liczone w przeglądarce (dawniej ggplot
// i renderUI na serwerze): Ryc. 2.2 (tabela krzyżowa ze słupkami albo mapą
// ciepła) i Ryc. 3.2 (zmienna ilościowa w grupach: pudełka i tabela).
// Dane: window.lc01Survey (01-typy-danych/modules/survey_data.R), rachunki jak
// w R: window.lc01 (01-typy-danych/modules/chart_utils.js), rysunek: wspólny
// silnik window.lcSF (R/lc_stat_figures.js, w R lc_sf_figure()).
(function () {
  "use strict";
  var C = window.lcCharts, U = window.lc01, S = window.lcSF;
  if (!C || !C.chart || !U || !S) return;
  var lcFmt = S.fmt, esc = S.esc;

  // ---- Pomocniki ------------------------------------------------------------
  // Paleta kategoryczna upwr_cat_n(): kolejność jak w R/palette.R, kolor z motywu.
  var CAT = [["grafit", "#2d3b3d"], ["bursztyn", "#c08540"], ["niebo", "#6a9dc4"], ["szalwia", "#4a8a6a"],
             ["kurkuma", "#cbb858"], ["indygo", "#3a5a8a"], ["terakota", "#b25838"], ["wrzos", "#a07894"]];
  function catColors(n) {
    return CAT.slice(0, n).map(function (c) { return C.token("--upwr-cat-" + c[0], c[1]); });
  }
  function catVars(n) {
    return CAT.slice(0, n).map(function (c) { return "var(--upwr-cat-" + c[0] + ")"; });
  }
  // Sekwencyjna skala bordowa (upwr_seq_burgundy, scale_fill_gradientn).
  var SEQ = ["#f5f0e6", "#e8d5c8", "#d9a89a", "#c27570", "#a04550", "#7a2535", "#4a0e1e"];
  function seqColor(t) {
    var p = Math.max(0, Math.min(1, t)) * (SEQ.length - 1), i = Math.min(SEQ.length - 2, Math.floor(p)), f = p - i;
    var a = parseInt(SEQ[i].slice(1), 16), b = parseInt(SEQ[i + 1].slice(1), 16);
    return "#" + [16, 8, 0].map(function (sh) {
      var c = Math.round((a >> sh & 255) * (1 - f) + (b >> sh & 255) * f);
      return (c < 16 ? "0" : "") + c.toString(16);
    }).join("");
  }
  // scales::label_wrap(n): łamanie po słowach, linia krótsza niż n znaków (strwrap).
  function wrap(text, n) {
    var lines = [], line = "";
    String(text).split(/\s+/).forEach(function (w) {
      if (line && line.length + w.length + 1 >= n) { lines.push(line); line = ""; }
      line += (line ? " " : "") + w;
    });
    if (line) lines.push(line);
    return lines.join("\n");
  }
  function labelsAt(levels, pos) {
    var out = {};
    levels.forEach(function (lv, i) { out[pos(i)] = wrap(lv, 12); });
    return out;
  }
  function pad(t) { return '<span class="lc-pad" aria-hidden="true">' + t + "</span>"; }
  function zeros(n) { return n > 0 ? new Array(n + 1).join("0") : ""; }
  // lc_num() z R: liczba z niewidocznym dopełnieniem części całkowitej i ułamka.
  function intWidth(xs, d) {
    return Math.max.apply(null, xs.filter(isFinite).map(function (v) { return lcFmt(v, d).split(".")[0].length; }));
  }
  function num(v, d, iw) {
    if (!isFinite(v)) return "–";
    var shown = lcFmt(v, d), lead = iw != null ? zeros(iw - shown.split(".")[0].length) : "";
    var out = (lead ? pad(lead) : "") + shown;
    if (!d) return out;
    var frac = shown.split(".")[1] || "", rest = (frac ? "" : ".") + zeros(d - frac.length);
    return out + (rest ? pad(rest) : "");
  }
  function caption(html) { return '<div class="lc-caption" data-tone="info"><div>' + html + "</div></div>"; }
  function setSelect(root, id, value) {
    var sel = root && root.querySelector("#" + id);
    if (sel) sel.value = value;
  }
  // Ankieta: kolumna jako etykiety kategorii (czynnik) albo liczby.
  function levelsOf(name) { return U.surveyMeta(name).levels; }

  // ---- Ryc. 2.2: tabela krzyżowa ---------------------------------------------
  // table(row, col) z R: wszystkie poziomy, także puste komórki.
  function crossTab(rowVar, colVar) {
    var r = U.survey(rowVar), c = U.survey(colVar), rl = levelsOf(rowVar), cl = levelsOf(colVar);
    var n = rl.map(function () { return cl.map(function () { return 0; }); });
    r.forEach(function (a, i) {
      var p = rl.indexOf(a), q = cl.indexOf(c[i]);
      if (p >= 0 && q >= 0) n[p][q]++;
    });
    return { rows: rl, cols: cl, n: n };
  }
  function sumOf(a) { return a.reduce(function (s, t) { return s + t; }, 0); }
  // Wartości tabeli dla rodzaju counts / row_pct / col_pct (jak lc_crosstab()).
  function crossValues(t, type) {
    var rowTot = t.n.map(sumOf), colTot = t.cols.map(function (c, j) { return sumOf(t.n.map(function (r) { return r[j]; })); });
    var total = sumOf(rowTot);
    var value = t.n.map(function (r, i) {
      return r.map(function (v, j) { return type === "row_pct" ? v / rowTot[i] * 100 : type === "col_pct" ? v / colTot[j] * 100 : v; });
    });
    var right = rowTot.map(function (v) { return type === "row_pct" ? 100 : type === "col_pct" ? v / total * 100 : v; });
    var bottom = colTot.map(function (v) { return type === "col_pct" ? 100 : type === "row_pct" ? v / total * 100 : v; });
    return { value: value, right: right, bottom: bottom, grand: type === "counts" ? total : 100,
             rowTot: rowTot, colTot: colTot, total: total };
  }
  // HTML jak lc_crosstab(lead = FALSE) w R; komórki to przyciski [data-stat-act].
  function crossTable(t, type, target, rowName, colName, short) {
    var m = crossValues(t, type), d = type === "counts" ? 0 : 1;
    var colW = t.cols.map(function (c, j) { return intWidth(m.value.map(function (r) { return r[j]; }).concat([m.bottom[j]]), d); });
    var rightW = intWidth(m.right.concat([m.grand]), d);
    var baseRow = function (i) { return type === "row_pct" && target[0] === i; };
    var baseCol = function (j) { return type === "col_pct" && target[1] === j; };
    var hasShort = !!short && t.cols.some(function (c, j) { return short[j] !== c; });
    var cls = function (list) { var s = list.filter(Boolean).join(" "); return s ? ' class="' + s + '"' : ""; };
    var head2 = t.cols.map(function (c, j) {
      var label = hasShort ? '<span class="lc-l-full">' + esc(c) + '</span><span class="lc-l-short" aria-hidden="true">' + esc(short[j]) + "</span>" : esc(c);
      return '<th scope="col"' + cls(["n", baseCol(j) && "is-base"]) + ">" + label + "</th>";
    }).join("");
    var body = t.rows.map(function (rn, i) {
      var cells = t.cols.map(function (c, j) {
        var on = target[0] === i && target[1] === j;
        return "<td" + cls(["n", on ? "is-target" : (baseRow(i) || baseCol(j)) && "is-base"]) + ' data-label="' + esc(c) + '">' +
          '<button type="button" class="lc-cell-btn" data-stat-act="cell:' + i + ":" + j + '" data-i="' + (i + 1) + '" data-j="' + (j + 1) +
          '" aria-pressed="' + on + '" aria-label="' + esc(rn + ", " + c + ": " + lcFmt(m.value[i][j], d) + (d ? "%" : "")) + '">' +
          num(m.value[i][j], d, colW[j]) + "</button></td>";
      }).join("");
      return "<tr><th scope=\"row\"" + cls([baseRow(i) && "is-base"]) + ">" + esc(rn) + "</th>" + cells +
        "<td" + cls(["n is-total", type === "row_pct" && "is-base-val", baseRow(i) && "is-base"]) + ">" + num(m.right[i], d, rightW) + "</td></tr>";
    }).join("");
    var foot = '<tr><th scope="row">Razem</th>' + t.cols.map(function (c, j) {
      return "<td" + cls(["n", type === "col_pct" && "is-base-val", baseCol(j) && "is-base"]) + ">" + num(m.bottom[j], d, colW[j]) + "</td>";
    }).join("") + '<td class="n is-total">' + num(m.grand, d, rightW) + "</td></tr>";
    var key = hasShort ? '<div class="lc-tbl-key is-short-key">' + t.cols.map(function (c, j) {
      return "<span><b>" + esc(short[j]) + "</b> " + esc(c) + "</span>";
    }).join("") + "</div>" : "";
    return '<div class="lc-tbl-block">' + key +
      '<div class="lc-tbl-scroll" tabindex="0" role="region" aria-label="Tabela krzyżowa"><table class="lc-tbl is-fit"><thead>' +
      '<tr><th scope="col" rowspan="2" class="lc-tbl-corner">' + esc(rowName) + "</th>" +
      '<th scope="colgroup" colspan="' + t.cols.length + '" class="lc-tbl-span">' + esc(colName) + "</th>" +
      '<th scope="col" rowspan="2" class="n is-total">Razem</th></tr><tr>' + head2 + "</tr></thead><tbody>" + body +
      "</tbody><tfoot>" + foot + "</tfoot></table></div></div>";
  }
  // Podpis odczytu zaznaczonej komórki (jak dawniej output$ch2_cross_caption).
  function crossCaption(t, type, target) {
    var m = crossValues(t, type), i = target[0], j = target[1], count = t.n[i][j];
    var q = function (x) { return "„" + esc(x) + "”"; }, rn = t.rows[i], cn = t.cols[j];
    if (type === "row_pct") {
      return "W wierszu " + q(rn) + " <b>" + lcFmt(count / m.rowTot[i] * 100, 1) + "%</b> osób to " + q(cn) +
        " (" + count + " z " + m.rowTot[i] + ").";
    }
    if (type === "col_pct") {
      return "W kolumnie " + q(cn) + " <b>" + lcFmt(count / m.colTot[j] * 100, 1) + "%</b> osób to " + q(rn) +
        " (" + count + " z " + m.colTot[j] + ").";
    }
    return q(rn) + " i " + q(cn) + ": <b>" + count + "</b> osób, czyli " + lcFmt(count / m.total * 100, 1) + "% całej próby.";
  }
  function crossTarget(state, t) {
    var tg = state.target || [0, 0];
    return tg[0] < t.rows.length && tg[1] < t.cols.length ? tg : [0, 0];
  }

  // Słupki obok siebie (geom_bar(position = "dodge")): w każdej grupie słupki
  // niezerowych komórek dzielą szerokość 0.9, jak w ggplot.
  function crossBars(t, rowName, colName, P) {
    var colors = catColors(t.cols.length), data = [], fills = [], top = 0;
    t.n.forEach(function (r, i) {
      var shown = r.map(function (v, j) { return v > 0 ? j : -1; }).filter(function (j) { return j >= 0; });
      var w = 0.9 / Math.max(1, shown.length);
      shown.forEach(function (j, k) {
        data.push([i + 1 - 0.45 + k * w, i + 1 - 0.45 + (k + 1) * w, r[j]]);
        fills.push(colors[j]);
        top = Math.max(top, r[j]);
      });
    });
    return { x: [0.4, t.rows.length + 0.6], y: [0, top * 1.05], xName: rowName, yName: "Liczebność", noXGrid: true,
             xLabels: labelsAt(t.rows, function (i) { return i + 1; }),
             layers: [{ type: "bins", data: data, colors: fills, alpha: 0.85, stroke: P.surface, lineWidth: 1 }],
             aria: "Wykres słupkowy liczebności: " + rowName + " na osi poziomej, słupki kategorii zmiennej " + colName +
                   " obok siebie. " + t.rows.map(function (rn, i) {
                     return rn + ": " + t.cols.map(function (c, j) { return c + " " + t.n[i][j]; }).join(", ");
                   }).join("; ") + "." };
  }
  // Mapa ciepła (geom_tile + geom_text): wartość w każdej komórce, kolor
  // ze skali bordowej od najmniejszej do największej wartości.
  function crossHeat(t, type, rowName, colName, P) {
    var m = crossValues(t, type), pct = type !== "counts", flat = [];
    m.value.forEach(function (r) { r.forEach(function (v) { flat.push(v); }); });
    var lo = Math.min.apply(null, flat), hi = Math.max.apply(null, flat), mid = (lo + hi) / 2;
    var cells = [], borders = [], texts = [];
    m.value.forEach(function (r, i) {
      r.forEach(function (v, j) {
        var box = [j + 0.5, j + 1.5, i + 0.5, i + 1.5];
        cells.push({ r: box, color: seqColor(hi > lo ? (v - lo) / (hi - lo) : 0) });
        borders.push(box);
        texts.push({ type: "text", x: j + 1, y: i + 1, valign: "middle", text: pct ? lcFmt(v, 1) + "%" : lcFmt(v, 0),
                     color: v > mid ? "#ffffff" : "#1c1a17" });
      });
    });
    var what = type === "row_pct" ? "procenty wierszowe" : type === "col_pct" ? "procenty kolumnowe" : "liczebności";
    return { x: [0.5, t.cols.length + 0.5], y: [0.5, t.rows.length + 0.5], xName: colName, yName: rowName,
             noXGrid: true, noYGrid: true,
             xLabels: labelsAt(t.cols, function (j) { return j + 1; }),
             yLabels: labelsAt(t.rows, function (i) { return i + 1; }),
             layers: [{ type: "cells", items: cells },
                      { type: "rects", items: borders, stroke: P.surface, lineWidth: 3, z: 3 }].concat(texts),
             aria: "Mapa ciepła: " + rowName + " w wierszach, " + colName + " w kolumnach, w komórkach " + what +
                   "; im ciemniejsza komórka, tym większa wartość. Największa wartość " +
                   (pct ? lcFmt(hi, 1) + "%" : lcFmt(hi, 0)) + "." };
  }
  // Legenda nad słupkami jak lc_legend() w R: tytuł i próbki kolorów kolumn.
  function crossLegend(title, labels) {
    var sw = catVars(labels.length);
    return '<div class="lc-legend" role="list"><span class="lc-legend-t">' + esc(title) + "</span>" +
      labels.map(function (l, j) {
        return '<span class="lc-legend-i" role="listitem"><i class="lc-th-swatch" aria-hidden="true" style="--lc-sw:' +
          sw[j] + ';"></i>' + esc(l) + "</span>";
      }).join("") + "</div>";
  }

  // ---- Ryc. 3.2: zmienna ilościowa w grupach -----------------------------------
  // Pudełko jak geom_boxplot: kwartyle typu 7, wąsy do skrajnych wartości w 1.5 IQR.
  function boxStats(x) {
    var s = U.sorted(x), q1 = U.quantile7(s, 0.25, true), q3 = U.quantile7(s, 0.75, true), iqr = q3 - q1;
    var lo = q1 - 1.5 * iqr, hi = q3 + 1.5 * iqr, inside = s.filter(function (t) { return t >= lo && t <= hi; });
    return { q1: q1, med: U.quantile7(s, 0.5, true), q3: q3, lo: inside[0], hi: inside[inside.length - 1] };
  }
  // Grupy jak droplevels(factor(g)): poziomy w kolejności czynnika, bez pustych.
  function groupData(numVar, grpVar) {
    var x = U.survey(numVar), g = U.survey(grpVar), levels = levelsOf(grpVar);
    if (!levels) levels = U.sorted(g).filter(function (t, i, a) { return i === 0 || t !== a[i - 1]; });
    var groups = levels.map(function (lv) { return x.filter(function (t, i) { return g[i] === lv; }); });
    var keep = groups.map(function (gx) { return gx.length > 0; });
    levels = levels.filter(function (lv, i) { return keep[i]; });
    groups = groups.filter(function (gx, i) { return keep[i]; });
    return { x: x, g: g.map(function (t) { return levels.indexOf(t); }), levels: levels, groups: groups,
             stats: groups.map(function (gx) {
               return { n: gx.length, mean: U.mean(gx), med: U.median(gx), sd: U.sd(gx) };
             }) };
  }
  function groupDigits(numVar) { return numVar === "srednia_ocen" ? 2 : 1; }
  // Tabela jak lc_table() z kolumnami lc_col(short, desc): skróty w nagłówku
  // i objaśnienia nad tabelą (lcSF.table).
  function groupTable(d, grpName, digits) {
    var cols = [{ label: grpName, row: true }, { label: "n", digits: 0 },
                { label: "Średnia", short: "x̄", desc: "średnia", digits: digits },
                { label: "Mediana", short: "Me", desc: "mediana", digits: digits },
                { label: "Odchylenie standardowe", short: "SD", desc: "odchylenie standardowe", digits: digits }];
    var rows = d.levels.map(function (lv, i) {
      var s = d.stats[i];
      return [lv, s.n, s.mean, s.med, isFinite(s.sd) ? s.sd : "–"];
    });
    return S.table(cols, rows);
  }
  // Podpis: największa różnica median, tylko grupy od 20 osób (małe grupy mają
  // przypadkowe mediany).
  function groupCaption(d, digits) {
    var big = d.levels.map(function (lv, i) { return { name: lv, med: d.stats[i].med, n: d.stats[i].n }; })
      .filter(function (t) { return t.n >= 20; });
    if (!big.length) return "";
    var hi = big[0], lo = big[0];
    big.forEach(function (t) { if (t.med > hi.med) hi = t; if (t.med < lo.med) lo = t; });
    return "Największa różnica median w grupach od 20 osób: " + esc(hi.name) + " − " + esc(lo.name) +
      " = <b>" + lcFmt(hi.med - lo.med, digits) + "</b>.";
  }

  var FIGURES = {
    "d02-xt": {
      init: function (state, root) {
        state.target = [0, 0];
        var r = root && root.querySelector("#ch2_cross_row"), c = root && root.querySelector("#ch2_cross_col");
        state.prev = { row: r ? r.value : "plec", col: c ? c.value : "kierunek" };
      },
      // Ta sama zmienna w wierszach i kolumnach: druga lista dostaje poprzednią
      // wartość pierwszej (zmiana wybranej zmiennej działa jak zamiana).
      prepare: function (state, v, vals, root) {
        var r = vals.ch2_cross_row, k = vals.ch2_cross_col, p = state.prev;
        if (r === k) {
          if (r !== p.row) { k = p.row; setSelect(root, "ch2_cross_col", k); vals.ch2_cross_col = k; }
          else { r = p.col; setSelect(root, "ch2_cross_row", r); vals.ch2_cross_row = r; }
        }
        state.prev = { row: r, col: k };
      },
      act: function (state, act, root) {
        if (act === "swap") {
          var p = state.prev;
          setSelect(root, "ch2_cross_row", p.col);
          setSelect(root, "ch2_cross_col", p.row);
        } else if (act.indexOf("cell:") === 0) {
          var ij = act.split(":");
          state.target = [Number(ij[1]), Number(ij[2])];
          state.focus = act;
        }
      },
      model: function (state, vals) {
        var cfg = state.config || {}, labels = cfg.labels || {}, short = cfg.short || {};
        var rv = vals.ch2_cross_row, cv = vals.ch2_cross_col, t = crossTab(rv, cv);
        return { t: t, rowName: labels[rv] || rv, colName: labels[cv] || cv, target: crossTarget(state, t),
                 short: short[cv] ? t.cols.map(function (c) { return short[cv][c] || c; }) : null,
                 type: vals.ch2_cross_type || "counts", chart: vals.ch2_cross_chart || "bar" };
      },
      draw: function (v, P, k, state, vals) {
        var m = this.model(state, vals);
        return m.chart === "heatmap" ? crossHeat(m.t, m.type, m.rowName, m.colName, P)
                                     : crossBars(m.t, m.rowName, m.colName, P);
      },
      update: function (root, state) {
        var vals = {};
        ["ch2_cross_row", "ch2_cross_col"].forEach(function (id) {
          var sel = root.querySelector("#" + id); vals[id] = sel ? sel.value : null;
        });
        ["ch2_cross_type", "ch2_cross_chart"].forEach(function (id) {
          var on = root.querySelector('input[name="' + id + '"]:checked'); vals[id] = on ? on.value : null;
        });
        var m = this.model(state, vals), html = this.render(m), tbl = root.querySelector('[data-role="table"]');
        if (tbl) tbl.innerHTML = html.table;
        var cap = root.querySelector('[data-role="caption"]'), leg = root.querySelector('[data-role="legend"]');
        if (cap) cap.innerHTML = html.caption;
        if (leg) leg.innerHTML = html.legend;
        // Kliknięta komórka zostaje w fokusie po przebudowaniu tabeli.
        if (state.focus && tbl) {
          var btn = tbl.querySelector('[data-stat-act="' + state.focus + '"]');
          if (btn) btn.focus();
          state.focus = null;
        }
      },
      render: function (m) {
        return { table: crossTable(m.t, m.type, m.target, m.rowName, m.colName, m.short),
                 caption: caption(crossCaption(m.t, m.type, m.target)),
                 legend: m.chart === "heatmap" ? "" : crossLegend(m.colName, m.t.cols) };
      }
    },
    "d02-box": {
      model: function (state, vals) {
        var cfg = state.config || {}, nv = vals.ch3_num, gv = vals.ch3_grp;
        return { d: groupData(nv, gv), numName: (cfg.num || {})[nv] || nv, grpName: (cfg.grp || {})[gv] || gv,
                 digits: groupDigits(nv) };
      },
      // Pudełka bez wartości odstających (outlier.shape = NA) i punkty osób
      // rozsunięte w pionie (geom_jitter(height = 0.18)) ze stałym ziarnem.
      draw: function (v, P, k, state, vals) {
        var m = this.model(state, vals), d = m.d, K = d.levels.length, colors = catColors(K);
        var pos = function (i) { return K - i; };
        var boxes = d.groups.map(function (gx, i) {
          var b = boxStats(gx);
          return { pos: pos(i), width: 0.55, fill: "transparent", q1: b.q1, med: b.med, q3: b.q3, lo: b.lo, hi: b.hi };
        });
        var jit = U.seeded(1), pts = [], cols = [];
        d.x.forEach(function (t, i) {
          if (d.g[i] < 0) return;
          pts.push([t, pos(d.g[i]) + jit.runif(-0.18, 0.18)]);
          cols.push(colors[d.g[i]]);
        });
        var lo = Math.min.apply(null, d.x), hi = Math.max.apply(null, d.x), padX = (hi - lo) * 0.05;
        return { x: [lo - padX, hi + padX], y: [0.4, K + 0.6], xName: m.numName, yName: m.grpName, noYGrid: true,
                 yLabels: labelsAt(d.levels, pos),
                 layers: [{ type: "box", horizontal: true, stroke: P.secondary, boxes: boxes },
                          { type: "points", data: pts, colors: cols, alpha: 0.6, size: 5, z: 5 }],
                 aria: "Wykresy pudełkowe zmiennej " + m.numName + " w grupach (" + m.grpName + ") z punktami pojedynczych osób: " +
                   d.levels.map(function (lv, i) {
                     return lv + " (n = " + d.stats[i].n + ", mediana " + lcFmt(d.stats[i].med, m.digits) + ")";
                   }).join(", ") + "." };
      },
      update: function (root, state) {
        var vals = {};
        ["ch3_num", "ch3_grp"].forEach(function (id) {
          var sel = root.querySelector("#" + id); vals[id] = sel ? sel.value : null;
        });
        var m = this.model(state, vals);
        var tbl = root.querySelector('[data-role="table"]'), cap = root.querySelector('[data-role="caption"]');
        if (tbl) tbl.innerHTML = groupTable(m.d, m.grpName, m.digits);
        if (cap) { var c = groupCaption(m.d, m.digits); cap.innerHTML = c ? caption(c) : ""; }
      }
    }
  };

  S.register(FIGURES);
  window.lc02Survey = { crossTab: crossTab, crossValues: crossValues, crossTable: crossTable, crossCaption: crossCaption,
                        groupData: groupData, groupTable: groupTable, groupCaption: groupCaption, boxStats: boxStats,
                        wrap: wrap, num: num, figures: FIGURES };
})();
