// Wspólny silnik rycin rysowanych w przeglądarce (ECharts SVG). Rycina to opis
// rysunku liczony w JS z kontrolek strony — działa także offline, bez serwera R.
// Wykład rejestruje ryciny: window.lcSF.register({ nazwa: figura }).
//
// Kontener (R: lc_sf_figure() w lc_stat_figures.R):
//   .lc-sf[data-fig][data-config]
//     .lc-sf-ctl            — kontrolka (natywny <select>, radia lc_segmented, pole
//                             wyboru, grupa pól wyboru → lista, suwak lc_sf_slider)
//     .lc-ec[data-chart=k]  — jeden lub więcej wykresów
//     .lc-sf-legend         — legenda (opcjonalnie)
//     [data-when="wartość"] — opisy policzone w R dla każdej opcji; JS pokazuje właściwy
//     [data-stat-act]       — przyciski obsługiwane przez figure.act
//
// Figura: funkcja draw(v, P, k) albo obiekt {init(state, root), prepare(state, v,
// vals, root), act(state, akcja, root), draw(v, P, k, state, vals, krok), update(root,
// state, v), text, table, noteKey}. state.config to JSON z data-config.
// draw zwraca opis: x, y (zakresy), xName, yName, legend, aria i warstwy:
//   bins {data:[[l,r,y]], color, alpha}     — słupki histogramu / kategorii
//   band {lo, hi, color, alpha}              — pionowy pas na całą wysokość
//   vline {x, color, width, dash, from, to}  — pionowa linia
//   line {data:[[x,y]], color, width, dash, fill, fillAlpha}  — krzywa (z polem)
//   segment, arrow(s), point(s), rect(s), segs, polygon, box, rug, cells
//   text {x, y, text, color, align, valign, bold, mono, size, halo}  (y = "top": przy górnej krawędzi)
// Inne klucze opisu: xBig, xTicks, xLabels, yLabels, xLabelEvery, xInt, noXGrid, noYGrid, hideY, alignLeft.
(function () {
  "use strict";
  var C = window.lcCharts;
  if (!C || !C.chart) return;
  var FIGURES = {};

  // lc_fmt z R: stała liczba cyfr, bez końcowych zer.
  function lcFmt(v, d) {
    var s = C.fmt(v, d);
    return s.indexOf(".") < 0 ? s : s.replace(/0+$/, "").replace(/\.$/, "");
  }
  function status(html) { return '<div class="lc-status" role="status" aria-live="polite">' + html + "</div>"; }
  function setValue(root, key, text) {
    root.querySelectorAll('[data-v="' + key + '"]').forEach(function (n) { n.textContent = text; });
  }
  // ---- Tabela jak lc_table() w R -----------------------------------------------
  // cols: [{label, digits, row}] (row: kolumna nagłówków wierszy); rows/foot: tablice
  // wartości. Liczby wyrównane niewidocznymi zerami (lc-pad), jak w R.
  function esc(t) { return String(t).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"); }
  function lcTable(cols, rows, foot) {
    var shape = cols.map(function (c, j) {
      if (c.row || c.digits == null) return null;
      var all = rows.concat(foot ? [foot] : []).map(function (r) { return r[j]; })
        .filter(function (v) { return typeof v === "number" && isFinite(v); });
      var parts = all.map(function (v) { var t = lcFmt(v, c.digits).split("."); return { i: t[0].length, d: (t[1] || "").length }; });
      return { i: Math.max.apply(null, parts.map(function (p) { return p.i; }).concat([0])),
               d: c.digits };  // miejsca po przecinku dopełniane do cyfr kolumny
    });
    function pad(t) { return '<span class="lc-pad" aria-hidden="true">' + t + "</span>"; }
    function zeros(n) { return new Array(n + 1).join("0"); }
    function cell(v, c, j, tag) {
      if (c.row) return '<th scope="row">' + esc(v) + "</th>";
      var body = "";
      if (typeof v === "number" && isFinite(v)) {
        var t = lcFmt(v, c.digits), p = t.split("."), sh = shape[j];
        var left = sh.i - p[0].length, right = sh.d - (p[1] || "").length;
        body = (left > 0 ? pad(zeros(left)) : "") + esc(t) + (right > 0 ? pad((p[1] ? "" : ".") + zeros(right)) : "");
      } else body = esc(v == null ? "" : v);
      return "<" + tag + ' class="n" data-label="' + esc(c.label) + '">' + body + "</" + tag + ">";
    }
    // short/desc jak lc_col() w R: skrót w nagłówku (<abbr>), objaśnienia nad tabelą.
    var head = cols.map(function (c) {
      var lab = c.short ? '<abbr title="' + esc(c.label) + '">' + esc(c.short) + "</abbr>" : esc(c.label);
      return '<th scope="col"' + (c.row ? "" : ' class="n"') + ">" + lab + "</th>";
    }).join("");
    var keyed = cols.filter(function (c) { return c.short && c.desc; });
    var key = keyed.length ? '<div class="lc-tbl-key">' + keyed.map(function (c) {
      return "<span><b>" + esc(c.short) + "</b> " + esc(c.desc) + "</span>"; }).join("") + "</div>" : "";
    var body = rows.map(function (r) { return "<tr>" + cols.map(function (c, j) { return cell(r[j], c, j, "td"); }).join("") + "</tr>"; }).join("");
    var tail = foot ? "<tfoot><tr>" + cols.map(function (c, j) { return cell(foot[j], c, j, "td"); }).join("") + "</tr></tfoot>" : "";
    return '<div class="lc-tbl-block">' + key + '<table class="lc-tbl is-fit"><thead><tr>' + head + "</tr></thead><tbody>" +
           body + "</tbody>" + tail + "</table></div>";
  }
  // round() z R: zaokrąglenie do d miejsc, wypisane bez końcowych zer.
  function rround(v, d) { return lcFmt(v, d); }

  // ---- Rysunek opisu --------------------------------------------------------
  function rectSeries(L, horizontal) {
    return { type: "custom", silent: true, z: 2, clip: true, data: L.data.map(function (r, i) { return r.concat([i]); }),
      encode: horizontal ? { y: [0, 1], x: 2 } : { x: [0, 1], y: 2 },
      renderItem: function (params, api) {
        var i = api.value(3), color = L.colors ? L.colors[i] : L.color, a, b, shape;
        if (horizontal) {
          a = api.coord([0, api.value(1)]); b = api.coord([api.value(2), api.value(0)]);
          shape = { x: Math.min(a[0], b[0]), y: Math.min(a[1], b[1]), width: Math.abs(b[0] - a[0]), height: Math.abs(b[1] - a[1]) };
        } else {
          a = api.coord([api.value(0), api.value(2)]); b = api.coord([api.value(1), 0]);
          shape = { x: a[0] + 0.5, y: a[1], width: Math.max(0, b[0] - a[0] - 1), height: b[1] - a[1] };
        }
        // stroke: obrys słupka (krawędź jak colour w geom_histogram), nieprzezroczysty.
        if (!L.stroke) return { type: "rect", x: 0, y: 0, shape: shape, style: { fill: color, opacity: L.alpha == null ? 1 : L.alpha } };
        return { type: "rect", x: 0, y: 0, shape: shape,
                 style: { fill: L.alpha == null ? color : C.alpha(color, L.alpha), stroke: L.stroke, lineWidth: L.lineWidth || 0.8 } };
      } };
  }

  // Histogram jak geom_histogram(bins = k): szerokość = rozstęp / (k − 1), przedziały
  // domknięte z prawej. Zwraca [{l, r, n}]; wysokości liczy rycina (liczebność, gęstość).
  // Histogram jak geom_histogram(binwidth = w, boundary = b): granice w b + k·w
  // (domyślnie b = w/2, jak w ggplot), przedziały domknięte z prawej. Zwraca [{l, r, n}].
  function binsWidth(values, w, boundary) {
    var lo = Infinity, hi = -Infinity, i;
    for (i = 0; i < values.length; i++) { if (values[i] < lo) lo = values[i]; if (values[i] > hi) hi = values[i]; }
    var b0 = boundary == null ? w / 2 : boundary;
    var origin = b0 + Math.floor((lo - b0) / w) * w;
    var k = Math.max(1, Math.ceil((hi - origin) / w - 1e-9)), out = [];
    for (i = 0; i < k; i++) out.push({ l: origin + i * w, r: origin + (i + 1) * w, n: 0 });
    values.forEach(function (v) {
      var j = Math.max(0, Math.min(k - 1, Math.ceil((v - origin) / w - 1e-9) - 1));
      out[j].n++;
    });
    return out;
  }

  function binsGgplot(values, k) {
    var lo = Infinity, hi = -Infinity, i;
    for (i = 0; i < values.length; i++) { if (values[i] < lo) lo = values[i]; if (values[i] > hi) hi = values[i]; }
    if (!(hi > lo)) return [{ l: lo - 0.5, r: lo + 0.5, n: values.length }];
    var w = (hi - lo) / (k - 1), boundary = w / 2;
    var origin = boundary + Math.floor((lo - boundary) / w) * w, out = [];
    for (var e = origin; e <= hi + (1 - 1e-8) * w; e += w) out.push({ l: e, r: e + w, n: 0 });
    out.pop();
    values.forEach(function (v) {
      var j = Math.max(0, Math.min(out.length - 1, Math.ceil((v - origin) / w - 1e-9) - 1));
      out[j].n++;
    });
    return out;
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
        series.push({ type: "scatter", silent: true, z: L.z || 5, symbol: L.symbol || "circle", symbolSize: L.size || 6, data: L.data,
          itemStyle: { color: L.colors ? function (p) { return L.colors[p.dataIndex]; } : L.color,
                       opacity: L.alpha == null ? 1 : L.alpha } });
      } else if (L.type === "rect") {
        // Prostokąt w jednostkach danych (x0, x1, y0, y1); bez obramowania, gdy stroke pusty.
        series.push({ type: "custom", silent: true, z: L.z || 2, data: [[0]],
          renderItem: function (params, api) {
            var a = api.coord([L.x0, L.y1]), b = api.coord([L.x1, L.y0]);
            return { type: "rect", x: 0, y: 0,
                     shape: { x: Math.min(a[0], b[0]), y: Math.min(a[1], b[1]), width: Math.abs(b[0] - a[0]), height: Math.abs(b[1] - a[1]) },
                     style: { fill: L.fill ? C.alpha(L.fill, L.alpha == null ? 1 : L.alpha) : "none",
                              stroke: L.stroke || null, lineWidth: L.lineWidth || 1 } };
          } });
      } else if (L.type === "rects") {
        // Wiele prostokątów jednego stylu: items [[x0, x1, y0, y1], ...].
        series.push({ type: "custom", silent: true, z: L.z || 2, data: L.items.map(function (r, i) { return [i]; }),
          renderItem: function (params, api) {
            var r = L.items[params.dataIndex], a = api.coord([r[0], r[3]]), b = api.coord([r[1], r[2]]);
            return { type: "rect", x: 0, y: 0,
                     shape: { x: Math.min(a[0], b[0]), y: Math.min(a[1], b[1]), width: Math.abs(b[0] - a[0]), height: Math.abs(b[1] - a[1]) },
                     style: { fill: L.fill ? C.alpha(L.fill, L.alpha == null ? 1 : L.alpha) : "none",
                              stroke: L.stroke || null, lineWidth: L.lineWidth || 1 } };
          } });
      } else if (L.type === "cells") {
        // Komórki mapy: items [{r: [x0, x1, y0, y1], color}] — kolor już policzony.
        series.push({ type: "custom", silent: true, z: 1, clip: true, data: L.items.map(function (r, i) { return [i]; }),
          renderItem: function (params, api) {
            var it = L.items[params.dataIndex], a = api.coord([it.r[0], it.r[3]]), b = api.coord([it.r[1], it.r[2]]);
            return { type: "rect", x: 0, y: 0,
                     shape: { x: Math.floor(Math.min(a[0], b[0])), y: Math.floor(Math.min(a[1], b[1])),
                              width: Math.ceil(Math.abs(b[0] - a[0])) + 1, height: Math.ceil(Math.abs(b[1] - a[1])) + 1 },
                     style: { fill: it.color } };
          } });
      } else if (L.type === "arrows") {
        series.push({ type: "lines", coordinateSystem: "cartesian2d", silent: true, z: 4,
          symbol: L.both ? ["arrow", "arrow"] : ["none", "arrow"], symbolSize: Math.round(fs * (L.head || 0.7)),
          data: L.items.map(function (it) { return { coords: [it.from, it.to] }; }),
          lineStyle: { color: L.color, width: L.width || 2, opacity: 1 } });
      } else if (L.type === "segs") {
        // Odcinki w różnych kolorach: items [{from, to, color}].
        series.push({ type: "custom", silent: true, z: L.z || 3, data: L.items.map(function (r, i) { return [i]; }),
          renderItem: function (params, api) {
            var it = L.items[params.dataIndex], a = api.coord(it.from), b = api.coord(it.to);
            return { type: "line", x: 0, y: 0, shape: { x1: a[0], y1: a[1], x2: b[0], y2: b[1] },
                     style: { stroke: it.color || L.color, lineWidth: L.width || 1.5, opacity: L.alpha == null ? 1 : L.alpha,
                              lineDash: L.dash || null } };
          } });
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
          // Napis obok punktu (middle + left/right) odsuwamy przez distance, nie offset:
          // offset przesuwa sam tekst, a tło halo zostawało w miejscu.
          distance: top ? 0 : (T.valign === "middle" && (T.align === "left" || T.align === "right")) ? 8 : 4,
          offset: top ? [T.align === "left" ? 6 : T.align === "right" ? -6 : 0, 6] :
                  T.valign === "middle" ? [0, 0] :
                  (T.align === "left" ? [4, 0] : T.align === "right" ? [-4, 0] : [0, 0]),
          fontSize: size, fontWeight: T.bold ? 600 : 400, fontStyle: T.italic ? "italic" : "normal",
          fontFamily: T.mono ? P.mono : sans,
          // halo: tło pod napisem, gdy napis leży na linii lub polu.
          backgroundColor: T.halo ? C.alpha(P.surface, 0.85) : undefined,
          padding: T.halo ? [1, 4] : 0, borderRadius: T.halo ? 3 : 0 } });
    });
    // Warstwy o tym samym z: późniejsza na wierzchu (kolejność warstw opisu, napisy na końcu). Bez
    // rysowania progresywnego, które dla dużych serii punktów psuło kolejność.
    series.forEach(function (sr, i) {
      sr.z = (sr.z == null ? 2 : sr.z) + i / 1000;
      if (sr.type === "scatter") sr.progressive = 0;
    });
    var xa = C.ecAxis(P, fs, { type: "value", min: spec.x[0], max: spec.x[1],
      name: spec.xName ? C.ecName(spec.xName, Math.max(250, W - 40), fs) : "", nameGap: Math.round(fs * 2.2) });
    xa.axisLabel.formatter = function (v) { return lcFmt(v, 6); };
    // xBig: liczby z odstępem tysięcy (np. 12 000 zł).
    if (spec.xBig) xa.axisLabel.formatter = function (v) { return C.fmt(v, 0, true); };
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
    if (spec.xLabelEvery) {
      // Etykiety na okrągłych wielokrotnościach (np. co 20 cm), także gdy kraniec osi nie jest okrągły.
      var step = spec.xLabelEvery, first = Math.ceil(spec.x[0] / step) * step, at = [];
      for (var t = first; t <= spec.x[1] + 1e-9; t += step) at.push(t);
      xa.axisLabel.customValues = at; xa.axisTick.customValues = at;
      xa.axisLabel.showMinLabel = true; xa.axisLabel.showMaxLabel = true;
    }
    if (spec.noXGrid) xa.splitLine = { show: false };
    if (spec.xInt) xa.minInterval = 1;  // tylko całkowite podziałki (zmienne dyskretne)
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
      // Etykiety kategorii: wszystkie, także przy krańcach osi (oś nie musi startować od 0).
      ya.axisLabel.showMaxLabel = true; ya.axisLabel.showMinLabel = true;
      ya.axisLabel.hideOverlap = false; ya.axisLabel.fontFamily = sans;
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
    if (items && !Array.isArray(items)) {
      // Skala barw: tytuł, „mało” — gradient — „dużo”.
      var at = items.at || items.stops.map(function (c, i) { return i / (items.stops.length - 1); });
      var css = items.stops.map(function (c, i) { return c + " " + Math.round(at[i] * 100) + "%"; }).join(", ");
      var wrap = document.createElement("span"), bar = document.createElement("span");
      wrap.className = "lc-sf-legend-item lc-sf-colorbar";
      bar.className = "lc-sf-colorbar-bar"; bar.style.setProperty("background", "linear-gradient(to right, " + css + ")");
      wrap.appendChild(document.createTextNode(items.title + ": " + items.from));
      wrap.appendChild(bar); wrap.appendChild(document.createTextNode(items.to));
      legend.appendChild(wrap); legend.hidden = false;
      return;
    }
    (items || []).forEach(function (it) {
      var item = document.createElement("span"), sw = document.createElement("span");
      item.className = "lc-sf-legend-item";
      sw.className = it.box ? "lc-sf-box-sw" : "lc-sf-line-sw" + (it.dash ? " is-dashed" : "");
      sw.style.setProperty("--lc-sw", it.color);
      item.appendChild(sw); item.appendChild(document.createTextNode(it.text));
      legend.appendChild(item);
    });
    legend.hidden = !(items && items.length);
  }

  // Figura: funkcja draw(v, P, k) albo obiekt {init, prepare, act, draw, update}.
  function mount(root) {
    var def = FIGURES[root.dataset.fig], els = root.querySelectorAll(".lc-ec");
    if (!def || !els.length) return false;
    var fig = typeof def === "function" ? { draw: def } : def, state = {};
    var ctls = root.querySelectorAll(".lc-sf-ctl"), ctl = ctls[0] || null;
    var legend = root.querySelector(".lc-sf-legend"), charts = [];
    var cfg = root.getAttribute("data-config");
    state.config = cfg ? JSON.parse(cfg) : null;
    if (fig.init) fig.init(state, root);
    function value(ctl) {
      if (!ctl) return null;
      var box = ctl.matches && ctl.matches('input[type="checkbox"]') ? ctl : ctl.querySelector('input[type="checkbox"]');
      // Grupa pól wyboru (checkboxGroupInput): lista zaznaczonych wartości.
      var boxes = ctl.querySelectorAll ? ctl.querySelectorAll('input[type="checkbox"]') : [];
      if (boxes.length > 1) {
        var on = [];
        boxes.forEach(function (b) { if (b.checked) on.push(b.getAttribute("value")); });
        return on;
      }
      if (box) return box.checked ? "on" : "off";
      var sel = ctl.matches && ctl.matches("select") ? ctl : ctl.querySelector("select");
      if (sel) return sel.value;
      var range = ctl.querySelector('input[type="range"]');
      if (range) return range.value;
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
    // Rycina krokowa (lc_step_widget): krok w atrybucie data-lc-step, tekst kroku
    // w #<id>_text, tabela w [data-role="table"]. Wykres zwracający null jest ukrywany.
    var stepper = root.querySelector(".lc-stepper");
    function step() { return stepper ? Number(stepper.getAttribute("data-lc-step")) || 1 : null; }
    function typeset(el) {
      var MJ = window.MathJax;
      if (MJ && MJ.Hub && MJ.Hub.Queue) MJ.Hub.Queue(["Typeset", MJ.Hub, el]);
      else if (MJ && MJ.typesetPromise) MJ.typesetPromise([el]);
    }
    function draw() {
      var vals = values();
      if (fig.prepare) fig.prepare(state, value(ctl), vals, root);
      vals = values();
      var v = value(ctl), P = C.palette(), fs = C.axisFontSize(root), st = step();
      charts.forEach(function (chart, k) {
        if (chart.isDisposed && chart.isDisposed()) return;
        var spec = fig.draw(v, P, k, state, vals, st), box = els[k].closest(".lc-sf-chart") || els[k].parentNode;
        if (box) box.hidden = !spec;
        if (!spec) return;
        chart.setOption(option(spec, P, fs, els[k].clientWidth || 640), true);
        els[k].setAttribute("aria-label", spec.aria);
        if (legend && (k === 0 || spec.legend)) legendItems(legend, spec.legend);
      });
      if (fig.update) fig.update(root, state, v);
      if (stepper && fig.text) {
        var out = root.querySelector("#" + stepper.getAttribute("id") + "_text");
        if (out) { out.innerHTML = fig.text(state, v, vals, st); typeset(out); }
      }
      if (fig.table) {
        var tbl = root.querySelector('[data-role="table"]');
        if (tbl) tbl.innerHTML = fig.table(state, v, vals, st);
      }
      var key = fig.noteKey ? fig.noteKey(vals, state) : v;
      root.querySelectorAll("[data-when]").forEach(function (n) { n.hidden = n.getAttribute("data-when") !== key; });
    }
    for (var k = 0; k < els.length; k++) {
      charts.push(C.chart(els[k], { animation: false }, { onResize: draw }));
    }
    ctls.forEach(function (c) { C.listen(c, "change", draw); C.listen(c, "input", draw); });
    if (stepper) {
      var watch = new MutationObserver(draw);
      watch.observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
      C.onDispose(root, function () { watch.disconnect(); });
    }
    if (fig.act) C.listen(root, "click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-stat-act]");
      if (!btn) return;
      fig.act(state, btn.getAttribute("data-stat-act"), root);
      draw();
    });
    // Odczyt suwaka lc_sf_slider (output[data-value=id]) nadąża za wartością.
    root.querySelectorAll("output[data-value]").forEach(function (out) {
      var input = root.querySelector("#" + out.getAttribute("data-value"));
      if (input) C.listen(input, "input", function () { out.textContent = input.value; });
    });
    C.zoomControl(root);
    C.onTheme(root, draw);
    draw();
  }

  // Wykład dopisuje ryciny do rejestru; kontenery, które czekały na swoją
  // rycinę, inicjują się przy kolejnym przeglądzie onReady.
  function register(figs) {
    Object.keys(figs).forEach(function (k) { FIGURES[k] = figs[k]; });
    C.onReady(".lc-sf", mount);
  }

  window.lcSF = { register: register, figures: FIGURES, option: option, fmt: lcFmt, status: status,
                  setValue: setValue, esc: esc, table: lcTable, legendItems: legendItems,
                  binsGgplot: binsGgplot, binsWidth: binsWidth };
})();
