// Ryciny wykładu 10 „Projekt badawczy” liczone w przeglądarce (dawniej wykresy ggplot
// i wyjścia serwera). Silnik: window.lcSF (shared/lc_stat_figures.js), regresja:
// window.lcStats (shared/lc_stats.js). Dane i modele na stałym zbiorze TeachingRatings
// liczy R przy budowaniu (modules/figures.R) i podaje w data-config.
//
//   s10_plot     — wykres tropu: rozrzut z prostą i pasem ufności albo pudełka z punktami
//   s10_beta     — Ryc. 7.1: współczynnik przy beauty w serii modeli
//   s10_custom   — Ryc. 7.2: model własny z wybranymi kontrolami (regresja w przeglądarce)
//   s10_data     — Ryc. 1.2: podgląd danych (sortowanie, okno wierszy, zakres kolumn; bez wykresu)
//   s10_preview  — podgląd konspektu z pól tekstowych (bez wykresu)
//
// Funkcje liczące są w window.lcS10 (testy: tests/js/stat10.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var T = root.lcStats;
  if (!T) return;

  // Model eval ~ beauty + wybrane kontrole; współczynniki bez wyrazu wolnego, z etykietami.
  function customModel(cfg, vars) {
    var columns = [cfg.beauty], labels = [cfg.beauty_label];
    vars.forEach(function (v) {
      (cfg.vars[v] || []).forEach(function (c) { columns.push(c.values); labels.push(c.label); });
    });
    var fit = T.lm(columns, cfg.y);
    return { fit: fit, terms: fit.coef.slice(1).map(function (c, i) {
      return { label: labels[i], estimate: c.estimate, se: c.se, p: c.p, lo: c.lo, hi: c.hi };
    }) };
  }
  // Zapis p jak w tabelach wykładu: „< 0.001” albo trzy miejsca po kropce.
  function pLabel(p) { return isNaN(p) ? "—" : p < 0.001 ? "< 0.001" : p.toFixed(3); }
  // Kolejność wierszy po sortowaniu malejącym (stabilnym) i okno dziesięciu od wiersza `from`.
  function dataWindow(values, n, from) {
    var order = [];
    for (var i = 0; i < n; i++) order.push(i);
    if (values) order.sort(function (a, b) { return values[b] - values[a]; });
    var start = Math.min(Math.max(1, Math.floor(from) || 1), n);
    return { order: order, first: start - 1, last: Math.min(n, start + 9) - 1 };
  }
  root.lcS10 = { customModel: customModel, pLabel: pLabel, dataWindow: dataWindow };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S) return;

  var figures = {};
  function colour(P, key) { return key === "wrzos" ? C.token("--upwr-cat-wrzos", "#a07894") : P[key]; }
  function pad(lo, hi, share) { var d = (hi - lo) * (share || 0.05) || 1; return [lo - d, hi + d]; }
  function range(values) { return [Math.min.apply(null, values), Math.max.apply(null, values)]; }

  figures.s10_plot = {
    draw: function (v, P, k, state) {
      var cfg = state.config, ys = cfg.points.map(function (p) { return p[1]; }), ry = range(ys);
      if (cfg.kind === "scatter") {
        var rx = range(cfg.points.map(function (p) { return p[0]; }));
        var band = cfg.upr.concat(cfg.lwr.slice().reverse());
        return { x: pad(rx[0], rx[1]), y: pad(ry[0], ry[1]),
          layers: [{ type: "points", data: cfg.points, color: P.ref, size: 7, alpha: 0.45 },
                   { type: "polygon", points: band, fill: C.alpha(P.szalwia, 0.12) },
                   { type: "line", data: cfg.fit, color: P.szalwia, width: 2.4 }],
          xName: cfg.x_label, yName: cfg.y_label, aria: "Wykres rozrzutu z prostą regresji i pasem ufności." };
      }
      var labels = {}, out = [];
      var boxes = cfg.boxes.map(function (b, i) {
        labels[i + 1] = cfg.levels[i];
        b.out.forEach(function (y) { out.push([i + 1, y]); });
        return { pos: i + 1, width: 0.75, q1: b.q1, med: b.med, q3: b.q3, lo: b.lo, hi: b.hi, fill: C.alpha(colour(P, cfg.fills[i]), 0.65) };
      });
      return { x: [0.4, cfg.levels.length + 0.6], y: pad(ry[0], ry[1]), xLabels: labels,
        layers: [{ type: "box", boxes: boxes, stroke: P.accent },
                 { type: "points", data: out, color: P.accent, size: 6, alpha: 0.25 },
                 { type: "points", data: cfg.points, color: P.accent, size: 4, alpha: 0.16 }],
        xName: cfg.x_label, yName: cfg.y_label, aria: "Wykresy pudełkowe w grupach z naniesionymi obserwacjami." };
    }
  };

  figures.s10_beta = {
    draw: function (v, P, k, state) {
      var cfg = state.config, labels = {}, layers = [], top = Math.max.apply(null, cfg.beta.concat([0])), low = Math.min.apply(null, cfg.beta.concat([0]));
      cfg.beta.forEach(function (b, i) {
        labels[i + 1] = cfg.models[i];
        layers.push({ type: "bins", data: [[i + 0.7, i + 1.3, b]], color: cfg.significant[i] ? P.szalwia : P.ref, stroke: P.surface });
      });
      layers.push({ type: "line", data: [[0.4, 0], [cfg.beta.length + 0.6, 0]], color: P.ref, width: 1.4 });
      return { x: [0.4, cfg.beta.length + 0.6], y: [low * 1.08, top * 1.08], layers: layers, xLabels: labels, xRotate: 20,
        xName: "", yName: "Współczynnik przy beauty", aria: "Współczynnik przy atrakcyjności w czterech kolejnych modelach kontrolnych." };
    }
  };

  function chosen(el) {
    return Array.prototype.map.call(el.querySelectorAll('input[name="ch7_vars"]:checked'), function (b) { return b.value; });
  }
  figures.s10_custom = {
    prepare: function (state, v, vals, el) { state.model = customModel(state.config, chosen(el)); },
    draw: function (v, P, k, state) {
      var terms = state.model.terms, n = terms.length, labels = {}, bars = [], pts = [], lo = 0, hi = 0;
      terms.forEach(function (t, i) {
        var y = n - i;                               // pierwszy predyktor u góry
        labels[y] = t.label; lo = Math.min(lo, t.lo); hi = Math.max(hi, t.hi);
        bars.push({ from: [t.lo, y], to: [t.hi, y] }, { from: [t.lo, y - 0.1], to: [t.lo, y + 0.1] }, { from: [t.hi, y - 0.1], to: [t.hi, y + 0.1] });
        pts.push([t.estimate, y]);
      });
      return { x: pad(lo, hi), y: [0.4, n + 0.6], yLabels: labels, noYGrid: true,
        layers: [{ type: "vline", x: 0, color: P.ref, width: 1.4, dash: [6, 5] },
                 { type: "segs", items: bars, color: P.ref, width: 1.6 },
                 { type: "points", data: pts, color: colour(P, "wrzos"), size: 10 }],
        xName: "Współczynnik z 95% CI", yName: "", aria: "Współczynniki modelu własnego z 95% przedziałami ufności." };
    },
    update: function (el, state) {
      var m = state.model, beauty = m.terms[0];
      S.setValue(el, "custom_adj", S.fmt(m.fit.adjR2, 3)); S.setValue(el, "custom_aic", S.fmt(m.fit.aic, 0));
      S.setValue(el, "custom_beta", S.fmt(beauty.estimate, 3)); S.setValue(el, "custom_p", pLabel(beauty.p));
      var box = el.querySelector('[data-role="coefs"]');
      if (!box) return;
      box.innerHTML = S.table([{ label: "Predyktor", row: true }, { label: "β", digits: 3 }, { label: "SE", digits: 3 }, { label: "p", digits: 3 }],
        m.terms.map(function (t) { return [t.label, t.estimate, t.se, t.p < 0.001 ? "< 0.001" : t.p]; }));
      // Istotne predyktory (p < 0.05) wyróżnione całym wierszem.
      box.querySelectorAll("tbody tr").forEach(function (tr, i) { tr.classList.toggle("is-best", m.terms[i].p < 0.05); });
    }
  };

  figures.s10_data = {
    update: function (el, state) {
      var table = el.querySelector("table"), body = table && table.querySelector("tbody");
      if (!body) return;
      if (!state.rows) state.rows = Array.prototype.slice.call(body.children);
      var sort = el.querySelector("#ch1_sort_var").value, view = el.querySelector("#ch1_col_view").value;
      var w = dataWindow(sort === "none" ? null : state.config.sort[sort], state.rows.length, Number(el.querySelector("#ch1_row_from").value));
      w.order.forEach(function (idx, pos) {
        var tr = state.rows[idx];
        tr.hidden = pos < w.first || pos > w.last;
        body.appendChild(tr);
      });
      el.classList.toggle("is-key", view !== "all");
    }
  };

  var FIELDS = [["ch4_goal", "Cel"], ["ch4_variables", "Zmienne, dane i pomiar"], ["ch4_hypotheses", "Tropy"], ["ch4_plan", "Plan interpretacji"]];
  figures.s10_preview = {
    update: function (el) {
      var values = FIELDS.map(function (f) { var a = el.querySelector("#" + f[0]); return a ? a.value.trim() : ""; });
      var empty = el.querySelector('[data-role="empty"]'), list = el.querySelector('[data-role="list"]');
      var any = values.some(function (v) { return v.length > 0; });
      empty.hidden = any; list.hidden = !any;
      list.textContent = "";
      FIELDS.forEach(function (f, i) {
        var li = document.createElement("li"), b = document.createElement("strong");
        b.textContent = f[1] + ":";
        li.appendChild(b); li.appendChild(document.createTextNode(" "));
        if (values[i]) li.appendChild(document.createTextNode(values[i]));
        else { var s = document.createElement("span"); s.className = "tropy-muted"; s.textContent = "do uzupełnienia"; li.appendChild(s); }
        list.appendChild(li);
      });
    }
  };

  S.register(figures);
})();
