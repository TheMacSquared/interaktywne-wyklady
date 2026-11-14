// Detektor wykładu 03 „Alarm i prawda” w przeglądarce, bez serwera R.
//
// Trzy suwaki tablicy 2×2 (#a3_prev, #a3_sens, #a3_fpr) są wspólne dla kilku
// paneli w różnych rozdziałach, jak dawniej wejścia Shiny:
//   [data-a3="table"] — tablica 2×2 dla 10 000 zmian (jak lc_crosstab w R)
//   [data-a3="tree"]  — drzewo Bayesa (jak alarm_tree_svg w R); kroki i wzory
//                       dokłada modules/drzewo.js, który sam reaguje na podmianę
//   [data-a3v="tp|fp|post"] — odczyty nad drzewem
//   rycina lcSF a3_double — dwa alarmy z kopiowaniem (suwak #a3_dependence)
// Pozostałe ryciny lcSF: a3_cost (próg reakcji c/L), a3_sp (Serpuchów, łańcuch szans).
// Funkcje liczące i rysujące są w window.lcA3 (testy: tests/js/a3_detektor.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;

  // --- Liczby jak w R --------------------------------------------------------

  // round() z R: połówki do parzystej
  function rround(x) {
    var r = Math.round(x);
    if (Math.abs(x % 1) === 0.5) r = 2 * Math.round(x / 2);
    return r;
  }
  // format(n, big.mark = " ")
  function big(n) { return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, " "); }
  // risk_fmt_p(x, digits)
  function fmtP(x, digits) {
    if (x == null || !isFinite(x)) return "—";
    var d = digits || 3;
    if (x > 0 && x < 0.01) d = Math.max(d, Math.min(12, Math.ceil(-Math.log10(x)) + 2));
    return x.toFixed(d);
  }
  // format(signif(x, 2), big.mark = " ", scientific = FALSE)
  function signif2(x) {
    var v = Number(x.toPrecision(2));
    if (v >= 1) return big(String(v));
    return v >= 1e-6 ? String(v) : v.toFixed(Math.max(0, 1 - Math.floor(Math.log10(v))));
  }
  function esc(s) { return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;"); }

  // --- risk_math.R ----------------------------------------------------------------

  function bayes(prev, sens, fpr) {
    var den = sens * prev + fpr * (1 - prev);
    return den === 0 ? NaN : sens * prev / den;
  }
  function twoAlarm(prev, sens, fpr, dep) {
    function joint(p) { return dep * p + (1 - dep) * p * p; }
    return bayes(prev, joint(sens), joint(fpr));
  }
  function detectorCounts(pop, prev, sens, fpr) {
    var events = rround(pop * prev), non = pop - events;
    var tp = rround(events * sens), fp = rround(non * fpr);
    return { alarm: [tp, fp], noAlarm: [events - tp, non - fp], total: [events, non] };
  }
  // a3_double_counts(): P(oba | stan) = d·p + (1 − d)·p²
  function doubleCounts(prev, sens, fpr, dep, pop) {
    pop = pop || 10000;
    function joint(p) { return dep * p + (1 - dep) * p * p; }
    return {
      count: [pop * prev * joint(sens), pop * (1 - prev) * joint(fpr)],
      independent: [pop * prev * sens * sens, pop * (1 - prev) * fpr * fpr],
      ceiling: [pop * prev * sens, pop * (1 - prev) * fpr]
    };
  }

  // --- Tablica 2×2 (lc_crosstab, measure = "n", big_mark = " ") ------------------------

  function crosstab(d) {
    var rows = [["Awaria", d.alarm[0], d.noAlarm[0], d.total[0]], ["Brak awarii", d.alarm[1], d.noAlarm[1], d.total[1]]];
    var foot = ["Razem", d.alarm[0] + d.alarm[1], d.noAlarm[0] + d.noAlarm[1], d.total[0] + d.total[1]];
    // Liczby wyrównane niewidocznymi zerami do najdłuższego zapisu w kolumnie (z sumą).
    var width = [1, 2, 3].map(function (j) {
      return Math.max.apply(null, rows.concat([foot]).map(function (r) { return big(r[j]).length; }));
    });
    function cell(v, j, label, total) {
      var t = big(v), pad = width[j - 1] - t.length;
      return '<td class="n' + (total ? " is-total" : "") + '"' + (label ? ' data-label="' + label + '"' : "") + ">" +
        (pad > 0 ? '<span class="lc-pad" aria-hidden="true">' + new Array(pad + 1).join("0") + "</span>" : "") + t + "</td>";
    }
    function row(r, tag, labels) {
      return "<tr><th scope=\"row\">" + r[0] + "</th>" + cell(r[1], 1, labels && "Alarm") + cell(r[2], 2, labels && "Brak alarmu") +
        cell(r[3], 3, null, true) + "</tr>";
    }
    return '<div class="lc-tbl-block"><div class="lc-tbl-scroll" tabindex="0" role="region" aria-label="Tablica 2×2 dla 10 000 zmian">' +
      '<table class="lc-tbl is-fit"><thead><tr><th scope="col" rowspan="2" class="lc-tbl-corner">Stan</th>' +
      '<th scope="colgroup" colspan="2" class="lc-tbl-span">Odczyt detektora</th><th scope="col" rowspan="2" class="n is-total">Razem</th></tr>' +
      '<tr><th scope="col" class="n">Alarm</th><th scope="col" class="n">Brak alarmu</th></tr></thead>' +
      "<tbody>" + row(rows[0], "td", true) + row(rows[1], "td", true) + "</tbody><tfoot>" +
      row(foot, "td", false).replace(/<td class="n" >/g, '<td class="n">') + "</tfoot></table></div></div>";
  }

  // --- Drzewo (alarm_tree_svg w block.R) ---------------------------------------------

  function treeSvg(d, prev, sens, fpr, C) {
    var tp = d.alarm[0], fp = d.alarm[1], accent = C.accent, alt = C.alt;
    function width(n) { return 3 + 21 * n / Math.max(1, tp, fp); }
    function node(k, x, y, label, n, color, dim) {
      return '<g class="a3-tree-node' + (dim ? " is-dim" : "") + '" data-k="' + k + '"><rect x="' + (x - 75) + '" y="' + (y - 18) +
        '" width="150" height="36" rx="18"' + (color ? ' style="stroke:' + color + ';stroke-width:2.5"' : "") + '/><text x="' + x + '" y="' + (y + 5) +
        '" text-anchor="middle"' + (color ? ' style="fill:' + color + '"' : "") + ">" + label + " · " + big(n) + "</text></g>";
    }
    function edge(k, x1, y1, x2, y2, w, color, dim) {
      var mx = (x1 + x2) / 2;
      return '<path class="a3-tree-edge' + (dim ? " is-dim" : "") + '" data-k="' + k + '" d="M' + x1 + " " + y1 + " C" + mx + " " + y1 + " " + mx + " " + y2 +
        " " + x2 + " " + y2 + '" style="stroke-width:' + (w == null ? 2 : w).toFixed(1) + (color ? ";stroke:" + color : "") + '"/>';
    }
    function prob(k, x, y, p) {
      return '<text class="a3-tree-p" data-k="' + k + '" x="' + x + '" y="' + y + '" text-anchor="middle">' + fmtP(p, 2) + "</text>";
    }
    var total = d.total[0] + d.total[1];
    return '<svg class="a3-tree-svg" viewBox="0 0 760 300" role="img" data-prev="' + prev + '" data-sens="' + sens + '" data-fpr="' + fpr +
      '" data-tp="' + tp + '" data-fn="' + d.noAlarm[0] + '" data-fp="' + fp + '" data-tn="' + d.noAlarm[1] + '" aria-label="Drzewo: ' +
      big(total) + " zmian dzieli się na awarie i brak awarii, a każda grupa na alarm i brak alarmu. Prawdziwych alarmów jest " +
      big(tp) + ", fałszywych " + big(fp) + '.">' +
      edge("e-a", 150, 150, 245, 70) + edge("e-n", 150, 150, 245, 230) +
      edge("e-hit", 395, 70, 485, 30, width(tp), accent) + edge("e-miss", 395, 70, 485, 110, null, null, true) +
      edge("e-fa", 395, 230, 485, 190, width(fp), alt) + edge("e-tn", 395, 230, 485, 270, null, null, true) +
      prob("p-a", 186, 96, prev) + prob("p-n", 186, 218, 1 - prev) +
      prob("p-hit", 440, 26, sens) + prob("p-miss", 440, 118, 1 - sens) +
      prob("p-fa", 440, 174, fpr) + prob("p-tn", 440, 278, 1 - fpr) +
      node("n-root", 75, 150, "Zmiany", total) +
      node("n-a", 320, 70, "Awaria", d.total[0]) + node("n-n", 320, 230, "Brak awarii", d.total[1]) +
      node("n-hit", 560, 30, "Alarm", tp, accent) + node("n-miss", 560, 110, "Bez alarmu", d.noAlarm[0], null, true) +
      node("n-fa", 560, 190, "Alarm", fp, alt) + node("n-tn", 560, 270, "Bez alarmu", d.noAlarm[1], null, true) +
      '<g data-k="b-alarm"><path class="a3-tree-brace" d="M642 30 h14 v160 h-14 M656 110 h10"/>' +
      '<text class="a3-tree-sum" x="672" y="106">' + big(tp + fp) + '</text><text class="a3-tree-sum" x="672" y="126">alarmów</text></g>' +
      '<g data-k="b-quiet"><path class="a3-tree-brace" d="M642 110 h14 v160 h-14 M656 190 h10"/><text class="a3-tree-sum" x="672" y="186">' +
      big(d.noAlarm[0] + d.noAlarm[1]) + '</text><text class="a3-tree-sum" x="672" y="206">cichych</text></g></svg>';
  }

  // --- Serpuchów (stop_serpuchow_case.R) ---------------------------------------------

  function spChain(k, m, s) {
    s = Math.min(Math.max(s, 0), 0.99);
    var lo = [-k, -k + m, -k + m + Math.log10(1 - s)];
    return { logOdds: lo, p: lo.map(function (v) { return 1 / (1 + Math.pow(10, -v)); }) };
  }
  function spFactor(x) { return "× " + signif2(x); }
  var SUPER = { "-": "⁻", 0: "⁰", 1: "¹", 2: "²", 3: "³", 4: "⁴", 5: "⁵", 6: "⁶", 7: "⁷", 8: "⁸", 9: "⁹" };
  function spAxisLabel(lo) {
    if (lo <= -3) return "10" + String(lo).replace(/[-0-9]/g, function (c) { return SUPER[c]; });
    if (lo === -2) return "0.01";
    if (lo === 0) return "0.5";
    if (lo === 2) return "0.99";
    if (lo === 4) return "0.9999";
    return signif2(1 / (1 + Math.pow(10, -lo)));
  }

  root.lcA3 = { rround: rround, big: big, fmtP: fmtP, bayes: bayes, twoAlarm: twoAlarm, detectorCounts: detectorCounts,
                doubleCounts: doubleCounts, crosstab: crosstab, treeSvg: treeSvg, spChain: spChain, spFactor: spFactor,
                spAxisLabel: spAxisLabel };

  if (typeof document === "undefined") return;
  var Ch = root.lcCharts, S = root.lcSF;

  // --- Wspólne suwaki detektora --------------------------------------------------------

  function num(id, fallback) { var el = document.getElementById(id); return el ? Number(el.value) : fallback; }
  function params() { return { prev: num("a3_prev", 0.01), sens: num("a3_sens", 0.95), fpr: num("a3_fpr", 0.05) }; }
  function colors() {
    var t = function (n, f) { return Ch ? Ch.token(n, f) : f; };
    return { accent: t("--upwr-accent", "#6b1a2a"), alt: t("--upwr-single-alt", "#c08540") };
  }
  function setText(sel, text) { document.querySelectorAll(sel).forEach(function (el) { el.textContent = text; }); }

  function update() {
    var p = params(), d = detectorCounts(10000, p.prev, p.sens, p.fpr), C = colors();
    document.querySelectorAll('[data-a3="table"]').forEach(function (el) {
      var html = crosstab(d); if (el.innerHTML !== html) el.innerHTML = html;
    });
    document.querySelectorAll('[data-a3="tree"]').forEach(function (el) {
      var html = treeSvg(d, p.prev, p.sens, p.fpr, C); if (el.innerHTML !== html) el.innerHTML = html;
    });
    setText('[data-a3v="tp"]', String(d.alarm[0]));
    setText('[data-a3v="fp"]', String(d.alarm[1]));
    setText('[data-a3v="post"]', fmtP(bayes(p.prev, p.sens, p.fpr)));
    ["a3_prev", "a3_sens", "a3_fpr"].forEach(function (id) {
      var el = document.getElementById(id);
      if (el) setText('output[data-value="' + id + '"]', el.value);
    });
  }
  var pending = false;
  function schedule() {
    if (pending) return;
    pending = true;
    requestAnimationFrame(function () { pending = false; update(); });
  }
  document.addEventListener("input", function (e) {
    if (!/^a3_(prev|sens|fpr)$/.test(e.target.id || "")) return;
    update();
    // Rycina a3_double czyta te suwaki, ale lcSF słucha tylko własnych kontrolek.
    var dep = document.getElementById("a3_dependence");
    if (dep) dep.dispatchEvent(new Event("input", { bubbles: true }));
  });
  // Panele wstawia renderUI rozdziału: rysujemy, gdy pojawi się pusty kontener.
  new MutationObserver(function () {
    if (document.querySelector('[data-a3="table"]:empty, [data-a3="tree"]:empty')) schedule();
  }).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener("DOMContentLoaded", schedule);
  schedule();

  if (!Ch || !S) return;

  // --- Ryciny lcSF -----------------------------------------------------------------------

  S.register({
    // Dwa alarmy: przerywany kontur — czujniki niezależne, pełny słupek — bieżące kopiowanie.
    // Oś y stała dla danych parametrów detektora (górna granica to d = 1).
    a3_double: {
      draw: function (v, P, k, state) {
        var p = params(), dep = Number(v), c = doubleCounts(p.prev, p.sens, p.fpr, dep), top = Math.max(c.ceiling[0], c.ceiling[1], 1);
        var half = 0.17, w = 0.3, col = [P.accent, colors().alt], layers = [], segs = [];
        state.p1 = bayes(p.prev, p.sens, p.fpr); state.p2 = twoAlarm(p.prev, p.sens, p.fpr, dep);
        [1, 2].forEach(function (x, i) {
          var a = x - half - w / 2, b = x - half + w / 2, h = c.independent[i];
          layers.push({ type: "rect", x0: a, x1: b, y0: 0, y1: h, fill: P.surface, z: 2 });
          segs.push({ from: [a, 0], to: [a, h] }, { from: [a, h], to: [b, h] }, { from: [b, h], to: [b, 0] });
          layers.push({ type: "rect", x0: x + half - w / 2, x1: x + half + w / 2, y0: 0, y1: c.count[i], fill: col[i], z: 2 },
                      { type: "text", x: x + half, y: c.count[i], text: big(rround(c.count[i])), color: P.ink, mono: true, size: 1.25 });
        });
        layers.push({ type: "segs", items: segs, color: P.ref, width: 1.4, dash: [5, 4] });
        return { x: [0.45, 2.55], y: [0, top * 1.12], xLabels: { 1: "Prawdziwe", 2: "Fałszywe" }, noXGrid: true,
                 layers: layers, yName: "Podwójne alarmy na 10 000 zmian",
                 aria: "Słupki podwójnych alarmów prawdziwych i fałszywych na 10 000 zmian; przerywany kontur pokazuje czujniki niezależne." };
      },
      update: function (rootEl, state) {
        S.setValue(rootEl, "p1", fmtP(state.p1)); S.setValue(rootEl, "p2", fmtP(state.p2));
      }
    },

    // Koszt reakcji c a oczekiwany koszt braku reakcji L · q; próg q* = c/L.
    a3_cost: {
      draw: function (v, P, k, state, vals) {
        var c = Number(vals.a3_cost_c), L = Number(vals.a3_cost_L), qs = c / L, qmax = 0.5, top = L * qmax, band = -0.13 * top;
        var cases = state.config.cases, layers = [
          { type: "rect", x0: qs, x1: qmax, y0: 0, y1: top, fill: P.accent, alpha: 0.06, z: 1 },
          { type: "vline", x: qs, from: 0, to: top, color: P.ref, width: 1.6, dash: [6, 5] },
          { type: "line", data: [[0, c], [qmax, c]], color: P.secondary, width: 2.8 },
          { type: "line", data: [[0, 0], [qmax, top]], color: P.secondary, width: 2.8, dash: [6, 5] }];
        cases.forEach(function (cs) {
          var act = cs.q > qs;
          layers.push({ type: "point", x: cs.q, y: 0, color: act ? P.accent : P.ref, size: 16 });
          if (!act) layers.push({ type: "point", x: cs.q, y: 0, color: P.surface, size: 11 });
          layers.push({ type: "text", x: cs.q, y: band, text: cs.label, color: act ? P.accent : P.ref, bold: true, valign: "middle", size: 1.15 });
        });
        state.qs = qs; state.c = c; state.L = L;
        return { x: [-0.015, 0.51], y: [1.25 * band, top * 1.04], xTicks: [0, 0.1, 0.2, 0.3, 0.4, 0.5], layers: layers,
                 xName: "Posterior q = P(awaria | alarm)", yName: "Oczekiwany koszt (zł)",
                 aria: "Oczekiwany koszt reakcji (pozioma linia) i braku reakcji (rosnąca linia) względem posterioru; pionowa linia w progu q* = c/L, punkty trzech sytuacji z przykładu 3.8." };
      },
      update: function (rootEl, state) {
        var hall = state.config.cases[1].q, act = hall > state.qs;
        S.setValue(rootEl, "qs", fmtP(state.qs));
        S.setValue(rootEl, "hall", act ? "reagować" : "nie reagować");
        var box = rootEl.querySelector('[data-v="hall"]'); box = box && box.closest(".lc-read");
        if (box) box.style.setProperty("--lc-read-color", act ? "var(--upwr-accent)" : "var(--upwr-reference)");
      }
    },

    // Serpuchów: łańcuch szans w skali log10; strzałki opisane mnożnikami.
    a3_sp: {
      draw: function (v, P, k, state, vals) {
        var K = Number(vals.a3_sp_prior), m = Number(vals.a3_sp_lr), s = Number(vals.a3_sp_radar);
        var ch = spChain(K, m, s), lo = ch.logOdds, gap = 0.11, layers = [];
        state.p = ch.p;
        var labels = [spFactor(Math.pow(10, m)), spFactor(1 - Math.min(Math.max(s, 0), 0.99))];
        layers.push({ type: "line", data: [[0.7, 0], [3.3, 0]], color: P.ref, width: 1.4, dash: [6, 5] });  // P(atak) = 0.5
        for (var i = 0; i < 2; i++) {
          var dlo = lo[i + 1] - lo[i], x = i + 1;
          layers.push({ type: "arrow", from: [x + gap, lo[i] + gap * dlo], to: [x + 1 - gap, lo[i + 1] - gap * dlo], color: P.secondary, width: 2.2 });
          var steep = Math.abs(dlo) >= 0.25;
          layers.push({ type: "text", x: x + 0.5 - (steep ? 0.03 * Math.sign(dlo) : 0), y: (lo[i] + lo[i + 1]) / 2 + 0.35, text: labels[i],
                        color: P.ink, size: 1.3, align: !steep ? null : dlo > 0 ? "right" : "left" });
        }
        layers.push({ type: "points", data: [[1, lo[0]], [2, lo[1]], [3, lo[2]]], colors: [P.secondary, P.secondary, P.accent], size: 17 });
        var ymin = Math.min(-8.6, Math.min.apply(null, lo) - 0.6), yLabels = {};
        for (var b = 2 * Math.floor(ymin / 2) + 2; b <= 4; b += 2) yLabels[b] = spAxisLabel(b);
        return { x: [0.7, 3.3], y: [ymin, 5.3], xLabels: { 1: "przed alarmem", 2: "po satelicie", 3: "po radarach" },
                 yLabels: yLabels, noXGrid: true, layers: layers, yName: "P(atak)",
                 aria: "Łańcuch szans w trzech etapach: przed alarmem, po satelicie i po radarach, na osi prawdopodobieństwa w skali logarytmicznej szans, ze strzałkami opisanymi mnożnikami." };
      },
      update: function (rootEl, state) {
        S.setValue(rootEl, "sp2", fmtP(state.p[1])); S.setValue(rootEl, "sp3", fmtP(state.p[2]));
      }
    }
  });
})();
