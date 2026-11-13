// Ryciny wykładu 04 liczone w przeglądarce (silnik lcSF): rozdziały 1–2.
// Silnik: window.lcSF, losowanie: window.lcDist. UI w figures_ch1.R i ch2_idea.R.
//
//   d04-tarcza   — Ryc. 1.1: cztery tarcze, obciążenie i rozrzut (dane z R w config)
//   d04-miarki   — Ryc. 1.2: cztery miarki, histogramy estymat z grupek (losuje JS)
//   d04-stos     — Ryc. 1.3: 4× więcej osób, połowa rozrzutu (słupki gęstości z R w config)
//   d04-pizzerie — Ryc. 1.4: dwie pizzerie, średnie ocen z kolejnych dni (losuje JS)
//   Ryc. 2.3     — quiz z kafelkami (.c04-quiz): odpowiedź i komentarz bez serwera
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist;
  if (!C || !S || !D) return;

  function wrzos() { return C.token("--upwr-cat-wrzos", "#a07894"); }
  function mean(v) { var s = 0; for (var i = 0; i < v.length; i++) s += v[i]; return s / v.length; }
  // sd() z R (mianownik n − 1); NaN dla mniej niż dwóch wartości.
  function sd(v) {
    if (v.length < 2) return NaN;
    var m = mean(v), s = 0;
    for (var i = 0; i < v.length; i++) s += (v[i] - m) * (v[i] - m);
    return Math.sqrt(s / (v.length - 1));
  }
  function median(v) {
    var s = v.slice().sort(function (a, b) { return a - b; }), h = s.length >> 1;
    return s.length % 2 ? s[h] : (s[h - 1] + s[h]) / 2;
  }

  // ---- Ryc. 1.2: cztery miarki ------------------------------------------------
  // Jedna grupka = 25 wzrostów z N(170, 10); z każdej cztery estymaty. Uszkodzona
  // miarka: brakuje 2 cm początku taśmy, każdy pomiar o 2 cm za duży.
  var EST_LABELS = ["Średnia", "Mediana", "Środek rozstępu", "Uszkodzona miarka"];
  function simulateGroups(est, k, cfg, g) {
    for (var j = 0; j < k; j++) {
      var x = g.many(cfg.n, function () { return g.norm(cfg.mu, cfg.sigma); });
      var lo = Math.min.apply(null, x), hi = Math.max.apply(null, x), m = mean(x);
      est[0].push(m);
      est[1].push(median(x));
      est[2].push((lo + hi) / 2);
      est[3].push(m + 2);
    }
  }
  var MIARKI = {
    init: function (state) { state.est = [[], [], [], []]; },
    act: function (state, name) {
      if (name === "reset") { state.est = [[], [], [], []]; return; }
      var k = { add1: 1, add100: 100, add1000: 1000 }[name];
      if (k) simulateGroups(state.est, k, state.config, D.r);
    },
    prepare: function (state) {
      // Histogramy jak geom_histogram(binwidth = 0.5, boundary = 0); wspólna oś y (facet_wrap).
      state.bins = state.est.map(function (v) { return v.length ? S.binsWidth(v, 0.5, 0) : []; });
      var top = 0;
      state.bins.forEach(function (b) { b.forEach(function (r) { if (r.n > top) top = r.n; }); });
      state.top = top || 1;
    },
    draw: function (v, P, k, state) {
      var cfg = state.config, top = state.top;
      return {
        x: [155, 185], y: [-0.05 * top, 1.05 * top], xLabelEvery: 5, noXGrid: true, xInt: false,
        xName: "Estymata wzrostu (cm)", yName: "Liczba grupek",
        layers: [
          { type: "bins", data: state.bins[k].map(function (r) { return [r.l, r.r, r.n]; }),
            color: k === 3 ? P.terakota : P.bursztyn, stroke: P.surface, lineWidth: 0.6 },
          { type: "vline", x: cfg.mu, color: P.secondary, width: 1.6, dash: [4, 4] }
        ],
        aria: EST_LABELS[k] + ": histogram estymat z " + state.est[k].length +
              " grupek po 25 osób, z przerywaną linią μ = 170 cm"
      };
    },
    update: function (root, state) {
      S.setValue(root, "groups", C.fmt(state.est[0].length, 0, true));
    },
    table: function (state) {
      var rows = state.est.map(function (v, i) {
        return [EST_LABELS[i], v.length ? C.fmt(mean(v), 1) : "–", v.length > 1 ? C.fmt(sd(v), 1) : "–"];
      });
      return S.table([{ label: "Estymator", row: true }, { label: "Średnia estymat, cm" }, { label: "SD estymat, cm" }], rows);
    }
  };

  // ---- Ryc. 1.1: tarcza ----------------------------------------------------------
  // config.panels: [{shots: [[x, y]], mean: [x, y] | null}]; środek tarczy = θ.
  var RING_T = D.seq(0, 2 * Math.PI, 181);
  var TARCZA = function (v, P, k, state) {
    var p = state.config.panels[k], layers = [];
    [1, 2, 3, 4].forEach(function (r) {
      layers.push({ type: "line", data: RING_T.map(function (t) { return [r * Math.cos(t), r * Math.sin(t)]; }),
                    color: P.ref, width: 1 });
    });
    layers.push({ type: "rect", x0: -4.2, x1: 4.2, y0: -4.2, y1: 4.2, stroke: P.rule, z: 1 },
                { type: "point", x: 0, y: 0, color: wrzos(), size: 8 },
                { type: "points", data: p.shots, color: P.bursztyn, size: 8 });
    if (p.mean) {
      // Średnia chmury strzałów obciążonego estymatora: krzyżyk (shape = 4).
      var m = p.mean, d = 0.17;
      layers.push({ type: "segs", color: P.ink, width: 1.6, z: 6, items: [
        { from: [m[0] - d, m[1] - d], to: [m[0] + d, m[1] + d] },
        { from: [m[0] - d, m[1] + d], to: [m[0] + d, m[1] - d] }] });
    }
    return { x: [-4.2, 4.2], y: [-4.2, 4.2], xTicks: [], hideY: true, noXGrid: true, noYGrid: true,
             layers: layers,
             aria: state.config.labels[k] + ": tarcza z dwudziestoma strzałami" +
                   (p.mean ? ", krzyżyk w średniej strzałów przesuniętej od środka" : " wokół środka") };
  };

  // ---- Ryc. 1.3: stos średnich ---------------------------------------------------
  // config.panels: [{n, sd, bins: [[l, r, gęstość]]}], config.mu.
  var STOS = function (v, P, k, state) {
    var cfg = state.config, p = cfg.panels[k], lo = cfg.mu - 2 * p.sd, hi = cfg.mu + 2 * p.sd, last = k === cfg.panels.length - 1;
    return {
      x: [163, 177], y: [-0.11, 0.95], xLabelEvery: 3, noXGrid: true,
      xName: last ? "Średnia z próby x̄ (cm)" : "", yName: "Gęstość",
      layers: [
        { type: "bins", data: p.bins, color: P.bursztyn, stroke: P.surface, lineWidth: 0.6 },
        { type: "vline", x: cfg.mu, color: P.secondary, width: 1.6, dash: [4, 4] },
        // Klamra ±2·SD pod histogramem.
        { type: "segs", color: P.niebo, width: 3, items: [
          { from: [lo, -0.06], to: [hi, -0.06] },
          { from: [lo, -0.085], to: [lo, -0.035] },
          { from: [hi, -0.085], to: [hi, -0.035] }] }
      ],
      aria: "n = " + p.n + ": histogram 3000 średnich wokół 170 cm, klamra ±2 SD od " + C.fmt(lo, 1) + " do " + C.fmt(hi, 1) + " cm"
    };
  };

  // ---- Ryc. 1.4: dwie pizzerie ---------------------------------------------------
  // config: init (wczorajsza średnia 4.4), rows [{who, n, y}], prob (1–5 ★).
  function addDays(state, k, g) {
    var cfg = state.config;
    cfg.rows.forEach(function (row, i) {
      for (var j = 0; j < k; j++) {
        var s = 0;
        for (var t = 0; t < row.n; t++) s += g.choose(cfg.prob);
        state.days[i].push([s / row.n, row.y + g.unif(-0.22, 0.22)]);
      }
    });
  }
  // SD średnich danej pizzerii łącznie z wczorajszą 4.4 ★.
  function pizzaSd(state, i) {
    return sd([state.config.init].concat(state.days[i].map(function (d) { return d[0]; })));
  }
  var PIZZERIE = {
    init: function (state) { state.days = state.config.rows.map(function () { return []; }); },
    act: function (state, name) {
      if (name === "reset") PIZZERIE.init(state);
      else if (name === "rep1") addDays(state, 1, D.r);
      else if (name === "rep20") addDays(state, 20, D.r);
    },
    draw: function (v, P, k, state) {
      var cfg = state.config, layers = [], yLabels = {};
      cfg.rows.forEach(function (row, i) {
        yLabels[row.y] = row.who;
        layers.push({ type: "segment", from: [1, row.y], to: [5, row.y], color: P.rule, width: 1.2 });
      });
      var reps = [].concat.apply([], state.days);
      if (reps.length) layers.push({ type: "points", data: reps, color: P.bursztyn, alpha: 0.35, size: 7 });
      cfg.rows.forEach(function (row) {
        layers.push({ type: "point", x: cfg.init, y: row.y, color: P.bursztyn, symbol: "diamond", size: 17 },
                    { type: "text", x: cfg.init, y: row.y + 0.32, text: C.fmt(cfg.init, 1) + " ★",
                      color: P.bursztyn, bold: true, valign: "middle" });
      });
      return {
        x: [1, 5.1], y: [0.4, 2.6], xLabels: { 1: "1 ★", 2: "2 ★", 3: "3 ★", 4: "4 ★", 5: "5 ★" }, yLabels: yLabels,
        noYGrid: true, xName: "Średnia ocena", layers: layers,
        aria: "Dwa wiersze na osi ocen od 1 do 5 gwiazdek: pizzeria U Gośki z 5 opiniami dziennie i Napoli z 200, " +
              "obie ze znacznikiem 4.4 i średnimi ocen z " + state.days[0].length + " kolejnych dni"
      };
    },
    update: function (root, state) {
      var s1 = pizzaSd(state, 0), s2 = pizzaSd(state, 1), f = function (s) { return isFinite(s) ? C.fmt(s, 2) + " ★" : "–"; };
      S.setValue(root, "days", String(state.days[0].length));
      S.setValue(root, "sd1", f(s1));
      S.setValue(root, "sd2", f(s2));
    },
    noteKey: function (vals, state) { return state.days[0].length ? "reps" : "empty"; }
  };

  S.register({ "d04-miarki": MIARKI, "d04-tarcza": TARCZA, "d04-stos": STOS, "d04-pizzerie": PIZZERIE });

  // ---- Ryc. 2.3: quiz z kafelkami --------------------------------------------------
  // .c04-quiz: kafelki [data-quiz-choice] i komentarze [data-quiz-feedback] z R. Jedna
  // odpowiedź: po wyborze kafelki znikają, a pojawia się komentarz do wybranej litery.
  document.addEventListener("click", function (e) {
    var tile = e.target.closest && e.target.closest("[data-quiz-choice]");
    var box = tile && tile.closest(".c04-quiz");
    if (!box || box.getAttribute("data-answered") != null) return;
    var val = tile.getAttribute("data-quiz-choice");
    box.setAttribute("data-answered", val === box.getAttribute("data-correct") ? "correct" : "wrong");
    var tiles = box.querySelector(".quiz-tiles");
    if (tiles) tiles.hidden = true;
    box.querySelectorAll("[data-quiz-feedback]").forEach(function (n) {
      n.hidden = n.getAttribute("data-quiz-feedback") !== val;
    });
  });

  window.lc04Ch12 = { simulateGroups: simulateGroups, sd: sd, median: median, pizzaSd: pizzaSd };
})();
