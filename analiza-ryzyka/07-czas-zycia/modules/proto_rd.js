// Prototypy punktów stop wykładu 07, grupa rd: ryciny lcSF liczone w przeglądarce.
// Kontenery w R: lc_sf_figure() w stop_proto_rd.R (c7p_4_ui, c7p_6_ui).
//
//   c7_koniec    — Koniec badania a średnia z awarii (P4): osiem wentylatorów jako linie
//                  życia, suwak końca badania c, naiwna średnia z awarii i szacunek (7.2).
//                  Opcjonalnie prawdziwe czasy życia (przerywane przyszłe awarie).
//   c7_powtorki  — To samo badanie powtórzone wiele razy (P6): symulacja z modelu
//                  wykładniczego MTTF 1500 h; dwa histogramy wyników jednego badania.
//                  Przyciski dopisują badania, „Od nowa” wraca do ziarna 2026.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcC7, D = window.lcDist;
  if (!C || !S || !Z || !D) return;
  var U = Z.util;

  // Stałe: wspólna oś czasu P4, prawdziwa średnia ośmiu czasów (1368.75 h).
  var X4 = [0, 3300], Y4 = [-0.9, 8.8], TRUE_MEAN = 1368.75;

  // Liczby z kropką dziesiętną, bez końcowych zer (jak lc_fmt w R).
  function num(x, d) { return S.fmt(x, d); }
  function hours(x, d) { return num(x, d) + " h"; }

  // ---- P4: pomiar badania przerwanego po c godzinach --------------------------
  // d = awarie (T ≤ c), sum = łączny czas obserwacji Σ min(T, c),
  // naive = średnia z samych awarii, est = szacunek (7.2) = sum / d.
  function measure(times, c) {
    var sum = 0, failSum = 0, d = 0;
    times.forEach(function (t) {
      sum += Math.min(t, c);
      if (t <= c) { d++; failSum += t; }
    });
    return { c: c, d: d, sum: sum, naive: d ? failSum / d : NaN, est: d ? sum / d : NaN };
  }

  var figKoniec = {
    prepare: function (state, v, vals) {
      state.calc = measure(state.config.times, Number(vals.c7p4_c));
      state.calc.truth = vals.c7p4_truth === "on";
    },
    draw: function (v, P, k, state) {
      var cfg = state.config, m = state.calc, c = m.c, truth = m.truth;
      var tr = [], trPts = [], life = [], fails = [], alive = [];
      cfg.times.forEach(function (t, j) {
        var y = j + 1, failed = t <= c;
        life.push({ from: [0, y], to: [Math.min(t, c), y], color: failed ? P.secondary : P.szalwia });
        if (failed) { fails.push([t, y]); return; }
        alive.push([c, y]);
        // Prawdziwa przyszła awaria: przerywany odcinek od końca badania do T.
        tr.push({ from: [c, y], to: [t, y], color: P.ref });
        trPts.push([t, y]);
      });

      var layers = [];
      if (truth) {
        layers.push({ type: "segs", items: tr, color: P.ref, width: 1.4, dash: [4, 4] });
        layers.push({ type: "points", data: trPts, color: P.ref, size: 7 });
      }
      layers.push({ type: "segs", items: life, color: P.secondary, width: 5 });
      layers.push({ type: "points", data: fails, color: P.accent, size: 11 });
      layers.push({ type: "points", data: alive, color: P.szalwia, symbol: "triangle", size: 12 });
      layers.push({ type: "vline", x: c, color: P.ink, width: 1.8, dash: [6, 4], from: 0.5, to: Y4[1] });
      // Przy prawej krawędzi osi napis stoi po lewej stronie kreski, żeby się nie ucinał.
      layers.push({ type: "text", x: c, y: "top", text: "koniec badania", color: P.ink,
                    align: c > 2800 ? "right" : "left", mono: true });
      // Wiersz „średnie” (y = 0): naiwna średnia, szacunek (7.2) przycięty do osi, prawdziwa średnia.
      if (isFinite(m.naive)) layers.push({ type: "point", x: m.naive, y: 0, symbol: "diamond", size: 14, color: P.accent });
      if (isFinite(m.est)) layers.push({ type: "point", x: Math.min(m.est, X4[1]), y: 0, symbol: "diamond", size: 14, color: P.niebo });
      if (truth) layers.push({ type: "point", x: TRUE_MEAN, y: 0, symbol: "diamond", size: 14, color: P.ink });

      var yLabels = { 0: "średnie" };
      cfg.times.forEach(function (t, j) { yLabels[j + 1] = String(j + 1); });

      return {
        x: X4, y: Y4, xLabelEvery: 500, xName: "Czas pracy (h)", yLabels: yLabels, noYGrid: true,
        legend: [
          { text: "awaria", color: P.accent, box: true },
          { text: "działa na końcu badania", color: P.szalwia, box: true },
          { text: "średnia z samych awarii", color: P.accent, box: true },
          { text: "szacunek ze wzoru (7.2)", color: P.niebo, box: true },
          { text: "prawdziwa średnia (zwykle nieznana)", color: P.ink, box: true }
        ],
        layers: layers,
        aria: "Osiem wentylatorów, badanie przerwane po " + c + " godzinach. Awarii: " + m.d +
          ". Średnia z samych awarii " + num(m.naive, 1) + " godzin, szacunek " + num(m.est, 1) + " godzin."
      };
    },
    update: function (root, state) {
      var m = state.calc, n = state.config.times.length;
      S.setValue(root, "d", m.d + " z " + n);
      S.setValue(root, "sum", hours(m.sum, 1));
      S.setValue(root, "naive", isFinite(m.naive) ? hours(m.naive, 1) : "–");
      S.setValue(root, "est", isFinite(m.est) ? hours(m.est, 1) : "–");
    }
  };

  // ---- P6: symulacja powtórzeń badania ----------------------------------------
  var X6 = [0, 4000], Y6 = 0.3, W6 = 100, NB6 = 40, MTTF_CUT = 4000;

  function median(a) {
    if (!a.length) return NaN;
    var s = a.slice().sort(function (x, y) { return x - y; }), h = s.length >> 1;
    return s.length % 2 ? s[h] : (s[h - 1] + s[h]) / 2;
  }

  // Jedno badanie: n wentylatorów z modelu wykładniczego, przerwane po c godzinach.
  // Badanie bez awarii (d = 0) jest pomijane, ale liczone w runs.
  function runOne(state) {
    var cfg = state.config, d = 0, failSum = 0, sum = 0, i, t;
    for (i = 0; i < cfg.n; i++) {
      t = state.r.exp(1 / cfg.mttf);
      if (t <= cfg.c) { d++; failSum += t; }
      sum += Math.min(t, cfg.c);
    }
    state.runs++;
    if (!d) { state.zero++; return; }
    state.naive.push(failSum / d);
    state.est.push(sum / d);
  }

  function startSim(state) {
    state.r = D.rng(state.config.seed);
    state.naive = []; state.est = []; state.runs = 0; state.zero = 0;
    runOne(state);
  }

  var ADD = { add1: 1, add10: 10, add100: 100, add1000: 1000 };

  var figPowtorki = {
    init: function (state) { startSim(state); },
    act: function (state, act) {
      if (act === "reset") { startSim(state); return; }
      var add = ADD[act] || 0;
      for (var i = 0; i < add; i++) runOne(state);
    },
    draw: function (v, P, k, state) {
      var cfg = state.config, vals = k === 0 ? state.naive : state.est;
      var total = Math.max(1, state.naive.length), col = k === 0 ? P.accent : P.niebo;
      var counts = [], j, bins = [];
      for (j = 0; j < NB6; j++) counts.push(0);
      vals.forEach(function (x) { if (x >= 0 && x < X6[1]) counts[Math.floor(x / W6)]++; });
      for (j = 0; j < NB6; j++) bins.push([j * W6, (j + 1) * W6, counts[j] / total]);

      var med = median(vals);
      var layers = [
        { type: "bins", data: bins, color: col, alpha: 0.75, stroke: P.surface, lineWidth: 0.6 },
        { type: "vline", x: cfg.mttf, color: P.ink, width: 1.8, dash: [6, 4], from: 0, to: Y6 },
        { type: "text", x: cfg.mttf, y: "top", text: "prawdziwy MTTF " + cfg.mttf + " h", color: P.ink, align: "left", mono: true }
      ];
      if (isFinite(med)) layers.push({ type: "vline", x: med, color: col, width: 2.4, from: 0, to: Y6 });

      return {
        x: X6, y: [0, Y6], xLabelEvery: 500, yName: "Udział badań",
        xName: k === 1 ? "Wynik jednego badania (h)" : "",
        alignLeft: true, layers: layers,
        aria: (k === 0 ? "Histogram średnich z samych awarii" : "Histogram szacunków ze wzoru (7.2)") +
          " z " + vals.length + " badań; pionowa kreska to mediana."
      };
    },
    update: function (root, state) {
      var nv = state.naive.length, est = state.est;
      var below = state.naive.filter(function (x) { return x < state.config.mttf; }).length;
      var out = est.filter(function (x) { return x >= MTTF_CUT; }).length;
      S.setValue(root, "n", String(state.runs));
      S.setValue(root, "naive", isFinite(median(state.naive)) ? U.fmt(median(state.naive), 0) + " h" : "–");
      S.setValue(root, "est", isFinite(median(est)) ? U.fmt(median(est), 0) + " h" : "–");
      S.setValue(root, "below", nv ? U.fmt(100 * below / nv, 0) + "%" : "–");
      S.setValue(root, "out", String(out));
    }
  };

  S.register({
    c7_koniec: figKoniec,
    c7_powtorki: figPowtorki
  });
})();
