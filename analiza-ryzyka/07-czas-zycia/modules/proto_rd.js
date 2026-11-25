// Prototypy punktów stop wykładu 07, grupa rd (P4): rycina lcSF liczona w przeglądarce.
// Kontenery w R: lc_sf_figure() w stop_proto_rd.R (c7p_4_ui).
//
//   c7_koniec    — Trzy rachunki a koniec badania (P4): osiem wentylatorów jako linie
//                  życia, suwak końca badania c, średnia z samych awarii.
//                  Opcjonalnie prawdziwe czasy życia (przerywane przyszłe awarie).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcC7;
  if (!C || !S || !Z) return;
  var U = Z.util;

  // Stałe: wspólna oś czasu P4, prawdziwa średnia ośmiu czasów (1368.75 h).
  var X4 = [0, 3300], Y4 = [-0.9, 8.8], TRUE_MEAN = 1368.75;

  // Liczby z kropką dziesiętną, bez końcowych zer (jak lc_fmt w R).
  function num(x, d) { return S.fmt(x, d); }
  function hours(x, d) { return num(x, d) + " h"; }

  // ---- P4: pomiar badania przerwanego po c godzinach --------------------------
  // d = awarie (T ≤ c), sum = łączny czas pracy Σ min(T, c). Trzy rachunki:
  // naive = średnia z rejestru awarii, lower = sum / n (dolna granica),
  // est = sum / d (wzór 7.2).
  function measure(times, c) {
    var sum = 0, failSum = 0, d = 0;
    times.forEach(function (t) {
      sum += Math.min(t, c);
      if (t <= c) { d++; failSum += t; }
    });
    return { c: c, d: d, sum: sum, naive: d ? failSum / d : NaN,
             lower: sum / times.length, est: d ? sum / d : NaN };
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
      // Wiersz „średnie” (y = 0): trzy rachunki i, po zaznaczeniu pola, prawdziwa średnia.
      if (isFinite(m.naive)) layers.push({ type: "point", x: m.naive, y: 0, symbol: "diamond", size: 14, color: P.accent });
      layers.push({ type: "point", x: m.lower, y: 0, symbol: "diamond", size: 14, color: P.szalwia });
      if (isFinite(m.est)) layers.push({ type: "point", x: Math.min(m.est, X4[1]), y: 0, symbol: "diamond", size: 14, color: P.niebo });
      if (truth) layers.push({ type: "point", x: TRUE_MEAN, y: 0, symbol: "diamond", size: 14, color: P.ink });

      var yLabels = { 0: "średnie" };
      cfg.times.forEach(function (t, j) { yLabels[j + 1] = String(j + 1); });

      return {
        x: X4, y: Y4, xLabelEvery: 500, xName: "Czas pracy (h)", yLabels: yLabels, noYGrid: true,
        legend: [
          { text: "średnia z rejestru awarii", color: P.accent, box: true },
          { text: "dolna granica", color: P.szalwia, box: true },
          { text: "szacunek (7.2)", color: P.niebo, box: true },
          { text: "prawdziwa średnia (zwykle nieznana)", color: P.ink, box: true }
        ],
        layers: layers,
        aria: "Osiem wentylatorów, badanie przerwane po " + c + " godzinach. Awarii: " + m.d +
          ". Średnia z rejestru awarii " + num(m.naive, 1) + ", dolna granica " + num(m.lower, 0) +
          ", szacunek ze wzoru 7.2 " + num(m.est, 0) + " godzin."
      };
    },
    update: function (root, state) {
      var m = state.calc, n = state.config.times.length;
      S.setValue(root, "d", m.d + " z " + n);
      S.setValue(root, "naive", isFinite(m.naive) ? hours(m.naive, 1) : "–");
      S.setValue(root, "lower", hours(m.lower, 0));
      S.setValue(root, "est", isFinite(m.est) ? hours(m.est, 0) : "–");
      S.setValue(root, "truth", m.truth ? hours(TRUE_MEAN, 2) : "nieznana");
    }
  };

  S.register({
    c7_koniec: figKoniec
  });
})();
