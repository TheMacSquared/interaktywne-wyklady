// Prototypy punktów stop wykładu 08, grupa rg (P19A, P19B, P23). Ryciny lcSF
// liczone w przeglądarce; kontenery w R: lc_sf_figure() w stop_proto_rg.R.
//
//   s8_galezie     — „Krzywa nasycenia z sufitem” (P19A): R układu dla n gałęzi
//                    o niezawodności r; ze wspólnym zasilaniem q sufit 1 − q.
//   s8_galezie_log — „Ryzyko gałąź po gałęzi” (P19B): ryzyko na osi log10,
//                    niezależne gałęzie na prostej, wspólne zasilanie: podłoga q.
//   s8_poprawa     — „Ta sama poprawa, trzy miejsca” (P23): zysk z poprawy
//                    o 0.02 każdego elementu szeregu (istotność Birnbauma · Δ).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcS8;
  if (!C || !S || !Z) return;
  var U = Z.util;

  var N_MAX = 6;                 // liczba gałęzi na osi (P19A, P19B)
  var LOG_FLOOR = -6.3;          // najniższa wartość log10 na osi P19B
  var LOG_GOAL = -4;             // log10(0.0001): cel ryzyka, R* = 0.9999

  function num(vals, id) { return Number(vals[id]); }

  // R układu z n gałęziami o niezawodności r i wspólnym zasilaniem q (wzór 8.13);
  // q = 0 daje krzywą bez wspólnej przyczyny.
  function rSys(r, n, q) { return (1 - q) * (1 - Math.pow(1 - r, n)); }
  // Ryzyko jednej serii gałęzi niezależnych: (1 − r)^n.
  function nRisk(r, n) { return Math.pow(1 - r, n); }
  // log10 ryzyka przycięty do dolnej granicy osi.
  function logClip(x) { return Math.max(LOG_FLOOR, Math.log10(x)); }
  // Najmniejsze n z (1 − r)^n ≤ 0.0001; zapas 1e-9 na zaokrąglenia.
  function gammaGoal(r) { return Math.ceil(Math.log(0.0001) / Math.log(1 - r) - 1e-9); }

  S.register({
    s8_galezie: {
      draw: function (v, P, k, state, vals) {
        var n = num(vals, "s8p19a_n"), r = num(vals, "s8p19a_r"), q = num(vals, "s8p19a_q");
        state.n = n; state.r = r; state.q = q;
        var main = [], indep = [], ticks = {};
        for (var i = 1; i <= N_MAX; i++) {
          ticks[i] = String(i);
          main.push([i, rSys(r, i, q)]);
          indep.push([i, rSys(r, i, 0)]);
        }
        var layers = [];
        if (q > 0) layers.push({ type: "line", data: indep, color: P.ref, width: 1.6, dash: [5, 4] });
        layers.push({ type: "line", data: main, color: P.accent, width: 2.4 });
        layers.push({ type: "points", data: main, color: P.accent, size: 8 });
        layers.push({ type: "point", x: n, y: rSys(r, n, q), color: P.secondary, size: 16 });
        if (q > 0) {
          var ceil = 1 - q;
          layers.push({ type: "line", data: [[0.5, ceil], [6.5, ceil]], color: P.ink, width: 1.6, dash: [6, 4] });
          layers.push({ type: "text", x: 0.6, y: ceil, text: "sufit 1 − q = " + U.fmt(ceil, 2),
            color: P.ink, align: "left", mono: true, halo: true });
        }
        var legend = [{ text: "R układu", color: P.accent }];
        if (q > 0) legend.push({ text: "bez wspólnej przyczyny", color: P.ref, dash: true });
        return {
          x: [0.5, 6.5], y: [0.5, 1.02], xLabels: ticks, xName: "Liczba gałęzi", yName: "R układu",
          legend: legend, layers: layers,
          aria: "Niezawodność układu " + n + " gałęzi o niezawodności " + r + (q > 0 ? " ze wspólnym zasilaniem q = " + q : "") +
            ": " + U.fmtP(rSys(r, n, q)) + "."
        };
      },
      update: function (root, state) {
        var n = state.n, r = state.r, q = state.q;
        var rn = rSys(r, n, q), prev = n > 1 ? rSys(r, n - 1, q) : null;
        S.setValue(root, "rs", U.fmtP(rn));
        S.setValue(root, "gain", prev == null ? "—" : U.fmtP(rn - prev));
        S.setValue(root, "risk", U.fmtP(1 - rn));
        S.setValue(root, "cost", String(n * 100) + " jedn.");
      }
    },

    s8_galezie_log: {
      draw: function (v, P, k, state, vals) {
        var r = num(vals, "s8p19b_r"), q = num(vals, "s8p19b_q");
        state.r = r; state.q = q;
        var ind = [], com = [];
        for (var i = 1; i <= N_MAX; i++) {
          // punkty poniżej zakresu osi pomijamy (linia urywa się na krawędzi, bez sztucznego załamania)
          if (Math.log10(nRisk(r, i)) >= LOG_FLOOR) ind.push([i, logClip(nRisk(r, i))]);
          com.push([i, logClip(1 - (1 - q) * (1 - nRisk(r, i)))]);
        }
        var logQ = Math.log10(q);
        var layers = [
          { type: "line", data: ind, color: P.secondary, width: 2.2, dash: q > 0 ? [5, 4] : null },
          { type: "points", data: ind, color: P.secondary, size: 7 }
        ];
        if (q > 0) {
          layers.push({ type: "line", data: com, color: P.accent, width: 2.8 });
          layers.push({ type: "points", data: com, color: P.accent, size: 8 });
          layers.push({ type: "line", data: [[0.5, logQ], [6.5, logQ]], color: P.accent, width: 1.4, dash: [3, 3] });
          // Napis przy prawym końcu, a dla słabszych gałęzi (r < 0.85) przy lewym: tam krzywe
          // niezależne schodzą w dół przez prawy koniec i zasłaniają napis.
          var floorRight = r >= 0.85;
          layers.push({ type: "text", x: floorRight ? 6.4 : 0.6, y: logQ, text: "podłoga q = " + U.fmt(q, 2),
            color: P.accent, align: floorRight ? "right" : "left", mono: true, halo: true });
        }
        layers.push({ type: "line", data: [[0.5, LOG_GOAL], [6.5, LOG_GOAL]], color: P.ink, width: 1.6, dash: [6, 4] });
        // Napis celu: przy mocnych gałęziach (r ≥ 0.9) prawy koniec, bo krzywa niezależna
          // przecina cel już przy n = 1…2 i zasłania lewy.
        var goalRight = r >= 0.9;
        layers.push({ type: "text", x: goalRight ? 6.4 : 0.6, y: LOG_GOAL, text: "cel: ryzyko 0.0001 (R* = 0.9999)",
          color: P.ink, align: goalRight ? "right" : "left", mono: true, halo: true });
        var legend = [{ text: "niezależne gałęzie", color: P.secondary }];
        if (q > 0) legend.push({ text: "ze wspólnym zasilaniem", color: P.accent });
        return {
          x: [0.5, 6.5], y: [LOG_FLOOR, 0.2],
          xLabels: { 1: "1", 2: "2", 3: "3", 4: "4", 5: "5", 6: "6" }, xName: "Liczba gałęzi",
          yLabels: { 0: "1", "-1": "0.1", "-2": "0.01", "-3": "0.001", "-4": "0.0001", "-5": "0.00001", "-6": "0.000001" },
          yName: "Ryzyko układu (skala log.)",
          legend: legend, layers: layers,
          aria: "Ryzyko układu o gałęziach o niezawodności " + r + " na skali logarytmicznej" +
            (q > 0 ? " ze wspólnym zasilaniem q = " + q : "") + "."
        };
      },
      update: function (root, state) {
        var r = state.r, q = state.q, goal = gammaGoal(r);
        S.setValue(root, "goal", String(goal));
        S.setValue(root, "goalc", q > 0 ? "nieosiągalne" : String(goal));
        var p6 = q > 0 ? 1 - (1 - q) * (1 - nRisk(r, N_MAX)) : nRisk(r, N_MAX);
        // bardzo małe ryzyko: bez ogona zer z fmtP
        S.setValue(root, "risk6", p6 < 1e-6 - 1e-12 ? "< 0.000001" : p6 < 0.01 ? U.fmtP(p6).replace(/0+$/, "") : U.fmtP(p6));
      }
    },

    s8_poprawa: {
      draw: function (v, P, k, state, vals) {
        var R = [num(vals, "s8p23_r1"), num(vals, "s8p23_r2"), num(vals, "s8p23_r3")];
        var names = ["wentylator", "czujnik", "zasilanie"];
        // Zysk z poprawy o 0.02 elementu i: 0.02 · iloczyn R pozostałych (wzór 8.15).
        var birn = R.map(function (_, i) {
          var p = 1;
          R.forEach(function (x, j) { if (j !== i) p *= x; });
          return p;
        });
        var gains = birn.map(function (b) { return 0.02 * b; });
        var best = 0;
        gains.forEach(function (g, i) { if (g > gains[best]) best = i; });
        var rs = R[0] * R[1] * R[2];
        state.rs = rs; state.best = best; state.birn = birn[best];
        state.names = names;
        var others = [], top = [], texts = [];
        gains.forEach(function (g, i) {
          var bin = [i + 1 - 0.3, i + 1 + 0.3, g];
          (i === best ? top : others).push(bin);
          texts.push({ type: "text", x: i + 1, y: g, text: U.fmt(g, 4), color: P.ink, mono: true, halo: true });
        });
        var layers = [{ type: "bins", data: others, color: P.secondary, alpha: 0.75 }];
        layers.push({ type: "bins", data: top, color: P.accent });
        return {
          x: [0.4, 3.6], y: [0, 0.022],
          xLabels: { 1: "Wentylator", 2: "Czujnik", 3: "Zasilanie" }, noXGrid: true,
          yName: "Wzrost R systemu po poprawie o 0.02",
          layers: layers.concat(texts),
          aria: "Zysk z poprawy o 0.02 w trzech elementach szeregu; najwięcej daje " + names[best] + " (" + U.fmt(gains[best], 4) + ")."
        };
      },
      update: function (root, state) {
        S.setValue(root, "rs", U.fmtP(state.rs));
        S.setValue(root, "best", state.names[state.best]);
        S.setValue(root, "birn", U.fmt(state.birn, 3));
      }
    }
  });
})();
