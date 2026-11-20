// Ryciny wykładu 04 liczone w przeglądarce (silnik lcSF): sf_steps
//   s04-tnorm  — Ryc. 3.1: rozkład t (df = 4) a N(0, 1), wartości krytyczne
//   s04-comp   — Przykłady A–C (rozdz. 3): CI grup i CI różnicy (dane z config)
// Ryc. 3.2–3.3 (budowa CI dla średniej i różnicy średnich) rysuje fig_32.js,
// Ryc. 4.1 i 4.3 (proporcja i różnica proporcji) rysuje fig_41.js.
// Grubości jak w moście ggplot→ECharts: linewidth × 2 px; punkt size s → średnica ≈ 2.1·s.
(function () {
  "use strict";
  var C = window.lcCharts, SF = window.lcSF, D = window.lcDist;
  if (!C || !SF || !D) return;
  var fmt = SF.fmt;  // round(v, d) z R: bez końcowych zer
  var Z = D.qnorm(0.975);

  function cat(name, fallback) { return C.token("--upwr-cat-" + name, fallback); }
  // geom_errorbar(orientation = "y"): odcinek lo–hi i wąsy o łącznej wysokości cap.
  function errbar(lo, hi, y, cap, color, width, alpha) {
    var h = cap / 2;
    return { type: "segs", color: color, width: width, alpha: alpha, z: 4, items: [
      { from: [lo, y], to: [hi, y] }, { from: [lo, y - h], to: [lo, y + h] }, { from: [hi, y - h], to: [hi, y + h] }] };
  }

  // ---- Ryc. 3.1 ---------------------------------------------------------------
  function tnorm(v, P) {
    var df = 4, t = D.qt(0.975, df), z = Z, indygo = cat("indygo", "#3a5a8a");
    var xs = D.seq(-5, 5, 600);
    function curve(f) { return xs.map(function (x) { return [x, f(x)]; }); }
    function tail(sign) {
      return xs.filter(function (x) { return Math.abs(x) >= t && x * sign > 0; })
               .map(function (x) { return [x, D.dt(x, df)]; });
    }
    var ticks = [-t, -z, 0, z, t].map(function (b) { return Math.round(b * 100) / 100; });
    return {
      x: [-4.5, 4.5], y: [0, 0.43], xTicks: ticks, hideY: true, noYGrid: true,
      aria: "Gęstość rozkładu normalnego (przerywana) i rozkładu t o 4 stopniach swobody; t ma niższy " +
            "środek i cięższe ogony, więc jego wartość krytyczna 2.78 leży dalej niż 1.96",
      layers: [
        { type: "line", data: tail(1), color: indygo, width: 0, fill: indygo, fillAlpha: 0.3 },
        { type: "line", data: tail(-1), color: indygo, width: 0, fill: indygo, fillAlpha: 0.3 },
        { type: "line", data: curve(function (x) { return D.dnorm(x); }), color: P.secondary, width: 1.2, dash: [4, 4] },
        { type: "line", data: curve(function (x) { return D.dt(x, df); }), color: indygo, width: 2.2 },
        { type: "segment", from: [-z, 0], to: [-z, D.dnorm(z) + 0.03], color: P.secondary, width: 1 },
        { type: "segment", from: [z, 0], to: [z, D.dnorm(z) + 0.03], color: P.secondary, width: 1 },
        { type: "segment", from: [-t, 0], to: [-t, D.dt(t, df) + 0.06], color: cat("terakota", P.terakota), width: 1.8 },
        { type: "segment", from: [t, 0], to: [t, D.dt(t, df) + 0.06], color: cat("terakota", P.terakota), width: 1.8 },
        { type: "text", x: 0.55, y: D.dnorm(0) + 0.005, text: "N(0, 1)", color: P.secondary, align: "left", valign: "middle" },
        { type: "text", x: -1.5, y: D.dt(-1.5, df) + 0.06, text: "t, df = 4", color: indygo, align: "right", valign: "middle", bold: true }
      ]
    };
  }

  // ---- Przykłady A–C: górny wykres CI grup, dolny CI różnicy ------------------
  // config: g1_name, g2_name, unit, m1, m2, ci1, ci2, md, ci_d (policzone w R).
  function pad(lo, hi) { var e = (hi - lo) * 0.05; return [lo - e, hi + e]; }
  var comp = {
    draw: function (v, P, k, state) {
      var c = state.config, ci = cat("niebo", P.niebo), est = cat("bursztyn", P.bursztyn);
      var two = function (n) { return n.toFixed(2); };
      if (k === 0) {
        var lo = Math.max(c.ci1[0], c.ci2[0]), hi = Math.min(c.ci1[1], c.ci2[1]);
        var layers = [];
        if (lo <= hi) layers.push({ type: "band", lo: lo, hi: hi, color: "#f1c40f", alpha: 0.25 });
        [[c.m1, c.ci1, 2], [c.m2, c.ci2, 1]].forEach(function (g) {
          layers.push(errbar(g[1][0], g[1][1], g[2], 0.18, ci, 2.8),
                      { type: "point", x: g[0], y: g[2], color: est, size: 9 },
                      { type: "text", x: g[0], y: g[2] + 0.1, text: two(g[0]), color: est, bold: true });
        });
        var yl = {}; yl[2] = c.g1_name; yl[1] = c.g2_name;
        return { x: pad(Math.min(c.ci1[0], c.ci2[0]), Math.max(c.ci1[1], c.ci2[1])), y: [0.5, 2.5],
                 yLabels: yl, xName: c.unit, noYGrid: true, layers: layers,
                 aria: "95% CI grup: " + c.g1_name + " od " + two(c.ci1[0]) + " do " + two(c.ci1[1]) + ", " +
                       c.g2_name + " od " + two(c.ci2[0]) + " do " + two(c.ci2[1]) };
      }
      // Dolny panel: wiersz 1 na osi [0, 2] (w R: 1.5 na [0.5, 2.5]).
      var yd = {}; yd[1] = c.g1_name + " − " + c.g2_name;
      var wrzos = cat("wrzos", "#a07894");
      return { x: pad(Math.min(0, c.ci_d[0]), Math.max(0, c.ci_d[1])), y: [0, 2], yLabels: yd, noYGrid: true,
               xName: "różnica — " + c.unit,
               aria: "95% CI różnicy od " + two(c.ci_d[0]) + " do " + two(c.ci_d[1]) + "; linia przerywana w zerze",
               layers: [
                 { type: "vline", x: 0, color: wrzos, width: 2, dash: "dashed" },
                 { type: "text", x: 0, y: 1.8, text: "0", color: wrzos, bold: true, valign: "middle" },
                 errbar(c.ci_d[0], c.ci_d[1], 1, 0.18, ci, 2.8),
                 { type: "point", x: c.md, y: 1, color: est, size: 9 },
                 { type: "text", x: c.md, y: 1.1, text: two(c.md), color: est, bold: true }
               ] };
    }
  };

  // ---- Ryc. 4.1: proporcja krok po kroku --------------------------------------
  // Słupki TAK / NIE: TAK w roli „data” (niebo), NIE w roli „group” (bursztyn), czarna krawędź.
  SF.register({ "s04-tnorm": tnorm, "s04-comp": comp });
})();
