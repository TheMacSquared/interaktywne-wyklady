// Ryciny wykładu 04 liczone w przeglądarce (silnik lcSF): sf_steps
//   s04-tnorm  — Ryc. 3.1: rozkład t (df = 4) a N(0, 1), wartości krytyczne
//   s04-comp   — Przykłady A–C (rozdz. 3): CI grup i CI różnicy (dane z config)
//   s04-pstep  — Ryc. 4.1: budowa przedziału dla proporcji krok po kroku
//   s04-pdstep — Ryc. 4.3: budowa przedziału dla różnicy proporcji
// Ryc. 3.2–3.3 (budowa CI dla średniej i różnicy średnich) rysuje ci_construction.js.
// Grubości jak w moście ggplot→ECharts: linewidth × 2 px; punkt size s → średnica ≈ 2.1·s.
(function () {
  "use strict";
  // Wzory w tekstach kroków jako HTML: wersja offline nie ma MathJax.
  var PH = "<i>p̂</i>", ME = "<i>ME</i> = <i>z</i>* · <i>SE</i> = 1.96 · ";
  function ph(k) { return "<i>p̂</i><sub>" + k + "</sub>"; }
  var C = window.lcCharts, SF = window.lcSF, D = window.lcDist;
  if (!C || !SF || !D) return;
  var fmt = SF.fmt;  // round(v, d) z R: bez końcowych zer
  var Z = D.qnorm(0.975);

  function cat(name, fallback) { return C.token("--upwr-cat-" + name, fallback); }
  // Role kroków jak STEP_ROLES w R/theme_upwr.R: nowy element w akcencie, znany w kolorze pomocniczym.
  function role(P, step, from) { return step === from ? P.accent : P.secondary; }
  // geom_errorbar(orientation = "y"): odcinek lo–hi i wąsy o łącznej wysokości cap.
  function errbar(lo, hi, y, cap, color, width, alpha) {
    var h = cap / 2;
    return { type: "segs", color: color, width: width, alpha: alpha, z: 4, items: [
      { from: [lo, y], to: [hi, y] }, { from: [lo, y - h], to: [lo, y + h] }, { from: [hi, y - h], to: [hi, y + h] }] };
  }
  function label(x, y, text, color, size) {
    return { type: "text", x: x, y: y, text: text, color: color, bold: true, valign: "middle", size: size || 1 };
  }
  function diamond(x, y, color) { return { type: "point", x: x, y: y, color: color, symbol: "diamond", size: 17 }; }

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
  function bars(P, groups, xmax) {
    var data = [], colors = [], texts = [], top = 0;
    groups.forEach(function (g) {
      [[g.at, g.n - g.x], [g.at + 1, g.x]].forEach(function (b, i) {
        data.push([b[0] - g.w / 2, b[0] + g.w / 2, b[1]]);
        colors.push(i ? cat("niebo", P.niebo) : cat("bursztyn", P.bursztyn));
        top = Math.max(top, b[1]);
      });
    });
    top *= groups.length > 1 ? 1.2 : 1.15;
    data.forEach(function (d) {
      texts.push({ type: "text", x: (d[0] + d[1]) / 2, y: d[2], text: String(d[2]), color: P.secondary, bold: true, mono: true, size: 1.1 });
    });
    var xl = {};
    groups.forEach(function (g) { xl[g.at] = "NIE"; xl[g.at + 1] = "TAK"; });
    return { x: [0.4, xmax], y: [0, top], yName: "Liczebność", xLabels: xl, noXGrid: true,
             layers: [{ type: "bins", data: data, colors: colors, alpha: 0.7, stroke: P.ink, lineWidth: 0.7 }].concat(texts) };
  }

  function propStats(x, n) {
    var p = x / n, se = Math.sqrt(p * (1 - p) / n);
    return { x: x, n: n, p: p, se: se, me: Z * se };
  }

  var pstep = {
    init: function (state) { this.act(state); },
    act: function (state) { state.x = D.r.binom(50, 0.6); },
    draw: function (v, P, k, state, vals, step) {
      var s = propStats(state.x, 50);
      if (k === 0) {
        var b = bars(P, [{ at: 1, n: s.n, x: s.x, w: 0.6 }], 2.6);
        b.aria = "Liczebności: " + s.x + " razy TAK, " + (s.n - s.x) + " razy NIE";
        return b;
      }
      var L = [], Y_EST = 0.30, Y_SE = 0.05, Y_CI = -0.25;
      if (step >= 2) {
        var c2 = role(P, step, 2);
        L.push({ type: "vline", x: s.p, color: P.secondary, width: 1.1, dash: [4, 4] },
               diamond(s.p, Y_EST, c2), label(s.p, Y_EST - 0.13, "p̂", c2, 1.1));
      }
      if (step >= 3) {
        var c3 = role(P, step, 3);
        L.push(errbar(s.p - s.se, s.p + s.se, Y_SE, 0.08, c3, step === 3 ? 3.6 : 2.2),
               label(s.p, Y_SE - 0.12, "± SE", c3));
      }
      if (step >= 4) {
        L.push(errbar(s.p - s.me, s.p + s.me, Y_CI, 0.12, P.accent, 4.4),
               label(s.p, Y_CI - 0.13, "95% CI", P.accent, 1.1));
      }
      return { x: [0, 1], y: [-0.6, 0.6], xLabelEvery: 0.2, xName: "Proporcja", hideY: true, noYGrid: true, layers: L,
               aria: "Oś proporcji; krok " + step + ". p̂ = " + fmt(s.p, 3) +
                     (step >= 4 ? ", 95% CI od " + fmt(s.p - s.me, 3) + " do " + fmt(s.p + s.me, 3) : "") };
    },
    text: function (state, v, vals, step) {
      var s = propStats(state.x, 50);
      if (step === 1) return "<p>" + s.n + " odpowiedzi: " + s.x + " razy TAK, " + (s.n - s.x) + " razy NIE.</p>";
      if (step === 2) return "<p>" + PH + " = <i>x</i>/<i>n</i> = " + s.x + "/" + s.n + " = " + fmt(s.p, 3) + "</p>";
      if (step === 3) return "<p><i>SE</i> = √(" + PH + "(1 − " + PH + ")/<i>n</i>) = √(" + fmt(s.p, 2) +
        " · " + fmt(1 - s.p, 2) + "/" + s.n + ") = " + fmt(s.se, 3) + "</p>" +
        "<p>Pasek ± SE to jeden błąd standardowy w każdą stronę. Przedział 95% sięga 1.96 SE.</p>";
      return "<p>" + ME + fmt(s.se, 3) + " = " + fmt(s.me, 3) + "</p>" +
             "<p>95% CI: <b>[" + fmt(s.p - s.me, 3) + " ; " + fmt(s.p + s.me, 3) + "]</b></p>";
    }
  };

  // ---- Ryc. 4.3: różnica proporcji --------------------------------------------
  function diffStats(x1, x2) {
    var n = 60, p1 = x1 / n, p2 = x2 / n, d = p1 - p2;
    var se = Math.sqrt(p1 * (1 - p1) / n + p2 * (1 - p2) / n);
    return { x1: x1, x2: x2, n1: n, n2: n, p1: p1, p2: p2, d: d, se: se, me: Z * se };
  }

  var pdstep = {
    init: function (state) { this.act(state); },
    act: function (state) { state.x1 = D.r.binom(60, 0.7); state.x2 = D.r.binom(60, 0.5); },
    draw: function (v, P, k, state, vals, step) {
      var s = diffStats(state.x1, state.x2);
      if (k === 0) {
        // facet_wrap(~grp): dwa panele obok siebie na jednej osi (NIE, TAK | NIE, TAK).
        var b = bars(P, [{ at: 1, n: s.n1, x: s.x1, w: 0.65 }, { at: 4, n: s.n2, x: s.x2, w: 0.65 }], 5.6);
        b.layers.unshift({ type: "vline", x: 3, color: P.rule, width: 1 });
        b.layers.push({ type: "text", x: 1.5, y: "top", text: "Grupa 1", color: P.ink, bold: true },
                      { type: "text", x: 4.5, y: "top", text: "Grupa 2", color: P.ink, bold: true });
        b.y[1] *= 1.08;  // miejsce na nagłówki paneli
        b.aria = "Liczebności TAK/NIE: grupa 1 " + s.x1 + "/" + (s.n1 - s.x1) + ", grupa 2 " + s.x2 + "/" + (s.n2 - s.x2);
        return b;
      }
      if (k === 1) {
        var L = [];
        if (step >= 2) {
          var c2 = role(P, step, 2);
          L.push(diamond(s.p1, 1, c2), diamond(s.p2, 2, c2),
                 label(s.p1, 1.45, "p̂₁ = " + fmt(s.p1, 3), c2), label(s.p2, 2.45, "p̂₂ = " + fmt(s.p2, 3), c2));
        }
        return { x: [0, 1], y: [0.4, 2.6], xLabelEvery: 0.2, yLabels: { 1: "Grupa 1", 2: "Grupa 2" }, xName: "Proporcja", noYGrid: true,
                 layers: L, aria: "Proporcje TAK w grupach" + (step >= 2 ? ": " + fmt(s.p1, 3) + " i " + fmt(s.p2, 3) : "") };
      }
      var lo = Math.min(-0.5, s.d - 1.3 * s.me), hi = Math.max(0.5, s.d + 1.3 * s.me), e = (hi - lo) * 0.08;
      var B = [{ type: "vline", x: 0, color: P.secondary, width: 1.1, dash: [4, 4] }];
      if (step >= 3) {
        var c3 = role(P, step, 3);
        B.push(diamond(s.d, 0, c3));
        if (step === 3) B.push(label(s.d, -0.22, "p̂₁ − p̂₂ = " + fmt(s.d, 3), c3));
      }
      if (step >= 4) {
        var c4 = role(P, step, 4);
        B.push(errbar(s.d - s.se, s.d + s.se, 0, 0.08, c4, step === 4 ? 3.6 : 2.2));
        if (step === 4) B.push(label(s.d, 0.17, "± SE = ±" + fmt(s.se, 3), c4, 0.95));
      }
      if (step >= 5) {
        B.push(errbar(s.d - s.me, s.d + s.me, 0, 0.14, P.accent, 4.4, 0.6),
               label(s.d, 0.20, "95% CI: [" + fmt(s.d - s.me, 3) + " ; " + fmt(s.d + s.me, 3) + "]", P.accent, 1.07));
      }
      return { x: [lo - e, hi + e], y: [-0.55, 0.55], xName: "Różnica proporcji  —  Grupa 1 − Grupa 2",
               hideY: true, noYGrid: true, layers: B,
               aria: "Skala różnicy; krok " + step + (step >= 3 ? ". Różnica " + fmt(s.d, 3) : "") +
                     (step >= 5 ? ", 95% CI od " + fmt(s.d - s.me, 3) + " do " + fmt(s.d + s.me, 3) : "") };
    },
    text: function (state, v, vals, step) {
      var s = diffStats(state.x1, state.x2);
      if (step === 1) return "<p>Grupa 1: " + s.x1 + " TAK i " + (s.n1 - s.x1) + " NIE. Grupa 2: " +
                             s.x2 + " TAK i " + (s.n2 - s.x2) + " NIE.</p>";
      if (step === 2) return "<p>" + ph(1) + " = " + s.x1 + "/" + s.n1 + " = " + fmt(s.p1, 3) +
                             "&emsp;&emsp;" + ph(2) + " = " + s.x2 + "/" + s.n2 + " = " + fmt(s.p2, 3) + "</p>";
      if (step === 3) return "<p>" + ph(1) + " − " + ph(2) + " = " + fmt(s.p1, 3) + " − " + fmt(s.p2, 3) + " = " +
                             fmt(s.d, 3) + "</p><p>Dolny panel ma skalę różnicy. Linia w zerze oznacza brak różnicy.</p>";
      if (step === 4) return "<p><i>SE</i> = √(" + ph(1) + "(1 − " + ph(1) + ")/<i>n</i><sub>1</sub> + " +
                             ph(2) + "(1 − " + ph(2) + ")/<i>n</i><sub>2</sub>) = " + fmt(s.se, 3) + "</p>";
      var lower = s.d - s.me, upper = s.d + s.me;
      var verdict = lower > 0 ? "Przedział nie obejmuje 0: w grupie 1 odsetek TAK jest wyższy, z 95% ufnością o co najmniej " + fmt(lower, 3) + "." :
        upper < 0 ? "Przedział nie obejmuje 0: w grupie 2 odsetek TAK jest wyższy, z 95% ufnością o co najmniej " + fmt(-upper, 3) + "." :
        "Przedział obejmuje 0: te próby nie rozstrzygają, która grupa ma wyższy odsetek.";
      return "<p>" + ME + fmt(s.se, 3) + " = " + fmt(s.me, 3) + "</p>" +
             "<p>95% CI: <b>[" + fmt(lower, 3) + " ; " + fmt(upper, 3) + "]</b></p><p>" + verdict + "</p>";
    }
  };

  SF.register({ "s04-tnorm": tnorm, "s04-comp": comp, "s04-pstep": pstep, "s04-pdstep": pdstep });
  window.lcStat04Steps = { propStats: propStats, diffStats: diffStats };
})();
