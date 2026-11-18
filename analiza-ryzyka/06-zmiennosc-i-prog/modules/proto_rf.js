// Prototypy punktów stop wykładu 06, grupa rf (P14A). Ryciny lcSF liczone
// w przeglądarce; kontenery w R: lc_sf_figure() w stop_proto_rf.R.
//
//   z6_modele   — galeria „Ten sam środek, inny ogon” (P14A): trzy gęstości na jednej
//                 osi, widok „Cały rozkład” / „Ogon powyżej 88°C”, tabela pod wykresem.
//
// Rozkłady: window.lcDist (dnorm, pnorm, dgamma, pgamma, dt). Dystrybuanty t₃ brak
// w lcDist, więc liczy ją t3cdf (wzór zamknięty dla ν = 3).
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist, Z = window.lcZ6;
  if (!C || !S || !D || !Z) return;
  var U = Z.util;

  // Model normalny N(82, 3); skośny: przesunięty gamma(3, √3) z przesunięciem 82 − 3√3;
  // ciężki ogon: 82 + √3·t₃.
  var MU = 82, SD = 3, SQ3 = Math.sqrt(3), SHIFT = MU - 3 * SQ3;
  var C1 = 85, C3 = 91;

  // Dystrybuanta t₃ (wzór zamknięty): F(t) = 1/2 + (1/π)·[(t/√3)/(1 + t²/3) + atan(t/√3)].
  function t3cdf(t) {
    var u = t / SQ3;
    return 0.5 + (u / (1 + t * t / 3) + Math.atan(u)) / Math.PI;
  }

  var MODELS = [
    { name: "Normalny", key: "secondary",
      pdf: function (x) { return D.dnorm(x, MU, SD); },
      tail: function (c) { return 1 - D.pnorm(c, MU, SD); } },
    { name: "Skośny (gamma)", key: "bursztyn",
      pdf: function (x) { return D.dgamma(x - SHIFT, 3, SQ3); },
      tail: function (c) { return D.pgamma(c - SHIFT, 3, SQ3, false); } },
    { name: "Ciężki ogon (t₃)", key: "niebo",
      pdf: function (x) { return D.dt((x - MU) / SQ3, 3) / SQ3; },
      tail: function (c) { return 1 - t3cdf((c - MU) / SQ3); } }
  ];

  // Krzywa gęstości [[x, f(x)], …] między a i b, krok 0.1°C.
  function curve(f, a, b) {
    var out = [], n = Math.round((b - a) / 0.1);
    for (var i = 0; i <= n; i++) { var x = a + (b - a) * i / n; out.push([x, f(x)]); }
    return out;
  }

  // Prawdopodobieństwa z kropką, bez końcowych zer. P(T > 85): trzy miejsca po przecinku
  // (0.159, 0.149, 0.091). P(T > 91): trzy cyfry znaczące (0.00135, 0.0118, 0.0069).
  function trimZeros(s) { return s.indexOf(".") < 0 ? s : s.replace(/0+$/, "").replace(/\.$/, ""); }
  function pct85(p) { return trimZeros(p.toFixed(3)); }
  function pct91(p) { return trimZeros(p.toPrecision(3)); }
  // Naturalna częstość na tysiąc: jedna cyfra po kropce poniżej 10.
  function per1000(p) {
    var k = 1000 * p;
    return k < 10 ? k.toFixed(1) : k.toFixed(0);
  }

  // Wiersze tabeli P14A: P(T > 85), P(T > 91), na tysiąc, stosunek do normalnego.
  function summary() {
    var base = MODELS[0].tail(C3);
    return MODELS.map(function (m) {
      var p91 = m.tail(C3);
      return { name: m.name, p85: m.tail(C1), p91: p91, ratio: p91 / base };
    });
  }

  S.register({
    // P14A: trzy krzywe na jednej osi. Widok „tail” powiększa ogon (88–100°C).
    z6_modele: {
      draw: function (v, P, k, state) {
        var tail = v === "tail", x = tail ? [88, 100] : [68, 100];
        var layers = MODELS.map(function (m) {
          return { type: "line", data: curve(m.pdf, x[0], x[1]), color: P[m.key], width: 2.4 };
        });
        if (!tail) layers.push({ type: "vline", x: C1, color: P.ink, width: 1.6, dash: "dashed" },
          { type: "text", x: C1, y: "top", text: "85°C · 1σ", color: P.ink, align: "left", mono: true });
        layers.push({ type: "vline", x: C3, color: P.ink, width: 1.6, dash: "dashed" },
          { type: "text", x: C3, y: "top", text: "91°C · 3σ", color: P.ink, align: "left", mono: true });
        return {
          x: x, y: [0, tail ? 0.02 : 0.26], xLabelEvery: tail ? 2 : 4,
          xName: "Temperatura łożyska (°C)", yName: "Gęstość",
          layers: layers,
          legend: MODELS.map(function (m) { return { text: m.name, color: P[m.key] }; }),
          aria: tail ? "Ogon rozkładów powyżej 88 stopni, w powiększeniu: trzy krzywe i próg 91 stopni."
                     : "Trzy rozkłady o średniej 82 i odchyleniu 3 stopnie, z progami 85 i 91 stopni."
        };
      },
      table: function () {
        var cols = [{ label: "Model", row: true }, { label: "P(T > 85°C)" }, { label: "P(T > 91°C)" },
                    { label: "Ponad 91°C na 1000" }, { label: "Razy więcej niż normalny" }];
        var rows = summary().map(function (s) {
          return [s.name, pct85(s.p85), pct91(s.p91), per1000(s.p91), s.ratio.toFixed(1) + "×"];
        });
        return S.table(cols, rows);
      }
    }
  });
})();

// Punkt stop P15B (18.11.2026): „Jak powstaje wykres kwantylowy” — scena „qq_budowa”.
// Kontener w R: z6p_scene_ui(..., "qq_budowa", z6rf_qq_cfg) w stop_proto_rf.R.
// Po lewej histogram pomiarów i krzywa modelu N(mu, sd) na wspólnej osi temperatury,
// po prawej wykres Q–Q. Kroki (Wstecz/Dalej): dane → kwantyl empiryczny → kwantyl
// teoretyczny → para na wykres → wszystkie pary. Przełącznik komory i suwak i.
(function () {
  "use strict";
  var Z = window.lcZ6;
  if (!Z) return;
  var U = Z.util, g = U.g;

  // Kwantyl N(0, 1) przez bisekcję dystrybuanty (dokładność daleko ponad potrzeby rysunku).
  function qnorm(p) {
    var lo = -8, hi = 8;
    for (var k = 0; k < 80; k++) { var m = (lo + hi) / 2; if (U.pnorm(m, 0, 1) < p) lo = m; else hi = m; }
    return (lo + hi) / 2;
  }
  var SUB = "₀₁₂₃₄₅₆₇₈₉";
  function sub(n) { return String(n).split("").map(function (c) { return SUB[+c]; }).join(""); }
  function sgn(x, d) { return (x >= 0 ? "+" : "−") + Math.abs(x).toFixed(d); }
  function ln(cls, x1, y1, x2, y2, style) {
    return '<line class="' + cls + '" x1="' + g(x1) + '" y1="' + g(y1) + '" x2="' + g(x2) + '" y2="' + g(y2) + '"' +
      (style ? ' style="' + style + '"' : "") + "/>";
  }
  function tx(x, y, s, extra) {
    return '<text x="' + g(x) + '" y="' + g(y) + '" ' + (extra || 'font-size="13"') + ">" + U.esc(s) + "</text>";
  }
  var EMP = "stroke:var(--upwr-accent);stroke-width:2", MOD = "stroke:var(--upwr-cat-indygo);stroke-width:2";
  var EMPD = EMP + ";stroke-dasharray:5 4", MODD = MOD + ";stroke-dasharray:5 4";

  Z.register({
    qq_budowa: function (cfg, ui) {
      var step = 0, N = cfg.steps.length, set = "a", root = ui.root, n = cfg.n, MU = cfg.mu, SD = cfg.sd;
      var sorted = {};
      Object.keys(cfg.data).forEach(function (k) { sorted[k] = cfg.data[k].slice().sort(function (a, b) { return a - b; }); });

      // Układ: lewa część (temperatura 72–100°C), prawa część (wykres Q–Q).
      var T0 = 72, T1 = 100, LX0 = 50, LX1 = 450;
      var HB = 165, HT = 40, DB = 340, DT = 215;
      var QX0 = 545, QX1 = 780, Z0 = -2.5, Z1 = 2.5, QB = 340, QT = 40, Q0 = 74, Q1 = 94;
      function cl(t, a, b) { return Math.min(Math.max(t, a), b); }
      function xT(t) { return LX0 + (cl(t, T0, T1) - T0) / (T1 - T0) * (LX1 - LX0); }
      function xZ(z) { return QX0 + (cl(z, Z0, Z1) - Z0) / (Z1 - Z0) * (QX1 - QX0); }
      function yQ(t) { return QB - (cl(t, Q0, Q1) - Q0) / (Q1 - Q0) * (QB - QT); }
      var yDmax = U.dnorm(MU, MU, SD) * 1.12;
      function yD(f) { return DB - f / yDmax * (DB - DT); }

      function rank() {
        var el = root.querySelector("#z6p15b_i");
        var i = el ? Math.round(+el.value) : cfg.i0;
        var out = root.querySelector('output[data-value="z6p15b_i"]');
        if (out) out.textContent = i;
        return cl(i, 1, n);
      }

      function hist(xs) {
        var counts = {}, top = 1, out = "";
        xs.forEach(function (t) { var b = Math.floor(t); counts[b] = (counts[b] || 0) + 1; top = Math.max(top, counts[b]); });
        var h = (HB - HT - 10) / Math.max(top, 6);
        Object.keys(counts).forEach(function (b) {
          b = +b;
          out += '<rect class="z6p19-bar" x="' + g(xT(b) + 0.5) + '" y="' + g(HB - counts[b] * h) + '" width="' +
            g(xT(b + 1) - xT(b) - 1) + '" height="' + g(counts[b] * h) + '"/>';
        });
        return out;
      }

      function render() {
        var xs = sorted[set], i = rank(), p = (i - 0.5) / n, xi = xs[i - 1], z = qnorm(p), q = MU + SD * z;
        var b = "";
        // Panele i osie.
        b += tx(LX0, HT - 14, "histogram pomiarów", 'class="z6p-subtle" font-size="12"');
        b += tx(LX0, DT - 14, "model N(" + MU + ", " + SD + ")", 'class="z6p-subtle" font-size="12"');
        b += tx(QX0, QT - 14, "wykres kwantylowy: pomiar (°C) względem z", 'class="z6p-subtle" font-size="12"');
        b += hist(xs) + ln("z6p-axis", LX0, HB, LX1, HB);
        b += '<path class="z6p-curve" d="' + U.normPath(MU, SD, xT, yD, T0, T1, false) + '"/>';
        b += U.axis(xT, DB, [72, 76, 80, 84, 88, 92, 96, 100], "temperatura (°C)");
        b += U.axis(xZ, QB, [-2, -1, 0, 1, 2], "kwantyl modelu (z)", ["−2", "−1", "0", "1", "2"]);
        b += U.axisY(yQ, QX0, [74, 78, 82, 86, 90, 94]);

        // Krok 2+: pomiary rosnąco jako kreski pod histogramem, i-ty wyróżniony.
        if (step >= 1) {
          xs.forEach(function (t, k) {
            b += ln("", xT(t), HB + 4, xT(t), HB + 16, k < i - 1 ? "stroke:var(--upwr-sage);stroke-width:2" :
              k === i - 1 ? "stroke:var(--upwr-accent);stroke-width:3" : "stroke:var(--upwr-ink-subtle);stroke-width:1.2");
          });
          b += ln("", xT(xi), HT - 12, xT(xi), HB + 18, EMP);
          b += tx(xT(xi) + 6, HT - 2, "x" + sub(i) + " = " + xi.toFixed(1) + "°C", 'class="z6p-mono z6p-bad" font-size="13" font-weight="700"');
        }
        // Krok 3+: to samo pole pod krzywą modelu, kwantyl teoretyczny.
        if (step >= 2) {
          var pts = [], m = 80;
          for (var k = 0; k <= m; k++) { var t = T0 + (q - T0) * k / m; pts.push(g(xT(t)) + "," + g(yD(U.dnorm(t, MU, SD)))); }
          b += '<path class="z6p-area-ok" d="M' + g(xT(T0)) + "," + g(DB) + " L" + pts.join(" L") + " L" + g(xT(q)) + "," + g(DB) + ' Z"/>';
          b += ln("", xT(q), DT, xT(q), DB, MOD);
          b += tx(xT(q) + 6, DT + 12, "z = " + z.toFixed(2), 'class="z6p-mono" font-size="13" font-weight="700" style="fill:var(--upwr-cat-indygo)"');
          b += tx(xT(q) - 8, DB - 8, "pole " + p.toFixed(3), 'class="z6p-mono z6p-subtle" font-size="12" text-anchor="end"');
        }
        // Krok 4+: prosta modelu i punkt (z, x_i) z przeniesieniem obu kwantyli.
        if (step >= 3) {
          b += ln("z6p-curve-ghost", xZ(Z0), yQ(MU + SD * Z0), xZ(Z1), yQ(MU + SD * Z1));
          if (step === 3) {
            b += ln("", xT(xi), HB, xZ(z), yQ(xi), EMPD);
            b += ln("", xT(q), DT + 22, xZ(z), QB, MODD);
          }
          b += ln("", xZ(z), yQ(xi), QX0, yQ(xi), EMPD) + ln("", xZ(z), yQ(xi), xZ(z), QB, MODD);
        }
        if (step >= 4) {
          xs.forEach(function (t, k) {
            var zz = qnorm((k + 0.5) / n);
            b += '<circle class="z6p-dot" cx="' + g(xZ(zz)) + '" cy="' + g(yQ(t)) + '" r="4"/>';
          });
        }
        if (step >= 3) b += '<circle class="z6p-dot-bad" cx="' + g(xZ(z)) + '" cy="' + g(yQ(xi)) + '" r="7"/>';

        var aria = cfg.sets[set] + ": " + cfg.steps[step].name + ". Pomiar nr " + i + " z " + n + ": " + xi.toFixed(1) +
          " stopni, kwantyl modelu z = " + z.toFixed(2) + ".";
        ui.set("head", U.head(cfg.steps[step].name, "Krok " + (step + 1) + " z " + N));
        ui.set("stage", U.svgWrap(800, 390, aria, b));

        var read = [
          "",
          "x" + sub(i) + " = " + xi.toFixed(1) + "°C · rząd (" + i + " − 0.5)/" + n + " = " + p.toFixed(3),
          "pole " + p.toFixed(3) + " → z = " + z.toFixed(2) + " → " + MU + " + " + SD + " · " + z.toFixed(2) + " = " + q.toFixed(1) + "°C",
          "punkt (" + z.toFixed(2) + "; " + xi.toFixed(1) + "°C) · od prostej " + sgn(xi - q, 1) + "°C",
          ""
        ][step];
        var text = cfg.steps[step].text + (step === N - 1 ? " " + cfg.end[set] : "");
        ui.set("body", '<p class="z6p-caption">' + U.esc(text) + "</p>" + (read ? '<p class="z6p-read">' + U.esc(read) + "</p>" : ""));
        ui.ends(step, N);
      }

      return {
        on: {
          prev: function () { step = Math.max(0, step - 1); },
          next: function () { step = Math.min(N - 1, step + 1); },
          set: function (btn) { set = U.press(btn); }
        },
        render: render
      };
    }
  });
})();
