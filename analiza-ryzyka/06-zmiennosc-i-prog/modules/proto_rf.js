// Prototypy punktów stop wykładu 06, grupa rf (P14A, P14B). Ryciny lcSF liczone
// w przeglądarce; kontenery w R: lc_sf_figure() w stop_proto_rf.R.
//
//   z6_modele   — galeria „Ten sam środek, inny ogon” (P14A): trzy gęstości na jednej
//                 osi, widok „Cały rozkład” / „Ogon powyżej 88°C”, tabela pod wykresem.
//   z6_modele_b — galeria „Trzy modele obok siebie” (P14B): wykres k-ty = model k.
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
    },

    // P14B: jeden wykres na model; k to numer wykresu (0 normalny, 1 skośny, 2 ciężki ogon).
    z6_modele_b: {
      draw: function (v, P, k) {
        var m = MODELS[k];
        return {
          x: [68, 100], y: [0, 0.26], xLabelEvery: 4, alignLeft: true,
          xName: "Temperatura łożyska (°C)",
          layers: [
            { type: "line", data: curve(m.pdf, C1, C3), color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.2 },
            { type: "line", data: curve(m.pdf, C3, 100), color: P.accent, width: 0, fill: P.accent, fillAlpha: 0.7 },
            { type: "line", data: curve(m.pdf, 68, 100), color: P[m.key], width: 2.4 },
            { type: "vline", x: C1, color: P.ink, width: 1.4, dash: "dashed" },
            { type: "vline", x: C3, color: P.ink, width: 1.4, dash: "dashed" },
            // Napisy po lewej stronie linii 85°C, żeby żadna pionowa kreska ich nie przecinała.
            { type: "text", x: C1 - 0.5, y: "top", align: "right", mono: true, size: 0.85, color: P.ink,
              text: "P(T > 85) = " + pct85(m.tail(C1)) + "\nP(T > 91) = " + pct91(m.tail(C3)) }
          ],
          aria: "Rozkład " + m.name + " z zaznaczonym polem powyżej 85 i 91 stopni."
        };
      }
    }
  });
})();
