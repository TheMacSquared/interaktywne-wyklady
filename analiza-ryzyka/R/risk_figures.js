// Wspólne funkcje rycin analizy ryzyka liczonych w przeglądarce (window.lcRisk).
// Odpowiedniki funkcji z R/risk_math.R i R/risk_block.R; ładować po shared/lc_dist.js.
// Testy zgodności z R: tests/js/risk_figures.test.js.
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist;

  // risk_fmt_p(): trzy miejsca po kropce, a dla p < 0.01 tyle, żeby były dwie cyfry znaczące.
  function fmtP(x) {
    if (typeof x !== "number" || !isFinite(x)) return "—";
    var d = 3;
    if (x > 0 && x < 0.01) d = Math.max(3, Math.min(12, Math.ceil(-Math.log10(x)) + 2));
    return x.toFixed(d);
  }
  // Liczba całkowita ze spacją tysięcy: format(x, big.mark = " ").
  function big(n) { return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, " "); }
  // round() z R dla dokładnych połówek zaokrągla do parzystej; tu wystarcza zwykłe zaokrąglenie
  // liczby całkowitej z p · populacja (połówki praktycznie nie występują).
  function naturalFrequency(p, population) {
    if (typeof p !== "number" || !isFinite(p)) return "—";
    var pop = population || 1000;
    return "około " + Math.round(p * pop) + " na " + big(pop);
  }
  // risk_normal_exceedance(): P(X > próg) dla N(mean, sd).
  function normalExceedance(threshold, mean, sd) { return 1 - D.pnorm(threshold, mean, sd); }
  // risk_stress_strength_normal(): P(L > S) dla niezależnych (rho = 0) albo skorelowanych normalnych.
  function stressStrength(loadMean, loadSd, strengthMean, strengthSd, rho) {
    var r = rho || 0, dm = strengthMean - loadMean;
    var ds = Math.sqrt(strengthSd * strengthSd + loadSd * loadSd - 2 * r * strengthSd * loadSd);
    if (ds === 0) return dm < 0 ? 1 : 0;
    return D.pnorm(0, dm, ds);
  }
  // Krzywa gęstości normalnej jako [[x, y]] w k punktach od a do b.
  function normalCurve(mean, sd, a, b, k) {
    var out = [], n = k || 400;
    for (var i = 0; i < n; i++) { var x = a + (b - a) * i / (n - 1); out.push([x, D.dnorm(x, mean, sd)]); }
    return out;
  }
  // Część krzywej od progu w prawo (pole ogona), zaczynając dokładnie w progu.
  function tailCurve(curve, threshold, density) {
    var out = [[threshold, density(threshold)]];
    curve.forEach(function (p) { if (p[0] > threshold) out.push(p); });
    return out;
  }
  // as.character(round(x, d)) w R: bez końcowych zer.
  function rnd(x, d) {
    var s = x.toFixed(d);
    return s.indexOf(".") < 0 ? s : s.replace(/0+$/, "").replace(/\.$/, "");
  }
  function mean(x) { var s = 0; for (var i = 0; i < x.length; i++) s += x[i]; return s / x.length; }
  function sd(x) {
    var m = mean(x), v = 0;
    for (var i = 0; i < x.length; i++) v += (x[i] - m) * (x[i] - m);
    return Math.sqrt(v / (x.length - 1));
  }

  root.lcRisk = { fmtP: fmtP, big: big, naturalFrequency: naturalFrequency, normalExceedance: normalExceedance,
    stressStrength: stressStrength, normalCurve: normalCurve, tailCurve: tailCurve, rnd: rnd, mean: mean, sd: sd };
})();
