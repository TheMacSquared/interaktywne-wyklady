// Wspólne funkcje rycin analizy ryzyka liczonych w przeglądarce (window.lcRisk).
// Odpowiedniki funkcji z R/risk_math.R i R/risk_block.R; ładować po shared/lc_dist.js.
// Testy zgodności z R: tests/js/risk_figures.test.js.
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist;

  // sprintf("%.df", x) z R dla x ≥ 0. toFixed() liczy na dokładnej wartości liczby tak jak C,
  // ale dokładną połówkę (np. 0.0625 przy trzech miejscach) zaokrągla w górę, a C do parzystej
  // cyfry — wtedy bierzemy wynik z parzystą ostatnią cyfrą.
  function fixed(x, d) {
    var up = x.toFixed(d);
    if (!(x >= 0)) return up;
    var full = x.toFixed(Math.min(100, d + 60)), cut = full.indexOf(".") + 1 + d;
    if (!/^50*$/.test(full.slice(cut))) return up;
    var down = d === 0 ? full.slice(0, cut - 1) : full.slice(0, cut);
    return Number(up.charAt(up.length - 1)) % 2 === 0 ? up : down;
  }
  // risk_fmt_p(): trzy miejsca po kropce, a dla p < 0.01 tyle, żeby były dwie cyfry znaczące.
  // fmtPd(x, digits): to samo z inną najmniejszą liczbą miejsc (risk_fmt_p(x, 6)).
  // fmtP ma jeden argument, żeby działało w [..].map(fmtP) (map podaje też indeks).
  function fmtPd(x, digits) {
    if (typeof x !== "number" || !isFinite(x)) return "—";
    var d = digits;
    if (x > 0 && x < 0.01) d = Math.max(digits, Math.min(12, Math.ceil(-Math.log10(x)) + 2));
    return fixed(x, d);
  }
  function fmtP(x) { return fmtPd(x, 3); }
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

  // risk_weibull(): niezawodność i hazard Weibulla (kształt, skala) w chwili t.
  function weibull(t, shape, scale) {
    return { reliability: Math.exp(-Math.pow(t / scale, shape)),
             hazard: (shape / scale) * Math.pow(t / scale, shape - 1) };
  }
  // Krzywa [[x, f(x)]] w k punktach od a do b: jak seq(a, b, length.out = k) w R.
  function curve(f, a, b, k) {
    var out = [], n = k || 400;
    for (var i = 0; i < n; i++) { var x = a + (b - a) * i / (n - 1); out.push([x, f(x)]); }
    return out;
  }
  // upwr_cat_n(n): pierwsze n barw palety kategorycznej z tokenów motywu
  // (rezerwowe wartości jak w shared/palette.R).
  var CAT = [["grafit", "#2d3b3d"], ["bursztyn", "#c08540"], ["niebo", "#6a9dc4"], ["szalwia", "#4a8a6a"],
             ["kurkuma", "#cbb858"], ["indygo", "#3a5a8a"], ["terakota", "#b25838"], ["wrzos", "#a07894"]];
  function cat(n) {
    var C = root.lcCharts;
    return CAT.slice(0, n).map(function (c) { return C && C.token ? C.token("--upwr-cat-" + c[0], c[1]) : c[1]; });
  }

  root.lcRisk = { weibull: weibull, curve: curve, cat: cat, fixed: fixed, fmtP: fmtP, fmtPd: fmtPd, big: big, naturalFrequency: naturalFrequency, normalExceedance: normalExceedance,
    stressStrength: stressStrength, normalCurve: normalCurve, tailCurve: tailCurve, rnd: rnd, mean: mean, sd: sd };
})();
