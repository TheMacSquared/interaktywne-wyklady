// Rozkłady prawdopodobieństwa w przeglądarce (window.lcDist): gęstości,
// dystrybuanty i kwantyle zgodne z R (d*, p*, q*) z dokładnością rzędu 1e-12
// (sprawdza tests/testthat/test-chart-numerics.R). Konwencje jak w R, poza
// dgeom/qgeom: numer próby z pierwszym sukcesem (1, 2, …), jak w wykładzie 03.
// Do symulacji: losowanie lcDist.rng() / lcDist.r i wygładzanie jądrowe lcDist.kde.
(function () {
  "use strict";
  var LOG_SQRT_2PI = 0.9189385332046728, SQRT2 = Math.SQRT2;

  // ln Γ(x): Lanczos (g = 7, 9 współczynników), dla x < 0.5 wzór odbicia.
  var LANCZOS = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
    771.32342877765313, -176.61502916214059, 12.507343278686905,
    -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
  function lgamma(x) {
    if (x < 0.5) return Math.log(Math.PI / Math.abs(Math.sin(Math.PI * x))) - lgamma(1 - x);
    x -= 1;
    var a = LANCZOS[0], t = x + 7.5;
    for (var i = 1; i < 9; i++) a += LANCZOS[i] / (x + i);
    return LOG_SQRT_2PI + (x + 0.5) * Math.log(t) - t + Math.log(a);
  }
  // ln k! dokładnie dla małych k (tablica), dalej przez lgamma.
  var LFACT = [0];
  for (var k = 1; k <= 170; k++) LFACT[k] = LFACT[k - 1] + Math.log(k);
  function lfact(k) { return k <= 170 ? LFACT[k] : lgamma(k + 1); }
  function lchoose(n, k) { return lfact(n) - lfact(k) - lfact(n - k); }

  // erfc z rozwinięciem W. J. Cody'ego (jak pnorm w R, błąd względny < 1e-15).
  function erfc(x) {
    var ax = Math.abs(x), r;
    if (ax < 0.5) return 1 - erf(x);
    if (ax < 4) {
      var p = [3.004592610201616e2, 4.519189537118729e2, 3.393208167343437e2, 1.529892850469404e2,
               4.316222722205674e1, 7.211758250883094, 5.641955174789740e-1, -1.368648573827167e-7];
      var q = [3.004592609569833e2, 7.909509253278980e2, 9.313540948506096e2, 6.389802644656312e2,
               2.775854447439876e2, 7.700015293522947e1, 1.278272731962942e1, 1];
      var num = p[7], den = q[7];
      for (var i = 6; i >= 0; i--) { num = num * ax + p[i]; den = den * ax + q[i]; }
      r = Math.exp(-ax * ax) * num / den;
    } else {
      var z = 1 / (ax * ax);
      var p2 = [-2.99610707703542174e-3, -4.94730910623250734e-2, -2.26956593539686930e-1,
                -2.78661308609647788e-1, -2.23192459734184686e-2];
      var q2 = [1.06209230528467918e-2, 1.91308926107829841e-1, 1.05167510706793207,
                1.98733201817135256, 1];
      var n2 = p2[4], d2 = q2[4];
      for (var j = 3; j >= 0; j--) { n2 = n2 * z + p2[j]; d2 = d2 * z + q2[j]; }
      r = Math.exp(-ax * ax) / ax * (0.5641895835477563 + z * n2 / d2);
    }
    return x < 0 ? 2 - r : r;
  }
  function erf(x) {
    if (Math.abs(x) >= 0.5) return 1 - erfc(x);
    var p = [3.16112374387056560, 1.13864154151050156e2, 3.77485237685302021e2, 3.20937758913846947e3, 1.85777706184603153e-1];
    var q = [2.36012909523441209e1, 2.44024637934444173e2, 1.28261652607737228e3, 2.84423683343917062e3];
    var z = x * x, num = p[4] * z, den = z;
    for (var i = 0; i < 3; i++) { num = (num + p[i]) * z; den = (den + q[i]) * z; }
    return x * (num + p[3]) / (den + q[3]);
  }

  function dnorm(x, mu, sd) {
    mu = mu || 0; sd = sd == null ? 1 : sd;
    var z = (x - mu) / sd;
    return Math.exp(-0.5 * z * z - LOG_SQRT_2PI) / sd;
  }
  function pnorm(x, mu, sd) {
    mu = mu || 0; sd = sd == null ? 1 : sd;
    return 0.5 * erfc(-(x - mu) / (sd * SQRT2));
  }
  // Kwantyl: przybliżenie Acklama i dwa kroki Halleya (do precyzji double).
  function qnorm(p, mu, sd) {
    mu = mu || 0; sd = sd == null ? 1 : sd;
    if (p <= 0) return -Infinity;
    if (p >= 1) return Infinity;
    var a = [-3.969683028665376e1, 2.209460984245205e2, -2.759285104469687e2, 1.383577518672690e2, -3.066479806614716e1, 2.506628277459239],
        b = [-5.447609879822406e1, 1.615858368580409e2, -1.556989798598866e2, 6.680131188771972e1, -1.328068155288572e1],
        c = [-7.784894002430293e-3, -3.223964580411365e-1, -2.400758277161838, -2.549732539343734, 4.374664141464968, 2.938163982698783],
        d = [7.784695709041462e-3, 3.224671290700398e-1, 2.445134137142996, 3.754408661907416];
    var x, q, r, lo = 0.02425;
    if (p < lo) {
      q = Math.sqrt(-2 * Math.log(p));
      x = (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    } else if (p <= 1 - lo) {
      q = p - 0.5; r = q * q;
      x = (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
    } else {
      q = Math.sqrt(-2 * Math.log(1 - p));
      x = -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    }
    // Krok Halleya na dolnym ogonie; górny z symetrii (1 − p jest dokładne dla p ≥ 0.5).
    var lower = p <= 0.5, pp = lower ? p : 1 - p;
    if (!lower) x = -x;
    for (var i = 0; i < 2; i++) {
      var u = (pnorm(x) - pp) * Math.sqrt(2 * Math.PI) * Math.exp(x * x / 2);
      x = x - u / (1 + x * u / 2);
    }
    if (!lower) x = -x;
    return mu + sd * x;
  }

  function dbinom(k, n, p) {
    if (k < 0 || k > n || k !== Math.floor(k)) return 0;
    if (p === 0) return k === 0 ? 1 : 0;
    if (p === 1) return k === n ? 1 : 0;
    return Math.exp(lchoose(n, k) + k * Math.log(p) + (n - k) * Math.log1p(-p));
  }
  function dpois(k, lambda) {
    if (k < 0 || k !== Math.floor(k)) return 0;
    return Math.exp(k * Math.log(lambda) - lambda - lfact(k));
  }
  function ppois(k, lambda) {
    var s = 0;
    for (var i = 0; i <= Math.floor(k); i++) s += dpois(i, lambda);
    return Math.min(1, s);
  }
  // Najmniejsze k z ppois(k) ≥ p (z tolerancją jak w R: 64 · ε).
  function qpois(p, lambda) {
    var s = 0, k = 0;
    for (;; k++) { s += dpois(k, lambda); if (s >= p * (1 - 64 * 2.220446e-16)) return k; }
  }
  // Numer próby z pierwszym sukcesem: P(X = k) = (1 − p)^(k − 1) p, k = 1, 2, …
  function dgeom(k, p) {
    if (k < 1 || k !== Math.floor(k)) return 0;
    return p * Math.pow(1 - p, k - 1);
  }
  // Odpowiednik qgeom(q, p) + 1 z R.
  function qgeom(q, p) {
    return Math.max(0, Math.ceil(Math.log1p(-q) / Math.log1p(-p) - 1 - 1e-12)) + 1;
  }
  function dunif(x, a, b) { return x >= a && x <= b ? 1 / (b - a) : 0; }
  function dexp(x, rate) { return x < 0 ? 0 : rate * Math.exp(-rate * x); }
  function qexp(p, rate) { return -Math.log1p(-p) / rate; }

  function dt(x, df) {
    return Math.exp(lgamma((df + 1) / 2) - lgamma(df / 2) - 0.5 * Math.log(df * Math.PI) -
                    (df + 1) / 2 * Math.log1p(x * x / df));
  }
  function dchisq(x, df) {
    if (x < 0) return 0;
    if (x === 0) return df < 2 ? Infinity : df === 2 ? 0.5 : 0;
    var k = df / 2;
    return Math.exp((k - 1) * Math.log(x) - x / 2 - k * Math.LN2 - lgamma(k));
  }
  // Regularyzowana niekompletna gamma P(a, x): szereg dla x < a + 1, dalej ułamek łańcuchowy.
  function pgammaReg(a, x) {
    if (x <= 0) return 0;
    var lg = lgamma(a), i;
    if (x < a + 1) {
      var ap = a, sum = 1 / a, del = sum;
      for (i = 0; i < 1000; i++) { ap += 1; del *= x / ap; sum += del; if (Math.abs(del) < Math.abs(sum) * 1e-16) break; }
      return sum * Math.exp(-x + a * Math.log(x) - lg);
    }
    var b = x + 1 - a, c = 1 / 1e-300, d = 1 / b, h = d;
    for (i = 1; i < 1000; i++) {
      var an = -i * (i - a); b += 2;
      d = an * d + b; if (Math.abs(d) < 1e-300) d = 1e-300;
      c = b + an / c; if (Math.abs(c) < 1e-300) c = 1e-300;
      d = 1 / d; var del2 = d * c; h *= del2;
      if (Math.abs(del2 - 1) < 1e-16) break;
    }
    return 1 - Math.exp(-x + a * Math.log(x) - lg) * h;
  }
  function pchisq(x, df) { return pgammaReg(df / 2, x / 2); }
  // pgamma(x, shape, scale = s, lower.tail) z R.
  function pgamma(x, shape, scale, lower) {
    var p = x <= 0 ? 0 : pgammaReg(shape, x / scale);
    return lower === false ? 1 - p : p;
  }
  // Kwantyl przez bisekcję na monotonicznej dystrybuancie.
  function invert(cdf, p, lo, hi) {
    while (cdf(hi) < p) hi *= 2;
    for (var i = 0; i < 300 && hi - lo > 2e-16 * hi; i++) {
      var mid = (lo + hi) / 2;
      if (cdf(mid) < p) lo = mid; else hi = mid;
    }
    return (lo + hi) / 2;
  }
  function qchisq(p, df) { return invert(function (x) { return pchisq(x, df); }, p, 0, Math.max(1, df * 2)); }
  function dlnorm(x, mu, sigma) {
    if (x <= 0) return 0;
    var z = (Math.log(x) - mu) / sigma;
    return Math.exp(-0.5 * z * z - LOG_SQRT_2PI) / (sigma * x);
  }
  function qlnorm(p, mu, sigma) { return Math.exp(qnorm(p, mu, sigma)); }

  // seq(from, to, length.out = n) z R.
  function seq(from, to, n) {
    var out = new Array(n), by = (to - from) / (n - 1);
    for (var i = 0; i < n; i++) out[i] = from + i * by;
    out[n - 1] = to;
    return out;
  }

  // dgamma(x, shape, scale = s) i dbeta(x, a, b) z R.
  function dgamma(x, shape, scale) {
    if (x < 0) return 0;
    if (x === 0) return shape < 1 ? Infinity : shape === 1 ? 1 / scale : 0;
    return Math.exp((shape - 1) * Math.log(x) - x / scale - lgamma(shape) - shape * Math.log(scale));
  }
  function dbeta(x, a, b) {
    if (x < 0 || x > 1) return 0;
    if (x === 0 || x === 1) return Infinity;
    return Math.exp((a - 1) * Math.log(x) + (b - 1) * Math.log1p(-x) + lgamma(a + b) - lgamma(a) - lgamma(b));
  }

  // ---- Losowanie (symulacje dawniej liczone w R) -------------------------------
  // rng() losuje przez Math.random, rng(ziarno) powtarzalnie (mulberry32).
  // Sekwencje nie są identyczne z R; zgodne są rozkłady.
  function rng(seed) {
    var state = seed >>> 0, spare = null;
    var u = seed == null ? function () { return Math.random(); } : function () {
      state = (state + 0x6D2B79F5) >>> 0;
      var t = state;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
    function norm(mu, sd) {
      var z;
      if (spare !== null) { z = spare; spare = null; } else {
        var a = 1 - u(), b = u(), r = Math.sqrt(-2 * Math.log(a));
        z = r * Math.cos(2 * Math.PI * b); spare = r * Math.sin(2 * Math.PI * b);
      }
      return (mu || 0) + (sd == null ? 1 : sd) * z;
    }
    // Marsaglia–Tsang; kształt < 1 przez wzmocnienie.
    function gamma(shape, scale) {
      if (shape < 1) return gamma(shape + 1, scale) * Math.pow(u(), 1 / shape);
      var d = shape - 1 / 3, c = 1 / Math.sqrt(9 * d);
      for (;;) {
        var z = norm(), v = 1 + c * z;
        if (v <= 0) continue;
        v = v * v * v;
        var w = u();
        if (w < 1 - 0.0331 * z * z * z * z || Math.log(w) < 0.5 * z * z + d * (1 - v + Math.log(v))) return d * v * scale;
      }
    }
    function pois(lambda) {
      // Knuth dla małych λ; dla dużych suma niezależnych Poissonów o λ ≤ 30.
      if (lambda > 30) { var s = 0, rest = lambda; while (rest > 0) { s += pois(Math.min(30, rest)); rest -= 30; } return s; }
      var L = Math.exp(-lambda), k = 0, p = 1;
      do { k++; p *= u(); } while (p > L);
      return k - 1;
    }
    function binom(n, p) { var k = 0; for (var i = 0; i < n; i++) if (u() < p) k++; return k; }
    // Indeks od 1 według wag (sample(1:k, prob = w) w R).
    function choose(w) {
      var total = 0, i; for (i = 0; i < w.length; i++) total += w[i];
      var r = u() * total;
      for (i = 0; i < w.length; i++) { r -= w[i]; if (r < 0) return i + 1; }
      return w.length;
    }
    function many(k, f) { var out = new Array(k); for (var i = 0; i < k; i++) out[i] = f(); return out; }
    return {
      u: u, norm: norm, gamma: gamma, pois: pois, binom: binom, choose: choose, many: many,
      unif: function (a, b) { return a + (b - a) * u(); },
      exp: function (rate) { return -Math.log(1 - u()) / rate; },
      beta: function (a, b) { var x = gamma(a, 1); return x / (x + gamma(b, 1)); },
      int: function (k) { return 1 + Math.floor(u() * k); }   // sample.int(k, 1)
    };
  }

  // ---- Wygładzanie jądrowe jak geom_density (bw.nrd0, jądro gaussowskie) --------
  function quantile7(s, p) {
    var h = (s.length - 1) * p, j = Math.floor(h);
    return j + 1 < s.length ? s[j] + (h - j) * (s[j + 1] - s[j]) : s[j];
  }
  function bwNrd0(x) {
    var n = x.length, m = 0, v = 0, i;
    for (i = 0; i < n; i++) m += x[i];
    m /= n;
    for (i = 0; i < n; i++) v += (x[i] - m) * (x[i] - m);
    var s = x.slice().sort(function (a, b) { return a - b; });
    var hi = Math.sqrt(v / (n - 1)), lo = Math.min(hi, (quantile7(s, 0.75) - quantile7(s, 0.25)) / 1.34);
    if (!(lo > 0)) lo = hi || Math.abs(x[0]) || 1;
    return 0.9 * lo * Math.pow(n, -0.2);
  }
  // Gęstość w k punktach od lo do hi. bounds [a, b]: odbicie jądra przy brzegach
  // (geom_density(bounds = …)); poza [a, b] gęstość 0. Duże próby liczymy na
  // siatce 1024 przedziałów (jak density() w R), co wystarcza na wykresie.
  function kde(x, lo, hi, k, bw, bounds) {
    var h = bw || bwNrd0(x), n = x.length, a = bounds ? bounds[0] : -Infinity, b = bounds ? bounds[1] : Infinity;
    var pts = x, wts = null, i, j;
    if (n > 2000) {
      var mn = Infinity, mx = -Infinity;
      for (i = 0; i < n; i++) { if (x[i] < mn) mn = x[i]; if (x[i] > mx) mx = x[i]; }
      var G = 1024, step = (mx - mn) / G || 1, cnt = new Array(G + 1).fill(0);
      for (i = 0; i < n; i++) cnt[Math.round((x[i] - mn) / step)]++;
      pts = []; wts = [];
      for (i = 0; i <= G; i++) if (cnt[i]) { pts.push(mn + i * step); wts.push(cnt[i]); }
    }
    var c = 1 / (n * h * Math.sqrt(2 * Math.PI)), out = [];
    function add(t, p) { var z = (t - p) / h; return Math.exp(-0.5 * z * z); }
    for (i = 0; i < k; i++) {
      var t = lo + (hi - lo) * i / (k - 1), s = 0;
      if (t >= a && t <= b) {
        for (j = 0; j < pts.length; j++) {
          var w = wts ? wts[j] : 1, p = pts[j], g = add(t, p);
          if (isFinite(a)) g += add(t, 2 * a - p);
          if (isFinite(b)) g += add(t, 2 * b - p);
          s += w * g;
        }
      }
      out.push([t, s * c]);
    }
    return out;
  }

  window.lcDist = {
    lgamma: lgamma, lchoose: lchoose, erf: erf, erfc: erfc,
    dnorm: dnorm, pnorm: pnorm, qnorm: qnorm,
    dbinom: dbinom, dpois: dpois, ppois: ppois, qpois: qpois,
    dgeom: dgeom, qgeom: qgeom, dunif: dunif, dexp: dexp, qexp: qexp,
    dt: dt, dchisq: dchisq, pchisq: pchisq, qchisq: qchisq,
    dlnorm: dlnorm, qlnorm: qlnorm, dgamma: dgamma, pgamma: pgamma, dbeta: dbeta, seq: seq,
    rng: rng, r: rng(), bwNrd0: bwNrd0, kde: kde
  };
})();
