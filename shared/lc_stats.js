// Testy i statystyki liczone w przeglądarce (window.lcStats) — odpowiedniki funkcji R
// używanych dawniej na serwerze. Ładować po shared/lc_dist.js.
// Wzorcem są wyniki R: testy w statystyka/tests/js/lc_stats.test.js.
//
//   mean, variance, sd, median, quantile (typ 7), ppoints
//   qqNormal(x)            — punkty stat_qq() i prosta stat_qq_line() (przez kwartyle)
//   shapiroWilk(x)         — shapiro.test(): {W, p}
//   levene(groups)         — rstatix::levene_test() (środek: mediana): {F, df1, df2, p}
//   bartlett(groups)       — bartlett.test(): {K2, df, p}
//   tTest(a, b, varEqual)  — t.test() dwóch prób: {t, df, p}
//   chisq2x2(tab)          — chisq.test(correct = FALSE) dla tabeli 2×2: {X2, p}
//   fisher2x2(tab)         — fisher.test() dwustronny: p
//   pf(x, d1, d2)          — dystrybuanta rozkładu F
//   fmtPValue(p), round(x, d) — format_p_value() i as.character(round(x, d))
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist;
  if (!D) return;

  function mean(x) { var s = 0; for (var i = 0; i < x.length; i++) s += x[i]; return s / x.length; }
  function variance(x) {
    var m = mean(x), v = 0;
    for (var i = 0; i < x.length; i++) v += (x[i] - m) * (x[i] - m);
    return v / (x.length - 1);
  }
  function sd(x) { return Math.sqrt(variance(x)); }
  function sorted(x) { return x.slice().sort(function (a, b) { return a - b; }); }
  // quantile(x, p, type = 7) — domyślny w R.
  function quantile(x, p) {
    var s = sorted(x), h = (s.length - 1) * p, lo = Math.floor(h), hi = Math.ceil(h);
    return s[lo] + (h - lo) * (s[hi] - s[lo]);
  }
  function median(x) { return quantile(x, 0.5); }
  // ppoints(n): (i − a) / (n + 1 − 2a), a = 3/8 dla n ≤ 10, inaczej 1/2.
  function ppoints(n) {
    var a = n <= 10 ? 3 / 8 : 1 / 2, out = [];
    for (var i = 1; i <= n; i++) out.push((i - a) / (n + 1 - 2 * a));
    return out;
  }
  // Wykres kwantylowy: punkty (kwantyl teoretyczny, obserwacja) i prosta przez kwartyle.
  function qqNormal(x) {
    var s = sorted(x), q = ppoints(s.length).map(function (p) { return D.qnorm(p); });
    var qy = [quantile(x, 0.25), quantile(x, 0.75)], qx = [D.qnorm(0.25), D.qnorm(0.75)];
    var slope = (qy[1] - qy[0]) / (qx[1] - qx[0]);
    return { points: s.map(function (v, i) { return [q[i], v]; }), intercept: qy[0] - slope * qx[0], slope: slope };
  }

  // Regularyzowana niekompletna beta I_x(a, b): ułamek łańcuchowy (Lentz).
  function betaReg(x, a, b) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    var front = Math.exp(D.lgamma(a + b) - D.lgamma(a) - D.lgamma(b) + a * Math.log(x) + b * Math.log1p(-x));
    if (x > (a + 1) / (a + b + 2)) return 1 - betaReg(1 - x, b, a);
    var tiny = 1e-300, c = 1, d = 1 - (a + b) * x / (a + 1);
    if (Math.abs(d) < tiny) d = tiny;
    d = 1 / d;
    var h = d;
    for (var m = 1; m <= 500; m++) {
      var m2 = 2 * m, aa = m * (b - m) * x / ((a + m2 - 1) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < tiny) d = tiny;
      c = 1 + aa / c; if (Math.abs(c) < tiny) c = tiny;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (a + b + m) * x / ((a + m2) * (a + m2 + 1));
      d = 1 + aa * d; if (Math.abs(d) < tiny) d = tiny;
      c = 1 + aa / c; if (Math.abs(c) < tiny) c = tiny;
      d = 1 / d;
      var del = d * c;
      h *= del;
      if (Math.abs(del - 1) < 1e-15) break;
    }
    return front * h / a;
  }
  // pf(x, d1, d2, lower.tail): górny ogon liczony wprost, bez odejmowania od 1.
  function pf(x, d1, d2, lower) {
    if (!(x > 0)) return lower === false ? 1 : 0;
    var upper = betaReg(d2 / (d2 + d1 * x), d2 / 2, d1 / 2);
    return lower === false ? upper : 1 - upper;
  }
  // 2 · P(T > |t|) dla rozkładu t o df stopniach swobody (df nie musi być całkowite).
  function pt2(t, df) { return betaReg(df / (df + t * t), df / 2, 0.5); }

  // Shapiro–Wilk: algorytm AS R94 (Royston 1995), jak swilk.c w R. n od 3 do 5000.
  function poly(cc, x) {
    var n = cc.length, ret = cc[0];
    if (n > 1) {
      var p = x * cc[n - 1];
      for (var j = n - 2; j > 0; j--) p = (p + cc[j]) * x;
      ret += p;
    }
    return ret;
  }
  function sign(v) { return v > 0 ? 1 : v < 0 ? -1 : 0; }
  function shapiroWilk(values) {
    var x = sorted(values), n = x.length, nn2 = Math.floor(n / 2), a = new Array(nn2 + 1), i, j;
    if (n < 3) return { W: NaN, p: NaN };
    var small = 1e-19, an = n;
    var c1 = [0, 0.221157, -0.147981, -2.07119, 4.434685, -2.706056];
    var c2 = [0, 0.042981, -0.293762, -1.752461, 5.682633, -3.582633];
    var c3 = [0.544, -0.39978, 0.025054, -6.714e-4], c4 = [1.3822, -0.77857, 0.062767, -0.0020322];
    var c5 = [-1.5861, -0.31082, -0.083751, 0.0038915], c6 = [-0.4803, -0.082676, 0.0030302], g = [-2.273, 0.459];
    if (n === 3) {
      a[1] = Math.SQRT1_2;
    } else {
      var an25 = an + 0.25, summ2 = 0;
      for (i = 1; i <= nn2; i++) { a[i] = D.qnorm((i - 0.375) / an25); summ2 += a[i] * a[i]; }
      summ2 *= 2;
      var ssumm2 = Math.sqrt(summ2), rsn = 1 / Math.sqrt(an), a1 = poly(c1, rsn) - a[1] / ssumm2, i1, fac;
      if (n > 5) {
        i1 = 3;
        var a2 = -a[2] / ssumm2 + poly(c2, rsn);
        fac = Math.sqrt((summ2 - 2 * (a[1] * a[1]) - 2 * (a[2] * a[2])) / (1 - 2 * (a1 * a1) - 2 * (a2 * a2)));
        a[2] = a2;
      } else {
        i1 = 2;
        fac = Math.sqrt((summ2 - 2 * (a[1] * a[1])) / (1 - 2 * (a1 * a1)));
      }
      a[1] = a1;
      for (i = i1; i <= nn2; i++) a[i] /= -fac;
    }
    var range = x[n - 1] - x[0];
    if (range < small) return { W: NaN, p: NaN };
    var sx = x[0] / range, sa = -a[1];
    for (i = 1, j = n - 1; i < n; j--) {
      sx += x[i] / range;
      i++;
      if (i !== j) sa += sign(i - j) * a[Math.min(i, j)];
    }
    sa /= n; sx /= n;
    var ssa = 0, ssx = 0, sax = 0;
    for (i = 0, j = n - 1; i < n; i++, j--) {
      var asa = i !== j ? sign(i - j) * a[1 + Math.min(i, j)] - sa : -sa, xsx = x[i] / range - sx;
      ssa += asa * asa; ssx += xsx * xsx; sax += asa * xsx;
    }
    // w1 = 1 − W liczone tak, żeby nie tracić dokładności dla W bliskiego 1.
    var ssassx = Math.sqrt(ssa * ssx), w1 = (ssassx - sax) * (ssassx + sax) / (ssa * ssx), w = 1 - w1, pw;
    if (n === 3) {
      pw = 1.90985931710274 * (Math.asin(Math.sqrt(w)) - 1.04719755119660);
      return { W: w, p: Math.max(0, pw) };
    }
    var y = Math.log(w1), xx = Math.log(an), m, s;
    if (n <= 11) {
      var gamma = poly(g, an);
      if (y >= gamma) return { W: w, p: 1e-99 };
      y = -Math.log(gamma - y);
      m = poly(c3, an); s = Math.exp(poly(c4, an));
    } else {
      m = poly(c5, xx); s = Math.exp(poly(c6, xx));
    }
    return { W: w, p: 1 - D.pnorm(y, m, s) };
  }

  // Jednoczynnikowa analiza wariancji: F i stopnie swobody.
  function anovaF(groups) {
    var all = [].concat.apply([], groups), grand = mean(all), k = groups.length, n = all.length, between = 0, within = 0;
    groups.forEach(function (gr) {
      var m = mean(gr);
      between += gr.length * (m - grand) * (m - grand);
      gr.forEach(function (v) { within += (v - m) * (v - m); });
    });
    var df1 = k - 1, df2 = n - k, f = (between / df1) / (within / df2);
    return { F: f, df1: df1, df2: df2, p: pf(f, df1, df2, false) };
  }
  // Test Levene'a z medianą jako środkiem (Brown–Forsythe), jak car::leveneTest i rstatix.
  function levene(groups) {
    return anovaF(groups.map(function (gr) {
      var med = median(gr);
      return gr.map(function (v) { return Math.abs(v - med); });
    }));
  }
  function bartlett(groups) {
    var k = groups.length, n = groups.map(function (gr) { return gr.length; }), total = 0, pooled = 0, logs = 0, inv = 0;
    groups.forEach(function (gr, i) {
      var v = variance(gr);
      total += n[i] - 1; pooled += (n[i] - 1) * v; logs += (n[i] - 1) * Math.log(v); inv += 1 / (n[i] - 1);
    });
    var stat = (total * Math.log(pooled / total) - logs) / (1 + (inv - 1 / total) / (3 * (k - 1)));
    return { K2: stat, df: k - 1, p: 1 - D.pchisq(stat, k - 1) };
  }
  // Test t dwóch prób niezależnych: Studenta (varEqual) albo Welcha.
  function tTest(a, b, varEqual) {
    var na = a.length, nb = b.length, va = variance(a), vb = variance(b), diff = mean(a) - mean(b), df, se;
    if (varEqual) {
      df = na + nb - 2;
      se = Math.sqrt(((na - 1) * va + (nb - 1) * vb) / df * (1 / na + 1 / nb));
    } else {
      var sa = va / na, sb = vb / nb;
      se = Math.sqrt(sa + sb);
      df = (sa + sb) * (sa + sb) / (sa * sa / (na - 1) + sb * sb / (nb - 1));
    }
    var t = diff / se;
    return { t: t, df: df, p: pt2(t, df) };
  }
  // Tabela 2×2 [[a, b], [c, d]]: χ² Pearsona bez poprawki; NaN, gdy któraś suma brzegowa jest zerem.
  function chisq2x2(tab) {
    var a = tab[0][0], b = tab[0][1], c = tab[1][0], d = tab[1][1], n = a + b + c + d;
    var r = [a + b, c + d], k = [a + c, b + d], stat = 0;
    [[a, 0, 0], [b, 0, 1], [c, 1, 0], [d, 1, 1]].forEach(function (cell) {
      var e = r[cell[1]] * k[cell[2]] / n;
      stat += (cell[0] - e) * (cell[0] - e) / e;
    });
    return { X2: stat, p: isFinite(stat) ? 1 - D.pchisq(stat, 1) : NaN };
  }
  // Test dokładny Fishera, dwustronny: suma prawdopodobieństw tabel nie bardziej prawdopodobnych od obserwowanej.
  function fisher2x2(tab) {
    var a = tab[0][0], b = tab[0][1], c = tab[1][0], d = tab[1][1];
    var m = a + b, n = c + d, k = a + c, lo = Math.max(0, k - n), hi = Math.min(k, m), logs = [], x, top = -Infinity;
    for (x = lo; x <= hi; x++) { var l = D.lchoose(m, x) + D.lchoose(n, k - x); logs.push(l); if (l > top) top = l; }
    var dens = logs.map(function (l) { return Math.exp(l - top); }), sum = dens.reduce(function (s, v) { return s + v; }, 0);
    var obs = dens[a - lo] / sum, p = 0;
    dens.forEach(function (v) { if (v / sum <= obs * (1 + 1e-7)) p += v / sum; });
    return Math.min(1, p);
  }

  // format_p_value() z shared/shared.R.
  function fmtPValue(p) {
    if (typeof p !== "number" || isNaN(p)) return "NA";
    if (p < 0.0001) return "<0.0001";
    var r = Number(p.toPrecision(2));
    if (r >= 1) return ">0.99";
    var s = String(r);
    return s.indexOf(".") < 0 ? s + ".0" : s;
  }
  // as.character(round(x, d)): bez końcowych zer.
  function round(x, d) { return String(Number(x.toFixed(d))); }

  root.lcStats = { mean: mean, variance: variance, sd: sd, median: median, quantile: quantile, ppoints: ppoints,
    qqNormal: qqNormal, betaReg: betaReg, pf: pf, shapiroWilk: shapiroWilk, anovaF: anovaF, levene: levene,
    bartlett: bartlett, tTest: tTest, chisq2x2: chisq2x2, fisher2x2: fisher2x2, fmtPValue: fmtPValue, round: round };
})();
