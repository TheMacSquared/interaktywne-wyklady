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
//   lm(columns, y)         — lm(): współczynniki, błędy, t, p, 95% CI, R², skor. R², AIC
//   ptukey, qtukey         — rozkład studentyzowanego rozstępu
//   gamesHowell(groups)    — rstatix::games_howell_test(): różnice, 95% CI, p skorygowane
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

  // Regresja liniowa MNK jak lm(): kolumny predyktorów (bez wyrazu wolnego, zmienne
  // jakościowe już jako kolumny 0/1) i wektor y. Rozkład QR Householdera, jak w R.
  // Zwraca współczynniki z błędami, t, p i 95% CI (pierwszy to wyraz wolny), R², skor. R², AIC;
  // null, gdy kolumny są liniowo zależne.
  function lm(columns, y) {
    var n = y.length, p = columns.length + 1, A = [new Float64Array(n).fill(1)], q = Float64Array.from(y), i, j, k;
    columns.forEach(function (c) { A.push(Float64Array.from(c)); });
    var scale = A.map(function (c) { var s = 0; for (var r = 0; r < n; r++) s += c[r] * c[r]; return Math.sqrt(s); });
    for (k = 0; k < p; k++) {
      var norm = 0;
      for (i = k; i < n; i++) norm += A[k][i] * A[k][i];
      norm = Math.sqrt(norm);
      if (!(norm > 1e-10 * scale[k])) return null;
      var alpha = A[k][k] > 0 ? -norm : norm, v = A[k].slice(k), v2 = 0;
      v[0] -= alpha;
      for (i = 0; i < v.length; i++) v2 += v[i] * v[i];
      var reflect = function (col) {
        var dot = 0, r;
        for (r = 0; r < v.length; r++) dot += v[r] * col[k + r];
        var f = 2 * dot / v2;
        for (r = 0; r < v.length; r++) col[k + r] -= f * v[r];
      };
      for (j = k; j < p; j++) reflect(A[j]);
      reflect(q);
    }
    // R górnotrójkątna: R[i][j] = A[j][i]; odwrotność przez podstawianie wstecz.
    var inv = [];
    for (i = 0; i < p; i++) inv.push(new Float64Array(p));
    for (j = p - 1; j >= 0; j--) {
      inv[j][j] = 1 / A[j][j];
      for (i = j - 1; i >= 0; i--) {
        var s = 0;
        for (k = i + 1; k <= j; k++) s += A[k][i] * inv[k][j];
        inv[i][j] = -s / A[i][i];
      }
    }
    var sse = 0, my = mean(y), sst = 0, df = n - p;
    for (i = p; i < n; i++) sse += q[i] * q[i];
    for (i = 0; i < n; i++) sst += (y[i] - my) * (y[i] - my);
    var sigma2 = sse / df, tq = D.qt(0.975, df), coef = [];
    for (i = 0; i < p; i++) {
      var est = 0, var_i = 0;
      for (k = i; k < p; k++) { est += inv[i][k] * q[k]; var_i += inv[i][k] * inv[i][k]; }
      var se = Math.sqrt(var_i * sigma2), tval = est / se;
      coef.push({ estimate: est, se: se, t: tval, p: pt2(tval, df), lo: est - tq * se, hi: est + tq * se });
    }
    var r2 = 1 - sse / sst;
    return { coef: coef, n: n, df: df, sigma: Math.sqrt(sigma2), r2: r2, adjR2: 1 - (1 - r2) * (n - 1) / df,
             aic: n * (Math.log(2 * Math.PI) + Math.log(sse / n) + 1) + 2 * (p + 1) };
  }

  // Rozkład studentyzowanego rozstępu: ptukey(q, nmeans, df) z R (algorytm AS 190 w wersji
  // nmath/ptukey.c: kwadratura Gaussa–Legendre'a). lower = false daje górny ogon.
  var XLEG = [0.981560634246719250690549090149, 0.904117256370474856678465866119, 0.769902674194304687036893833213,
              0.587317954286617447296702418941, 0.367831498998180193752691536644, 0.125233408511468915472441369464];
  var ALEG = [0.047175336386511827194615961485, 0.106939325995318430960254718194, 0.160078328543346226334652529543,
              0.203167426723065921749064455810, 0.233492536538354808760849898925, 0.249147045813402785000562436043];
  var XLEGQ = [0.989400934991649932596154173450, 0.944575023073232576077988415535, 0.865631202387831743880467897712,
               0.755404408355003033895101194847, 0.617876244402643748446671764049, 0.458016777657227386342419442984,
               0.281603550779258913230460501460, 0.950125098376374401853193354250e-1];
  var ALEGQ = [0.271524594117540948517805724560e-1, 0.622535239386478928628438369944e-1, 0.951585116824927848099251076022e-1,
               0.124628971255533872052476282192, 0.149595988816576732081501730547, 0.169156519395002538189312079030,
               0.182603415044923588866763667969, 0.189450610455068496285396723208];
  // Prawdopodobieństwo rozstępu w próbie z rozkładu normalnego (część całki bez df).
  function wprob(w, rr, cc) {
    var qsqz = w * 0.5;
    if (qsqz >= 8) return 1;
    var prW = 2 * D.pnorm(qsqz) - 1;
    prW = prW >= Math.exp(-50 / cc) ? Math.pow(prW, cc) : 0;
    var wincr = w > 3 ? 2 : 3, blb = qsqz, binc = (8 - qsqz) / wincr, bub = blb + binc, einsum = 0, cc1 = cc - 1;
    for (var wi = 1; wi <= wincr; wi++) {
      var elsum = 0, a = 0.5 * (bub + blb), b = 0.5 * (bub - blb);
      for (var jj = 1; jj <= 12; jj++) {
        var j, xx;
        if (jj > 6) { j = 12 - jj + 1; xx = XLEG[j - 1]; } else { j = jj; xx = -XLEG[j - 1]; }
        var ac = a + b * xx, qexpo = ac * ac;
        if (qexpo > 60) break;
        var rinsum = D.pnorm(ac) - D.pnorm(ac - w);
        if (rinsum >= Math.exp(-30 / cc1)) elsum += ALEG[j - 1] * Math.exp(-0.5 * qexpo) * Math.pow(rinsum, cc1);
      }
      einsum += elsum * 2 * b * cc * 0.398942280401432677939946059934;
      blb = bub; bub += binc;
    }
    prW += einsum;
    if (prW <= Math.exp(-30 / rr)) return 0;
    prW = Math.pow(prW, rr);
    return prW >= 1 ? 1 : prW;
  }
  function ptukey(q, nmeans, df, lower) {
    var ans;
    if (!(q > 0)) ans = 0;
    else if (!isFinite(q)) ans = 1;
    else if (df > 25000) ans = wprob(q, 1, nmeans);
    else {
      var f2 = df * 0.5, f2lf = f2 * Math.log(df) - df * Math.LN2 - D.lgamma(f2), f21 = f2 - 1, ff4 = df * 0.25;
      var ulen = df <= 100 ? 1 : df <= 800 ? 0.5 : df <= 5000 ? 0.25 : 0.125, otsum = 0;
      f2lf += Math.log(ulen);
      ans = 0;
      for (var i = 1; i <= 50; i++) {
        otsum = 0;
        var twa1 = (2 * i - 1) * ulen;
        for (var jj = 1; jj <= 16; jj++) {
          var j, t1, node;
          if (jj > 8) { j = jj - 9; node = XLEGQ[j] * ulen; t1 = f2lf + f21 * Math.log(twa1 + node) - (node + twa1) * ff4; }
          else { j = jj - 1; node = -XLEGQ[j] * ulen; t1 = f2lf + f21 * Math.log(twa1 + node) + (-node - twa1) * ff4; }
          if (t1 >= -30) otsum += wprob(q * Math.sqrt((twa1 + node) * 0.5), 1, nmeans) * ALEGQ[j] * Math.exp(t1);
        }
        if (i * ulen >= 1 && otsum <= 1e-14) break;
        ans += otsum;
      }
      if (ans > 1) ans = 1;
    }
    return lower === false ? 1 - ans : ans;
  }
  // qtukey(p, nmeans, df): kwantyl przez bisekcję dystrybuanty.
  function qtukey(p, nmeans, df) {
    var lo = 0, hi = 10;
    while (ptukey(hi, nmeans, df) < p && hi < 1e4) hi *= 2;
    for (var i = 0; i < 60; i++) { var mid = (lo + hi) / 2; if (ptukey(mid, nmeans, df) < p) lo = mid; else hi = mid; }
    return (lo + hi) / 2;
  }
  // Porównania par Gamesa-Howella jak rstatix::games_howell_test(): dla każdej pary (i < j)
  // różnica średnich (grupa j − grupa i), 95% CI i p skorygowane rozkładem rozstępu.
  function gamesHowell(groups) {
    var k = groups.length, stats = groups.map(function (g) { return { n: g.length, m: mean(g), v: variance(g) }; }), out = [];
    for (var i = 0; i < k; i++) for (var j = i + 1; j < k; j++) {
      var a = stats[i], b = stats[j], va = a.v / a.n, vb = b.v / b.n, diff = b.m - a.m;
      var df = (va + vb) * (va + vb) / (va * va / (a.n - 1) + vb * vb / (b.n - 1));
      var se = Math.sqrt(0.5 * (va + vb)), tval = Math.abs(diff) / Math.sqrt(va + vb), crit = qtukey(0.95, k, df);
      out.push({ i: i, j: j, estimate: diff, lo: diff - crit * se, hi: diff + crit * se, df: df,
                 p: ptukey(tval * Math.SQRT2, k, df, false) });
    }
    return out;
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
    bartlett: bartlett, tTest: tTest, lm: lm, ptukey: ptukey, qtukey: qtukey, gamesHowell: gamesHowell, chisq2x2: chisq2x2, fisher2x2: fisher2x2, fmtPValue: fmtPValue, round: round };
})();
