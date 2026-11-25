// Prototypy sceny wykładu 06, rozdział 7 (P19A, P19B): „dwa światy z powtórzeniem”.
// Kontener w R: z6p_scene_ui(..., "dwa_swiaty", cfg) w stop_proto_rj.R.
//
//   dwa_swiaty — dwa procesy o tej samej średniej i tym samym SD: A symetryczny
//                N(mu, sd), B z twardą dolną granicą: min + Gamma(k, θ) (długi prawy
//                ogon). Przyciski dopisują kolejne obserwacje; histogramy na wspólnej osi
//                i wspólnej skali wysokości, obserwacje ponad progiem wyróżnione, licznik
//                przekroczeń dla każdego procesu. Teksty, jednostki i oś podaje R (cfg).
(function () {
  "use strict";
  var Z = window.lcZ6;
  if (!Z) return;
  var U = Z.util;

  // Gamma(k, θ) metodą Marsaglii–Tsanga (k ≥ 1).
  function rgamma(rnd, k, theta) {
    var d = k - 1 / 3, c = 1 / Math.sqrt(9 * d), x, v, u;
    for (;;) {
      do { x = U.rnorm(rnd); v = 1 + c * x; } while (v <= 0);
      v = v * v * v; u = rnd();
      if (u < 1 - 0.0331 * x * x * x * x) return d * v * theta;
      if (Math.log(u) < 0.5 * x * x + d * (1 - v + Math.log(v))) return d * v * theta;
    }
  }

  function stats(a) {
    var n = a.length, m = U.mean(a), ss = 0;
    for (var i = 0; i < n; i++) ss += (a[i] - m) * (a[i] - m);
    return { n: n, mean: m, sd: n > 1 ? Math.sqrt(ss / (n - 1)) : NaN };
  }

  Z.register({
    dwa_swiaty: function (cfg, ui) {
      var rnd = Math.random, A, B, X0 = 170, X1 = 770, T0 = cfg.axis[0], T1 = cfg.axis[1], BIN = cfg.bin;
      var D = cfg.digits == null ? 0 : cfg.digits;
      var xOf = function (t) { return X0 + (Math.min(Math.max(t, T0), T1) - T0) / (T1 - T0) * (X1 - X0); };

      function one() {
        A.push(cfg.a.mu + cfg.a.sd * U.rnorm(rnd));
        B.push(cfg.b.min + rgamma(rnd, cfg.b.k, cfg.b.theta));
      }
      function add(n) { for (var i = 0; i < n; i++) one(); }
      function reset() { A = []; B = []; add(1); }

      function bins(a) {
        var nb = Math.round((T1 - T0) / BIN), out = [], j;
        for (j = 0; j < nb; j++) out.push(0);
        a.forEach(function (t) { j = Math.floor((Math.min(Math.max(t, T0), T1 - 1e-9) - T0) / BIN); out[j]++; });
        return out;
      }

      function strip(a, y0, h, w, top) {
        var b = bins(a), out = "", j, x, bh, over;
        out += '<text x="0" y="' + U.g(y0 + 28) + '" font-size="16" font-weight="700">' + U.esc(w.name) + "</text>";
        out += '<text class="z6p-subtle" x="0" y="' + U.g(y0 + 48) + '" font-size="13">' + U.esc(w.sub) + "</text>";
        out += '<line class="z6p-axis" x1="' + X0 + '" x2="' + X1 + '" y1="' + U.g(y0 + h) + '" y2="' + U.g(y0 + h) + '"/>';
        for (j = 0; j < b.length; j++) {
          if (!b[j]) continue;
          x = xOf(T0 + j * BIN); bh = b[j] / top * (h - 8);
          over = T0 + j * BIN >= cfg.limit - 1e-9;
          out += '<rect class="' + (over ? "z6p-cell-bad" : "z6p19-bar") + '" x="' + U.g(x + 0.5) + '" y="' + U.g(y0 + h - bh) +
            '" width="' + U.g(xOf(T0 + BIN) - xOf(T0) - 1) + '" height="' + U.g(bh) + '"/>';
        }
        if (a.length) {
          var t = a[a.length - 1];
          out += '<path class="z6p19-today" d="M' + U.g(xOf(t)) + "," + U.g(y0 + h + 4) + ' l-6,10 h12 z"/>';
        }
        return out;
      }

      function render() {
        var top = Math.max(1, Math.max.apply(null, bins(A).concat(bins(B))));
        var body = strip(A, 10, 110, cfg.a, top) + strip(B, 150, 110, cfg.b, top);
        var xl = xOf(cfg.limit);
        body += '<line class="z6p-limit" x1="' + U.g(xl) + '" x2="' + U.g(xl) + '" y1="8" y2="276"/>' +
          '<text class="z6p-mono" x="' + U.g(xl + 6) + '" y="22" font-size="13">' + U.esc(cfg.limit_label) + "</text>";
        body += U.axis(xOf, 292, cfg.ticks, null, cfg.tick_labels);
        var n = A.length;
        ui.set("stage", U.svgWrap(800, 320, cfg.aria + " Liczba obserwacji: " + n + ".", body));

        function line(a, w) {
          var s = stats(a), over = a.filter(function (t) { return t > cfg.limit; }).length;
          return '<p class="z6p-read">' + U.esc(w.who) + ": średnio " + U.fmt(s.mean, D + 1) + " " + cfg.unit + " · SD " +
            (isFinite(s.sd) ? U.fmt(s.sd, D + 1) : "–") + " " + cfg.unit + " · " + U.esc(cfg.over_word) + ": " + over + " z " + s.n + "</p>";
        }
        var last = cfg.item + " " + n + ": " + cfg.a.who + " " + U.fmt(A[n - 1], D) + " " + cfg.unit + ", " +
          cfg.b.who + " " + U.fmt(B[n - 1], D) + " " + cfg.unit + ".";
        ui.set("read", '<p class="z6p-caption">' + U.esc(last) + "</p>" + line(A, cfg.a) + line(B, cfg.b));
      }

      reset();
      var on = { reset: reset };
      cfg.steps.forEach(function (s) { on["add" + s.n] = function () { add(s.n); }; });
      return { on: on, render: render };
    }
  });
})();
