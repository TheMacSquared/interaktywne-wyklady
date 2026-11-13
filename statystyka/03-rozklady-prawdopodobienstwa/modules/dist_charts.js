// Ryciny rozkładów teoretycznych wykładu 03 liczone w przeglądarce (dawniej
// ggplot na serwerze). Silnik: window.lcSF (statystyka/R/lc_stat_figures.js),
// rozkłady: window.lcDist (statystyka/R/lc_dist.js). Scenariusze, kolory
// i podpisy przychodzą z R w data-config (jedno źródło definicji).
//
//   pmf    — lizaki P(X = k) kilku scenariuszy (Ryc. 3.4, 3.8, 3.11)
//   pdf    — krzywe gęstości z polem (Ryc. 4.6, 4.8, 5.11, 5.12, 5.14)
//   compare — cztery rozkłady dyskretne obok siebie z E(X) i ±SD (Ryc. 3.12)
//   normal — eksploracja N(μ, σ) z regułą 68–95–99.7 i presetami (Ryc. 5.2)
//   zprob  — pole pod N(0, 1): P(Z < a), P(Z > a), P(a < Z < b) (Ryc. 5.6)
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist;
  if (!C || !S || !D) return;
  var fmt = S.fmt;

  function max(a) { return Math.max.apply(null, a); }
  // Wybrane scenariusze w kolejności definicji (jak factor levels w R).
  function chosen(cfg, v) {
    var on = Array.isArray(v) ? v : [];
    return Object.keys(cfg.defs).filter(function (id) { return on.indexOf(id) >= 0; })
      .map(function (id) { return cfg.defs[id]; });
  }
  function legend(defs) { return defs.map(function (d) { return { text: d.label, color: d.color, box: true }; }); }
  // Lista „E(X) = …, SD = …” pod wykresem (teksty policzone w R).
  function statsList(root, defs) {
    var ul = root.querySelector('[data-role="stats"]');
    if (!ul) return;
    ul.innerHTML = defs.map(function (d) { return "<li>" + S.esc(d.stats) + "</li>"; }).join("");
    ul.hidden = !defs.length;
  }

  // ---- PMF: lizaki z przesunięciem jak position_dodge(width = 0.5) ----------
  function pmfValues(dist, d) {
    if (dist === "binom") return { from: 0, to: d.n, p: function (k) { return D.dbinom(k, d.n, d.p); } };
    if (dist === "pois") return { p: function (k) { return D.dpois(k, d.lambda); } };
    return { p: function (k) { return D.dgeom(k, d.p); } };
  }
  function pmfRange(dist, defs) {
    if (dist === "binom") return [0, max(defs.map(function (d) { return d.n; }))];
    if (dist === "pois") return [0, max(defs.map(function (d) { return D.qpois(0.999, d.lambda); }))];
    return [1, Math.min(40, max(defs.map(function (d) { return D.qgeom(0.999, d.p); })))];
  }

  var FIGS = {};

  FIGS.pmf = {
    draw: function (v, P, k, state) {
      var cfg = state.config, defs = chosen(cfg, v);
      if (!defs.length) return null;
      var r = pmfRange(cfg.dist, defs), n = defs.length, w = n > 1 ? 0.5 : 0;
      var pts = [], cols = [], segs = [], top = 0;
      defs.forEach(function (d, i) {
        var pv = pmfValues(cfg.dist, d), off = n > 1 ? -w / 2 + (i + 0.5) * w / n : 0;
        var lo = pv.from == null ? r[0] : pv.from, hi = pv.to == null ? r[1] : pv.to;
        for (var x = lo; x <= hi; x++) {
          var y = pv.p(x);
          pts.push([x + off, y]); cols.push(d.color);
          segs.push({ from: [x + off, 0], to: [x + off, y], color: d.color });
          top = Math.max(top, y);
        }
      });
      var pad = Math.max(0.7, (r[1] - r[0]) * 0.04);
      return {
        x: [r[0] - pad, r[1] + pad], y: [0, top * 1.1], xName: cfg.xName, yName: cfg.yName,
        noXGrid: true, xInt: true,
        layers: [{ type: "segs", items: segs, width: 2, alpha: 0.6 },
                 { type: "points", data: pts, colors: cols, size: 11, alpha: 0.85 }],
        legend: legend(defs),
        aria: "Rozkład prawdopodobieństwa: " + defs.map(function (d) { return d.label; }).join("; ")
      };
    },
    update: function (root, state, v) { statsList(root, chosen(state.config, v)); }
  };

  // ---- PDF: krzywe z polem (geom_area alpha 0.15 + geom_line) ----------------
  function pdfCurve(dist, d, xs) {
    if (dist === "unif") {
      // Prostokąt z pionowymi krawędziami zamiast 1000 punktów próbkowania.
      var h = 1 / (d.b - d.a);
      return [[xs[0], 0], [d.a, 0], [d.a, h], [d.b, h], [d.b, 0], [xs[xs.length - 1], 0]];
    }
    var f = dist === "exp" ? function (x) { return D.dexp(x, d.lambda); } :
            dist === "t" ? function (x) { return d.normal ? D.dnorm(x) : D.dt(x, d.df); } :
            dist === "chisq" ? function (x) { return D.dchisq(x, d.df); } :
            function (x) { return D.dlnorm(x, d.mu, d.sigma); };
    return xs.map(function (x) { return [x, f(x)]; });
  }
  function pdfGrid(dist, defs) {
    if (dist === "unif") return D.seq(-1, 12, 2);
    if (dist === "exp") return D.seq(0, max(defs.map(function (d) { return D.qexp(0.99, d.lambda); })), 500);
    if (dist === "t") return D.seq(-5, 5, 500);
    if (dist === "chisq") return D.seq(0.01, max(defs.map(function (d) { return D.qchisq(0.99, d.df); })), 500);
    return D.seq(0.001, max(defs.map(function (d) { return D.qlnorm(0.99, d.mu, d.sigma); })), 500);
  }

  FIGS.pdf = {
    draw: function (v, P, k, state, vals) {
      var cfg = state.config, defs = chosen(cfg, v), curves = defs.slice();
      // Ryc. 5.12: N(0, 1) jako odniesienie (drugie pole wyboru).
      if (cfg.normal && vals[cfg.normal] === "on") curves.push({ label: "N(0, 1)", color: "#999999", normal: true });
      if (!curves.length) return null;
      var xs = pdfGrid(cfg.dist, defs.length ? defs : curves), top = 0, layers = [];
      curves.forEach(function (d) {
        var pts = pdfCurve(cfg.dist, d, xs);
        top = Math.max(top, max(pts.map(function (p) { return p[1]; })));
        layers.push({ type: "line", data: pts, color: d.color, width: 2.4, fill: d.color, fillAlpha: 0.15 });
      });
      var span = xs[xs.length - 1] - xs[0];
      return {
        x: [xs[0] - span * 0.02, xs[xs.length - 1] + span * 0.02], y: [0, top * 1.1],
        xName: "x", yName: "f(x)", layers: layers, legend: legend(curves),
        aria: "Gęstości: " + curves.map(function (d) { return d.label; }).join("; ")
      };
    },
    update: function (root, state, v) { statsList(root, chosen(state.config, v)); }
  };

  // ---- Ryc. 3.12: cztery rozkłady w siatce 2 × 2 -----------------------------
  var COMPARE = [
    { key: "unif", xs: [1, 2, 3, 4, 5, 6], p: function () { return 1 / 6; }, mu: 3.5, sd: Math.sqrt(35 / 12), color: "niebo" },
    { key: "binom", xs: range(0, 20), p: function (k) { return D.dbinom(k, 20, 0.3); }, mu: 6, sd: Math.sqrt(20 * 0.3 * 0.7), color: "bursztyn" },
    { key: "pois", xs: range(0, 15), p: function (k) { return D.dpois(k, 4); }, mu: 4, sd: 2, color: "szalwia" },
    { key: "geom", xs: range(1, 25), p: function (k) { return D.dgeom(k, 0.2); }, mu: 5, sd: Math.sqrt(0.8 / 0.04), color: "indygo" }
  ];
  function range(a, b) { var out = []; for (var i = a; i <= b; i++) out.push(i); return out; }
  var COMPARE_TOP = max(COMPARE.map(function (c) { return max(c.xs.map(c.p)); })) * 1.15;

  FIGS.compare = function (v, P, k, state, vals) {
    var c = COMPARE[k], cfg = state.config || {};
    var xs = c.xs.filter(function (x) { return c.key === "unif" || c.p(x) > 0.001; });
    var col = C.token("--upwr-cat-" + c.color, cfg.colors ? cfg.colors[c.color] : P.niebo);
    var layers = [{ type: "bins", data: xs.map(function (x) { return [x - 0.35, x + 0.35, c.p(x)]; }), color: col, alpha: 0.85 }];
    if (vals.ch3_compare_show_sd === "on") layers.unshift({ type: "band", lo: c.mu - c.sd, hi: c.mu + c.sd, color: P.terakota, alpha: 0.08 });
    if (vals.ch3_compare_show_ev === "on") layers.push({ type: "vline", x: c.mu, color: P.terakota, width: 2, dash: [6, 4] });
    var lo = xs[0], hi = xs[xs.length - 1], pad = Math.max(0.6, (hi - lo) * 0.05);
    return { x: [lo - pad, hi + pad], y: [0, COMPARE_TOP], xName: "Wartość", yName: k % 2 === 0 ? "Prawdopodobieństwo" : "",
             alignLeft: true, noXGrid: true, layers: layers,
             aria: (cfg.titles ? cfg.titles[k] : c.key) + ": E(X) = " + fmt(c.mu, 2) + ", SD = " + fmt(c.sd, 2) };
  };

  // ---- Ryc. 5.2: eksploracja N(μ, σ) ------------------------------------------
  function num(root, id) { var el = root.querySelector("#" + id); return el ? Number(el.value) : NaN; }
  function setRange(root, id, r) {
    var el = root.querySelector("#" + id), out = root.querySelector('output[data-value="' + id + '"]');
    if (!el) return;
    el.setAttribute("min", r[0]); el.setAttribute("max", r[1]); el.setAttribute("step", r[3]);
    el.min = r[0]; el.max = r[1]; el.step = r[3];
    el.value = String(r[2]); el.setAttribute("value", r[2]);
    if (out) out.textContent = String(r[2]);
  }

  FIGS.normal = {
    init: function (state, root) { state.root = root; },
    act: function (state, name) {
      var pre = state.config.presets[name];
      if (!pre) return;
      setRange(state.root, "ch5_mu", pre.mu);
      setRange(state.root, "ch5_sigma", pre.sigma);
    },
    draw: function (v, P, k, state, vals) {
      var mu = num(state.root, "ch5_mu"), sigma = num(state.root, "ch5_sigma");
      var col = C.token("--upwr-cat-wrzos", "#a07894"), xs = D.seq(mu - 4 * sigma, mu + 4 * sigma, 500);
      var curve = xs.map(function (x) { return [x, D.dnorm(x, mu, sigma)]; }), yTop = D.dnorm(mu, mu, sigma);
      var layers = [];
      function area(a, b, alpha) {
        var s = D.seq(mu + a * sigma, mu + b * sigma, 120).map(function (x) { return [x, D.dnorm(x, mu, sigma)]; });
        layers.push({ type: "line", data: s, color: col, width: 0, fill: col, fillAlpha: alpha });
      }
      if (vals.ch5_show_empirical === "on") {
        area(-1, 1, 0.4); area(-2, -1, 0.25); area(1, 2, 0.25); area(-3, -2, 0.12); area(2, 3, 0.12);
      }
      layers.push({ type: "line", data: curve, color: col, width: 3 },
                  { type: "vline", x: mu, color: P.secondary, width: 1.5, dash: [6, 4] });
      if (vals.ch5_show_empirical === "on") {
        layers.push({ type: "text", x: mu, y: yTop * 0.6, text: "68%", color: P.secondary, bold: true, size: 1.25, valign: "middle", halo: true },
                    { type: "text", x: mu, y: yTop * 0.35, text: "95%", color: P.secondary, size: 1.15, valign: "middle", halo: true },
                    { type: "text", x: mu, y: yTop * 0.15, text: "99.7%", color: P.ref, size: 1.05, valign: "middle", halo: true });
      }
      return { x: [xs[0], xs[xs.length - 1]], y: [0, yTop * 1.06], xName: "x", yName: "f(x)", layers: layers,
               aria: "Gęstość N(" + fmt(mu, 6) + ", " + fmt(sigma, 6) + ")" };
    },
    update: function (root) {
      var mu = num(root, "ch5_mu"), sigma = num(root, "ch5_sigma");
      S.setValue(root, "mu", String(mu));
      S.setValue(root, "sigma", String(sigma));
      // round(x, 1) z R: bez końcowego zera.
      S.setValue(root, "i68", "[" + fmt(Math.round((mu - sigma) * 10) / 10, 1) + ", " +
                              fmt(Math.round((mu + sigma) * 10) / 10, 1) + "]");
    }
  };

  // ---- Ryc. 5.6: pole pod N(0, 1) ---------------------------------------------
  function zprob(type, a, b) {
    return type === "less" ? D.pnorm(a) : type === "greater" ? 1 - D.pnorm(a) : Math.max(0, D.pnorm(b) - D.pnorm(a));
  }
  FIGS.zprob = {
    init: function (state, root) { state.root = root; },
    draw: function (type, P, k, state) {
      var a = num(state.root, "ch5_prob_a"), b = num(state.root, "ch5_prob_b");
      var xs = D.seq(-4, 4, 500), curve = xs.map(function (x) { return [x, D.dnorm(x)]; });
      var lo = type === "greater" ? a : type === "less" ? -4 : a, hi = type === "less" ? a : type === "greater" ? 4 : b;
      var fill = type === "less" ? P.niebo : type === "greater" ? P.terakota : C.token("--upwr-cat-wrzos", "#a07894");
      var layers = [];
      if (hi > lo) {
        layers.push({ type: "line", width: 0, color: fill, fill: fill, fillAlpha: 0.4,
          data: D.seq(lo, hi, 200).map(function (x) { return [x, D.dnorm(x)]; }) });
      }
      layers.push({ type: "line", data: curve, color: P.secondary, width: 2.4 },
                  { type: "vline", x: a, color: P.terakota, width: 1.5, dash: [6, 4] });
      if (type === "between") layers.push({ type: "vline", x: b, color: P.terakota, width: 1.5, dash: [6, 4] });
      var p = zprob(type, a, b);
      layers.push({ type: "text", x: 0, y: 0.2, text: "P = " + C.fmt(p, 4), color: P.secondary, bold: true, size: 1.45, valign: "middle" });
      return { x: [-4.2, 4.2], y: [0, 0.42], xName: "z", yName: "f(z)", layers: layers,
               aria: "Pole pod krzywą N(0, 1): P = " + C.fmt(p, 4) };
    },
    update: function (root, state, type) {
      var a = num(root, "ch5_prob_a"), b = num(root, "ch5_prob_b"), p = zprob(type, a, b);
      var bBox = root.querySelector('[data-role="b"]');
      if (bBox) bBox.hidden = type !== "between";
      S.setValue(root, "plabel", type === "less" ? "P(Z < " + a + ")" : type === "greater" ? "P(Z > " + a + ")" :
                                 "P(" + a + " < Z < " + b + ")");
      S.setValue(root, "p", C.fmt(p, 4));
      S.setValue(root, "pct", C.fmt(p * 100, 2) + "%");
    }
  };

  S.register({ "d03-pmf": FIGS.pmf, "d03-pdf": FIGS.pdf, "d03-compare": FIGS.compare,
               "d03-normal": FIGS.normal, "d03-zprob": FIGS.zprob });
})();
