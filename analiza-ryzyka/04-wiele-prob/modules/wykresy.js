// Wykresy wykładu 04 „Wiele prób” liczone w przeglądarce (dawniej ggplot na
// serwerze). Silnik: window.lcSF (R/lc_stat_figures.js, ECharts SVG), rozkłady
// i losowanie: window.lcDist (R/lc_dist.js). Działają też offline.
//
//   p4_assume  — Galeria „Trzy partie, ta sama średnia” (dane z R, ziarno 4403)
//   p4_bg      — Galeria „Rozkład dwumianowy B(n, p)” (pola wyboru)
//   p4_binom   — Rozkład: B(n, p) z wyróżnionym zakresem odpowiedzi
//   p4_batches — Powtórzenia: 500 partii (pierwsze losowanie z R, ziarno 2404)
//   p4_bt      — Symulacja „Partia po partii” (+1 … +1000)
//   p4_one     — Krzywa: P(X ≥ 1) wraz z liczbą prób
//   p4_np      — Przypadek brzegowy: 1 - (1 - p)ⁿ a skrót n · p
//   p4_oc      — Piaskownica: krzywa operacyjna planu (n, c)
//   p4_zero    — Galeria „Trzech dostawców bez wad”
//   p4_closing — Domknięcie: B(100, 0.02) i pięć pytań
//
// Funkcje liczące (dbinom, pbinom, qbinom, fmtP) są w window.lcP4Charts do testów.
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;
  var D = root.lcDist;

  // --- Liczby jak w R --------------------------------------------------------

  function dbinom(k, n, p) { return D.dbinom(k, n, p); }
  function pbinom(c, n, p) { var s = 0; for (var k = 0; k <= c; k++) s += dbinom(k, n, p); return Math.min(1, s); }
  // qbinom(q, n, p): najmniejsze k z F(k) ≥ q (tolerancja jak w R)
  function qbinom(q, n, p) {
    var s = 0;
    for (var k = 0; k <= n; k++) { s += dbinom(k, n, p); if (s >= q * (1 - 64 * 2.220446e-16)) return k; }
    return n;
  }
  // risk_fmt_p() z R/risk_block.R: dla małych p tyle miejsc, żeby były dwie cyfry znaczące.
  function fmtP(x) {
    if (!isFinite(x)) return "—";
    var d = 3;
    if (x > 0 && x < 0.01) d = Math.max(3, Math.min(12, Math.ceil(-Math.log10(x)) + 2));
    return x.toFixed(d);
  }
  function pct(x) { return (100 * x).toFixed(0) + "%"; }
  function mean(a) { var s = 0; for (var i = 0; i < a.length; i++) s += a[i]; return s / a.length; }
  function range(a, b, step) { var out = []; for (var x = a; x <= b + 1e-9; x += step || 1) out.push(Math.round(x * 1e6) / 1e6); return out; }
  function share(xs, kmax) {
    var out = range(0, kmax).map(function () { return 0; });
    xs.forEach(function (x) { out[Math.min(x, kmax)]++; });
    return out.map(function (c) { return xs.length ? c / xs.length : 0; });
  }

  root.lcP4Charts = { dbinom: dbinom, pbinom: pbinom, qbinom: qbinom, fmtP: fmtP, share: share };

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S || !D) return;

  // --- Wspólne warstwy ---------------------------------------------------------

  function cat(name) { return C.token("--upwr-cat-" + name); }
  // Słupki o szerokości w wokół k (geom_col).
  function cols(ys, w, x0) {
    return ys.map(function (y, k) { var x = (x0 || 0) + k; return [x - w / 2, x + w / 2, y]; });
  }
  // Kropki modelu: białe z obwódką (shape = 21 w ggplot) — dwie warstwy punktów.
  function modelDots(P, data, size) {
    var s = size || 11;
    return [{ type: "points", data: data, color: P.ink, size: s, z: 6 },
            { type: "points", data: data, color: P.surface, size: s - 4, z: 6 }];
  }
  function ticks(a, b) { return range(a, b); }
  function readColor(rootEl, key, color) {
    var v = rootEl.querySelector('[data-v="' + key + '"]');
    var box = v && v.closest(".lc-read");
    if (box) box.style.setProperty("--lc-read-color", color);
  }

  var figures = {};

  // --- (3) Trzy partie, ta sama średnia -----------------------------------------

  figures.p4_assume = {
    prepare: function (state, v, vals) { state.split = v === "mixture" && vals.p4p_split === "apart"; },
    noteKey: function (vals, state) { return state.split ? "split" : "joint"; },
    draw: function (v, P, k, state) {
      var cfg = state.config, layers = [];
      if (state.split) {
        var sp = cfg.split, labels = {};
        [0, 2, 4, 6, 8, 10].forEach(function (x) { var t = x === 10 ? "10+" : String(x); labels[x] = t; labels[x + 12] = t; });
        layers.push({ type: "bins", data: cols(sp.obsA, 0.75), color: P.szalwia },
                    { type: "bins", data: cols(sp.obsB, 0.75, 12), color: P.accent },
                    { type: "vline", x: 11, color: P.rule, width: 1 });
        layers = layers.concat(modelDots(P, sp.refA.map(function (y, i) { return [i, y]; }).concat(
          sp.refB.map(function (y, i) { return [i + 12, y]; }))));
        layers.push({ type: "text", x: 5, y: "top", text: "Magazyn A: p = 0.01", color: P.ink, bold: true },
                    { type: "text", x: 17, y: "top", text: "Magazyn B: p = 0.03", color: P.ink, bold: true });
        return { x: [-0.6, 22.6], y: [0, 0.4], xLabels: labels, noXGrid: true, layers: layers,
                 xName: "Liczba niesprawnych w partii", yName: "Udział partii",
                 aria: "Udziały partii z liczbą wad osobno dla magazynu A i B, każdy na tle swojego modelu dwumianowego." };
      }
      var lab = {};
      range(0, 10).forEach(function (x) { lab[x] = x === 10 ? "10+" : String(x); });
      layers.push({ type: "bins", data: cols(cfg.obs[v], 0.75), color: v === "stable" ? P.secondary : P.accent });
      layers = layers.concat(modelDots(P, cfg.ref.map(function (y, i) { return [i, y]; })));
      return { x: [-0.6, 10.6], y: [0, 0.32], xLabels: lab, noXGrid: true, layers: layers,
               xName: "Liczba niesprawnych w partii", yName: "Udział partii",
               aria: "Histogram liczby wad w symulowanych partiach na tle modelu dwumianowego." };
    },
    update: function (rootEl, state, v) {
      var cfg = state.config, splitCtl = rootEl.querySelector("#p4p_split");
      if (splitCtl) splitCtl.style.display = v === "mixture" ? "" : "none";
      if (state.split) {
        S.setValue(rootEl, "as1_l", "Średnia A · B"); S.setValue(rootEl, "as1", cfg.split.mean);
        S.setValue(rootEl, "as2_l", "P(X = 0) w A"); S.setValue(rootEl, "as2", cfg.split.p0A);
        S.setValue(rootEl, "as3_l", "P(X = 0) w B"); S.setValue(rootEl, "as3", cfg.split.p0B);
        readColor(rootEl, "as2", "var(--upwr-cat-szalwia)");
      } else {
        var r = cfg.reads[v];
        S.setValue(rootEl, "as1_l", "Średnia"); S.setValue(rootEl, "as1", r.mean);
        S.setValue(rootEl, "as2_l", "P(X = 0)"); S.setValue(rootEl, "as2", r.p0);
        S.setValue(rootEl, "as3_l", "P(X ≥ 5)"); S.setValue(rootEl, "as3", r.p5);
        readColor(rootEl, "as2", "var(--upwr-accent)");
      }
    }
  };

  // --- Galeria B(n, p) ----------------------------------------------------------

  var CAT = ["grafit", "bursztyn", "niebo", "szalwia", "kurkuma"];
  figures.p4_bg = {
    prepare: function (state, v) { state.sel = (v || []).filter(function (key) { return state.config.defs[key]; }); },
    draw: function (v, P, k, state) {
      var sel = state.sel, defs = state.config.defs, m = sel.length;
      if (!m) return null;
      var xmax = 0, ymax = 0, layers = [], legend = [];
      sel.forEach(function (key) {
        var s = defs[key]; xmax = Math.max(xmax, Math.min(s.n, qbinom(0.9999, s.n, s.p) + 1));
      });
      sel.forEach(function (key, i) {
        var s = defs[key], color = cat(CAT[i]), off = m > 1 ? (i - (m - 1) / 2) * 0.5 / m : 0, pts = [], segs = [];
        for (var x = 0; x <= Math.min(s.n, qbinom(0.9999, s.n, s.p) + 1); x++) {
          var y = dbinom(x, s.n, s.p); ymax = Math.max(ymax, y);
          pts.push([x + off, y]); segs.push({ from: [x + off, 0], to: [x + off, y] });
        }
        layers.push({ type: "segs", items: segs, color: color, width: 2, alpha: 0.6 },
                    { type: "points", data: pts, color: color, size: 11, alpha: 0.85 });
        legend.push({ text: s.label, color: color });
      });
      var step = xmax > 20 ? 2 : 1;
      return { x: [-0.6, xmax + 0.6], y: [0, ymax * 1.1], xTicks: range(0, xmax, step), noXGrid: true,
               layers: layers, legend: legend, xName: "Liczba sukcesów (k)", yName: "P(X = k)",
               aria: "Rozkłady dwumianowe dla wybranych scenariuszy, nałożone na jednej osi." };
    },
    update: function (rootEl, state) {
      var out = rootEl.querySelector('[data-role="stats"]');
      if (!out) return;
      out.innerHTML = state.sel.length ? "<ul>" + state.sel.map(function (key) {
        var s = state.config.defs[key];
        return "<li>" + S.esc(s.label + ":  E(X) = " + s.mean + ",  SD = " + s.sd) + "</li>";
      }).join("") + "</ul>" : "";
    }
  };

  // --- Rozkład: B(n, p) i pytanie o k ----------------------------------------------

  figures.p4_binom = {
    // k nie może przekroczyć n: zakres suwaka k idzie za n.
    prepare: function (state, v, vals, rootEl) {
      var n = Number(vals.p4_n), kIn = rootEl.querySelector("#p4_k");
      if (kIn) {
        kIn.max = String(n);
        if (Number(kIn.value) > n) kIn.value = String(n);
        var out = rootEl.querySelector('output[data-value="p4_k"]');
        if (out) out.textContent = kIn.value;
      }
    },
    draw: function (v, P, k, state, vals) {
      var n = Number(vals.p4_n), p = Number(vals.p4_p), kk = Math.min(Number(vals.p4_k), n), q = vals.p4_query;
      var data = [], colors = [], ymax = 0, prob = 0;
      for (var x = 0; x <= n; x++) {
        var y = dbinom(x, n, p), on = q === "exactly" ? x === kk : q === "at_least" ? x >= kk : x <= kk;
        if (on) prob += y;
        ymax = Math.max(ymax, y);
        data.push([x - 0.45, x + 0.45, y]); colors.push(on ? P.accent : P.ref);
      }
      state.prob = Math.min(1, prob);
      return { x: [-0.6, n + 0.6], y: [0, ymax * 1.08], xTicks: range(0, n, n > 15 ? 2 : 1), noXGrid: true,
               layers: [{ type: "bins", data: data, colors: colors }],
               legend: [{ text: "Odpowiedź", color: P.accent, box: true }, { text: "Pozostałe", color: P.ref, box: true }],
               xName: "Liczba niesprawnych", yName: "Prawdopodobieństwo",
               aria: "Słupkowy rozkład dwumianowy z wyróżnionym zakresem odpowiedzi." };
    },
    update: function (rootEl, state) { S.setValue(rootEl, "bi1", fmtP(state.prob)); }
  };

  // --- Powtórzenia: 500 partii ----------------------------------------------------

  figures.p4_batches = {
    init: function (state) { state.xs = state.config.first.slice(); },
    act: function (state) { state.xs = D.r.many(500, function () { return D.r.binom(100, 0.02); }); },
    draw: function (v, P, k, state) {
      var counts = range(0, 9).map(function (x) { return state.xs.filter(function (y) { return y === x; }).length; });
      return { x: [-0.5, 9.5], y: [0, 170], xTicks: range(0, 9), noXGrid: true,
               layers: [{ type: "bins", data: counts.map(function (c, x) { return [x - 0.5, x + 0.5, c]; }),
                          color: P.secondary, stroke: P.surface, lineWidth: 1 },
                        { type: "vline", x: 2, color: P.accent, width: 2 }],
               xName: "Liczba niesprawnych", yName: "Liczba partii",
               aria: "Histogram liczby niesprawnych w wielu partiach z linią wartości oczekiwanej." };
    },
    update: function (rootEl, state) {
      var xs = state.xs;
      S.setValue(rootEl, "ba1", "2");
      S.setValue(rootEl, "ba2", mean(xs).toFixed(2));
      S.setValue(rootEl, "ba3", pct(xs.filter(function (x) { return x === 2; }).length / xs.length));
      S.setValue(rootEl, "ba4", Math.min.apply(null, xs) + "–" + Math.max.apply(null, xs));
    }
  };

  // --- (6) Partia po partii ---------------------------------------------------------

  figures.p4_bt = {
    init: function (state) { state.xs = []; },
    act: function (state, a) {
      var m = { one: 1, ten: 10, hund: 100, thou: 1000 }[a];
      if (a === "reset") state.xs = [];
      else if (m) for (var i = 0; i < m; i++) state.xs.push(D.r.binom(100, 0.02));
    },
    draw: function (v, P, k, state) {
      var xs = state.xs, lab = {};
      range(0, 9).forEach(function (x) { lab[x] = x === 9 ? "9+" : String(x); });
      var layers = [{ type: "bins", data: cols(share(xs, 9), 0.75), color: P.secondary }];
      layers = layers.concat(modelDots(P, range(0, 9).map(function (x) { return [x, dbinom(x, 100, 0.02)]; })));
      if (xs.length) layers.push({ type: "vline", x: mean(xs), color: P.accent, width: 2.2 });
      return { x: [-0.6, 9.6], y: [0, 0.45], xLabels: lab, noXGrid: true, layers: layers,
               xName: "Liczba niesprawnych w partii", yName: "Udział partii",
               aria: "Histogram liczby wad w kolejnych partiach, z bieżącą średnią i modelem dwumianowym." };
    },
    update: function (rootEl, state) {
      var xs = state.xs, m = xs.length;
      S.setValue(rootEl, "bt1", String(m));
      S.setValue(rootEl, "bt2", m ? mean(xs).toFixed(2) : "—");
      S.setValue(rootEl, "bt3", m ? pct(xs.filter(function (x) { return x === 2; }).length / m) : "—");
      S.setValue(rootEl, "bt4", m ? pct(xs.filter(function (x) { return x === 0; }).length / m) : "—");
    }
  };

  // --- Krzywa: ryzyko wraz z liczbą prób ----------------------------------------------

  figures.p4_one = {
    draw: function (v, P) {
      var p = Number(v), data = range(1, 300).map(function (n) { return [n, 1 - Math.pow(1 - p, n)]; });
      return { x: [0, 300], y: [0, 1], layers: [{ type: "line", data: data, color: P.accent, width: 2.2 }],
               xName: "Liczba prób", yName: "P(X ≥ 1)",
               aria: "Rosnąca krzywa prawdopodobieństwa co najmniej jednej niesprawności." };
    },
    update: function (rootEl, state, v) { S.setValue(rootEl, "on1", fmtP(1 - Math.pow(1 - Number(v), 100))); }
  };

  // --- (7) Skrót n · p ---------------------------------------------------------------

  figures.p4_np = {
    draw: function (v, P) {
      var p = Number(v), n = range(1, 300);
      return { x: [0, 300], y: [0, 1.6],
               layers: [{ type: "rect", x0: 0, x1: 300, y0: 1, y1: 1.6, fill: P.accent, alpha: 0.07, z: 1 },
                        { type: "segment", from: [0, 1], to: [300, 1], color: P.ref, width: 1.2 },
                        { type: "line", data: n.map(function (m) { return [m, m * p]; }), color: P.secondary, width: 2.4, dash: [6, 5] },
                        { type: "line", data: n.map(function (m) { return [m, 1 - Math.pow(1 - p, m)]; }), color: P.accent, width: 2.4 }],
               xName: "Liczba prób n", yName: "Prawdopodobieństwo",
               aria: "Krzywa ryzyka skumulowanego i prosta n razy p, która przekracza 1." };
    },
    update: function (rootEl, state, v) {
      var p = Number(v), cross = Math.ceil(1 / p - 1e-9);
      S.setValue(rootEl, "np1", (1 - Math.pow(1 - p, 100)).toFixed(3) + " vs " + (100 * p).toFixed(3));
      S.setValue(rootEl, "np2", String(cross));
      S.setValue(rootEl, "np3", (1 - Math.pow(1 - p, cross)).toFixed(3));
    }
  };

  // --- (10) Krzywa operacyjna planu (n, c) ------------------------------------------------

  figures.p4_oc = {
    draw: function (v, P, k, state, vals) {
      var n = Number(vals.p4p_oc_n), c = Number(vals.p4p_oc_c), cfg = state.config, ps = range(0, 0.15, 0.001);
      var good = [cfg.good, pbinom(c, n, cfg.good)], bad = [cfg.bad, pbinom(c, n, cfg.bad)];
      return { x: [0, 0.15], y: [0, 1], xTicks: range(0, 0.14, 0.02),
               layers: [{ type: "vline", x: cfg.good, color: P.rule, width: 1.2 },
                        { type: "vline", x: cfg.bad, color: P.rule, width: 1.2 },
                        { type: "line", data: ps.map(function (p) { return [p, pbinom(1, 50, p)]; }), color: P.ref, width: 1.8, dash: [6, 5] },
                        { type: "line", data: ps.map(function (p) { return [p, pbinom(c, n, p)]; }), color: P.secondary, width: 2.8 },
                        { type: "points", data: [good, bad], color: P.surface, size: 17, z: 6 },
                        { type: "points", data: [good, bad], colors: [P.szalwia, P.accent], size: 13, z: 6 }],
               xName: "Jakość partii p (udział niesprawnych)", yName: "P(przyjęcia partii)",
               aria: "Krzywa operacyjna planu odbioru: prawdopodobieństwo przyjęcia partii w zależności od jej jakości." };
    },
    update: function (rootEl, state, v) {
      var vals = { n: Number(rootEl.querySelector("#p4p_oc_n").value), c: Number(rootEl.querySelector("#p4p_oc_c").value) };
      S.setValue(rootEl, "oc1", (1 - pbinom(vals.c, vals.n, state.config.good)).toFixed(3));
      S.setValue(rootEl, "oc2", pbinom(vals.c, vals.n, state.config.bad).toFixed(3));
    }
  };

  // --- (12) Trzech dostawców bez wad ------------------------------------------------------

  figures.p4_zero = function (v, P, k, state) {
    var ns = state && state.config ? state.config.n : [30, 100, 300], ps = range(0, 0.14, 0.0005);
    var dashes = ["solid", [8, 4], [2, 4]], nudge = [0.13, 0.25, 0.37], layers = [
      { type: "vline", x: 0.02, color: P.ref, width: 1.4, dash: [6, 5] },
      { type: "segment", from: [0, 0.05], to: [0.14, 0.05], color: P.accent, width: 1.2 }];
    ns.forEach(function (n, i) {
      layers.push({ type: "line", data: ps.map(function (p) { return [p, Math.pow(1 - p, n)]; }), color: P.secondary, width: 2.2, dash: dashes[i] });
    });
    ns.forEach(function (n, i) {
      var pu = 1 - Math.pow(0.05, 1 / n);
      layers.push({ type: "point", x: pu, y: 0.05, color: P.accent, size: 11 },
                  { type: "text", x: pu, y: 0.05 + nudge[i], text: n + " kontroli: p ≤ " + pu.toFixed(3), color: P.accent, align: "left", halo: true });
    });
    return { x: [0, 0.14], y: [0, 1], xTicks: range(0, 0.14, 0.02), layers: layers,
             xName: "Prawdziwe p dostawcy", yName: "P(zero wad w n kontrolach)",
             aria: "Trzy krzywe prawdopodobieństwa zera wad i górne granice p dla 30, 100 i 300 kontroli." };
  };

  // --- (13) Jeden rozkład, pięć pytań ------------------------------------------------------

  figures.p4_closing = function (v, P) {
    var ks = range(0, 8), br = [
      { x0: -0.4, x1: 0.4, y: 0.49, t: "P(X = 0) = 0.133" },
      { x0: 0.6, x1: 8.4, y: 0.37, t: "P(X ≥ 1) = 0.867 · co najmniej jedna" },
      { x0: -0.4, x1: 2.4, y: 0.43, t: "P(X ≤ 2) = 0.677 · przyjęcie w planie (100, 2)" },
      { x0: 2.6, x1: 8.4, y: 0.31, t: "P(X ≥ 3) = 0.323" }];
    var segs = [];
    br.forEach(function (b) {
      segs.push({ from: [b.x0, b.y], to: [b.x1, b.y] }, { from: [b.x0, b.y], to: [b.x0, b.y - 0.012] },
                { from: [b.x1, b.y], to: [b.x1, b.y - 0.012] });
    });
    var layers = [{ type: "bins", data: cols(ks.map(function (x) { return dbinom(x, 100, 0.02); }), 0.75), color: P.secondary },
                  { type: "segment", from: [2, 0], to: [2, 0.29], color: P.accent, width: 2 },
                  { type: "segs", items: segs, color: P.accent, width: 1.8 },
                  { type: "text", x: 2, y: 0.02, text: "E(X) = 2", color: P.accent, bold: true, halo: true }];
    br.forEach(function (b) { layers.push({ type: "text", x: b.x0, y: b.y + 0.008, text: b.t, color: P.ink, align: "left" }); });
    return { x: [-0.6, 8.6], y: [0, 0.56], xTicks: ks, noXGrid: true, layers: layers,
             xName: "Liczba niesprawnych w partii 100 zaworów", yName: "P(X = k)",
             aria: "Rozkład B(100, 0.02) z zaznaczonymi odpowiedziami na pytania z wykładu." };
  };

  S.register(figures);
})();
