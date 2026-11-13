// Punkty stop rozdziałów 1–2 (stops_ch1.R, stops_ch2.R, stops_ch2_var.R): Ryc. 1.1, 2.2, 2.3, 2.4, 2.5.
// Silnik: window.lcSF, rozkłady i losowanie: window.lcDist, pomocniki: window.lc03Sim (sim_charts.js).
//
//   d03-dice   — Ryc. 1.1 „Stos kostek”: kafelki rzutów, potem słupki częstości
//   d03-back   — Ryc. 2.2 „Ile wraca z 1 zł”: średnia wygrana z gier na skali log.
//   d03-phone  — Ryc. 2.3 „Ubezpieczenie telefonu”: dwa rozkłady bilansu roku
//   d03-routes — Ryc. 2.4 „Dwie trasy na egzamin”: gęstości gamma i pola spóźnienia
//   d03-pays   — Ryc. 2.5 „Pasy wypłat”: chmury wypłat trzech loterii i klamry ±SD
//
// Dane i stałe teksty przychodzą z R w data-config; losowanie i stan są w przeglądarce.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist, H = window.lc03Sim;
  if (!C || !S || !D || !H) return;
  var fmt = S.fmt, R = D.r;

  function sum(a) { var s = 0; for (var i = 0; i < a.length; i++) s += a[i]; return s; }
  function pct(x, d) { return fmt(100 * x, d == null ? 1 : d) + "%"; }
  // Pokazuje lub ukrywa element także wtedy, gdy klasa nadaje mu display (np. .lc-reads).
  function show(el, on) { if (!el) return; el.hidden = !on; el.style.display = on ? "" : "none"; }
  function typeset(el) {
    var MJ = window.MathJax;
    if (MJ && MJ.Hub && MJ.Hub.Queue) MJ.Hub.Queue(["Typeset", MJ.Hub, el]);
    else if (MJ && MJ.typesetPromise) MJ.typesetPromise([el]);
  }
  function hline(y, x0, x1, color, width, dash) {
    return { type: "line", data: [[x0, y], [x1, y]], color: color, width: width, dash: dash };
  }
  // Pionowe linie siatki (gdy podziałki xTicks wyłączają siatkę silnika).
  function vgrid(xs, y0, y1, color) {
    return { type: "segs", z: 1, width: 1, items: xs.map(function (x) { return { from: [x, y0], to: [x, y1], color: color }; }) };
  }

  // Etykiety rzędów 1..k (yLabels).
  function yLabels(names) {
    var out = {};
    names.forEach(function (t, i) { out[i + 1] = t; });
    return out;
  }

  var FIGS = {};

  // ---- Ryc. 1.1: stos kostek ----------------------------------------------------
  FIGS.dice = {
    init: function (state) { state.rolls = []; },
    act: function (state, name) {
      if (name === "reset") { state.rolls = []; return; }
      var k = Number(name.slice(1));
      for (var i = 0; i < k; i++) state.rolls.push(R.int(6));
    },
    draw: function (v, P, k, state) {
      var r = state.rolls, n = r.length, max = (state.config && state.config.tiles_max) || 60;
      if (!n) return H.empty("Rzuć kostką: każdy rzut położy kafelek nad swoją ścianką");
      if (n <= max) {
        var height = [0, 0, 0, 0, 0, 0, 0], old = [], last = [], text = [];
        r.forEach(function (f, i) {
          var h = ++height[f], tile = [f - 0.4, f + 0.4, h - 0.43, h + 0.43];
          (i === n - 1 ? last : old).push(tile);
          text.push({ type: "text", x: f, y: h, text: String(f), color: P.surface, bold: true, valign: "middle", size: 0.9 });
        });
        var top = Math.max(10, H.max(height) + 1), line = n / 6 + 0.5;
        return { x: [0.5, 7], y: [0.5, top], xTicks: H.range(1, 6), noXGrid: true,
                 xName: "Liczba oczek", yName: "Liczba rzutów",
                 layers: [{ type: "rects", items: old, fill: P.secondary, stroke: P.surface, lineWidth: 1.5 },
                          { type: "rects", items: last, fill: P.accent, stroke: P.surface, lineWidth: 1.5 },
                          hline(line, 0.5, 7, P.accent, 1.5, [4, 4]),
                          { type: "text", x: 6.55, y: line, text: "n/6", color: P.accent, align: "left", bold: true }]
                          .concat(text),
                 legend: [{ text: "Kafelek: jeden rzut", color: P.secondary, box: true },
                          { text: "Ostatni rzut", color: P.accent, box: true },
                          { text: "Linia n/6: tyle, ile każdej ścianki „należałoby się”", color: P.accent, dash: true }],
                 aria: n + " rzutów kostką jako kafelki nad ściankami; liczebności " + height.slice(1).join(", ") +
                       " na tle linii n/6 = " + fmt(n / 6, 1) };
      }
      var f = H.tab(r, 6).map(function (c) { return c / n; });
      return { x: [0.5, 7], y: [0, 0.3], xTicks: H.range(1, 6), noXGrid: true,
               xName: "Liczba oczek", yName: "Częstość względna",
               layers: [{ type: "bins", data: H.bars(f, 0.8), color: P.secondary, alpha: 0.85 },
                        hline(1 / 6, 0.5, 7, P.accent, 1.5, [4, 4]),
                        { type: "text", x: 6.55, y: 1 / 6, text: "1/6", color: P.accent, align: "left", bold: true }]
                 .concat(f.map(function (t, i) { return { type: "text", x: i + 1, y: t, text: fmt(t, 3), color: P.ink }; })),
               legend: [{ text: n + " rzutów: kafelków za dużo, słupki pokazują odsetki", color: P.secondary, box: true },
                        { text: "1/6", color: P.accent, dash: true }],
               aria: "Częstości ścianek w " + n + " rzutach: " + f.map(function (t) { return fmt(t, 3); }).join(", ") + " na tle 1/6" };
    },
    update: function (root, state) {
      var r = state.rolls, n = r.length, cap = state.config && state.config.captions, max = (state.config && state.config.tiles_max) || 60;
      S.setValue(root, "n", String(n));
      S.setValue(root, "last", n ? String(r[n - 1]) : "–");
      var box = root.querySelector('[data-role="caption"]');
      if (box) box.hidden = !n;
      if (n && cap) S.setValue(root, "cap", n <= 12 ? cap.few : n <= max ? cap.tiles : cap.bars);
    }
  };

  // ---- Ryc. 2.2: ile wraca z 1 zł ------------------------------------------------
  // Oś liczby gier logarytmiczna: rysujemy log10(n) i podpisujemy potęgi dziesięciu.
  function logLabels(top) {
    var out = {};
    for (var e = 0; e <= Math.floor(top + 1e-9); e++) out[e] = C.fmt(Math.pow(10, e), 0, true);
    return out;
  }
  FIGS.back = {
    init: function (state) { state.wins = []; state.cum = []; },
    prepare: function (state, v, vals) {
      var game = v || "roulette", ev = vals && vals.s3p_2a_ev;
      // Zmiana gry czyści wyniki (jak observeEvent na wyborze gry w R).
      if (state.game !== game) { state.game = game; state.wins = []; state.cum = []; }
      state.showEv = ev === "on" || game === "all";
    },
    act: function (state, name) {
      if (name === "reset") { state.wins = []; state.cum = []; return; }
      var gm = state.config.games[state.game];
      if (!gm) return;
      var k = Number(name.slice(1)), s = state.cum.length ? state.cum[state.cum.length - 1] : 0;
      for (var i = 0; i < k; i++) {
        var w = gm.x[R.choose(gm.p) - 1];
        state.wins.push(w); s += w; state.cum.push(s);
      }
    },
    draw: function (v, P, k, state) {
      var cfg = state.config, game = state.game;
      if (game === "all") {
        var rows = cfg.order.map(function (key, i) { var g = cfg.games[key]; return { name: g.name, back: g.back, y: i + 1 }; });
        var yl = yLabels(rows.map(function (r) { return r.name; }));
        var layers = [vgrid([0, 0.25, 0.5, 0.75, 1, 1.25], 0.5, rows.length + 0.5, P.ruleSoft),
          { type: "rects", items: rows.map(function (r) { return [0, r.back, r.y - 0.32, r.y + 0.32]; }), fill: P.secondary },
          { type: "rects", items: rows.map(function (r) { return [r.back, 1, r.y - 0.32, r.y + 0.32]; }), fill: P.rule }];
        rows.forEach(function (r) {
          layers.push({ type: "text", x: r.back / 2, y: r.y, text: "wraca " + fmt(r.back, 2), color: P.surface, bold: true, valign: "middle", size: 1.05 },
                      { type: "text", x: 1.012, y: r.y, text: "zostaje " + fmt(1 - r.back, 2), color: P.ink, align: "left", valign: "middle" });
        });
        return { x: [0, 1.25], y: [0.5, rows.length + 0.5], xTicks: [0, 0.25, 0.5, 0.75, 1], yLabels: yl, noYGrid: true,
                 xName: "Z 1 zł stawki", layers: layers,
                 legend: [{ text: "Ciemne: średnio wraca do gracza", color: P.secondary, box: true },
                          { text: "Jasne: zostaje u organizatora", color: P.rule, box: true }],
                 aria: "Z 1 zł stawki średnio wraca: " + rows.map(function (r) { return r.name + " " + fmt(r.back, 2); }).join(", ") };
      }
      var gm = cfg.games[game], n = state.wins.length, ev = gm.back;
      var top = Math.log10(Math.max(10, n));
      var L = [hline(1, 0, top, P.ref, 1.5, [4, 4]),
               { type: "text", x: 0, y: 1, text: "gra „na zero”", color: P.ref, align: "left" }];
      if (state.showEv) {
        L.push(hline(ev, 0, top, P.accent, 2.5, [4, 4]),
               { type: "text", x: 0, y: ev - 0.1, text: "E = " + fmt(ev, 2) + " z 1 zł", color: P.accent, align: "left", valign: "middle", bold: true });
      }
      var aria = gm.name + ": " + n + " gier";
      if (n > 0) {
        var m = Math.min(n, 400), seen = {}, pts = [];
        for (var i = 0; i < m; i++) {
          var j = Math.round(Math.exp(m === 1 ? 0 : i * Math.log(n) / (m - 1)));
          if (seen[j]) continue;
          seen[j] = true;
          pts.push([Math.log10(j), state.cum[j - 1] / (j * gm.price)]);
        }
        L.push({ type: "line", data: pts, color: P.secondary, width: 2 },
               { type: "point", x: pts[pts.length - 1][0], y: pts[pts.length - 1][1], color: P.secondary, size: 10 });
        aria += ", średnio wraca " + fmt(pts[pts.length - 1][1], 2) + " z 1 zł";
      }
      return { x: [0, top], y: [0, 2], xLabels: logLabels(top), xName: "Liczba gier (skala log.)", layers: L,
               aria: aria + (state.showEv ? "; wartość oczekiwana " + fmt(ev, 2) + " z 1 zł" : "") };
    },
    update: function (root, state) {
      var cfg = state.config, game = state.game, all = game === "all", cap = cfg.captions;
      ["play", "reset", "reads"].forEach(function (r) { show(root.querySelector('[data-role="' + r + '"]'), !all); });
      var n = state.wins.length;
      if (!all) {
        var gm = cfg.games[game], won = n ? state.cum[n - 1] : 0;
        S.setValue(root, "n", C.fmt(n, 0, true));
        S.setValue(root, "spent", C.fmt(n * gm.price, 0, true) + " zł");
        S.setValue(root, "won", C.fmt(won, 0, true) + " zł");
        S.setValue(root, "back", n ? fmt(won / (n * gm.price), 2) : "–");
      }
      var calc = root.querySelector('[data-role="calc"]');
      if (calc) {
        if (state.showEv) {
          var tex = cfg.tex[game];
          if (calc.getAttribute("data-tex") !== game) {
            calc.setAttribute("data-tex", game);
            calc.innerHTML = "$$" + tex + "$$";
            typeset(calc);
          }
        }
        show(calc, state.showEv);
      }
      var text = !state.showEv ? (n === 0 ? cap.start : cap.hint)
        : game === "lotto" ? (n < 1000 ? cap.lotto_few : cap.lotto) : cap[game];
      S.setValue(root, "cap", text);
    }
  };

  // ---- Ryc. 2.3: ubezpieczenie telefonu -----------------------------------------
  FIGS.phone = {
    prepare: function (state, v) { state.prem = Number(v) || 250; },
    draw: function (v, P, k, state) {
      var cfg = state.config, prem = state.prem, risk = cfg.risk;
      var p = k === 0 ? [1 - risk, risk] : [1];
      var labels = k === 0 ? { 1: "0 zł\n(nic się nie stało)", 2: "−" + cfg.phone + " zł\n(nowy telefon)" }
                           : { 1: "−" + prem + " zł\n(składka)" };
      return { x: [0.4, p.length + 0.6], y: [0, 1.12], xLabels: labels, noXGrid: true, alignLeft: true,
               xName: "Bilans roku", yName: "Prawdopodobieństwo",
               layers: [{ type: "bins", data: H.bars(p, 0.5), color: P.secondary, alpha: 0.85 }]
                 .concat(p.map(function (t, i) { return { type: "text", x: i + 1, y: t, text: fmt(t, 2), color: P.ink }; })),
               aria: k === 0 ? "Bez ubezpieczenia: 0 zł z prawdopodobieństwem " + fmt(1 - risk, 2) +
                               ", strata " + cfg.phone + " zł z prawdopodobieństwem " + fmt(risk, 2)
                             : "Z ubezpieczeniem: pewna składka " + prem + " zł" };
    },
    update: function (root, state) {
      var prem = state.prem, cfg = state.config;
      S.setValue(root, "e2", fmt(-prem));
      S.setValue(root, "withp", fmt(-prem));
      S.setValue(root, "cap", prem > cfg.phone * cfg.risk
        ? "Telefon za 3000 zł psuje się albo ginie w ciągu roku z szansą 5%. Bez polisy tracisz średnio 150 zł, z polisą pewne " + prem +
          " zł. Rachunek E(X) mówi „nie ubezpieczaj”, a jednak wiele osób kupuje polisę: płaci za to, żeby nie trafić na stratę 3000 zł."
        : "Przy składce do 150 zł polisa wygrywa nawet w rachunku E(X). Ubezpieczyciel nie mógłby takiej sprzedawać: na każdej polisie średnio by tracił.");
    }
  };

  // ---- Ryc. 2.4: dwie trasy na egzamin ------------------------------------------
  function routeColors() { return { A: H.cat("indygo", "#3a5a8a"), B: H.cat("terakota", "#b25838") }; }
  function late(state) {
    var out = {}, rt = state.config.routes;
    Object.keys(rt).forEach(function (key) { out[key] = D.pgamma(state.t, rt[key].shape, rt[key].scale, false); });
    return out;
  }
  FIGS.routes = {
    prepare: function (state, v) { state.t = Number(v) || 35; },
    draw: function (v, P, k, state) {
      var rt = state.config.routes, t = state.t, col = routeColors(), xs = D.seq(0, 80, 600), fills = [], lines = [];
      ["A", "B"].forEach(function (key) {
        var r = rt[key], f = function (x) { return D.dgamma(x, r.shape, r.scale); };
        var tail = [[t, f(t)]].concat(xs.filter(function (x) { return x > t; }).map(function (x) { return [x, f(x)]; }));
        fills.push({ type: "line", data: tail, color: col[key], width: 0, fill: col[key], fillAlpha: 0.3 });
        lines.push({ type: "line", data: xs.map(function (x) { return [x, f(x)]; }), color: col[key], width: 2.5 });
      });
      var l = late(state);
      return { x: [0, 80], y: [0, 0.21], xTicks: [0, 20, 40, 60, 80], xName: "Czas przejazdu (min)", yName: "Gęstość",
               layers: fills.concat(lines, [{ type: "vline", x: t, color: P.ink, width: 2 },
                                            { type: "text", x: t, y: "top", text: "egzamin", color: P.ink, align: "left", bold: true, halo: true }]),
               legend: ["A", "B"].map(function (key) {
                 var r = rt[key];
                 return { text: r.name + " (średnio " + r.mean + " min, SD " + r.sd + " min)", color: col[key] };
               }),
               aria: "Gęstości czasu przejazdu dwiema trasami; egzamin za " + t + " min; szansa spóźnienia: A " +
                     pct(l.A) + ", B " + pct(l.B) };
    },
    update: function (root, state) {
      var l = late(state);
      S.setValue(root, "lateA", pct(l.A));
      S.setValue(root, "lateB", pct(l.B));
      S.setValue(root, "cap", "Trasa B jest średnio o 5 minut szybsza. Przy tym czasie bezpieczniej jechać " +
        (l.A < l.B ? "obwodnicą (A)" : "przez centrum (B)") +
        ". Gdy liczy się termin, o wyborze decyduje rozrzut, a nie średnia.");
    }
  };

  // ---- Ryc. 2.5: pasy wypłat -----------------------------------------------------
  var KEYS = ["A", "B", "C"], ROWS = { A: 3, B: 2, C: 1 };
  function payColors() { return { A: H.cat("grafit", "#2d3b3d"), B: H.cat("terakota", "#b25838"), C: H.cat("indygo", "#3a5a8a") }; }
  function popSd(a) { var m = H.mean(a), s = 0; for (var i = 0; i < a.length; i++) s += (a[i] - m) * (a[i] - m); return Math.sqrt(s / a.length); }
  FIGS.pays = {
    init: function (state) { state.pays = { A: [], B: [], C: [] }; },
    prepare: function (state, v) { state.sd = v === "on"; },
    act: function (state, name) {
      if (name === "reset") { state.pays = { A: [], B: [], C: [] }; return; }
      var k = Number(name.slice(1)), lots = state.config.lots;
      KEYS.forEach(function (key) {
        var l = lots[key];
        for (var i = 0; i < k; i++) state.pays[key].push(l.x.length === 1 ? l.x[0] : l.x[R.choose(l.p) - 1]);
      });
    },
    draw: function (v, P, k, state) {
      var lots = state.config.lots, p = state.pays, n = p.A.length, col = payColors();
      var yl = yLabels(["C", "B", "A"].map(function (key) { return lots[key].name; }));
      var L = [vgrid([0, 20, 40, 60, 80, 100], 0.5, 3.85, P.ruleSoft),
               { type: "vline", x: 50, color: P.accent, width: 2, dash: [4, 4] },
               { type: "text", x: 50, y: 3.75, text: "E = 50 zł", color: P.accent, bold: true, halo: true }];
      if (n > 0) {
        // Rozrzut punktów stały między przerysowaniami: ziarno = liczba gier (set.seed(n) w R).
        var g = D.rng(n), alpha = n > 50 ? 0.35 : 0.8;
        KEYS.forEach(function (key) {
          var jx = g.many(n, function () { return g.unif(-2.5, 2.5); }), jy = g.many(n, function () { return g.unif(-0.22, 0.22); });
          L.push({ type: "points", data: p[key].map(function (x, i) { return [x + jx[i], ROWS[key] + jy[i]]; }),
                   color: col[key], alpha: alpha, size: 8 });
        });
      }
      if (state.sd) {
        KEYS.forEach(function (key) {
          var s = n > 1 ? popSd(p[key]) : state.config.sd[key], y = ROWS[key] - 0.33, lo = 50 - s, hi = 50 + s;
          L.push({ type: "segs", width: 2, z: 6, color: P.ink, items: [
                     { from: [lo, y], to: [hi, y] }, { from: [lo, y - 0.04], to: [lo, y + 0.04] }, { from: [hi, y - 0.04], to: [hi, y + 0.04] }] },
                 { type: "text", x: 50, y: y, text: "SD ≈ " + fmt(s, 0) + " zł", color: P.ink, bold: true, halo: true, size: 0.9 });
        });
      }
      return { x: [-5, 105], y: [0.5, 3.85], xTicks: [0, 20, 40, 60, 80, 100], yLabels: yl, noYGrid: true,
               xName: "Wypłata w jednej grze (zł)", layers: L,
               aria: n ? n + " gier w każdą loterię; średnie wypłaty A / B / C: " +
                         KEYS.map(function (key) { return fmt(H.mean(p[key]), 0); }).join(" / ") + " zł; E = 50 zł"
                       : "Trzy loterie o tej samej wartości oczekiwanej 50 zł; jeszcze bez gier" };
    },
    update: function (root, state) {
      var p = state.pays, n = p.A.length, cap = state.config.captions;
      S.setValue(root, "n", String(n));
      S.setValue(root, "means", n ? KEYS.map(function (key) { return fmt(H.mean(p[key]), 0); }).join(" / ") : "–");
      S.setValue(root, "cap", n ? cap.played : cap.start);
    }
  };

  S.register({ "d03-dice": FIGS.dice, "d03-back": FIGS.back, "d03-phone": FIGS.phone,
               "d03-routes": FIGS.routes, "d03-pays": FIGS.pays });
})();
