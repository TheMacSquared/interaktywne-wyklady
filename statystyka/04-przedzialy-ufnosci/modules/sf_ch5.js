// Ryciny rozdziału 5 wykładu 04 liczone w przeglądarce (silnik lcSF, rozkłady lcDist).
// UI: ch5_czynniki.R i figures_ch5.R. Działają także offline, bez serwera R.
//
//   c5-poll    — Ryc. 5.1: pracownia dokupuje ankiety (przedziały i ścieżka koszt–margines)
//   c5-factors — Ryc. 5.2: jak zmienia się szerokość przedziału (krzywa ME(n) i pasek CI)
//   c5-plan    — Ryc. 5.3: kalkulator wielkości próby (wzór, krzywa ME(n), osiągnięte ME)
//   c5-cmp     — Ryc. 5.4: trzy poziomy ufności (statystyki prób policzone w R, config)
//   c5-edge    — Przykład 1–3: poziom ufności zmienia werdykt (dane przypadku w config)
//   c5-anatomy — Ryc. 5.5: anatomia przedziału (krokowa, teksty kroków w config)
//
// Grubości i rozmiary jak w moście ggplot→ECharts: linia = 2 × linewidth,
// punkt = 2.1 × size, napis = size / 4.5 rozmiaru czcionki osi.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, D = window.lcDist;
  if (!C || !S || !D) return;
  var fmt = S.fmt, esc = S.esc;

  function lw(v) { return v * 2; }
  function pt(v) { return v * 2.1; }
  function ts(v) { return Math.max(1, v / 4.5); }
  // Rozszerzenie skali jak w ggplot (expand = 5% z każdej strony).
  function expand(lo, hi) { var d = (hi - lo) * 0.05 || 0.5; return [lo - d, hi + d]; }
  function range(a) { return [Math.min.apply(null, a), Math.max.apply(null, a)]; }
  function num(root, id) {
    var el = root && root.querySelector("#" + id);
    return el ? parseFloat(el.value) : NaN;
  }
  function colors() {
    return { hyp: C.token("--upwr-cat-indygo", "#8e44ad"), wrzos: C.token("--upwr-cat-wrzos", "#a07894") };
  }
  // Poziomy pasek przedziału jak geom_errorbar(orientation = "y"): odcinek i dwa wąsy.
  function errorbar(lo, hi, y, cap, color, width, alpha) {
    return { type: "segs", color: color, width: width, alpha: alpha, z: 4, items: [
      { from: [lo, y], to: [hi, y] },
      { from: [lo, y - cap / 2], to: [lo, y + cap / 2] },
      { from: [hi, y - cap / 2], to: [hi, y + cap / 2] }] };
  }
  function diamond(x, y, color, size) { return { type: "point", x: x, y: y, color: color, symbol: "diamond", size: pt(size) }; }
  function readout(label, value, color) {
    return '<div class="lc-read" style="--lc-read-color:' + color + ';"><span class="lc-read-label">' + esc(label) +
           '</span><span class="lc-read-value">' + esc(value) + "</span></div>";
  }
  function setHTML(root, role, html) {
    var el = root && root.querySelector('[data-role="' + role + '"]');
    // Porównanie z ostatnio wpisanym tekstem, nie z innerHTML: MathJax podmienia
    // treść przy składaniu wzoru i każde przerysowanie cofałoby wzór do źródła.
    if (el && el.lcHtml !== html) { el.lcHtml = html; el.innerHTML = html; return el; }
    return null;
  }

  // ---- Ryc. 5.1: pracownia dokupuje ankiety --------------------------------------
  // Margines błędu proporcji przy p(1 − p) = 0.25, z* = 1.96.
  function pollMe(n) { return 1.96 * Math.sqrt(0.25 / n); }
  var POLL = {
    init: function (state) { state.ns = [100]; },
    act: function (state, a) {
      var last = state.ns[state.ns.length - 1];
      if (a === "dokup") state.ns = state.ns.concat([last + 100]);
      else if (a === "double") state.ns = state.ns.concat([2 * last]);
      else if (a === "reset") state.ns = [100];
    },
    draw: function (v, P, k, state) {
      var ns = state.ns, now = ns[ns.length - 1];
      if (k === 0) {
        // Bieżący przedział (gruby) i do 12 poprzednich jako blade ślady; oś w procentach.
        var hist = ns.slice(0, -1).reverse().slice(0, 12), layers = [
          { type: "vline", x: 50, color: P.ref, width: 1.2, dash: [2, 3] }];
        if (hist.length) layers.push({ type: "segs", color: P.niebo, alpha: 0.22, width: lw(1.6), items: hist.map(function (n, i) {
          var me = 100 * pollMe(n); return { from: [50 - me, -(i + 1)], to: [50 + me, -(i + 1)] }; }) });
        var meNow = 100 * pollMe(now);
        layers.push({ type: "segment", from: [50 - meNow, 0], to: [50 + meNow, 0], color: P.niebo, width: lw(3.4) },
                    { type: "point", x: 50, y: 0, color: P.bursztyn, size: pt(3) });
        return { x: expand(30, 70), y: expand(-hist.length - 0.7, 0.7), hideY: true, noYGrid: true,
                 xName: "Poparcie (%)", xLabels: { 30: "30%", 40: "40%", 50: "50%", 60: "60%", 70: "70%" },
                 layers: layers,
                 aria: "Przedziały 50% ± margines błędu dla sondażu, najnowszy na górze, poprzednie blade; " +
                       "bieżący: ± " + fmt(meNow, 1) + " pkt proc. przy " + now + " ankietach" };
      }
      var pts = ns.map(function (n) { return [20 * n / 1000, 100 * pollMe(n)]; }), lastPt = pts[pts.length - 1];
      var xmax = Math.max(5, Math.max.apply(null, pts.map(function (p) { return p[0]; })) * 1.08);
      var xr = expand(0, xmax), path = [
        { type: "line", data: [[xr[0], 3], [xr[1], 3]], color: P.ref, width: 1.2, dash: [2, 3] },
        { type: "text", x: 0, y: 3, text: "±3 pkt: typowy sondaż", color: P.ref, align: "left", size: ts(3.6) }];
      if (pts.length > 1) path.push({ type: "line", data: pts, color: P.niebo, width: lw(1) });
      path.push({ type: "points", data: pts, color: P.niebo, size: pt(2.2) },
                { type: "point", x: lastPt[0], y: lastPt[1], color: P.accent, size: pt(4) });
      return { x: xr, y: expand(0, 11), noXGrid: true, layers: path,
               xName: "Koszt badania (tys. zł)", yName: "Margines błędu (pkt proc.)",
               aria: "Margines błędu w pkt proc. w zależności od kosztu badania w tys. zł" };
    },
    update: function (root, state) {
      var n = state.ns[state.ns.length - 1];
      S.setValue(root, "poll_n", C.fmt(n, 0, true));
      S.setValue(root, "poll_me", "± " + fmt(100 * pollMe(n), 1) + " pkt proc.");
      S.setValue(root, "poll_cost", C.fmt(20 * n, 0, true) + " zł");
    }
  };

  // ---- Ryc. 5.2: jak zmienia się szerokość przedziału ----------------------------
  // Kwantyle t z pamięcią: suwak przelicza krzywą ME(n) przy każdym ruchu.
  var T_CACHE = {};
  function tStar(conf, df) {
    var key = conf + ":" + df;
    if (!(key in T_CACHE)) T_CACHE[key] = D.qt(1 - (1 - conf) / 2, Math.max(df, 1));
    return T_CACHE[key];
  }
  function factorsVals(root) {
    var n = num(root, "ch5_n"), conf = num(root, "ch5_conf"), s = num(root, "ch5_s");
    var t = tStar(conf, n - 1);
    return { n: n, conf: conf, s: s, t: t, me: t * s / Math.sqrt(n) };
  }
  var FACTORS = {
    init: function (state, root) { state.root = root; },
    draw: function (v, P, k, state) {
      var f = factorsVals(state.root), xbar = 170;
      if (k === 0) {
        var curve = [];
        for (var n = 5; n <= 100; n++) curve.push([n, tStar(f.conf, n - 1) * f.s / Math.sqrt(n)]);
        var yr = range(curve.map(function (p) { return p[1]; }).concat([f.me + 0.3])), xf = expand(5, Math.max(100, f.n + 4));
        return { x: xf, y: expand(yr[0], yr[1]), alignLeft: true,
                 xName: "Wielkość próby (n)", yName: "Margines błędu (ME)",
                 layers: [
                   { type: "line", data: curve, color: P.niebo, width: lw(1.2) },
                   { type: "line", data: [[xf[0], f.me], [xf[1], f.me]], color: P.bursztyn, width: 1.2, dash: [2, 3] },
                   { type: "point", x: f.n, y: f.me, color: P.bursztyn, size: pt(4) },
                   { type: "text", x: f.n + 4, y: f.me + 0.3, text: "ME", color: P.bursztyn, bold: true, valign: "middle", size: ts(4.5) }],
                 aria: "Margines błędu w zależności od n przy poziomie ufności " + Math.round(f.conf * 100) +
                       "% i s = " + f.s + "; przy n = " + f.n + " ME = " + fmt(f.me, 2) };
      }
      // Stała oś: najszerszy przedział (n = 5, 99%, s = 12).
      var worst = D.qt(0.995, 4) * 12 / Math.sqrt(5);
      return { x: expand(xbar - worst * 1.05, xbar + worst * 1.05), y: expand(-0.6, 0.6), alignLeft: true,
               hideY: true, noYGrid: true, xName: "Wartość (np. wzrost w cm)",
               layers: [
                 { type: "vline", x: xbar, color: P.ref, width: lw(0.6), dash: [6, 4] },
                 { type: "text", x: xbar, y: 0.5, text: "środek", color: P.ref, align: "left", valign: "middle", size: ts(4) },
                 errorbar(xbar - f.me, xbar + f.me, 0, 0.18, P.niebo, lw(2.4), 0.7),
                 diamond(xbar, 0, P.bursztyn, 7),
                 { type: "text", x: xbar, y: -0.42, text: Math.round(f.conf * 100) + "% CI", color: P.niebo, bold: true, valign: "middle", size: ts(4.8) }],
               aria: "Przedział " + fmt(xbar - f.me, 2) + "–" + fmt(xbar + f.me, 2) + " wokół środka 170 na stałej osi" };
    },
    update: function (root) {
      var f = factorsVals(root);
      S.setValue(root, "f_me", fmt(f.me, 2));
      S.setValue(root, "f_width", fmt(2 * f.me, 2));
      S.setValue(root, "f_t", fmt(f.t, 3));
    }
  };

  // ---- Ryc. 5.3: kalkulator wielkości próby ---------------------------------------
  function planVals(root) {
    var me = num(root, "ch5_plan_me"), s = num(root, "ch5_plan_s"), conf = num(root, "ch5_plan_conf");
    if (!(me > 0) || !(s > 0) || !(conf > 0 && conf < 1)) return null;
    var z = D.qnorm(1 - (1 - conf) / 2), raw = Math.pow(z * s / me, 2);
    var n = Math.ceil(raw);
    return { me: me, s: s, conf: conf, z: z, raw: raw, n: n, meActual: z * s / Math.sqrt(n) };
  }
  var PLAN = {
    init: function (state, root) { state.root = root; },
    draw: function (v, P, k, state) {
      var p = planVals(state.root), center = 100;
      if (!p) return null;
      if (k === 0) {
        var top = Math.max(p.n * 2, 100), step = Math.max(1, Math.ceil((top - 5) / 600)), curve = [];
        for (var n = 5; n <= top; n += step) curve.push([n, p.z * p.s / Math.sqrt(n)]);
        if (curve[curve.length - 1][0] !== top) curve.push([top, p.z * p.s / Math.sqrt(top)]);
        var yr = range(curve.map(function (q) { return q[1]; }).concat([p.me, p.me + 0.3])), xp = expand(5, top);
        return { x: xp, y: expand(yr[0], yr[1]), alignLeft: true, xName: "n", yName: "Margines błędu",
                 layers: [
                   { type: "line", data: curve, color: P.niebo, width: lw(1.2) },
                   { type: "line", data: [[xp[0], p.me], [xp[1], p.me]], color: P.terakota, width: lw(1), dash: [6, 4] },
                   { type: "point", x: p.n, y: p.me, color: P.szalwia, size: pt(5) },
                   { type: "text", x: p.n, y: p.me + 0.3, text: "n = " + p.n, color: P.szalwia, bold: true, valign: "middle", size: ts(5) }],
                 aria: "Margines błędu w zależności od n; wymagany margines " + p.me + " osiągany przy n = " + p.n };
      }
      return { x: expand(center - 3 * p.me, center + 3 * p.me), y: expand(-0.6, 0.6), alignLeft: true,
               hideY: true, noYGrid: true, xName: "Wartość (jednostki dowolne)",
               layers: [
                 { type: "rect", x0: center - p.me, x1: center + p.me, y0: -0.66, y1: 0.66, fill: P.rule, alpha: 0.4, z: 1 },
                 { type: "vline", x: center, color: P.ref, width: lw(0.6), dash: [6, 4] },
                 errorbar(center - p.meActual, center + p.meActual, 0, 0.18, P.szalwia, lw(2.4), 0.8),
                 diamond(center, 0, P.bursztyn, 7),
                 { type: "text", x: center, y: -0.42, text: "Osiągnięte ME ≤ wymagane ✓", color: P.szalwia, bold: true, valign: "middle", size: ts(4.8) }],
               aria: "Przedział przy n = " + p.n + " mieści się w dopuszczalnej strefie ± " + p.me };
    },
    update: function (root) {
      var p = planVals(root);
      var html = !p ? "" : '<div class="lc-status" role="status" aria-live="polite"><p><span class="lc-status-ok"><strong>Wymagana wielkość próby:</strong></span></p>' +
        // Wzór jako HTML: wersja offline nie ma MathJax.
        "<p><i>n</i> = (" + fmt(p.z, 3) + " · " + p.s + " / " + p.me + ")² = " +
        fmt(p.raw, 1) + " ≈ <b>" + p.n + "</b></p></div>";
      setHTML(root, "plan_result", html);
    }
  };

  // ---- Ryc. 5.4: trzy poziomy ufności ---------------------------------------------
  // config.data: {klucz: {xbar, s, n}} policzone w R (set.seed(42) i pliki z dane/).
  var LEVELS = [0.90, 0.95, 0.99];
  function cmpRows(d) {
    return LEVELS.map(function (conf) {
      var me = tStar(conf, d.n - 1) * d.s / Math.sqrt(d.n);
      return { conf: conf, label: Math.round(conf * 100) + "%", lower: d.xbar - me, upper: d.xbar + me, me: me };
    });
  }
  var CMP = {
    act: function (state, a, root) {
      if (a !== "calc") return;
      var sel = root.querySelector("#ch5_cmp_data");
      state.key = sel ? sel.value : null;
    },
    draw: function (v, P, k, state) {
      var d = state.key && state.config.data[state.key];
      if (!d) {
        return { x: [0, 1], y: [0, 1], xTicks: [], hideY: true, noXGrid: true, noYGrid: true,
                 layers: [{ type: "text", x: 0.5, y: 0.5, text: "Kliknij „Oblicz 3 przedziały”", color: P.ref, valign: "middle", size: ts(6) }],
                 aria: "Wykres pojawi się po kliknięciu „Oblicz 3 przedziały”" };
      }
      var rows = cmpRows(d), cols = [P.bursztyn, P.niebo, colors().wrzos], layers = [];
      rows.forEach(function (r, i) {
        var y = 3 - i;
        layers.push(errorbar(r.lower, r.upper, y, 0.3, cols[i], lw(2), 1),
                    diamond(d.xbar, y, P.bursztyn, 4),
                    { type: "text", x: d.xbar, y: y + 0.28, text: "[" + fmt(r.lower, 2) + " ; " + fmt(r.upper, 2) + "]",
                      color: P.ink, valign: "middle", size: ts(4) });
      });
      return { x: expand(rows[2].lower, rows[2].upper), y: expand(0.85, 3.28), noYGrid: true,
               yLabels: { 1: "99%", 2: "95%", 3: "90%" }, xName: "Wartość", yName: "Poziom ufności", layers: layers,
               aria: "Trzy przedziały ufności z jednej próby: " + rows.map(function (r) {
                 return r.label + " od " + fmt(r.lower, 2) + " do " + fmt(r.upper, 2); }).join(", ") };
    },
    update: function (root, state, v, P) {
      var d = state.key && state.config.data[state.key], html = "";
      if (d) {
        var P2 = C.palette(), cols = [P2.bursztyn, P2.niebo, colors().wrzos];
        html = cmpRows(d).map(function (r, i) { return readout(r.label, "±" + fmt(r.me, 2), cols[i]); }).join("");
      }
      setHTML(root, "cmp_stats", html);
    }
  };

  // ---- Przykład 1–3: poziom ufności zmienia werdykt -----------------------------------
  // config: {kind: "mean"|"prop", xbar, s, n | x, n, text, bound, dir, xlab}.
  function edgeCi(c, conf) {
    if (c.kind === "mean") {
      var t = tStar(conf, c.n - 1), me = t * c.s / Math.sqrt(c.n);
      return { center: c.xbar, lower: c.xbar - me, upper: c.xbar + me, me: me };
    }
    var p = c.x / c.n, z = D.qnorm(1 - (1 - conf) / 2), m = z * Math.sqrt(p * (1 - p) / c.n);
    return { center: p, lower: p - m, upper: p + m, me: m };
  }
  function edgeVerdict(lo, hi, bound, dir) {
    if (dir === "gt") return lo > bound ? "yes" : hi < bound ? "no" : "maybe";
    return hi < bound ? "yes" : lo > bound ? "no" : "maybe";
  }
  var VERDICT = { yes: ["ok", "TAK"], no: ["danger", "NIE"], maybe: ["warning", "NIEPEWNE"] };
  function edgeExplain(c, conf, revealed) {
    var hyp = "<p><strong>Hipoteza:</strong> " + esc(c.text) + "</p>", pc = conf == null ? null : Math.round(conf * 100);
    if (conf == null) return S.status(hyp + "<p><em>Wybierz poziom ufności (90%, 95% lub 99%), żeby zobaczyć przedział.</em></p>");
    if (!revealed) {
      return S.status(hyp + "<p><strong>Poziom ufności:</strong> " + pc + "%</p>" +
        "<p><em>Gdzie leży przedział względem granicy hipotezy? Przycisk „Pokaż werdykt” odsłoni odpowiedź.</em></p>");
    }
    var ci = edgeCi(c, conf), v = edgeVerdict(ci.lower, ci.upper, c.bound, c.dir), body;
    if (v === "yes") body = "Cały przedział " + pc + "% leży w obszarze hipotezy. Przy tym poziomie ufności możemy stwierdzić: " +
      esc(c.text.charAt(0).toLowerCase() + c.text.slice(1)) + ".";
    else if (v === "no") body = "Cały przedział " + pc + "% leży poza obszarem hipotezy. Dane przemawiają przeciwko niej.";
    else body = "Przedział " + pc + "% przecina granicę hipotezy (" + fmt(c.bound, 3) + "). Dane są zgodne z wartościami " +
      "po obu stronach progu, więc przy tym poziomie ufności nie możemy hipotezy ani przyjąć, ani odrzucić. Sprawdź pozostałe poziomy.";
    return S.status(hyp + '<p><span class="lc-status-' + VERDICT[v][0] + '"><strong>Werdykt przy ' + pc + "% ufności: " +
      VERDICT[v][1] + "</strong></span></p><p>" + body + "</p>");
  }
  var EDGE = {
    init: function (state) { state.conf = null; state.revealed = false; },
    act: function (state, a) {
      var m = /^c(\d+)$/.exec(a);
      if (m) { state.conf = Number(m[1]) / 100; state.revealed = false; }
      else if (a === "reveal" && state.conf != null) state.revealed = true;
    },
    draw: function (v, P, k, state) {
      var c = state.config, hyp = colors().hyp, hi99 = edgeCi(c, 0.99);
      var xr = range([hi99.lower, hi99.upper, c.bound]), pad = (xr[1] - xr[0]) * 0.2;
      var xl = [xr[0] - pad, xr[1] + pad], x = expand(xl[0], xl[1]), y = expand(-0.65, 0.65), center = hi99.center;
      var layers = [
        c.dir === "gt" ? { type: "rect", x0: c.bound, x1: x[1], y0: y[0], y1: y[1], fill: hyp, alpha: 0.15, z: 1 }
                       : { type: "rect", x0: x[0], x1: c.bound, y0: y[0], y1: y[1], fill: hyp, alpha: 0.15, z: 1 },
        { type: "vline", x: c.bound, color: hyp, width: lw(1) },
        { type: "text", x: c.bound, y: 0.55, text: (c.dir === "gt" ? "≥ " : "≤ ") + c.bound, color: hyp, bold: true,
          align: "left", valign: "middle", size: ts(4.5) },
        diamond(center, 0, P.bursztyn, 7),
        { type: "text", x: center, y: -0.22, text: (c.kind === "mean" ? "x̄ = " : "p̂ = ") + fmt(center, 3), color: P.bursztyn,
          bold: true, valign: "middle", size: ts(4.5) }];
      var aria = c.xlab + ": estymata " + fmt(center, 3) + ", granica hipotezy " + c.bound;
      if (state.conf != null) {
        var ci = edgeCi(c, state.conf);
        layers.push(errorbar(ci.lower, ci.upper, 0, 0.18, P.niebo, lw(2.4), 0.7),
          { type: "text", x: center, y: 0.24, text: Math.round(state.conf * 100) + "% CI: [" + fmt(ci.lower, 3) + " ; " + fmt(ci.upper, 3) + "]",
            color: P.niebo, bold: true, valign: "middle", size: ts(4.8) });
        aria += "; przedział " + Math.round(state.conf * 100) + "% od " + fmt(ci.lower, 3) + " do " + fmt(ci.upper, 3);
      } else {
        layers.push({ type: "text", x: (xl[0] + xl[1]) / 2, y: 0.35, text: "Wybierz poziom ufności powyżej", color: P.ref,
                      italic: true, valign: "middle", size: ts(4.5) });
      }
      return { x: x, y: y, hideY: true, noYGrid: true, xName: c.xlab, layers: layers, aria: aria };
    },
    update: function (root, state) {
      root.querySelectorAll("[data-stat-act]").forEach(function (b) {
        var m = /^c(\d+)$/.exec(b.getAttribute("data-stat-act"));
        if (!m) return;
        var on = state.conf != null && Math.abs(state.conf - Number(m[1]) / 100) < 1e-9;
        b.classList.remove(on ? "is-outline" : "is-solid");
        b.classList.add(on ? "is-solid" : "is-outline");
        b.setAttribute("aria-pressed", on ? "true" : "false");
      });
      var rev = root.querySelector('[data-stat-act="reveal"]');
      if (rev) {
        rev.hidden = !(state.conf != null && !state.revealed);
      }
      setHTML(root, "edge_explain", edgeExplain(state.config, state.conf, state.revealed));
    }
  };

  // ---- Ryc. 5.5: anatomia przedziału (krokowa) ----------------------------------------
  // Kroki dokładają warstwy: 1 estymata, 2 ±1 SE, 3 95%, 4 warianty, 5 próg 26.
  var ANATOMY = {
    draw: function (v, P, k, state, vals, step) {
      var xbar = 28.5, s = 8, n = 40, se = s / Math.sqrt(n);
      function ci(seV, df, p) { var t = D.qt(p, df); return [xbar - t * seV, xbar + t * seV]; }
      var main = ci(se, n - 1, 0.975), variants = [
        { y: -1, lab: "n × 4", r: ci(s / Math.sqrt(4 * n), 4 * n - 1, 0.975) },
        { y: -2, lab: "s / 2", r: ci((s / 2) / Math.sqrt(n), n - 1, 0.975) },
        { y: -3, lab: "90%", r: ci(se, n - 1, 0.95) }];
      var layers = [], label = function (y, t) {
        return { type: "text", x: 21.7, y: y, text: t, color: P.soft, align: "left", valign: "middle", size: ts(3.6) }; };
      var aria = "Czas dojazdu: estymata 28.5 min";
      if (step >= 5) layers.push({ type: "rect", x0: 26, x1: 35, y0: -3.7, y1: 1, fill: P.accent, alpha: 0.07, z: 1 });
      if (step >= 4) {
        layers.push({ type: "segs", color: P.niebo, alpha: 0.6, width: lw(1.2), items: variants.map(function (w) {
          return { from: [w.r[0], w.y], to: [w.r[1], w.y] }; }) });
        variants.forEach(function (w) { layers.push(label(w.y, w.lab)); });
        aria += "; węższe warianty: n × 4, s / 2, 90%";
      }
      if (step >= 2) {
        layers.push({ type: "segs", color: P.niebo, alpha: 0.45, width: lw(7), items: [{ from: [xbar - se, 0], to: [xbar + se, 0] }] });
        aria += "; ±1 SE = " + fmt(se, 2);
      }
      if (step >= 3) {
        layers.push(errorbar(main[0], main[1], 0, 0.36, P.niebo, lw(1.6), 1), label(0, "95%"));
        aria += "; przedział 95% od " + fmt(main[0], 1) + " do " + fmt(main[1], 1) + " min";
      }
      layers.push(diamond(xbar, 0, P.bursztyn, 6));
      if (step >= 5) {
        layers.push({ type: "vline", x: 26, color: P.secondary, width: 1.2, dash: [4, 4] },
                    { type: "text", x: 26, y: 0.75, text: "próg 26 min", color: P.secondary, align: "left", valign: "middle", size: ts(3.6) });
        aria += "; próg 26 min wewnątrz przedziału";
      }
      return { x: expand(21.5, 35), y: expand(-3.7, 1), xTicks: [22, 24, 26, 28, 30, 32, 34], hideY: true, noYGrid: true,
               xName: "Czas dojazdu (min)", layers: layers, aria: aria };
    },
    text: function (state, v, vals, step) { return esc(state.config.texts[step - 1]); }
  };

  S.register({ "c5-poll": POLL, "c5-factors": FACTORS, "c5-plan": PLAN, "c5-cmp": CMP, "c5-edge": EDGE, "c5-anatomy": ANATOMY });
  // Do testów: pomocniki liczące.
  window.lc04Ch5 = { pollMe: pollMe, tStar: tStar, edgeCi: edgeCi, edgeVerdict: edgeVerdict, cmpRows: cmpRows };
})();
