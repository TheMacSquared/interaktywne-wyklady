// Sceny wykładu 02: kadry historii, które budują obraz związku dwóch zmiennych.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper).
// config.kind:
//   "stop"    kontrola drogowa: płeć × mandat, od kolejki do tabeli i procentów
//   "branze"  pensje absolwentów: jedna oś, trzy branże, mediany, pudełka
//   "staz"    staż a pensja: osoba → punkt → chmura → krzyż średnich
// Kadr wybiera pasek kroków widgetu (data-lc-step); przy zmianie kroku scena
// przesuwa obiekty z poprzedniego kadru do nowego. Każdy kind zwraca:
//   tokens(step) → tablica obiektów {x, y, s, o, ...} (te same indeksy w każdym kadrze)
//   back(step, g), front(step, g) → elementy stałe kadru (rysowane po animacji)
//   draw(g, t) → jeden obiekt
// Rysuje SVG; serwer R nie bierze udziału (tekst kroków renderuje R).
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640;
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function fmt(x, d) { return Number(x).toFixed(d); }
  function ease(u) { return u * u * (3 - 2 * u); }
  function lerp(a, b, u) { return a + (b - a) * u; }

  // Animacja klatkowa z możliwością przerwania (nowy kadr przerywa stary).
  function tween(ms, step, done) {
    var stop = false;
    if (REDUCE || ms <= 0) { step(1); done(); return function () {}; }
    var t0 = null;
    function frame(t) {
      if (stop) return;
      if (t0 === null) t0 = t;
      var u = Math.min(1, (t - t0) / ms);
      step(u);
      if (u < 1) requestAnimationFrame(frame); else done();
    }
    requestAnimationFrame(frame);
    return function () { stop = true; };
  }

  // Odczyt pod sceną: fragmenty [{t: "tekst", c: "klasa"}], opcjonalnie kropka koloru.
  function readout(g, x, y, parts, anchor) {
    var t = svg("text", { x: x, y: y, "text-anchor": anchor || "start", class: "lc-sc-n" }, g);
    parts.forEach(function (p) { svg("tspan", p.c ? { class: p.c } : {}, t, p.t); });
    return t;
  }

  function person(g, x, y, s, cls, o) {
    var q = svg("g", { class: "lc-sc-person " + cls, opacity: o }, g);
    svg("circle", { cx: x, cy: y - s * 0.8, r: s * 0.17 }, q);
    svg("rect", { x: x - s * 0.18, y: y - s * 0.6, width: s * 0.36, height: s * 0.6, rx: s * 0.13 }, q);
    return q;
  }

  var KINDS = {};

  // =========================================================================
  // STOP: kontrola drogowa. cfg.sex[i] ("K"/"M"), cfg.fine[i] (1 = mandat).
  // Kadry: 1 kolejka zatrzymanych, 2 mandat albo pouczenie, 3 tabela 2×2,
  // 4 każdy wiersz jako 100%.
  // =========================================================================
  KINDS.stop = function (cfg) {
    var N = cfg.sex.length, H = 360;
    var ROWS = ["K", "M"], ROWNAME = { K: "kobiety", M: "mężczyźni" };
    // tabela (kadr 3) i paski (kadr 4)
    var COLX = { 1: [170, 320], 0: [340, 620] };          // mandat | pouczenie
    var ROWY = { K: [62, 138], M: [158, 262] };
    var GX = 20, GY = 28, NUMW = 34;                       // siatka ludzików, miejsce na liczbę
    var BAR = { x0: 170, x1: 610, h: 30 };
    var cnt = { K: { 0: 0, 1: 0 }, M: { 0: 0, 1: 0 } };
    var rank = new Array(N);                               // pozycja w komórce
    var rowRank = new Array(N);                            // pozycja w wierszu (mandaty najpierw)
    var nRow = { K: 0, M: 0 };
    for (var i = 0; i < N; i++) { rank[i] = cnt[cfg.sex[i]][cfg.fine[i]]++; nRow[cfg.sex[i]]++; }
    var seen = { K: 0, M: 0 };
    for (var f = 1; f >= 0; f--) {
      for (var j = 0; j < N; j++) if (cfg.fine[j] === f) rowRank[j] = seen[cfg.sex[j]]++;
    }
    function pct(r) { return 100 * cnt[r][1] / nRow[r]; }
    function barY(r) { return (ROWY[r][0] + ROWY[r][1]) / 2; }

    function tokens(step) {
      var out = [];
      for (var i = 0; i < N; i++) {
        var sx = cfg.sex[i], fi = cfg.fine[i], t = { sex: sx, fine: fi, badge: step >= 2 };
        if (step <= 2) {
          t.x = 62 + (i % 14) * 40; t.y = 72 + Math.floor(i / 14) * 58; t.s = 34; t.o = 1;
        } else if (step === 3) {
          var cx = COLX[fi], cy = ROWY[sx], per = Math.floor((cx[1] - cx[0] - 12 - NUMW) / GX);
          t.x = cx[0] + 16 + (rank[i] % per) * GX; t.y = cy[0] + 32 + Math.floor(rank[i] / per) * GY;
          t.s = 20; t.o = 1;
        } else {
          var w = (BAR.x1 - BAR.x0) / nRow[sx];
          t.x = BAR.x0 + (rowRank[i] + 0.5) * w; t.y = barY(sx) + 8; t.s = 12; t.o = 0;
        }
        out.push(t);
      }
      return out;
    }

    function draw(g, t) {
      var q = person(g, t.x, t.y, t.s, t.sex === "K" ? "is-k" : "is-m", t.o);
      if (t.badge && t.o > 0) {
        svg("rect", { x: t.x + t.s * 0.12, y: t.y - t.s * 0.52, width: t.s * 0.26, height: t.s * 0.34,
          rx: t.s * 0.04, class: "lc-sc-ticket " + (t.fine ? "is-fine" : "is-warn") }, q);
      }
    }

    function legend(g, y) {
      var t = readout(g, 62, y, []);
      svg("tspan", { class: "lc-sc-sw is-k" }, t, "● ");
      svg("tspan", {}, t, "kobiety " + nRow.K + "   ");
      svg("tspan", { class: "lc-sc-sw is-m" }, t, "● ");
      svg("tspan", {}, t, "mężczyźni " + nRow.M);
    }

    function back(step, g) {
      if (step === 3) {
        ROWS.forEach(function (r) {
          [1, 0].forEach(function (f) {
            svg("rect", { x: COLX[f][0], y: ROWY[r][0], width: COLX[f][1] - COLX[f][0],
              height: ROWY[r][1] - ROWY[r][0], rx: 6, class: "lc-sc-cell" }, g);
          });
        });
      }
      if (step === 4) {
        ROWS.forEach(function (r) {
          var y = barY(r) - BAR.h / 2, xm = BAR.x0 + (BAR.x1 - BAR.x0) * pct(r) / 100;
          svg("rect", { x: BAR.x0, y: y, width: xm - BAR.x0, height: BAR.h, class: "lc-sc-bar is-fine" }, g);
          svg("rect", { x: xm, y: y, width: BAR.x1 - xm, height: BAR.h, class: "lc-sc-bar is-warn" }, g);
        });
      }
    }

    function front(step, g) {
      if (step <= 2) {
        legend(g, 350);
        if (step === 2) {
          var t = readout(g, 610, 350, [], "end");
          svg("tspan", { class: "lc-sc-sw is-fine" }, t, "■ ");
          svg("tspan", {}, t, "mandat " + (cnt.K[1] + cnt.M[1]) + "   ");
          svg("tspan", { class: "lc-sc-sw is-warn" }, t, "□ ");
          svg("tspan", {}, t, "pouczenie " + (cnt.K[0] + cnt.M[0]));
        }
        return;
      }
      ROWS.forEach(function (r) {
        svg("text", { x: 156, y: barY(r) + 5, "text-anchor": "end", class: "lc-sc-row " + (r === "K" ? "is-k" : "is-m") }, g, ROWNAME[r]);
      });
      if (step === 3) {
        svg("text", { x: (COLX[1][0] + COLX[1][1]) / 2, y: 52, "text-anchor": "middle", class: "lc-sc-head" }, g, "mandat");
        svg("text", { x: (COLX[0][0] + COLX[0][1]) / 2, y: 52, "text-anchor": "middle", class: "lc-sc-head" }, g, "pouczenie");
        ROWS.forEach(function (r) {
          [1, 0].forEach(function (f) {
            svg("text", { x: COLX[f][1] - 10, y: (ROWY[r][0] + ROWY[r][1]) / 2 + 6, "text-anchor": "end",
              class: "lc-sc-count" + (f ? " is-hit" : "") }, g, String(cnt[r][f]));
          });
        });
        readout(g, 170, 300, [{ t: "mandaty: " }, { t: "kobiety " + cnt.K[1], c: "is-hit" },
          { t: " · " }, { t: "mężczyźni " + cnt.M[1], c: "is-hit" }]);
      }
      if (step === 4) {
        ROWS.forEach(function (r) {
          var xm = BAR.x0 + (BAR.x1 - BAR.x0) * pct(r) / 100;
          svg("text", { x: xm + 6, y: barY(r) - BAR.h / 2 - 8, class: "lc-sc-count is-hit" }, g, fmt(pct(r), 0) + "%");
        });
        [0, 50, 100].forEach(function (v) {
          var x = BAR.x0 + (BAR.x1 - BAR.x0) * v / 100, y = barY("M") + BAR.h / 2;
          svg("line", { x1: x, x2: x, y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
          svg("text", { x: x, y: y + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, v + "%");
        });
        readout(g, 170, 300, [{ t: "mandat: " }, { t: "kobiety " + fmt(pct("K"), 0) + "%", c: "is-hit" },
          { t: " · " }, { t: "mężczyźni " + fmt(pct("M"), 0) + "%", c: "is-hit" }]);
      }
    }

    return { height: H, tokens: tokens, draw: draw, back: back, front: front };
  };

  // =========================================================================
  // BRANZE: pensje absolwentów. cfg.names[3], cfg.b[i] (indeks branży),
  // cfg.x[i] (tys. zł), cfg.stats[k] = {q1, med, q3, lo, hi} z R.
  // Kadry: 1 wszyscy na jednej osi, 2 trzy rzędy, 3 mediany, 4 pudełka.
  // =========================================================================
  KINDS.branze = function (cfg) {
    var H = 460, N = cfg.x.length, X0 = 100, X1 = 610, VMIN = cfg.xmin, VMAX = cfg.xmax;
    var BIN = 0.4, DY = 9.5, R = 4.4;
    var BASE = [115, 230, 345], AXIS = 387, BOX = [8, 30];   // pas pudełka pod rzędem
    function xOf(v) { return X0 + (X1 - X0) * (v - VMIN) / (VMAX - VMIN); }
    function stackY(groupOf) {
      var h = {}, y = new Array(N);
      var order = cfg.x.map(function (v, i) { return i; }).sort(function (a, b) { return cfg.x[a] - cfg.x[b]; });
      order.forEach(function (i) {
        var key = groupOf(i) + ":" + Math.floor(cfg.x[i] / BIN);
        h[key] = (h[key] || 0) + 1;
        y[i] = h[key];
      });
      return y;
    }
    var yAll = stackY(function () { return 0; });
    var yRow = stackY(function (i) { return cfg.b[i]; });
    function binX(v) { return xOf((Math.floor(v / BIN) + 0.5) * BIN); }
    var sorted = cfg.x.slice().sort(function (a, b) { return a - b; });
    var medAll = N % 2 ? sorted[(N - 1) / 2] : (sorted[N / 2 - 1] + sorted[N / 2]) / 2;

    function tokens(step) {
      return cfg.x.map(function (v, i) {
        if (step === 1) return { x: binX(v), y: BASE[2] - R - (yAll[i] - 1) * DY, s: R, o: 1, cls: "is-all" };
        return { x: binX(v), y: BASE[cfg.b[i]] - R - (yRow[i] - 1) * DY, s: R, o: 1, cls: "is-b" + cfg.b[i] };
      });
    }
    function draw(g, t) {
      svg("circle", { cx: t.x, cy: t.y, r: t.s, class: "lc-sc-dot " + t.cls, opacity: t.o }, g);
    }
    function back(step, g) {
      var rows = step === 1 ? [2] : [0, 1, 2];
      rows.forEach(function (k) {
        svg("line", { x1: X0, x2: X1, y1: BASE[k], y2: BASE[k], class: "lc-sc-base" }, g);
      });
      svg("line", { x1: X0, x2: X1, y1: AXIS, y2: AXIS, class: "lc-sc-axis" }, g);
      for (var v = Math.ceil(VMIN); v <= VMAX; v += 2) {
        svg("line", { x1: xOf(v), x2: xOf(v), y1: AXIS, y2: AXIS + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: xOf(v), y: AXIS + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(v));
      }
      svg("text", { x: (X0 + X1) / 2, y: AXIS + 40, "text-anchor": "middle", class: "lc-sc-axtitle" }, g,
        "pensja brutto rok po studiach (tys. zł)");
      if (step >= 4) {
        cfg.stats.forEach(function (s, k) {
          var y0 = BASE[k] + BOX[0], y1 = BASE[k] + BOX[1], ym = (y0 + y1) / 2;
          svg("line", { x1: xOf(s.lo), x2: xOf(s.q1), y1: ym, y2: ym, class: "lc-sc-whisk" }, g);
          svg("line", { x1: xOf(s.q3), x2: xOf(s.hi), y1: ym, y2: ym, class: "lc-sc-whisk" }, g);
          svg("rect", { x: xOf(s.q1), y: y0, width: xOf(s.q3) - xOf(s.q1), height: y1 - y0, class: "lc-sc-box" }, g);
        });
      }
    }
    function front(step, g) {
      if (step === 1) {
        readout(g, X0, 451, [{ t: N + " absolwentów · mediana " + fmt(medAll, 1) + " tys. zł" }]);
        return;
      }
      cfg.names.forEach(function (nm, k) {
        svg("text", { x: 20, y: BASE[k] - 8, class: "lc-sc-row is-b" + k }, g, nm);
      });
      if (step >= 3) {
        cfg.stats.forEach(function (s, k) {
          var x = xOf(s.med);
          svg("line", { x1: x, x2: x, y1: BASE[k] - 64, y2: BASE[k] + (step >= 4 ? BOX[1] : 3), class: "lc-sc-med" }, g);
          svg("text", { x: x, y: BASE[k] - 69, "text-anchor": "middle", class: "lc-sc-count is-hit" }, g, fmt(s.med, 1));
        });
      }
      if (step === 2) readout(g, X0, 451, [{ t: "po " + (N / 3) + " osób w każdej branży" }]);
      if (step === 4) {
        var d = cfg.stats[2].med - cfg.stats[0].med;
        readout(g, X0, 451, [{ t: "różnica median, " + cfg.names[2] + " − " + cfg.names[0] + ": " },
          { t: fmt(d, 1) + " tys. zł", c: "is-hit" }]);
      }
    }
    return { height: H, tokens: tokens, draw: draw, back: back, front: front };
  };

  // =========================================================================
  // STAZ: staż (lata) a pensja (tys. zł) w jednej firmie. cfg.x, cfg.y,
  // cfg.focus (indeks osoby z kadru 1), cfg.mx, cfg.my, cfg.r z R.
  // Kadry: 1 jedna osoba (karta), 2 punkt, 3 cała firma, 4 krzyż średnich.
  // =========================================================================
  KINDS.staz = function (cfg) {
    var H = 400, N = cfg.x.length, F = cfg.focus;
    var PX0 = 92, PX1 = 610, PY0 = 30, PY1 = 330;
    function xOf(v) { return PX0 + (PX1 - PX0) * (v - cfg.xlim[0]) / (cfg.xlim[1] - cfg.xlim[0]); }
    function yOf(v) { return PY1 - (PY1 - PY0) * (v - cfg.ylim[0]) / (cfg.ylim[1] - cfg.ylim[0]); }
    var CARD = { x: 230, y: 70, w: 230, h: 104 };
    function quad(i) { return (cfg.x[i] > cfg.mx) === (cfg.y[i] > cfg.my) ? "is-con" : "is-dis"; }

    function tokens(step) {
      var out = [];
      for (var i = 0; i < N; i++) {
        var t = { x: xOf(cfg.x[i]), y: yOf(cfg.y[i]), s: 5, o: 1, cls: "is-pt" };
        if (i === F) {
          if (step === 1) { t.x = CARD.x + 40; t.y = CARD.y + CARD.h / 2; t.s = 14; t.o = 0; }
          if (step === 2) { t.s = 7; t.cls = "is-focus"; }
        } else if (step <= 2) { t.s = 0; t.o = 0; }
        if (step === 4) t.cls = quad(i);
        out.push(t);
      }
      return out;
    }
    function draw(g, t) {
      if (t.o <= 0 || t.s <= 0) return;
      svg("circle", { cx: t.x, cy: t.y, r: t.s, class: "lc-sc-dot " + t.cls, opacity: t.o }, g);
    }
    function back(step, g) {
      if (step === 4) {
        var xm = xOf(cfg.mx), ym = yOf(cfg.my);
        svg("rect", { x: xm, y: PY0, width: PX1 - xm, height: ym - PY0, class: "lc-sc-quad is-con" }, g);
        svg("rect", { x: PX0, y: ym, width: xm - PX0, height: PY1 - ym, class: "lc-sc-quad is-con" }, g);
        svg("rect", { x: PX0, y: PY0, width: xm - PX0, height: ym - PY0, class: "lc-sc-quad is-dis" }, g);
        svg("rect", { x: xm, y: ym, width: PX1 - xm, height: PY1 - ym, class: "lc-sc-quad is-dis" }, g);
      }
      svg("line", { x1: PX0, x2: PX1, y1: PY1, y2: PY1, class: "lc-sc-axis" }, g);
      svg("line", { x1: PX0, x2: PX0, y1: PY0, y2: PY1, class: "lc-sc-axis" }, g);
      for (var v = cfg.xlim[0]; v <= cfg.xlim[1]; v += 4) {
        svg("line", { x1: xOf(v), x2: xOf(v), y1: PY1, y2: PY1 + 5, class: "lc-sc-axis" }, g);
        svg("text", { x: xOf(v), y: PY1 + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, String(v));
      }
      for (var w = cfg.ylim[0]; w <= cfg.ylim[1]; w += 2) {
        svg("line", { x1: PX0 - 5, x2: PX0, y1: yOf(w), y2: yOf(w), class: "lc-sc-axis" }, g);
        svg("text", { x: PX0 - 9, y: yOf(w) + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, String(w));
      }
      svg("text", { x: (PX0 + PX1) / 2, y: PY1 + 40, "text-anchor": "middle", class: "lc-sc-axtitle" }, g, "staż (lata)");
      svg("text", { x: 22, y: (PY0 + PY1) / 2, "text-anchor": "middle", class: "lc-sc-axtitle",
        transform: "rotate(-90 22 " + (PY0 + PY1) / 2 + ")" }, g, "pensja (tys. zł)");
    }
    function front(step, g) {
      if (step === 1) {
        svg("rect", { x: CARD.x, y: CARD.y, width: CARD.w, height: CARD.h, rx: 10, class: "lc-sc-card" }, g);
        person(g, CARD.x + 40, CARD.y + 82, 56, "is-staz", 1);
        svg("text", { x: CARD.x + 82, y: CARD.y + 44, class: "lc-sc-card-t" }, g, "staż " + fmt(cfg.x[F], 1) + " roku");
        svg("text", { x: CARD.x + 82, y: CARD.y + 72, class: "lc-sc-card-t" }, g, "pensja " + fmt(cfg.y[F], 1) + " tys. zł");
      }
      if (step === 2) {
        var x = xOf(cfg.x[F]), y = yOf(cfg.y[F]);
        svg("line", { x1: x, x2: x, y1: y, y2: PY1, class: "lc-sc-guide" }, g);
        svg("line", { x1: PX0, x2: x, y1: y, y2: y, class: "lc-sc-guide" }, g);
        svg("text", { x: x + 6, y: PY1 - 8, class: "lc-sc-count is-hit" }, g, fmt(cfg.x[F], 1));
        svg("text", { x: PX0 + 6, y: y - 6, class: "lc-sc-count is-hit" }, g, fmt(cfg.y[F], 1));
      }
      if (step === 3) readout(g, PX0, 392, [{ t: "n = " + N + " osób z jednej firmy biotech" }]);
      if (step === 4) {
        var xm = xOf(cfg.mx), ym = yOf(cfg.my), c = { a: 0, b: 0, c: 0, d: 0 };
        for (var i = 0; i < N; i++) {
          var r = cfg.x[i] > cfg.mx, u = cfg.y[i] > cfg.my;
          c[r ? (u ? "a" : "d") : (u ? "b" : "c")]++;
        }
        svg("line", { x1: xm, x2: xm, y1: PY0, y2: PY1, class: "lc-sc-mean" }, g);
        svg("line", { x1: PX0, x2: PX1, y1: ym, y2: ym, class: "lc-sc-mean" }, g);
        svg("text", { x: PX1 - 8, y: PY0 + 20, "text-anchor": "end", class: "lc-sc-count is-hit" }, g, String(c.a));
        svg("text", { x: PX0 + 8, y: PY1 - 10, class: "lc-sc-count is-hit" }, g, String(c.c));
        svg("text", { x: PX0 + 8, y: PY0 + 20, class: "lc-sc-count is-ok" }, g, String(c.b));
        svg("text", { x: PX1 - 8, y: PY1 - 10, "text-anchor": "end", class: "lc-sc-count is-ok" }, g, String(c.d));
        readout(g, PX0, 392, [{ t: "x̄ = " + fmt(cfg.mx, 1) + " roku · ȳ = " + fmt(cfg.my, 1) + " tys. zł · " },
          { t: "r = " + fmt(cfg.r, 2), c: "is-hit" }]);
      }
    }
    return { height: H, tokens: tokens, draw: draw, back: back, front: front };
  };

  // =========================================================================
  // widget
  // =========================================================================
  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    if (!stepper) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var scene = KINDS[cfg.kind](cfg);
    var s = svg("svg", { class: "lc-sc-svg", role: "img", viewBox: "0 0 " + W + " " + scene.height,
      "aria-label": cfg.aria || "Scena" }, root);
    var gBack = svg("g", {}, s), gTok = svg("g", {}, s), gFront = svg("g", {}, s);
    var cur = null, shown = 0, cancel = null;

    function stepNow() { return Number(stepper.getAttribute("data-lc-step")) || 1; }
    function paint(step, toks) {
      gTok.textContent = "";
      toks.forEach(function (t) { scene.draw(gTok, t); });
      gBack.textContent = ""; gFront.textContent = "";
      if (step !== null) { scene.back(step, gBack); scene.front(step, gFront); }
    }
    function go(step) {
      if (step === shown) return;
      var next = scene.tokens(step);
      if (cancel) cancel();
      if (!cur) { cur = next; shown = step; paint(step, next); return; }
      var from = cur;
      gBack.textContent = ""; gFront.textContent = "";
      shown = step;
      cancel = tween(900, function (u) {
        var e = ease(u);
        var mid = next.map(function (b, i) {
          var a = from[i], t = {};
          Object.keys(b).forEach(function (k) { t[k] = typeof b[k] === "number" ? lerp(a[k], b[k], e) : (u < 0.5 ? a[k] : b[k]); });
          return t;
        });
        paint(null, mid);
        cur = mid;
      }, function () { cancel = null; cur = next; paint(step, next); });
    }
    go(stepNow());
    new MutationObserver(function () { go(stepNow()); })
      .observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
  }

  function scan() { document.querySelectorAll(".lc-sc:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
