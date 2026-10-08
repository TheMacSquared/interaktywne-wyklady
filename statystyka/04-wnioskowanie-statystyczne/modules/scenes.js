// Sceny wykładu 04 (rozdział 09, ANOVA). Dane stałe liczy R i podaje w data-config.
// Kontener: .lc-sc[data-config] wewnątrz widgetu krokowego (.lc-stepper)
// albo wewnątrz .lc-sc-host (scena bez kroków, jeden obraz z przełącznikiem).
// config.kind:
//   "jars"    słoiki jogurtu z trzech komór: cały rozrzut → między + wewnątrz → F (kadry)
//   "worlds"  dwie partie słoików: komory się różnią / komory bez znaczenia (dwa światy)
//   "stress"  stres na trzech stanowiskach: ANOVA zapala lampkę, post hoc wskazuje pary (kadry)
// Numer kroku czyta z data-lc-step korzenia widgetu. Przełączniki: [data-sc-opt] "nazwa:wartość".
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H = 420;
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

  // Animacja klatkowa z możliwością przerwania: zwraca funkcję stop().
  function tween(ms, step, done) {
    var alive = true;
    if (REDUCE || ms <= 0) { step(1); if (done) done(); return function () {}; }
    var t0 = null;
    function frame(t) {
      if (!alive) return;
      if (t0 === null) t0 = t;
      var u = Math.min(1, (t - t0) / ms);
      step(u);
      if (u < 1) requestAnimationFrame(frame); else if (done) done();
    }
    requestAnimationFrame(frame);
    return function () { alive = false; };
  }

  // Jeden krótki odczyt pod rysunkiem: [znacznik] tekst   [znacznik] tekst ...
  function readout(g, y, items) {
    var CH = 7.8, MW = 22, GAP = 8, SEP = 26, tot = 0;
    items.forEach(function (it, i) { tot += (it.mark ? MW + GAP : 0) + it.text.length * CH + (i ? SEP : 0); });
    var x = W / 2 - tot / 2;
    items.forEach(function (it, i) {
      if (i) x += SEP;
      if (it.mark) { it.mark(x, y); x += MW + GAP; }
      svg("text", { x: x, y: y + 4.5, class: "lc-sc-n" }, g, it.text);
      x += it.text.length * CH;
    });
  }
  function markDash(g) {
    return function (x, y) { svg("line", { x1: x, x2: x + 22, y1: y, y2: y, class: "lc-sc-param" }, g); };
  }
  function markLine(g, cls) {
    return function (x, y) { svg("line", { x1: x, x2: x + 22, y1: y, y2: y, class: cls }, g); };
  }
  function markBox(g, cls) {
    return function (x, y) { svg("rect", { x: x, y: y - 6, width: 22, height: 12, rx: 2, class: cls }, g); };
  }

  // Tory (pasy) dla obiektów stojących zbyt blisko siebie na osi: indeks toru dla każdej pozycji.
  function lanes(xs, gap) {
    var order = xs.map(function (x, i) { return i; }).sort(function (a, b) { return xs[a] - xs[b]; });
    var last = [], out = new Array(xs.length);
    order.forEach(function (i) {
      var k = 0;
      while (last[k] !== undefined && xs[i] - last[k] < gap) k++;
      last[k] = xs[i]; out[i] = k;
    });
    var n = Math.max.apply(null, out) + 1;
    return { lane: out, n: n };
  }

  function axisX(g, x0, x1, y, lo, hi, by, d, title) {
    svg("line", { x1: x0, x2: x1, y1: y, y2: y, class: "lc-sc-axis" }, g);
    for (var v = lo; v <= hi + 1e-9; v += by) {
      var x = x0 + (x1 - x0) * (v - lo) / (hi - lo);
      svg("line", { x1: x, x2: x, y1: y, y2: y + 5, class: "lc-sc-axis" }, g);
      svg("text", { x: x, y: y + 19, "text-anchor": "middle", class: "lc-sc-tick" }, g, fmt(v, d));
    }
    if (title) svg("text", { x: x1, y: y + 38, "text-anchor": "end", class: "lc-sc-axtitle" }, g, title);
  }

  // Słoik: korpus z wieczkiem, środek korpusu w (x, y).
  function jar(g, x, y, s) {
    svg("rect", { x: x - 6 * s, y: y - 7 * s, width: 12 * s, height: 15 * s, rx: 2 * s, class: "lc-sc-jar" }, g);
    svg("rect", { x: x - 7 * s, y: y - 10.5 * s, width: 14 * s, height: 4 * s, rx: 1 * s, class: "lc-sc-lid" }, g);
  }

  // Postać pracownika (jak person() w scenach 00/02, mniejsza).
  function person(g, x, y, s) {
    svg("circle", { cx: x, cy: y - 9 * s, r: 4 * s, class: "lc-sc-person" }, g);
    svg("rect", { x: x - 5 * s, y: y - 4 * s, width: 10 * s, height: 13 * s, rx: 4 * s, class: "lc-sc-person" }, g);
  }

  var KINDS = {};

  // =========================================================================
  // JARS: skąd się bierze rozrzut (kadry: Słoiki · Całość · Dwie części · F)
  // =========================================================================
  KINDS.jars = function (cfg, api) {
    var PL = 110, PR = 610, ROWS = [72, 152, 232], LANE = 22, AX = 292;
    var BAR1 = 352, BAR2 = 374, RD = 406;
    function X(v) { return PL + (PR - PL) * (v - cfg.lo) / (cfg.hi - cfg.lo); }
    var gmx = X(cfg.gm);
    var pos = cfg.y.map(function (row, j) {
      var xs = row.map(X), L = lanes(xs, 15);
      return xs.map(function (x, i) { return { x: x, y: ROWS[j] + (L.lane[i] - (L.n - 1) / 2) * LANE }; });
    });
    var st = { last: 0, stop: function () {} };

    function draw(step, u) {
      var g = api.stage, lo = api.low;
      g.textContent = ""; lo.textContent = "";
      ROWS.forEach(function (ry, j) {
        svg("line", { x1: PL, x2: PR, y1: ry, y2: ry, class: "lc-sc-grid" }, g);
        svg("text", { x: 24, y: ry + 4, class: "lc-sc-sub" }, g, cfg.temps[j]);
      });
      axisX(g, PL, PR, AX, cfg.lo, cfg.hi, 0.2, 1, "pH");
      var segs = svg("g", {}, g);
      svg("line", { x1: gmx, x2: gmx, y1: 26, y2: AX - 8, class: "lc-sc-param" }, g);
      var marks = svg("g", {}, g), jars = svg("g", {}, g);

      pos.forEach(function (row, j) {
        var cmx = X(cfg.means[j]);
        if (step >= 3) {
          var sx = step === 3 ? lerp(gmx, cmx, ease(u)) : cmx;
          svg("line", { x1: sx, x2: sx, y1: ROWS[j] - 36, y2: ROWS[j] + 36, class: "lc-sc-cmean" }, marks);
          row.forEach(function (p) {
            svg("line", { x1: gmx, x2: sx, y1: p.y, y2: p.y, class: "lc-sc-between" }, segs);
            svg("line", { x1: sx, x2: p.x, y1: p.y, y2: p.y, class: "lc-sc-within" }, segs);
          });
        } else if (step === 2) {
          row.forEach(function (p) {
            var ex = lerp(p.x, gmx, ease(u));
            svg("line", { x1: p.x, x2: ex, y1: p.y, y2: p.y, class: "lc-sc-total" }, segs);
          });
        }
        row.forEach(function (p) { jar(jars, p.x, p.y, 1); });
      });

      if (step >= 4) {
        var e = step === 4 ? ease(u) : 1, mx = Math.max(cfg.msb, cfg.msw);
        svg("rect", { x: PL, y: BAR1 - 8, width: Math.max(2, (PR - PL) * cfg.msb / mx * e), height: 14, rx: 2, class: "lc-sc-bar-b" }, lo);
        svg("rect", { x: PL, y: BAR2 - 8, width: Math.max(2, (PR - PL) * cfg.msw / mx * e), height: 14, rx: 2, class: "lc-sc-bar-w" }, lo);
        readout(lo, RD, [
          { mark: markBox(lo, "lc-sc-bar-b"), text: "między = " + fmt(cfg.msb, 4) },
          { mark: markBox(lo, "lc-sc-bar-w"), text: "wewnątrz = " + fmt(cfg.msw, 4) },
          { text: "F = " + fmt(cfg.F, 2) }
        ]);
      } else if (step === 3) {
        readout(lo, RD, [
          { text: "całość = " + fmt(cfg.sst, 3) },
          { mark: markLine(lo, "lc-sc-between"), text: "między = " + fmt(cfg.ssb, 3) },
          { mark: markLine(lo, "lc-sc-within"), text: "wewnątrz = " + fmt(cfg.ssw, 3) }
        ]);
      } else if (step === 2) {
        readout(lo, RD, [{ mark: markLine(lo, "lc-sc-total"), text: "suma kwadratów: całość = " + fmt(cfg.sst, 3) }]);
      } else {
        readout(lo, RD, [{ mark: markDash(lo), text: "średnia ogólna pH = " + fmt(cfg.gm, 2) }]);
      }
    }

    function render() {
      var step = api.step();
      st.stop();
      if (step === st.last + 1 && step > 1) {
        st.stop = tween(step === 2 ? 700 : 650, function (u) { draw(step, u); });
      } else {
        draw(step, 1);
      }
      st.last = step;
    }
    return { render: render, reset: function () { st.last = 0; }, opt: function () {} };
  };

  // =========================================================================
  // WORLDS: dwie partie słoików (komory się różnią / komory bez znaczenia)
  // =========================================================================
  KINDS.worlds = function (cfg, api) {
    var PL = 110, PR = 610, ROWS = [66, 152, 238], LANE = 18, AX = 292;
    var BAR1 = 352, BAR2 = 374, RD = 406;
    function X(v) { return PL + (PR - PL) * (v - cfg.lo) / (cfg.hi - cfg.lo); }
    var MX = 0;
    Object.keys(cfg.worlds).forEach(function (k) {
      var w = cfg.worlds[k];
      MX = Math.max(MX, w.msb, w.msw);
      w.pos = w.y.map(function (row, j) {
        var xs = row.map(X), L = lanes(xs, 13);
        return xs.map(function (x, i) { return { x: x, y: ROWS[j] + (L.lane[i] - (L.n - 1) / 2) * LANE }; });
      });
    });
    var st = { from: cfg.world, to: cfg.world, u: 1, stop: function () {} };

    function draw() {
      var A = cfg.worlds[st.from], B = cfg.worlds[st.to], e = ease(st.u);
      var g = api.stage, lo = api.low;
      g.textContent = ""; lo.textContent = "";
      ROWS.forEach(function (ry, j) {
        svg("line", { x1: PL, x2: PR, y1: ry, y2: ry, class: "lc-sc-grid" }, g);
        svg("text", { x: 24, y: ry + 4, class: "lc-sc-sub" }, g, cfg.temps[j]);
      });
      axisX(g, PL, PR, AX, cfg.lo, cfg.hi, 0.2, 1, "pH");
      var gmx = X(lerp(A.gm, B.gm, e));
      svg("line", { x1: gmx, x2: gmx, y1: 20, y2: AX - 8, class: "lc-sc-param" }, g);
      var jars = svg("g", {}, g);
      ROWS.forEach(function (ry, j) {
        var cx = X(lerp(A.means[j], B.means[j], e));
        svg("line", { x1: cx, x2: cx, y1: ry - 40, y2: ry + 40, class: "lc-sc-cmean" }, g);
        A.pos[j].forEach(function (p, i) {
          var q = B.pos[j][i];
          jar(jars, lerp(p.x, q.x, e), lerp(p.y, q.y, e), 0.85);
        });
      });
      var wb = lerp(A.msb, B.msb, e), ww = lerp(A.msw, B.msw, e);
      svg("rect", { x: PL, y: BAR1 - 8, width: Math.max(2, (PR - PL) * wb / MX), height: 14, rx: 2, class: "lc-sc-bar-b" }, lo);
      svg("rect", { x: PL, y: BAR2 - 8, width: Math.max(2, (PR - PL) * ww / MX), height: 14, rx: 2, class: "lc-sc-bar-w" }, lo);
      readout(lo, RD, [
        { mark: markBox(lo, "lc-sc-bar-b"), text: "między = " + fmt(B.msb, 4) },
        { mark: markBox(lo, "lc-sc-bar-w"), text: "wewnątrz = " + fmt(B.msw, 4) },
        { text: "F = " + fmt(B.F, 2) }
      ]);
    }

    return {
      render: function () { draw(); },
      reset: function () {},
      opt: function (name, v) {
        if (name !== "world" || v === st.to) return;
        st.stop();
        st.from = st.to; st.to = v; st.u = 0;
        st.stop = tween(900, function (u) { st.u = u; draw(); });
      }
    };
  };

  // =========================================================================
  // STRESS: co odstaje? (kadry: Trzy grupy · ANOVA · Które pary)
  // =========================================================================
  KINDS.stress = function (cfg, api) {
    var CX = [200, 340, 480], YT = 118, YB = 360, LANEW = 15, RD = 410;
    function Y(v) { return YB - (YB - YT) * (v - cfg.lo) / (cfg.hi - cfg.lo); }
    var pos = cfg.y.map(function (col, j) {
      // rój: tor 0, +1, −1, +2, … — pierwszy, w którym nikt nie stoi bliżej niż 22 px w pionie
      var order = col.map(function (v, i) { return i; }).sort(function (a, b) { return col[a] - col[b]; });
      var placed = [], out = new Array(col.length);
      order.forEach(function (i) {
        var y = Y(col[i]), k = 0, cand = [0, 1, -1, 2, -2, 3, -3, 4, -4];
        for (var c = 0; c < cand.length; c++) {
          k = cand[c];
          var hit = placed.some(function (p) { return p.k === k && Math.abs(p.y - y) < 22; });
          if (!hit) break;
        }
        placed.push({ k: k, y: y });
        out[i] = { x: CX[j] + k * LANEW, y: y };
      });
      return out;
    });
    var st = { last: 0, stop: function () {} };

    function lamp(g, x, y, op) {
      var q = svg("g", { opacity: op }, g);
      svg("circle", { cx: x, cy: y, r: 26, class: "lc-sc-halo" }, q);
      svg("circle", { cx: x, cy: y, r: 13, class: "lc-sc-bulb" }, q);
      svg("rect", { x: x - 6, y: y + 12, width: 12, height: 8, rx: 2, class: "lc-sc-lamp-base" }, q);
      svg("text", { x: x, y: y + 40, "text-anchor": "middle", class: "lc-sc-sub" }, q, "coś odstaje");
    }

    function bracket(g, a, b, y, pr, op) {
      var x1 = CX[a] + 6, x2 = CX[b] - 6;
      var q = svg("g", { opacity: op }, g);
      svg("path", { d: "M " + x1 + " " + (y + 9) + " V " + y + " H " + x2 + " V " + (y + 9),
        class: pr.sig ? "lc-sc-pair is-sig" : "lc-sc-pair", fill: "none" }, q);
      svg("text", { x: (x1 + x2) / 2, y: y - 6, "text-anchor": "middle",
        class: pr.sig ? "lc-sc-pval is-sig" : "lc-sc-pval" }, q, fmt(pr.p, 3));
    }

    function draw(step, u) {
      var g = api.stage, lo = api.low;
      g.textContent = ""; lo.textContent = "";
      // oś stresu
      svg("line", { x1: 110, x2: 110, y1: YT - 6, y2: YB, class: "lc-sc-axis" }, g);
      for (var v = cfg.lo; v <= cfg.hi; v += 10) {
        var y = Y(v);
        svg("line", { x1: 105, x2: 110, y1: y, y2: y, class: "lc-sc-axis" }, g);
        svg("line", { x1: 112, x2: 560, y1: y, y2: y, class: "lc-sc-grid" }, g);
        svg("text", { x: 100, y: y + 4, "text-anchor": "end", class: "lc-sc-tick" }, g, String(v));
      }
      svg("text", { x: 40, y: (YT + YB) / 2, "text-anchor": "middle", class: "lc-sc-axtitle",
        transform: "rotate(-90 40 " + (YT + YB) / 2 + ")" }, g, "stres (pkt)");
      cfg.groups.forEach(function (name, j) {
        svg("text", { x: CX[j], y: YB + 20, "text-anchor": "middle", class: "lc-sc-sub" }, g, name);
        pos[j].forEach(function (p) { person(g, p.x, p.y + 2, 1); });
        var my = Y(cfg.means[j]);
        svg("line", { x1: CX[j] - 46, x2: CX[j] + 46, y1: my, y2: my, class: "lc-sc-gmean" }, g);
      });
      if (step >= 2) lamp(g, 596, 150, step === 2 ? ease(u) : 1);
      if (step >= 3) {
        var e = step === 3 ? ease(u) : 1;
        cfg.pairs.forEach(function (pr) {
          var y = (pr.b - pr.a) > 1 ? 40 : 80;
          bracket(g, pr.a, pr.b, y, pr, e);
        });
        readout(lo, RD, [
          { mark: markLine(lo, "lc-sc-pair is-sig"), text: "p.adj < 0.05" },
          { mark: markLine(lo, "lc-sc-pair"), text: "p.adj ≥ 0.05" }
        ]);
      } else if (step === 2) {
        readout(lo, RD, [{ text: "F = " + fmt(cfg.F, 2) + " · p = " + fmt(cfg.p, 3) }]);
      } else {
        readout(lo, RD, [{ mark: markLine(lo, "lc-sc-gmean"), text: "średnia: " +
          cfg.groups.map(function (n, j) { return n + " " + fmt(cfg.means[j], 1); }).join(" · ") }]);
      }
    }

    function render() {
      var step = api.step();
      st.stop();
      if (step === st.last + 1 && step > 1) st.stop = tween(600, function (u) { draw(step, u); });
      else draw(step, 1);
      st.last = step;
    }
    return { render: render, reset: function () { st.last = 0; }, opt: function () {} };
  };

  // =========================================================================
  // widget
  // =========================================================================
  function init(root) {
    if (root.dataset.ready) return;
    var stepper = root.closest(".lc-stepper");
    var host = stepper || root.closest(".lc-sc-host");
    if (!host) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    if (!KINDS[cfg.kind]) return;
    var s = svg("svg", { class: "lc-sc-svg", role: "img", viewBox: "0 0 " + W + " " + (cfg.height || H),
      "aria-label": cfg.aria || "Scena" }, root);
    var api = {
      stage: svg("g", {}, s), low: svg("g", {}, s),
      step: function () { return stepper ? (Number(stepper.getAttribute("data-lc-step")) || 1) : 1; }
    };
    var scene = KINDS[cfg.kind](cfg, api);
    scene.render();

    host.addEventListener("click", function (e) {
      var o = e.target.closest("[data-sc-opt]");
      if (o && host.contains(o)) {
        var kv = o.getAttribute("data-sc-opt").split(":");
        o.parentNode.querySelectorAll("[data-sc-opt]").forEach(function (b) {
          b.setAttribute("aria-pressed", b === o ? "true" : "false");
        });
        scene.opt(kv[0], kv[1]);
        return;
      }
      if (e.target.closest('[data-lc-nav="reset"]')) setTimeout(function () { scene.reset(); scene.render(); }, 0);
    });

    if (stepper) {
      new MutationObserver(function () { scene.render(); })
        .observe(stepper, { attributes: true, attributeFilter: ["data-lc-step"] });
    }
  }

  function scan() { document.querySelectorAll(".lc-sc:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
