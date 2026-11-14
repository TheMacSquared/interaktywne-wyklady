// Prototypy punktów stop wykładu 10: drzewo, interwencje, scenariusze, kryterium.
// Parametry modelu przychodzą z R w data-config (i10p_cfg) — tu nie ma liczb modelu.
//
//   scena drzewo        — P11A i P13B: drzewo TOP = I ∩ (D ∪ S) z liczbami; warianty z config.variants.
//   i10_opcje           — P13A: interwencje na stałej osi P(TOP), budżet z suwaka.
//   i10_scen            — P14: przedział optymistyczny–ostrożny dla każdej opcji.
//   i10_odwrocenie      — P15: P(TOP) w scenariuszu ostrożnym jako funkcja u.
//   i10_kryterium       — P16: limit z suwaka na tle przedziałów.
(function () {
  "use strict";
  var C = window.lcCharts, S = window.lcSF, Z = window.lcI10;
  if (!Z) return;
  var U = Z.util;

  // ---- Scena „drzewo” ---------------------------------------------------------
  function node(x, y, w, h, cls, name, value, note, noteCls) {
    var cx = x + w / 2;
    return '<rect class="' + cls + '" x="' + x + '" y="' + y + '" width="' + w + '" height="' + h + '" rx="6"/>' +
      '<text x="' + cx + '" y="' + (y + 20) + '" font-size="13" text-anchor="middle">' + U.esc(name) + "</text>" +
      '<text class="i10p-mono" x="' + cx + '" y="' + (y + 46) + '" font-size="21" font-weight="700" text-anchor="middle">' + value + "</text>" +
      (note ? '<text class="i10p-mono ' + (noteCls || "i10p-subtle") + '" x="' + cx + '" y="' + (y + 64) + '" font-size="12" text-anchor="middle">' + U.esc(note) + "</text>" : "");
  }
  function link(x1, y1, x2, y2) {
    var ym = (y1 + y2) / 2;
    return '<path class="i10p-link" d="M' + x1 + "," + y1 + " V" + ym + " H" + x2 + " V" + y2 + '"/>';
  }
  function gate(x, y, label) {
    return '<rect class="i10p-fill-sec" x="' + (x - 30) + '" y="' + (y - 11) + '" width="60" height="22" rx="11"/>' +
      '<text class="i10p-mono i10p-inv" x="' + x + '" y="' + (y + 4) + '" font-size="12" font-weight="700" text-anchor="middle">' + label + "</text>";
  }
  function pct(a, b) {
    var r = Math.round(100 * (a / b - 1));
    return (r > 0 ? "+" : r < 0 ? "−" : "") + Math.abs(r) + "%";
  }
  Z.register({
    drzewo: function (cfg, ui) {
      var k = 0;
      function calc(v) { return U.mission(cfg, { model: v.model, sens: v.sens, intervention: v.intervention }); }
      var base = calc(cfg.variants[0]);
      return {
        on: { variant: function (btn) { k = Number(U.press(btn)); } },
        render: function () {
          var m = calc(cfg.variants[k]), or = 1 - (1 - m.miss) * m.system_r, orBase = 1 - (1 - base.miss) * base.system_r;
          function ch(a, b) { return Math.abs(a - b) > 1e-12; }
          function was(a, b) { return ch(a, b) ? "było " + U.fmtP(b) : ""; }
          function cls(a, b) { return ch(a, b) ? "i10p-box-hit" : "i10p-box"; }
          var svg =
            link(400, 86, 160, 150) + link(400, 86, 590, 150) + link(590, 222, 480, 270) + link(590, 222, 700, 270) +
            node(280, 10, 240, 76, ch(m.top, base.top) ? "i10p-box-ok" : "i10p-box", "TOP: utrata ochrony w misji", U.fmtP(m.top),
              ch(m.top, base.top) ? "było " + U.fmtP(base.top) + " (" + pct(m.top, base.top) + ")" : U.per10000(m.top) + " misji") +
            gate(400, 118, "ORAZ") +
            node(60, 150, 200, 72, cls(m.initiation, base.initiation), "I: zapotrzebowanie", U.fmtP(m.initiation), was(m.initiation, base.initiation), "i10p-bad") +
            node(480, 150, 220, 72, "i10p-box-dim", "D ∪ S: brak detekcji lub chłodzenia", U.fmtP(or), was(or, orBase)) +
            gate(590, 246, "LUB") +
            node(380, 270, 200, 72, cls(m.miss, base.miss), "D: przeoczenie", U.fmtP(m.miss), was(m.miss, base.miss), "i10p-bad") +
            node(600, 270, 200, 72, cls(m.system_r, base.system_r), "S: chłodzenie nie wytrzyma", U.fmtP(m.cooling_failure), was(m.cooling_failure, base.cooling_failure), "i10p-bad");
          ui.set("stage", U.svgWrap(820, 352, "Drzewo błędów: TOP " + U.fmtP(m.top) + ", zapotrzebowanie " + U.fmtP(m.initiation) +
            ", przeoczenie " + U.fmtP(m.miss) + ", awaria chłodzenia " + U.fmtP(m.cooling_failure) + ".", svg));
          var cost = cfg.variants[k].cost;
          ui.set("read", '<p class="i10p-read">P(TOP) = ' + U.fmtP(m.initiation) + " · [1 − " + U.fmtP(1 - m.miss) + " · " + U.fmtP(m.system_r) +
            "] = " + U.fmtP(m.top) + " · " + U.per10000(m.top) + " misji" + (cost != null ? " · koszt " + cost : "") + "</p>");
        }
      };
    }
  });

  if (!C || !S) return;

  // ---- Wspólne rachunki rycin ----------------------------------------------------
  function top(cfg, id, stress, model) { return U.mission(cfg, { intervention: id, stress: stress, model: model }).top; }
  function wrap(label) { var i = label.indexOf(" "); return i < 0 ? label : label.slice(0, i) + "\n" + label.slice(i + 1); }
  // Wiersze od góry: opcje w kolejności tabeli interwencji (pozycja y malejąca).
  function rows(cfg, withNone) {
    var list = (withNone ? [{ id: "none", label: "Bez działania", cost: 0 }] : []).concat(cfg.interventions);
    return list.map(function (x, i) { return { id: x.id, label: x.label, cost: x.cost, y: list.length - i }; });
  }
  function yLabels(rs) { var o = {}; rs.forEach(function (r) { o[r.y] = wrap(r.label); }); return o; }

  S.register({
    i10_opcje: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, budget = Number(vals.i10p13a_budget), model = vals.i10p13a_model;
        var rs = rows(cfg, false), base = top(cfg, "none", 1, model), layers = [], best = null;
        rs.forEach(function (r) {
          r.top = top(cfg, r.id, 1, model); r.ok = r.cost <= budget;
          if (r.ok && (!best || r.top < best.top)) best = r;
        });
        state.best = best; state.base = base;
        layers.push({ type: "hbins", data: rs.map(function (r) { return [r.y - 0.32, r.y + 0.32, r.top]; }),
          colors: rs.map(function (r) { return r.ok ? P.secondary : C.alpha(P.subtle, 0.3); }) });
        rs.forEach(function (r) {
          layers.push({ type: "text", x: 0, y: r.y, text: U.fmtP(r.top) + " · koszt " + r.cost + (r.ok ? "" : " · poza budżetem"),
            color: r.ok ? P.surface : P.ink, align: "left", valign: "middle", mono: true, bold: r === best });
        });
        layers.push({ type: "vline", x: base, color: P.ink, width: 1.6, dash: [6, 4] });
        layers.push({ type: "text", x: base, y: "top", text: "bez działania " + U.fmtP(base), color: P.ink, align: "right", mono: true });
        return { x: [0, cfg.axis], y: [0.4, rs.length + 0.9], yLabels: yLabels(rs), noYGrid: true, layers: layers,
          xName: "P(TOP) na misję po działaniu",
          aria: "Ryzyko po czterech interwencjach na wspólnej osi; bez działania " + U.fmtP(base) + "." };
      },
      update: function (root, state) {
        var b = state.best;
        S.setValue(root, "best", b ? b.label : "brak");
        S.setValue(root, "top", b ? U.fmtP(b.top) : "–");
        S.setValue(root, "cut", b ? Math.round(100 * (1 - b.top / state.base)) + "%" : "–");
      }
    },

    i10_scen: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, u = Number(vals.i10p14_u), model = vals.i10p14_model;
        var rs = rows(cfg, true), layers = [];
        rs.forEach(function (r) {
          r.lo = top(cfg, r.id, 1 - u, model); r.mid = top(cfg, r.id, 1, model); r.hi = top(cfg, r.id, 1 + u, model);
        });
        var acts = rs.filter(function (r) { return r.id !== "none"; });
        var best = acts.reduce(function (a, b) { return b.mid < a.mid ? b : a; });
        state.spread = best.hi - best.lo;
        state.gap = Math.max.apply(null, acts.map(function (r) { return r.mid; })) - best.mid;
        layers.push({ type: "vline", x: cfg.limit, color: P.ink, width: 1.6, dash: [6, 4] });
        layers.push({ type: "text", x: cfg.limit, y: "top", text: "limit " + U.fmtP(cfg.limit), color: P.ink, align: "left", mono: true });
        layers.push({ type: "segs", width: 9, items: rs.map(function (r) {
          return { from: [r.lo, r.y], to: [r.hi, r.y], color: r.id === "none" ? P.subtle : P.niebo }; }) });
        layers.push({ type: "points", data: rs.map(function (r) { return [r.mid, r.y]; }), color: P.ink, size: 11 });
        rs.forEach(function (r) {
          layers.push({ type: "text", x: Math.min(r.hi, cfg.top_axis * 0.86), y: r.y, text: U.fmtP(r.lo) + " – " + U.fmtP(r.hi), color: P.soft,
            align: "left", valign: "middle", mono: true, size: 0.85 });
        });
        return { x: [0, cfg.top_axis], y: [0.4, rs.length + 0.9], yLabels: yLabels(rs), noYGrid: true, layers: layers,
          legend: [{ text: "od scenariusza optymistycznego do ostrożnego", color: P.niebo }, { text: "scenariusz bazowy", color: P.ink, box: true }],
          xName: "P(TOP) na misję po działaniu",
          aria: "Przedziały ryzyka po działaniu w trzech scenariuszach przy zakresie niepewności " + u + "." };
      },
      update: function (root, state) {
        S.setValue(root, "spread", U.fmtP(state.spread));
        S.setValue(root, "gap", U.fmtP(state.gap));
      }
    },

    i10_odwrocenie: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, u = Number(vals.i10p15_u), model = vals.i10p15_model, layers = [];
        var a = cfg.pair[0], b = cfg.pair[1];
        function curve(id) {
          var pts = [];
          for (var i = 0; i <= 100; i++) { var x = cfg.u_max * i / 100; pts.push([x, top(cfg, id, 1 + x, model)]); }
          return pts;
        }
        // Przecięcie linii pary (bisekcja po zmianie znaku różnicy).
        function diff(x) { return top(cfg, a, 1 + x, model) - top(cfg, b, 1 + x, model); }
        var cross = null;
        for (var i = 1; i <= 100 && cross == null; i++) {
          var x0 = cfg.u_max * (i - 1) / 100, x1 = cfg.u_max * i / 100;
          if (diff(x0) < 0 && diff(x1) >= 0) {
            for (var j = 0; j < 30; j++) { var xm = (x0 + x1) / 2; if (diff(xm) < 0) x0 = xm; else x1 = xm; }
            cross = (x0 + x1) / 2;
          }
        }
        cfg.interventions.forEach(function (it) {
          if (it.id !== a && it.id !== b) layers.push({ type: "line", data: curve(it.id), color: P.rule, width: 1.4 });
        });
        if (cross != null) {
          layers.push({ type: "vline", x: cross, color: P.subtle, width: 1.2, dash: [3, 3] });
          layers.push({ type: "text", x: cross, y: "top", text: "przecięcie u ≈ " + U.fmt(cross, 2), color: P.subtle, align: "right", mono: true });
        }
        layers.push({ type: "line", data: curve(a), color: P.accent, width: 2.6 });
        layers.push({ type: "line", data: curve(b), color: P.niebo, width: 2.6 });
        var ta = top(cfg, a, 1 + u, model), tb = top(cfg, b, 1 + u, model);
        state.ta = ta; state.tb = tb; state.cross = cross;
        layers.push({ type: "vline", x: u, color: P.ink, width: 1.6, dash: [6, 4] });
        layers.push({ type: "point", x: u, y: ta, color: P.accent, size: 11 });
        layers.push({ type: "point", x: u, y: tb, color: P.niebo, size: 11 });
        var name = {}; cfg.interventions.forEach(function (it) { name[it.id] = it.label.toLowerCase(); });
        return { x: [0, cfg.u_max], y: [0, cfg.axis], layers: layers,
          legend: [{ text: name[a], color: P.accent }, { text: name[b], color: P.niebo }, { text: "pozostałe działania", color: P.rule }],
          xName: "Zakres niepewności u (scenariusz ostrożny: m = 1 + u)", yName: "P(TOP) po działaniu",
          aria: "Ryzyko po działaniu w scenariuszu ostrożnym jako funkcja zakresu niepewności; przy u = " + u + " wartości " +
                U.fmtP(ta) + " i " + U.fmtP(tb) + "." };
      },
      update: function (root, state) {
        var cfg = state.config, lab = {};
        cfg.interventions.forEach(function (it) { lab[it.id] = it.label; });
        S.setValue(root, "prev", U.fmtP(state.ta));
        S.setValue(root, "fan", U.fmtP(state.tb));
        S.setValue(root, "win", lab[state.ta <= state.tb ? cfg.pair[0] : cfg.pair[1]]);
      }
    },

    i10_kryterium: {
      draw: function (v, P, k, state, vals) {
        var cfg = state.config, limit = Number(vals.i10p16_limit), rs = rows(cfg, false), layers = [], best = null;
        rs.forEach(function (r) {
          r.lo = top(cfg, r.id, 1 - cfg.u, "exp"); r.mid = top(cfg, r.id, 1, "exp"); r.hi = top(cfg, r.id, 1 + cfg.u, "exp");
          r.ok = r.cost <= cfg.budget; r.meets = r.hi <= limit;
          if (r.ok && (!best || r.mid < best.mid)) best = r;
        });
        state.best = best; state.limit = limit;
        layers.push({ type: "segs", width: 9, items: rs.map(function (r) {
          return { from: [r.lo, r.y], to: [r.hi, r.y], color: !r.ok ? C.alpha(P.subtle, 0.35) : r.meets ? P.szalwia : P.accent }; }) });
        layers.push({ type: "points", data: rs.map(function (r) { return [r.mid, r.y]; }),
          colors: rs.map(function (r) { return r.ok ? P.ink : P.subtle; }), size: 11 });
        rs.forEach(function (r) {
          layers.push({ type: "text", x: Math.max(r.hi, limit), y: r.y,
            text: !r.ok ? "poza budżetem" : (r.meets ? "spełnia: " : "nie spełnia: ") + U.fmtP(r.hi),
            color: !r.ok ? P.subtle : r.meets ? P.szalwia : P.accent, align: "left", valign: "middle", mono: true, bold: r.ok });
        });
        layers.push({ type: "vline", x: limit, color: P.ink, width: 1.8, dash: [6, 4] });
        layers.push({ type: "text", x: limit, y: "top", text: "limit " + U.fmtP(limit), color: P.ink, align: "left", mono: true });
        return { x: [0, cfg.top_axis], y: [0.4, rs.length + 0.9], yLabels: yLabels(rs), noYGrid: true, layers: layers,
          legend: [{ text: "najgorszy scenariusz w limicie", color: P.szalwia }, { text: "najgorszy scenariusz ponad limitem", color: P.accent },
                   { text: "scenariusz bazowy", color: P.ink, box: true }],
          xName: "P(TOP) na misję po działaniu",
          aria: "Przedziały ryzyka po działaniu na tle limitu " + U.fmtP(limit) + "." };
      },
      update: function (root, state) {
        var b = state.best;
        S.setValue(root, "best", b.label);
        S.setValue(root, "worst", U.fmtP(b.hi));
        S.setValue(root, "verdict", b.meets ? "spełnione" : "niespełnione");
      }
    }
  });
})();
