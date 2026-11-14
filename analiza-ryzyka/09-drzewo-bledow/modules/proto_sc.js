// Prototypy punktów stop wykładu 09, grupa „sc”: sceny interaktywne i rycina P14.
// Kontenery w R: stop_proto_sc.R (f9p_scene_ui, lc_sf_figure).
//
//   lancuch     — P4A/P4B: dwa światy obok siebie (łańcuch i zapas), klikane elementy
//   konstruktor — P5: bramka OR/AND, wybór wejść, klikane liście
//   stany       — P6A: klikane drzewo Bananpolu i tabela ośmiu kombinacji
//   parametry   — P9: suwaki, liczby na drzewie, siatka 10 000 magazyno-lat
//   f9_wspolna  — P14 (lcSF): P(TOP) w funkcji q dla łańcucha i dwóch barier
(function () {
  "use strict";
  var F = window.lcF9;
  if (!F) return;
  var U = F.util, g = U.g, esc = U.esc;

  // Schemat jednego świata sceny „lancuch”: start → elementy → koniec.
  // broken: [bool, bool]; elementy klikane (data-f9p="toggle", data-value="w-i").
  function worldSvg(w, wi, broken) {
    var fails = w.type === "chain" ? broken[0] || broken[1] : broken[0] && broken[1];
    function box(i, x, y) {
      var bad = broken[i], p = w.parts[i];
      return '<g class="f9p4-part' + (bad ? " is-bad" : "") + '" data-f9p="toggle" data-value="' + wi + "-" + i + '" role="button" tabindex="0" aria-pressed="' + bad + '">' +
        '<rect x="' + x + '" y="' + y + '" width="132" height="58" rx="6"/>' +
        '<text x="' + (x + 66) + '" y="' + (y + 25) + '" font-size="15" font-weight="700" text-anchor="middle">' + esc(p.name) + "</text>" +
        '<text class="f9p4-state" x="' + (x + 66) + '" y="' + (y + 45) + '" font-size="13" text-anchor="middle">' + esc(bad ? p.bad : p.ok) + "</text></g>";
    }
    function wire(d, live) { return '<path class="f9p4-wire' + (live ? " is-live" : "") + '" d="' + d + '"/>'; }
    var s = "";
    if (w.type === "chain") {
      s += wire("M22,80 H62", true) + wire("M194,80 H226", !broken[0]) + wire("M358,80 H398", !fails) + box(0, 62, 51) + box(1, 226, 51);
    } else {
      s += wire("M22,80 H60 M60,40 V120", true) + wire("M60,40 H144", true) + wire("M60,120 H144", true) +
        wire("M276,40 H360 V80", !broken[0]) + wire("M276,120 H360 V80", !broken[1]) + wire("M360,80 H398", !fails) +
        box(0, 144, 11) + box(1, 144, 91);
    }
    s += '<circle class="f9p4-start" cx="14" cy="80" r="8"/><text class="f9p-subtle" x="14" y="' + (w.type === "chain" ? 108 : 150) + '" font-size="12" text-anchor="start">' + esc(w.start) + "</text>" +
      '<circle class="f9p4-end' + (fails ? " is-bad" : "") + '" cx="410" cy="80" r="12"/>' +
      '<text class="' + (fails ? "f9p-bad" : "f9p-ok") + '" x="422" y="' + (w.type === "chain" ? 112 : 150) + '" font-size="13" font-weight="700" text-anchor="end">' + esc(fails ? w.end_bad : w.end_ok) + "</text>";
    return { svg: U.svgWrap(430, 158, w.title + ": " + (fails ? w.bad : w.ok), s), fails: fails };
  }
  // Cztery układy usterek: żadna, pierwsza, druga, obie; bieżący obwiedziony.
  function combos(w, broken) {
    var names = ["bez usterek", w.parts[0].name.toLowerCase(), w.parts[1].name.toLowerCase(), "obie"];
    var sets = [[false, false], [true, false], [false, true], [true, true]], bad = 0;
    var chips = sets.map(function (st, k) {
      var f = w.type === "chain" ? st[0] || st[1] : st[0] && st[1];
      if (f) bad++;
      var now = st[0] === broken[0] && st[1] === broken[1];
      return '<span class="f9p4-chip' + (f ? " is-bad" : "") + (now ? " is-now" : "") + '">' + esc(names[k]) + "</span>";
    }).join("");
    return '<div class="f9p4-chips">' + chips + '</div><p class="f9p-read">' + esc(w.count) + " w " + bad + " z 4 układów usterek</p>";
  }

  F.register({
    lancuch: function (cfg, ui) {
      var broken = cfg.worlds.map(function () { return [false, false]; });
      return {
        on: {
          toggle: function (el) {
            var v = el.getAttribute("data-value").split("-");
            broken[+v[0]][+v[1]] = !broken[+v[0]][+v[1]];
          }
        },
        render: function () {
          ui.set("stage", '<div class="f9p-row">' + cfg.worlds.map(function (w, wi) {
            var r = worldSvg(w, wi, broken[wi]);
            return '<div class="f9p-cell"><h4>' + esc(w.title) + "</h4>" + r.svg +
              '<p class="f9p4-verdict' + (r.fails ? " is-bad" : "") + '">' + esc(r.fails ? w.bad : w.ok) + "</p>" + combos(w, broken[wi]) + "</div>";
          }).join("") + "</div>");
        }
      };
    },

    konstruktor: function (cfg, ui) {
      var gate = cfg.gate, chosen = cfg.chosen.slice(), lit = [];
      return {
        on: {
          gate: function (btn) { gate = U.press(btn); },
          cause: function (btn) {
            var id = btn.getAttribute("data-value"), k = chosen.indexOf(id);
            if (k >= 0) { chosen.splice(k, 1); lit = lit.filter(function (e) { return e !== id; }); } else chosen.push(id);
            btn.setAttribute("aria-pressed", k >= 0 ? "false" : "true");
          },
          leaf: function (el) {
            var id = el.getAttribute("data-value"), k = lit.indexOf(id);
            if (k >= 0) lit.splice(k, 1); else lit.push(id);
          },
          clear: function () { lit = []; }
        },
        render: function () {
          var kids = cfg.causes.filter(function (c) { return chosen.indexOf(c.id) >= 0; });
          var n = kids.length;
          if (!n) {
            ui.set("stage", '<p class="f9p-caption">Wybierz co najmniej jedną przyczynę.</p>');
            ui.set("body", "");
            return;
          }
          var spec = { id: "X", label: cfg.label, gate: gate, children: kids.map(function (c) { return { id: c.id, label: c.label, sym: c.id, ev: c.id }; }) };
          var on = U.evalTree(spec, lit).X, total = Math.pow(2, n), hits = gate === "or" ? total - 1 : 1;
          ui.set("stage", U.tree(spec, { lit: lit }, { click: "leaf" }));
          ui.set("body", '<p class="f9p-caption">' + (gate === "or"
              ? "Bramka OR: każde wejście samo wystarcza."
              : "Bramka AND: potrzebne są wszystkie wejścia naraz.") +
            (on ? " Wyjście zapalone." : " Wyjście zgaszone.") + "</p>" +
            '<p class="f9p-read">zachodzi ' + lit.length + " z " + n + " wejść · wyjście zapala " + hits + " z " + total + " kombinacji</p>");
        }
      };
    },

    stany: function (cfg, ui) {
      var evs = U.events(cfg.tree), all = U.subsets(evs), lit = [], seen = {};
      function key(s) { return evs.filter(function (e) { return s.indexOf(e) >= 0; }).join(""); }
      return {
        on: {
          leaf: function (el) {
            var id = el.getAttribute("data-value"), k = lit.indexOf(id);
            if (k >= 0) lit.splice(k, 1); else lit.push(id);
          },
          clear: function () { lit = []; },
          reset: function () { lit = []; seen = {}; }
        },
        render: function () {
          seen[key(lit)] = true;
          var nSeen = 0, nTop = 0;
          var rows = all.map(function (s) {
            var k = key(s), vis = !!seen[k], top = U.evalTree(cfg.tree, s)[cfg.tree.id];
            if (vis) { nSeen++; if (top) nTop++; }
            return '<tr class="' + (k === key(lit) ? "is-now" : "") + (vis && top ? " is-hit" : "") + '">' +
              evs.map(function (e) { return '<td class="n">' + (s.indexOf(e) >= 0 ? "●" : "·") + "</td>"; }).join("") +
              '<td class="f9p6-top">' + (vis ? (top ? "aktywne" : "nie") : "?") + "</td></tr>";
          }).join("");
          var table = '<table class="lc-tbl f9p6-tbl"><thead><tr>' + evs.map(function (e) { return '<th scope="col" class="n">' + esc(e) + "</th>"; }).join("") +
            '<th scope="col">TOP</th></tr></thead><tbody>' + rows + "</tbody></table>";
          ui.set("stage", '<div class="f9p-row"><div class="f9p-cell" style="flex-grow:2">' + U.tree(cfg.tree, { lit: lit }, { click: "leaf" }) +
            '</div><div class="f9p-cell f9p6-side">' + table + "</div></div>");
          ui.set("body", '<p class="f9p-read">sprawdzone ' + nSeen + " z " + all.length + " kombinacji · TOP aktywne w " + nTop + "</p>");
        }
      };
    },

    parametry: function (cfg, ui) {
      var base = cfg.base, top0 = base.i * (1 - (1 - base.d) * (1 - base.s));
      function grid(nInit, nTop) {
        var c = 3, side = 100 * c, s = '<rect class="f9p-grid-bg" x="1" y="1" width="' + side + '" height="' + side + '"/>', lines = "";
        for (var k = 1; k < 10; k++) lines += "M" + (1 + k * 10 * c) + ",1 V" + (side + 1) + " M1," + (1 + k * 10 * c) + " H" + (side + 1);
        s += '<path class="f9p-grid-line" d="' + lines + '"/>';
        for (var i = 0; i < nInit; i++) {
          s += '<rect class="' + (i < nTop ? "f9p-grid-top" : "f9p-grid-init") + '" x="' + ((i % 10) * c + 1) + '" y="' + (Math.floor(i / 10) * c + 1) + '" width="' + c + '" height="' + c + '"/>';
        }
        return U.svgWrap(side + 2, side + 2, "Siatka 10 000 magazyno-lat: " + nInit + " z inicjacją, " + nTop + " z nieopanowanym pożarem.", s);
      }
      return {
        on: {
          base: function () {
            [["f9p9_i", base.i], ["f9p9_d", base.d], ["f9p9_s", base.s]].forEach(function (p) {
              var el = ui.root.querySelector("#" + p[0]);
              if (el) el.value = p[1];
            });
            ui.root.querySelectorAll("output[data-value]").forEach(function (o) {
              var el = ui.root.querySelector("#" + o.getAttribute("data-value"));
              if (el) o.textContent = el.value;
            });
          }
        },
        render: function () {
          var i = U.sliderValue(ui.root, "f9p9_i", base.i), d = U.sliderValue(ui.root, "f9p9_d", base.d), s = U.sliderValue(ui.root, "f9p9_s", base.s);
          var or = 1 - (1 - d) * (1 - s), top = i * or;
          var view = { p: { I: U.num(i, 3), D: U.num(d, 2), S: U.num(s, 2) }, values: { M: U.fmt(or, 3), TOP: U.fmt(top, 6) } };
          var nInit = Math.round(10000 * i), nTop = Math.round(10000 * top);
          ui.set("stage", '<div class="f9p-row"><div class="f9p-cell" style="flex-grow:2">' + U.tree(cfg.tree, view) +
            '</div><div class="f9p-cell"><h4>10 000 magazyno-lat</h4>' + grid(nInit, nTop) +
            '<p class="f9p-note">' + nInit + " z inicjacją · " + U.num(10000 * top, 1) + " nieopanowanych</p></div></div>");
          ui.set("body", '<p class="f9p-read">P(D ∪ S | I) = ' + U.fmt(or, 3) + " · P(TOP) = " + U.fmt(top, 6) +
            " · względem stanu bazowego ×" + U.fmt(top / top0, 2) + "</p>");
        }
      };
    }
  });

  // ---- P14: rycina lcSF -----------------------------------------------------------
  var C = window.lcCharts, S = window.lcSF;
  if (!C || !S) return;
  var XQ = [0, 0.2], YMAX = 16;  // oś Y: na 10 000 magazyno-lat

  S.register({
    f9_wspolna: {
      draw: function (v, P, k, state, vals) {
        var c = state.config, q = Number(vals.f9p14_q), or = 1 - (1 - c.d) * (1 - c.s), and = c.d * c.s;
        function y(q, local) { return 10000 * c.pi * (q + (1 - q) * local); }
        function line(local) { var pts = []; for (var j = 0; j <= 40; j++) { var x = XQ[1] * j / 40; pts.push([x, y(x, local)]); } return pts; }
        state.q = q; state.chain = y(q, or) / 10000; state.par = y(q, and) / 10000; state.floor = c.pi * q;
        state.chain0 = c.pi * or; state.par0 = c.pi * and;
        return { x: XQ, y: [0, YMAX],
          layers: [
            { type: "line", data: [[0, y(0, or)], [XQ[1], y(0, or)]], color: P.secondary, width: 1.4, dash: [5, 4] },
            { type: "line", data: [[0, y(0, and)], [XQ[1], y(0, and)]], color: P.niebo, width: 1.4, dash: [5, 4] },
            { type: "line", data: [[0, 0], [XQ[1], 10000 * c.pi * XQ[1]]], color: P.accent, width: 1.6, dash: [2, 3] },
            { type: "line", data: line(or), color: P.secondary, width: 2.6 },
            { type: "line", data: line(and), color: P.niebo, width: 2.6 },
            { type: "vline", x: q, color: P.subtle, width: 1.4, dash: [3, 3] },
            { type: "point", x: q, y: y(q, or), color: P.secondary, size: 11 },
            { type: "point", x: q, y: y(q, and), color: P.niebo, size: 11 }
          ],
          legend: [{ text: "łańcuch z C", color: P.secondary }, { text: "dwie samodzielne bariery z C", color: P.niebo },
                   { text: "bez wspólnej przyczyny", color: P.subtle, dash: true }, { text: "sam przekrój {I, C}: P(I) · q", color: P.accent, dash: true }],
          xName: "q = P(C | I): wspólne niepowodzenie obu funkcji", yName: "P(TOP) na 10 000 magazyno-lat",
          aria: "P(TOP) w funkcji prawdopodobieństwa wspólnej przyczyny q dla łańcucha i dla dwóch samodzielnych barier; q = " + q + "." };
      },
      update: function (root, state) {
        function rel(a, b) {
          var r = a / b;
          return r < 1.5 ? "+" + U.fmt(100 * (r - 1), 0) + "%" : "×" + U.fmt(r, 2);
        }
        S.setValue(root, "chain", U.fmt(state.chain, 6) + " (" + rel(state.chain, state.chain0) + ")");
        S.setValue(root, "par", U.fmt(state.par, 7) + " (" + rel(state.par, state.par0) + ")");
        S.setValue(root, "floor", U.fmt(state.floor, 6));
      }
    }
  });
})();
