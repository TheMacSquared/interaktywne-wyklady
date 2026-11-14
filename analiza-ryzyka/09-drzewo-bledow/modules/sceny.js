// Silnik scen wykładu 09 „Analiza drzewa błędów” w przeglądarce, bez serwera R.
//
// Kontener .f9p-scene[data-scene][data-config]; sloty [data-f9p-out="…"],
// przyciski [data-f9p="…"] (R: f9p_scene_ui, f9p_button, f9p_seg w stop_proto_common.R).
// Wbudowane sceny:
//   kadry  — stałe kadry policzone w R (SVG i opis każdego kadru w config.frames);
//            JS tylko przełącza (Wstecz / Dalej).
//   drzewo — kadry albo widoki drzewa błędów: R podaje drzewa (config.trees) i dla
//            każdego kadru listę rysunków (panels: które liście zachodzą, liczby,
//            wyróżnienia); rysuje lcF9.util.tree. config.nav: "kadry" | "seg" | "none".
// Sceny interaktywne rejestrują pliki modules/proto_*.js:
//   lcF9.register({ nazwa: function (cfg, ui, rnd) { return { on: {akcja: fn(btn)}, render: fn }; } });
// ui: root, slot(name), set(name, html) → true gdy treść się zmieniła, ends(f, n).
// Po każdej akcji, ruchu suwaka (input[type=range]) i zmianie pola silnik woła render().
//
// Drzewo (JSON z R: f9p_gate, f9p_leaf):
//   liść    {id, label: [wiersze], sym, ev}          ev — zdarzenie bazowe (powtórzony
//                                                    liść ma inne id i to samo ev)
//   bramka  {id, label: [wiersze], gate: "and" | "or", short, children: [...]}
// Narzędzia rysunkowe i rachunkowe: lcF9.util (testy: tests/js/f9_sceny.test.js).
(function () {
  "use strict";

  function g(x) { return x === 0 ? "0" : String(Number(x.toPrecision(6))); }  // sprintf("%g")
  function esc(s) { return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;"); }
  function fmt(x, d) { return Number(x).toFixed(d); }  // kropka dziesiętna
  function range(a, b) { var o = []; for (var i = a; i <= b; i++) o.push(i); return o; }
  // Liczba bez końcowych zer, najwyżej d miejsc po kropce (jak f9p_num w R).
  function num(x, d) {
    var s = Number(x).toFixed(d == null ? 6 : d);
    return s.indexOf(".") < 0 ? s : s.replace(/0+$/, "").replace(/\.$/, "");
  }
  // risk_fmt_p() z R/risk_block.R: trzy miejsca, dla małych p dwie cyfry znaczące.
  function fmtP(x) {
    if (!isFinite(x)) return "—";
    var d = 3;
    if (x > 0 && x < 0.01) d = Math.max(3, Math.min(12, Math.ceil(-Math.log10(x)) + 2));
    return x.toFixed(d);
  }

  function svgWrap(w, h, aria, body) {
    return '<svg class="f9p-svg" style="max-width:' + w + 'px" viewBox="0 0 ' + g(w) + " " + g(h) + '" role="img" aria-label="' + esc(aria) + '">' + body + "</svg>";
  }
  function head(name, step) {
    return '<div class="f9p-head"><span class="f9p-name">' + esc(name) + '</span><span class="f9p-step">' + esc(step) + "</span></div>";
  }
  // Przełącznik f9p_seg: zaznacza kliknięty przycisk w jego grupie, zwraca data-value.
  function press(btn) {
    btn.parentNode.querySelectorAll("button").forEach(function (b) { b.setAttribute("aria-pressed", b === btn ? "true" : "false"); });
    return btn.getAttribute("data-value");
  }
  function sliderValue(root, id, fallback) {
    var el = root.querySelector("#" + id);
    return el ? Number(el.value) : fallback;
  }

  // --- Logika i rachunek drzewa ---------------------------------------------------
  function isLeaf(n) { return !n.children || !n.children.length; }
  function evOf(n) { return n.ev || n.id; }
  function walk(n, fn, parent, depth) {
    fn(n, parent || null, depth || 0);
    (n.children || []).forEach(function (c) { walk(c, fn, n, (depth || 0) + 1); });
  }
  function leaves(tree) { var o = []; walk(tree, function (n) { if (isLeaf(n)) o.push(n); }); return o; }
  // Zdarzenia bazowe drzewa bez powtórzeń, w kolejności rysunku.
  function events(tree) {
    var o = [];
    leaves(tree).forEach(function (n) { if (o.indexOf(evOf(n)) < 0) o.push(evOf(n)); });
    return o;
  }
  // Stan każdego węzła przy zachodzących zdarzeniach bazowych lit: {id: true/false}.
  function evalTree(tree, lit) {
    var out = {};
    (function ev(n) {
      var v;
      if (isLeaf(n)) v = lit.indexOf(evOf(n)) >= 0;
      else {
        var k = n.children.map(ev);
        v = n.gate === "and" ? k.every(Boolean) : k.some(Boolean);
      }
      out[n.id] = v;
      return v;
    })(tree);
    return out;
  }
  // Rachunek bramka po bramce (każdy liść traktowany jak niezależny): {id: p}.
  // p: prawdopodobieństwa zdarzeń bazowych według ev. Poprawny, gdy żadne zdarzenie
  // się nie powtarza.
  function probTree(tree, p) {
    var out = {};
    (function pr(n) {
      var v;
      if (isLeaf(n)) v = p[evOf(n)];
      else {
        var k = n.children.map(pr);
        v = n.gate === "and" ? k.reduce(function (a, b) { return a * b; }, 1)
                             : 1 - k.reduce(function (a, b) { return a * (1 - b); }, 1);
      }
      out[n.id] = v;
      return v;
    })(tree);
    return out;
  }
  // Minimalne przekroje: rozwinięcie do sumy iloczynów, idempotentność, pochłanianie.
  function cutSets(tree) {
    function uniq(a) { return a.filter(function (x, i) { return a.indexOf(x) === i; }).sort(); }
    function subset(a, b) { return a.every(function (x) { return b.indexOf(x) >= 0; }); }
    function absorb(sets) {
      var keys = {}, u = [];
      sets.forEach(function (s) { var k = s.join("|"); if (!keys[k]) { keys[k] = 1; u.push(s); } });
      return u.filter(function (s) { return !u.some(function (t) { return t !== s && t.length < s.length && subset(t, s); }); });
    }
    function cs(n) {
      if (isLeaf(n)) return [[evOf(n)]];
      var kids = n.children.map(cs);
      if (n.gate === "or") return absorb([].concat.apply([], kids));
      return absorb(kids.reduce(function (acc, k) {
        var o = [];
        acc.forEach(function (a) { k.forEach(function (b) { o.push(uniq(a.concat(b))); }); });
        return o;
      }, [[]]));
    }
    return cs(tree).sort(function (a, b) { return a.length - b.length || a.join().localeCompare(b.join()); });
  }
  // Dokładne P(suma przekrojów) z zasady włączeń i wyłączeń (zdarzenia niezależne).
  function probCuts(cuts, p) {
    var total = 0, m = cuts.length;
    for (var mask = 1; mask < (1 << m); mask++) {
      var set = [], bits = 0;
      for (var j = 0; j < m; j++) if (mask & (1 << j)) {
        bits++;
        cuts[j].forEach(function (e) { if (set.indexOf(e) < 0) set.push(e); });
      }
      var prod = set.reduce(function (a, e) { return a * p[e]; }, 1);
      total += bits % 2 ? prod : -prod;
    }
    return total;
  }
  // Wszystkie podzbiory listy zdarzeń (do tabeli stanów), od pustego.
  function subsets(evs) {
    var o = [];
    for (var mask = 0; mask < (1 << evs.length); mask++) {
      o.push(evs.filter(function (e, i) { return mask & (1 << (evs.length - 1 - i)); }));
    }
    return o;
  }

  // --- Rysunek drzewa -------------------------------------------------------------
  var GEO = {
    full: { sx: 150, sy: 140, bw: 150, bh: 46, gw: 46, gh: 36, r: 26, padL: 8, padR: 64, padT: 6, leafH: 116, lab: 13, sym: 18 },
    // tight: kilka drzew obok siebie — węższe kolumny, te same napisy.
    tight: { sx: 112, sy: 132, bw: 128, bh: 44, gw: 44, gh: 34, r: 24, padL: 6, padR: 58, padT: 6, leafH: 110, lab: 12, sym: 17 },
    mini: { sx: 50, sy: 62, bw: 44, bh: 20, gw: 22, gh: 17, r: 13, padL: 4, padR: 4, padT: 3, leafH: 30, lab: 0, sym: 11 }
  };
  // Pozycje węzłów: liście zajmują kolejne kolumny, rodzic stoi nad środkiem dzieci.
  function layout(tree, mini) {
    var G = mini === "tight" ? GEO.tight : mini ? GEO.mini : GEO.full, slot = 0, nodes = [], maxY = 0;
    (function place(n, parent, depth) {
      var it = { n: n, parent: parent, depth: depth, leaf: isLeaf(n) };
      nodes.push(it);
      if (it.leaf) it.col = slot++;
      else {
        var kids = n.children.map(function (c) { return place(c, it, depth + 1); });
        it.kids = kids;
        it.col = (kids[0].col + kids[kids.length - 1].col) / 2;
      }
      it.y = G.padT + depth * G.sy;
      maxY = Math.max(maxY, it.y + (it.leaf ? G.leafH : G.bh + 8 + G.gh));
      return it;
    })(tree, null, 0);
    nodes.forEach(function (it) { it.x = G.padL + (it.col + 0.5) * G.sx; });
    return { nodes: nodes, w: G.padL + slot * G.sx + G.padR, h: maxY + 4, G: G };
  }
  function gatePath(kind, cx, yt, G) {
    var w = G.gw / 2, yb = yt + G.gh;
    if (kind === "and") {
      return "M" + g(cx - w) + "," + g(yb) + " L" + g(cx - w) + "," + g(yt + w) + " A" + g(w) + " " + g(w) + " 0 0 1 " +
        g(cx + w) + "," + g(yt + w) + " L" + g(cx + w) + "," + g(yb) + " Z";
    }
    return "M" + g(cx - w) + "," + g(yb) + " Q" + g(cx - w) + "," + g(yt + G.gh * 0.3) + " " + g(cx) + "," + g(yt) +
      " Q" + g(cx + w) + "," + g(yt + G.gh * 0.3) + " " + g(cx + w) + "," + g(yb) +
      " Q" + g(cx) + "," + g(yb - G.gh * 0.3) + " " + g(cx - w) + "," + g(yb) + " Z";
  }
  // Rysunek drzewa jako <svg>. view: {lit, no, mark, dim, ghost, hide, values, p, mini};
  // opts.click — nazwa akcji: liście stają się klikalne (data-f9p, data-value = ev).
  function tree(spec, view, opts) {
    view = view || {}; opts = opts || {};
    var L = layout(spec, view.mini ? true : view.tight ? "tight" : false), G = L.G, mini = !!view.mini;
    var on = evalTree(spec, view.lit || []);
    function has(list, it) { return !!list && (list.indexOf(it.n.id) >= 0 || (it.leaf && list.indexOf(evOf(it.n)) >= 0)); }
    function hidden(it) { return has(view.hide, it); }
    var edges = "", body = "";
    L.nodes.forEach(function (it) {
      if (it.leaf || hidden(it)) return;
      var yb = it.y + G.bh + 8 + G.gh, bus = it.y + G.sy - (mini ? 8 : 16);
      it.kids.forEach(function (c) {
        if (hidden(c)) return;
        var cls = "f9p-edge" + (on[c.n.id] ? " is-on" : "") + (has(view.ghost, c) ? " is-ghost" : "") + (has(view.dim, c) ? " is-dim" : "");
        var d = "M" + g(it.x) + "," + g(yb - 6) + " V" + g(bus) + " H" + g(c.x) + " V" + g(c.y + 2);
        // Zapalone gałęzie rysujemy na wierzchu.
        if (on[c.n.id]) edges += '<path class="' + cls + '" d="' + d + '"/>';
        else edges = '<path class="' + cls + '" d="' + d + '"/>' + edges;
      });
    });
    L.nodes.forEach(function (it) {
      if (hidden(it)) return;
      var n = it.n, cls = "f9p-node" + (on[n.id] ? " is-on" : "") + (has(view.no, it) ? " is-no" : "") +
        (has(view.mark, it) ? " is-mark" : "") + (has(view.dim, it) ? " is-dim" : "") + (has(view.ghost, it) ? " is-ghost" : "");
      var lines = n.label || [], s = "", i;
      if (it.leaf) {
        var cy = it.y + G.r + 2, click = opts.click ? ' data-f9p="' + esc(opts.click) + '" data-value="' + esc(evOf(n)) + '" role="button" tabindex="0"' : "";
        s += '<circle class="f9p-shape" cx="' + g(it.x) + '" cy="' + g(cy) + '" r="' + g(G.r) + '"/>' +
          '<text class="f9p-sym" x="' + g(it.x) + '" y="' + g(cy + G.sym * 0.36) + '" font-size="' + G.sym + '">' + esc(n.sym || n.id) + "</text>";
        if (!mini) {
          for (i = 0; i < lines.length; i++) {
            s += '<text class="f9p-lab" x="' + g(it.x) + '" y="' + g(it.y + 2 * G.r + 22 + i * 16) + '">' + esc(lines[i]) + "</text>";
          }
          var pv = view.p && view.p[n.id] != null ? view.p[n.id] : (view.p && view.p[evOf(n)]);
          if (pv != null) s += '<text class="f9p-pv" x="' + g(it.x) + '" y="' + g(it.y + 2 * G.r + 22 + lines.length * 16 + 4) + '">' + esc(pv) + "</text>";
        }
        body += '<g class="' + cls + '"' + click + ">" + s + "</g>";
      } else {
        var gy = it.y + G.bh + 8;
        s += '<rect class="f9p-shape" x="' + g(it.x - G.bw / 2) + '" y="' + g(it.y) + '" width="' + g(G.bw) + '" height="' + g(G.bh) + '" rx="' + (mini ? 3 : 5) + '"/>';
        if (mini) {
          s += '<text class="f9p-sym" x="' + g(it.x) + '" y="' + g(it.y + G.bh * 0.72) + '" font-size="' + (G.sym - 1) + '">' + esc(n.short || n.id) + "</text>";
        } else {
          var y0 = it.y + G.bh / 2 + 5 - (lines.length - 1) * 8;
          for (i = 0; i < lines.length; i++) {
            s += '<text class="f9p-lab" x="' + g(it.x) + '" y="' + g(y0 + i * 16) + '" style="font-size:' + G.lab + 'px"' + (it.depth === 0 ? ' font-weight="700"' : "") + ">" + esc(lines[i]) + "</text>";
          }
        }
        s += '<line class="f9p-edge' + (on[n.id] ? " is-on" : "") + '" x1="' + g(it.x) + '" x2="' + g(it.x) + '" y1="' + g(it.y + G.bh) + '" y2="' + g(gy + 2) + '"/>' +
          '<path class="f9p-gate" d="' + gatePath(n.gate, it.x, gy, G) + '"/>';
        if (!mini) {
          s += '<text class="f9p-gate-t" x="' + g(it.x) + '" y="' + g(gy + G.gh * (n.gate === "and" ? 0.68 : 0.62)) + '">' + (n.gate === "and" ? "AND" : "OR") + "</text>";
          var val = view.values && view.values[n.id];
          if (val != null) s += '<text class="f9p-val" x="' + g(it.x + G.gw / 2 + 8) + '" y="' + g(gy + G.gh / 2 + 5) + '">' + esc(val) + "</text>";
        }
        body += '<g class="' + cls + '">' + s + "</g>";
      }
    });
    var lit = (view.lit || []).join(", ");
    var aria = opts.aria || ("Drzewo błędów: " + (spec.label || []).join(" ") + (lit ? "; zachodzą zdarzenia: " + lit : "") + ".");
    var out = svgWrap(L.w, L.h, aria, edges + body);
    return view.maxWidth ? out.replace("max-width:" + L.w + "px", "max-width:" + view.maxWidth + "px") : out;
  }
  // Jeden rysunek kadru: tytuł, drzewo (albo gotowy SVG z R) i wiersz pod spodem.
  function panel(trees, v, opts) {
    var pic = v.svg || tree(trees[v.tree || "main"], v, opts);
    return '<div class="f9p-cell' + (v.hit ? " is-hit" : "") + '"' + (v.grow ? ' style="flex-grow:' + Number(v.grow) + '"' : "") + ">" + (v.title ? "<h4>" + esc(v.title) + "</h4>" : "") + pic +
      (v.note ? '<p class="f9p-note">' + esc(v.note) + "</p>" : "") + "</div>";
  }
  function panels(trees, list, opts) {
    var mini = list.length && list.every(function (v) { return v.mini; });
    // Kilka rysunków w jednym kadrze: drzewa w węższej geometrii.
    if (list.length > 1) list.forEach(function (v) { if (!v.mini) v.tight = true; });
    return '<div class="f9p-row' + (mini ? " is-mini" : "") + '">' + list.map(function (v) { return panel(trees, v, opts); }).join("") + "</div>";
  }

  var scenes = {
    // Kadry policzone w R: config.frames = [{name, svg, body}].
    kadry: function (cfg, ui) {
      var f = 0, n = cfg.frames.length;
      return {
        on: {
          prev: function () { f = Math.max(0, f - 1); },
          next: function () { f = Math.min(n - 1, f + 1); }
        },
        render: function () {
          var fr = cfg.frames[f];
          ui.set("head", head(fr.name, "Kadr " + (f + 1) + " z " + n));
          ui.set("stage", fr.svg);
          ui.set("body", fr.body);
          ui.ends(f, n);
        }
      };
    },
    // Kadry albo widoki drzewa: config = {trees, frames: [{name, body, panels}], nav}.
    drzewo: function (cfg, ui) {
      var f = 0, n = cfg.frames.length;
      return {
        on: {
          prev: function () { f = Math.max(0, f - 1); },
          next: function () { f = Math.min(n - 1, f + 1); },
          view: function (btn) { f = Number(press(btn)); }
        },
        render: function () {
          var fr = cfg.frames[f];
          if (cfg.nav === "kadry") ui.set("head", head(fr.name, "Kadr " + (f + 1) + " z " + n));
          ui.set("stage", panels(cfg.trees, fr.panels));
          ui.set("body", fr.body || "");
          ui.ends(f, n);
        }
      };
    }
  };

  function init(root) {
    if (root.dataset.ready) return;
    var make = scenes[root.getAttribute("data-scene")];
    if (!make) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.getAttribute("data-config") || "{}");
    var ui = {
      root: root,
      slot: function (name) { return root.querySelector('[data-f9p-out="' + name + '"]'); },
      set: function (name, html) {
        var el = ui.slot(name);
        if (!el || el.innerHTML === html) return false;
        el.innerHTML = html;
        return true;
      },
      ends: function (f, n) {
        var prev = root.querySelector('[data-f9p="prev"]'), next = root.querySelector('[data-f9p="next"]');
        if (prev) prev.disabled = f === 0;
        if (next) next.disabled = f === n - 1;
      }
    };
    var scene = make(cfg, ui, Math.random);
    function act(el) {
      if (!el || !root.contains(el) || el.disabled) return;
      var fn = scene.on && scene.on[el.getAttribute("data-f9p")];
      if (!fn) return;
      fn(el);
      scene.render();
    }
    root.addEventListener("click", function (e) { act(e.target.closest && e.target.closest("[data-f9p]")); });
    // Klikalne węzły SVG (role=button) działają też z klawiatury.
    root.addEventListener("keydown", function (e) {
      if (e.key !== "Enter" && e.key !== " ") return;
      var el = e.target.closest && e.target.closest('[data-f9p][role="button"]');
      if (el) { e.preventDefault(); act(el); }
    });
    function outputs() {
      root.querySelectorAll("output[data-value]").forEach(function (out) {
        var input = root.querySelector("#" + out.getAttribute("data-value"));
        if (input) out.textContent = input.value;
      });
    }
    root.addEventListener("input", function (e) { if (e.target.type === "range") { outputs(); scene.render(); } });
    root.addEventListener("change", function (e) { if (e.target.type !== "range") scene.render(); });
    scene.render();
  }

  function scan() {
    if (typeof document === "undefined") return;
    document.querySelectorAll(".f9p-scene:not([data-ready])").forEach(init);
  }
  function register(more) {
    Object.keys(more).forEach(function (k) { scenes[k] = more[k]; });
    scan();
  }

  (typeof window !== "undefined" ? window : globalThis).lcF9 = {
    register: register, scenes: scenes, init: init,
    util: { g: g, esc: esc, fmt: fmt, num: num, range: range, fmtP: fmtP, svgWrap: svgWrap, head: head,
            press: press, sliderValue: sliderValue, isLeaf: isLeaf, evOf: evOf, walk: walk, leaves: leaves,
            events: events, evalTree: evalTree, probTree: probTree, cutSets: cutSets, probCuts: probCuts,
            subsets: subsets, layout: layout, tree: tree, panel: panel, panels: panels }
  };

  if (typeof document === "undefined") return;
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
