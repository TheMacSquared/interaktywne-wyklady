// Silnik scen wykładu 08 „Niezawodność systemu” w przeglądarce, bez serwera R.
//
// Kontener .s8p-scene[data-scene][data-config]; sloty [data-s8p-out="…"],
// przyciski [data-s8p="…"] (R: s8p_scene_ui, s8p_button, s8p_seg w stop_proto_common.R).
// Wbudowane sceny (cały rysunek liczy R, JS tylko przełącza gotowe SVG):
//   kadry  — config.frames = [{name, svg, body}]; Wstecz / Dalej.
//   widoki — config.views = {klucz: {svg, body}}; klucz to wartości wciśniętych
//            przycisków kolejnych przełączników s8p_seg połączone znakiem "|".
//   uklad  — klikalny schemat blokowy: config.views[maska] = {svg, body}, maska
//            bitowa działających bloków (bit i − 1 = blok i); klik bloku
//            ([data-s8p="blk"][data-value=i]) zmienia jego stan. Kilka schematów
//            pod przełącznikiem: config.archs = {klucz: {n, views}}.
// Sceny losowe rejestrują pliki modules/proto_*.js:
//   lcS8.register({ nazwa: function (cfg, ui, rnd) { return { on: {akcja: fn(btn)}, render: fn }; } });
// ui: root, slot(name), set(name, html) → true gdy treść się zmieniła, ends(f, n).
// Po każdej akcji, ruchu suwaka (input[type=range]) i zmianie pola silnik woła render().
// Narzędzia rysunkowe i rachunkowe: lcS8.util (testy: tests/js/s8_sceny.test.js).
(function () {
  "use strict";

  function g(x) { return x === 0 ? "0" : String(Number(x.toPrecision(6))); }  // sprintf("%g")
  function esc(s) { return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;"); }
  function fmt(x, d) { return Number(x).toFixed(d); }  // kropka dziesiętna
  function sum(a) { var s = 0; for (var i = 0; i < a.length; i++) s += +a[i]; return s; }
  function mean(a) { return a.length ? sum(a) / a.length : NaN; }
  function range(a, b) { var o = []; for (var i = a; i <= b; i++) o.push(i); return o; }

  // Generator o stałym ziarnie (ten sam co w wykładach 04–05).
  function mulberry32(a) {
    return function () {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      var t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }
  // --- Rachunek niezawodności (jak R/risk_math.R) ------------------------------
  function series(rs) { var p = 1; for (var i = 0; i < rs.length; i++) p *= rs[i]; return p; }
  function parallel(rs) { var q = 1; for (var i = 0; i < rs.length; i++) q *= 1 - rs[i]; return 1 - q; }
  function expR(t, mttf) { return Math.exp(-t / mttf); }           // R(t) = e^(−t/MTTF)
  function common(rIndep, q) { return (1 - q) * rIndep; }           // wzór (8.10)
  // Układ k z n jednakowych elementów o niezawodności r.
  function kOfN(k, n, r) {
    var s = 0;
    for (var j = k; j <= n; j++) {
      var c = 1;
      for (var i = 1; i <= j; i++) c = c * (n - j + i) / i;
      s += c * Math.pow(r, j) * Math.pow(1 - r, n - j);
    }
    return s;
  }
  // Beta-factor dla dwóch jednakowych gałęzi (wzór 8.12); lt = λt.
  function betaR(beta, lt) {
    var a = 1 - Math.exp(-(1 - beta) * lt);
    return Math.exp(-beta * lt) * (1 - a * a);
  }

  function svgWrap(w, h, aria, body) {
    return '<svg class="s8p-svg" style="max-width:' + w + 'px" viewBox="0 0 ' + w + " " + h + '" role="img" aria-label="' + esc(aria) + '">' + body + "</svg>";
  }
  function head(name, step) {
    return '<div class="s8p-head"><span class="s8p-name">' + esc(name) + '</span><span class="s8p-step">' + esc(step) + "</span></div>";
  }
  // Pozioma oś z podziałką (jak s8p_axis w R). labels: opcjonalne napisy podziałek.
  function axis(xOf, y, ticks, label, labels) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="s8p-axis" x1="' + g(xOf(lo)) + '" x2="' + g(xOf(hi)) + '" y1="' + g(y) + '" y2="' + g(y) + '"/>' +
      ticks.map(function (t, i) {
        return '<line class="s8p-axis" x1="' + g(xOf(t)) + '" x2="' + g(xOf(t)) + '" y1="' + g(y) + '" y2="' + g(y + 5) +
          '"/><text class="s8p-tick" x="' + g(xOf(t)) + '" y="' + g(y + 18) + '">' + esc(labels ? labels[i] : t) + "</text>";
      }).join("") +
      (label ? '<text class="s8p-subtle" x="' + g(xOf(hi)) + '" y="' + g(y + 34) + '" font-size="12" text-anchor="end">' + esc(label) + "</text>" : "");
  }
  // Pionowa oś z podziałką po lewej stronie.
  function axisY(yOf, x, ticks, labels) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="s8p-axis" x1="' + g(x) + '" x2="' + g(x) + '" y1="' + g(yOf(lo)) + '" y2="' + g(yOf(hi)) + '"/>' +
      ticks.map(function (t, i) {
        return '<line class="s8p-axis" x1="' + g(x - 5) + '" x2="' + g(x) + '" y1="' + g(yOf(t)) + '" y2="' + g(yOf(t)) +
          '"/><text class="s8p-tick" style="text-anchor:end" x="' + g(x - 8) + '" y="' + g(yOf(t) + 4) + '">' + esc(labels ? labels[i] : t) + "</text>";
      }).join("");
  }
  // Przełącznik s8p_seg: zaznacza kliknięty przycisk w jego grupie, zwraca data-value.
  function press(btn) {
    btn.parentNode.querySelectorAll("button").forEach(function (b) { b.setAttribute("aria-pressed", b === btn ? "true" : "false"); });
    return btn.getAttribute("data-value");
  }
  function sliderValue(root, id, fallback) {
    var el = root.querySelector("#" + id);
    return el ? Number(el.value) : fallback;
  }

  // risk_fmt_p() z R/risk_block.R: trzy miejsca, dla małych p dwie cyfry znaczące.
  function fmtP(x) {
    if (!isFinite(x)) return "—";
    var d = 3;
    if (x > 0 && x < 0.01) d = Math.max(3, Math.min(12, Math.ceil(-Math.log10(x)) + 2));
    return x.toFixed(d);
  }
  // Naturalna częstość: „159 na 1000”; poniżej 1 na 1000 z jedną cyfrą po kropce.
  function per1000(p) {
    var k = 1000 * p;
    return (k >= 10 ? k.toFixed(0) : k >= 0.95 ? k.toFixed(0) : k.toFixed(1)) + " na 1000";
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
    // Widoki policzone w R: klucz z wciśniętych przycisków przełączników.
    widoki: function (cfg, ui) {
      function key() {
        var parts = [];
        ui.root.querySelectorAll(".lc-seg").forEach(function (seg) {
          var b = seg.querySelector('button[aria-pressed="true"]');
          if (b) parts.push(b.getAttribute("data-value"));
        });
        return parts.join("|");
      }
      return {
        on: { view: function (btn) { press(btn); } },
        render: function () {
          var v = cfg.views[key()];
          if (!v) return;
          ui.set("stage", v.svg);
          ui.set("body", v.body);
        }
      };
    },
    // Klikalny schemat blokowy: stan to maska bitowa działających bloków.
    uklad: function (cfg, ui) {
      // cfg.archs = {klucz: {n, views}}: kilka schematów pod przełącznikiem [data-s8p="arch"].
      var keys = cfg.archs ? Object.keys(cfg.archs) : null;
      var cur = keys ? cfg.archs[keys[0]] : cfg;
      var full = Math.pow(2, cur.n) - 1, mask = full;
      return {
        on: {
          blk: function (btn) { mask = toggleBit(mask, Number(btn.getAttribute("data-value"))); },
          reset: function () { mask = full; },
          arch: function (btn) {
            cur = cfg.archs[press(btn)];
            full = Math.pow(2, cur.n) - 1; mask = full;
          }
        },
        render: function () {
          var v = cur.views[mask];
          ui.set("stage", v.svg);
          ui.set("body", v.body);
          var r = ui.root.querySelector('[data-s8p="reset"]');
          if (r) r.disabled = mask === full;
        },
        mask: function () { return mask; }
      };
    }
  };
  // Zmiana stanu bloku i (od 1) w masce bitowej.
  function toggleBit(mask, i) { return mask ^ (1 << (i - 1)); }

  function init(root) {
    if (root.dataset.ready) return;
    var make = scenes[root.getAttribute("data-scene")];
    if (!make) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.getAttribute("data-config") || "{}");
    var rnd = cfg.seed ? mulberry32(cfg.seed) : Math.random;
    var ui = {
      root: root,
      slot: function (name) { return root.querySelector('[data-s8p-out="' + name + '"]'); },
      set: function (name, html) {
        var el = ui.slot(name);
        if (!el || el.innerHTML === html) return false;
        el.innerHTML = html;
        return true;
      },
      ends: function (f, n) {
        var prev = root.querySelector('[data-s8p="prev"]'), next = root.querySelector('[data-s8p="next"]');
        if (prev) prev.disabled = f === 0;
        if (next) next.disabled = f === n - 1;
      }
    };
    var scene = make(cfg, ui, rnd);
    root.addEventListener("click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-s8p]");
      if (!btn || !root.contains(btn) || btn.disabled) return;
      var fn = scene.on && scene.on[btn.getAttribute("data-s8p")];
      if (!fn) return;
      fn(btn);
      scene.render();
    });
    // Bloki schematu to <g role="button">: Enter i spacja działają jak klik.
    root.addEventListener("keydown", function (e) {
      if (e.key !== "Enter" && e.key !== " ") return;
      var el = e.target.closest && e.target.closest('g[data-s8p]');
      if (!el || !root.contains(el)) return;
      e.preventDefault();
      var fn = scene.on && scene.on[el.getAttribute("data-s8p")];
      if (fn) { fn(el); scene.render(); }
    });
    root.addEventListener("input", function (e) { if (e.target.type === "range") scene.render(); });
    root.addEventListener("change", function (e) { if (e.target.type !== "range") scene.render(); });
    scene.render();
  }

  function scan() {
    if (typeof document === "undefined") return;
    document.querySelectorAll(".s8p-scene:not([data-ready])").forEach(init);
  }
  function register(more) {
    Object.keys(more).forEach(function (k) { scenes[k] = more[k]; });
    scan();
  }

  (typeof window !== "undefined" ? window : globalThis).lcS8 = {
    register: register, scenes: scenes, init: init,
    util: { g: g, esc: esc, fmt: fmt, sum: sum, mean: mean, range: range, mulberry32: mulberry32,
            svgWrap: svgWrap, head: head, axis: axis, axisY: axisY, press: press,
            sliderValue: sliderValue, fmtP: fmtP, per1000: per1000, toggleBit: toggleBit,
            series: series, parallel: parallel, expR: expR, common: common, kOfN: kOfN, betaR: betaR }
  };

  if (typeof document === "undefined") return;
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
