// Silnik scen wykładu 06 „Zmienność i próg” w przeglądarce, bez serwera R.
//
// Kontener .z6p-scene[data-scene][data-config]; sloty [data-z6p-out="…"],
// przyciski [data-z6p="…"] (R: z6p_scene_ui, z6p_button, z6p_seg w stop_proto_common.R).
// Wbudowana scena:
//   kadry — stałe kadry policzone w R (SVG i opis każdego kadru w config.frames);
//           JS tylko przełącza (Wstecz / Dalej).
// Sceny losowe rejestrują pliki modules/proto_*.js:
//   lcZ6.register({ nazwa: function (cfg, ui, rnd) { return { on: {akcja: fn(btn)}, render: fn }; } });
// ui: root, slot(name), set(name, html) → true gdy treść się zmieniła, ends(f, n).
// Po każdej akcji, ruchu suwaka (input[type=range]) i zmianie pola silnik woła render().
// Narzędzia rysunkowe i rachunkowe: lcZ6.util (testy: tests/js/z6_sceny.test.js).
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
  // Losowanie N(0, 1) z generatora jednostajnego rnd (Box–Muller).
  function rnorm(rnd) {
    var u = 0, v = 0;
    while (u === 0) u = rnd();
    while (v === 0) v = rnd();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  }
  function dnorm(x, mu, sd) { var z = (x - mu) / sd; return Math.exp(-0.5 * z * z) / (sd * Math.sqrt(2 * Math.PI)); }
  // Dystrybuanta N(mu, sd): erfc z rozwinięcia Abramowitza–Steguna 7.1.26 poprawionego
  // do błędu < 1.2e-7 (Numerical Recipes, erfcc).
  function pnorm(x, mu, sd) {
    var z = (x - (mu || 0)) / (sd == null ? 1 : sd), a = Math.abs(z) / Math.SQRT2, t = 1 / (1 + 0.5 * a);
    var r = t * Math.exp(-a * a - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 +
      t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 +
      t * (-0.82215223 + t * 0.17087277)))))))));
    return z >= 0 ? 1 - 0.5 * r : 0.5 * r;
  }

  function svgWrap(w, h, aria, body) {
    return '<svg class="z6p-svg" style="max-width:' + w + 'px" viewBox="0 0 ' + w + " " + h + '" role="img" aria-label="' + esc(aria) + '">' + body + "</svg>";
  }
  function head(name, step) {
    return '<div class="z6p-head"><span class="z6p-name">' + esc(name) + '</span><span class="z6p-step">' + esc(step) + "</span></div>";
  }
  // Pozioma oś z podziałką (jak z6p_axis w R). labels: opcjonalne napisy podziałek.
  function axis(xOf, y, ticks, label, labels) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="z6p-axis" x1="' + g(xOf(lo)) + '" x2="' + g(xOf(hi)) + '" y1="' + g(y) + '" y2="' + g(y) + '"/>' +
      ticks.map(function (t, i) {
        return '<line class="z6p-axis" x1="' + g(xOf(t)) + '" x2="' + g(xOf(t)) + '" y1="' + g(y) + '" y2="' + g(y + 5) +
          '"/><text class="z6p-tick" x="' + g(xOf(t)) + '" y="' + g(y + 18) + '">' + esc(labels ? labels[i] : t) + "</text>";
      }).join("") +
      (label ? '<text class="z6p-subtle" x="' + g(xOf(hi)) + '" y="' + g(y + 34) + '" font-size="12" text-anchor="end">' + esc(label) + "</text>" : "");
  }
  // Pionowa oś z podziałką po lewej stronie.
  function axisY(yOf, x, ticks, labels) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="z6p-axis" x1="' + g(x) + '" x2="' + g(x) + '" y1="' + g(yOf(lo)) + '" y2="' + g(yOf(hi)) + '"/>' +
      ticks.map(function (t, i) {
        return '<line class="z6p-axis" x1="' + g(x - 5) + '" x2="' + g(x) + '" y1="' + g(yOf(t)) + '" y2="' + g(yOf(t)) +
          '"/><text class="z6p-tick" style="text-anchor:end" x="' + g(x - 8) + '" y="' + g(yOf(t) + 4) + '">' + esc(labels ? labels[i] : t) + "</text>";
      }).join("");
  }
  // Ścieżka krzywej normalnej (jak z6p_norm_path w R).
  function normPath(mu, sd, xOf, yOf, from, to, close, n) {
    n = n || 120;
    var pts = [];
    for (var i = 0; i < n; i++) {
      var x = from + (to - from) * i / (n - 1);
      pts.push(fmt(xOf(x), 1) + "," + fmt(yOf(dnorm(x, mu, sd)), 1));
    }
    if (!close) return "M" + pts.join(" L");
    return "M" + fmt(xOf(from), 1) + "," + fmt(yOf(0), 1) + " L" + pts.join(" L") + " L" + fmt(xOf(to), 1) + "," + fmt(yOf(0), 1) + " Z";
  }
  // Przełącznik z6p_seg: zaznacza kliknięty przycisk w jego grupie, zwraca data-value.
  function press(btn) {
    btn.parentNode.querySelectorAll("button").forEach(function (b) { b.setAttribute("aria-pressed", b === btn ? "true" : "false"); });
    return btn.getAttribute("data-value");
  }
  function sliderValue(root, id, fallback) {
    var el = root.querySelector("#" + id);
    return el ? Number(el.value) : fallback;
  }

  // --- Narzędzia rycin lcSF (krzywe normalne) ---------------------------------
  // Punkty krzywej N(mu, sd) między from i to: [[x, gęstość], …] (warstwa „line”).
  function curve(mu, sd, from, to, step) {
    var pts = [], n = Math.round((to - from) / (step || 0.1));
    for (var i = 0; i <= n; i++) { var x = from + (to - from) * i / n; pts.push([x, dnorm(x, mu, sd)]); }
    return pts;
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
    }
  };

  function init(root) {
    if (root.dataset.ready) return;
    var make = scenes[root.getAttribute("data-scene")];
    if (!make) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.getAttribute("data-config") || "{}");
    var rnd = cfg.seed ? mulberry32(cfg.seed) : Math.random;
    var ui = {
      root: root,
      slot: function (name) { return root.querySelector('[data-z6p-out="' + name + '"]'); },
      set: function (name, html) {
        var el = ui.slot(name);
        if (!el || el.innerHTML === html) return false;
        el.innerHTML = html;
        return true;
      },
      ends: function (f, n) {
        var prev = root.querySelector('[data-z6p="prev"]'), next = root.querySelector('[data-z6p="next"]');
        if (prev) prev.disabled = f === 0;
        if (next) next.disabled = f === n - 1;
      }
    };
    var scene = make(cfg, ui, rnd);
    root.addEventListener("click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-z6p]");
      if (!btn || !root.contains(btn) || btn.disabled) return;
      var fn = scene.on && scene.on[btn.getAttribute("data-z6p")];
      if (!fn) return;
      fn(btn);
      scene.render();
    });
    root.addEventListener("input", function (e) { if (e.target.type === "range") scene.render(); });
    root.addEventListener("change", function (e) { if (e.target.type !== "range") scene.render(); });
    scene.render();
  }

  function scan() {
    if (typeof document === "undefined") return;
    document.querySelectorAll(".z6p-scene:not([data-ready])").forEach(init);
  }
  function register(more) {
    Object.keys(more).forEach(function (k) { scenes[k] = more[k]; });
    scan();
  }

  (typeof window !== "undefined" ? window : globalThis).lcZ6 = {
    register: register, scenes: scenes, init: init,
    util: { g: g, esc: esc, fmt: fmt, sum: sum, mean: mean, range: range, mulberry32: mulberry32,
            rnorm: rnorm, dnorm: dnorm, pnorm: pnorm, svgWrap: svgWrap, head: head, axis: axis,
            axisY: axisY, normPath: normPath, press: press, sliderValue: sliderValue,
            curve: curve, fmtP: fmtP, per1000: per1000 }
  };

  if (typeof document === "undefined") return;
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
