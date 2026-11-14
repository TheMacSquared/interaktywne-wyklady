// Silnik scen wykładu 07 „Czas życia elementu” w przeglądarce, bez serwera R.
//
// Kontener .c7p-scene[data-scene][data-config]; sloty [data-c7p-out="…"],
// przyciski [data-c7p="…"] (R: c7p_scene_ui, c7p_button, c7p_seg w stop_proto_common.R).
// Wbudowana scena:
//   kadry — stałe kadry policzone w R (SVG i opis każdego kadru w config.frames);
//           JS tylko przełącza (Wstecz / Dalej).
// Sceny losowe rejestrują pliki modules/proto_*.js:
//   lcC7.register({ nazwa: function (cfg, ui, rnd) { return { on: {akcja: fn(btn)}, render: fn }; } });
// ui: root, slot(name), set(name, html) → true gdy treść się zmieniła, ends(f, n).
// Po każdej akcji, ruchu suwaka (input[type=range]) i zmianie pola silnik woła render().
// Narzędzia rysunkowe i rachunkowe (w tym Weibull: wR, wh, wf, wq, wMean, park): lcC7.util (testy: tests/js/c7_sceny.test.js).
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
    return '<svg class="c7p-svg" style="max-width:' + w + 'px" viewBox="0 0 ' + w + " " + h + '" role="img" aria-label="' + esc(aria) + '">' + body + "</svg>";
  }
  function head(name, step) {
    return '<div class="c7p-head"><span class="c7p-name">' + esc(name) + '</span><span class="c7p-step">' + esc(step) + "</span></div>";
  }
  // Pozioma oś z podziałką (jak c7p_axis w R). labels: opcjonalne napisy podziałek.
  function axis(xOf, y, ticks, label, labels) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="c7p-axis" x1="' + g(xOf(lo)) + '" x2="' + g(xOf(hi)) + '" y1="' + g(y) + '" y2="' + g(y) + '"/>' +
      ticks.map(function (t, i) {
        return '<line class="c7p-axis" x1="' + g(xOf(t)) + '" x2="' + g(xOf(t)) + '" y1="' + g(y) + '" y2="' + g(y + 5) +
          '"/><text class="c7p-tick" x="' + g(xOf(t)) + '" y="' + g(y + 18) + '">' + esc(labels ? labels[i] : t) + "</text>";
      }).join("") +
      (label ? '<text class="c7p-subtle" x="' + g(xOf(hi)) + '" y="' + g(y + 34) + '" font-size="12" text-anchor="end">' + esc(label) + "</text>" : "");
  }
  // Pionowa oś z podziałką po lewej stronie.
  function axisY(yOf, x, ticks, labels) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="c7p-axis" x1="' + g(x) + '" x2="' + g(x) + '" y1="' + g(yOf(lo)) + '" y2="' + g(yOf(hi)) + '"/>' +
      ticks.map(function (t, i) {
        return '<line class="c7p-axis" x1="' + g(x - 5) + '" x2="' + g(x) + '" y1="' + g(yOf(t)) + '" y2="' + g(yOf(t)) +
          '"/><text class="c7p-tick" style="text-anchor:end" x="' + g(x - 8) + '" y="' + g(yOf(t) + 4) + '">' + esc(labels ? labels[i] : t) + "</text>";
      }).join("");
  }
  // Ścieżka krzywej normalnej.
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
  // Przełącznik c7p_seg: zaznacza kliknięty przycisk w jego grupie, zwraca data-value.
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

  // --- Czas życia: Weibull (β = 1 to wykładniczy z MTTF = η) ---------------------
  function wR(t, beta, eta) { return t <= 0 ? 1 : Math.exp(-Math.pow(t / eta, beta)); }
  function wF(t, beta, eta) { return 1 - wR(t, beta, eta); }
  function wH(t, beta, eta) { return Math.pow(t / eta, beta); }                      // skumulowany hazard
  function wh(t, beta, eta) { return (beta / eta) * Math.pow(t / eta, beta - 1); }   // hazard (1/h)
  function wf(t, beta, eta) { return wh(t, beta, eta) * wR(t, beta, eta); }          // gęstość (1/h)
  function wq(u, beta, eta) { return eta * Math.pow(-Math.log(1 - u), 1 / beta); }   // kwantyl
  // Czas, do którego niezawodność spada do r (wzór 7.16).
  function wTime(r, beta, eta) { return eta * Math.pow(-Math.log(r), 1 / beta); }
  // Funkcja gamma (Lanczos, g = 7), dokładność ~1e-13 dla x > 0.
  var LG = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
    -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
  function gammaFn(x) {
    if (x < 0.5) return Math.PI / (Math.sin(Math.PI * x) * gammaFn(1 - x));
    x -= 1;
    var a = LG[0], t = x + 7.5;
    for (var i = 1; i < 9; i++) a += LG[i] / (x + i);
    return Math.sqrt(2 * Math.PI) * Math.pow(t, x + 0.5) * Math.exp(-t) * a;
  }
  function wMean(beta, eta) { return eta * gammaFn(1 + 1 / beta); }                  // wzór 7.14
  // Park n wentylatorów: czasy życia = kwantyle (i − 0.5)/n, rosnąco (jak c7p_park w R).
  function park(n, beta, eta) {
    var out = [];
    for (var i = 1; i <= n; i++) out.push(wq((i - 0.5) / n, beta, eta));
    return out;
  }
  // Punkty krzywej y = f(x) między from i to: [[x, y], …] (warstwa „line” lcSF).
  function fcurve(f, from, to, n) {
    var pts = []; n = n || 200;
    for (var i = 0; i <= n; i++) { var x = from + (to - from) * i / n; pts.push([x, f(x)]); }
    return pts;
  }
  // Godziny bez miejsc dziesiętnych („1812 h” → fmtH(1812.5, 1) = „1812.5 h”).
  function fmtH(x, d) { return Number(x).toFixed(d || 0) + " h"; }

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
      slot: function (name) { return root.querySelector('[data-c7p-out="' + name + '"]'); },
      set: function (name, html) {
        var el = ui.slot(name);
        if (!el || el.innerHTML === html) return false;
        el.innerHTML = html;
        return true;
      },
      ends: function (f, n) {
        var prev = root.querySelector('[data-c7p="prev"]'), next = root.querySelector('[data-c7p="next"]');
        if (prev) prev.disabled = f === 0;
        if (next) next.disabled = f === n - 1;
      }
    };
    var scene = make(cfg, ui, rnd);
    root.addEventListener("click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-c7p]");
      if (!btn || !root.contains(btn) || btn.disabled) return;
      var fn = scene.on && scene.on[btn.getAttribute("data-c7p")];
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
    document.querySelectorAll(".c7p-scene:not([data-ready])").forEach(init);
  }
  function register(more) {
    Object.keys(more).forEach(function (k) { scenes[k] = more[k]; });
    scan();
  }

  (typeof window !== "undefined" ? window : globalThis).lcC7 = {
    register: register, scenes: scenes, init: init,
    util: { g: g, esc: esc, fmt: fmt, sum: sum, mean: mean, range: range, mulberry32: mulberry32,
            rnorm: rnorm, dnorm: dnorm, pnorm: pnorm, svgWrap: svgWrap, head: head, axis: axis,
            axisY: axisY, normPath: normPath, press: press, sliderValue: sliderValue,
            curve: curve, fmtP: fmtP, per1000: per1000,
            wR: wR, wF: wF, wH: wH, wh: wh, wf: wf, wq: wq, wTime: wTime, gammaFn: gammaFn,
            wMean: wMean, park: park, fcurve: fcurve, fmtH: fmtH }
  };

  if (typeof document === "undefined") return;
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
