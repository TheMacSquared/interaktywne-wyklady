// Silnik scen wykładu 10 „Od modelu do decyzji” w przeglądarce, bez serwera R.
//
// Kontener .i10p-scene[data-scene][data-config]; sloty [data-i10p-out="…"],
// przyciski [data-i10p="…"] (R: i10p_scene_ui, i10p_button, i10p_seg w stop_proto_common.R).
// Wbudowana scena:
//   kadry — stałe kadry policzone w R (SVG i opis każdego kadru w config.frames);
//           JS tylko przełącza (Wstecz / Dalej).
// Sceny interaktywne rejestrują pliki modules/proto_*.js:
//   lcI10.register({ nazwa: function (cfg, ui, rnd) { return { on: {akcja: fn(btn)}, render: fn }; } });
// ui: root, slot(name), set(name, html) → true gdy treść się zmieniła, ends(f, n).
// Po każdej akcji, ruchu suwaka (input[type=range]) i zmianie pola silnik woła render().
// Narzędzia rysunkowe i rachunkowe: lcI10.util (testy: tests/js/i10_sceny.test.js).
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
  function svgWrap(w, h, aria, body) {
    return '<svg class="i10p-svg" style="max-width:' + w + 'px" viewBox="0 0 ' + w + " " + h + '" role="img" aria-label="' + esc(aria) + '">' + body + "</svg>";
  }
  function head(name, step) {
    return '<div class="i10p-head"><span class="i10p-name">' + esc(name) + '</span><span class="i10p-step">' + esc(step) + "</span></div>";
  }
  // Pozioma oś z podziałką (jak i10p_axis w R). labels: opcjonalne napisy podziałek.
  function axis(xOf, y, ticks, label, labels) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="i10p-axis" x1="' + g(xOf(lo)) + '" x2="' + g(xOf(hi)) + '" y1="' + g(y) + '" y2="' + g(y) + '"/>' +
      ticks.map(function (t, i) {
        return '<line class="i10p-axis" x1="' + g(xOf(t)) + '" x2="' + g(xOf(t)) + '" y1="' + g(y) + '" y2="' + g(y + 5) +
          '"/><text class="i10p-tick" x="' + g(xOf(t)) + '" y="' + g(y + 18) + '">' + esc(labels ? labels[i] : t) + "</text>";
      }).join("") +
      (label ? '<text class="i10p-subtle" x="' + g(xOf(hi)) + '" y="' + g(y + 34) + '" font-size="12" text-anchor="end">' + esc(label) + "</text>" : "");
  }
  // Pionowa oś z podziałką po lewej stronie.
  function axisY(yOf, x, ticks, labels) {
    var lo = Math.min.apply(null, ticks), hi = Math.max.apply(null, ticks);
    return '<line class="i10p-axis" x1="' + g(x) + '" x2="' + g(x) + '" y1="' + g(yOf(lo)) + '" y2="' + g(yOf(hi)) + '"/>' +
      ticks.map(function (t, i) {
        return '<line class="i10p-axis" x1="' + g(x - 5) + '" x2="' + g(x) + '" y1="' + g(yOf(t)) + '" y2="' + g(yOf(t)) +
          '"/><text class="i10p-tick" style="text-anchor:end" x="' + g(x - 8) + '" y="' + g(yOf(t) + 4) + '">' + esc(labels ? labels[i] : t) + "</text>";
      }).join("");
  }
  // Przełącznik i10p_seg: zaznacza kliknięty przycisk w jego grupie, zwraca data-value.
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
  // Naturalna częstość na 10 000 misji: „16 na 10 000”; poniżej 10 z jedną cyfrą po kropce.
  function per10000(p) {
    var k = 10000 * p;
    return (k >= 9.95 ? k.toFixed(0) : k.toFixed(1)) + " na 10 000";
  }

  // --- Rachunek misji (jak risk_mission_analysis() w R/risk_math.R) ---------------
  // Wszystkie parametry przychodzą z R w cfg (i10p_cfg w stop_proto_common.R):
  // mttf, beta, eta, base_time, power_r, controller_r, init, sens, efficacy, time.
  // Niezawodność wentylatora (wzór 10.4).
  function fanR(cfg, t, model) {
    return model === "weibull" ? Math.exp(-Math.pow(t / cfg.eta, cfg.beta)) : Math.exp(-t / cfg.mttf);
  }
  // o: {time, model, intervention, stress, sens, init, power_r, controller_r} — braki z cfg.
  function mission(cfg, o) {
    o = o || {};
    var time = o.time == null ? cfg.time : o.time, m = o.stress == null ? 1 : o.stress;
    var sens = o.sens == null ? cfg.sens : o.sens, id = o.intervention || "none";
    var fan = Math.pow(fanR(cfg, time, o.model || "exp"), m);
    var k = time / cfg.base_time * m;
    var power = Math.pow(o.power_r == null ? cfg.power_r : o.power_r, k);
    var controller = Math.pow(o.controller_r == null ? cfg.controller_r : o.controller_r, k);
    var init = Math.min(1, (o.init == null ? cfg.init : o.init) * m), miss = Math.min(1, (1 - sens) * m);
    // Założona skuteczność działania: cfg.efficacy bazowo, słabsza w ostrożnym scenariuszu.
    var e = Math.max(0, Math.min(1, cfg.efficacy * (2 - m))), branches = 2;
    if (id === "detector") miss *= 1 - e;
    else if (id === "prevention") init *= 1 - e;
    else if (id === "power") power = 1 - (1 - power) * (1 - power);
    else if (id === "fan") branches = 3;
    var par = 1 - Math.pow(1 - fan, branches), sys = power * controller * par;
    return { fan_r: fan, power_r: power, controller_r: controller, parallel_r: par, system_r: sys,
             initiation: init, miss: miss, cooling_failure: 1 - sys,
             top: init * (1 - (1 - miss) * sys), efficacy: e, branches: branches };
  }
  // Jednostronna górna granica ufności dla p przy zerze zdarzeń w n próbach (wzór 10.3).
  function zeroUpper(n, conf) { return 1 - Math.pow(1 - (conf == null ? 0.95 : conf), 1 / n); }
  // Wzór Bayesa (10.2): P(I | alarm).
  function bayes(prev, sens, fpr) {
    var den = sens * prev + fpr * (1 - prev);
    return den === 0 ? NaN : sens * prev / den;
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
      slot: function (name) { return root.querySelector('[data-i10p-out="' + name + '"]'); },
      set: function (name, html) {
        var el = ui.slot(name);
        if (!el || el.innerHTML === html) return false;
        el.innerHTML = html;
        return true;
      },
      ends: function (f, n) {
        var prev = root.querySelector('[data-i10p="prev"]'), next = root.querySelector('[data-i10p="next"]');
        if (prev) prev.disabled = f === 0;
        if (next) next.disabled = f === n - 1;
      }
    };
    var scene = make(cfg, ui, rnd);
    root.addEventListener("click", function (e) {
      var btn = e.target.closest && e.target.closest("[data-i10p]");
      if (!btn || !root.contains(btn) || btn.disabled) return;
      var fn = scene.on && scene.on[btn.getAttribute("data-i10p")];
      if (!fn) return;
      fn(btn);
      scene.render();
    });
    root.addEventListener("input", function (e) {
      if (e.target.type !== "range") return;
      var out = root.querySelector('output[data-value="' + e.target.id + '"]');
      if (out) out.textContent = e.target.value;
      scene.render();
    });
    root.addEventListener("change", function (e) { if (e.target.type !== "range") scene.render(); });
    scene.render();
  }

  function scan() {
    if (typeof document === "undefined") return;
    document.querySelectorAll(".i10p-scene:not([data-ready])").forEach(init);
  }
  function register(more) {
    Object.keys(more).forEach(function (k) { scenes[k] = more[k]; });
    scan();
  }

  (typeof window !== "undefined" ? window : globalThis).lcI10 = {
    register: register, scenes: scenes, init: init,
    util: { g: g, esc: esc, fmt: fmt, sum: sum, mean: mean, range: range, mulberry32: mulberry32,
            svgWrap: svgWrap, head: head, axis: axis, axisY: axisY, press: press,
            sliderValue: sliderValue, fmtP: fmtP, per10000: per10000, fanR: fanR,
            mission: mission, zeroUpper: zeroUpper, bayes: bayes }
  };

  if (typeof document === "undefined") return;
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
