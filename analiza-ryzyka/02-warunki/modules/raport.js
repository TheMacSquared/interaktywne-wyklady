// Scena „Raport kierownika” (rozdział o prawdopodobieństwie całkowitym).
//
// Kontener .lc-rk[data-config] z listą kadrów (cfg.frames): każdy kadr podaje udział
// przeciążenia, liczby incydentów w obu halach (n_o, n_n) i to, czy kropki stoją
// w halach, czy w segregatorze po prawej. Hale mają tę samą wysokość, więc ich
// powierzchnia jest proporcjonalna do liczby zmian. Przyciski [data-rk="prev|next"].
//
// Całość działa w przeglądarce, bez serwera.
(function () {
  var VW = 720, VH = 340;
  var HX = 24, HY = 44, HW = 420, HH = 224, GAP = 12;   // hale
  var BX = 520, BY = 44, BW = 176, BH = 224;             // segregator
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var K = REDUCE ? 0 : 1;

  function mulberry32(a) {
    return function () {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      var t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var frames = cfg.frames || [];
    var stage = root.querySelector(".lc-rk-stage");
    var caption = root.querySelector(".lc-rk-caption");
    var readout = root.querySelector(".lc-rk-readout");
    var nameEl = root.querySelector('[data-rk-out="name"]');
    var stepEl = root.querySelector('[data-rk-out="step"]');
    var prevBtn = root.querySelector('[data-rk="prev"]'), nextBtn = root.querySelector('[data-rk="next"]');

    function mk(cls, text) {
      var d = document.createElement("div");
      d.className = cls;
      if (text != null) d.textContent = text;
      stage.appendChild(d);
      return d;
    }
    // Pozycja i rozmiar w przestrzeni sceny, w procentach.
    function pl(x, y, w, h, el) {
      el.style.left = (x / VW * 100) + "%";
      el.style.top = (y / VH * 100) + "%";
      if (w != null) el.style.width = (w / VW * 100) + "%";
      if (h != null) el.style.height = (h / VH * 100) + "%";
    }
    function setDot(el, p) {
      el.style.left = (p.x / VW * 100) + "%";
      el.style.top = (p.y / VH * 100) + "%";
    }
    function hallRects(share) {
      var inner = HW - GAP, wo = inner * share;
      return { o: { x: HX, w: wo }, n: { x: HX + wo + GAP, w: inner - wo } };
    }
    // Kropki w hali bez nakładania się, deterministycznie.
    function scatter(rnd, r, n, minD) {
      var pts = [], d = minD, tries = 0, m = 6;
      while (pts.length < n) {
        if (++tries > 3000) { d *= 0.9; tries = 0; }
        var x = r.x + m + rnd() * (r.w - 2 * m), y = HY + m + rnd() * (HH - 2 * m), ok = true;
        for (var j = 0; j < pts.length; j++) {
          if (Math.hypot(pts[j].x - x, pts[j].y - y) < d) { ok = false; break; }
        }
        if (ok) pts.push({ x: x, y: y });
      }
      return pts;
    }
    function homes(share, nO, nN) {
      var r = hallRects(share), rnd = mulberry32(42 + nO * 100 + nN);
      return { o: scatter(rnd, r.o, nO, 13), n: scatter(rnd, r.n, nN, 30) };
    }
    // Miejsce w segregatorze: przeciążenie u góry, normalna praca na dole.
    function slot(d) {
      var col = d.i % 8, row = Math.floor(d.i / 8), x = 540 + col * 18;
      return d.g === "o" ? { x: x, y: 76 + row * 16 } : { x: x, y: 190 + row * 16 };
    }

    var hallO = mk("lc-rk-hall is-over"), hallN = mk("lc-rk-hall is-norm");
    var riskO = mk("lc-rk-risk", "0.15"), riskN = mk("lc-rk-risk", "0.01");
    var nameO = mk("lc-rk-hname", "Przeciążenie"), nameN = mk("lc-rk-hname", "Normalna praca");
    var binder = mk("lc-rk-binder"), bdiv = mk("lc-rk-bdiv");
    var lblA = mk("lc-rk-blbl", "Przeciążenie"), lblB = mk("lc-rk-blbl", "Normalna praca");
    pl(BX, BY, BW, BH, binder);
    pl(BX, 160, BW, null, bdiv);
    pl(540, 52, null, null, lblA);
    pl(540, 168, null, null, lblB);
    nameO.style.left = (HX / VW * 100) + "%";
    nameO.style.top = (276 / VH * 100) + "%";
    nameN.style.right = ((VW - (HX + HW)) / VW * 100) + "%";
    nameN.style.top = (276 / VH * 100) + "%";

    var dots = [], dotKey = "none", timers = [], current = -1;
    function later(fn, ms) { if (ms <= 0) fn(); else timers.push(setTimeout(fn, ms)); }
    function clearLater() { timers.forEach(clearTimeout); timers = []; }
    function removeDots() {
      dots.forEach(function (d) {
        var el = d.el;
        el.style.opacity = 0;
        setTimeout(function () { if (el.parentNode) el.parentNode.removeChild(el); }, 450 * K + 50);
      });
      dots = [];
    }
    function createDots(h) {
      var all = [];
      h.o.forEach(function (p, i) { all.push({ g: "o", i: i, home: p }); });
      h.n.forEach(function (p, i) { all.push({ g: "n", i: i, home: p }); });
      all.forEach(function (d, k) {
        var el = document.createElement("div");
        el.className = "lc-rk-dot";
        setDot(el, d.home);
        el.style.transitionDelay = (k * 20 * K) + "ms";
        stage.appendChild(el);
        d.el = el;
        dots.push(d);
      });
      requestAnimationFrame(function () {
        requestAnimationFrame(function () { dots.forEach(function (d) { d.el.style.opacity = 1; }); });
      });
    }
    function moveDots(target) {
      dots.forEach(function (d, k) {
        d.el.style.transitionDelay = (k * 45 * K) + "ms";
        setDot(d.el, target === "binder" ? slot(d) : d.home);
      });
    }

    function show(f) {
      clearLater();
      var F = frames[f], r = hallRects(F.share);
      pl(r.o.x, HY, r.o.w, HH, hallO);
      pl(r.n.x, HY, r.n.w, HH, hallN);
      pl(r.o.x + r.o.w / 2, 18, null, null, riskO);
      pl(r.n.x + r.n.w / 2, 18, null, null, riskN);
      stage.classList.toggle("is-binder", !!F.binder);
      var key = F.dots ? (F.n_o + "-" + F.n_n) : "none", rebuilt = key !== dotKey;
      if (rebuilt) {
        removeDots();
        dotKey = key;
        if (F.dots) createDots(homes(F.share, F.n_o, F.n_n));
      }
      if (F.dots) {
        var target = F.binder ? "binder" : "hall";
        later(function () { moveDots(target); }, rebuilt ? 1000 * K : 0);
      }
      if (nameEl) nameEl.textContent = F.name;
      if (stepEl) stepEl.textContent = "Kadr " + (f + 1) + " z " + frames.length;
      caption.textContent = F.text;
      readout.textContent = F.read;
      prevBtn.disabled = f === 0;
      nextBtn.disabled = f === frames.length - 1;
      stage.setAttribute("aria-label", F.name + ". " + F.text);
      current = f;
    }

    prevBtn.addEventListener("click", function () { if (current > 0) show(current - 1); });
    nextBtn.addEventListener("click", function () { if (current < frames.length - 1) show(current + 1); });
    show(0);
    setTimeout(function () { stage.classList.add("is-ready"); }, 60);
  }

  function scan() { document.querySelectorAll(".lc-rk:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
