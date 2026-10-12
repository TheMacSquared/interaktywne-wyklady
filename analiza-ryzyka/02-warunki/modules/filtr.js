// Dwa widgety rozdziału „Naturalne częstości w Bananpolu”.
//
// Kalendarz (.lc-cal[data-config]): rok pracy hali jako 50 tygodni × 5 dni × 2 zmiany
// = 500 komórek. Przegrzania skupiają się latem, kropka to incydent. Przycisk
// [data-cal="sort"] układa zmiany w dwie grupy obok siebie (z przegrzaniem i bez,
// po 10 w kolumnie, incydenty na dole), a odczyty [data-cal-out] pokazują wtedy
// udział incydentów w każdej grupie.
//
// Mozaika (.lc-mz[data-config]): kwadrat wszystkich zmian. Szerokość pasa to udział
// przegrzania (suwak cfg.slider), wysokość zamalowanej części to P(incydent | pas),
// przerywana linia to P(incydent) w całym zakładzie.
//
// Całość działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function fmt(x, d) { return x.toFixed(d); }

  // Deterministyczny generator (mulberry32): ten sam kalendarz przy każdym otwarciu.
  function mulberry32(a) {
    return function () {
      a |= 0; a = a + 0x6D2B79F5 | 0;
      var t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }

  // --- Kalendarz ------------------------------------------------------------

  var WEEKS = 50, DAYS = 5;
  var MONTHS = ["sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz", "paź", "lis", "gru"];
  var DAY_NAMES = ["pon", "wt", "śr", "czw", "pt"];
  var START = Date.UTC(2026, 0, 5);

  function initCalendar(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var nHot = cfg.hot || 50, incHot = cfg.inc_hot || 6, incCold = cfg.inc_cold || 2;
    var rnd = mulberry32(cfg.seed || 20260915);
    var board = root.querySelector(".lc-cal-board");
    var btn = root.querySelector('[data-cal="sort"]');
    var status = root.querySelector(".lc-cal-status");
    var out = {};
    root.querySelectorAll("[data-cal-out]").forEach(function (e) { out[e.getAttribute("data-cal-out")] = e; });
    var second = out.label2 && out.label2.closest(".lc-read");
    var sorted = false;

    // Przegrzania ważone porą roku: szczyt w lipcu (tydzień 29).
    function weekWeight(w) { var d = w - 29; return 0.5 + 9 * Math.exp(-(d * d) / 32); }

    var cells = [];
    for (var w = 0; w < WEEKS; w++)
      for (var d = 0; d < DAYS; d++)
        for (var s = 0; s < 2; s++) cells.push({ w: w, d: d, s: s, hot: false, inc: false });

    // Losowanie ważone bez zwracania: klucz u^(1/waga), bierzemy największe.
    cells.forEach(function (c) { c.key = Math.pow(rnd(), 1 / weekWeight(c.w)); });
    cells.slice().sort(function (a, b) { return b.key - a.key; })
      .slice(0, nHot).forEach(function (c) { c.hot = true; });
    function sample(arr, n) {
      arr = arr.slice();
      for (var i = 0; i < n; i++) {
        var j = i + Math.floor(rnd() * (arr.length - i));
        var t = arr[i]; arr[i] = arr[j]; arr[j] = t;
        arr[i].inc = true;
      }
    }
    var hot = cells.filter(function (c) { return c.hot; });
    var cold = cells.filter(function (c) { return !c.hot; });
    sample(hot, incHot);
    sample(cold, incCold);
    // W każdej grupie najpierw incydenty (lądują na dole), potem reszta w kolejności dat.
    [hot, cold].forEach(function (g) {
      g.filter(function (c) { return c.inc; }).concat(g.filter(function (c) { return !c.inc; }))
        .forEach(function (c, i) { c.slot = i; });
    });
    var nCold = cold.length, incAll = incHot + incCold;
    var hotCols = Math.ceil(nHot / 10), coldCols = Math.ceil(nCold / 10);

    var monthEls = [], prev = -1;
    for (w = 0; w < WEEKS; w++) {
      var mon = new Date(START + w * 7 * 86400000).getUTCMonth();
      if (mon !== prev) {
        var m = document.createElement("div");
        m.className = "lc-cal-month"; m.textContent = MONTHS[mon];
        board.appendChild(m); monthEls.push({ el: m, w: w }); prev = mon;
      }
    }
    var dayEls = DAY_NAMES.map(function (name) {
      var el = document.createElement("div");
      el.className = "lc-cal-day"; el.textContent = name;
      board.appendChild(el); return el;
    });
    function groupLabel(text, cls) {
      var el = document.createElement("div");
      el.className = "lc-cal-group " + cls; el.textContent = text;
      board.appendChild(el); return el;
    }
    var hotLabel = groupLabel("z przegrzaniem", "is-hot");
    var coldLabel = groupLabel("bez przegrzania", "is-cold");
    cells.forEach(function (c) {
      var el = document.createElement("div");
      el.className = "lc-cal-cell" + (c.hot ? " is-hot" : "") + (c.inc ? " is-inc" : "");
      el.title = "Tydzień " + (c.w + 1) + ", " + DAY_NAMES[c.d] + ", zmiana " +
        (c.s ? "nocna" : "dzienna") + (c.hot ? ", przegrzanie" : "") + (c.inc ? ", incydent" : "");
      board.appendChild(el); c.el = el;
    });

    var geo = {};
    function layout() {
      var W = board.clientWidth;
      if (!W) return;
      var labelW = W < 420 ? 28 : 36;
      var pitch = Math.max(4, Math.floor((W - labelW) / WEEKS));
      var big = pitch >= 10;
      geo = {
        labelW: labelW, pitch: pitch, size: pitch - (big ? 2 : 1),
        dayGap: big ? 6 : 3, top: big ? 20 : 16
      };
      geo.gridH = DAYS * 2 * pitch + (DAYS - 1) * geo.dayGap;
      // Po sortowaniu: dwa bloki po 10 wierszy, wyrównane do dołu kalendarza.
      geo.bottom = geo.top + geo.gridH - pitch;
      geo.coldX = hotCols * pitch + labelW;
      board.style.height = (geo.top + geo.gridH + 4) + "px";
      var font = big ? 11 : 9;
      monthEls.forEach(function (m) {
        m.el.style.left = (labelW + m.w * pitch) + "px"; m.el.style.fontSize = font + "px";
      });
      dayEls.forEach(function (el, i) {
        el.style.top = (geo.top + i * (2 * pitch + geo.dayGap)) + "px";
        el.style.height = (2 * pitch) + "px"; el.style.fontSize = font + "px";
      });
      var labTop = geo.bottom - 10 * pitch - (big ? 4 : 2);
      hotLabel.style.left = "0px"; hotLabel.style.top = labTop + "px";
      coldLabel.style.left = (geo.coldX + coldCols * pitch - (pitch - geo.size)) + "px";
      coldLabel.style.top = labTop + "px";
      hotLabel.style.fontSize = coldLabel.style.fontSize = (font + 1) + "px";
      cells.forEach(function (c) {
        c.el.style.width = geo.size + "px"; c.el.style.height = geo.size + "px";
      });
      render(true);
    }

    function render(instant) {
      if (instant) board.classList.add("is-instant");
      cells.forEach(function (c) {
        var x, y, delay = 0;
        if (sorted) {
          var cols = c.hot ? hotCols : coldCols;
          x = (c.hot ? 0 : geo.coldX) + (c.slot % cols) * geo.pitch;
          y = geo.bottom - Math.floor(c.slot / cols) * geo.pitch;
          delay = instant || REDUCE ? 0 : c.w * 12;
        } else {
          x = geo.labelW + c.w * geo.pitch;
          y = geo.top + c.d * (2 * geo.pitch + geo.dayGap) + c.s * geo.pitch;
        }
        c.el.style.transform = "translate(" + x + "px," + y + "px)";
        c.el.style.transitionDelay = delay + "ms";
      });
      board.classList.toggle("is-sorted", sorted);
      if (instant) { void board.offsetWidth; board.classList.remove("is-instant"); }
    }

    function ratio(k, n) {
      return k + " / " + n + " = " + (k / n).toFixed(3).replace(/0+$/, "").replace(/\.$/, "");
    }
    function texts() {
      if (sorted) {
        out.label.textContent = "Z przegrzaniem";
        out.value.textContent = ratio(incHot, nHot);
        out.label2.textContent = "Bez przegrzania";
        out.value2.textContent = ratio(incCold, nCold);
        status.textContent = "Ten sam rok, posortowany według przegrzania.";
        btn.querySelector("span").textContent = "Wróć do kalendarza";
      } else {
        out.label.textContent = "Wszystkie zmiany";
        out.value.textContent = ratio(incAll, cells.length);
        status.textContent = "Bursztyn: przegrzanie. Kropka: incydent.";
        btn.querySelector("span").textContent = "Posortuj według przegrzania";
      }
      if (second) second.hidden = !sorted;
      btn.setAttribute("aria-pressed", String(sorted));
    }

    btn.addEventListener("click", function () { sorted = !sorted; render(false); texts(); });
    // Rozdział może być ukryty przy starcie: układ liczymy, gdy panel dostanie szerokość.
    if (window.ResizeObserver) new ResizeObserver(function () { layout(); }).observe(board);
    else window.addEventListener("resize", layout);
    layout(); texts();
  }

  // --- Mozaika --------------------------------------------------------------

  var MW = 640, MH = 380, X0 = 64, Y0 = 40, PW = 548, PH = 292, YMAX = 0.20;

  function initMosaic(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var share = cfg.share || 0.10, pHot = cfg.p_hot || 0.12, pCold = cfg.p_cold || 0.005;
    var stage = root.querySelector(".lc-mz-stage");
    var out = {};
    root.querySelectorAll("[data-mz-out]").forEach(function (e) { out[e.getAttribute("data-mz-out")] = e; });

    var s = svg("svg", { viewBox: "0 0 " + MW + " " + MH, class: "lc-mz-svg", role: "img",
      "aria-label": "Mozaika zmian: szerokość pasa to udział przegrzania, wysokość zamalowanej części to prawdopodobieństwo incydentu w pasie." }, stage);
    function Y(p) { return Y0 + PH - p / YMAX * PH; }
    for (var k = 0; k <= 4; k++) {
      var v = YMAX * k / 4;
      svg("line", { x1: X0, x2: X0 + PW, y1: Y(v), y2: Y(v), class: "lc-mz-grid" }, s);
      svg("text", { x: X0 - 8, y: Y(v) + 4, class: "lc-mz-tick", "text-anchor": "end" }, s, fmt(v, 2));
    }
    svg("text", { x: 14, y: Y0 + PH / 2, class: "lc-mz-axis", "text-anchor": "middle",
      transform: "rotate(-90 14 " + (Y0 + PH / 2) + ")" }, s, "P(incydent | grupa)");
    var hotBg = svg("rect", { class: "lc-mz-hot", y: Y0, height: PH }, s);
    var coldBg = svg("rect", { class: "lc-mz-cold", y: Y0, height: PH }, s);
    var hotInc = svg("rect", { class: "lc-mz-inc", y: Y(pHot), height: Y0 + PH - Y(pHot) }, s);
    var coldInc = svg("rect", { class: "lc-mz-inc", y: Y(pCold), height: Y0 + PH - Y(pCold) }, s);
    var sep = svg("line", { y1: Y0, y2: Y0 + PH, class: "lc-mz-sep" }, s);
    var ref = svg("line", { x1: X0, x2: X0 + PW, class: "lc-mz-ref" }, s);
    var hotLab = svg("text", { x: X0, y: Y0 - 12, class: "lc-mz-lab is-hot" }, s);
    var coldLab = svg("text", { x: X0 + PW, y: Y0 - 12, class: "lc-mz-lab", "text-anchor": "end" }, s);
    svg("text", { x: X0 + PW / 2, y: Y0 + PH + 30, class: "lc-mz-axis", "text-anchor": "middle" }, s,
      "udział zmian w zakładzie");

    function render() {
      var pAll = share * pHot + (1 - share) * pCold;
      var xs = X0 + share * PW;
      [hotBg, hotInc].forEach(function (r) { r.setAttribute("x", X0); r.setAttribute("width", xs - X0); });
      [coldBg, coldInc].forEach(function (r) { r.setAttribute("x", xs); r.setAttribute("width", X0 + PW - xs); });
      sep.setAttribute("x1", xs); sep.setAttribute("x2", xs);
      ref.setAttribute("y1", Y(pAll)); ref.setAttribute("y2", Y(pAll));
      hotLab.textContent = "przegrzanie · " + fmt(share, 2);
      coldLab.textContent = "brak przegrzania · " + fmt(1 - share, 2);
      if (out.hot) out.hot.textContent = fmt(pHot, 2);
      if (out.cold) out.cold.textContent = fmt(pCold, 3);
      if (out.all) out.all.textContent = fmt(pAll, 4).replace(/0+$/, "");
    }

    var input = cfg.slider && document.getElementById(cfg.slider);
    if (input) {
      var onChange = function () {
        var v = Number(input.value);
        if (isFinite(v)) { share = v; render(); }
      };
      if (window.jQuery) window.jQuery(input).on("change", onChange);
      input.addEventListener("input", onChange);
    }
    render();
  }

  function scan() {
    document.querySelectorAll(".lc-cal:not([data-ready])").forEach(initCalendar);
    document.querySelectorAll(".lc-mz:not([data-ready])").forEach(initMosaic);
  }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
