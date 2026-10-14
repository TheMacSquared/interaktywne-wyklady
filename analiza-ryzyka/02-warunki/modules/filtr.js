// Dwa widgety rozdziału „Naturalne częstości w Bananpolu”.
//
// Kalendarz (.lc-cal[data-config]): rok pracy hali jako 50 tygodni × 5 dni × 2 zmiany
// = 500 komórek na rok. Przegrzania skupiają się latem, kropka to incydent.
// Przyciski [data-cal-sort="hot"|"inc"] układają zmiany w dwie grupy obok siebie
// (według przegrzania albo według incydentu; w każdej grupie zmiany z drugim
// zdarzeniem na dole), a odczyty [data-cal-out] pokazują udział w każdej grupie.
// Ponowne kliknięcie wciśniętego przycisku wraca do kalendarza.
//
// Mozaika (.lc-mz[data-config]): kwadrat wszystkich zmian. Szerokość pasa to udział
// przegrzania (suwak cfg.slider), wysokość zamalowanej części to P(incydent | pas),
// przerywana linia to P(incydent) w całym zakładzie.
//
// Scena „Odprawa” (.lc-od[data-config]): ten sam rejestr zmian na tablicy i trzy
// osoby z trzema pytaniami o incydent. Kadry [data-od="prev|next"] obrysowują
// kolejno wszystkie zmiany, zmiany z przegrzaniem i zmiany z incydentem.
//
// Kalendarz i scena losują rejestr tym samym generatorem i ziarnem, więc
// pokazują dokładnie te same zmiany.
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

  // --- Rejestr zmian ---------------------------------------------------------

  var WEEKS = 50, DAYS = 5;
  var MONTHS = ["sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz", "paź", "lis", "gru"];
  var DAY_NAMES = ["pon", "wt", "śr", "czw", "pt"];
  var START = Date.UTC(2026, 0, 5);

  // Lata × 50 tygodni × 5 dni × 2 zmiany. Przegrzania ważone porą roku (szczyt
  // w lipcu, tydzień 29), incydenty losowane osobno w obu grupach.
  function makeRegister(cfg) {
    var years = cfg.years || 1, nHot = cfg.hot || 50;
    var rnd = mulberry32(cfg.seed || 20260915);
    function weekWeight(w) { var d = w - 29; return 0.5 + 9 * Math.exp(-(d * d) / 32); }
    var cells = [];
    for (var y = 0; y < years; y++)
      for (var w = 0; w < WEEKS; w++)
        for (var d = 0; d < DAYS; d++)
          for (var s = 0; s < 2; s++) cells.push({ y: y, w: w, d: d, s: s, hot: false, inc: false });
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
    sample(cells.filter(function (c) { return c.hot; }), cfg.inc_hot || 0);
    sample(cells.filter(function (c) { return !c.hot; }), cfg.inc_cold || 0);
    return cells;
  }

  // --- Kalendarz ------------------------------------------------------------

  function initCalendar(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var years = cfg.years || 1, nHot = cfg.hot || 50, incHot = cfg.inc_hot || 6, incCold = cfg.inc_cold || 2;
    var board = root.querySelector(".lc-cal-board");
    var btns = Array.prototype.slice.call(root.querySelectorAll("[data-cal-sort]"));
    btns.forEach(function (b) { b.dataset.label = b.querySelector("span").textContent; });
    var status = root.querySelector(".lc-cal-status");
    var out = {};
    root.querySelectorAll("[data-cal-out]").forEach(function (e) { out[e.getAttribute("data-cal-out")] = e; });
    var second = out.label2 && out.label2.closest(".lc-read");
    var mode = null;  // null = kalendarz, "hot" = według przegrzania, "inc" = według incydentu

    var cells = makeRegister(cfg);
    var nAll = cells.length, nCold = nAll - nHot, incAll = incHot + incCold, nNoInc = nAll - incAll;
    // Po sortowaniu każda grupa ma tyle wierszy, ile kalendarz (10 na rok).
    var rows = 10 * years;
    var SORTS = {
      hot: { key: "hot", mark: "inc", labels: ["z przegrzaniem", "bez przegrzania"] },
      inc: { key: "inc", mark: "hot", labels: ["z incydentem", "bez incydentu"] }
    };
    // W każdej grupie najpierw zmiany z drugim zdarzeniem (lądują na dole), potem reszta w kolejności dat.
    Object.keys(SORTS).forEach(function (k) {
      var sk = SORTS[k], groups = [[], []];
      cells.forEach(function (c) { groups[c[sk.key] ? 0 : 1].push(c); });
      groups.forEach(function (g, gi) {
        g.filter(function (c) { return c[sk.mark]; }).concat(g.filter(function (c) { return !c[sk.mark]; }))
          .forEach(function (c, i) { c["slot_" + k] = i; c["grp_" + k] = gi; });
      });
      sk.cols = [Math.ceil(groups[0].length / rows), Math.ceil(groups[1].length / rows)];
    });

    var monthEls = [], prev = -1;
    for (var w = 0; w < WEEKS; w++) {
      var mon = new Date(START + w * 7 * 86400000).getUTCMonth();
      if (mon !== prev) {
        var m = document.createElement("div");
        m.className = "lc-cal-month"; m.textContent = MONTHS[mon];
        board.appendChild(m); monthEls.push({ el: m, w: w }); prev = mon;
      }
    }
    var dayEls = [];
    for (var yy = 0; yy < years; yy++) {
      DAY_NAMES.forEach(function (name, i) {
        var el = document.createElement("div");
        el.className = "lc-cal-day"; el.textContent = name;
        board.appendChild(el); dayEls.push({ el: el, y: yy, d: i });
      });
    }
    var yearEls = [];
    if (years > 1) {
      for (yy = 0; yy < years; yy++) {
        var ye = document.createElement("div");
        ye.className = "lc-cal-year"; ye.textContent = "rok " + (yy + 1);
        board.appendChild(ye); yearEls.push(ye);
      }
    }
    function groupLabel(text, cls) {
      var el = document.createElement("div");
      el.className = "lc-cal-group " + cls; el.textContent = text;
      board.appendChild(el); return el;
    }
    var hotLabel = groupLabel("", "is-hot");
    var coldLabel = groupLabel("", "is-cold");
    cells.forEach(function (c) {
      var el = document.createElement("div");
      el.className = "lc-cal-cell" + (c.hot ? " is-hot" : "") + (c.inc ? " is-inc" : "");
      el.title = (years > 1 ? "Rok " + (c.y + 1) + ", tydzień " : "Tydzień ") + (c.w + 1) + ", " + DAY_NAMES[c.d] + ", zmiana " +
        (c.s ? "nocna" : "dzienna") + (c.hot ? ", przegrzanie" : "") + (c.inc ? ", incydent" : "");
      board.appendChild(el); c.el = el;
    });

    var geo = {};
    function layout() {
      var W = board.clientWidth;
      if (!W) return;
      var labelW = (W < 420 ? 28 : 36) + (years > 1 ? 10 : 0);
      var pitch = Math.max(4, Math.floor((W - labelW) / WEEKS));
      var big = pitch >= 10;
      geo = {
        labelW: labelW, pitch: pitch, size: pitch - (big ? 2 : 1),
        dayGap: big ? 6 : 3, top: big ? 20 : 16
      };
      geo.gridH = DAYS * 2 * pitch + (DAYS - 1) * geo.dayGap;
      geo.bandGap = years > 1 ? (big ? 22 : 16) : 0;
      geo.band = function (y) { return geo.top + y * (geo.gridH + geo.bandGap); };
      // Po sortowaniu: dwa bloki po 10 wierszy na rok, wyrównane do dołu kalendarza.
      geo.bottom = geo.band(years - 1) + geo.gridH - pitch;
      // Druga grupa dosunięta do prawej krawędzi, żeby zmieściła się także przy 983 zmianach.
      geo.cols = Math.floor((W - (geo.size)) / pitch) + 1;
      geo.right = W - geo.size;
      board.style.height = (geo.bottom + pitch + 4) + "px";
      var font = big ? 11 : 9;
      monthEls.forEach(function (m) {
        m.el.style.left = (labelW + m.w * pitch) + "px"; m.el.style.fontSize = font + "px";
      });
      dayEls.forEach(function (o) {
        o.el.style.top = (geo.band(o.y) + o.d * (2 * pitch + geo.dayGap)) + "px";
        o.el.style.height = (2 * pitch) + "px"; o.el.style.fontSize = font + "px";
      });
      yearEls.forEach(function (el, y) {
        el.style.left = "0px"; el.style.fontSize = font + "px";
        el.style.top = (y === 0 ? 0 : geo.band(y) - font - 6) + "px";
      });
      var labTop = geo.bottom - rows * pitch - (big ? 4 : 2);
      hotLabel.style.left = "0px"; hotLabel.style.top = labTop + "px";
      coldLabel.style.left = (geo.right + geo.size) + "px";
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
        if (mode) {
          var sk = SORTS[mode], g = c["grp_" + mode], slot = c["slot_" + mode], cols = sk.cols[g];
          var x0 = g === 0 ? 0 : geo.right - (sk.cols[1] - 1) * geo.pitch;
          x = x0 + (slot % cols) * geo.pitch;
          y = geo.bottom - Math.floor(slot / cols) * geo.pitch;
          delay = instant || REDUCE ? 0 : c.w * 12 + c.y * 120;
        } else {
          x = geo.labelW + c.w * geo.pitch;
          y = geo.band(c.y) + c.d * (2 * geo.pitch + geo.dayGap) + c.s * geo.pitch;
        }
        c.el.style.transform = "translate(" + x + "px," + y + "px)";
        c.el.style.transitionDelay = delay + "ms";
      });
      board.classList.toggle("is-sorted", !!mode);
      if (instant) { void board.offsetWidth; board.classList.remove("is-instant"); }
    }

    function ratio(k, n) {
      return k + " / " + n + " = " + (k / n).toFixed(3).replace(/0+$/, "").replace(/\.$/, "");
    }
    function texts() {
      var two = true;
      if (mode === "hot") {
        out.label.textContent = "Incydent przy przegrzaniu";
        out.value.textContent = ratio(incHot, nHot);
        out.label2.textContent = "Incydent bez przegrzania";
        out.value2.textContent = ratio(incCold, nCold);
        status.textContent = "Ten sam rejestr, posortowany według przegrzania.";
      } else if (mode === "inc") {
        out.label.textContent = "Przegrzanie przy incydencie";
        out.value.textContent = ratio(incHot, incAll);
        out.label2.textContent = "Przegrzanie bez incydentu";
        out.value2.textContent = ratio(nHot - incHot, nNoInc);
        status.textContent = "Ten sam rejestr, posortowany według incydentu.";
      } else {
        out.label.textContent = btns.length > 1 ? "Incydent" : "Wszystkie zmiany";
        out.value.textContent = ratio(incAll, nAll);
        two = btns.length > 1;
        if (two) {
          out.label2.textContent = "Przegrzanie";
          out.value2.textContent = ratio(nHot, nAll);
        }
        status.textContent = "Bursztyn: przegrzanie. Kropka: incydent.";
      }
      if (mode) {
        hotLabel.textContent = SORTS[mode].labels[0];
        hotLabel.classList.toggle("is-acc", mode === "inc");
        coldLabel.textContent = SORTS[mode].labels[1];
      }
      if (second) second.hidden = !two;
      btns.forEach(function (b) {
        var on = b.getAttribute("data-cal-sort") === mode;
        b.setAttribute("aria-pressed", String(on));
        b.querySelector("span").textContent = on ? "Wróć do kalendarza" : b.dataset.label;
      });
    }

    btns.forEach(function (b) {
      b.addEventListener("click", function () {
        var m = b.getAttribute("data-cal-sort");
        mode = mode === m ? null : m;
        render(false); texts();
      });
    });
    // Rozdział może być ukryty przy starcie: układ liczymy, gdy panel dostanie szerokość.
    if (window.ResizeObserver) new ResizeObserver(function () { layout(); }).observe(board);
    else window.addEventListener("resize", layout);
    layout(); texts();
  }


  // --- Scena „Odprawa” --------------------------------------------------------

  var OD_COLS = 40, OD_PITCH = 13, OD_X = 80, OD_Y = 128;
  var OD_PEOPLE = [
    { x: 270, name: "dyrektor", tie: true },
    { x: 440, name: "kierownik zmiany", helmet: true },
    { x: 610, name: "specjalista BHP", vest: true }
  ];

  function initOdprawa(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var frames = cfg.frames || [];
    var cells = makeRegister(cfg);
    var stage = root.querySelector(".lc-od-stage");
    var caption = root.querySelector(".lc-od-caption");
    var nameEl = root.querySelector('[data-od-out="name"]');
    var stepEl = root.querySelector('[data-od-out="step"]');
    var prevBtn = root.querySelector('[data-od="prev"]'), nextBtn = root.querySelector('[data-od="next"]');
    var rows = Math.ceil(cells.length / OD_COLS);
    var W = 720, H = OD_Y + rows * OD_PITCH + 16, k = 0;
    var s = svg("svg", { viewBox: "0 0 " + W + " " + H, class: "lc-od-svg", role: "img",
      "aria-label": "Rejestr " + cells.length + " zmian na tablicy i trzy osoby pytające o incydent." }, stage);

    var people = OD_PEOPLE.map(function (p, i) {
      var g = svg("g", { class: "lc-od-person", transform: "translate(" + p.x + ",0)" }, s);
      if (p.helmet) svg("path", { d: "M-15 14 Q0 -2 15 14 Z", class: "lc-od-helmet" }, g);
      svg("circle", { cx: 0, cy: 22, r: 12, class: "lc-od-face" }, g);
      svg("path", { d: "M-22 70 Q-22 40 0 40 Q22 40 22 70 Z", class: "lc-od-body" + (p.vest ? " is-vest" : "") }, g);
      if (p.tie) svg("path", { d: "M0 41 L-4 48 L0 62 L4 48 Z", class: "lc-od-tie" }, g);
      svg("text", { x: 0, y: 88, "text-anchor": "middle", class: "lc-od-name" }, g, p.name);
      var bubble = svg("g", { class: "lc-od-bubble" }, g);
      svg("rect", { x: 26, y: 2, width: 30, height: 26, rx: 9 }, bubble);
      svg("path", { d: "M30 26 L24 34 L38 27 Z" }, bubble);
      svg("text", { x: 41, y: 21, "text-anchor": "middle" }, bubble, "?");
      return g;
    });

    svg("rect", { x: OD_X - 10, y: OD_Y - 10, width: OD_COLS * OD_PITCH + 18, height: rows * OD_PITCH + 18,
      rx: 6, class: "lc-od-board" }, s);
    cells.forEach(function (c, i) {
      var x = OD_X + (i % OD_COLS) * OD_PITCH, y = OD_Y + Math.floor(i / OD_COLS) * OD_PITCH;
      var g = svg("g", { class: "lc-od-cell" + (c.hot ? " is-hot" : "") + (c.inc ? " is-inc" : "") }, s);
      svg("rect", { x: x, y: y, width: OD_PITCH - 2, height: OD_PITCH - 2, rx: 1.5 }, g);
      if (c.inc) svg("circle", { cx: x + (OD_PITCH - 2) / 2, cy: y + (OD_PITCH - 2) / 2, r: 3 }, g);
      c.g = g;
    });
    var frac = svg("text", { x: OD_X - 10, y: 58, class: "lc-od-frac" }, s);

    function show(i) {
      k = Math.max(0, Math.min(frames.length - 1, i));
      var f = frames[k];
      cells.forEach(function (c) {
        var inGroup = f.group === "all" || (f.group === "hot" && c.hot) || (f.group === "inc" && c.inc);
        c.g.classList.toggle("is-dim", !!f.group && !inGroup);
        c.g.classList.toggle("is-den", !!f.group && f.group !== "all" && inGroup);
      });
      s.classList.toggle("is-all", f.group === "all");
      people.forEach(function (g, j) {
        g.classList.toggle("is-on", f.person === j);
        g.classList.toggle("is-off", f.person !== undefined && f.person !== j);
      });
      frac.textContent = "";
      if (f.num !== undefined) {
        svg("tspan", { class: "is-num" }, frac, String(f.num));
        svg("tspan", {}, frac, " / " + f.den);
      }
      caption.textContent = f.text;
      if (nameEl) nameEl.textContent = f.name;
      if (stepEl) stepEl.textContent = "Kadr " + (k + 1) + " z " + frames.length;
      prevBtn.disabled = k === 0;
      nextBtn.disabled = k === frames.length - 1;
    }
    prevBtn.addEventListener("click", function () { show(k - 1); });
    nextBtn.addEventListener("click", function () { show(k + 1); });
    show(0);
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
    document.querySelectorAll(".lc-od:not([data-ready])").forEach(initOdprawa);
  }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
