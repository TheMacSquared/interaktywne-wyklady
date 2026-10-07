// Ćwiczenie 3: losowanie jednej palety z dostawy. Plac to przestrzeń Ω (24 palety),
// zdarzenie A to palety z uszkodzonym zabezpieczeniem (ich liczbę zmieniają − i +),
// reszta to dopełnienie Aᶜ. Przycisk losuje ω i mówi, czy A zaszło; powtórzenia
// pokazują częstość obok |A|/|Ω|. Tryb „na oko” wybiera częściej palety przy bramie,
// więc częstość odjeżdża od |A|/|Ω|.
// Kontener: .lc-om[data-config], przyciski [data-om], [data-om-mode], [data-om-a],
// odczyty [data-om-out].
// Całość działa w przeglądarce, bez serwera.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var W = 640, H = 332;
  var COLS = 6, ROWS = 4, CW = 80, RH = 62, X0 = 140, Y0 = 64, PW = 56, PH = 40;
  var GATE = { x: 92, y1: 132, y2: 208 };
  var REDUCE = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  function svg(name, attrs, parent, text) {
    var e = document.createElementNS(NS, name);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }

  // Numeracja kolumnami: 1–4 w pierwszej kolumnie (przy bramie), 5–8 w drugiej itd.
  function cell(id) {
    var i = id - 1;
    return { c: Math.floor(i / ROWS), r: i % ROWS };
  }
  function center(id) {
    var p = cell(id);
    return { x: X0 + p.c * CW + PW / 2, y: Y0 + p.r * RH + PH / 2 };
  }

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var cfg = JSON.parse(root.dataset.config || "{}");
    var N = cfg.n || COLS * ROWS, A = cfg.a === undefined ? 6 : cfg.a;
    var stage = root.querySelector(".lc-om-stage");
    var status = root.querySelector(".lc-om-status");
    var goBtn = root.querySelector('[data-om="go"]');
    var mode = "rand", busy = false;
    var st;

    // Wybór „na oko”: im dalej od bramy, tym rzadziej.
    var eyeW = [];
    for (var id = 1; id <= N; id++) eyeW.push(Math.exp(-0.6 * cell(id).c));
    var eyeSum = eyeW.reduce(function (a, b) { return a + b; }, 0);

    function pick() {
      if (mode === "rand") return 1 + Math.floor(Math.random() * N);
      var u = Math.random() * eyeSum;
      for (var j = 0; j < N; j++) { u -= eyeW[j]; if (u <= 0) return j + 1; }
      return N;
    }

    function reset() {
      st = { n: 0, k: 0, counts: new Array(N + 1).fill(0), last: null };
      status.classList.remove("is-hit");
      if (A === 0) {
        status.textContent = "A = ∅: żadna paleta nie jest uszkodzona, więc A jest zdarzeniem niemożliwym i P(A) = 0.";
      } else if (A === N) {
        status.textContent = "A = Ω: wszystkie palety są uszkodzone, więc A jest zdarzeniem pewnym i P(A) = 1.";
      } else {
        status.textContent = mode === "rand"
          ? "Każda paleta ma w generatorze jeden numer i tę samą szansę 1/" + N + "."
          : "Inspektor wybiera paletę na oko: te przy bramie częściej niż te w głębi placu.";
      }
    }

    var s = svg("svg", { viewBox: "0 0 " + W + " " + H, class: "lc-om-svg", role: "img",
      "aria-label": "Plac z 24 paletami: przestrzeń Ω, zdarzenie A i wylosowana paleta ω" }, stage);

    function aOutline() {
      // obrys zbioru A: cała pierwsza kolumna i tyle palet z drugiej, ile potrzeba
      var pad = 9, full = Math.floor(A / ROWS), rest = A % ROWS;
      var xL = X0 - pad, yT = Y0 - pad, yB = Y0 + (ROWS - 1) * RH + PH + pad;
      var xFull = X0 + (full - 1) * CW + PW + pad;
      if (full === 0) {
        var xR = X0 + PW + pad, yR = Y0 + (rest - 1) * RH + PH + pad;
        return "M" + xL + " " + yT + " H" + xR + " V" + yR + " H" + xL + " Z";
      }
      if (rest === 0) {
        return "M" + xL + " " + yT + " H" + xFull + " V" + yB + " H" + xL + " Z";
      }
      var xRest = X0 + full * CW + PW + pad, yRest = Y0 + (rest - 1) * RH + PH + pad;
      return "M" + xL + " " + yT + " H" + xRest + " V" + yRest + " H" + xFull + " V" + yB + " H" + xL + " Z";
    }

    function drawPallet(g, id, heat) {
      var p = cell(id), x = X0 + p.c * CW, y = Y0 + p.r * RH, bad = id <= A;
      var q = svg("g", { transform: "translate(" + x + "," + y + ")" }, g);
      if (heat > 0) svg("rect", { x: -6, y: -6, width: PW + 12, height: PH + 12, rx: 6,
        class: "lc-om-heat", "fill-opacity": (0.08 + 0.42 * heat).toFixed(3) }, q);
      svg("rect", { x: 6, y: 0, width: PW - 12, height: PH - 12, rx: 2, class: "lc-om-box" }, q);
      svg("rect", { x: 0, y: PH - 12, width: PW, height: 5, class: "lc-om-wood" }, q);
      svg("rect", { x: 0, y: PH - 4, width: PW, height: 4, class: "lc-om-wood" }, q);
      [4, PW / 2 - 3, PW - 10].forEach(function (bx) {
        svg("rect", { x: bx, y: PH - 7, width: 6, height: 3, class: "lc-om-wood" }, q);
      });
      // taśma zabezpieczająca: cała albo zerwana
      if (bad) {
        svg("line", { x1: 6, y1: 14, x2: PW / 2 - 6, y2: 14, class: "lc-om-strap is-bad" }, q);
        svg("line", { x1: PW / 2 + 4, y1: 17, x2: PW - 6, y2: 13, class: "lc-om-strap is-bad" }, q);
      } else {
        svg("line", { x1: 6, y1: 14, x2: PW - 6, y2: 14, class: "lc-om-strap" }, q);
      }
      svg("text", { x: PW / 2, y: PH - 18, "text-anchor": "middle", class: "lc-om-num" }, q, String(id));
    }

    function draw(mark) {
      s.textContent = "";
      // Ω: ogrodzony plac z bramą po lewej
      svg("rect", { x: GATE.x, y: 22, width: W - GATE.x - 14, height: H - 44, rx: 10, class: "lc-om-omega" }, s);
      svg("rect", { x: GATE.x - 3, y: GATE.y1, width: 6, height: GATE.y2 - GATE.y1, class: "lc-om-gap" }, s);
      svg("text", { x: GATE.x - 10, y: (GATE.y1 + GATE.y2) / 2 + 4, "text-anchor": "end", class: "lc-om-gate" }, s, "brama");
      svg("text", { x: W - 92, y: 48, "text-anchor": "end", class: "lc-om-set" }, s, "Ω");
      svg("text", { x: W - 28, y: 46, "text-anchor": "end", class: "lc-om-setsub" }, s, "|Ω| = " + N);

      var maxC = Math.max.apply(null, st.counts);
      var g = svg("g", {}, s);
      for (var id = 1; id <= N; id++) drawPallet(g, id, maxC > 0 ? st.counts[id] / maxC : 0);

      // A: obrys palet z uszkodzonym zabezpieczeniem; Aᶜ: pozostałe palety
      if (A > 0) svg("path", { d: aOutline(), class: "lc-om-a" }, s);
      svg("text", { x: X0 - 9, y: Y0 - 16, class: "lc-om-set is-a" }, s, A === 0 ? "A = ∅" : "A");
      svg("text", { x: X0 + (A === 0 ? 50 : 10), y: Y0 - 16, class: "lc-om-setsub is-a" }, s, "|A| = " + A);
      svg("text", { x: W - 92, y: H - 30, "text-anchor": "end", class: "lc-om-set is-c" }, s, "Aᶜ");
      svg("text", { x: W - 28, y: H - 32, "text-anchor": "end", class: "lc-om-setsub is-c" }, s, "|Aᶜ| = " + (N - A));

      if (mark) {
        var c = center(mark.id);
        svg("rect", { x: c.x - PW / 2 - 7, y: c.y - PH / 2 - 7, width: PW + 14, height: PH + 14, rx: 7,
          class: "lc-om-mark" + (mark.final ? " is-final" : "") }, s);
        if (mark.final) svg("text", { x: c.x, y: c.y + PH / 2 + 20, "text-anchor": "middle", class: "lc-om-omega-l" }, s, "ω");
      }
    }

    function out(name, v) {
      var e = root.querySelector('[data-om-out="' + name + '"]');
      if (e) e.textContent = v;
    }
    function readouts() {
      out("a", String(A));
      out("n", String(st.n));
      out("k", String(st.k));
      out("f", st.n ? (st.k / st.n).toFixed(3) : "–");
      out("p", (A / N).toFixed(3).replace(/\.?0+$/, "") || "0");
      out("pc", (1 - A / N).toFixed(3).replace(/\.?0+$/, "") || "0");
      root.querySelectorAll("[data-om-a]").forEach(function (b) {
        var d = Number(b.getAttribute("data-om-a"));
        b.disabled = busy || A + d < 0 || A + d > N;
      });
    }
    function setBusy(b) {
      busy = b;
      root.querySelectorAll("[data-om]").forEach(function (x) { x.disabled = b; });
    }
    function describe(id) {
      var inA = id <= A;
      status.textContent = "ω = " + id + ". " + (inA
        ? "Paleta należy do A (ω ∈ A), więc zdarzenie A zaszło."
        : "Paleta nie należy do A (ω ∉ A), więc zdarzenie A nie zaszło.");
      status.classList.toggle("is-hit", inA);
    }
    function record(id) {
      st.n += 1; st.counts[id] += 1; st.last = id;
      if (id <= A) st.k += 1;
    }

    function drawOne() {
      var final = pick();
      var hops = REDUCE ? 0 : 9, i = 0;
      setBusy(true);
      (function hop() {
        if (i < hops) {
          i++;
          draw({ id: 1 + Math.floor(Math.random() * N), final: false });
          setTimeout(hop, 60 + i * 8);
          return;
        }
        record(final);
        draw({ id: final, final: true });
        describe(final);
        setBusy(false);
        readouts();
      })();
    }

    function drawMany(m) {
      var id;
      for (var i = 0; i < m; i++) { id = pick(); record(id); }
      draw({ id: id, final: true });
      readouts();
      status.textContent = "Ostatnia: ω = " + id + ". A zaszło w " + st.k + " z " + st.n +
        " wyborów, czyli w " + (st.k / st.n).toFixed(3) + " przypadków; |A|/|Ω| = " + A + "/" + N + ".";
      status.classList.remove("is-hit");
    }

    root.addEventListener("click", function (e) {
      var ab = e.target.closest("[data-om-a]");
      if (ab && !busy) {
        A = Math.max(0, Math.min(N, A + Number(ab.getAttribute("data-om-a"))));
        reset(); draw(null); readouts();
        return;
      }
      var m = e.target.closest("[data-om-mode]");
      if (m && !busy) {
        mode = m.getAttribute("data-om-mode");
        root.querySelectorAll("[data-om-mode]").forEach(function (b) {
          b.setAttribute("aria-pressed", b === m ? "true" : "false");
        });
        if (goBtn) goBtn.querySelector("span").textContent = mode === "rand" ? "Losuj paletę" : "Wybierz paletę";
        reset(); draw(null); readouts();
        return;
      }
      var b = e.target.closest("[data-om]");
      if (!b || busy) return;
      var a = b.getAttribute("data-om");
      if (a === "go") drawOne(); else drawMany(Number(a));
    });

    reset(); draw(null); readouts();
  }

  function scan() { document.querySelectorAll(".lc-om:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
