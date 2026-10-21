// Krokowe drzewo „Wzór Bayesa na drzewie” (rozdział „Wzór Bayesa”, sekcja „Od drzewa do Bayesa”).
//
// Drzewo (svg.a3-tree-svg) rysuje serwer i podmienia je po każdej zmianie suwaków
// tablicy 2×2. Elementy drzewa mają klucze data-k, a parametry i liczebności stoją
// w atrybutach data-* elementu svg. Ten skrypt trzyma bieżący krok, podświetla
// elementy kroku i dopisuje linijki wzoru; po podmianie drzewa stosuje krok ponownie.
// Przyciski [data-tr="prev|next"].
(function () {
  var ALARM = ["n-hit", "n-fa", "e-hit", "e-fa", "b-alarm"];
  var HIT_PATH = ["n-root", "e-a", "p-a", "n-a", "e-hit", "p-hit", "n-hit"];
  var FA_PATH = ["e-n", "p-n", "n-n", "e-fa", "p-fa", "n-fa"];
  var QUIET = ["n-root", "e-a", "p-a", "n-a", "e-miss", "p-miss", "n-miss",
    "e-n", "p-n", "n-n", "e-tn", "p-tn", "n-tn", "b-quiet"];

  // Bez zbędnych zer na końcu: 0.010 → 0.01.
  function f(x, d) { return String(Number(x.toFixed(d))); }
  function n(x) { return String(x).replace(/\B(?=(\d{3})+(?!\d))/g, " "); }

  // Każdy krok: nazwa, klucze podświetlone (null = całe drzewo jak w wersji
  // statycznej), styl ciszy oraz funkcja zwracająca linijki wzoru [tekst, opis].
  var STEPS = [
    { name: "Drzewo", on: null, lines: function () {
      return [["Do alarmu prowadzą dwie drogi: przez awarię i bez awarii.", ""]];
    } },
    { name: "Pytanie dyżurnego", on: ALARM, lines: function (v) {
      return [["P(A | 🚨) = P(A ∩ 🚨) / P(🚨)", "definicja warunku: licznik to jedna droga, mianownik to wszystkie alarmy"]];
    } },
    { name: "Licznik", on: HIT_PATH, lines: function (v) {
      return [["P(A | 🚨) = P(A ∩ 🚨) / P(🚨)", "definicja warunku"],
        ["P(A ∩ 🚨) = P(A) · P(🚨 | A) = " + f(v.p, 3) + " · " + f(v.s, 2) + " = " + f(v.p * v.s, 4), "reguła mnożenia: idziemy po drodze przez awarię"]];
    } },
    { name: "Mianownik", on: HIT_PATH.concat(FA_PATH, ["b-alarm"]), lines: function (v) {
      return [["P(A | 🚨) = P(A ∩ 🚨) / P(🚨)", "definicja warunku"],
        ["P(A ∩ 🚨) = " + f(v.p * v.s, 4), "reguła mnożenia"],
        ["P(🚨) = " + f(v.p * v.s, 4) + " + " + f(1 - v.p, 3) + " · " + f(v.f, 2) + " = " + f(v.p * v.s + (1 - v.p) * v.f, 4), "prawdopodobieństwo całkowite: suma obu dróg do alarmu"]];
    } },
    { name: "Wynik", on: HIT_PATH.concat(FA_PATH, ["b-alarm"]), lines: function (v) {
      var den = v.p * v.s + (1 - v.p) * v.f;
      return [["P(A ∩ 🚨) = " + f(v.p * v.s, 4) + ",  P(🚨) = " + f(den, 4), "licznik i mianownik"],
        ["P(A | 🚨) = " + f(v.p * v.s, 4) + " / " + f(den, 4) + " ≈ " + f(v.p * v.s / den, 3), "wzór Bayesa (3.4): " + n(v.tp) + " z " + n(v.tp + v.fp) + " alarmów jest prawdziwych"]];
    } },
    { name: "A cisza?", on: QUIET, quiet: true, lines: function (v) {
      var den = v.p * (1 - v.s) + (1 - v.p) * (1 - v.f);
      return [["P(A | 🔕) = " + f(v.p, 3) + " · " + f(1 - v.s, 2) + " / " + f(den, 4) + " ≈ " + f(v.p * (1 - v.s) / den, 5), "ta sama konstrukcja dla ciszy: " + n(v.fn) + " przeoczonych awarii na " + n(v.fn + v.tn) + " cichych zmian"],
        ["P(Aᶜ | 🔕) ≈ " + f((1 - v.p) * (1 - v.f) / den, 4), "wzór (3.5): cisza jest bardzo wiarygodna, alarm znacznie mniej"]];
    } }
  ];

  function init(root) {
    if (root.dataset.ready) return;
    root.dataset.ready = "1";
    var lines = root.querySelector(".lc-tr-lines");
    var nameEl = root.querySelector('[data-tr-out="name"]'), stepEl = root.querySelector('[data-tr-out="step"]');
    var prevBtn = root.querySelector('[data-tr="prev"]'), nextBtn = root.querySelector('[data-tr="next"]');
    var current = 0;

    function values(svg) {
      var d = svg.dataset;
      return { p: +d.prev, s: +d.sens, f: +d.fpr, tp: +d.tp, fn: +d.fn, fp: +d.fp, tn: +d.tn };
    }

    function apply() {
      var svg = root.querySelector("svg.a3-tree-svg");
      var S = STEPS[current];
      if (nameEl) nameEl.textContent = S.name;
      if (stepEl) stepEl.textContent = "Krok " + (current + 1) + " z " + STEPS.length;
      prevBtn.disabled = current === 0;
      nextBtn.disabled = current === STEPS.length - 1;
      if (!svg) return;
      svg.classList.toggle("is-stepped", !!S.on);
      svg.classList.toggle("is-quiet", !!S.quiet);
      svg.querySelectorAll("[data-k]").forEach(function (e) {
        e.classList.toggle("is-lit", !!S.on && S.on.indexOf(e.dataset.k) >= 0);
      });
      lines.innerHTML = "";
      var L = S.lines(values(svg));
      L.forEach(function (pair, i) {
        var row = document.createElement("div");
        row.className = "lc-tr-line" + (i === L.length - 1 ? " is-new" : "");
        var t = document.createElement("span");
        t.className = "lc-tr-tex";
        t.textContent = pair[0];
        row.appendChild(t);
        if (pair[1]) {
          var note = document.createElement("span");
          note.className = "lc-tr-note";
          note.textContent = pair[1];
          row.appendChild(note);
        }
        lines.appendChild(row);
      });
    }

    prevBtn.addEventListener("click", function () { if (current > 0) { current--; apply(); } });
    nextBtn.addEventListener("click", function () { if (current < STEPS.length - 1) { current++; apply(); } });
    // Serwer podmienia drzewo po zmianie suwaków: wtedy stosujemy krok jeszcze raz.
    var holder = root.querySelector("#a3_tree") || root;
    new MutationObserver(function () { requestAnimationFrame(apply); })
      .observe(holder, { childList: true, subtree: true });
    apply();
  }

  function scan() { document.querySelectorAll(".lc-tr:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();
})();
