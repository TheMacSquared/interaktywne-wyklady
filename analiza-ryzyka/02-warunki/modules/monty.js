// Gra Monty Halla „Idź na całość” w przeglądarce, bez serwera R (dawniej
// monty_server.R). Kontener .w2-monty: sloty [data-mt="controls|doors|feedback"],
// panel symulacji [data-mt="sim"] z ryciną lcSF „w2_monty” (odsetek wygranych).
// Stan gry i symulacji trzyma kontener (root.__monty); rycina czyta go w prepare.
// Bramki rysowane tokenami motywu, więc działają też w ciemnym trybie.
// Funkcje gry są w window.lcW2Monty (testy: tests/js/w2_monty.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;

  function pct(x) { return (100 * x).toFixed(1) + "%"; }  // scales::percent(x, accuracy = 0.1)
  function big(n) { return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, " "); }
  function int3(rnd) { return 1 + Math.floor(rnd() * 3); }

  // --- Logika gry (jak warunki_monty_server w R) ---------------------------------

  function newGame(rnd) { return { prize: int3(rnd), chosen: null, opened: null, fin: null, strategy: null }; }
  function choose(g, door, rnd) {
    var zonks = [1, 2, 3].filter(function (k) { return k !== door && k !== g.prize; });
    g.chosen = door; g.opened = zonks[Math.floor(rnd() * zonks.length)]; g.fin = null; g.strategy = null;
  }
  function stay(g) { if (g.opened == null) return; g.fin = g.chosen; g.strategy = "pozostanie"; }
  function change(g) { if (g.opened == null) return; g.fin = 6 - g.chosen - g.opened; g.strategy = "zmiana"; }
  // Dograne partie: nagroda i pierwszy wybór losowe; „zostaję” wygrywa, gdy się pokrywają.
  function addGames(sim, n, rnd) {
    for (var i = 0; i < n; i++) { if (int3(rnd) === int3(rnd)) sim.stay++; else sim.sw++; }
    sim.n += n;
  }

  // --- Rysunki bramek (.monty_svg_* w block.R) -------------------------------------

  var WHITE = "var(--upwr-surface)";
  function doorClosed(door, highlight) {
    return '<svg viewBox="0 0 120 150" width="100%" height="140" role="img" aria-label="Bramka ' + door + ': zamknięta">' +
      '<rect x="8" y="6" width="104" height="138" rx="10" fill="' + (highlight ? "var(--upwr-accent)" : "var(--upwr-secondary)") + '"/>' +
      '<rect x="18" y="16" width="84" height="118" rx="6" fill="none" stroke="' + WHITE + '" stroke-opacity="0.35" stroke-width="3"/>' +
      '<circle cx="96" cy="78" r="5" fill="var(--upwr-cat-bursztyn)"/>' +
      '<text x="58" y="90" text-anchor="middle" font-size="46" font-weight="bold" fill="' + WHITE + '">' + door + "</text></svg>";
  }
  function doorway(content, label) {
    return '<svg viewBox="0 0 120 150" width="100%" height="140" role="img" aria-label="' + label + '">' +
      '<rect x="8" y="6" width="104" height="138" rx="10" fill="var(--upwr-secondary)"/>' +
      '<rect x="16" y="14" width="88" height="122" rx="6" fill="var(--upwr-panel)"/>' + content + "</svg>";
  }
  function car() {
    var b = "var(--upwr-cat-bursztyn)", w = "var(--upwr-secondary)";
    return '<path d="M36 86 Q41 64 58 64 L72 64 Q86 64 91 86 Z" fill="' + b + '"/>' +
      '<rect x="24" y="84" width="72" height="24" rx="9" fill="' + b + '"/>' +
      '<rect x="47" y="70" width="21" height="13" rx="3" fill="' + WHITE + '" fill-opacity="0.85"/>' +
      '<circle cx="41" cy="110" r="9" fill="' + w + '"/><circle cx="41" cy="110" r="3.5" fill="' + WHITE + '"/>' +
      '<circle cx="79" cy="110" r="9" fill="' + w + '"/><circle cx="79" cy="110" r="3.5" fill="' + WHITE + '"/>' +
      '<circle cx="93" cy="92" r="2.5" fill="' + WHITE + '"/>';
  }
  function cat() {
    var c = "var(--upwr-reference)", l = "var(--upwr-secondary)";
    return '<path d="M78 112 Q98 110 96 90 Q95 80 88 84" stroke="' + c + '" stroke-width="6" fill="none" stroke-linecap="round"/>' +
      '<ellipse cx="60" cy="104" rx="22" ry="20" fill="' + c + '"/>' +
      '<rect x="46" y="110" width="9" height="16" rx="4" fill="' + c + '"/><rect x="63" y="110" width="9" height="16" rx="4" fill="' + c + '"/>' +
      '<path d="M42 66 L46 46 L58 60 Z" fill="' + c + '"/><path d="M78 66 L74 46 L62 60 Z" fill="' + c + '"/>' +
      '<circle cx="60" cy="72" r="18" fill="' + c + '"/>' +
      '<ellipse cx="53" cy="69" rx="2.4" ry="3.4" fill="' + l + '"/><ellipse cx="67" cy="69" rx="2.4" ry="3.4" fill="' + l + '"/>' +
      '<path d="M57.5 76 L62.5 76 L60 79 Z" fill="' + l + '"/>' +
      ["M50 77 L34 74", "M50 80 L35 83", "M70 77 L86 74", "M70 80 L85 83"].map(function (d) {
        return '<path d="' + d + '" stroke="' + l + '" stroke-width="1.6" stroke-linecap="round"/>';
      }).join("");
  }
  function doorCard(door, state, title, caption, border) {
    var svg = state === "closed" ? doorClosed(door, false) : state === "chosen" ? doorClosed(door, true) :
      state === "zonk" ? doorway(cat(), "Bramka " + door + ": Zonk") : doorway(car(), "Bramka " + door + ": nagroda");
    return '<div style="text-align:center; padding:0.6rem 0.5rem; border:2px solid ' + border +
      '; border-radius:12px; background:var(--upwr-surface); height:100%;">' + svg +
      '<div style="font-weight:700; margin-top:0.35rem;">Bramka ' + door + " · " + title + "</div>" +
      '<div style="font-size:0.85rem; color:var(--upwr-ink-soft);">' + caption + "</div></div>";
  }
  function doorsHtml(g) {
    var acc = "var(--upwr-accent)", ref = "var(--upwr-reference)";
    return '<div class="lc-stat-grid" style="--lc-stat-cols:3;">' + [1, 2, 3].map(function (d) {
      if (g.opened != null && d === g.opened) return doorCard(d, "zonk", "Zonk", "Prowadzący odsłonił tę bramkę", ref);
      if (g.fin != null) return doorCard(d, d === g.prize ? "car" : "zonk", d === g.prize ? "Nagroda" : "Zonk",
        d === g.fin ? "Twój ostateczny wybór" : "Niewybrana bramka", d === g.fin ? acc : ref);
      if (g.chosen != null && d === g.chosen) return doorCard(d, "chosen", "Twój wybór", "Bramka pozostaje zamknięta", acc);
      return doorCard(d, "closed", "Zamknięta", "Nagroda albo Zonk", "var(--upwr-secondary)");
    }).join("") + "</div>";
  }

  // --- Sterowanie i komunikaty -----------------------------------------------------

  var RESET = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true" focusable="false">' +
    '<path d="M3 12a9 9 0 1 0 3-6.7"></path><path d="M3 4v5h5"></path></svg>';
  function button(act, label) { return '<button type="button" class="lc-action is-solid" data-mt-act="' + act + '"><span>' + label + "</span></button>"; }
  function controlsHtml(g) {
    function head(step, title) { return '<div class="lc-eyebrow">Krok ' + step + " z 3</div><h4>" + title + "</h4>"; }
    if (g.chosen == null) {
      return head(1, "Wybierz jedną bramkę") + "<p>Za jedną jest nagroda, za dwiema pozostałymi — Zonk.</p>" +
        '<div class="lc-toolbar">' + button("door1", "Bramka 1") + button("door2", "Bramka 2") + button("door3", "Bramka 3") + "</div>";
    }
    if (g.fin == null) {
      return head(2, "Wybrałeś bramkę " + g.chosen) +
        "<p>Prowadzący wiedział, gdzie jest nagroda, i odsłonił Zonka za bramką " + g.opened + ".</p><p>Co robisz z nową informacją?</p>" +
        '<div class="lc-toolbar">' + button("stay", "Zostaję przy wyborze") + button("switch", "Zmieniam bramkę") + "</div>";
    }
    return head(3, "Sprawdź wynik i zagraj ponownie") +
      '<button type="button" class="lc-action is-ghost is-icon" data-mt-act="new" aria-label="Nowa gra" title="Nowa gra">' + RESET + "</button>";
  }
  function feedbackHtml(g) {
    if (g.opened == null) return "";
    if (g.fin == null) {
      return '<div class="lc-status" role="status" aria-live="polite"><strong>Nowa informacja:</strong> bramka ' + g.opened +
        " na pewno nie zawiera nagrody. Zostajesz czy zmieniasz?</div>";
    }
    var won = g.fin === g.prize;
    return '<div class="lc-status" role="status" aria-live="polite"><span class="lc-status-' + (won ? "ok" : "warning") + '"><strong>' +
      (won ? "Nagroda!" : "Zonk.") + "</strong></span> Strategia: " + g.strategy + ". Nagroda była za bramką " + g.prize +
      ". Jedna gra nie rozstrzyga, która strategia jest lepsza — uruchom symulację.</div>";
  }

  root.lcW2Monty = { newGame: newGame, choose: choose, stay: stay, change: change, addGames: addGames,
                     doorsHtml: doorsHtml, controlsHtml: controlsHtml, feedbackHtml: feedbackHtml, pct: pct };
  if (typeof document === "undefined") return;

  function init(box) {
    if (box.dataset.ready) return;
    box.dataset.ready = "1";
    var rnd = Math.random, M = box.__monty = { g: newGame(rnd), sim: { n: 0, stay: 0, sw: 0 } };
    function slot(n) { return box.querySelector('[data-mt="' + n + '"]'); }
    // Rycina lcSF przerysowuje się po zdarzeniu na swojej kontrolce (ukryte pole).
    function redrawChart() {
      var t = box.querySelector('[data-mt="tick"]');
      if (t) t.dispatchEvent(new Event("input", { bubbles: true }));
    }
    function render() {
      slot("controls").innerHTML = controlsHtml(M.g);
      slot("doors").innerHTML = doorsHtml(M.g);
      slot("feedback").innerHTML = feedbackHtml(M.g);
      slot("sim").hidden = M.g.fin == null;
      redrawChart();
    }
    box.addEventListener("click", function (e) {
      var b = e.target.closest && e.target.closest("[data-mt-act]");
      if (!b) return;
      var a = b.getAttribute("data-mt-act");
      if (/^door\d$/.test(a)) choose(M.g, Number(a.slice(4)), rnd);
      else if (a === "stay") stay(M.g);
      else if (a === "switch") change(M.g);
      else if (a === "new") { M.g = newGame(rnd); M.sim = { n: 0, stay: 0, sw: 0 }; }
      render();
    });
    render();
  }
  function scan() { document.querySelectorAll(".w2-monty:not([data-ready])").forEach(init); }
  document.addEventListener("DOMContentLoaded", scan);
  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  scan();

  // --- Rycina: odsetek wygranych obu strategii ---------------------------------------

  var S = root.lcSF;
  if (!S) return;
  S.register({
    w2_monty: {
      prepare: function (state, v, vals, rootEl) {
        var box = rootEl.closest(".w2-monty");
        state.M = box && box.__monty;
      },
      act: function (state, a) {
        var n = { g1: 1, g10: 10, g100: 100, g1000: 1000 }[a];
        if (n && state.M && state.M.g.fin != null) addGames(state.M.sim, n, Math.random);
      },
      noteKey: function (vals, state) {
        var n = state.M ? state.M.sim.n : 0;
        return n === 0 ? "none" : n < 30 ? "few" : "many";
      },
      draw: function (v, P, k, state) {
        var sim = state.M ? state.M.sim : { n: 0, stay: 0, sw: 0 }, n = sim.n;
        if (!n) {
          return { x: [0, 2], y: [0, 118], xTicks: [], noXGrid: true, yName: "Odsetek wygranych",
                   yLabels: { 0: "0%", 25: "25%", 50: "50%", 75: "75%", 100: "100%" },
                   layers: [{ type: "text", x: 1, y: 65, text: "Dograj partie przyciskami powyżej", color: P.secondary, size: 1.2 }],
                   aria: "Wykres czeka na dograne partie." };
        }
        var res = [[sim.stay, P.ref], [sim.sw, P.accent]], layers = [];
        // Oś w procentach (0–118), żeby podziałki 25/50/75/100% miały etykiety.
        layers.push({ type: "bins", data: res.map(function (r, i) { return [i + 1 - 0.31, i + 1 + 0.31, 100 * r[0] / n]; }),
                      colors: res.map(function (r) { return r[1]; }) });
        [100 / 3, 200 / 3].forEach(function (y) {
          layers.push({ type: "line", data: [[0.4, y], [2.6, y]], color: P.ref, width: 1.2, dash: [2, 4] });
        });
        res.forEach(function (r, i) {
          layers.push({ type: "text", x: i + 1, y: 100 * r[0] / n, text: pct(r[0] / n) + "\n(" + r[0] + " z " + n + ")", color: P.ink, bold: true });
        });
        return { x: [0.4, 2.6], y: [0, 118], xLabels: { 1: "Zostaję", 2: "Zmieniam" }, noXGrid: true,
                 yLabels: { 0: "0%", 25: "25%", 50: "50%", 75: "75%", 100: "100%" }, layers: layers, yName: "Odsetek wygranych",
                 aria: "Porównanie odsetka wygranych przy pozostaniu przy pierwszej bramce i przy zmianie bramki." };
      },
      update: function (rootEl, state) {
        var sim = state.M ? state.M.sim : { n: 0 }, n = sim.n, reads = rootEl.querySelector(".lc-reads");
        if (reads) reads.style.display = n ? "" : "none";
        if (!n) return;
        S.setValue(rootEl, "games", big(n));
        S.setValue(rootEl, "stay", pct(sim.stay / n));
        S.setValue(rootEl, "switch", pct(sim.sw / n));
      }
    }
  });
})();
