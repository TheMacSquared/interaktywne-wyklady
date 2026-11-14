// Wykład 01 „Język ryzyka” w przeglądarce, bez serwera R (dawniej jezyk_*_server):
//   łańcuch ryzyka     — klik w węzeł pokazuje definicję (gotowe opisy z R w data-config)
//   ćwiczenia 1.1, 1.2 — ocena przypisania kart lc_drop_match po „Sprawdź klasyfikację”
//   Demonstracja 1.1   — przełącza kadry SVG diagramu Venna za krokiem lc_step_widget
//   rycina j1_freq     — „Teoria kontra kolejne zmiany”: ukryte P, siatka i częstość
//   rycina j1_events   — „Suma, iloczyn i dopełnienie”: siatka 100 kontroli z suwakami
// Ryciny rysuje lcSF (R/lc_stat_figures.js, ECharts). Funkcje liczące są
// w window.lcJ1 (testy: tests/js/j1_jezyk.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;

  // --- Liczby i tekst jak w R -------------------------------------------------------

  function big(n) { return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, " "); }
  function fmtP(x) {  // risk_fmt_p()
    if (x == null || !isFinite(x)) return "—";
    var d = 3;
    if (x > 0 && x < 0.01) d = Math.max(3, Math.min(12, Math.ceil(-Math.log10(x)) + 2));
    return x.toFixed(d);
  }
  function esc(s) { return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;"); }

  // Ocena klasyfikacji (score_risk_classification w helpers.R). assignment: {pole: karta}.
  // Wariant rows: pola to zdania, karty to role; inaczej pola to role, karty to zdania.
  function classify(assignment, items, labels, rows) {
    var answers = {};
    items.forEach(function (it) { answers[it.id] = ""; });
    Object.keys(assignment || {}).forEach(function (zone) {
      var card = assignment[zone];
      if (rows) { if (zone in answers && card in labels) answers[zone] = card; }
      else if (card in answers && zone in labels) answers[card] = zone;
    });
    var correct = items.map(function (it) { return answers[it.id] === it.correct; });
    return { answers: answers, correct: correct, score: correct.filter(Boolean).length, total: items.length };
  }
  function classificationHtml(res, cfg) {
    var all = res.score === res.total;
    var li = cfg.items.map(function (it, i) {
      var sel = res.answers[it.id];
      var verdict = res.correct[i] ? "Dobrze rozpoznane. " :
        sel ? (cfg.rows ? "Przypisana rola: " : "Trafiło do pola ") + cfg.labels[sel] + ". " :
        cfg.rows ? "Bez przypisanej roli. " : "Nie trafiło do żadnego pola. ";
      return "<li><strong>" + esc(cfg.labels[it.correct]) + ": </strong>" + esc(it.text) + " " + esc(verdict) + esc(it.explanation) + "</li>";
    }).join("");
    return '<div class="lc-status" role="status" aria-live="polite"><span class="lc-status-' + (all ? "ok" : "warning") +
      '"><strong>Wynik: ' + res.score + "/" + res.total + ".</strong></span>" +
      esc(all ? cfg.ok : " Sprawdź różnicę między źródłem szkody, kontaktem, zdarzeniem i następstwem.") + "<ul>" + li + "</ul></div>";
  }

  // Siatka 100 kontroli (event_set_counts / build_event_grid w helpers.R).
  function eventCounts(nA, nB, overlap) {
    var lo = Math.max(0, nA + nB - 100), hi = Math.min(nA, nB), o = Math.min(Math.max(overlap, lo), hi);
    return { lo: lo, hi: hi, overlap: o, groups: [o, nA - o, nB - o, 100 - (nA + nB - o)] };
  }
  // Pola siatki 10 × 10 wierszami od góry, grupa po grupie: warstwy lcSF „rects”
  // (po jednej na kolor), z odstępem między polami (inner = bok pola w jednostkach osi).
  function gridLayers(groups, colors, inner) {
    var w = inner == null ? 0.88 : inner, c = (1 - w) / 2, id = 0;
    return groups.map(function (n, g) {
      var items = [];
      for (var j = 0; j < n; j++, id++) {
        var col = id % 10 + 1, row = Math.floor(id / 10) + 1;
        items.push([col - 0.5 + c, col + 0.5 - c, -row - 0.5 + c, -row + 0.5 - c]);
      }
      return { type: "rects", items: items, fill: colors[g] };
    }).filter(function (L) { return L.items.length; });
  }
  function cumulative(history) {
    var s = 0;
    return history.map(function (x, i) { s += x; return [i + 1, s / (i + 1)]; });
  }

  root.lcJ1 = { big: big, fmtP: fmtP, classify: classify, classificationHtml: classificationHtml,
                eventCounts: eventCounts, gridLayers: gridLayers, cumulative: cumulative };
  if (typeof document === "undefined") return;

  // --- Łańcuch ryzyka --------------------------------------------------------------

  root.lcChainSelect = function (el) {
    el.closest("svg").querySelectorAll(".lc-chain-node").forEach(function (n) { n.classList.toggle("is-active", n === el); });
    var box = document.querySelector('[data-ch1="chain-detail"]');
    if (!box) return;
    var cfg = JSON.parse(box.getAttribute("data-config") || "{}");
    box.innerHTML = cfg[el.dataset.key] || "";
  };

  // --- Ćwiczenia 1.1 i 1.2: przyciski „Sprawdź klasyfikację” -------------------------

  document.addEventListener("click", function (e) {
    var btn = e.target.closest && e.target.closest("[data-ch1-check]");
    if (!btn) return;
    var id = btn.getAttribute("data-ch1-check");
    var dm = document.querySelector('[data-lc-dm-input="' + id + '"]');
    var out = document.querySelector('[data-ch1-feedback="' + id + '"]');
    if (!dm || !out) return;
    var assignment = {};
    dm.querySelectorAll("[data-lc-dm-zone]").forEach(function (zone) {
      var card = zone.querySelector("[data-lc-dm-item]");
      if (card) assignment[zone.getAttribute("data-lc-dm-zone")] = card.getAttribute("data-lc-dm-item");
    });
    var cfg = JSON.parse(out.getAttribute("data-config"));
    out.innerHTML = classificationHtml(classify(assignment, cfg.items, cfg.labels, cfg.rows), cfg);
  });

  // --- Demonstracja 1.1: kadr za krokiem lc_step_widget --------------------------------

  function syncVenn() {
    document.querySelectorAll(".j1-venn").forEach(function (box) {
      var stepper = box.closest(".lc-stepper");
      if (!stepper) return;
      var step = Number(stepper.getAttribute("data-lc-step")) || 1;
      box.querySelectorAll("[data-venn]").forEach(function (f) { f.hidden = Number(f.getAttribute("data-venn")) !== step; });
      // Pole opisu szukamy w tym steperze, nie po id w całym dokumencie: build offline
      // trzyma kopie stepera z tym samym id i getElementById wiązałby je w pętlę zapisów.
      var out = stepper.querySelector('[id="' + stepper.id + '_text"]');
      var texts = JSON.parse(box.getAttribute("data-config") || "{}").texts || [];
      // Porównujemy numer wpisanego kroku, nie innerHTML: inne skrypty (np. pristine.js
      // w buildzie offline) dopisują atrybuty do wstawionych spanów i wpis by się zapętlił.
      if (out && out.getAttribute("data-j1-step") !== String(step)) {
        out.setAttribute("data-j1-step", String(step));
        out.innerHTML = texts[step - 1] || "";
      }
    });
  }
  new MutationObserver(function (muts) {
    if (muts.some(function (m) { return m.type === "childList" || m.attributeName === "data-lc-step"; })) syncVenn();
  }).observe(document.documentElement, { subtree: true, childList: true, attributes: true, attributeFilter: ["data-lc-step"] });
  document.addEventListener("DOMContentLoaded", syncVenn);

  // --- Ryciny lcSF -------------------------------------------------------------------

  var C = root.lcCharts, S = root.lcSF;
  if (!C || !S) return;
  function token(n) { return C.token("--upwr-" + n); }
  var CANDIDATES = [];
  for (var c = 1; c <= 30; c++) CANDIDATES.push(c / 100);
  function pick(except) {
    var pool = CANDIDATES.filter(function (p) { return p !== except; });
    return pool[Math.floor(Math.random() * pool.length)];
  }

  S.register({
    // Ćwiczenie 2: ukryte modelowe P, dodawanie zmian, odsłonięcie i nowa seria.
    j1_freq: {
      init: function (state) { state.h = []; state.p = pick(); state.shown = false; },
      act: function (state, a) {
        var n = { add1: 1, add10: 10, add100: 100, add1000: 1000 }[a];
        if (n) for (var i = 0; i < n; i++) state.h.push(Math.random() < state.p ? 1 : 0);
        else if (a === "reveal") state.shown = true;
        else if (a === "reset") { state.h = []; state.p = pick(state.p); state.shown = false; }
      },
      noteKey: function (vals, state) { return state.shown ? "shown" : "hidden"; },
      draw: function (v, P, k, state, vals) {
        if (k === 0) {
          // Pola w kolejności zmian (jedna zmiana = jedna grupa jednoelementowa), potem
          // scalone w trzy warstwy według koloru: zdarzenie, brak zdarzenia, puste.
          var last = state.h.slice(-100), cols = last.map(function (x) { return x ? P.accent : P.niebo; });
          while (cols.length < 100) cols.push(P.rule);
          var cells = gridLayers(cols.map(function () { return 1; }), cols);
          var layers = [P.accent, P.niebo, P.rule].map(function (color) {
            return { type: "rects", fill: color, items: cells.filter(function (L) { return L.fill === color; })
              .map(function (L) { return L.items[0]; }) };
          }).filter(function (L) { return L.items.length; });
          return { x: [0.5, 10.5], y: [-10.5, -0.5], xTicks: [], hideY: true, noXGrid: true, noYGrid: true,
                   layers: layers,
                   aria: "Siatka ostatnich stu zmian; burgundowe pola to zmiany z poślizgnięciem." };
        }
        var data = cumulative(state.h), n = data.length, guess = Number(vals.ch2_guess), xmax = n ? n : 2;
        // Przy długich seriach co któryś punkt (kształt linii bez zmian, lżejszy SVG).
        var step = Math.max(1, Math.floor(n / 1500)), pts = data.filter(function (d, i) { return i % step === 0 || i === n - 1; });
        var layers = [{ type: "line", data: [[0, guess], [xmax, guess]], color: token("cat-bursztyn"), width: 2, dash: [2, 3] }];
        if (state.shown) layers.push({ type: "line", data: [[0, state.p], [xmax, state.p]], color: P.accent, width: 2, dash: [7, 5] });
        if (n) layers.push({ type: "line", data: pts, color: P.niebo, width: 1.8 });
        return { x: [n ? 1 : 0, xmax], y: [0, 1], layers: layers, xName: "Liczba obserwowanych zmian", yName: "Skumulowana częstość",
                 aria: "Skumulowana częstość poślizgnięć z linią oszacowania; po odsłonięciu także linia modelowego P." };
      },
      update: function (rootEl, state) {
        var n = state.h.length, k = state.h.filter(Boolean).length;
        S.setValue(rootEl, "n", big(n));
        S.setValue(rootEl, "k", big(k));
        S.setValue(rootEl, "f", n ? fmtP(k / n) : "—");
        S.setValue(rootEl, "p", state.shown ? fmtP(state.p) : "ukryte");
        var fb = rootEl.querySelector('[data-role="fb"]');
        if (!fb) return;
        if (!state.shown) { fb.innerHTML = ""; return; }
        var guess = Number(rootEl.querySelector("#ch2_guess").value), p = state.p;
        fb.innerHTML = '<div class="lc-status" role="status" aria-live="polite"><strong>Porównanie:</strong> Twoje oszacowanie ' +
          fmtP(guess) + ", modelowe P " + fmtP(p) +
          (n ? ", częstość po " + big(n) + " zmianach " + fmtP(k / n) + ". Przy tej liczbie zmian częstość waha się wokół P typowo o √(p(1 - p)/n) ≈ " +
            fmtP(Math.sqrt(p * (1 - p) / n)) + "." : ". Dodaj zmiany, żeby porównać z częstością.") + "</div>";
      }
    },

    // Ćwiczenie 4: część wspólna nie może wyjść poza [max(0, Me + Be − 100), min(Me, Be)].
    j1_events: {
      prepare: function (state, v, vals, rootEl) {
        var nA = Number(vals.ch4_n_a), nB = Number(vals.ch4_n_b), o = rootEl.querySelector("#ch4_overlap");
        var ev = eventCounts(nA, nB, Number(vals.ch4_overlap));
        if (o) {
          o.min = String(ev.lo); o.max = String(ev.hi);
          if (Number(o.value) !== ev.overlap) o.value = String(ev.overlap);
          var out = rootEl.querySelector('output[data-value="ch4_overlap"]');
          if (out) out.textContent = o.value;
        }
        state.ev = ev; state.nA = nA;
      },
      draw: function (v, P, k, state) {
        var colors = [token("cat-wrzos"), P.terakota, P.niebo, P.ref];
        return { x: [0.5, 10.5], y: [-10.5, -0.5], xTicks: [], hideY: true, noXGrid: true, noYGrid: true,
                 layers: gridLayers(state.ev.groups, colors),
                 legend: ["Me i Be", "Tylko Me", "Tylko Be", "Ani Me, ani Be"].map(function (t, i) { return { text: t, color: colors[i], box: true }; }),
                 aria: "Siatka stu kontroli podzielonych na zdarzenie Me, zdarzenie Be, ich część wspólną oraz wyniki nienależące do żadnego zdarzenia." };
      },
      update: function (rootEl, state) {
        var g = state.ev.groups, union = g[0] + g[1] + g[2];
        S.setValue(rootEl, "and", fmtP(g[0] / 100));
        S.setValue(rootEl, "or", fmtP(union / 100));
        S.setValue(rootEl, "notA", fmtP(1 - state.nA / 100));
        S.setValue(rootEl, "none", fmtP((100 - union) / 100));
      }
    }
  });
})();
