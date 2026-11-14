// Quiz kafelkowy (lc_tile_quiz() w lecture_layout.R): losuje n pytań z puli,
// pokazuje je po jednym, po odpowiedzi daje omówienie, na końcu wynik.
// Działa w przeglądarce, bez serwera. Dane w script[data-lc-tile-quiz="<id>"]:
//   { n, questions: [{ scenario, question, options[], correct (od 1), explanation }],
//     colors: { letter, score, question, correct, score_levels: { start, ok, mid, low } },
//     option_colors: [kolor litery dla każdej odpowiedzi] albo null,
//     labels: { question, score, result, correct, next, finish, again }, tiles_class,
//     summary: [{ min, text }] — pierwszy próg, który wynik (w %) osiąga }
// Miejsca na treść: #<id>_progress, _question, _options, _feedback, _summary.
// Funkcje stanu są w globalThis.lcTileQuiz (testy: tests/js/lc_quiz.test.js).
(function () {
  "use strict";
  var root = typeof window !== "undefined" ? window : globalThis;

  // --- Stan quizu (bez DOM) -------------------------------------------------------------

  function shuffle(items, random) {
    var a = items.slice();
    for (var i = a.length - 1; i > 0; i--) {
      var j = Math.floor(random() * (i + 1));
      var tmp = a[i]; a[i] = a[j]; a[j] = tmp;
    }
    return a;
  }
  function optionOrder(question, random) {
    return shuffle(question.options.map(function (_, i) { return i; }), random);
  }
  // Nowa gra: n losowych pytań, pierwsze z wylosowaną kolejnością odpowiedzi.
  function start(cfg, random) {
    var n = Math.min(cfg.n || cfg.questions.length, cfg.questions.length);
    var questions = shuffle(cfg.questions, random).slice(0, n);
    return { questions: questions, total: n, index: 0, correct: 0, wrong: 0,
      answered: false, finished: false, last: null, order: optionOrder(questions[0], random) };
  }
  // Odpowiedź: i to pozycja kafelka (od 0) w bieżącej kolejności.
  function answer(state, i) {
    if (state.finished || state.answered) return state;
    var q = state.questions[state.index];
    var ok = state.order[i] + 1 === q.correct;
    if (ok) state.correct += 1; else state.wrong += 1;
    state.answered = true;
    state.last = { ok: ok, label: q.options[q.correct - 1], explanation: q.explanation };
    return state;
  }
  function next(state, random) {
    if (state.index >= state.total - 1) return finish(state);
    state.index += 1;
    state.answered = false;
    state.order = optionOrder(state.questions[state.index], random);
    return state;
  }
  function finish(state) {
    state.finished = true;
    state.answered = false;
    return state;
  }
  // Wynik końcowy: procent, typ werdyktu (≥ 70 ok, ≥ 50 uwaga) i tekst z progów.
  function summary(state, cfg) {
    var pct = Math.round(state.correct / state.total * 100);
    var hit = (cfg.summary || []).filter(function (s) { return pct >= s.min; })[0];
    return { pct: pct, type: pct >= 70 ? "ok" : pct >= 50 ? "warning" : "danger", text: hit ? hit.text : "" };
  }
  // Kolor odczytu wyniku w trakcie gry: stały albo z progów (≥ 70% ok, ≥ 50% mid, mniej low).
  function scoreColor(state, colors) {
    var levels = colors.score_levels, answered = state.correct + state.wrong;
    if (!levels) return colors.score;
    if (answered === 0) return levels.start;
    var share = state.correct / answered;
    return share >= 0.7 ? levels.ok : share >= 0.5 ? levels.mid : levels.low;
  }
  root.lcTileQuiz = { shuffle: shuffle, start: start, answer: answer, next: next, finish: finish,
    summary: summary, scoreColor: scoreColor };
  if (typeof document === "undefined") return;

  // --- Widok ----------------------------------------------------------------------------

  var games = {};
  var LETTERS = "ABCDEFGH";

  function esc(s) {
    return String(s).replace(/[&<>"]/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c];
    });
  }
  function readout(label, value, color) {
    return '<div class="lc-read"' + (color ? ' style="--lc-read-color:' + esc(color) + ';"' : "") +
      '><span class="lc-read-label">' + esc(label) + '</span><span class="lc-read-value">' + esc(value) + "</span></div>";
  }
  function action(attr, id, label) {
    return '<button type="button" class="action-button lc-action is-solid" ' + attr + '="' + esc(id) + '"><span>' +
      esc(label) + "</span></button>";
  }
  function config(id) {
    var el = document.querySelector('script[data-lc-tile-quiz="' + id + '"]');
    return el ? JSON.parse(el.textContent) : null;
  }
  function slot(id, name) { return document.getElementById(id + "_" + name); }
  function fill(id, name, html) { var el = slot(id, name); if (el) el.innerHTML = html; }

  function render(id) {
    var cfg = config(id), s = games[id];
    if (!cfg || !s) return;
    var q = s.questions[s.index], colors = cfg.colors || {}, labels = cfg.labels || {};
    var answered = s.correct + s.wrong;
    fill(id, "progress", readout(labels.question || "pytanie", (s.index + 1) + " / " + s.total, colors.question) +
      readout(labels.score || "wynik", s.correct + " / " + answered, scoreColor(s, colors)));
    fill(id, "question", s.finished ? "" :
      (q.scenario ? '<div class="lc-status">' + esc(q.scenario) + "</div>" : "") + "<h4>" + esc(q.question) + "</h4>");
    fill(id, "options", s.finished || s.answered ? "" :
      '<div class="quiz-tiles ' + esc(cfg.tiles_class || "quiz-cols-2") + '">' + s.order.map(function (k, i) {
        var letter = cfg.option_colors ? cfg.option_colors[k] : colors.letter;
        return '<button type="button" class="btn btn-default quiz-tile" data-lc-quiz-answer="' + esc(id) +
          '" data-i="' + i + '"><div class="tile-letter"' +
          (letter ? ' style="background:' + esc(letter) + ';"' : "") + ">" + LETTERS.charAt(i) +
          '</div><div class="tile-text">' + esc(q.options[k]) + "</div></button>";
      }).join("") + "</div>");
    fill(id, "feedback", !s.answered || s.finished ? "" :
      '<div class="lc-status" role="status" aria-live="polite"><span class="lc-status-' +
      (s.last.ok ? "ok" : "danger") + '"><strong>' + (s.last.ok ? "Dobrze!" : "Nie tym razem.") + "</strong></span>" +
      (s.last.ok ? "" : " Poprawna odpowiedź: <strong>" + esc(s.last.label) + "</strong>.") +
      "<p>" + esc(s.last.explanation) + "</p></div>" +
      (s.index < s.total - 1 ? action("data-lc-quiz-next", id, labels.next || "Następne pytanie")
                             : action("data-lc-quiz-finish", id, labels.finish || "Zobacz wynik")));
    var sum = s.finished ? summary(s, cfg) : null;
    fill(id, "summary", !sum ? "" :
      '<div class="lc-reads">' + readout(labels.result || "wynik", sum.pct + "%", colors.score) +
      readout(labels.correct || "poprawne", s.correct + " / " + s.total, colors.correct) + "</div>" +
      '<div class="lc-status" role="status" aria-live="polite"><span class="lc-status-' + sum.type +
      '"><strong>' + esc(sum.text) + "</strong></span></div>" +
      '<div class="lc-toolbar">' + action("data-lc-quiz-start", id, labels.again || "Spróbuj ponownie") + "</div>");
  }

  document.addEventListener("click", function (e) {
    var t = e.target.closest && e.target.closest(
      "[data-lc-quiz-start], [data-lc-quiz-answer], [data-lc-quiz-next], [data-lc-quiz-finish]");
    if (!t) return;
    var id;
    if ((id = t.getAttribute("data-lc-quiz-start"))) {
      var cfg = config(id);
      if (!cfg) return;
      games[id] = start(cfg, Math.random);
    } else if ((id = t.getAttribute("data-lc-quiz-answer"))) {
      if (!games[id]) return;
      answer(games[id], Number(t.getAttribute("data-i")));
    } else if ((id = t.getAttribute("data-lc-quiz-next"))) {
      if (!games[id]) return;
      next(games[id], Math.random);
    } else if ((id = t.getAttribute("data-lc-quiz-finish"))) {
      if (!games[id]) return;
      finish(games[id]);
    }
    render(id);
  });
})();
