// Widgety v2 i tabele v2: logika klienta komponentów z lecture_layout.R.
// Plik identyczny we wszystkich kursach.
(function() {
  'use strict';

  // Język strony: potrzebny do dzielenia wyrazów (hyphens: auto) w justowanym tekście.
  document.documentElement.setAttribute('lang', 'pl');

  // --- Liczby jak lc_fmt() w R: kropka dziesiętna, bez końcowych zer ---
  function lcFormat(value, digits, suffix) {
    var x = Number(value);
    if (!isFinite(x)) return String(value);
    var s = Math.abs(x).toFixed(digits);
    if (digits > 0) s = s.replace(/\.?0+$/, '');
    var neg = x < 0 && Number(s) !== 0;
    return (neg ? '-' : '') + s + (suffix || '');
  }

  // --- Suwak: wartość w etykiecie ---
  function updateSliderOutput(input) {
    if (!input || !input.id) return;
    var outputs = document.querySelectorAll('output[data-lc-slider="' + input.id + '"]');
    if (!outputs.length) return;
    var slider = window.jQuery && window.jQuery(input).data('ionRangeSlider');
    var value = slider ? slider.result.from : input.value;
    outputs.forEach(function(o) {
      var digits = Number(o.getAttribute('data-digits')) || 0;
      var suffix = o.getAttribute('data-suffix');
      o.textContent = lcFormat(value, digits, suffix);
      // Końce toru: aktualne min i max (updateSliderInput może je zmienić).
      var group = o.closest('.lc-slider');
      var ends = group && group.querySelectorAll('.lc-slider-ends span');
      if (slider && ends && ends.length === 2) {
        ends[0].textContent = lcFormat(slider.result.min, digits, suffix);
        ends[1].textContent = lcFormat(slider.result.max, digits, suffix);
      }
    });
  }
  document.addEventListener('input', function(e) {
    if (e.target.classList && e.target.classList.contains('js-range-slider')) updateSliderOutput(e.target);
  });
  if (window.jQuery) {
    window.jQuery(document).on('change', '.js-range-slider', function() { updateSliderOutput(this); });
    // updateSliderInput(): stan suwaka zmienia się po obsłudze komunikatu.
    window.jQuery(document).on('shiny:updateinput', '.js-range-slider', function() {
      var input = this;
      setTimeout(function() { updateSliderOutput(input); }, 0);
    });
  }

  // Strona statyczna (bez Shiny): suwaki uruchamiamy sami. ionRangeSlider czyta
  // konfigurację z atrybutów data-* i sam zgłasza zdarzenia input i change.
  function initStaticSliders() {
    var $ = window.jQuery;
    if (window.Shiny || !$ || !$.fn || !$.fn.ionRangeSlider) return;
    $('.js-range-slider').each(function() {
      if (!$(this).data('ionRangeSlider')) $(this).ionRangeSlider();
      updateSliderOutput(this);
    });
  }
  document.addEventListener('DOMContentLoaded', initStaticSliders);

  // --- Segment: wyłączenie wartości wybranej w segmencie powiązanym ---
  function syncExclusive(group) {
    var otherId = group.getAttribute('data-lc-exclusive');
    var other = otherId && document.getElementById(otherId);
    if (!other) return;
    [[group, other], [other, group]].forEach(function(pair) {
      var checked = pair[1].querySelector('input[type="radio"]:checked');
      var taken = checked ? checked.value : null;
      pair[0].querySelectorAll('input[type="radio"]').forEach(function(r) {
        r.disabled = r.value === taken;
      });
    });
  }
  function syncAllExclusive(root) {
    (root || document).querySelectorAll('.lc-seg-input[data-lc-exclusive]').forEach(syncExclusive);
  }
  document.addEventListener('change', function(e) {
    var group = e.target.closest && e.target.closest('.lc-seg-input');
    if (!group) return;
    syncAllExclusive(document);
  });

  // --- Karty odpowiedzi z kluczem: div.lc-choices[data-correct] ---
  // Kontener dostaje data-answered="correct" | "wrong"; kolor w CSS. Bez
  // data-reveal ocena przychodzi od razu po wyborze; z data-reveal="<id
  // przycisku>" dopiero po kliknięciu przycisku, a zmiana wyboru ją kasuje.
  function markChoices(box) {
    var checked = box.querySelector('input[type="radio"]:checked');
    if (!checked) { box.removeAttribute('data-answered'); return; }
    box.setAttribute('data-answered',
      checked.value === box.getAttribute('data-correct') ? 'correct' : 'wrong');
  }
  document.addEventListener('change', function(e) {
    var box = e.target.closest && e.target.closest('.lc-choices[data-correct]');
    if (!box || e.target.type !== 'radio') return;
    if (box.hasAttribute('data-reveal')) box.removeAttribute('data-answered');
    else markChoices(box);
  });
  document.addEventListener('click', function(e) {
    var btn = e.target.closest && e.target.closest('button[id]');
    if (!btn) return;
    document.querySelectorAll('.lc-choices[data-correct][data-reveal]').forEach(function(box) {
      if (box.getAttribute('data-reveal') === btn.id) markChoices(box);
    });
  });

  // --- Quiz z kluczem: ocena i omówienie po kliknięciu przycisku ---
  // Przycisk [data-lc-quiz-check="<id>"], dane pytań w script[data-lc-quiz-data="<id>"]
  // (name, question, correct, correct_label, explanation), wynik w #<id>_quiz_feedback.
  function escapeHtml(s) {
    return String(s).replace(/[&<>"]/g, function(c) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c];
    });
  }
  function quizStatus(type, label, text) {
    return '<div class="lc-status" role="status" aria-live="polite">' +
      '<span class="lc-status-' + type + '"><strong>' + escapeHtml(label) + '</strong></span>' +
      escapeHtml(text) + '</div>';
  }
  document.addEventListener('click', function(e) {
    var btn = e.target.closest && e.target.closest('[data-lc-quiz-check]');
    if (!btn) return;
    var id = btn.getAttribute('data-lc-quiz-check');
    var data = document.querySelector('script[data-lc-quiz-data="' + id + '"]');
    var out = document.getElementById(id + '_quiz_feedback');
    if (!data || !out) return;
    var questions = JSON.parse(data.textContent);
    var score = 0, missing = 0;
    var items = questions.map(function(q) {
      var checked = document.querySelector('input[type="radio"][name="' + q.name + '"]:checked');
      var answer = checked ? checked.value : '';
      var ok = answer === q.correct;
      if (ok) score += 1;
      if (!answer) missing += 1;
      return '<li>' + quizStatus(ok ? 'ok' : 'warning',
        ok ? 'Poprawnie:' : !answer ? 'Brak odpowiedzi:' : 'Do poprawy:',
        ' ' + q.question + ' Poprawna odpowiedź: ' + q.correct_label + '. ' + q.explanation) + '</li>';
    });
    out.innerHTML = quizStatus(score === questions.length ? 'ok' : 'warning',
      'Wynik: ' + score + '/' + questions.length + '.',
      missing ? ' Bez odpowiedzi: ' + missing + '.' : ' Poniżej omówienie każdej odpowiedzi.') +
      '<ol>' + items.join('') + '</ol>';
  });

  // --- Głosowanie („Najpierw zdecyduj”): komentarz do wybranej odpowiedzi ---
  // Przycisk [data-lc-vote-check="<id>"], dane w script[data-lc-vote-data="<id>"]:
  // { output: id miejsca na komentarz, feedback: { <wartość>: html, ".other": html,
  // ".none": html } }. Po pierwszym sprawdzeniu komentarz podąża za wyborem.
  function voteRender(id) {
    var data = document.querySelector('script[data-lc-vote-data="' + id + '"]');
    if (!data) return;
    var cfg = JSON.parse(data.textContent);
    var out = document.getElementById(cfg.output);
    if (!out) return;
    var checked = document.querySelector('input[type="radio"][name="' + id + '"]:checked');
    var fb = cfg.feedback;
    out.innerHTML = !checked ? fb['.none'] :
      fb[checked.value] !== undefined ? fb[checked.value] : (fb['.other'] || '');
    out.setAttribute('data-lc-vote-open', 'true');
  }
  document.addEventListener('click', function(e) {
    var btn = e.target.closest && e.target.closest('[data-lc-vote-check]');
    if (btn) voteRender(btn.getAttribute('data-lc-vote-check'));
  });
  document.addEventListener('change', function(e) {
    if (e.target.type !== 'radio' || !e.target.name) return;
    var data = document.querySelector('script[data-lc-vote-data="' + e.target.name + '"]');
    if (!data) return;
    var out = document.getElementById(JSON.parse(data.textContent).output);
    if (out && out.hasAttribute('data-lc-vote-open')) voteRender(e.target.name);
  });

  // --- Warianty treści (lc_variants): widoczny wariant = wartość kontrolki ---
  // div[data-lc-variants="<id kontrolki>"] > div[data-lc-variant="<wartość>"].
  // Z data-lc-variants-reveal="<id przycisku>" treść pojawia się po kliknięciu
  // przycisku, a zmiana wyboru ją chowa.
  // Ostatnia wartość kontrolki: w wersji Shiny rozdziały są wstawiane pojedynczo,
  // więc kontrolka z innego rozdziału (np. śledzona zmienna) bywa poza dokumentem.
  var rememberedValues = {};
  function controlValue(id) {
    var el = document.getElementById(id);
    if (el && el.tagName === 'SELECT') return el.value;
    var checked = document.querySelector('input[type="radio"][name="' + id + '"]:checked');
    if (checked) return checked.value;
    if (el && typeof el.value === 'string') return el.value;
    return Object.prototype.hasOwnProperty.call(rememberedValues, id) ? rememberedValues[id] : null;
  }
  function variantsSync(box, show) {
    var value = controlValue(box.getAttribute('data-lc-variants'));
    var changed = false;
    Array.prototype.forEach.call(box.children, function(child) {
      if (!child.hasAttribute('data-lc-variant')) return;
      var hide = !(show && child.getAttribute('data-lc-variant') === value);
      if (child.hidden !== hide) changed = true;
      child.hidden = hide;
    });
    // Spis sekcji i wykresy w odsłoniętym wariancie reagują na zmianę.
    if (changed) {
      document.dispatchEvent(new CustomEvent('lc:variants', { detail: { id: box.getAttribute('data-lc-variants') } }));
      setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 0);
    }
  }
  document.addEventListener('change', function(e) {
    var id = e.target.name || e.target.id;
    if (!id) return;
    if (e.target.tagName === 'SELECT' || (e.target.type === 'radio' && e.target.checked)) rememberedValues[id] = e.target.value;
    document.querySelectorAll('[data-lc-variants]').forEach(function(box) {
      if (box.getAttribute('data-lc-variants') !== id || box.hasAttribute('data-lc-variants-pick')) return;
      variantsSync(box, !box.hasAttribute('data-lc-variants-reveal'));
    });
  });
  // Treść wstawiona później (rozdział z renderUI): warianty kontrolki, której nie ma
  // już w dokumencie, ustawiamy według zapamiętanej wartości.
  var variantsQueued = false;
  new MutationObserver(function() {
    if (variantsQueued || !Object.keys(rememberedValues).length) return;
    variantsQueued = true;
    setTimeout(function() {
      variantsQueued = false;
      document.querySelectorAll('[data-lc-variants]').forEach(function(box) {
        if (box.hasAttribute('data-lc-variants-pick') || box.hasAttribute('data-lc-variants-reveal')) return;
        var id = box.getAttribute('data-lc-variants');
        if (document.getElementById(id) || document.querySelector('input[name="' + id + '"]')) return;
        if (Object.prototype.hasOwnProperty.call(rememberedValues, id)) variantsSync(box, true);
      });
    }, 0);
  }).observe(document.documentElement, { childList: true, subtree: true });

  // Wybór kliknięciem: [data-lc-pick="<grupa>"][data-value]; ponowne kliknięcie chowa treść.
  function pickVariant(el) {
    var group = el.getAttribute('data-lc-pick'), value = el.getAttribute('data-value');
    var on = el.getAttribute('aria-pressed') !== 'true';
    document.querySelectorAll('[data-lc-pick]').forEach(function(p) {
      if (p.getAttribute('data-lc-pick') === group) p.setAttribute('aria-pressed', on && p === el ? 'true' : 'false');
    });
    document.querySelectorAll('[data-lc-variants-pick]').forEach(function(box) {
      if (box.getAttribute('data-lc-variants') !== group) return;
      Array.prototype.forEach.call(box.children, function(child) {
        if (child.hasAttribute('data-lc-variant')) child.hidden = !(on && child.getAttribute('data-lc-variant') === value);
      });
    });
  }
  document.addEventListener('click', function(e) {
    var el = e.target.closest && e.target.closest('[data-lc-pick]');
    if (el) pickVariant(el);
  });
  document.addEventListener('keydown', function(e) {
    if (e.key !== 'Enter' && e.key !== ' ') return;
    var el = e.target.closest && e.target.closest('[data-lc-pick]');
    if (el && el === e.target) { e.preventDefault(); pickVariant(el); }
  });

  // Przełącznik treści (lc_toggle_button + lc_toggle_content).
  document.addEventListener('click', function(e) {
    var btn = e.target.closest && e.target.closest('[data-lc-toggle-button]');
    if (!btn) return;
    var on = btn.getAttribute('aria-pressed') !== 'true';
    btn.setAttribute('aria-pressed', on ? 'true' : 'false');
    var label = btn.querySelector('span');
    if (label) label.textContent = btn.getAttribute(on ? 'data-label-on' : 'data-label-off');
    document.querySelectorAll('[data-lc-toggle]').forEach(function(box) {
      if (box.getAttribute('data-lc-toggle') !== btn.id) return;
      Array.prototype.forEach.call(box.children, function(child) {
        if (child.hasAttribute('data-lc-toggle-state')) {
          child.hidden = (child.getAttribute('data-lc-toggle-state') === 'on') !== on;
        }
      });
    });
  });
  document.addEventListener('click', function(e) {
    var btn = e.target.closest && e.target.closest('button[id]');
    if (!btn) return;
    document.querySelectorAll('[data-lc-variants-reveal]').forEach(function(box) {
      if (box.getAttribute('data-lc-variants-reveal') === btn.id) variantsSync(box, true);
    });
  });

  // --- Treść odsłaniana przyciskiem (lc_reveal): div[data-lc-reveal="<id przycisku>"] ---
  document.addEventListener('click', function(e) {
    var btn = e.target.closest && e.target.closest('button[id]');
    if (!btn) return;
    document.querySelectorAll('[data-lc-reveal]').forEach(function(box) {
      if (box.getAttribute('data-lc-reveal') === btn.id) box.hidden = false;
    });
  });

  // --- Klikalne komórki tabeli: input$<id> = c(i, j) ---
  document.addEventListener('click', function(e) {
    var btn = e.target.closest && e.target.closest('.lc-cell-btn[data-lc-cell-input]');
    if (!btn || !window.Shiny) return;
    window.Shiny.setInputValue(btn.getAttribute('data-lc-cell-input'),
      [Number(btn.getAttribute('data-i')), Number(btn.getAttribute('data-j'))],
      { priority: 'event' });
  });

  // --- Stronicowanie tabel ---
  // Z data-lc-page-input stronę renderuje serwer (input$<id> = numer strony);
  // bez niego przeglądarka ukrywa wiersze spoza bieżącej strony.
  document.addEventListener('click', function(e) {
    var btn = e.target.closest && e.target.closest('.lc-pager [data-lc-page-to]');
    if (!btn || btn.disabled) return;
    var pager = btn.closest('.lc-pager');
    var to = Number(btn.getAttribute('data-lc-page-to'));
    var inputId = pager.getAttribute('data-lc-page-input');
    if (inputId) {
      if (window.Shiny) window.Shiny.setInputValue(inputId, to, { priority: 'event' });
      return;
    }
    var size = Number(pager.getAttribute('data-lc-page-size'));
    var total = Number(pager.getAttribute('data-lc-total'));
    var pages = Math.ceil(total / size);
    to = Math.max(1, Math.min(pages, to));
    var block = pager.closest('.lc-tbl-block');
    var rows = block.querySelectorAll('table.lc-tbl > tbody > tr');
    rows.forEach(function(row, i) {
      row.classList.toggle('is-paged-out', i < (to - 1) * size || i >= to * size);
    });
    var from = (to - 1) * size + 1, last = Math.min(total, to * size);
    pager.querySelector('[data-lc-page-range]').textContent = 'Wiersze ' + from + '–' + last + ' z ' + total;
    pager.querySelector('[data-lc-page-label]').textContent = to + ' / ' + pages;
    var navs = pager.querySelectorAll('[data-lc-page-to]');
    navs[0].setAttribute('data-lc-page-to', to - 1); navs[0].disabled = to <= 1;
    navs[1].setAttribute('data-lc-page-to', to + 1); navs[1].disabled = to >= pages;
  });

  // --- Przełączniki drugorzędne (lc_chips): jeden aktywny albo żaden ---
  document.addEventListener('click', function(e) {
    var chip = e.target.closest && e.target.closest('.lc-chip');
    if (!chip) return;
    var group = chip.closest('[data-lc-chips]');
    var on = chip.getAttribute('aria-pressed') !== 'true';
    group.querySelectorAll('.lc-chip').forEach(function(x) { x.setAttribute('aria-pressed', 'false'); });
    if (on) chip.setAttribute('aria-pressed', 'true');
    if (window.Shiny) {
      window.Shiny.setInputValue(group.getAttribute('data-lc-chips'),
        on ? chip.getAttribute('data-value') : null, { priority: 'event' });
    }
  });

  // --- Widget krokowy z paskiem kroków: stan w przeglądarce, input$<id>_step ---
  function stepperSync(root, k, send) {
    var tabs = root.querySelectorAll('.lc-step-tab'), n = tabs.length;
    k = Math.max(1, Math.min(n, k | 0 || 1));
    root.setAttribute('data-lc-step', k);
    tabs.forEach(function(b, i) {
      if (i + 1 === k) b.setAttribute('aria-current', 'step'); else b.removeAttribute('aria-current');
      b.classList.toggle('is-done', i + 1 < k);
    });
    var kick = root.querySelector('.lc-stepper-kicker'), cur = tabs[k - 1];
    if (kick && cur) kick.textContent = 'Krok ' + k + ' z ' + n + ' · ' + (cur.querySelector('span') || cur).textContent;
    var nav = function(a) { return root.querySelector('[data-lc-nav="' + a + '"]'); };
    if (nav('prev')) nav('prev').disabled = k === 1;
    if (nav('next')) nav('next').disabled = k === n;
    // Opisy kroków z lc_step_widget(texts = ): widoczny opis bieżącego kroku.
    root.querySelectorAll('[data-lc-step-text]').forEach(function(t) {
      if (t.closest('.lc-stepper') === root) t.hidden = Number(t.getAttribute('data-lc-step-text')) !== k;
    });
    root.querySelectorAll('[data-lc-from]').forEach(function(c) {
      var off = k < Number(c.getAttribute('data-lc-from'));
      c.classList.toggle('lc-is-off', off);
      c.setAttribute('aria-disabled', off ? 'true' : 'false');
    });
    if (send && window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue(root.id + '_step', k, { priority: 'event' });
    }
  }
  document.addEventListener('click', function(e) {
    var root = e.target.closest && e.target.closest('.lc-stepper');
    if (!root) return;
    var cur = Number(root.getAttribute('data-lc-step')) || 1;
    var go = e.target.closest('[data-lc-go]'), nav = e.target.closest('[data-lc-nav]');
    if (go) stepperSync(root, Number(go.getAttribute('data-lc-go')), true);
    else if (nav) {
      var a = nav.getAttribute('data-lc-nav');
      stepperSync(root, a === 'next' ? cur + 1 : a === 'prev' ? cur - 1 : 1, true);
    }
  });
  document.addEventListener('keydown', function(e) {
    var root = e.target.closest && e.target.closest('.lc-stepper');
    if (!root || /^(INPUT|SELECT|TEXTAREA)$/.test(e.target.tagName)) return;
    if (e.target.closest('.irs, .lc-seg')) return;
    var cur = Number(root.getAttribute('data-lc-step')) || 1;
    if (e.key === 'ArrowRight') { stepperSync(root, cur + 1, true); e.preventDefault(); }
    if (e.key === 'ArrowLeft') { stepperSync(root, cur - 1, true); e.preventDefault(); }
  });
  function stepperInitAll() {
    document.querySelectorAll('.lc-stepper').forEach(function(r) {
      stepperSync(r, Number(r.getAttribute('data-lc-step')) || 1, false);
    });
  }
  document.addEventListener('DOMContentLoaded', stepperInitAll);
  if (window.jQuery) {
    window.jQuery(document).on('shiny:connected', function() {
      window.Shiny.addCustomMessageHandler('lc-step-set', function(m) {
        var r = document.getElementById(m.id);
        if (r) stepperSync(r, m.step, true);
      });
    });
    // Kontrolki z renderUI pojawiają się później: odśwież stan po każdym renderze.
    window.jQuery(document).on('shiny:value', function() { setTimeout(stepperInitAll, 0); });
  }

  // --- Kroki demonstracji: input binding, wartość = numer kroku ---
  function renderSteps(el) {
    var n = Number(el.getAttribute('data-lc-steps'));
    var step = Number(el.getAttribute('data-step')) || 0;
    var min = Number(el.getAttribute('data-lc-min')) || 0;
    var prev = el.querySelector('[data-lc-step="prev"]');
    var next = el.querySelector('[data-lc-step="next"]');
    if (prev) prev.disabled = step <= min;
    if (next) {
      next.disabled = step >= n;
      var label = next.querySelector('span');
      if (label) {
        label.textContent = step === 0 ? el.getAttribute('data-start-label')
                                       : el.getAttribute('data-next-label');
      }
    }
    el.querySelectorAll('.lc-step-dot').forEach(function(dot) {
      var current = Number(dot.getAttribute('data-lc-step-to')) === step;
      dot.setAttribute('aria-current', current ? 'step' : 'false');
    });
  }
  function setStep(el, step) {
    var n = Number(el.getAttribute('data-lc-steps'));
    var min = Number(el.getAttribute('data-lc-min')) || 0;
    step = Math.max(min, Math.min(n, step));
    el.setAttribute('data-step', step);
    renderSteps(el);
    if (window.jQuery) window.jQuery(el).trigger('lc-step-change');
  }
  document.addEventListener('click', function(e) {
    var target = e.target.closest && e.target.closest('.lc-step-nav [data-lc-step], .lc-step-nav [data-lc-step-to]');
    if (!target) return;
    var el = target.closest('.lc-step-nav');
    var step = Number(el.getAttribute('data-step')) || 0;
    if (target.hasAttribute('data-lc-step-to')) {
      setStep(el, Number(target.getAttribute('data-lc-step-to')));
    } else {
      setStep(el, step + (target.getAttribute('data-lc-step') === 'next' ? 1 : -1));
    }
  });

  function registerBindings() {
    if (!window.Shiny || !window.Shiny.InputBinding || registerBindings.done) return;
    registerBindings.done = true;
    var $ = window.jQuery;
    var stepBinding = new window.Shiny.InputBinding();
    $.extend(stepBinding, {
      find: function(scope) { return $(scope).find('.lc-step-nav[data-lc-steps]'); },
      initialize: function(el) { renderSteps(el); },
      getValue: function(el) { return Number(el.getAttribute('data-step')) || 0; },
      setValue: function(el, value) { setStep(el, Number(value)); },
      receiveMessage: function(el, data) { if (data && data.step !== undefined) setStep(el, data.step); },
      subscribe: function(el, callback) { $(el).on('lc-step-change.lcSteps', function() { callback(false); }); },
      unsubscribe: function(el) { $(el).off('.lcSteps'); }
    });
    window.Shiny.inputBindings.register(stepBinding, 'lc.stepNav');
  }
  registerBindings();
  document.addEventListener('DOMContentLoaded', registerBindings);

  // --- conditionalPanel() na stronie statycznej ---
  // Shiny ocenia warunki data-display-if w przeglądarce; bez Shiny robimy to sami.
  // W warunku dostępne jest `input` z wartościami kontrolek czytanymi z dokumentu:
  // kroki lc_step_nav, widget krokowy (<id>_step), pola wyboru, radio, listy, pola liczbowe.
  function staticInputValue(id) {
    if (/_step$/.test(id)) {
      var stepper = document.getElementById(id.replace(/_step$/, ''));
      if (stepper && stepper.classList.contains('lc-stepper')) return Number(stepper.getAttribute('data-lc-step')) || 1;
    }
    var el = document.getElementById(id);
    if (el && el.classList.contains('lc-step-nav')) return Number(el.getAttribute('data-step')) || 0;
    if (el && el.tagName === 'INPUT' && el.type === 'checkbox') return el.checked;
    if (el && el.tagName === 'INPUT' && (el.type === 'number' || el.classList.contains('js-range-slider'))) {
      var raw = el.value !== '' ? el.value : el.getAttribute('data-from');
      return raw === null || raw === '' ? null : Number(raw);
    }
    if (el && (el.tagName === 'SELECT' || el.tagName === 'INPUT' || el.tagName === 'TEXTAREA')) return el.value;
    var boxes = document.querySelectorAll('input[type="checkbox"][name="' + id + '"]');
    if (boxes.length) {
      return Array.prototype.filter.call(boxes, function(b) { return b.checked; }).map(function(b) { return b.value; });
    }
    var radio = document.querySelector('input[type="radio"][name="' + id + '"]:checked');
    return radio ? radio.value : null;
  }
  var staticInput = typeof Proxy === 'undefined' ? null : new Proxy({}, {
    has: function() { return true; },
    get: function(_, name) { return typeof name === 'string' ? staticInputValue(name) : undefined; }
  });
  function syncConditionalPanels() {
    if (window.Shiny || !staticInput) return;
    var shown = false;
    document.querySelectorAll('[data-display-if]').forEach(function(panel) {
      var visible;
      try {
        visible = !!(new Function('input', 'output', 'return (' + panel.getAttribute('data-display-if') + ');'))(staticInput, {});
      } catch (err) { visible = false; }
      var was = panel.classList.contains('shiny-conditional--shown');
      // Arkusz Shiny chowa panele bez tej klasy; display ustawiamy jak Shiny.
      panel.classList.toggle('shiny-conditional--shown', visible);
      panel.style.display = visible ? '' : 'none';
      if (visible && !was) shown = true;
    });
    // Wykresy w panelu ukrytym do tej pory mierzą się dopiero teraz.
    if (shown) setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 0);
  }
  var conditionalQueued = false;
  function queueConditionalPanels() {
    if (conditionalQueued) return;
    conditionalQueued = true;
    setTimeout(function() { conditionalQueued = false; syncConditionalPanels(); }, 0);
  }
  ['click', 'change', 'input', 'keyup'].forEach(function(type) {
    document.addEventListener(type, queueConditionalPanels);
  });
  document.addEventListener('DOMContentLoaded', syncConditionalPanels);

  // lc_more(): wyjście Shiny w zwiniętym <details> jest ukryte i nie renderuje
  // się; po rozwinięciu trzeba powiedzieć Shiny, że stało się widoczne.
  document.addEventListener('toggle', function(e) {
    var d = e.target;
    if (d.classList && d.classList.contains('lc-more') && d.open && window.jQuery) {
      window.jQuery(d).trigger('shown');
    }
  }, true);

  // Treść dodana przez renderUI: odśwież stan segmentów i kroków.
  if (window.jQuery) {
    window.jQuery(document).on('shiny:value shiny:bound', function() {
      syncAllExclusive(document);
      document.querySelectorAll('.lc-step-nav[data-lc-steps]').forEach(renderSteps);
    });
  }
})();
