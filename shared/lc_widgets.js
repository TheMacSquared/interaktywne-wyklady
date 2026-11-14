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
