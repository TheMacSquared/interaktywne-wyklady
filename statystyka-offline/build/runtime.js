// Wersja offline: zastępuje serwer Shiny tam, gdzie wykład potrzebuje go tylko
// do nawigacji. Wszystkie rozdziały są w pliku; pokazujemy jeden naraz.
(function () {
  'use strict';

  var handlers = {};

  function show(id) {
    var secs = document.querySelectorAll('.lc-off-chapter');
    var target = document.getElementById(id);
    if (!target || !target.classList.contains('lc-off-chapter')) target = secs[0];
    if (!target) return;
    // Skrypty wykładu (np. spis sekcji) szukają aktywnego rozdziału po tym atrybucie.
    secs.forEach(function (s) {
      var on = s === target;
      s.classList.toggle('lc-off-active', on);
      if (on) s.setAttribute('data-lc-chapter-content', 'true');
      else s.removeAttribute('data-lc-chapter-content');
    });
    var main = document.getElementById('lc-main');
    if (main) main.scrollTop = 0;
    if (handlers.setActiveChapter) handlers.setActiveChapter(target.id);
    try { history.replaceState(null, '', '#' + target.id); } catch (e) {}
    setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 50);
  }

  function noop() {}
  window.Shiny = {
    offline: true,
    addCustomMessageHandler: function (name, fn) { handlers[name] = fn; },
    setInputValue: function (name, value) { if (name === 'lc__switch_chapter') show(value); },
    InputBinding: function () {},
    OutputBinding: function () {},
    inputBindings: { register: noop },
    outputBindings: { register: noop },
    bindAll: noop, unbindAll: noop, initializeInputs: noop
  };

  // Wzory są już złożone jako SVG; skrypty wykładu czekają jednak na MathJax.
  window.MathJax = {
    Hub: {
      Register: { MessageHook: noop, StartupHook: noop },
      Queue: function () {
        for (var i = 0; i < arguments.length; i++)
          if (typeof arguments[i] === 'function') setTimeout(arguments[i], 0);
      },
      Config: noop, Typeset: noop
    }
  };

  document.addEventListener('click', function (e) {
    var btn = e.target.closest && e.target.closest('.lc-nav-chapter-btn');
    if (!btn) return;
    var ch = btn.closest('[data-lc-chapter]');
    if (ch) show(ch.getAttribute('data-lc-chapter'));
  });

  document.addEventListener('DOMContentLoaded', function () {
    setTimeout(function () { show(decodeURIComponent(location.hash.slice(1))); }, 0);
  });
})();
