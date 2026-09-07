/* ==========================================================================
   lc_drop_match — przypisywanie kart do pól przez przeciąganie i klawiaturę.
   Komponent UI generuje lecture_layout.R::lc_drop_match(); style są
   w shared_styles.css (sekcja .lc-dm*). Bez zależności zewnętrznych.

   Stan trzymamy wyłącznie w DOM: karta leży albo w .lc-dm-pool, albo w
   .lc-dm-slot pola. Po każdej zmianie wysyłamy do Shiny mapę
   { kod_pola: id_karty } — puste pola są pomijane.
   ========================================================================== */

(function () {
  var LIFTED = null;      // aktualnie podniesiona karta (tryb klawiatury)
  var TARGET_INDEX = 0;   // indeks podświetlonego pola przy podniesionej karcie

  function root(el) {
    return el ? el.closest('.lc-dm') : null;
  }

  function zones(container) {
    return Array.prototype.slice.call(
      container.querySelectorAll('.lc-dm-zone')
    );
  }

  function pool(container) {
    return container.querySelector('.lc-dm-pool');
  }

  function cardIn(zone) {
    return zone.querySelector('.lc-dm-card');
  }

  function zoneLabel(zone) {
    var label = zone.querySelector('.lc-dm-zone-label');
    return label ? label.textContent.trim() : zone.getAttribute('data-lc-dm-zone');
  }

  function announce(container, message) {
    var status = container.querySelector('.lc-dm-status');
    if (status) status.textContent = message;
  }

  function poolCount(container) {
    var p = pool(container);
    return p ? p.querySelectorAll('.lc-dm-card').length : 0;
  }

  function remaining(container) {
    var n = poolCount(container);
    if (n === 0) return 'Pula jest pusta.';
    if (n === 1) return 'W puli została 1 karta.';
    if (n >= 2 && n <= 4) return 'W puli zostały ' + n + ' karty.';
    return 'W puli zostało ' + n + ' kart.';
  }

  // ---- Synchronizacja z Shiny -------------------------------------------
  function sync(container) {
    var inputId = container.getAttribute('data-lc-dm-input');
    if (!inputId || typeof Shiny === 'undefined' || !Shiny.setInputValue) return;

    var value = {};
    zones(container).forEach(function (zone) {
      var card = cardIn(zone);
      if (card) value[zone.getAttribute('data-lc-dm-zone')] = card.getAttribute('data-lc-dm-item');
    });
    Shiny.setInputValue(inputId, value, { priority: 'event' });
  }

  // ---- Przenoszenie kart -------------------------------------------------
  function toPool(container, card) {
    var p = pool(container);
    if (!p || !card) return;
    var order = parseInt(card.getAttribute('data-lc-dm-order'), 10);
    // Karty wracają na swoje miejsce w wyjściowej kolejności puli.
    var after = Array.prototype.slice.call(p.children).filter(function (other) {
      return parseInt(other.getAttribute('data-lc-dm-order'), 10) > order;
    })[0];
    p.insertBefore(card, after || null);
    card.classList.remove('lc-dm-card-placed');
  }

  function toZone(container, card, zone) {
    var slot = zone.querySelector('.lc-dm-slot');
    if (!slot || !card) return null;

    var displaced = cardIn(zone);
    if (displaced === card) return null;
    if (displaced) toPool(container, displaced);

    slot.appendChild(card);
    card.classList.add('lc-dm-card-placed');
    return displaced;
  }

  function drop(container, card, zone) {
    var displaced = toZone(container, card, zone);
    if (displaced === null && cardIn(zone) !== card) return;

    sync(container);
    announce(
      container,
      'Karta trafiła do pola ' + zoneLabel(zone) + '. ' +
        (displaced ? 'Poprzednia karta wróciła do puli. ' : '') + remaining(container)
    );
  }

  // ---- Tryb klawiatury ---------------------------------------------------
  function clearTargets(container) {
    zones(container).forEach(function (zone) {
      zone.classList.remove('lc-dm-target');
    });
  }

  function highlight(container) {
    var list = zones(container);
    clearTargets(container);
    if (list[TARGET_INDEX]) list[TARGET_INDEX].classList.add('lc-dm-target');
  }

  function lift(card) {
    var container = root(card);
    if (!container) return;

    drop_release();
    LIFTED = card;
    card.classList.add('lc-dm-lifted');
    card.setAttribute('aria-pressed', 'true');

    var list = zones(container);
    var own = card.closest('.lc-dm-zone');
    TARGET_INDEX = own ? Math.max(0, list.indexOf(own)) : 0;
    highlight(container);

    announce(
      container,
      'Karta podniesiona. Strzałkami wybierz pole, Enter upuszcza, Escape anuluje. ' +
        'Wybrane pole: ' + zoneLabel(list[TARGET_INDEX]) + '.'
    );
  }

  function drop_release() {
    if (!LIFTED) return;
    var container = root(LIFTED);
    LIFTED.classList.remove('lc-dm-lifted');
    LIFTED.setAttribute('aria-pressed', 'false');
    if (container) clearTargets(container);
    LIFTED = null;
  }

  // ---- Zdarzenia myszy ---------------------------------------------------
  document.addEventListener('dragstart', function (event) {
    var card = event.target.closest ? event.target.closest('.lc-dm-card') : null;
    if (!card) return;
    drop_release();
    card.classList.add('lc-dm-dragging');
    if (event.dataTransfer) {
      event.dataTransfer.effectAllowed = 'move';
      event.dataTransfer.setData('text/plain', card.getAttribute('data-lc-dm-item'));
    }
  });

  document.addEventListener('dragend', function (event) {
    var card = event.target.closest ? event.target.closest('.lc-dm-card') : null;
    if (card) card.classList.remove('lc-dm-dragging');
    document.querySelectorAll('.lc-dm-over').forEach(function (el) {
      el.classList.remove('lc-dm-over');
    });
  });

  document.addEventListener('dragover', function (event) {
    var target = event.target.closest
      ? event.target.closest('.lc-dm-zone, .lc-dm-pool')
      : null;
    if (!target || !document.querySelector('.lc-dm-dragging')) return;
    event.preventDefault();
    if (event.dataTransfer) event.dataTransfer.dropEffect = 'move';
    target.classList.add('lc-dm-over');
  });

  document.addEventListener('dragleave', function (event) {
    var target = event.target.closest
      ? event.target.closest('.lc-dm-zone, .lc-dm-pool')
      : null;
    if (target && !target.contains(event.relatedTarget)) {
      target.classList.remove('lc-dm-over');
    }
  });

  document.addEventListener('drop', function (event) {
    var target = event.target.closest
      ? event.target.closest('.lc-dm-zone, .lc-dm-pool')
      : null;
    var card = document.querySelector('.lc-dm-dragging');
    if (!target || !card) return;

    event.preventDefault();
    target.classList.remove('lc-dm-over');
    card.classList.remove('lc-dm-dragging');

    var container = root(target);
    if (!container || container !== root(card)) return;

    if (target.classList.contains('lc-dm-pool')) {
      if (card.closest('.lc-dm-zone')) {
        toPool(container, card);
        sync(container);
        announce(container, 'Karta wróciła do puli. ' + remaining(container));
      }
      return;
    }
    drop(container, card, target);
  });

  // ---- Zdarzenia klawiatury ----------------------------------------------
  document.addEventListener('keydown', function (event) {
    var card = event.target.closest ? event.target.closest('.lc-dm-card') : null;
    if (!card) return;
    var container = root(card);
    if (!container) return;

    var key = event.key;

    if (key === 'Enter' || key === ' ' || key === 'Spacebar') {
      event.preventDefault();
      if (LIFTED === card) {
        var target = zones(container)[TARGET_INDEX];
        drop_release();
        if (target) {
          drop(container, card, target);
          card.focus();
        }
      } else {
        lift(card);
      }
      return;
    }

    if (key === 'Escape' || key === 'Esc') {
      if (LIFTED === card) {
        event.preventDefault();
        drop_release();
        announce(container, 'Anulowano. Karta została na swoim miejscu.');
      }
      return;
    }

    if (key === 'Backspace' || key === 'Delete') {
      if (!card.closest('.lc-dm-zone')) return;
      event.preventDefault();
      drop_release();
      toPool(container, card);
      sync(container);
      announce(container, 'Karta wróciła do puli. ' + remaining(container));
      card.focus();
      return;
    }

    if (LIFTED !== card) return;

    var list = zones(container);
    var step = 0;
    if (key === 'ArrowRight' || key === 'ArrowDown') step = 1;
    if (key === 'ArrowLeft' || key === 'ArrowUp') step = -1;
    if (step === 0 || list.length === 0) return;

    event.preventDefault();
    TARGET_INDEX = (TARGET_INDEX + step + list.length) % list.length;
    highlight(container);
    announce(container, 'Wybrane pole: ' + zoneLabel(list[TARGET_INDEX]) + '.');
  });

  document.addEventListener('focusout', function (event) {
    if (LIFTED && event.target === LIFTED) drop_release();
  });

  // ---- Reset --------------------------------------------------------------
  document.addEventListener('click', function (event) {
    var button = event.target.closest ? event.target.closest('[data-lc-dm-reset]') : null;
    if (!button) return;
    var container = root(button);
    if (!container) return;

    event.preventDefault();
    drop_release();
    zones(container).forEach(function (zone) {
      var card = cardIn(zone);
      if (card) toPool(container, card);
    });
    sync(container);
    announce(container, 'Wszystkie karty wróciły do puli.');
  });
})();
