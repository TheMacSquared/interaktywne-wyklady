// Wstrzykiwany do działającej aplikacji Shiny (przez chromote) przed zrzutem.
// Przełącza rozdziały, czeka na wykresy, zamraża kontrolki i zwraca HTML.
window.__off = (function () {
  'use strict';

  var EXCLUDE = /ściąga|sciaga|quiz|ćwiczenia|cwiczenia|dzień dobroci/i;
  var store = [];
  var sleep = function (ms) { return new Promise(function (r) { setTimeout(r, ms); }); };

  function busy() {
    if (document.documentElement.classList.contains('shiny-busy')) return true;
    // Ukryte rozdziały (analiza ryzyka) mogą wisieć w stanie .recalculating.
    return Array.prototype.some.call(document.querySelectorAll('.recalculating'),
      function (e) { return e.offsetParent !== null; });
  }

  async function idle(quiet, timeout) {
    quiet = quiet || 1200; timeout = timeout || 120000;
    var t0 = Date.now(), since = null;
    while (Date.now() - t0 < timeout) {
      if (busy()) since = null;
      else {
        if (since === null) since = Date.now();
        if (Date.now() - since >= quiet) return true;
      }
      await sleep(150);
    }
    return false;
  }

  function mathDone() {
    if (!window.MathJax || !MathJax.Hub) return Promise.resolve();
    return new Promise(function (r) { MathJax.Hub.Queue(r); });
  }

  function chapters() {
    return Array.prototype.map.call(document.querySelectorAll('.lc-nav-chapter[data-lc-chapter]'), function (el) {
      var id = el.getAttribute('data-lc-chapter');
      var t = el.querySelector('.lc-nav-chapter-title');
      var title = t ? t.textContent.trim() : id;
      return { id: id, title: title, keep: !(EXCLUDE.test(id) || EXCLUDE.test(title)) };
    });
  }

  // Wzory jako samodzielne SVG: plik offline nie potrzebuje MathJax ani jego fontów.
  async function initMath() {
    if (!window.MathJax || !MathJax.Hub) return false;
    MathJax.Hub.Config({ SVG: { useGlobalCache: false } });
    await new Promise(function (r) {
      MathJax.Hub.Queue(['setRenderer', MathJax.Hub, 'SVG'], ['Rerender', MathJax.Hub], r);
    });
    return true;
  }

  // Analiza ryzyka montuje wszystkie rozdziały naraz i ukrywa nieaktywne,
  // więc liczy się tylko widoczna sekcja.
  function currentSection() {
    return Array.prototype.find.call(document.querySelectorAll('section[data-lc-chapter-content]'),
      function (s) { return s.offsetParent !== null; }) || null;
  }

  async function blobToDataURL(blob) {
    return new Promise(function (r) {
      var fr = new FileReader();
      fr.onload = function () { r(fr.result); };
      fr.readAsDataURL(blob);
    });
  }

  async function capture(id) {
    var errors = [];
    Shiny.setInputValue('lc__switch_chapter', id, { priority: 'event' });
    var t0 = Date.now();
    while (!(currentSection() && currentSection().id === id)) {
      if (Date.now() - t0 > 30000) return { ok: false, errors: ['rozdział się nie pojawił'] };
      await sleep(100);
    }
    var sec = currentSection();
    var main = document.getElementById('lc-main');

    // Treść schowana w <details> renderuje się dopiero po rozwinięciu.
    var closed = Array.prototype.slice.call(sec.querySelectorAll('details:not([open])'));
    closed.forEach(function (d) { d.setAttribute('data-off-closed', '1'); d.open = true; });
    await sleep(300);
    window.dispatchEvent(new Event('resize'));

    // Przewinięcie całego rozdziału budzi elementy reagujące na widoczność.
    for (var y = 0; y < main.scrollHeight; y += 700) { main.scrollTop = y; await sleep(80); }
    main.scrollTop = 0;

    await idle();
    await mathDone();
    await sleep(900);  // animacje Chart.js i rysowanie strzałek wzorów
    await idle(600);

    sec.querySelectorAll('.shiny-output-error').forEach(function (e) {
      if (!e.classList.contains('shiny-output-error-validation') && e.textContent.trim())
        errors.push((e.id || '?') + ': ' + e.textContent.trim().slice(0, 160));
    });

    // visNetwork: przerysowanie na dużym płótnie, żeby po powiększeniu dało się czytać.
    var nets = Array.prototype.map.call(sec.querySelectorAll('.visNetwork.html-widget'), function (w) {
      return Array.prototype.find.call(w.querySelectorAll('[id^="graph"]'), function (g) { return g.chart; });
    }).filter(Boolean);
    for (var n = 0; n < nets.length; n++) {
      var el = nets[n], r0 = el.getBoundingClientRect();
      if (!r0.width) continue;
      el.setAttribute('data-off-size', JSON.stringify([el.style.width, el.style.height, r0.width]));
      el.style.width = '2400px'; el.style.height = '1500px';
      el.chart.setSize('2400px', '1500px');
      el.chart.fit({ animation: false });
      el.chart.redraw();
    }
    if (nets.length) await sleep(800);

    // Canvas (Chart.js, visNetwork) → obraz.
    var nCanvas = 0;
    sec.querySelectorAll('canvas').forEach(function (c) {
      if (c.closest('.lc-sc')) return;
      var r = c.getBoundingClientRect();
      if (!r.width || !r.height) return;
      var img = document.createElement('img');
      try { img.src = c.toDataURL('image/png'); } catch (e) { errors.push('canvas: ' + e); return; }
      img.className = 'lc-off-canvas';
      var net = c.closest('[data-off-size]');
      if (net) {
        // Pełna szerokość panelu, klik otwiera powiększenie.
        var sz = JSON.parse(net.getAttribute('data-off-size'));
        net.style.width = sz[0] || '100%'; net.style.height = 'auto';
        net.removeAttribute('data-off-size');
        net.querySelectorAll('.vis-navigation, .vis-manipulation, .vis-edit-mode, .vis-close').forEach(function (x) { x.remove(); });
        net.classList.add('lc-zoomable');
        img.alt = 'Diagram (kliknij, aby powiększyć)';
        img.style.width = '100%';
        img.style.cursor = 'zoom-in';
      } else {
        img.style.width = r.width + 'px';
      }
      img.style.maxWidth = '100%';
      img.style.height = 'auto';
      c.replaceWith(img);
      nCanvas++;
    });

    // Obrazki z serwera aplikacji → data URI.
    var imgs = sec.querySelectorAll('img[src]:not([src^="data:"])');
    for (var i = 0; i < imgs.length; i++) {
      var u = new URL(imgs[i].getAttribute('src'), location.href);
      if (u.origin !== location.origin) continue;
      try { imgs[i].src = await blobToDataURL(await (await fetch(u)).blob()); }
      catch (e) { errors.push('img ' + u.pathname + ': ' + e); }
    }

    // Przycisk powiększenia wykresu otwiera w pliku offline powiększenie po stronie
    // przeglądarki (shared_toc.js, obrazki w .lc-zoomable) zamiast modala z R.
    sec.querySelectorAll('.lc-zoom-plot-wrap').forEach(function (w) {
      w.classList.add('lc-zoomable');
      var b = w.querySelector('.lc-zoom-btn');
      if (b) { b.classList.remove('action-button', 'shiny-bound-input'); b.removeAttribute('id'); }
    });

    // Kontrolki liczone w R nie działają bez serwera: zamrażamy je w stanie domyślnym.
    sec.querySelectorAll('.shiny-bound-input').forEach(function (el) {
      if (el.closest('.lc-sc')) return;
      var box = el.closest('.form-group, .shiny-input-container, .lc-seg, .lc-chips, .lc-step-nav, .lc-drop-match') || el;
      box.classList.add('lc-off-frozen');
      box.setAttribute('aria-disabled', 'true');
      if (box.matches('input, button, select, textarea')) box.setAttribute('disabled', '');
      box.querySelectorAll('input, button, select, textarea').forEach(function (x) { x.setAttribute('disabled', ''); });
    });
    // Klikalne komórki tabel i chipy wysyłają wartość prosto do serwera;
    // zostają czytelne, tylko przestają reagować.
    sec.querySelectorAll('[data-lc-cell-input], [data-lc-chips]').forEach(function (el) {
      if (el.closest('.lc-sc')) return;
      el.classList.add('lc-off-inert');
      if (el.matches('button')) el.setAttribute('disabled', '');
    });
    // Widget krokowy bez sceny JS: kolejne kroki liczy serwer.
    sec.querySelectorAll('.lc-stepper').forEach(function (st) {
      if (st.querySelector('.lc-sc')) return;
      st.querySelectorAll('[data-lc-go], [data-lc-nav]').forEach(function (b) {
        b.classList.add('lc-off-frozen'); b.setAttribute('disabled', '');
      });
    });
    sec.querySelectorAll('.recalculating').forEach(function (e) { e.classList.remove('recalculating'); });

    var clone = sec.cloneNode(true);
    clone.querySelectorAll('details[data-off-closed]').forEach(function (d) {
      d.removeAttribute('open'); d.removeAttribute('data-off-closed');
    });
    // Sceny JS zbudują się od nowa w pliku offline.
    clone.querySelectorAll('.lc-sc').forEach(function (s) {
      s.removeAttribute('data-ready');
      s.querySelectorAll(':scope > svg.lc-sc-svg').forEach(function (x) { x.remove(); });
    });
    clone.querySelectorAll('script[type="application/json"][data-for]').forEach(function (x) { x.remove(); });
    clone.classList.add('lc-off-chapter');
    clone.removeAttribute('data-lc-chapter-content');
    store.push(clone.outerHTML);

    return {
      ok: true, errors: errors, canvas: nCanvas,
      plots: sec.querySelectorAll('.shiny-plot-output img').length,
      frozen: sec.querySelectorAll('.lc-off-frozen').length
    };
  }

  // Fonty ikon (Font Awesome, glyphicons) z arkuszy aplikacji → data URI (tylko woff2).
  async function inlineFonts(css, base) {
    var re = /url\((['"]?)([^'")]+\.woff2)(\?[^'")]*)?\1\)/g, m, urls = {};
    while ((m = re.exec(css))) urls[m[2] + (m[3] || '')] = m[2];
    for (var k in urls) {
      try {
        var data = await blobToDataURL(await (await fetch(new URL(urls[k], base))).blob());
        css = css.split(k).join(data.replace(/^data:[^;]*;/, 'data:font/woff2;'));
      } catch (e) {}
    }
    return css;
  }

  async function head() {
    var out = [];
    var kids = document.head.children;
    for (var i = 0; i < kids.length; i++) {
      var el = kids[i], tag = el.tagName, u;
      if (tag === 'LINK' && /stylesheet/i.test(el.rel)) {
        u = new URL(el.href, location.href);
        if (u.origin === location.origin)
          out.push({ kind: 'style', src: u.pathname, text: await inlineFonts(await (await fetch(u)).text(), u) });
        else out.push({ kind: 'skip', src: u.href });
      } else if (tag === 'SCRIPT') {
        if (el.src) {
          u = new URL(el.src, location.href);
          if (u.origin === location.origin)
            out.push({ kind: 'script-src', src: u.pathname, text: await (await fetch(u)).text() });
          else out.push({ kind: 'skip', src: u.href });
        } else if (el.type && !/javascript/i.test(el.type)) {
          out.push({ kind: 'skip', src: 'inline ' + el.type });
        } else {
          out.push({ kind: 'script', src: 'inline', text: el.textContent });
        }
      } else if (tag === 'STYLE') {
        out.push({ kind: 'style', src: el.id || 'inline', text: el.textContent });
      }
    }
    return out;
  }

  // Szkielet strony: pasek modułów, sidebar z rozdziałami, wszystkie rozdziały w <main>.
  function shell(keepIds, tabHrefs) {
    var sh = document.querySelector('.lc-shell').cloneNode(true);
    sh.querySelectorAll('.lc-nav-chapter[data-lc-chapter]').forEach(function (el) {
      if (keepIds.indexOf(el.getAttribute('data-lc-chapter')) < 0) el.remove();
    });
    sh.querySelectorAll('.lc-nav-chapter-btn').forEach(function (b) {
      b.classList.remove('action-button', 'shiny-bound-input');
      b.removeAttribute('id');
    });
    sh.querySelectorAll('.lc-nav-toc').forEach(function (u) { u.innerHTML = ''; });
    var spans = sh.querySelectorAll('.lc-nav-progress-text span');
    if (spans.length > 1) spans[spans.length - 1].textContent = String(keepIds.length);

    // Zakładki modułów → sąsiednie pliki offline.
    ['.lc-tabs-list .lc-tab', '.lc-tabs-menu-panel .lc-tabs-menu-item'].forEach(function (sel) {
      sh.querySelectorAll(sel).forEach(function (a, i) {
        if (tabHrefs[i]) a.setAttribute('href', tabHrefs[i]);
        a.removeAttribute('data-lc-hub-tab');
      });
    });
    var logo = sh.querySelector('.lc-tabs-logo');
    if (logo) { logo.setAttribute('href', 'index.html'); logo.removeAttribute('target'); logo.setAttribute('title', 'Spis wykładów'); }
    sh.querySelectorAll('script').forEach(function (s) { s.remove(); });

    var note = document.createElement('div');
    note.className = 'lc-off-banner';
    note.textContent = 'Wersja offline. Wykresy pokazują ustawienia domyślne, ' +
      'a wyszarzone kontrolki działają tylko w wersji z zajęć.';
    var anchor = sh.querySelector('.lc-nav-module-title') || sh.querySelector('.lc-nav-module-label');
    if (anchor) anchor.insertAdjacentElement('afterend', note);

    var main = sh.querySelector('#lc-main');
    main.innerHTML = store.join('\n');
    main.querySelectorAll('a.lc-next[data-lc-next-target]').forEach(function (a) {
      if (keepIds.indexOf(a.getAttribute('data-lc-next-target')) < 0) a.remove();
    });
    var first = main.querySelector('.lc-off-chapter');
    if (first) first.classList.add('lc-off-active');
    return sh.outerHTML;
  }

  return { chapters: chapters, initMath: initMath, capture: capture, head: head, shell: shell, idle: idle };
})();
true;
