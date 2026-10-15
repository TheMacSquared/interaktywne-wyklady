// Wzór ze znacznikami (lc_formula_annotated w lecture_layout.R): strzałki
// od kolorowych części wzoru do opisów pod wzorem, nad nim albo na prawo.
// Rysowane po złożeniu wzoru przez MathJax i po każdej zmianie rozmiaru
// (także przy pokazaniu rozdziału).
(function() {
  'use strict';

  var SVG_NS = 'http://www.w3.org/2000/svg';

  // MathJax 2 nadaje klasę także ukrytej kopii MathML (dla czytników ekranu);
  // ją pomijamy, a z widocznych bierzemy największy element.
  function partRect(box, k) {
    var best = null, bestArea = 0;
    box.querySelectorAll('.lc-fa-tex .lc-fa-p' + k).forEach(function(el) {
      if (el.closest('.MJX_Assistive_MathML, math')) return;
      var r = el.getBoundingClientRect();
      var area = r.width * r.height;
      if (area > bestArea) { best = r; bestArea = area; }
    });
    return best;
  }

  // Rozkład odcinków na osi: każdy możliwie przy swoim celu `t`, kolejność
  // zostaje, sąsiedzi trzymają odstęp `gap`, całość w [0, max].
  function spread(targets, sizes, gap, max) {
    var pos = [];
    for (var i = 0; i < targets.length; i++) {
      var lo = i === 0 ? 0 : pos[i - 1] + sizes[i - 1] + gap;
      pos.push(Math.max(targets[i] - sizes[i] / 2, lo));
    }
    if (max != null) {
      for (var j = targets.length - 1; j >= 0; j--) {
        var hi = j === targets.length - 1 ? max - sizes[j] : pos[j + 1] - sizes[j] - gap;
        pos[j] = Math.max(0, Math.min(pos[j], hi));
      }
    }
    return pos;
  }

  function addPath(svg, d, color) {
    var path = document.createElementNS(SVG_NS, 'path');
    path.setAttribute('d', d);
    // Zmienna CSS, nie wartość: kolor sam podąża za motywem jasnym/ciemnym.
    path.style.stroke = color;
    svg.appendChild(path);
  }

  // Prosta strzałka z grotem ustawionym wzdłuż linii.
  function addArrow(svg, x1, y1, x2, y2, color) {
    var len = Math.hypot(x2 - x1, y2 - y1);
    if (len < 8) return;
    var ux = (x2 - x1) / len, uy = (y2 - y1) / len;
    addPath(svg, 'M' + x1 + ' ' + y1 + ' L' + x2 + ' ' + y2, color);
    addPath(svg, 'M' + (x2 - 6 * ux - 4 * uy) + ' ' + (y2 - 6 * uy + 4 * ux) +
      ' L' + x2 + ' ' + y2 +
      ' L' + (x2 - 6 * ux + 4 * uy) + ' ' + (y2 - 6 * uy - 4 * ux), color);
  }

  // Rząd opisów pod wzorem (dir = 1) albo nad nim (dir = -1). Każdy opis
  // możliwie pod swoją częścią, więc proste strzałki są prawie pionowe.
  // w: szerokości opisów zmierzone w układzie domyślnym (szerokość tekstu).
  function placeRow(row, notes, parts, w) {
    var rr = row.getBoundingClientRect();
    var em = parseFloat(getComputedStyle(row).fontSize);
    var targets = parts.map(function(pr) {
      return pr ? pr.left + pr.width / 2 - rr.left : rr.width / 2;
    });
    var left = spread(targets, w, 1.2 * em, rr.width);
    notes.forEach(function(note, i) {
      note.style.width = w[i] + 'px';
      note.style.marginLeft = (i === 0 ? left[0] : left[i] - left[i - 1] - w[i - 1]) + 'px';
    });
  }

  function linkRow(svg, notes, parts, br, dir) {
    notes.forEach(function(note, i) {
      var pr = parts[i];
      if (!pr) return;
      var nr = note.getBoundingClientRect();
      var x1 = pr.left + pr.width / 2 - br.left;
      var y1 = (dir > 0 ? pr.bottom : pr.top) - br.top + 5 * dir;
      var x2 = nr.left + nr.width / 2 - br.left;
      var y2 = (dir > 0 ? nr.top : nr.bottom) - br.top - 5 * dir;
      addArrow(svg, x1, y1, x2, y2, 'var(--fa-' + note.getAttribute('data-k') + ')');
    });
  }

  // Opisy na prawo od wzoru, każdy na wysokości swojej części.
  function drawRight(box, svg, notes, parts, br) {
    var col = box.querySelector('.lc-fa-notes:not(.lc-fa-above)');
    var cr = col.getBoundingClientRect();
    var em = parseFloat(getComputedStyle(col).fontSize);
    var sizes = notes.map(function(n) { return n.getBoundingClientRect().height; });
    var targets = parts.map(function(pr) {
      return pr ? pr.top + pr.height / 2 - cr.top : 0;
    });
    var top = spread(targets, sizes, 0.6 * em, null);
    notes.forEach(function(note, i) {
      note.style.marginTop = (i === 0 ? top[0] : top[i] - top[i - 1] - sizes[i - 1]) + 'px';
    });

    notes.forEach(function(note, i) {
      var pr = parts[i];
      if (!pr) return;
      var nr = note.getBoundingClientRect();
      addArrow(svg, pr.right - br.left + 6, pr.top + pr.height / 2 - br.top,
        nr.left - br.left - 6, nr.top + nr.height / 2 - br.top,
        'var(--fa-' + note.getAttribute('data-k') + ')');
    });
  }

  function draw(box) {
    var svg = box.querySelector('.lc-fa-links');
    var notes = Array.prototype.slice.call(box.querySelectorAll('.lc-fa-note'));
    if (!svg || !notes.length) return;
    while (svg.firstChild) svg.removeChild(svg.firstChild);

    var br = box.getBoundingClientRect();
    if (!br.width) return;

    // Najpierw układ domyślny, z niego rozmiary i decyzja o zawijaniu.
    box.classList.remove('is-placed');
    notes.forEach(function(n) { n.style.width = ''; n.style.marginLeft = ''; n.style.marginTop = ''; });

    var right = box.classList.contains('lc-fa-right');
    var rows = Array.prototype.slice.call(box.querySelectorAll('.lc-fa-notes'))
      .filter(function(r) { return r.querySelector('.lc-fa-note'); });
    var wrapped;
    if (right) {
      // Wąski ekran: CSS stawia kolumnę opisów pod wzorem.
      var tr = box.querySelector('.lc-fa-tex').getBoundingClientRect();
      wrapped = rows[0].getBoundingClientRect().top >= tr.bottom - 2;
    } else {
      // Któryś rząd opisów zawinięty: strzałki by je przecinały.
      wrapped = rows.some(function(r) {
        var ns = Array.prototype.slice.call(r.querySelectorAll('.lc-fa-note'));
        return ns.some(function(n) { return Math.abs(n.offsetTop - ns[0].offsetTop) > 2; });
      });
    }
    box.classList.toggle('is-wrapped', wrapped);
    if (wrapped) return;

    // Szerokość tekstu każdego opisu w układzie domyślnym (równe kolumny),
    // najwyżej 10.5em; krótki opis może wtedy stanąć tuż pod swoją częścią.
    var widths = rows.map(function(r) {
      var em = parseFloat(getComputedStyle(r).fontSize);
      return Array.prototype.map.call(r.querySelectorAll('.lc-fa-note'), function(n) {
        var range = document.createRange();
        range.selectNodeContents(n);
        return Math.min(Math.ceil(range.getBoundingClientRect().width) + 2, 10.5 * em);
      });
    });
    box.classList.add('is-placed');
    var noteSets = rows.map(function(r) {
      return Array.prototype.slice.call(r.querySelectorAll('.lc-fa-note'));
    });
    var partsOf = function(ns) {
      return ns.map(function(n) { return partRect(box, n.getAttribute('data-k')); });
    };
    if (right) {
      drawRight(box, svg, noteSets[0], partsOf(noteSets[0]), br);
      return;
    }
    // Najpierw ułożenie wszystkich rzędów (zmienia wysokości i przesuwa wzór),
    // dopiero potem pomiar części i strzałki.
    rows.forEach(function(r, i) { placeRow(r, noteSets[i], partsOf(noteSets[i]), widths[i]); });
    br = box.getBoundingClientRect();
    rows.forEach(function(r, i) {
      linkRow(svg, noteSets[i], partsOf(noteSets[i]), br,
        r.classList.contains('lc-fa-above') ? -1 : 1);
    });
  }

  var observer = window.ResizeObserver ? new ResizeObserver(function(entries) {
    entries.forEach(function(e) { draw(e.target); });
  }) : null;

  function init() {
    document.querySelectorAll('.lc-fa').forEach(function(box) {
      if (!box.hasAttribute('data-lc-fa')) {
        box.setAttribute('data-lc-fa', '1');
        if (observer) observer.observe(box);
      }
      draw(box);
    });
  }

  function hookMathJax() {
    if (window.MathJax && MathJax.Hub && MathJax.Hub.Register) {
      MathJax.Hub.Register.MessageHook('End Process', function() { init(); });
      MathJax.Hub.Queue(init);
    } else {
      setTimeout(hookMathJax, 200);
    }
  }

  document.addEventListener('DOMContentLoaded', function() {
    init();
    hookMathJax();
  });
  window.addEventListener('resize', function() {
    document.querySelectorAll('.lc-fa').forEach(draw);
  });
})();
