// Wyróżnienie zapisów prawdopodobieństwa w tekście wykładu.
//
// Każde P(…) w treści rozdziałów dostaje własny styl: zdarzenie, kreska warunku
// i warunek mają osobne kolory, a cały zapis nie łamie się między wierszami.
// Warunek (to, co za kreską) wskazuje mianownik, więc ma stały kolor w całym
// wykładzie. Tekst źródłowy w R pozostaje zwykłym napisem.
//
// Pomijamy SVG, wzory MathJax, ramki wzorów, linijki wyprowadzeń, pola formularzy
// i odczyty liczbowe. Działa w przeglądarce, także dla treści dorysowanej przez
// serwer (MutationObserver).
(function () {
  var PATTERN = /(?<![A-Za-zÀ-ž])P\(([^()|]*?)(?:\s*\|\s*([^()]*?))?\)/g;
  var SKIP = "svg, script, style, textarea, input, select, option, pre, code, " +
    "mjx-container, .MathJax, .lc-formula-box, .lc-read-value, .lc-pm";

  function span(cls, text) {
    var s = document.createElement("span");
    s.className = cls;
    s.textContent = text;
    return s;
  }

  function build(event, cond) {
    var root = span("lc-pm", "");
    root.appendChild(span("lc-pm-p", "P("));
    root.appendChild(span("lc-pm-ev", event.trim()));
    if (cond !== undefined) {
      root.appendChild(span("lc-pm-bar", "|"));
      root.appendChild(span("lc-pm-cond", cond.trim()));
    }
    root.appendChild(span("lc-pm-p", ")"));
    return root;
  }

  function wrap(node) {
    var text = node.nodeValue, last = 0, m, frag = null;
    PATTERN.lastIndex = 0;
    while ((m = PATTERN.exec(text)) !== null) {
      if (!frag) frag = document.createDocumentFragment();
      frag.appendChild(document.createTextNode(text.slice(last, m.index)));
      frag.appendChild(build(m[1], m[2]));
      last = m.index + m[0].length;
    }
    if (!frag) return;
    frag.appendChild(document.createTextNode(text.slice(last)));
    node.parentNode.replaceChild(frag, node);
  }

  function scan() {
    document.querySelectorAll(".lc-chapter").forEach(function (root) {
      var walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT, {
        acceptNode: function (n) {
          if (n.nodeValue.indexOf("P(") < 0) return NodeFilter.FILTER_REJECT;
          return n.parentElement.closest(SKIP) ? NodeFilter.FILTER_REJECT : NodeFilter.FILTER_ACCEPT;
        }
      });
      var nodes = [];
      while (walker.nextNode()) nodes.push(walker.currentNode);
      nodes.forEach(wrap);
    });
  }

  var pending = false;
  function schedule() {
    if (pending) return;
    pending = true;
    requestAnimationFrame(function () { pending = false; scan(); });
  }
  document.addEventListener("DOMContentLoaded", schedule);
  new MutationObserver(schedule).observe(document.documentElement, { childList: true, subtree: true });
  schedule();
})();
