// lecture_nav.js
// Per-chapter navigation: setActiveChapter + TOC discovery + within-chapter scroll-spy

(function () {
  "use strict";

  var sidebar = null;
  var main    = null;
  var scrollSpyChapterId = null;
  var scrollTicking = false;

  // -------------------------------------------------------------------------
  // Init — ustawia scroll-spy na #lc-main
  // -------------------------------------------------------------------------
  function init() {
    sidebar = document.getElementById("lc-sidebar");
    main    = document.getElementById("lc-main");
    if (!main) return;

    main.addEventListener("scroll", function () {
      if (!scrollTicking) {
        window.requestAnimationFrame(function () {
          updateScrollSpy();
          scrollTicking = false;
        });
        scrollTicking = true;
      }
    });
  }

  // -------------------------------------------------------------------------
  // setActiveChapter — aktualizuje sidebar (aktywny przycisk), resetuje TOC
  // -------------------------------------------------------------------------
  function setActiveChapter(chapterId) {
    if (!sidebar) {
      sidebar = document.getElementById("lc-sidebar");
    }
    if (!main) {
      main = document.getElementById("lc-main");
    }

    // Scroll do góry przy zmianie rozdziału
    if (main) main.scrollTop = 0;

    if (!sidebar) return;

    var activeIdx = 0;
    var total = 0;
    var navChapters = sidebar.querySelectorAll("[data-lc-chapter]");
    navChapters.forEach(function (navCh, idx) {
      total += 1;
      var chId  = navCh.getAttribute("data-lc-chapter");
      var btn   = navCh.querySelector(".lc-nav-chapter-btn");
      var toc   = navCh.querySelector(".lc-nav-toc");
      var isAct = (chId === chapterId);

      if (isAct) activeIdx = idx + 1;
      if (btn) btn.classList.toggle("lc-active", isAct);
      // TOC zawsze zamykamy — buildTOC otworzy właściwy po wczytaniu DOM
      if (toc) {
        toc.classList.remove("lc-toc-open");
        toc.innerHTML = "";
      }
    });

    // Update progress bar (X/N + wypełnienie)
    if (total > 0 && activeIdx > 0) {
      var fill = document.getElementById("lc-nav-progress-fill");
      var current = document.getElementById("lc-nav-progress-current");
      if (fill) fill.style.width = (activeIdx / total * 100) + "%";
      if (current) current.textContent = String(activeIdx);
    }

    scrollSpyChapterId = null; // zatrzymaj poprzedni scroll-spy
  }

  // -------------------------------------------------------------------------
  // buildTOC — odkrywa h2 w aktywnym rozdziale i wypełnia TOC w sidebarze
  // Wywoływane z opóźnieniem po renderUI Shiny (DOM musi być gotowy)
  // -------------------------------------------------------------------------
  function buildTOC(chapterId) {
    if (!sidebar) sidebar = document.getElementById("lc-sidebar");
    if (!main)    main    = document.getElementById("lc-main");
    if (!sidebar || !main) return;

    var tocList = sidebar.querySelector("[data-lc-toc-for='" + chapterId + "']");
    if (!tocList) return;

    // Szukaj sekcji rozdziału w #lc-main (renderUI może wstawić wrapper div)
    var section = main.querySelector("[data-lc-chapter-content]");

    var headings = section
      ? Array.from(section.querySelectorAll(
          "h2[data-lc-section], h2.lc-h2[id]"
        ))
      : [];

    headings.forEach(function (h) {
      var secId    = h.getAttribute("data-lc-section") || h.id;
      var secNum   = h.getAttribute("data-lc-section-num")   || "";
      var secTitle = h.getAttribute("data-lc-section-title") || h.textContent.trim();

      var li = document.createElement("li");
      var a  = document.createElement("a");
      a.href = "#" + secId;
      a.setAttribute("data-lc-sec-target", secId);

      if (secNum) {
        var numSpan = document.createElement("span");
        numSpan.className = "lc-nav-toc-num";
        numSpan.textContent = "§ " + secNum;
        a.appendChild(numSpan);
      }
      a.appendChild(document.createTextNode(secTitle));

      a.addEventListener("click", function (e) {
        e.preventDefault();
        var target = document.getElementById(secId);
        if (target) target.scrollIntoView({ behavior: "smooth", block: "start" });
      });

      li.appendChild(a);
      tocList.appendChild(li);
    });

    if (headings.length > 0) {
      tocList.classList.add("lc-toc-open");
    }

    scrollSpyChapterId = chapterId;
    updateScrollSpy();
  }

  // -------------------------------------------------------------------------
  // Scroll-spy — podświetla aktywną sekcję w TOC aktywnego rozdziału
  // -------------------------------------------------------------------------
  function updateScrollSpy() {
    if (!main || !sidebar || !scrollSpyChapterId) return;

    var scrollTop = main.scrollTop + 80;
    var section   = main.querySelector("[data-lc-chapter-content]");
    if (!section) return;

    var headings = Array.from(
      section.querySelectorAll(
        "h2[data-lc-section], h2.lc-h2[id]"
      )
    );

    var activeSectionId = null;
    headings.forEach(function (h) {
      if (h.offsetTop <= scrollTop) {
        activeSectionId = h.getAttribute("data-lc-section") || h.id;
      }
    });

    sidebar
      .querySelectorAll("[data-lc-toc-for='" + scrollSpyChapterId + "'] a")
      .forEach(function (a) {
        a.classList.toggle(
          "lc-active",
          a.getAttribute("data-lc-sec-target") === activeSectionId
        );
      });
  }

  // -------------------------------------------------------------------------
  // Shiny message handlers
  // -------------------------------------------------------------------------
  function setupShinyHandlers() {
    // Serwer → klient: przełącz aktywny rozdział i po 250ms zbuduj TOC
    Shiny.addCustomMessageHandler("setActiveChapter", function (chapterId) {
      setActiveChapter(chapterId);
      setTimeout(function () { buildTOC(chapterId); }, 250);
    });

    // Serwer → klient: wymuś przełączenie rozdziału przez Shiny input
    // (używane przez session$sendCustomMessage("switchToChapter", id) w modułach)
    Shiny.addCustomMessageHandler("switchToChapter", function (chapterId) {
      Shiny.setInputValue("lc__switch_chapter", chapterId, { priority: "event" });
    });
  }

  // -------------------------------------------------------------------------
  // Start
  // -------------------------------------------------------------------------
  if (document.readyState === "complete" || document.readyState === "interactive") {
    init();
  } else {
    document.addEventListener("DOMContentLoaded", init);
  }

  // Shiny.addCustomMessageHandler można wywołać przed inicjalizacją Shiny
  if (window.Shiny) {
    setupShinyHandlers();
  } else {
    document.addEventListener("DOMContentLoaded", function () {
      if (window.Shiny) {
        setupShinyHandlers();
      } else {
        $(document).one("shiny:idle", setupShinyHandlers);
      }
    });
  }

  // ===========================================================================
  // Image zoom — kliknięcie w <img> wewnątrz .lc-figure-panel powiększa
  // obrazek na overlayu. ESC lub klik w tło zamyka.
  // CSS dla .lc-zoom-overlay i .lc-figure-panel img w shared_styles.css.
  // ===========================================================================

  var zoomOverlay = null;
  var zoomImg = null;

  function ensureZoomOverlay() {
    if (zoomOverlay) return zoomOverlay;
    zoomOverlay = document.createElement("div");
    zoomOverlay.className = "lc-zoom-overlay";
    zoomOverlay.setAttribute("role", "dialog");
    zoomOverlay.setAttribute("aria-modal", "true");
    zoomOverlay.setAttribute("aria-label", "Powiększony obrazek");
    zoomImg = document.createElement("img");
    zoomImg.alt = "";
    zoomOverlay.appendChild(zoomImg);
    document.body.appendChild(zoomOverlay);
    zoomOverlay.addEventListener("click", function (e) {
      // Klik gdziekolwiek w overlay zamyka — także w samym obrazku.
      // (Klikalna powierzchnia obrazka i tak nie ma żadnej innej akcji.)
      closeZoom();
    });
    return zoomOverlay;
  }

  function openZoom(src, alt) {
    var overlay = ensureZoomOverlay();
    zoomImg.src = src;
    zoomImg.alt = alt || "";
    // Force reflow przed dodaniem klasy, żeby transition zadziałało
    void overlay.offsetWidth;
    overlay.classList.add("is-open");
    document.body.style.overflow = "hidden";
  }

  function closeZoom() {
    if (!zoomOverlay) return;
    zoomOverlay.classList.remove("is-open");
    document.body.style.overflow = "";
  }

  // Event delegation — łapiemy kliki tylko w statyczne <img> które są
  // bezpośrednim dzieckiem .lc-figure-panel, lub explicit opt-in przez
  // klasę .lc-zoomable. Pomijamy obrazki w plotOutput (Shiny) bo niektóre
  // mają zarejestrowany click handler.
  document.addEventListener("click", function (e) {
    var img = e.target.closest(
      ".lc-figure-panel > img, .lc-figure-panel .lc-zoomable img"
    );
    if (!img) return;
    openZoom(img.currentSrc || img.src, img.alt);
  });

  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape" && zoomOverlay && zoomOverlay.classList.contains("is-open")) {
      closeZoom();
    }
  });

})();

// ============================================================================
// Plot fullscreen — natywny Fullscreen API dla lc_plot_fullscreen()
// ============================================================================
(function () {
  "use strict";

  function fullscreenElement() {
    return document.fullscreenElement ||
      document.webkitFullscreenElement ||
      document.mozFullScreenElement ||
      document.msFullscreenElement;
  }

  function requestFullscreen(el) {
    if (el.requestFullscreen) return el.requestFullscreen();
    if (el.webkitRequestFullscreen) return el.webkitRequestFullscreen();
    if (el.mozRequestFullScreen) return el.mozRequestFullScreen();
    if (el.msRequestFullscreen) return el.msRequestFullscreen();
    return null;
  }

  function exitFullscreen() {
    if (document.exitFullscreen) return document.exitFullscreen();
    if (document.webkitExitFullscreen) return document.webkitExitFullscreen();
    if (document.mozCancelFullScreen) return document.mozCancelFullScreen();
    if (document.msExitFullscreen) return document.msExitFullscreen();
    return null;
  }

  document.addEventListener("click", function (event) {
    var button = event.target.closest("[data-lc-fullscreen-toggle]");
    if (!button) return;

    var wrap = button.closest(".lc-plot-fullscreen-wrap");
    if (!wrap) return;

    event.preventDefault();

    if (fullscreenElement()) {
      exitFullscreen();
    } else {
      requestFullscreen(wrap);
    }
  });

  document.addEventListener("fullscreenchange", function () {
    document.querySelectorAll("[data-lc-fullscreen-toggle]").forEach(function (button) {
      var active = !!fullscreenElement();
      button.setAttribute("aria-pressed", active ? "true" : "false");
      button.setAttribute("title", active ? "Zamknij pełny ekran" : "Pełny ekran");
      button.setAttribute("aria-label", active ? "Zamknij pełny ekran" : "Pełny ekran");
    });
  });
})();

// ============================================================================
// Theme switcher — sidebar light/dark, persisted per browser.
// ============================================================================
(function () {
  "use strict";

  var storageKey = "lc-theme";
  var allowed = { light: true, dark: true };

  function getStoredTheme() {
    try {
      return window.localStorage.getItem(storageKey);
    } catch (e) {
      return null;
    }
  }

  function storeTheme(theme) {
    try {
      window.localStorage.setItem(storageKey, theme);
    } catch (e) {
      // localStorage can be unavailable in restrictive browser settings.
    }
  }

  function setTheme(theme, persist) {
    if (!allowed[theme]) theme = "light";
    document.documentElement.setAttribute("data-lc-theme", theme);

    document.querySelectorAll(".lc-theme-option[data-lc-theme]").forEach(function (button) {
      var isActive = button.getAttribute("data-lc-theme") === theme;
      button.classList.toggle("lc-active", isActive);
      button.setAttribute("aria-pressed", isActive ? "true" : "false");
    });

    if (persist) storeTheme(theme);
  }

  function initThemeSwitcher() {
    setTheme(getStoredTheme() || "light", false);

    document.addEventListener("click", function (event) {
      var button = event.target.closest(".lc-theme-option[data-lc-theme]");
      if (!button) return;
      setTheme(button.getAttribute("data-lc-theme"), true);
    });
  }

  if (document.readyState === "complete" || document.readyState === "interactive") {
    initThemeSwitcher();
  } else {
    document.addEventListener("DOMContentLoaded", initThemeSwitcher);
  }
})();

// ============================================================================
// Font size switcher — sidebar S/M/L, persisted per browser.
// ============================================================================
(function () {
  "use strict";

  var storageKey = "lc-font-size";
  var allowed = { small: true, medium: true, large: true };

  function getStoredSize() {
    try {
      return window.localStorage.getItem(storageKey);
    } catch (e) {
      return null;
    }
  }

  function storeSize(size) {
    try {
      window.localStorage.setItem(storageKey, size);
    } catch (e) {
      // localStorage can be unavailable in restrictive browser settings.
    }
  }

  function setSize(size, persist) {
    if (!allowed[size]) size = "medium";
    document.documentElement.setAttribute("data-lc-font-size", size);

    document.querySelectorAll(".lc-font-size-option[data-lc-font-size]").forEach(function (button) {
      var isActive = button.getAttribute("data-lc-font-size") === size;
      button.classList.toggle("lc-active", isActive);
      button.setAttribute("aria-pressed", isActive ? "true" : "false");
    });

    if (persist) storeSize(size);
  }

  function initFontSizeSwitcher() {
    setSize(getStoredSize() || "medium", false);

    document.addEventListener("click", function (event) {
      var button = event.target.closest(".lc-font-size-option[data-lc-font-size]");
      if (!button) return;
      setSize(button.getAttribute("data-lc-font-size"), true);
    });
  }

  if (document.readyState === "complete" || document.readyState === "interactive") {
    initFontSizeSwitcher();
  } else {
    document.addEventListener("DOMContentLoaded", initFontSizeSwitcher);
  }
})();

// ============================================================================
// Glossary popups — obsługa .lc-gloss
// ============================================================================
(function () {
  "use strict";

  var popup = null;
  var activeSpan = null;

  function ensurePopup() {
    if (popup) return popup;
    popup = document.createElement("div");
    popup.className = "lc-gloss-popup";
    popup.innerHTML =
      '<div class="lc-gloss-term"></div><div class="lc-gloss-def"></div>';
    document.body.appendChild(popup);
    return popup;
  }

  function showGloss(span) {
    var p = ensurePopup();
    p.querySelector(".lc-gloss-term").textContent = span.textContent;
    p.querySelector(".lc-gloss-def").textContent = span.dataset.def || "";
    activeSpan = span;

    // Pozycjonowanie: poniżej terminu, nie wychodź za prawy/dolny brzeg
    var rect = span.getBoundingClientRect();
    var top  = rect.bottom + 6;
    var left = rect.left;
    var vpW  = window.innerWidth;
    var vpH  = window.innerHeight;

    // Wymuś display:block żeby zmierzyć wymiary popupu
    p.style.visibility = "hidden";
    p.style.display = "block";
    var pw = p.offsetWidth;
    var ph = p.offsetHeight;
    p.style.display = "";
    p.style.visibility = "";

    if (left + pw > vpW - 8) left = vpW - pw - 8;
    if (top  + ph > vpH - 8) top  = rect.top - ph - 6;
    left = Math.max(8, left);

    p.style.top  = top  + "px";
    p.style.left = left + "px";
    p.classList.add("is-visible");
  }

  function hideGloss() {
    if (popup) popup.classList.remove("is-visible");
    activeSpan = null;
  }

  document.addEventListener("click", function (e) {
    var span = e.target.closest(".lc-gloss");
    if (span) {
      e.stopPropagation();
      if (activeSpan === span) {
        hideGloss();
      } else {
        showGloss(span);
      }
    } else {
      hideGloss();
    }
  });

  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") hideGloss();
  });

})();
