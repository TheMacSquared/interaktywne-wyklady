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

})();
