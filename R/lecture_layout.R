# lecture_layout.R
# Shared layout functions dla interaktywnych wykładów.
# Zastępuje navbarPage() w apkach używających nowego designu.
# Używanie: source(file.path(project_root, "R", "lecture_layout.R"), local=TRUE)

# Przechwytuje project_root w momencie source() — parent.frame() to środowisko
# skryptu wywołującego, gdzie project_root jest zdefiniowane.
.LC_PROJ_ROOT <- local({
  env <- parent.frame(2)  # 2: source() → eval() → tu
  if (exists("project_root", envir = env, inherits = FALSE))
    get("project_root", envir = env)
  else {
    # Fallback: lokalizacja tego pliku → ../..
    ofile <- tryCatch(normalizePath(sys.frame(sys.nframe())$ofile),
                      error = function(e) NULL)
    if (!is.null(ofile)) dirname(dirname(ofile)) else getwd()
  }
})

# ============================================================================
# KONFIGURACJA KURSÓW (moduły i wykłady — top nav)
# ============================================================================

.LC_MODULES <- list(
  list(num = "I",   slug = "opisowa",    title = "Statystyka opisowa",        href = "#"),
  list(num = "II",  slug = "rozklady",   title = "Rozkłady prawdopodobieństwa", href = "#"),
  list(num = "III", slug = "wnioskowanie", title = "Wnioskowanie",             href = "#"),
  list(num = "IV",  slug = "regresja",   title = "Regresja",                   href = "#")
)

# Mapowanie lecture_id → slug modułu
.LC_LECTURE_MODULE <- list(
  "typy-danych"                 = "opisowa",
  "rozklady-prawdopodobienstwa" = "rozklady",
  "przedzialy-ufnosci"          = "wnioskowanie",
  "wnioskowanie-statystyczne"   = "wnioskowanie",
  "zalozenia-testow"            = "wnioskowanie",
  "regresja"                    = "regresja"
)

# ============================================================================
# module_tabs() — górny pasek zakładek modułów
# ============================================================================

module_tabs <- function(current_slug = NULL) {
  logo <- tags$a(
    class = "lc-tabs-logo",
    href  = "#",
    tags$div(class = "lc-tabs-logo-mark", "Σ"),
    tags$div(
      class = "lc-tabs-logo-text",
      tags$div(class = "lc-tabs-logo-title", "Statystyka"),
      tags$div(class = "lc-tabs-logo-sub",   "Skrypt · 25/26")
    )
  )

  tabs <- lapply(.LC_MODULES, function(m) {
    is_active <- !is.null(current_slug) && identical(m$slug, current_slug)
    tags$a(
      class = paste("lc-tab", if (is_active) "lc-tab-active"),
      href  = m$href,
      tags$span(class = "lc-tab-num", m$num),
      m$title
    )
  })

  tags$nav(class = "lc-tabs", logo, tags$div(style="display:flex;", tabs))
}

# ============================================================================
# lecture_chapter() — definicja jednego rozdziału
# Zwraca list(id, num, title, duration, content)
# ============================================================================

lecture_chapter <- function(id, num, title, content, duration = NULL) {
  list(id = id, num = num, title = title, duration = duration,
       content = content)
}

# ============================================================================
# lecture_page() — główna funkcja layoutu; zastępuje navbarPage()
#
# Argumenty:
#   lecture_id     — slug apki, np. "typy-danych" (do wyboru aktywnej zakładki)
#   lecture_num    — numer wykładu do wyświetlenia, np. "01"
#   lecture_title  — tytuł wyświetlany w sidebarze
#   module_label   — etykieta modułu w sidebarze, np. "Moduł I"
#   chapters       — lista list() z lecture_chapter() lub starym tabPanel-em
#   header_extras  — tagList z app-specyficznym JS/CSS (Chart.js itp.)
# ============================================================================

lecture_page <- function(lecture_id      = NULL,
                         lecture_num     = "",
                         lecture_title   = "",
                         module_label    = "",
                         chapters        = list(),
                         header_extras   = NULL) {

  # Ustal aktywny moduł na podstawie lecture_id
  current_module <- if (!is.null(lecture_id))
    .LC_LECTURE_MODULE[[lecture_id]] else NULL

  proj_root <- .LC_PROJ_ROOT

  # Buduj sidebar: lista rozdziałów (przyciski jako Shiny action-button)
  nav_chapters <- lapply(seq_along(chapters), function(i) {
    ch <- chapters[[i]]

    # Obsługa starego formatu tabPanel (Shiny tag list)
    if (inherits(ch, "shiny.tag") || inherits(ch, "shiny.tag.list")) {
      ch <- list(id = paste0("chapter-", i),
                 num = sprintf("%02d", i),
                 title = attr(ch, "attribs")[["data-value"]] %||%
                   paste("Rozdział", i),
                 content = ch)
    }

    tags$div(
      class = "lc-nav-chapter",
      `data-lc-chapter` = ch$id,
      # action-button bez Bootstrap .btn — Shiny obserwuje kliknięcia
      tags$button(
        id    = paste0("lc__nav_", i),
        class = "lc-nav-chapter-btn action-button",
        type  = "button",
        tags$div(
          class = "lc-nav-chapter-inner",
          tags$span(class = "lc-nav-chapter-num",   ch$num),
          tags$span(class = "lc-nav-chapter-title",  ch$title)
        ),
        if (!is.null(ch$duration))
          tags$div(class = "lc-nav-chapter-dur", ch$duration)
      ),
      # TOC — wypełniany przez JS po wczytaniu rozdziału
      tags$ul(class = "lc-nav-toc", `data-lc-toc-for` = ch$id)
    )
  })

  bootstrapPage(
    # ---- HEAD ----
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(
        rel  = "stylesheet",
        href = paste0(
          "https://fonts.googleapis.com/css2?",
          "family=Source+Serif+4:ital,opsz,wght@",
          "0,8..60,400;0,8..60,600;0,8..60,700;1,8..60,400&",
          "family=Atkinson+Hyperlegible:ital,wght@0,400;0,700;1,400&",
          "family=JetBrains+Mono:wght@400;500;600&",
          "display=swap&subset=latin-ext"
        )
      ),
      withMathJax(),
      includeCSS(file.path(proj_root, "R", "shared_styles.css")),
      includeScript(file.path(proj_root, "R", "shared_toc.js")),
      header_extras
    ),

    # ---- SHELL ----
    tags$div(
      class = "lc-shell",

      # --- Górny pasek ---
      module_tabs(current_module),

      # --- Body: sidebar + main ---
      tags$div(
        class = "lc-body",

        # Lewy sidebar
        tags$aside(
          class = "lc-nav",
          id    = "lc-sidebar",
          if (nchar(module_label) > 0)
            tags$div(class = "lc-nav-module-label", module_label),
          if (nchar(lecture_title) > 0)
            tags$div(class = "lc-nav-module-title", lecture_title),
          nav_chapters
        ),

        # Główna treść — jeden rozdział na raz (renderUI po stronie serwera)
        tags$main(
          id    = "lc-main",
          class = "lc-main",
          uiOutput("lc__chapter_content")
        )
      )
    )
  )
}

# ============================================================================
# lecture_server() — zarządza przełączaniem rozdziałów po stronie serwera
#
# Wywołaj w server(): lc <- lecture_server(chapters, input, output, session)
# Nawigacja z poziomu app.R:  lc$switch_to("ch-id")
# Nawigacja z modułów ch*:    session$sendCustomMessage("switchToChapter", "ch-id")
# ============================================================================

lecture_server <- function(chapters, input, output, session) {

  # Normalizuj do listy (na wypadek starych tabPanel-ów)
  chs <- lapply(seq_along(chapters), function(i) {
    ch <- chapters[[i]]
    if (inherits(ch, "shiny.tag") || inherits(ch, "shiny.tag.list")) {
      list(id = paste0("chapter-", i),
           num = sprintf("%02d", i),
           title = paste("Rozdział", i),
           content = ch)
    } else {
      ch
    }
  })

  lc_idx <- reactiveVal(1)

  # Kliknięcia przycisków sidebara
  lapply(seq_along(chs), function(i) {
    observeEvent(input[[paste0("lc__nav_", i)]], {
      lc_idx(i)
    }, ignoreInit = TRUE)
  })

  # switchToChapter z JS (session$sendCustomMessage z modułów rozdziałów)
  observeEvent(input$lc__switch_chapter, {
    chapter_id <- input$lc__switch_chapter
    idx <- which(vapply(chs, function(ch) ch$id == chapter_id, logical(1)))
    if (length(idx) > 0) lc_idx(idx[[1]])
  }, ignoreInit = TRUE)

  # Renderuj aktywny rozdział
  output$lc__chapter_content <- renderUI({
    idx <- lc_idx()
    ch  <- chs[[idx]]
    tags$section(
      id    = ch$id,
      class = "lc-chapter lc-content-wrap",
      `data-lc-chapter-content` = "true",
      ch$content
    )
  })

  # Powiadamiaj JS o zmianie aktywnego rozdziału (setActiveChapter → sidebar + TOC)
  observe({
    idx <- lc_idx()
    ch  <- chs[[idx]]
    session$sendCustomMessage("setActiveChapter", ch$id)
  })

  # Zwracany kontroler — do użycia w app.R
  list(
    switch_to = function(chapter_id) {
      idx <- which(vapply(chs, function(ch) ch$id == chapter_id, logical(1)))
      if (length(idx) > 0) lc_idx(idx[[1]])
    }
  )
}

# ============================================================================
# Komponenty treści — nowe, do używania w rozdziałach
# ============================================================================

# Nagłówek sekcji §N — używaj wewnątrz rozdziałów
lc_h2 <- function(id, num, title) {
  tags$h2(
    id    = id,
    class = "lc-h2",
    `data-lc-section` = id,
    `data-lc-section-num`   = num,
    `data-lc-section-title` = title,
    tags$span(class = "lc-h2-kicker", paste0("§ ", num)),
    title
  )
}

# Podsekcja
lc_h3 <- function(title) {
  tags$h3(class = "lc-h3", title)
}

# Akapit narracyjny (pełna typografia)
lc_p <- function(..., drop = NULL) {
  content <- list(...)
  if (!is.null(drop)) {
    content <- c(list(tags$span(class = "lc-drop", drop)), content)
  }
  tags$p(class = "lc-p", content)
}

# Callout na prawym marginesie
# color: "uwaga" (burgundy), "wskazowka" (gold, domyślny), "ok" (sage)
margin_callout <- function(label = "Zapamiętaj", ..., color = "wskazowka") {
  css_class <- paste("lc-margin-callout", switch(color,
    uwaga     = "lc-callout-uwaga",
    ok        = "lc-callout-ok",
    wskazowka = "",
    ""
  ))
  tags$div(
    class = paste("lc-margin", css_class),
    tags$div(class = "lc-margin-callout-label", label),
    tags$div(class = "lc-margin-callout-body", ...)
  )
}

# Notka na marginesie (bez etykiety)
margin_note <- function(...) {
  tags$div(
    class = "lc-margin",
    tags$div(
      style = "font-family:var(--lc-serif);font-size:13px;line-height:1.5;color:var(--lc-ink-faded);font-style:italic;",
      ...
    )
  )
}

# Ramka z plakietką (wykresy, ćwiczenia, ściągi)
figure_panel <- function(label, ..., title = NULL, color = "#6b1a26",
                          full_width = TRUE) {
  outer_class <- if (full_width) "lc-figure-panel lc-full" else "lc-figure-panel"
  tags$div(
    class = outer_class,
    tags$div(
      class = "lc-figure-panel-badge",
      style = paste0("background:", color, ";"),
      label
    ),
    if (!is.null(title))
      tags$div(class = "lc-figure-panel-title", title),
    ...
  )
}

# Wrapper dla całej siatki treści rozdziału (tekst + prawy margines)
lc_grid <- function(...) {
  tags$div(class = "lc-grid", ...)
}

# Helper: NULL coalescing
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
