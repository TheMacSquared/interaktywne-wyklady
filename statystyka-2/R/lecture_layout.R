# lecture_layout.R
# Shared layout functions dla interaktywnych wykładów.
# Kanoniczny shell dla wykładów w nowym designie.
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
  list(num = "I",  slug = "symulacje",  title = "Symulacje",                   short = "Symulacje",    href = "#"),
  list(num = "II", slug = "bayes",      title = "Bayes",                       short = "Bayes",        href = "#"),
  list(num = "III",   slug = "kierunkowe", title = "Kierunkowe",                  short = "Kierunkowe",   href = "#"),
  list(num = "IV", slug = "szeregi",    title = "Szeregi czasowe",             short = "Szeregi",      href = "#")
)

# Mapowanie lecture_id → slug modułu
.LC_LECTURE_MODULE <- list(
  "symulacje-statystyczne"      = "symulacje",
  "metody-bayesowskie"          = "bayes",
  "kierunkowe"                  = "kierunkowe",
  "szeregi-czasowe"             = "szeregi"
)

# ============================================================================
# module_tabs() — górny pasek zakładek modułów
# ============================================================================

module_tabs <- function(current_slug = NULL) {
  current_module <- NULL
  if (!is.null(current_slug)) {
    idx <- which(vapply(.LC_MODULES, function(m) identical(m$slug, current_slug), logical(1)))
    if (length(idx) > 0) current_module <- .LC_MODULES[[idx[[1]]]]
  }

  # Gdy wykład uruchomił hub (hub/app.R), logo wraca do spisu wykładów.
  # Bez zmiennej LC_HUB_URL zostaje placeholderem "#", dokładnie jak dotąd, więc
  # ręczne shiny::runApp() zachowuje się bez zmian. Nazwany target sprawia, że
  # klik przełącza na kartę huba, a nie nadpisuje karty wykładu.
  .lc_hub_url <- Sys.getenv("LC_HUB_URL", "")

  logo <- tags$a(
    class  = "lc-tabs-logo",
    href   = if (nzchar(.lc_hub_url)) .lc_hub_url else "#",
    target = if (nzchar(.lc_hub_url)) "wyklady_hub" else NULL,
    title  = if (nzchar(.lc_hub_url)) "Powrót do spisu wykładów" else NULL,
    tags$div(class = "lc-tabs-logo-mark", "Σ"),
    tags$div(
      class = "lc-tabs-logo-text",
      tags$div(class = "lc-tabs-logo-title", "Statystyka 2"),
      tags$div(class = "lc-tabs-logo-sub",   "Skrypt wykładowy")
    )
  )

  tabs <- lapply(.LC_MODULES, function(m) {
    is_active <- !is.null(current_slug) && identical(m$slug, current_slug)
    tags$a(
      class = paste("lc-tab", if (is_active) "lc-tab-active"),
      href  = m$href,
      title = m$title,
      tags$span(class = "lc-tab-num", m$num),
      tags$span(class = "lc-tab-title-full", m$title),
      tags$span(class = "lc-tab-title-short", m$short)
    )
  })

  menu_items <- lapply(.LC_MODULES, function(m) {
    is_active <- !is.null(current_slug) && identical(m$slug, current_slug)
    tags$a(
      class = paste("lc-tabs-menu-item", if (is_active) "lc-tabs-menu-item-active"),
      href  = m$href,
      title = m$title,
      tags$span(class = "lc-tab-num", m$num),
      tags$span(class = "lc-tabs-menu-item-title", m$title)
    )
  })

  current_num <- if (!is.null(current_module)) current_module$num else "—"
  current_title <- if (!is.null(current_module)) current_module$title else "Moduły"

  menu <- tags$details(
    class = "lc-tabs-menu",
    tags$summary(
      class = "lc-tabs-menu-summary",
      tags$span(class = "lc-tab-num", current_num),
      tags$span(class = "lc-tabs-menu-current", current_title),
      tags$span(class = "lc-tabs-menu-caret", "▾")
    ),
    tags$div(class = "lc-tabs-menu-panel", menu_items)
  )

  scroll_controls <- tagList(
    tags$button(
      class = "lc-tabs-scroll lc-tabs-scroll-prev",
      type = "button",
      title = "Poprzedni moduł",
      `aria-label` = "Poprzedni moduł",
      `data-lc-tabs-scroll` = "prev",
      "‹"
    ),
    tags$button(
      class = "lc-tabs-scroll lc-tabs-scroll-next",
      type = "button",
      title = "Następny moduł",
      `aria-label` = "Następny moduł",
      `data-lc-tabs-scroll` = "next",
      "›"
    )
  )

  tags$nav(
    class = "lc-tabs",
    logo,
    tags$div(
      class = "lc-tabs-strip",
      scroll_controls[[1]],
      tags$div(class = "lc-tabs-list", tabs),
      scroll_controls[[2]]
    ),
    menu
  )
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
# lecture_page() — główna funkcja layoutu
#
# Argumenty:
#   lecture_id     — slug apki, np. "typy-danych" (do wyboru aktywnej zakładki)
#   lecture_num    — numer wykładu do wyświetlenia, np. "01"
#   lecture_title  — tytuł wyświetlany w sidebarze
#   module_label   — etykieta modułu w sidebarze, np. "Moduł I"
#   chapters       — lista list() z lecture_chapter()
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

    tags$div(
      class = "lc-nav-chapter",
      `data-lc-chapter` = ch$id,
      # Klasa action-button pozwala Shiny obserwować kliknięcia na własnym znaczniku.
      tags$button(
        id    = paste0("lc__nav_", i),
        class = "lc-nav-chapter-btn action-button",
        type  = "button",
        title = ch$title,  # natywny tooltip kiedy tytuł jest schowany (minimalist sidebar)
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

  # Progress bar: "X/N" pod listą rozdziałów (aktualizowany przez JS)
  n_chapters <- length(chapters)
  progress_block <- tags$div(
    class = "lc-nav-progress",
    tags$div(
      class = "lc-nav-progress-bar",
      tags$div(
        class = "lc-nav-progress-fill",
        id    = "lc-nav-progress-fill",
        style = paste0("width:", round(100 / n_chapters, 2), "%;")
      )
    ),
    tags$div(
      class = "lc-nav-progress-text",
      tags$span(id = "lc-nav-progress-current", "1"),
      " / ",
      tags$span(as.character(n_chapters))
    )
  )

  font_size_control <- tags$div(
    class = "lc-font-size-control",
    `aria-label` = "Rozmiar tekstu",
    tags$div(class = "lc-font-size-label", "Tekst"),
    tags$div(
      class = "lc-font-size-options",
      tags$button(
        class = "lc-font-size-option",
        type = "button",
        `data-lc-font-size` = "small",
        title = "Mały tekst",
        `aria-label` = "Mały tekst",
        "S"
      ),
      tags$button(
        class = "lc-font-size-option",
        type = "button",
        `data-lc-font-size` = "medium",
        title = "Średni tekst",
        `aria-label` = "Średni tekst",
        "M"
      ),
      tags$button(
        class = "lc-font-size-option",
        type = "button",
        `data-lc-font-size` = "large",
        title = "Duży tekst",
        `aria-label` = "Duży tekst",
        "L"
      )
    )
  )

  theme_control <- tags$div(
    class = "lc-theme-control",
    `aria-label` = "Tryb kolorów",
    tags$div(class = "lc-theme-label", "Tryb"),
    tags$div(
      class = "lc-theme-options",
      tags$button(
        class = "lc-theme-option",
        type = "button",
        `data-lc-theme` = "light",
        title = "Jasny tryb",
        `aria-label` = "Jasny tryb",
        "J"
      ),
      tags$button(
        class = "lc-theme-option",
        type = "button",
        `data-lc-theme` = "dark",
        title = "Ciemny tryb",
        `aria-label` = "Ciemny tryb",
        "C"
      )
    )
  )

  bootstrapPage(
    # ---- HEAD ----
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(
        rel  = "stylesheet",
        href = paste0(
          "https://fonts.googleapis.com/css2?",
          "family=IBM+Plex+Sans:ital,wght@0,400;0,500;0,600;0,700;1,400;1,700&",
          "family=IBM+Plex+Mono:wght@400;500;600;700&",
          "display=swap&subset=latin-ext"
        )
      ),
      # Tekst wewnątrz \text{...} dziedziczy krój interfejsu. Domyślny font
      # MathJax nie składa polskich znaków spójnie z resztą wykładu.
      tags$script(
        type = "text/x-mathjax-config",
        HTML("MathJax.Hub.Config({
          'HTML-CSS': { mtextFontInherit: true },
          CommonHTML: { mtextFontInherit: true }
        });")
      ),
      withMathJax(),
      includeCSS(file.path(proj_root, "R", "shared_styles.css")),
      # Paleta UPWr jako CSS custom properties — źródło wartości: R/palette.R.
      # Wstrzyknięte po shared_styles.css, żeby wartości z palette.R miały
      # pierwszeństwo nad wartościami fallbackowymi w CSS.
      .lc_palette_css(),
      tags$script(HTML("
(function() {
  try {
    var size = window.localStorage.getItem('lc-font-size');
    if (size === 'small' || size === 'medium' || size === 'large') {
      document.documentElement.setAttribute('data-lc-font-size', size);
    }
    var theme = window.localStorage.getItem('lc-theme');
    if (theme === 'light' || theme === 'dark') {
      document.documentElement.setAttribute('data-lc-theme', theme);
    }
  } catch (e) {}
})();
      ")),
      includeScript(file.path(proj_root, "R", "shared_toc.js")),
      includeScript(file.path(proj_root, "R", "lc_widgets.js")),
      tags$script(HTML("
function lcUpdateTabsScrollState(list) {
  if (!list) return;
  var strip = list.closest('.lc-tabs-strip');
  if (!strip) return;
  var prev = strip.querySelector('[data-lc-tabs-scroll=\"prev\"]');
  var next = strip.querySelector('[data-lc-tabs-scroll=\"next\"]');
  var maxScroll = Math.max(0, list.scrollWidth - list.clientWidth);
  var atStart = list.scrollLeft <= 1;
  var atEnd = list.scrollLeft >= maxScroll - 1;
  if (prev) prev.disabled = atStart;
  if (next) next.disabled = atEnd;
  strip.classList.toggle('lc-tabs-strip-scrollable', maxScroll > 1);
}

function lcInitTabsScroll() {
  document.querySelectorAll('.lc-tabs-list').forEach(function(list) {
    var active = list.querySelector('.lc-tab-active');
    if (active) {
      active.scrollIntoView({ block: 'nearest', inline: 'center' });
    }
    lcUpdateTabsScrollState(list);
    list.addEventListener('scroll', function() {
      lcUpdateTabsScrollState(list);
    }, { passive: true });
  });
}

document.addEventListener('click', function(event) {
  var button = event.target.closest('[data-lc-tabs-scroll]');
  if (!button || button.disabled) return;

  var strip = button.closest('.lc-tabs-strip');
  if (!strip) return;

  var list = strip.querySelector('.lc-tabs-list');
  var active = list ? list.querySelector('.lc-tab-active') : null;
  var firstTab = list ? list.querySelector('.lc-tab') : null;
  if (!list || !firstTab) return;

  var tabWidth = active ? active.getBoundingClientRect().width : firstTab.getBoundingClientRect().width;
  var gap = 8;
  var direction = button.getAttribute('data-lc-tabs-scroll') === 'prev' ? -1 : 1;
  list.scrollBy({ left: direction * (tabWidth + gap), behavior: 'smooth' });
});

window.addEventListener('resize', function() {
  document.querySelectorAll('.lc-tabs-list').forEach(lcUpdateTabsScrollState);
});

document.addEventListener('DOMContentLoaded', lcInitTabsScroll);
      ")),
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
          nav_chapters,
          progress_block,
          font_size_control,
          theme_control
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

  chs <- chapters

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

# Nagłówek sekcji wykrywany przez TOC. `num` jest opcjonalne: nowe moduły mogą
# używać samego `lc_h2(id, title)`, a starsze wywołania zachowują numerację.
lc_h2 <- function(id, num = NULL, title = NULL) {
  if (is.null(title)) {
    title <- num
    num <- NULL
  }

  num_label <- NULL
  if (!is.null(num) && nchar(as.character(num)) > 0) {
    num_label <- as.character(num)
    if (!grepl("^§\\s*", num_label)) {
      num_label <- paste0("§ ", num_label)
    }
  }

  tags$h2(
    id    = id,
    class = "lc-h2",
    `data-lc-section` = id,
    `data-lc-section-num`   = if (is.null(num)) "" else as.character(num),
    `data-lc-section-title` = title,
    if (!is.null(num_label)) tags$span(class = "lc-h2-kicker", num_label),
    title
  )
}

# Podsekcja z opcjonalnym numerem w mono (zamiast „(1) …” w kursywie).
lc_h3 <- function(title, num = NULL) {
  tags$h3(class = "lc-h3",
    if (!is.null(num)) tags$span(class = "lc-h3-n", num),
    title
  )
}

# Akapit narracyjny (pełna typografia)
lc_p <- function(..., drop = NULL) {
  content <- list(...)
  if (!is.null(drop)) {
    content <- c(list(tags$span(class = "lc-drop", drop)), content)
  }
  tags$p(class = "lc-p", content)
}

# Dawny callout na marginesie. Marginesu nie ma: renderuje się jako lc_note().
# W nowym kodzie używaj lc_note(); `color` jest ignorowany.
margin_callout <- function(label = "Zapamiętaj", ..., color = NULL) {
  lc_note(label, ..., rule = identical(label, "Zasada"))
}

# Dawny zwijany callout. Notki się nie zwijają: renderuje się jako lc_note().
# Rozwinięcia opcjonalne i rozwiązania: lc_more(). `color` i `open` są ignorowane.
inline_callout <- function(label = "Zapamiętaj", ..., color = NULL, open = NULL) {
  lc_note(label, ..., rule = identical(label, "Zasada"))
}

# Dawna notka na marginesie (bez etykiety); renderuje się jako lc_note().
margin_note <- function(...) {
  lc_note("Notka", ...)
}

# Ramka z plakietką (wykresy, ćwiczenia, ściągi)
# Domyślnie: szerokość kolumny tekstu (lepszy kontrast z narracją).
# full_width = TRUE tylko gdy wykres naprawdę potrzebuje pełnej szerokości.
figure_panel <- function(label, ..., title = NULL, color = "#6b1a26",
                          full_width = FALSE) {
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

# Wykres z natywnym trybem pełnoekranowym.
# UI-only wrapper wokół plotOutput(); server używa zwykłego output$... <- renderPlot().
lc_plot_fullscreen <- function(outputId, height = "300px", width = "100%",
                               label = "Pełny ekran", ...) {
  tags$div(
    class = "lc-plot-fullscreen-wrap",
    shiny::plotOutput(outputId, height = height, width = width, ...),
    tags$button(
      class = "lc-plot-fullscreen-btn",
      type = "button",
      title = label,
      `aria-label` = label,
      `data-lc-fullscreen-toggle` = "true",
      HTML("&#x26F6;")
    )
  )
}

# Blok wzoru lub krótkiego zapisu matematycznego.
lc_formula_box <- function(...) {
  tags$div(class = "lc-formula-box", ...)
}

# Pojedyncza metryka/statystyka. `color` steruje lewym akcentem.
lc_stat_box <- function(label, value = NULL, ..., caption = NULL,
                        color = upwr_accent) {
  value_parts <- c(if (!is.null(value)) list(value), list(...))
  tags$div(
    class = "lc-stat-box",
    style = paste0("--lc-stat-color:", color, ";"),
    tags$div(class = "lc-stat-label", label),
    if (length(value_parts) > 0) tags$div(class = "lc-stat-value", value_parts),
    if (!is.null(caption)) tags$div(class = "lc-stat-caption", caption)
  )
}

# Siatka metryk/statystyk.
lc_stat_grid <- function(..., columns = NULL) {
  style <- if (!is.null(columns)) {
    paste0("--lc-stat-cols:", as.integer(columns), ";")
  } else {
    NULL
  }
  tags$div(class = "lc-stat-grid", style = style, ...)
}

# Dynamiczny feedback/status w renderUI(), np. po kliknięciu quizu.
lc_feedback <- function(..., type = c("info", "ok", "warning", "danger"),
                        style = NULL,
                        live = !is.null(shiny::getDefaultReactiveDomain())) {
  type <- match.arg(type)
  tags$div(
    class = paste("lc-feedback", paste0("lc-feedback-", type)),
    role = if (isTRUE(live)) {
      if (identical(type, "danger")) "alert" else "status"
    },
    `aria-live` = if (isTRUE(live)) {
      if (identical(type, "danger")) "assertive" else "polite"
    },
    style = style,
    ...
  )
}

# Wrapper dla całej siatki treści rozdziału (tekst + prawy margines)
lc_grid <- function(...) {
  tags$div(class = "lc-grid", ...)
}

# Małe utility layoutu dla powtarzalnych układów kontrolek i statusów.
lc_stack <- function(..., gap = c("md", "sm")) {
  gap <- match.arg(gap)
  tags$div(class = paste("lc-stack", paste0("lc-stack-", gap)), ...)
}

lc_inline_row <- function(..., gap = c("md", "sm"), align = c("start", "center")) {
  gap <- match.arg(gap)
  align <- match.arg(align)
  tags$div(
    class = paste("lc-inline-row", paste0("lc-inline-row-", gap), paste0("lc-align-", align)),
    ...
  )
}

lc_center <- function(...) {
  tags$div(class = "lc-center", ...)
}

lc_spacer <- function(size = c("md", "lg")) {
  size <- match.arg(size)
  tags$div(class = paste("lc-spacer", paste0("lc-spacer-", size)))
}

# ============================================================================
# lc_chapter_hero() — okładka rozdziału: kicker + duża cyfra + tytuł + squiggle + lead
# Wypełnia istniejący CSS: .lc-chapter-header / .lc-chapter-hero / .lc-chapter-num
#   / .lc-chapter-title / .lc-chapter-kicker / .lc-chapter-lead
# ============================================================================

lc_chapter_hero <- function(kicker = NULL, num, title, lead = NULL) {
  tags$header(
    class = "lc-chapter-header",
    if (!is.null(kicker) && nchar(kicker) > 0)
      tags$div(class = "lc-chapter-kicker", kicker),
    tags$div(
      class = "lc-chapter-hero",
      tags$div(class = "lc-chapter-num", num),
      tags$h1(class = "lc-chapter-title", title)
    ),
    if (!is.null(lead) && (is.list(lead) || nchar(as.character(lead)) > 0))
      tags$p(class = "lc-chapter-lead", lead)
  )
}

# ============================================================================
# margin_code_note() — callout "W kodzie" z blokiem monospace
# ============================================================================

# Kod z krótkim opisem jako notka z etykietą (dawniej na marginesie).
margin_code_note <- function(code, description = NULL, label = "W kodzie") {
  lc_note(label,
    tags$pre(tags$code(code)),
    if (!is.null(description)) tags$p(description)
  )
}

# ============================================================================
# lc_chapter_next() — przejście „Dalej — 02 · Tytuł” jako blok w treści.
# Klika → session$sendCustomMessage("switchToChapter", target_id) przez JS.
# ============================================================================

lc_chapter_next <- function(num, title, lead = NULL, target_id) {
  tags$a(
    class = "lc-next",
    href  = "#",
    `data-lc-next-target` = target_id,
    onclick = paste0(
      "event.preventDefault();",
      "Shiny.setInputValue('lc__switch_chapter', '", target_id,
      "', {priority:'event'});"
    ),
    tags$span(class = "lc-next-l", "Dalej"),
    tags$span(class = "lc-next-t",
      tags$span(class = "lc-next-num", num), title
    ),
    if (!is.null(lead) && nchar(lead) > 0)
      tags$span(class = "lc-next-lead", lead)
  )
}

# ============================================================================
# .lc_palette_css() — generuje blok <style> z tokenami --upwr-* pobieranymi
# z R/palette.R. Wstrzykiwane w tags$head przez lecture_page() po includeCSS.
# Jedno źródło prawdy dla wszystkich kolorów projektu (ggplot + CSS + inline).
#
# Tokeny pochodne (tints/hovers) są wyliczane z palet sekwencyjnych, żeby nie
# duplikować hex-ów w palette.R — paleta ma zostać czysto semantyczna (role),
# a zmienne UI pomocnicze są wariantami tych ról.
# ============================================================================

.lc_palette_css <- function() {
  # Tokeny pochodne — wyliczone z palety sekwencyjnej i kat.
  panel_sunken     <- "#ece6d8"   # ciemniejsza wersja upwr_panel (dla tła wciśniętych elementów UI)
  ink_subtle       <- "#6e665c"   # tekst pomocniczy; kontrast 4,5:1 na jasnym tle
  rule_soft        <- "#e8e1d2"   # jaśniejsza niż upwr_rule (dla miękkich dividerów)
  accent_hover     <- upwr_seq_burgundy[6]   # ciemniejszy burgund na hover
  accent_tint      <- upwr_seq_burgundy[2]   # jasne tło dla callout-uwaga
  alt_tint         <- upwr_seq_gold[2]       # jasne tło dla callout-kod
  sage             <- unname(upwr_cat["szalwia"])
  sage_tint        <- "#dee8de"              # jasne tło dla callout-ok

  css <- sprintf(
    ":root {
  color-scheme: light;
  --upwr-bg:                %s;
  --upwr-panel:             %s;
  --upwr-surface:           #ffffff;
  --upwr-surface-sunken:    %s;
  --upwr-ink:               %s;
  --upwr-ink-soft:          %s;
  --upwr-ink-subtle:        %s;
  --upwr-reference:         %s;
  --upwr-rule:              %s;
  --upwr-rule-soft:         %s;
  --upwr-accent:            %s;
  --upwr-accent-hover:      %s;
  --upwr-accent-tint:       %s;
  --upwr-single-alt:        %s;
  --upwr-single-alt-tint:   %s;
  --upwr-sage:              %s;
  --upwr-sage-tint:         %s;
  --upwr-warning:           #8f5b17;
  --upwr-secondary:         %s;
  --upwr-cat-grafit:        %s;
  --upwr-cat-bursztyn:      %s;
  --upwr-cat-niebo:         %s;
  --upwr-cat-szalwia:       %s;
  --upwr-cat-kurkuma:       %s;
  --upwr-cat-indygo:        %s;
  --upwr-cat-terakota:      %s;
  --upwr-cat-wrzos:         %s;
}

html[data-lc-theme=\"dark\"] {
  color-scheme: dark;
  --upwr-bg:                #161412;
  --upwr-panel:             #201d19;
  --upwr-surface:           #26221d;
  --upwr-surface-sunken:    #1b1815;
  --upwr-ink:               #f7efe4;
  --upwr-ink-soft:          #ded1c2;
  --upwr-ink-subtle:        #8d8276;
  --upwr-reference:         #aa9d90;
  --upwr-rule:              #4a4238;
  --upwr-rule-soft:         #342f29;
  --upwr-accent:            #d98a99;
  --upwr-accent-hover:      #e3a8b2;
  --upwr-accent-tint:       #3f2028;
  --upwr-single-alt:        #d6b15b;
  --upwr-single-alt-tint:   #3a301d;
  --upwr-sage:              #82bf9c;
  --upwr-sage-tint:         #1f3528;
  --upwr-warning:           #e0b072;
  --upwr-secondary:         #b8c7c4;
  --upwr-cat-grafit:        #b8c7c4;
  --upwr-cat-bursztyn:      #d8a35d;
  --upwr-cat-niebo:         #89b9df;
  --upwr-cat-szalwia:       #82bf9c;
  --upwr-cat-kurkuma:       #d9ca6a;
  --upwr-cat-indygo:        #8aa8dc;
  --upwr-cat-terakota:      #d27a59;
  --upwr-cat-wrzos:         #c89ab9;
}",
    upwr_bg, upwr_panel, panel_sunken,
    upwr_ink, upwr_ink_soft, ink_subtle, upwr_reference,
    upwr_rule, rule_soft,
    upwr_accent, accent_hover, accent_tint,
    upwr_single_alt, alt_tint,
    sage, sage_tint,
    upwr_secondary,
    unname(upwr_cat["grafit"]),   unname(upwr_cat["bursztyn"]),
    unname(upwr_cat["niebo"]),    unname(upwr_cat["szalwia"]),
    unname(upwr_cat["kurkuma"]),  unname(upwr_cat["indygo"]),
    unname(upwr_cat["terakota"]), unname(upwr_cat["wrzos"])
  )
  tags$style(HTML(css))
}

# ============================================================================
# WIDGETY V2 I TABELE V2
# Źródło: handoffy „Widgety v2” i „Tabele v2”. Style obejmują każdy
# figure_panel(); tabele lc_table() działają też w toku tekstu.
# Logika klienta (kroki, wartość suwaka, klikalne komórki) jest w
# R/lc_widgets.js. Kod tej sekcji jest identyczny we wszystkich kursach.
# ============================================================================

.lc_icon_paths <- list(
  reset   = '<path d="M3 12a9 9 0 1 0 3-6.7"></path><path d="M3 4v5h5"></path>',
  shuffle = '<path d="M21 12a9 9 0 1 1-3-6.7"></path><path d="M21 4v5h-5"></path>',
  prev    = '<path d="M15 6l-6 6 6 6"></path>',
  `next`  = '<path d="M9 6l6 6-6 6"></path>'
)

lc_icon <- function(name = c("reset", "shuffle", "prev", "next")) {
  name <- match.arg(name)
  HTML(paste0(
    '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" ',
    'stroke-linecap="round" aria-hidden="true" focusable="false">',
    .lc_icon_paths[[name]], "</svg>"
  ))
}

# Przycisk akcji Shiny (input$<id> jak w actionButton) w stylu v2.
# Nie używa klas Bootstrapa ani prefiksu lc-btn-, więc nie dziedziczy ich reguł.
lc_action <- function(input_id, label = NULL, icon = NULL,
                      variant = c("outline", "solid", "ghost"),
                      aria_label = NULL) {
  variant <- match.arg(variant)
  icon_only <- is.null(label)
  if (icon_only && is.null(aria_label)) {
    stop("Przycisk bez etykiety wymaga aria_label.")
  }
  tags$button(
    id = input_id, type = "button",
    class = paste("action-button lc-action", paste0("is-", variant),
                  if (icon_only) "is-icon"),
    `aria-label` = aria_label, title = if (icon_only) aria_label,
    if (!is.null(icon)) lc_icon(icon),
    if (!is.null(label)) tags$span(label)
  )
}

# Pasek sterowania nad treścią widgetu: elementy zawijają się same.
lc_toolbar <- function(...) {
  tags$div(class = "lc-toolbar", ...)
}

# Grupa w pasku: krótka etykieta nad kontrolką.
lc_group <- function(label = NULL, ..., grow = FALSE) {
  tags$div(class = paste("lc-grp", if (grow) "lc-grow"),
    if (!is.null(label)) tags$span(class = "lc-grp-l", label),
    ...
  )
}

.lc_choices <- function(choices) {
  values <- unname(as.character(choices))
  labels <- names(choices)
  if (is.null(labels)) labels <- values
  labels[!nzchar(labels)] <- values[!nzchar(labels)]
  list(values = values, labels = labels)
}

# Radio z ≤ 4 krótkimi opcjami jako segment. Wartość w input$<id>.
# exclusive_with: id drugiego segmentu, w którym nie można wybrać tej samej
# wartości (np. zmienna w wierszach i kolumnach tabeli krzyżowej).
lc_segmented <- function(input_id, label = NULL, choices, selected = NULL,
                         exclusive_with = NULL) {
  ch <- .lc_choices(choices)
  selected <- if (is.null(selected)) ch$values[[1]] else as.character(selected)
  label_id <- paste0(input_id, "-label")
  tags$div(
    id = input_id, class = "lc-grp shiny-input-radiogroup lc-seg-input",
    `data-lc-exclusive` = exclusive_with,
    if (!is.null(label)) tags$span(class = "lc-grp-l", id = label_id, label),
    tags$div(class = "lc-seg", role = "radiogroup",
      `aria-labelledby` = if (!is.null(label)) label_id,
      lapply(seq_along(ch$values), function(i) {
        tags$label(
          tags$input(type = "radio", name = input_id, value = ch$values[[i]],
                     checked = if (identical(ch$values[[i]], selected)) NA),
          ch$labels[[i]]
        )
      })
    )
  )
}

# Grupa akcji jako jeden segment, np. +1 · +10 · +100 · +1000.
# Argumenty: nazwane id = etykieta, np. lc_action_group(ch1_roll_1 = "+1").
lc_action_group <- function(..., label = NULL) {
  actions <- list(...)
  if (is.null(names(actions)) || any(!nzchar(names(actions)))) {
    stop("lc_action_group() wymaga par id = etykieta.")
  }
  lc_group(label,
    tags$div(class = "lc-seg", role = "group", `aria-label` = label,
      lapply(names(actions), function(id) {
        tags$button(id = id, type = "button", class = "action-button",
                    actions[[id]])
      })
    )
  )
}

.lc_decimals <- function(x) {
  s <- format(x, scientific = FALSE, drop0trailing = TRUE)
  if (!grepl("\\.", s)) 0L else nchar(sub("^[^.]*\\.", "", s))
}

# Liczba jako tekst: kropka dziesiętna i zwykły minus (jak w R i jamovi), bez
# końcowych zer. Do podpisów, odczytów i etykiet (bez wypełnienia).
lc_fmt <- function(x, digits = 0, big_mark = "") {
  vapply(x, function(v) {
    if (is.na(v)) return("–")
    s <- formatC(abs(v), format = "f", digits = digits, big.mark = big_mark)
    if (digits > 0) s <- sub("\\.?0+$", "", s)
    neg <- v < 0 && as.numeric(gsub("[^0-9.]", "", s)) != 0
    paste0(if (neg) "-", s)
  }, character(1), USE.NAMES = FALSE)
}

# Suwak v2: wartość w etykiecie (aktualizuje R/lc_widgets.js), min i max pod
# torem, bez siatki. digits domyślnie z kroku suwaka (także domyślnego kroku Shiny).
lc_slider <- function(input_id, label, min, max, value, step = NULL,
                      digits = NULL, suffix = "") {
  if (is.null(digits)) {
    # Bez step Shiny sam dobiera krok; liczba miejsc po kropce wynika z niego.
    eff_step <- if (!is.null(step)) step else tryCatch(
      shiny:::findStepSize(min, max, NULL), error = function(e) 1)
    digits <- max(.lc_decimals(eff_step), .lc_decimals(value))
  }
  fmt <- function(x) paste0(lc_fmt(x, digits), suffix)
  tags$div(class = "lc-grp lc-grow lc-slider",
    tags$div(class = "lc-slider-label",
      tags$label(`for` = input_id, label),
      tags$output(`data-lc-slider` = input_id, `data-digits` = digits,
                  `data-suffix` = suffix, fmt(value))
    ),
    sliderInput(input_id, label = NULL, min = min, max = max, value = value,
                step = step, ticks = FALSE, width = "100%"),
    tags$div(class = "lc-slider-ends", `aria-hidden` = "true",
      tags$span(fmt(min)), tags$span(fmt(max))
    )
  )
}

# Odczyt liczby w pasku zamiast lc_stat_box(). Z swatch = TRUE pełni rolę
# legendy serii (wtedy legend.position = "none" w ggplot).
lc_readout <- function(label, value, color = NULL, swatch = FALSE) {
  tags$div(class = "lc-read",
    style = if (!is.null(color)) paste0("--lc-read-color:", color, ";"),
    tags$span(class = "lc-read-label",
      if (isTRUE(swatch)) tags$i(class = "lc-read-swatch", `aria-hidden` = "true"),
      label
    ),
    tags$span(class = "lc-read-value", value)
  )
}

# Kontener odczytów, dosunięty do prawej krawędzi paska.
lc_readouts <- function(...) {
  tags$div(class = "lc-reads lc-push", ...)
}

# Jedno zdanie pod wykresem lub tabelą, z kropką statusu.
lc_caption <- function(..., tone = NULL) {
  if (!is.null(tone)) tone <- match.arg(tone, c("ok", "info"))
  tags$div(class = "lc-caption", `data-tone` = tone, tags$div(...))
}

# Wykres z wysokością z proporcji kontenera zamiast stałej wysokości w px.
# Serwer bez zmian: zoom_plot_server() rysuje w rozmiarze kontenera.
# max_height (np. "560px") tylko dla wykresów, które potrzebują więcej niż 440 px.
lc_plot <- function(plot_id, ratio = NULL, ratio_narrow = NULL, max_height = NULL) {
  style <- paste0(c(
    if (!is.null(ratio)) paste0("--lc-plot-ratio:", ratio, ";"),
    if (!is.null(ratio_narrow)) paste0("--lc-plot-ratio-narrow:", ratio_narrow, ";"),
    if (!is.null(max_height)) paste0("--lc-plot-max:", max_height, ";")
  ), collapse = "")
  tags$div(class = "lc-plot", style = if (nzchar(style)) style,
    zoom_plot_ui(plot_id, height = "100%")
  )
}

# Dwa wykresy obok siebie od 760 px kontenera, pod sobą na węższym.
lc_plots <- function(...) {
  tags$div(class = "lc-plots", ...)
}

# Pusty stan wykresu lub tabeli: tekst w przerywanej ramce.
lc_empty <- function(text) {
  tags$div(class = "lc-tbl-empty", text)
}

lc_table_empty <- lc_empty

# Widget bez wykresu: etykieta, pasek długości, wartość.
# rows: lista list(label, kicker = NULL, value, share = 0..1, ok = NA, color = NULL).
lc_compare_rows <- function(rows) {
  tags$div(class = "lc-cmp",
    lapply(rows, function(r) {
      state <- if (isTRUE(r$ok)) "is-ok" else if (isFALSE(r$ok)) "is-bad"
      share <- max(0, min(1, as.numeric(r$share)))
      tags$div(class = paste("lc-cmp-row", state),
        style = if (!is.null(r$color)) paste0("--lc-cmp-color:", r$color, ";"),
        tags$div(class = "lc-cmp-label",
          if (!is.null(r$kicker)) tags$em(r$kicker), r$label),
        tags$div(class = "lc-cmp-bar", `aria-hidden` = "true",
          tags$i(style = sprintf("width:%.1f%%;", 100 * share))),
        tags$div(class = "lc-cmp-value", r$value)
      )
    })
  )
}

# Nawigacja kroków demonstracji. Wartość input$<id>: numer kroku (0 = start).
# Nie da się cofnąć poniżej kroku `start` (start = 1: bez pustego stanu).
# Stan trzyma klient (R/lc_widgets.js); lc_update_step() ustawia go z serwera.
lc_step_nav <- function(input_id, steps, start = 0L,
                        start_label = "Zacznij", next_label = "Dalej") {
  tags$div(
    id = input_id, class = "lc-step-nav lc-push",
    `data-lc-steps` = length(steps), `data-step` = as.integer(start),
    `data-lc-min` = as.integer(start),
    `data-start-label` = start_label, `data-next-label` = next_label,
    tags$button(type = "button", class = "lc-action is-ghost is-icon",
      `data-lc-step` = "prev", `aria-label` = "Poprzedni krok",
      title = "Poprzedni krok", lc_icon("prev")),
    tags$div(class = "lc-step-dots",
      lapply(seq_along(steps), function(i) {
        tags$button(type = "button", class = "lc-step-dot",
          `data-lc-step-to` = i,
          `aria-label` = paste0("Krok ", i, ": ", steps[[i]]))
      })
    ),
    tags$button(type = "button", class = "lc-action is-solid",
      `data-lc-step` = "next",
      tags$span(if (start == 0) start_label else next_label), lc_icon("next"))
  )
}

lc_update_step <- function(session, input_id, step) {
  session$sendInputMessage(input_id, list(step = as.integer(step)))
}

# Opis bieżącego kroku: znacznik, opcjonalny tytuł i treść.
lc_step_text <- function(kicker, ..., title = NULL) {
  tags$div(class = "lc-step-text", `aria-live` = "polite",
    tags$div(class = "lc-step-kicker", kicker),
    if (!is.null(title)) tags$div(class = "lc-step-title", title),
    tags$div(class = "lc-step-body", ...)
  )
}

# --- Tabele v2 --------------------------------------------------------------

# Liczba w komórce tabeli: format lc_fmt() plus niewidoczne wypełnienie
# brakujących cyfr, żeby kropki dziesiętne stały w jednej linii. Zwraca HTML jako tekst.
# int_width: liczba znaków części całkowitej, do której dopełniamy z lewej.
# Dzięki temu cyfry stoją w jednej linii także w kolumnie wyśrodkowanej.
lc_num <- function(x, digits = 0, big_mark = "", int_width = NULL) {
  pad_span <- function(p) paste0('<span class="lc-pad" aria-hidden="true">', p, "</span>")
  vapply(x, function(v) {
    if (is.na(v)) return("–")
    full <- formatC(abs(v), format = "f", digits = digits, big.mark = big_mark)
    shown <- lc_fmt(v, digits, big_mark)
    lead <- ""
    if (!is.null(int_width)) {
      missing_int <- int_width - nchar(sub("\\..*$", "", shown))
      if (missing_int > 0) lead <- pad_span(strrep("0", missing_int))
    }
    if (digits == 0) return(paste0(lead, shown))
    full_frac <- sub("^[^.]*\\.", "", full)
    shown_frac <- if (grepl(".", shown, fixed = TRUE)) sub("^[^.]*\\.", "", shown) else ""
    pad <- paste0(if (!nzchar(shown_frac)) ".",
                  strrep("0", nchar(full_frac) - nchar(shown_frac)))
    paste0(lead, shown, if (nzchar(pad)) pad_span(pad))
  }, character(1), USE.NAMES = FALSE)
}

# Najdłuższa część całkowita w kolumnie liczbowej (dla lc_num(int_width =)).
.lc_int_width <- function(x, digits = 0) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(NULL)
  max(nchar(sub("\\..*$", "", lc_fmt(x, digits))))
}

# Wartość p: „< 0.001” poniżej progu, inaczej 3 miejsca. Zwraca HTML jako tekst.
lc_pval <- function(p) {
  out <- lc_num(p, 3)
  out[!is.na(p) & p < 0.001] <- "&lt; 0.001"
  out
}

# Deklaracja kolumny tabeli.
# type: "row" (nagłówek wiersza), "num" (liczba), "text".
# short + desc trafiają do widocznej legendy skrótów nad tabelą.
# sub: druga linia nagłówka (np. typ zmiennej).
# digits i suffix (jednostka, np. " cm", "%") mogą być wektorami — osobno dla
# każdego wiersza; krótsze sufiksy i ułamki dopełnia niewidoczny lc-pad, więc
# kropki dziesiętne stoją w jednej linii.
lc_col <- function(key, label, type = c("num", "text", "row"), digits = 0,
                   short = NULL, desc = NULL, width = NULL, class = NULL,
                   sub = NULL, suffix = NULL) {
  structure(
    list(key = key, label = label, type = match.arg(type), digits = digits,
         short = short, desc = desc, width = width, class = class, sub = sub,
         suffix = suffix),
    class = "lc_col"
  )
}

.lc_auto_cols <- function(df) {
  lapply(seq_along(df), function(i) {
    key <- names(df)[i]
    type <- if (i == 1) "row" else if (is.numeric(df[[i]])) "num" else "text"
    lc_col(key, key, type)
  })
}

.lc_cell_content <- function(col, value, i = NULL) {
  if (is.list(value)) value <- value[[1]]
  if (inherits(value, c("shiny.tag", "shiny.tag.list", "html"))) return(value)
  if (is.null(value) || (length(value) == 1 && is.na(value))) {
    return(if (identical(col$type, "num")) HTML("–") else "")
  }
  if (is.numeric(value)) {
    pick <- function(x) if (length(x) > 1 && !is.null(i)) x[[i]] else x[[1]]
    pad <- function(p) paste0('<span class="lc-pad" aria-hidden="true">', p, "</span>")
    d <- pick(col$digits)
    out <- lc_num(value, d, int_width = col$int_width)
    max_d <- max(col$digits)
    # Dopełnienie za jednostką: kropki stoją w jednej linii, a jednostka
    # przylega do liczby.
    gap <- strrep("0", max_d - d)
    if (d == 0 && max_d > 0) gap <- paste0(".", gap)
    if (!is.null(col$suffix)) {
      sfx <- pick(col$suffix)
      out <- paste0(out, htmltools::htmlEscape(sfx))
      gap <- paste0(gap, strrep("0", max(nchar(col$suffix)) - nchar(sfx)))
    }
    if (nzchar(gap)) out <- paste0(out, pad(gap))
    return(HTML(out))
  }
  # W kolumnie liczbowej tekst to gotowy wynik lc_num() / lc_pval().
  if (identical(col$type, "num")) return(HTML(as.character(value)))
  as.character(value)
}

.lc_col_header <- function(col) {
  label <- if (!is.null(col$short)) tags$abbr(title = col$label, col$short) else col$label
  list(label, if (!is.null(col$sub)) tags$span(class = "lc-th-sub", col$sub))
}

.lc_classes <- function(...) {
  x <- unlist(list(...), use.names = FALSE)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x)) paste(x, collapse = " ")
}

.lc_cell_class <- function(cell_class, key, i) {
  if (is.null(cell_class) || is.null(cell_class[[key]])) return(NULL)
  v <- cell_class[[key]]
  if (length(v) == 1) v else v[[i]]
}

# Sam element <table class="lc-tbl">; lc_table() dokłada blok, legendę
# i przewijanie.
.lc_table_tag <- function(df, cols, foot = NULL, caption = NULL, number = NULL,
                          class = NULL, row_class = NULL, cell_class = NULL,
                          colgroup = FALSE, roles = FALSE) {
  role <- function(r) if (roles) r
  n <- nrow(df)
  cols <- lapply(cols, function(col) {
    if (identical(col$type, "num") && is.null(col$int_width)) {
      vals <- c(if (is.numeric(df[[col$key]])) df[[col$key]],
                if (!is.null(foot) && is.numeric(foot[[col$key]])) foot[[col$key]])
      col$int_width <- .lc_int_width(vals, max(col$digits))
    }
    col
  })
  is_num <- function(col) identical(col$type, "num")
  head_cells <- lapply(cols, function(col) {
    tags$th(scope = "col", role = role("columnheader"),
      class = .lc_classes(if (is_num(col)) "n", col$class),
      .lc_col_header(col))
  })
  make_row <- function(values, i = NULL, tr_class = NULL) {
    tags$tr(role = role("row"), class = tr_class,
      lapply(cols, function(col) {
        cls <- .lc_classes(if (is_num(col)) "n", col$class,
                           if (!is.null(i)) .lc_cell_class(cell_class, col$key, i))
        content <- .lc_cell_content(col, values[[col$key]], i)
        if (identical(col$type, "row")) {
          tags$th(scope = "row", role = role("rowheader"), class = cls, content)
        } else {
          tags$td(role = role("cell"), class = cls,
                  `data-label` = col$label, content)
        }
      })
    )
  }
  body_rows <- lapply(seq_len(n), function(i) {
    make_row(lapply(df, function(column) column[i]), i,
             if (!is.null(row_class)) .lc_classes(row_class[[i]]))
  })
  tags$table(
    class = .lc_classes("lc-tbl", class), role = role("table"),
    if (!is.null(caption) || !is.null(number)) tags$caption(
      if (!is.null(number)) tags$span(class = "lc-tbl-num", number), caption
    ),
    if (isTRUE(colgroup)) tags$colgroup(lapply(cols, function(col) {
      tags$col(style = if (!is.null(col$width)) paste0("width:", col$width, ";"))
    })),
    tags$thead(role = role("rowgroup"), tags$tr(role = role("row"), head_cells)),
    tags$tbody(role = role("rowgroup"), body_rows),
    if (!is.null(foot)) tags$tfoot(role = role("rowgroup"), make_row(as.list(foot)))
  )
}

# Legenda skrótów z kolumn, które mają short i desc.
lc_table_key <- function(cols, class = NULL) {
  keyed <- Filter(function(col) !is.null(col$short) && !is.null(col$desc), cols)
  if (!length(keyed)) return(NULL)
  tags$div(class = .lc_classes("lc-tbl-key", class),
    lapply(keyed, function(col) tags$span(tags$b(col$short), col$desc))
  )
}

.lc_scroll <- function(x, label) {
  tags$div(class = "lc-tbl-scroll", tabindex = "0", role = "region",
           `aria-label` = label, x)
}

# Tabela v2. Komórki liczbowe formatuje lc_num(); kolumny tekstowe mogą
# zawierać tagi (kolumna-lista). foot: nazwana lista lub 1-wierszowy df.
# narrow: zachowanie tabeli tekstowej na wąskim kontenerze.
# fit = TRUE: tabela liczbowa nie rozciąga się na pełną szerokość.
# cell_class: nazwana lista klucz kolumny → wektor klas (is-target, is-best…).
# page_size: stronicowanie. Bez page_input strony przełącza przeglądarka
# (w HTML są wszystkie wiersze; dla małych tabel). Z page_input renderowana
# jest tylko strona `page`, a przyciski ustawiają input$<page_input>
# (dla dużych zbiorów): lc_table(..., page = input$x_page, page_input = "x_page").
lc_table <- function(df, cols = NULL, foot = NULL, caption = NULL, number = NULL,
                     narrow = c("none", "cards", "stack-last"), fit = NULL,
                     row_class = NULL, cell_class = NULL, key = TRUE,
                     scroll = FALSE, sticky_first = FALSE, label = NULL,
                     prose = FALSE, colgroup = FALSE, lead = NULL, note = NULL,
                     page_size = NULL, page = 1, page_input = NULL) {
  narrow <- match.arg(narrow)
  if (is.null(cols)) cols <- .lc_auto_cols(df)
  pager <- NULL
  total <- nrow(df)
  if (!is.null(page_size) && total > page_size) {
    # Szerokości cyfr z całego zbioru, żeby wyrównanie nie skakało między stronami.
    cols <- lapply(cols, function(col) {
      if (identical(col$type, "num") && is.numeric(df[[col$key]])) {
        col$int_width <- .lc_int_width(c(df[[col$key]],
          if (!is.null(foot) && is.numeric(foot[[col$key]])) foot[[col$key]]), max(col$digits))
      }
      col
    })
    n_pages <- ceiling(total / page_size)
    page <- max(1L, min(n_pages, as.integer(page %||% 1L)))
    rows <- ((page - 1) * page_size + 1):min(total, page * page_size)
    if (is.null(page_input)) {
      paged_out <- ifelse(seq_len(total) %in% rows, NA_character_, "is-paged-out")
      row_class <- if (is.null(row_class)) paged_out else
        ifelse(is.na(paged_out), row_class, paste(row_class, paged_out))
    } else {
      df <- df[rows, , drop = FALSE]
      if (!is.null(row_class)) row_class <- row_class[rows]
      if (!is.null(cell_class)) cell_class <- lapply(cell_class, function(v) {
        if (length(v) == 1) v else v[rows]
      })
    }
    pager <- .lc_pager(page, n_pages, min(rows), max(rows), total, page_size, page_input)
  }
  types <- vapply(cols, `[[`, character(1), "type")
  if (is.null(fit)) fit <- narrow == "none" && any(types == "num")
  text_table <- narrow != "none" || !any(types == "num")
  table_class <- .lc_classes(
    if (isTRUE(fit)) "is-fit",
    if (text_table) "is-text",
    if (narrow == "cards") "is-cards",
    if (narrow == "stack-last") "is-stack-last",
    if (isTRUE(sticky_first)) c("is-sticky-first", "is-data")
  )
  tbl <- .lc_table_tag(df, cols, foot = foot, caption = caption, number = number,
                       class = table_class, row_class = row_class,
                       cell_class = cell_class, colgroup = colgroup,
                       roles = narrow != "none")
  if (isTRUE(scroll)) tbl <- .lc_scroll(tbl, label %||% caption %||% "Tabela")
  tags$div(class = .lc_classes("lc-tbl-block", if (isTRUE(prose)) "lc-tbl-prose"),
    if (!is.null(lead)) tags$p(class = "lc-tbl-lead", lead),
    if (isTRUE(key)) lc_table_key(cols),
    tbl,
    pager,
    if (!is.null(note)) tags$div(class = "lc-tbl-note", note)
  )
}

# Pasek stronicowania pod tabelą. Logika przycisków: R/lc_widgets.js.
.lc_pager <- function(page, n_pages, from, to, total, page_size, page_input = NULL) {
  nav_button <- function(to_page, label, icon, disabled) {
    tags$button(type = "button", class = "lc-action is-ghost is-icon",
      `data-lc-page-to` = to_page, `aria-label` = label, title = label,
      disabled = if (disabled) NA, lc_icon(icon))
  }
  tags$div(class = "lc-pager", `data-lc-page-input` = page_input,
    `data-lc-page-size` = page_size, `data-lc-total` = total,
    tags$span(class = "lc-pager-info", `aria-live` = "polite",
      `data-lc-page-range` = NA, sprintf("Wiersze %d–%d z %d", from, to, total)),
    tags$div(class = "lc-pager-nav",
      nav_button(page - 1, "Poprzednia strona", "prev", page <= 1),
      tags$span(class = "lc-pager-info", `data-lc-page-label` = NA,
                sprintf("%d / %d", page, n_pages)),
      nav_button(page + 1, "Następna strona", "next", page >= n_pages)
    )
  )
}

# Jedna szeroka tabela, a na wąskim kontenerze (< 34em) kilka węższych
# z powtórzoną kolumną wierszy. groups: lista wektorów kluczy kolumn.
# Kolumna typu "row" jest dokładana do każdej grupy. foot trafia do grup,
# w których ma niepuste wartości.
lc_table_split <- function(df, cols, groups, foot = NULL, label = "Tabela",
                           cell_class = NULL, key = TRUE, lead = NULL) {
  keys <- vapply(cols, `[[`, character(1), "key")
  row_cols <- cols[vapply(cols, function(col) identical(col$type, "row"), logical(1))]
  group_tables <- lapply(groups, function(g) {
    gcols <- c(row_cols, cols[match(g, keys)])
    gfoot <- NULL
    if (!is.null(foot)) {
      vals <- unlist(foot[g], use.names = FALSE)
      if (any(!is.na(vals) & nzchar(as.character(vals)))) gfoot <- foot
    }
    .lc_table_tag(df, gcols, foot = gfoot, class = "is-fit", cell_class = cell_class)
  })
  tags$div(class = "lc-tbl-block",
    if (!is.null(lead)) tags$p(class = "lc-tbl-lead", lead),
    if (isTRUE(key)) lc_table_key(cols),
    tags$div(class = "lc-tbl-v-wide",
      .lc_scroll(.lc_table_tag(df, cols, foot = foot, class = "is-fit",
                               cell_class = cell_class), label)),
    tags$div(class = "lc-tbl-v-narrow", group_tables)
  )
}

# Podgląd pierwszych n obserwacji: kolumna numeru i wybrane kolumny,
# rozłożone na `split` tabel obok siebie (na wąskim jedna pod drugą).
lc_table_preview <- function(df, n = 20, split = 2, cols = NULL, total = nrow(df)) {
  shown <- utils::head(df, n)
  shown <- cbind(data.frame(Nr = seq_len(nrow(shown))), shown)
  if (is.null(cols)) cols <- lapply(names(df), function(k) {
    lc_col(k, k, if (is.numeric(df[[k]])) "num" else "text")
  })
  cols <- c(list(lc_col("Nr", "Nr", "row", class = "n")), cols)
  parts <- split(seq_len(nrow(shown)), ceiling(seq_len(nrow(shown)) /
                 ceiling(nrow(shown) / split)))
  tags$div(class = "lc-tbl-block",
    tags$div(class = "lc-tbl-pair",
      lapply(parts, function(idx) {
        .lc_table_tag(shown[idx, , drop = FALSE], cols, class = "is-fit")
      })
    ),
    tags$div(class = "lc-tbl-note",
             sprintf("Pierwsze %d z %d obserwacji", nrow(shown), total))
  )
}

# Tabela krzyżowa z sumami brzegowymi. measure: "n", "row" (% wierszowe),
# "col" (% kolumnowe). target = c(i, j): komórka opisana w tekście.
# input_id: komórki stają się przyciskami; kliknięcie ustawia input$<id>
# na c(i, j). short_labels: krótkie etykiety kolumn na wąskim kontenerze.
# cell_tags: macierz etykiet znaczeniowych komórek (np. „trafienie”).
# col_colours: kolory kategorii kolumn (próbka w nagłówku zamiast legendy wykresu).
lc_crosstab <- function(tab, measure = c("n", "row", "col"), target = NULL,
                        row_name = "", col_name = "", short_labels = NULL,
                        cell_tags = NULL, input_id = NULL, digits = 1,
                        lead = TRUE, label = "Tabela krzyżowa", col_colours = NULL) {
  measure <- match.arg(measure)
  tab <- as.matrix(tab)
  storage.mode(tab) <- "double"
  rows <- rownames(tab)
  cols <- colnames(tab)
  row_tot <- rowSums(tab)
  col_tot <- colSums(tab)
  total <- sum(tab)
  value <- switch(measure,
    n = tab,
    row = sweep(tab, 1, row_tot, "/") * 100,
    col = sweep(tab, 2, col_tot, "/") * 100
  )
  value_digits <- if (measure == "n") 0 else digits
  right <- switch(measure, n = row_tot, row = rep(100, length(rows)),
                  col = row_tot / total * 100)
  bottom <- switch(measure, n = col_tot, col = rep(100, length(cols)),
                   row = col_tot / total * 100)
  grand <- if (measure == "n") total else 100
  col_width <- vapply(seq_along(cols), function(j) {
    .lc_int_width(c(value[, j], bottom[j]), value_digits)
  }, numeric(1))
  right_width <- .lc_int_width(c(right, grand), value_digits)
  is_target <- function(i, j) !is.null(target) && target[1] == i && target[2] == j
  is_base_row <- function(i) measure == "row" && !is.null(target) && target[1] == i
  is_base_col <- function(j) measure == "col" && !is.null(target) && target[2] == j
  has_short <- !is.null(short_labels) && any(short_labels != cols)
  col_label <- function(j) {
    swatch <- if (!is.null(col_colours)) {
      tags$i(class = "lc-th-swatch", `aria-hidden` = "true",
             style = paste0("--lc-sw:", col_colours[[j]], ";"))
    }
    if (!has_short) return(tagList(swatch, cols[j]))
    tagList(swatch, tags$span(class = "lc-l-full", cols[j]),
            tags$span(class = "lc-l-short", `aria-hidden` = "true", short_labels[j]))
  }
  cell <- function(i, j) {
    shown <- HTML(lc_num(value[i, j], value_digits, int_width = col_width[j]))
    content <- tagList(
      if (!is.null(cell_tags)) tags$span(class = "lc-cell-tag", cell_tags[i, j]),
      shown
    )
    if (!is.null(input_id)) {
      content <- tags$button(type = "button", class = "lc-cell-btn",
        `data-lc-cell-input` = input_id, `data-i` = i, `data-j` = j,
        `aria-pressed` = if (is_target(i, j)) "true" else "false", content)
    }
    tags$td(
      class = .lc_classes("n", if (is_target(i, j)) "is-target"
                          else if (is_base_row(i) || is_base_col(j)) "is-base"),
      `data-label` = cols[j], content)
  }
  row_total <- function(i) {
    v <- lc_num(right[i], value_digits, int_width = right_width)
    tags$td(class = .lc_classes("n is-total", if (measure == "row") "is-base-val",
                                if (is_base_row(i)) "is-base"), HTML(v))
  }
  col_total <- function(j) {
    v <- lc_num(bottom[j], value_digits, int_width = col_width[j])
    tags$td(class = .lc_classes("n", if (measure == "col") "is-base-val",
                                if (is_base_col(j)) "is-base"), HTML(v))
  }
  tbl <- tags$table(class = "lc-tbl is-fit",
    tags$thead(
      tags$tr(
        tags$th(scope = "col", rowspan = 2, class = "lc-tbl-corner", row_name),
        tags$th(scope = "colgroup", colspan = length(cols), class = "lc-tbl-span", col_name),
        tags$th(scope = "col", rowspan = 2, class = "n is-total", "Razem")
      ),
      tags$tr(lapply(seq_along(cols), function(j) {
        tags$th(scope = "col", class = .lc_classes("n", if (is_base_col(j)) "is-base"),
                col_label(j))
      }))
    ),
    tags$tbody(lapply(seq_along(rows), function(i) {
      tags$tr(
        tags$th(scope = "row", class = if (is_base_row(i)) "is-base", rows[i]),
        lapply(seq_along(cols), function(j) cell(i, j)),
        row_total(i)
      )
    })),
    tags$tfoot(tags$tr(
      tags$th(scope = "row", "Razem"),
      lapply(seq_along(cols), col_total),
      tags$td(class = "n is-total", HTML(lc_num(grand, value_digits, int_width = right_width)))
    ))
  )
  lead_tag <- if (isTRUE(lead)) tags$p(class = "lc-tbl-lead", switch(measure,
    n = paste0("Liczebności, N = ", lc_fmt(total), "."),
    row = tagList(tags$b("% wierszowe:"), " każdy wiersz sumuje się do 100."),
    col = tagList(tags$b("% kolumnowe:"), " każda kolumna sumuje się do 100.")
  ))
  short_key <- if (has_short) tags$div(class = "lc-tbl-key is-short-key",
    lapply(seq_along(cols), function(j) tags$span(tags$b(short_labels[j]), cols[j])))
  tags$div(class = "lc-tbl-block", lead_tag, short_key, .lc_scroll(tbl, label))
}

# --- Bloki treści v2 ---------------------------------------------------------
# Źródło: handoff „Bloki v2”. Hierarchię niesie typografia: tło mają tylko
# widgety (figure_panel) i pułapki (lc_warn). Margines boczny nie istnieje;
# dawne margin_callout() / inline_callout() renderują się jako lc_note().

# Notka z wiszącą etykietą (Zasada, Uwaga, Przykład, Jak czytać…).
# rule = TRUE tylko dla „Zasady”: najwyżej jedna na sekcję.
# title: pogrubiony tytuł nad treścią (np. termin definicji, tytuł przykładu).
lc_note <- function(label, ..., rule = FALSE, title = NULL) {
  tags$div(class = .lc_classes("lc-note", if (isTRUE(rule)) "lc-note-rule"),
    tags$div(class = "lc-note-l", label),
    tags$div(class = "lc-note-b",
      if (!is.null(title)) tags$div(class = "lc-note-t", title),
      ...
    )
  )
}

# Pułapka: błąd, który student realnie popełnia. Najwyżej jedna na sekcję.
lc_warn <- function(label, ...) {
  tags$div(class = "lc-warn", role = "note",
    tags$div(class = "lc-warn-l", label),
    tags$div(class = "lc-warn-b", ...)
  )
}

# Treść zwinięta: tylko rozwiązania, odpowiedzi i opcjonalne rozwinięcia
# (np. „Chcesz więcej matematyki?”, „Skąd to się bierze”). Notki się nie zwijają.
lc_more <- function(label, ..., open = FALSE) {
  tags$details(class = "lc-more", open = if (isTRUE(open)) NA,
    tags$summary(class = "lc-more-l", label),
    tags$div(class = "lc-more-b", ...)
  )
}

# Podsumowanie sekcji: każdy argument w ... to jeden numerowany punkt.
lc_recap <- function(..., label = "Najważniejsze do zapamiętania") {
  tags$div(class = "lc-recap",
    tags$div(class = "lc-recap-l", label),
    tags$ol(lapply(list(...), tags$li))
  )
}

# Śledzona zmienna w jednej linii. stats: nazwany wektor sformatowanych liczb,
# np. c("x̄" = lc_fmt(171.14, 2), Me = lc_fmt(170.65, 2)).
lc_tracker <- function(label, stats, kicker = "Śledzisz") {
  tags$div(class = "lc-tracker", role = "status",
    tags$span(class = "lc-tracker-l", kicker),
    tags$span(class = "lc-tracker-v", label),
    tags$dl(Map(function(k, v) tags$div(tags$dt(k), tags$dd(v)),
                names(stats), unname(stats)))
  )
}

# Spis przykładów. items: list(list(code = "B1", title = "…", target = "id-sekcji")).
lc_index <- function(items) {
  tags$ul(class = "lc-index", lapply(items, function(it) {
    tags$li(tags$a(href = paste0("#", it$target),
      tags$span(class = "lc-index-c", it$code),
      tags$span(it$title),
      tags$span(class = "lc-index-go", `aria-hidden` = "true", "→")
    ))
  }))
}

# Status widgetu w renderUI() wewnątrz figure_panel(), pod wykresem: opis kroku,
# wynik testu, dłuższy komentarz. Jedno zdanie z kropką statusu to lc_caption().
lc_status <- function(..., live = TRUE) {
  tags$div(class = "lc-status",
    role = if (isTRUE(live)) "status", `aria-live` = if (isTRUE(live)) "polite", ...)
}

# Werdykt w lc_status(): kolor tylko na fragmencie tekstu, bez tła.
# ok = zgodne / poprawne, warning = uwaga (np. ekstrapolacja), danger = błąd,
# info = bez koloru (gdy typ liczy się w locie i bywa neutralny).
lc_verdict <- function(..., type = c("ok", "warning", "danger", "info")) {
  type <- match.arg(type)
  tags$span(class = paste0("lc-status-", type), ...)
}

# Przełączniki drugorzędne (np. hipotezy): jeden aktywny albo żaden.
# Wartość w input$<input_id> (NULL, gdy nic nie wybrano). Logika: R/lc_widgets.js.
lc_chips <- function(input_id, choices, label = NULL) {
  ch <- .lc_choices(choices)
  tags$div(class = "lc-chips", `data-lc-chips` = input_id,
    if (!is.null(label)) tags$span(class = "lc-chips-l", label),
    lapply(seq_along(ch$values), function(i) {
      tags$button(type = "button", class = "lc-chip", `aria-pressed` = "false",
                  `data-value` = ch$values[[i]], ch$labels[[i]])
    })
  )
}

# Pogrubienie i kursywa bez spacji przed interpunkcją: htmltools wstawia znak
# nowej linii między dziećmi taga, więc p("jest ", strong("x"), ", gdy")
# renderuje się jako „x , gdy”.
b_ <- function(...) tags$strong(..., .noWS = "outside")
em_ <- function(...) tags$em(..., .noWS = "outside")

# --- Widget krokowy z paskiem kroków -----------------------------------------
# Źródło: handoff „Widget krokowy v2”. Drugi wzorzec obok lc_step_nav()
# (kropki): pasek z numerami i nazwami kroków, gdy nazwy niosą narrację.
# Układ: tytuł + sterowanie, pasek kroków, wykres o stałej proporcji, opis
# kroku i nawigacja. Kroki od 1. Serwer czyta input$<id>_step (lc_step_server),
# opis kroku renderuje output$<id>_text jako tekst inline. Logika: R/lc_widgets.js.
# Widget bez wykresu (np. tabela budowana krok po kroku): body zamiast plot_id.
# above: treść między paskiem kroków a wykresem (np. tabela i odczyty w rzędzie).
lc_step_widget <- function(id, steps, plot_id = NULL, toolbar = NULL, title = NULL,
                           extra = NULL, ratio = "2.5/1", body = NULL, above = NULL) {
  n <- length(steps)
  stopifnot(n >= 2, !is.null(plot_id) || !is.null(body))
  tags$div(
    class = "lc-stepper", id = id, `data-lc-step` = 1L,
    tags$div(class = "lc-stepper-head",
      if (!is.null(title)) tags$div(class = "lc-stepper-title", title),
      toolbar
    ),
    tags$ol(
      class = "lc-step-track", style = sprintf("--lc-steps:%d;", n),
      `data-dense` = if (n > 8) NA,
      lapply(seq_len(n), function(i) tags$li(
        tags$button(type = "button", class = "lc-step-tab", `data-lc-go` = i,
                    `aria-current` = if (i == 1L) "step",
                    tags$b(i), tags$span(steps[[i]]))
      ))
    ),
    if (!is.null(above)) tags$div(class = "lc-step-above", above),
    if (!is.null(plot_id)) {
      tags$div(class = "lc-plot lc-step-plot",
        style = sprintf("--lc-plot-ratio:%s;", ratio),
        zoom_plot_ui(plot_id, height = "100%")
      )
    } else {
      tags$div(class = "lc-step-body", body)
    },
    tags$div(class = "lc-stepper-foot",
      tags$div(class = "lc-stepper-text", `aria-live` = "polite",
        tags$span(class = "lc-stepper-kicker", sprintf("Krok 1 z %d · %s", n, steps[[1]])),
        uiOutput(paste0(id, "_text"), inline = TRUE)
      ),
      tags$div(class = "lc-stepper-actions",
        tags$button(type = "button", class = "lc-action is-ghost is-icon",
          `data-lc-nav` = "reset", title = "Od początku", `aria-label` = "Od początku",
          lc_icon("reset")),
        tags$button(type = "button", class = "lc-action is-outline", `data-lc-nav` = "prev",
          disabled = NA, lc_icon("prev"), tags$span("Wstecz")),
        tags$button(type = "button", class = "lc-action is-solid", `data-lc-nav` = "next",
          tags$span("Dalej"), lc_icon("next"))
      )
    ),
    extra
  )
}

# Kontrolka aktywna od kroku `from`; wcześniej widoczna i wyszarzona.
lc_step_from <- function(from, ...) {
  tags$div(class = "lc-step-ctl", `data-lc-from` = from, ...)
}

# Serwer widgetu krokowego: reaktywny numer kroku (1..n) i ustawianie kroku.
lc_step_server <- function(id, input, session = shiny::getDefaultReactiveDomain()) {
  key <- paste0(id, "_step")
  list(
    step = reactive({
      s <- input[[key]]
      if (is.null(s)) 1L else as.integer(s)
    }),
    set = function(k) {
      session$sendCustomMessage("lc-step-set", list(id = id, step = as.integer(k)))
    }
  )
}
