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
  list(num = "I",    slug = "opisowa",    title = "Statystyka opisowa",          short = "Opisowa",      href = "#"),
  list(num = "II",   slug = "rozklady",   title = "Rozkłady prawdopodobieństwa", short = "Rozkłady",     href = "#"),
  list(num = "III",  slug = "przedzialy", title = "Przedziały ufności",          short = "Przedziały",   href = "#"),
  list(num = "IV",   slug = "testowanie", title = "Testowanie",                  short = "Testowanie",   href = "#"),
  list(num = "V",    slug = "regresja",   title = "Regresja",                    short = "Regresja",     href = "#"),
  list(num = "IVa",  slug = "zalozenia",  title = "Założenia testów",            short = "Założenia",    href = "#"),
  list(num = "VI",   slug = "symulacje",  title = "Symulacje",                   short = "Symulacje",    href = "#"),
  list(num = "VII",  slug = "bayes",      title = "Bayes",                       short = "Bayes",        href = "#"),
  list(num = "VIII", slug = "dane",       title = "Dobre dane",                  short = "Dane",         href = "#"),
  list(num = "IX",   slug = "case",       title = "Case studies",                short = "Case studies", href = "#")
)

# Mapowanie lecture_id → slug modułu
.LC_LECTURE_MODULE <- list(
  "typy-danych"                 = "opisowa",
  "rozklady-prawdopodobienstwa" = "rozklady",
  "przedzialy-ufnosci"          = "przedzialy",
  "wnioskowanie-statystyczne"   = "testowanie",
  "zalozenia-testow"            = "zalozenia",
  "regresja"                    = "regresja",
  "symulacje-statystyczne"      = "symulacje",
  "metody-bayesowskie"          = "bayes",
  "dobre-dane"                  = "dane",
  "case-studies"                = "case"
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
      # Paleta UPWr jako CSS custom properties — źródło wartości: R/palette.R.
      # Wstrzyknięte po shared_styles.css, żeby wartości z palette.R miały
      # pierwszeństwo nad wartościami fallbackowymi w CSS.
      .lc_palette_css(),
      includeScript(file.path(proj_root, "R", "shared_toc.js")),
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
          progress_block
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

# Zwinięty callout w głównej kolumnie tekstu.
# Używany eksperymentalnie tam, gdzie callout w marginesie zaburza rytm treści.
inline_callout <- function(label = "Zapamiętaj", ..., color = "wskazowka",
                           open = FALSE) {
  css_class <- paste("lc-inline-callout", switch(color,
    uwaga     = "lc-callout-uwaga",
    ok        = "lc-callout-ok",
    wskazowka = "",
    ""
  ))
  tags$details(
    class = css_class,
    if (isTRUE(open)) list(open = NA),
    tags$summary(class = "lc-inline-callout-label", label),
    tags$div(class = "lc-inline-callout-body", ...)
  )
}

# Notka na marginesie (bez etykiety)
margin_note <- function(...) {
  tags$div(
    class = "lc-margin",
    tags$div(
      style = "font-family:var(--upwr-serif);font-size:13px;line-height:1.5;color:var(--upwr-reference);font-style:italic;",
      ...
    )
  )
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
                        style = NULL) {
  type <- match.arg(type)
  tags$div(
    class = paste("lc-feedback", paste0("lc-feedback-", type)),
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

margin_code_note <- function(code, description = NULL, label = "W kodzie") {
  tags$div(
    class = "lc-margin lc-margin-callout lc-callout-kod",
    tags$div(class = "lc-margin-callout-label", label),
    tags$div(
      class = "lc-margin-callout-body",
      tags$pre(class = "lc-margin-code", tags$code(code)),
      if (!is.null(description))
        tags$div(class = "lc-margin-code-desc", description)
    )
  )
}

# ============================================================================
# lc_chapter_next() — navigational "→ Dalej — 02 · Tytuł" na marginesie.
# Klika → session$sendCustomMessage("switchToChapter", target_id) przez JS.
# ============================================================================

lc_chapter_next <- function(num, title, lead = NULL, target_id) {
  tags$a(
    class = "lc-margin lc-margin-callout lc-callout-next",
    href  = "#",
    `data-lc-next-target` = target_id,
    onclick = paste0(
      "event.preventDefault();",
      "Shiny.setInputValue('lc__switch_chapter', '", target_id,
      "', {priority:'event'});"
    ),
    tags$div(class = "lc-margin-callout-label", "Dalej"),
    tags$div(
      class = "lc-margin-callout-body",
      tags$div(class = "lc-chapter-next-title",
        tags$span(class = "lc-chapter-next-num", num), " · ", title
      ),
      if (!is.null(lead) && nchar(lead) > 0)
        tags$div(class = "lc-chapter-next-lead", lead)
    )
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
  ink_subtle       <- "#b8b1a5"   # jaśniejsza niż reference (dla subtelnego tekstu)
  rule_soft        <- "#e8e1d2"   # jaśniejsza niż upwr_rule (dla miękkich dividerów)
  accent_hover     <- upwr_seq_burgundy[6]   # ciemniejszy burgund na hover
  accent_tint      <- upwr_seq_burgundy[2]   # jasne tło dla callout-uwaga
  alt_tint         <- upwr_seq_gold[2]       # jasne tło dla callout-kod
  sage             <- unname(upwr_cat["szalwia"])
  sage_tint        <- "#dee8de"              # jasne tło dla callout-ok

  css <- sprintf(
    ":root {
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
  --upwr-secondary:         %s;
  --upwr-cat-grafit:        %s;
  --upwr-cat-bursztyn:      %s;
  --upwr-cat-niebo:         %s;
  --upwr-cat-szalwia:       %s;
  --upwr-cat-kurkuma:       %s;
  --upwr-cat-indygo:        %s;
  --upwr-cat-terakota:      %s;
  --upwr-cat-wrzos:         %s;
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
