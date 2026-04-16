# Co czyni dobry zbior danych?
# Interaktywny wyklad oparty o case studies - ocena jakosci danych do analiz statystycznych

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(AER)
library(palmerpenguins)
library(ISLR)
library(fivethirtyeight)

# ============================================================================
# KOLORY
# ============================================================================

col_good    <- "#27ae60"
col_mixed   <- "#f39c12"
col_bad     <- "#e74c3c"
col_primary <- "#3498db"
col_dark    <- "#2c3e50"

# ============================================================================
# DANE
# ============================================================================

# --- Tab 1: CASchool (AER) ---
data("CASchools", package = "AER")

# --- Tab 3: penguins (palmerpenguins) ---
data("penguins", package = "palmerpenguins")

# --- Tab 4: tarantino (fivethirtyeight) ---
data("tarantino", package = "fivethirtyeight")

# --- Tab 6: Wage (ISLR) ---
data("Wage", package = "ISLR")

# --- Generowane zbiory ---
set.seed(2025)

# Tab 2: Za malo danych (n=8)
small_n <- 8
small_data <- data.frame(
  plec = factor(sample(c("Kobieta", "Mezczyzna"), small_n, replace = TRUE)),
  kierunek = factor(sample(c("Informatyka", "Biologia", "Psychologia", "Ekonomia"), small_n, replace = TRUE)),
  godziny_nauki = round(rnorm(small_n, mean = 15, sd = 5), 1),
  stres = round(pmin(10, pmax(1, rnorm(small_n, mean = 5.5, sd = 2))), 1),
  srednia_ocen = round(pmin(5.0, pmax(2.0, rnorm(small_n, mean = 3.8, sd = 0.6))), 2),
  stringsAsFactors = FALSE
)

# Tab 5: Ankieta firmowa - brak zmiennosci (n=80)
corp_n <- 80
corp_data <- data.frame(
  zadowolenie = sample(1:5, corp_n, replace = TRUE, prob = c(0.01, 0.02, 0.02, 0.35, 0.60)),
  departament = factor(sample(c("IT", "HR", "Marketing", "Finanse"), corp_n, replace = TRUE, prob = c(0.94, 0.02, 0.02, 0.02))),
  staz_pracy = round(runif(corp_n, 2.8, 3.5), 1),
  wynagrodzenie = round(runif(corp_n, 4800, 5200)),
  plec = factor(sample(c("M", "K"), corp_n, replace = TRUE, prob = c(0.9, 0.1))),
  stringsAsFactors = FALSE
)

# Tab 7: Trudna ankieta - zle zdefiniowane zmienne (n=90)
messy_n <- 90
messy_data <- data.frame(
  czas_na_studia = sample(c("duzo", "3-4h", "5", "caly dzien", "malo", "ok. 2 godziny",
                            "nie wiem", "3", "6h dziennie", "weekendy"), messy_n, replace = TRUE),
  ocena_kursu = sample(c("8/10", "dobrze", "4", "B+", "7.5", "srednia", "9", "bardzo dobrze",
                         "6/10", "slabo", "10", "ok"), messy_n, replace = TRUE),
  aktywnosc = sample(c("tak", "nie", "czasami", "3 razy w tygodniu", "rzadko",
                       "codziennie", "2x", "nie wiem"), messy_n, replace = TRUE),
  samopoczucie = sample(seq(10, 100, by = 10), messy_n, replace = TRUE,
                        prob = c(0.02, 0.03, 0.05, 0.08, 0.15, 0.20, 0.20, 0.15, 0.08, 0.04)),
  ulubiony_kolor = sample(c("niebieski", "czerwony", "zielony", "czarny", "bialy",
                            "rozowy", "fioletowy"), messy_n, replace = TRUE),
  stringsAsFactors = FALSE
)

# Tab 8: Ceny mieszkan - outliery i bledy (n=150)
apt_n <- 150
apt_powierzchnia <- round(runif(apt_n, 25, 120), 1)
apt_cena <- round(apt_powierzchnia * runif(apt_n, 4000, 7000) + rnorm(apt_n, 0, 20000))
apt_data <- data.frame(
  cena = apt_cena,
  powierzchnia = apt_powierzchnia,
  pokoje = pmin(5, pmax(1, round(apt_powierzchnia / 25))),
  dzielnica = factor(sample(c("Srodmiescie", "Mokotow", "Wola", "Praga", "Ursynow", "Bielany"),
                            apt_n, replace = TRUE)),
  rok_budowy = sample(1960:2024, apt_n, replace = TRUE),
  stringsAsFactors = FALSE
)
# Wprowadzenie bledow
apt_data$cena[3] <- 45           # brak zer
apt_data$cena[17] <- 5500000     # dodatkowe zero
apt_data$cena[42] <- -300000     # znak
apt_data$powierzchnia[28] <- 1200 # dodatkowe zero
apt_data$pokoje[55] <- 42        # blad klawiatury
apt_data$rok_budowy[71] <- 2204  # literowka

# Tab 9: Ankieta studencka - wzorcowa (n=150)
surv_n <- 150
surv_plec <- sample(c("Kobieta", "Mezczyzna"), surv_n, replace = TRUE, prob = c(0.55, 0.45))
survey_data <- data.frame(
  plec = factor(surv_plec),
  kierunek = factor(sample(c("Informatyka", "Biologia", "Psychologia", "Ekonomia"),
                           surv_n, replace = TRUE, prob = c(0.3, 0.2, 0.25, 0.25))),
  rok_studiow = factor(sample(1:5, surv_n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
                        levels = 1:5, ordered = TRUE),
  godziny_nauki = round(pmax(0, rnorm(surv_n, mean = 15, sd = 5)), 1),
  stres = round(pmin(10, pmax(1, rnorm(surv_n, mean = 5.5, sd = 2))), 0),
  srednia_ocen = round(pmin(5.0, pmax(2.0, rnorm(surv_n, mean = 3.8, sd = 0.6))), 2),
  liczba_kursow = sample(3:8, surv_n, replace = TRUE),
  stringsAsFactors = FALSE
)

# ============================================================================
# FUNKCJE POMOCNICZE
# ============================================================================

# Render verdict checklist
render_verdict <- function(criteria, type = "good") {
  bg_color <- switch(type, good = "#eafaf1", mixed = "#fef9e7", bad = "#fdedec")
  border_color <- switch(type, good = col_good, mixed = col_mixed, bad = col_bad)
  icon_yes <- "\u2705"
  icon_no <- "\u274c"
  icon_warn <- "\u26a0\ufe0f"

  critical_criteria <- c(
    "Dane odpowiadaja hipotezie badawczej",
    "Wystarczajaca liczba obserwacji (n \u2265 20-30 na grupe)",
    "Mix typow zmiennych (ilosciowe + jakosciowe)",
    "Zmiennosc w danych",
    "Struktura danych pasuje do planowanych analiz",
    "Niezaleznosc obserwacji"
  )

  fixable_criteria <- c(
    "Malo brakow danych (< 5%)",
    "Jednoznaczne definicje zmiennych",
    "Brak bledow i podejrzanych wartosci"
  )

  render_items <- function(criteria_labels, statuses, start_idx) {
    sapply(seq_along(criteria_labels), function(i) {
      status <- statuses[start_idx + i - 1]
      icon <- if (status == "yes") icon_yes else if (status == "warn") icon_warn else icon_no
      paste0("<div style='padding: 3px 0;'>", icon, " ", criteria_labels[i], "</div>")
    })
  }

  critical_items <- render_items(critical_criteria, criteria, 1)
  fixable_items <- render_items(fixable_criteria, criteria, 7)

  HTML(paste0(
    "<div style='background: ", bg_color, "; border-left: 4px solid ", border_color,
    "; padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;'>",
    "<strong>Werdykt:</strong>",
    "<div style='margin-top: 8px; font-size: 13px; color: #7f8c8d; font-weight: bold;'>KRYTYCZNE (wymagaja nowego zbioru):</div>",
    paste(critical_items, collapse = ""),
    "<div style='margin-top: 8px; font-size: 13px; color: #7f8c8d; font-weight: bold;'>NAPRAWIALNE (wymagaja pracy, ale sie da):</div>",
    paste(fixable_items, collapse = ""),
    "</div>"
  ))
}

# Safe as.numeric with error counting
safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Round all numeric columns in a data.frame to n digits
round_df <- function(df, digits = 2) {
  df[] <- lapply(df, function(x) {
    if (is.numeric(x)) round(x, digits) else x
  })
  df
}

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Co czyni dobry zbior danych?",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),

  header = tagList(
    tags$head(
    tags$style(HTML("
    /* Narrative text */
    .narrative { font-size: 16px; line-height: 1.7; color: #2c3e50; margin-bottom: 15px; }
    .narrative p { margin-bottom: 12px; }

    /* Widget containers */
    .widget-block {
      background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px;
      padding: 20px; margin: 25px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    }

    /* Headings */
    .section-title {
      font-size: 22px; font-weight: bold; color: #34495e;
      margin-top: 35px; margin-bottom: 15px;
      border-left: 4px solid #3498db; padding-left: 12px;
    }

    /* Alert/callout boxes */
    .callout-info {
      background: #eaf4fc; border-left: 4px solid #3498db;
      padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
    }
    .callout-warning {
      background: #fef9e7; border-left: 4px solid #f39c12;
      padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
    }
    .callout-danger {
      background: #fdedec; border-left: 4px solid #e74c3c;
      padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
    }
    .callout-success {
      background: #eafaf1; border-left: 4px solid #27ae60;
      padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
    }

    /* Chapter navigation */
    .chapter-transition {
      background: linear-gradient(135deg, #eaf4fc, #f0f7ee);
      border: 1px solid #b8d4e8; border-radius: 8px;
      padding: 20px; margin: 30px 0 15px 0; text-align: center;
    }
    .chapter-transition p {
      font-size: 16px; color: #2c3e50; margin-bottom: 12px;
    }
    .chapter-transition .btn {
      font-size: 16px; padding: 10px 30px;
    }

    /* Verdict badge */
    .verdict-badge {
      display: inline-block; padding: 4px 12px; border-radius: 12px;
      color: white; font-weight: bold; font-size: 13px;
    }
    .verdict-good { background: #27ae60; }
    .verdict-mixed { background: #f39c12; }
    .verdict-bad { background: #e74c3c; }

    /* Reveal sections */
    .reveal-section {
      border: 2px dashed #3498db; border-radius: 8px;
      padding: 15px; margin: 15px 0; background: #f0f8ff;
    }

    /* Sticky TOC */
    #sticky-toc {
      position: fixed; top: 70px; left: 10px; width: 180px;
      max-height: calc(100vh - 90px); overflow-y: auto;
      background: rgba(255,255,255,0.95); border: 1px solid #dee2e6;
      border-radius: 8px; padding: 10px 8px; font-size: 12px;
      z-index: 1000; box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    }
    #sticky-toc .toc-title {
      font-weight: bold; color: #2c3e50; margin-bottom: 6px;
      font-size: 13px; padding-bottom: 4px; border-bottom: 1px solid #dee2e6;
    }
    #sticky-toc a {
      display: block; padding: 3px 6px; color: #7f8c8d;
      text-decoration: none; border-radius: 4px; line-height: 1.3;
      margin-bottom: 2px;
    }
    #sticky-toc a:hover { color: #3498db; background: #eaf4fc; }
    #sticky-toc a.toc-active { color: #3498db; font-weight: bold; background: #eaf4fc; }
    @media (max-width: 1400px) { #sticky-toc { display: none !important; } }
  /* Mobile TOC toggle */
  #toc-mobile-btn {
    display: none; position: fixed; bottom: 20px; right: 20px;
    width: 48px; height: 48px; border-radius: 50%; background: #3498db;
    color: white; border: none; font-size: 22px; line-height: 48px;
    text-align: center; cursor: pointer; z-index: 1001;
    box-shadow: 0 2px 10px rgba(0,0,0,0.2);
  }
  #toc-mobile-btn:hover { background: #2980b9; }
  #toc-overlay {
    display: none; position: fixed; top: 0; left: 0; right: 0; bottom: 0;
    background: rgba(0,0,0,0.4); z-index: 1000;
  }
  @media (max-width: 1400px) {
    #toc-mobile-btn { display: block; }
    #sticky-toc.toc-open {
      display: block !important; position: fixed;
      top: 50%; left: 50%; transform: translate(-50%, -50%);
      width: 85%; max-width: 320px; max-height: 70vh;
      z-index: 1002; box-shadow: 0 4px 20px rgba(0,0,0,0.3);
    }
    #toc-overlay.toc-open { display: block; }
  }

    /* Quiz tiles */
    .quiz-tiles { display: grid; gap: 12px; margin: 15px 0; }
    .quiz-cols-2 { grid-template-columns: repeat(2, 1fr); }
    .quiz-cols-3 { grid-template-columns: repeat(3, 1fr); }
    .quiz-cols-4 { grid-template-columns: repeat(2, 1fr); }

    .quiz-tiles .quiz-tile {
      background: white; border: 2px solid #dee2e6; border-radius: 12px;
      padding: 20px 12px; text-align: center; cursor: pointer; transition: all 0.3s;
      display: block; width: 100%; font-family: inherit; color: inherit;
    }
    .quiz-tiles .quiz-tile:hover {
      border-color: #3498db; transform: translateY(-4px);
      box-shadow: 0 8px 25px rgba(52,152,219,0.2);
    }
    .quiz-tiles .quiz-tile:focus { outline: none; }
    .quiz-tiles .quiz-tile .tile-letter {
      display: inline-block; width: 36px; height: 36px; line-height: 36px;
      border-radius: 50%; background: #3498db; color: white;
      font-weight: 700; font-size: 16px; margin-bottom: 8px;
    }
    .quiz-tiles .quiz-tile .tile-text { font-size: 13px; color: #2c3e50; }
    .quiz-cols-4 .quiz-tile .tile-text { font-size: 12px; }
    .quiz-tile.correct { border-color: #27ae60 !important; background: #eafaf1 !important; }
    .quiz-tile.wrong { border-color: #e74c3c !important; background: #fdedec !important; }
    .quiz-tile.disabled { pointer-events: none; opacity: 0.7; }
  ")),
  tags$script(HTML("
    $(function() {
      var tocEl = $('<div id=\"sticky-toc\"></div>').appendTo('body');
      function buildToc() {
        var activeTab = $('.tab-pane.active');
        if (!activeTab.length) return;
        var sections = activeTab.find('.section-title');
        if (sections.length < 2) { tocEl.hide(); return; }
        var html = '<div class=\"toc-title\">Spis tresci</div>';
        sections.each(function(i) {
          var el = $(this); var id = 'toc-sec-' + i; el.attr('id', id);
          var text = el.text().trim();
          if (text.length > 35) text = text.substring(0, 33) + '...';
          html += '<a href=\"#' + id + '\">' + text + '</a>';
        });
        tocEl.html(html).show();
      }
      function updateActive() {
        var scrollTop = $(window).scrollTop(); var current = null;
        $('.tab-pane.active .section-title').each(function() {
          if ($(this).offset().top - 100 <= scrollTop) current = $(this).attr('id');
        });
        tocEl.find('a').removeClass('toc-active');
        if (current) tocEl.find('a[href=\"#' + current + '\"]').addClass('toc-active');
      }
      tocEl.on('click', 'a', function(e) {
        e.preventDefault();
        var target = $($(this).attr('href'));
        if (target.length) $('html, body').animate({ scrollTop: target.offset().top - 60 }, 300);
      });
      $(document).on('shown.bs.tab', function() { setTimeout(buildToc, 150); });
      $(window).on('scroll', updateActive);
      setTimeout(buildToc, 500);
    });
  "))
  )),

  # ==========================================================================
  # TAB 0: WPROWADZENIE
  # ==========================================================================
  tabPanel("0. Wprowadzenie",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Zanim zaczniesz analize - ocen swoje dane"),

    div(class = "narrative",
      p("Na tym kursie bedziemy wykonywac rozne analizy statystyczne: testy hipotez,
        korelacje, regresje. Kazda z tych metod wymaga, zeby dane spelnialy okreslone warunki."),
      p("Zanim zaczniesz liczyc - zatrzymaj sie i ocen swoj zbior danych.
        Ponizszy checklist pomoze Ci to zrobic systematycznie.")
    ),

    div(class = "widget-block",
      h4("Checklist jakosci danych"),
      tags$p(tags$strong(style = "color: #e74c3c;", "KRYTYCZNE"),
        " - jesli nie spelniasz, szukaj innego zbioru:"),
      checkboxGroupInput("intro_critical", NULL,
        choices = c(
          "Dane odpowiadaja hipotezie badawczej (mierza to, co chcesz badac)" = "hyp",
          "Wystarczajaca liczba obserwacji (n \u2265 20-30 na grupe/podgrupe)" = "n",
          "Mix typow zmiennych (ilosciowe + jakosciowe)" = "mix",
          "Zmiennosc w danych (nie wszystko takie samo)" = "var",
          "Struktura danych pasuje do planowanych analiz" = "fit",
          "Niezaleznosc obserwacji (lub mozliwosc agregacji)" = "indep"
        )
      ),
      tags$p(tags$strong(style = "color: #f39c12;", "NAPRAWIALNE"),
        " - wymagaja pracy, ale sie da:"),
      checkboxGroupInput("intro_fixable", NULL,
        choices = c(
          "Malo brakow danych (< 5%)" = "missing",
          "Jednoznaczne definicje zmiennych" = "def",
          "Brak bledow i podejrzanych wartosci" = "errors"
        )
      ),
      uiOutput("intro_thermometer")
    ),

    div(class = "callout-info",
      tags$strong("Zadanie:"),
      " W kolejnych zakladkach zobaczysz 10 zbiorow danych. ",
      "Sprobuj sam ocenic kazdy, zanim zobaczysz werdykt."
    ),

    div(class = "chapter-transition",
      p("Zaczynamy od wzorcowego zbioru danych."),
      actionButton("ch0_next", "Dalej: 1. Szkoly w Kalifornii \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 1: SZKOLY W KALIFORNII (CASchool) - DOBRY
  # ==========================================================================
  tabPanel("1. Szkoly",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Szkoly w Kalifornii"),

    div(class = "narrative",
      p("Zbior danych z 420 okregow szkolnych w Kalifornii. Zawiera wyniki testow
        standaryzowanych, wydatki na ucznia, dochody w okregu i dane demograficzne."),
      p("Zrodlo: pakiet AER w R (Academic Economic Research).")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab1_table")
    ),

    div(class = "callout-info",
      tags$strong("Zmienne:"), " district, school (identyfikatory), ",
      "students, teachers (liczebnosci), expenditure (wydatki/ucznia $), ",
      "income (sredni dochod w okregu $tys.), english (% uczniow uczacych sie angielskiego), ",
      "lunch (% uczniow z darmowym lunchem), calworks (% rodzin na zasilku), ",
      "read, math (wyniki testow Stanford 9)."
    ),

    div(class = "section-title", "Eksploracja zmiennych"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab1_var", "Wybierz zmienna:",
          choices = c("read", "math", "expenditure", "income", "english", "lunch",
                      "students", "teachers", "calworks"))),
        column(8, plotOutput("tab1_hist", height = "300px"))
      ),
      verbatimTextOutput("tab1_summary")
    ),

    div(class = "section-title", "Zaleznosci miedzy zmiennymi"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab1_x", "Zmienna X:", choices = c("expenditure", "income", "english", "lunch", "calworks", "students"), selected = "income")),
        column(4, selectInput("tab1_y", "Zmienna Y:", choices = c("read", "math"), selected = "read"))
      ),
      plotOutput("tab1_scatter_plot", height = "350px")
    ),

    div(class = "section-title", "Werdykt"),

    uiOutput("tab1_verdict"),

    div(class = "chapter-transition",
      p("To byl wzorcowy zbior. Nastepny bedzie... inny."),
      actionButton("ch1_next", "Dalej: 2. Ankieta na grupie \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 2: ZA MALO DANYCH - ZLY
  # ==========================================================================
  tabPanel("2. Grupa",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ankieta na grupie"),

    div(class = "narrative",
      p("Kolega zbiera dane do projektu. Dzien przed deadline'em pyta 8 znajomych
        ze swojej grupy. Oto co uzyskal:")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab2_table")
    ),

    div(class = "section-title", "Ile obserwacji naprawde potrzebujesz?"),

    div(class = "widget-block",
      sliderInput("tab2_n", "Liczba obserwacji:", min = 5, max = 200, value = 8, step = 1),
      fluidRow(
        column(6, plotOutput("tab2_hist", height = "280px")),
        column(6, plotOutput("tab2_ci", height = "280px"))
      ),
      plotOutput("tab2_power", height = "280px")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("Problem:"), " n = 8 to zdecydowanie za malo.",
      tags$br(),
      "Przy tak malej probie moc testu wynosi ok. 10-15% - nawet duza roznica ",
      "miedzy grupami bedzie nieistotna statystycznie.",
      tags$br(), tags$br(),
      tags$strong("Zasada:"), " Liczy sie n na grupe, nie n ogolne! ",
      "Jesli porownujesz 3 grupy i masz n = 30, to tylko 10 na grupe - wciaz za malo.",
      tags$br(),
      "Minimum 20-30 obserwacji w kazdej podgrupie, ktora chcesz analizowac. ",
      "Regresja z k predyktorami potrzebuje n > 10k + 50."
    ),

    uiOutput("tab2_verdict"),

    div(class = "chapter-transition",
      p("Zobaczmy teraz zbior, ktory radzi sobie lepiej."),
      actionButton("ch2_next", "Dalej: 3. Pingwiny \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 3: PINGWINY (palmerpenguins) - DOBRY
  # ==========================================================================
  tabPanel("3. Pingwiny",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Pingwiny z Antarktydy"),

    div(class = "narrative",
      p("Dane z badania 344 pingwinow trzech gatunkow (Adelie, Chinstrap, Gentoo)
        na trzech wyspach archipelagu Palmera na Antarktydzie.
        Pomiary ciala: dziob, pletwy, masa."),
      p("Zrodlo: pakiet palmerpenguins w R (Horst, Hill & Gorman, 2020).")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab3_table")
    ),

    div(class = "section-title", "Czy sa braki danych?"),

    div(class = "widget-block",
      plotOutput("tab3_missing", height = "250px"),
      uiOutput("tab3_missing_info")
    ),

    div(class = "section-title", "Eksploracja"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab3_var", "Zmienna:",
          choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"))),
        column(8, plotOutput("tab3_boxplot", height = "300px"))
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Dobry zbior!"),
      " n = 344, trzy zbalansowane grupy gatunkow, jasno zdefiniowane zmienne pomiarowe.",
      tags$br(),
      "Niewielkie braki danych (< 3%) - mozna je bezpiecznie usunac (listwise deletion).",
      tags$br(),
      "Mozliwe analizy: test t, ANOVA, korelacja, regresja, chi-kwadrat."
    ),

    uiOutput("tab3_verdict"),

    div(class = "chapter-transition",
      p("Nastepny zbior wyglada ciekawie... ale czy nadaje sie do analizy?"),
      actionButton("ch3_next", "Dalej: 4. Filmy Tarantino \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 4: FILMY TARANTINO - ZLY
  # ==========================================================================
  tabPanel("4. Tarantino",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Filmy Tarantino"),

    div(class = "narrative",
      p("Kolega znalazl ciekawy zbior danych o filmach Quentina Tarantino.
        Zawiera informacje o kazdym przeklenstwie i kazdej smierci w jego filmach.
        'Super temat na projekt!' - mowi."),
      p("Zrodlo: pakiet fivethirtyeight w R.")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab4_table")
    ),

    div(class = "callout-info",
      tags$strong("Zmienne:"),
      " movie (tytul filmu), type ('word' lub 'death'), ",
      "word (konkretne slowo, jesli type='word'), minutes_in (minuta filmu)."
    ),

    div(class = "section-title", "Eksploracja"),

    div(class = "widget-block",
      fluidRow(
        column(6, actionButton("tab4_hist", "Histogram: minutes_in", class = "btn-outline-primary", width = "100%")),
        column(6, actionButton("tab4_bar", "Porownanie filmow", class = "btn-outline-primary", width = "100%"))
      ),
      plotOutput("tab4_explore_plot", height = "350px")
    ),

    div(class = "section-title", "Proba analiz"),

    div(class = "widget-block",
      h4("Jaka analiza tu pasuje?"),
      uiOutput("tab4_quiz_options"),
      uiOutput("tab4_quiz_result")
    ),

    div(class = "widget-block",
      h4("Moze agregacja pomoze?"),
      div(class = "narrative",
        p("Kazdy wiersz to jedno zdarzenie (przeklenstwo lub smierc). Aby uzywac klasycznej
          statystyki, musielibysmy zagregowac dane do poziomu filmow.")
      ),
      actionButton("tab4_aggregate", "Zagreguj dane", class = "btn-warning"),
      uiOutput("tab4_agg_result")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("Zly zbior do klasycznej statystyki!"),
      tags$br(),
      tags$strong("Problem 1:"), " Dane eventowe - kazdy wiersz to zdarzenie, nie obserwacja w sensie statystycznym.",
      tags$br(),
      tags$strong("Problem 2:"), " Po agregacji do poziomu filmow mamy n = 7. To za malo na jakakolwiek analize.",
      tags$br(),
      tags$strong("Problem 3:"), " Brak zmiennych ilosciowych do korelacji/regresji."
    ),

    uiOutput("tab4_verdict"),

    div(class = "chapter-transition",
      p("Czasem dane maja odpowiednia wielkosc, ale inny problem..."),
      actionButton("ch4_next", "Dalej: 5. Ankieta firmowa \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 5: ANKIETA FIRMOWA - ZLY (brak zmiennosci)
  # ==========================================================================
  tabPanel("5. Firma",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ankieta firmowa"),

    div(class = "narrative",
      p("Firma przeprowadza anonimowa ankiete zadowolenia pracownikow.
        Problem w tym, ze wszyscy wiedza, ze szef ja czyta...
        Zebrano dane od 80 pracownikow.")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab5_table")
    ),

    div(class = "section-title", "Zmienna 1: Zadowolenie z pracy"),

    div(class = "widget-block",
      plotOutput("tab5_plot_zadowolenie", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem: brak zroznicowania odpowiedzi."),
      " 95% pracownikow zaznaczylo 4 lub 5. To klasyczny efekt aprobaty spolecznej \u2014
      wszyscy wiedza, ze szef czyta ankiete. Skala 1\u20135 w praktyce dziala tu jak skala 1\u20132."
    ),

    div(class = "section-title", "Zmienna 2: Dzial"),

    div(class = "widget-block",
      plotOutput("tab5_plot_departament", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem: niezbalansowane grupy."),
      " 94% respondentow to dzial IT. Pozostale dzialy maja po 1\u20132 osoby \u2014
      jakiekolwiek porownanie miedzy dzialami bedzie niemozliwe."
    ),

    div(class = "section-title", "Zmienna 3: Staz pracy"),

    div(class = "widget-block",
      plotOutput("tab5_plot_staz", height = "300px")
    ),
    div(class = "callout-warning",
      tags$strong("Uwaga: waska rozpitosc wartosci."),
      " Wszyscy pracownicy maja staz w przedziale 2.8\u20133.5 roku. Sama w sobie mala zmiennosc
      nie jest bledem \u2014 zdarzaja sie takie dane. Ale gdy ",
      tags$em("caly zbior"), " wyglada podobnie, wykrycie jakichkolwiek zaleznosci staje sie
      bardzo trudne."
    ),

    div(class = "section-title", "Zmienna 4: Wynagrodzenie"),

    div(class = "widget-block",
      plotOutput("tab5_plot_wynagrodzenie", height = "300px")
    ),
    div(class = "callout-warning",
      tags$strong("Uwaga: waska rozpitosc wartosci."),
      " Wynagrodzenia mieszcza sie w przedziale 4800\u20135200 PLN \u2014 rozstep to tylko 400 PLN.
      Podobnie jak ze stazem: sama w sobie to nie jest katastrofa, ale razem z pozostalymi
      zmiennymi tworzy zbior, w ktorym trudno o jakikolwiek interesujacy sygnal."
    ),

    div(class = "section-title", "Zmienna 5: Plec"),

    div(class = "widget-block",
      plotOutput("tab5_plot_plec", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem: niezbalansowane grupy."),
      " 90% respondentow to mezczyzni (ok. 72 os.), kobiet jest ok. 8. Porownanie
      wedlug plci nie ma sensu przy takiej dysproporcji."
    ),

    div(class = "section-title", "Co sie dzieje gdy probujemy szukac zaleznosci?"),

    div(class = "callout-info",
      "Wezmy dwie zmienne ilosciowe \u2014 staz pracy i wynagrodzenie \u2014 i sprawdzmy
      czy miedzy nimi jest jakis zwiazek. Obie maja waska rozpitosc, wiec..."
    ),

    div(class = "widget-block",
      plotOutput("tab5_scatter", height = "300px")
    ),

    div(class = "section-title", "Co by bylo, gdyby dane mialy normalna zmiennosc?"),

    div(class = "callout-info",
      "Przesuniecie suwaka symuluje sytuacje, w ktorej staz i wynagrodzenia mialy
      szerszy rozrzut. Obserwuj jak zmienia sie korelacja."
    ),

    div(class = "widget-block",
      sliderInput("tab5_sd_mult", "Mnoznik rozrzutu danych:", min = 1, max = 5, value = 1, step = 0.5),
      plotOutput("tab5_scatter_sim", height = "300px")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("Ten zbior danych nie nadaje sie do analizy."),
      tags$br(),
      "Kazda zmienna z osobna wyglada niegroznie, ale lacznie: odpowiedzi skupione
      przy maksimum, dzialy i plec skrajnie niezbalansowane, zmienne ilosciowe
      bez zadnego zroznicowania. Nie ma tu czego analizowac."
    ),

    uiOutput("tab5_verdict"),

    div(class = "chapter-transition",
      p("Pora na duzy, dobry zbior danych."),
      actionButton("ch5_next", "Dalej: 6. Wynagrodzenia \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 6: WYNAGRODZENIA (Wage) - DOBRY
  # ==========================================================================
  tabPanel("6. Wynagrodzenia",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Wynagrodzenia w USA"),

    div(class = "narrative",
      p("Dane z Current Population Survey: 3000 mezczyzn z regionu Mid-Atlantic.
        Informacje o zarobkach, wyksztalceniu, zawodzie, wieku i zdrowiu."),
      p("Zrodlo: pakiet ISLR w R (Introduction to Statistical Learning).")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab6_table")
    ),

    div(class = "section-title", "Eksploracja"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab6_var", "Wybierz zmienna:",
          choices = c("wage", "age", "education", "jobclass", "health", "maritl", "race"))),
        column(8, plotOutput("tab6_hist", height = "300px"))
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Bardzo dobry zbior!"),
      " n = 3000, kompletne dane, bogaty mix zmiennych ilosciowych i jakosciowych.",
      tags$br(),
      "Mozliwe analizy: test t, ANOVA, korelacja, regresja wieloraka, chi-kwadrat.",
      tags$br(),
      tags$em("Ale uwaga: dane tylko dla mezczyzn z jednego regionu USA - ",
              "ograniczona generalizowalnosc.")
    ),

    uiOutput("tab6_verdict"),

    div(class = "chapter-transition",
      p("Nastepny zbior to przyklad zlej ankiety."),
      actionButton("ch6_next", "Dalej: 7. Trudna ankieta \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 7: TRUDNA ANKIETA - ZLY (zle zmienne)
  # ==========================================================================
  tabPanel("7. Ankieta",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Trudna ankieta"),

    div(class = "narrative",
      p("Student zaprojektowal ankiete bez konsultacji z prowadzacym i bez pilotazu.
        Rozeslal ja na grupie i zebrala 90 odpowiedzi. Oto wynik:")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab7_table")
    ),

    div(class = "section-title", "Sprobuj policzyc srednia"),

    div(class = "widget-block",
      selectInput("tab7_var", "Wybierz zmienna:",
        choices = c("czas_na_studia", "ocena_kursu", "aktywnosc", "samopoczucie", "ulubiony_kolor")),
      actionButton("tab7_mean", "Policz srednia", class = "btn-primary"),
      uiOutput("tab7_mean_result")
    ),

    div(class = "section-title", "Jak to naprawic?"),

    div(class = "widget-block",
      radioButtons("tab7_toggle", "Widok danych:", choices = c("Surowe", "Oczyszczone"), inline = TRUE),
      DT::dataTableOutput("tab7_clean_table"),
      uiOutput("tab7_clean_info")
    ),

    div(class = "callout-info",
      tags$strong("Jak tego uniknac:"),
      tags$br(),
      "1. Zamkniete pytania (gotowe opcje do wyboru)",
      tags$br(),
      "2. Spojne skale (np. zawsze 1-10 albo zawsze 1-5)",
      tags$br(),
      "3. Pilotaz ankiety (przetestuj na 5 osobach przed rozeslaniem)",
      tags$br(),
      "4. Jasna instrukcja (np. 'podaj liczbe godzin tygodniowo')"
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("Dane wymagaja gruntownego czyszczenia!"),
      tags$br(),
      "Zmienne tekstowe zamiast liczbowych, niespojne skale, brak kodowania.",
      tags$br(),
      "Zmienna 'ulubiony_kolor' jest irrelewantna - nie wiaze sie z zadnym pytaniem badawczym.",
      tags$br(),
      "R nie wie, co zrobic z '3-4h' albo 'dobrze' jako wartoscia liczbowa."
    ),

    uiOutput("tab7_verdict"),

    div(class = "chapter-transition",
      p("Nastepny zbior ma inny rodzaj problemow - bledy w danych."),
      actionButton("ch7_next", "Dalej: 8. Ceny mieszkan \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 8: CENY MIESZKAN - MIESZANY (outliery)
  # ==========================================================================
  tabPanel("8. Mieszkania",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ceny mieszkan"),

    div(class = "narrative",
      p("Dane z portalu z ogloszeniami nieruchomosci - 150 ofert skopiowanych do Excela.
        Chcemy zbadac zaleznosc ceny od powierzchni.")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab8_table")
    ),

    div(class = "section-title", "Cena vs powierzchnia"),

    div(class = "widget-block",
      plotOutput("tab8_scatter_raw", height = "350px")
    ),

    div(class = "section-title", "Szukanie outlierow"),

    div(class = "widget-block",
      selectInput("tab8_var", "Zmienna do boxplotu:",
        choices = c("cena", "powierzchnia", "pokoje", "rok_budowy")),
      plotOutput("tab8_boxplot", height = "300px")
    ),

    div(class = "widget-block",
      checkboxInput("tab8_clean", "Usun podejrzane obserwacje", value = FALSE),
      conditionalPanel("input.tab8_clean",
        plotOutput("tab8_scatter_clean", height = "350px")
      )
    ),

    div(class = "section-title", "Quiz: blad czy prawdziwy outlier?"),

    div(class = "widget-block",
      uiOutput("tab8_quiz"),
      actionButton("tab8_check_quiz", "Sprawdz odpowiedzi", class = "btn-primary"),
      uiOutput("tab8_quiz_result")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-warning",
      tags$strong("Dane dobre po czyszczeniu!"),
      tags$br(),
      "Podstawowa struktura zbioru jest dobra (n=150, zroznicowane zmienne, jasne definicje).",
      tags$br(),
      "Ale bledy wprowadzania danych drastycznie zaburzaja wyniki (R\u00b2 skacze po ich usunieciu).",
      tags$br(),
      tags$strong("Klucz:"), " Rozroznij blad danych (usun) od prawdziwego outliera (przemysl zachowanie)."
    ),

    uiOutput("tab8_verdict"),

    div(class = "chapter-transition",
      p("Nastepny zbior to przyklad dobrze zaprojektowanej ankiety."),
      actionButton("ch8_next", "Dalej: 9. Ankieta studencka \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 9: ANKIETA STUDENCKA - DOBRY
  # ==========================================================================
  tabPanel("9. Studenci",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ankieta studencka"),

    div(class = "narrative",
      p("Wyobraz sobie, ze projektujesz ankiete do projektu koncowego.
        Oto przyklad dobrze zaprojektowanej ankiety z 150 respondentami.")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab9_table")
    ),

    div(class = "callout-info",
      tags$strong("Zmienne i ich typy:"),
      tags$br(),
      "plec (nominalna) | kierunek (nominalna) | rok_studiow (porzadkowa)",
      tags$br(),
      "godziny_nauki (ciagla) | stres (porzadkowa/Likert 1-10) | srednia_ocen (ciagla) | liczba_kursow (dyskretna)"
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Wzorcowa ankieta!"),
      tags$br(),
      "Zamkniete pytania, spojne skale, jasne kodowanie.",
      tags$br(),
      "n = 150, mix typow zmiennych, kazda analiza z kursu jest mozliwa.",
      tags$br(),
      tags$em("Porownaj z Trudna ankieta (tab 7) - te same tematy, ale swiat roznic w jakosci!")
    ),

    uiOutput("tab9_verdict"),

    div(class = "chapter-transition",
      p("Ostatni zbior - wyglada dobrze, ale ma ukryty problem..."),
      actionButton("ch9_next", "Dalej: 10. Jakosc powietrza \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 10: JAKOSC POWIETRZA (airquality) - ZLY
  # ==========================================================================
  tabPanel("10. Powietrze",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Jakosc powietrza w Nowym Jorku"),

    div(class = "narrative",
      p("Dane o jakosci powietrza w Nowym Jorku. 153 pomiary z lata 1973.
        Zmienne: Ozone (ppb), Solar.R (promieniowanie), Wind (mph), Temp (F)."),
      p("Zrodlo: wbudowany zbior 'airquality' w R.")
    ),

    div(class = "section-title", "Podglad danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab10_table")
    ),

    div(class = "section-title", "Czy sa braki danych?"),

    div(class = "widget-block",
      plotOutput("tab10_missing", height = "300px"),
      uiOutput("tab10_missing_info")
    ),

    div(class = "section-title", "Odkryj ukryty problem"),

    div(class = "widget-block",
      actionButton("tab10_reveal", "Pokaz dane w kolejnosci", class = "btn-warning btn-lg", width = "100%"),
      conditionalPanel("input.tab10_reveal > 0",
        plotOutput("tab10_lineplot", height = "350px"),
        div(class = "callout-danger",
          tags$strong("To nie sa niezalezne obserwacje!"),
          " To pomiary dzienne - widac wyrazna sezonowosc.",
          tags$br(),
          "Temperatura i ozon zmieniaja sie sezonowo - kazdy dzien zalezy od poprzedniego."
        )
      )
    ),

    conditionalPanel("input.tab10_reveal > 0",
      div(class = "widget-block",
        h4("Autokorelacja - dowod braku niezaleznosci"),
        plotOutput("tab10_lag", height = "300px"),
        uiOutput("tab10_autocorr_info")
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("DWA powazne problemy:"),
      tags$br(),
      tags$strong("1. Braki danych:"), " Ozone ma 24% brakow (37 z 153). ",
      "Po usunieciu brakow zostaje 111 obserwacji.",
      tags$br(),
      tags$strong("2. Brak niezaleznosci:"), " To szereg czasowy! ",
      "Obserwacje dzienne sa silnie autokorelowane.",
      tags$br(),
      "Klasyczne testy (t-test, korelacja Pearsona) zakladaja niezaleznosc obserwacji - ",
      "tutaj to zalozenie jest zlamane."
    ),

    uiOutput("tab10_verdict"),

    div(class = "chapter-transition",
      p("To byl ostatni zbior danych. Zobaczmy podsumowanie."),
      actionButton("ch10_next", "Dalej: Sciaga \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 11: SCIAGA
  # ==========================================================================
  tabPanel("11. Sciaga",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Sciaga - jak ocenic zbior danych"),

    div(class = "section-title", "Podsumowanie 10 zbiorow"),

    div(class = "widget-block",
      tableOutput("tab11_summary")
    ),

    div(class = "section-title", "Checklist jakosci danych"),

    div(class = "callout-danger",
      HTML("
        <strong style='font-size: 15px;'>KRYTYCZNE - jesli nie spelniasz, szukaj innego zbioru:</strong>
        <ol>
          <li><strong>Czy dane odpowiadaja hipotezie badawczej?</strong> Najpierw sformuluj co chcesz badac, potem sprawdz czy dane to mierza.</li>
          <li><strong>Czy masz n &ge; 20-30 na grupe?</strong> Liczy sie n w kazdej podgrupie. Porownujesz 3 grupy? Potrzebujesz 3 &times; 30 = 90.</li>
          <li><strong>Czy masz mix typow zmiennych?</strong> Ilosciowe do korelacji/regresji, jakosciowe do t-testow i chi-kwadrat.</li>
          <li><strong>Czy jest zmiennosc?</strong> SD &asymp; 0 oznacza brak mozliwosci analizy.</li>
          <li><strong>Czy struktura danych pasuje do analiz?</strong> Sprawdz czy masz odpowiednie zmienne do kazdej planowanej analizy.</li>
          <li><strong>Czy obserwacje sa niezalezne?</strong> Dane czasowe lub z klastrow wymagaja specjalnych metod (lub agregacji).</li>
        </ol>
      ")
    ),

    div(class = "callout-warning",
      HTML("
        <strong style='font-size: 15px;'>NAPRAWIALNE - wymagaja pracy, ale sie da:</strong>
        <ol start='7'>
          <li><strong>Czy braki &lt; 5%?</strong> Mozna usunac obserwacje z brakami lub imputowac. Powyzej 20-30% w zmiennej - ta zmienna moze odpasc.</li>
          <li><strong>Czy zmienne sa jednoznacznie zdefiniowane?</strong> Mozna rekodowac, przejsc na rangi - ale kazda decyzja ma konsekwencje.</li>
          <li><strong>Czy nie ma bledow i outlierow?</strong> Sprawdz zakresy, literowki. Odrozniaj bledy (usun) od prawdziwych outlierow (przemysl).</li>
        </ol>
      ")
    ),

    div(class = "section-title", "Dopasowanie analizy do danych"),

    div(class = "widget-block",
      tableOutput("tab11_analysis_table")
    ),

    div(class = "callout-info",
      tags$strong("Wskazowka:"),
      " Uzyj tego checklistu oceniajac dane do swojego projektu koncowego.",
      tags$br(),
      "Jesli nie spelniasz kryteriow krytycznych - szukaj innego zbioru.",
      tags$br(),
      "Jesli masz problemy naprawialne - mozesz pracowac z tymi danymi, ale zaplanuj czas na czyszczenie."
    ),

    div(style = "height: 60px;")
  )))

) # end navbarPage

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA
  # ==========================================================================

  observeEvent(input$ch0_next, { updateNavbarPage(session, "main_nav", selected = "1. Szkoly") })
  observeEvent(input$ch1_next, { updateNavbarPage(session, "main_nav", selected = "2. Grupa") })
  observeEvent(input$ch2_next, { updateNavbarPage(session, "main_nav", selected = "3. Pingwiny") })
  observeEvent(input$ch3_next, { updateNavbarPage(session, "main_nav", selected = "4. Tarantino") })
  observeEvent(input$ch4_next, { updateNavbarPage(session, "main_nav", selected = "5. Firma") })
  observeEvent(input$ch5_next, { updateNavbarPage(session, "main_nav", selected = "6. Wynagrodzenia") })
  observeEvent(input$ch6_next, { updateNavbarPage(session, "main_nav", selected = "7. Ankieta") })
  observeEvent(input$ch7_next, { updateNavbarPage(session, "main_nav", selected = "8. Mieszkania") })
  observeEvent(input$ch8_next, { updateNavbarPage(session, "main_nav", selected = "9. Studenci") })
  observeEvent(input$ch9_next, { updateNavbarPage(session, "main_nav", selected = "10. Powietrze") })
  observeEvent(input$ch10_next, { updateNavbarPage(session, "main_nav", selected = "11. Sciaga") })

  # ==========================================================================
  # TAB 0: WPROWADZENIE
  # ==========================================================================

  output$intro_thermometer <- renderUI({
    n_critical <- length(input$intro_critical)
    n_fixable <- length(input$intro_fixable)
    n_total <- n_critical + n_fixable
    pct <- n_total / 9 * 100

    # Krytyczne decyduja o kolorze
    if (n_critical <= 3) {
      color <- col_bad
      label <- "Dane wymagaja pracy - problemy krytyczne!"
    } else if (n_critical <= 4 || n_total <= 6) {
      color <- col_mixed
      label <- "Dane OK z zastrzezeniami"
    } else {
      color <- col_good
      label <- "Dane gotowe do analizy!"
    }

    tagList(
      div(style = "background: #ecf0f1; border-radius: 10px; height: 30px; margin-top: 15px;",
        div(style = paste0("background: ", color, "; height: 30px; border-radius: 10px; width: ", pct, "%;
                            transition: width 0.3s; text-align: center; line-height: 30px; color: white; font-weight: bold;"),
          paste0(n_total, "/9")
        )
      ),
      div(style = paste0("text-align: center; margin-top: 8px; font-weight: bold; color: ", color, ";"), label),
      if (n_critical < 6 && n_fixable > 0)
        div(style = "text-align: center; margin-top: 4px; font-size: 13px; color: #7f8c8d;",
          "Naprawialne kryteria nie ratuja krytycznych problemow!")
    )
  })

  # ==========================================================================
  # TAB 1: SZKOLY W KALIFORNII
  # ==========================================================================

  output$tab1_table <- DT::renderDataTable({
    datatable(round_df(CASchools[, c("district", "school", "students", "teachers", "expenditure",
                            "income", "english", "lunch", "calworks", "read", "math")]),
              options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab1_hist <- renderPlot({
    req(input$tab1_var)
    ggplot(CASchools, aes(x = .data[[input$tab1_var]])) +
      geom_histogram(bins = 25, fill = col_primary, color = "white", alpha = 0.8) +
      labs(title = paste("Rozklad:", input$tab1_var), x = input$tab1_var, y = "Liczebnosc") +
      theme_minimal(base_size = 14)
  })

  output$tab1_summary <- renderPrint({
    req(input$tab1_var)
    summary(CASchools[[input$tab1_var]])
  })

  output$tab1_scatter_plot <- renderPlot({
    ggplot(CASchools, aes(x = .data[[input$tab1_x]], y = .data[[input$tab1_y]])) +
      geom_point(alpha = 0.5, color = col_dark) +
      geom_smooth(method = "lm", color = col_primary, se = TRUE) +
      labs(title = paste(input$tab1_y, "~", input$tab1_x),
           x = input$tab1_x, y = input$tab1_y) +
      theme_minimal(base_size = 14)
  })

  output$tab1_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    render_verdict(rep("yes", 9), "good")
  })

  # ==========================================================================
  # TAB 2: ZA MALO DANYCH
  # ==========================================================================

  output$tab2_table <- DT::renderDataTable({
    datatable(round_df(small_data), options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })

  # Slider simulations
  sim_data <- reactive({
    n <- input$tab2_n
    set.seed(42)
    data.frame(
      godziny = rnorm(n, 15, 5),
      oceny = rnorm(n, 3.8, 0.6)
    )
  })

  output$tab2_hist <- renderPlot({
    d <- sim_data()
    ggplot(d, aes(x = oceny)) +
      geom_histogram(bins = max(5, input$tab2_n / 5), fill = col_primary, color = "white", alpha = 0.8) +
      labs(title = paste0("Histogram (n = ", input$tab2_n, ")"), x = "Srednia ocen", y = "Liczebnosc") +
      theme_minimal(base_size = 14)
  })

  output$tab2_ci <- renderPlot({
    ns <- seq(5, 200, by = 5)
    ci_widths <- 2 * qt(0.975, ns - 1) * 0.6 / sqrt(ns)  # assuming SD = 0.6
    df_ci <- data.frame(n = ns, ci_width = ci_widths)

    ggplot(df_ci, aes(x = n, y = ci_width)) +
      geom_line(color = col_bad, linewidth = 1.2) +
      geom_point(data = df_ci[df_ci$n == max(ns[ns <= input$tab2_n]), ],
                 color = col_bad, size = 4) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = col_good) +
      annotate("text", x = 150, y = 0.55, label = "Akceptowalna szerokosc", color = col_good, size = 4) +
      labs(title = "Szerokosc 95% CI", x = "Liczba obserwacji (n)", y = "Szerokosc CI") +
      theme_minimal(base_size = 14)
  })

  output$tab2_power <- renderPlot({
    ns <- seq(5, 200, by = 5)
    # Power simulation: detect effect size d=0.5
    powers <- sapply(ns, function(n) {
      set.seed(123)
      rejections <- replicate(500, {
        x <- rnorm(n / 2, 0, 1)
        y <- rnorm(n / 2, 0.5, 1)  # effect size d = 0.5
        t.test(x, y)$p.value < 0.05
      })
      mean(rejections)
    })
    df_pow <- data.frame(n = ns, power = powers)

    ggplot(df_pow, aes(x = n, y = power)) +
      geom_line(color = col_primary, linewidth = 1.2) +
      geom_point(data = df_pow[df_pow$n == max(ns[ns <= input$tab2_n]), ],
                 color = col_primary, size = 4) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = col_good) +
      annotate("text", x = 150, y = 0.83, label = "Moc = 80% (standard)", color = col_good, size = 4) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(title = "Moc testu (effect size d = 0.5)", x = "Liczba obserwacji (n)", y = "Moc testu") +
      theme_minimal(base_size = 14)
  })

  output$tab2_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    render_verdict(c("yes", "no", "yes", "yes", "no", "yes", "yes", "yes", "yes"), "bad")
  })

  # ==========================================================================
  # TAB 3: PINGWINY
  # ==========================================================================

  output$tab3_table <- DT::renderDataTable({
    datatable(round_df(penguins), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab3_missing <- renderPlot({
    miss_pct <- sapply(penguins, function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss$color <- ifelse(df_miss$pct > 5, col_bad, ifelse(df_miss$pct > 0, col_mixed, col_good))

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct, fill = color)) +
      geom_col() +
      scale_fill_identity() +
      geom_hline(yintercept = 5, linetype = "dashed", color = col_bad) +
      annotate("text", x = 2, y = 6, label = "Prog 5%", color = col_bad, size = 4) +
      labs(title = "Procent brakow danych", x = NULL, y = "% brakow") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$tab3_missing_info <- renderUI({
    n_complete <- sum(complete.cases(penguins))
    n_total <- nrow(penguins)
    div(class = "callout-info",
      paste0("Kompletne obserwacje: ", n_complete, " z ", n_total,
             " (", round(n_complete / n_total * 100, 1), "%). ",
             "Braki dotycza glownie zmiennej sex (", sum(is.na(penguins$sex)), " NA).")
    )
  })

  output$tab3_boxplot <- renderPlot({
    req(input$tab3_var)
    ggplot(penguins %>% filter(!is.na(.data[[input$tab3_var]])),
           aes(x = species, y = .data[[input$tab3_var]], fill = species)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c(col_primary, col_mixed, col_good)) +
      labs(title = paste(input$tab3_var, "wg gatunku"), x = "Gatunek", y = input$tab3_var) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })

  output$tab3_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    render_verdict(c("yes", "yes", "yes", "yes", "yes", "yes", "warn", "yes", "yes"), "good")
  })

  # ==========================================================================
  # TAB 4: FILMY TARANTINO
  # ==========================================================================

  output$tab4_table <- DT::renderDataTable({
    datatable(round_df(tarantino), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$tab4_hist, {
    output$tab4_explore_plot <- renderPlot({
      ggplot(tarantino, aes(x = minutes_in)) +
        geom_histogram(bins = 30, fill = col_primary, color = "white", alpha = 0.8) +
        labs(title = "Rozklad minutes_in", x = "Minuta filmu", y = "Liczba zdarzen") +
        theme_minimal(base_size = 14)
    })
  })

  observeEvent(input$tab4_bar, {
    output$tab4_explore_plot <- renderPlot({
      tarantino %>%
        count(movie, type) %>%
        ggplot(aes(x = reorder(movie, n), y = n, fill = type)) +
        geom_col(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("death" = col_bad, "word" = col_mixed)) +
        coord_flip() +
        labs(title = "Zdarzenia wg filmu", x = NULL, y = "Liczba", fill = "Typ") +
        theme_minimal(base_size = 14)
    })
  })

  tab4_quiz_answered <- reactiveVal(FALSE)
  tab4_quiz_selected <- reactiveVal(NULL)

  tab4_quiz_choices <- list(
    list(letter = "A", value = "Test t", text = "Test t"),
    list(letter = "B", value = "Korelacja", text = "Korelacja"),
    list(letter = "C", value = "Regresja", text = "Regresja"),
    list(letter = "D", value = "Zadna z klasycznych", text = "\u017badna z klasycznych")
  )

  output$tab4_quiz_options <- renderUI({
    if (tab4_quiz_answered()) return(NULL)
    div(class = "quiz-tiles quiz-cols-4",
      lapply(tab4_quiz_choices, function(opt) {
        actionButton(paste0("tab4_tile_", gsub(" ", "_", opt$value)),
          tagList(
            div(class = "tile-letter", opt$letter),
            div(class = "tile-text", opt$text)
          ),
          class = "quiz-tile"
        )
      })
    )
  })

  observe({
    for (opt in tab4_quiz_choices) {
      local({
        val <- opt$value
        btn_id <- paste0("tab4_tile_", gsub(" ", "_", val))
        observeEvent(input[[btn_id]], {
          if (tab4_quiz_answered()) return()
          tab4_quiz_selected(val)
          tab4_quiz_answered(TRUE)
        }, ignoreInit = TRUE)
      })
    }
  })

  output$tab4_quiz_result <- renderUI({
    req(tab4_quiz_answered())
    answer <- tab4_quiz_selected()
    if (answer == "Zadna z klasycznych") {
      div(class = "callout-success", style = "margin-top: 10px;",
        tags$strong("Dokladnie!"),
        " Dane eventowe nie nadaja sie do klasycznych testow.",
        " Kazdy wiersz to zdarzenie, nie niezalezna obserwacja."
      )
    } else {
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Nie do konca."),
        paste0(" ", answer, " wymaga zmiennych odpowiedniego typu i niezaleznych obserwacji. "),
        "Tutaj mamy dane eventowe - kazdy wiersz to jedno przeklenstwo lub smierc w filmie. ",
        "Poprawna odpowiedz: 'Zadna z klasycznych'."
      )
    }
  })

  output$tab4_agg_result <- renderUI({
    req(input$tab4_aggregate > 0)
    agg <- tarantino %>%
      group_by(movie) %>%
      summarise(
        n_profanity = sum(type == "word", na.rm = TRUE),
        n_deaths = sum(type == "death", na.rm = TRUE),
        .groups = "drop"
      )

    tagList(
      div(style = "margin-top: 15px;",
        DT::renderDataTable({
          datatable(round_df(agg), options = list(dom = 't', pageLength = 10), rownames = FALSE)
        })
      ),
      div(class = "callout-danger", style = "margin-top: 15px;",
        tags$strong("Problem:"),
        paste0(" Po agregacji mamy n = ", nrow(agg), " filmow. "),
        "To zdecydowanie za malo na jakakolwiek analize statystyczna.",
        tags$br(),
        "Korelacja n_profanity vs n_deaths przy n=7 nie ma mocy statystycznej."
      )
    )
  })

  output$tab4_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Tarantino: dane nie odpowiadaja hipotezie, n=7 po agregacji, brak mix, zla struktura, brak niezaleznosci
    render_verdict(c("no", "no", "no", "yes", "no", "no", "yes", "yes", "yes"), "bad")
  })

  # ==========================================================================
  # TAB 5: ANKIETA FIRMOWA
  # ==========================================================================

  output$tab5_table <- DT::renderDataTable({
    datatable(round_df(corp_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab5_plot_zadowolenie <- renderPlot({
    pct_45 <- round(100 * mean(corp_data$zadowolenie >= 4))
    ggplot(corp_data, aes(x = factor(zadowolenie))) +
      geom_bar(fill = col_bad, alpha = 0.85) +
      labs(
        title = paste0("Zadowolenie z pracy (skala 1\u20135): ", pct_45, "% odpowiedzi to 4 lub 5"),
        x = "Ocena zadowolenia", y = "Liczba pracownikow"
      ) +
      theme_minimal(base_size = 14)
  })

  output$tab5_plot_departament <- renderPlot({
    dept_counts <- corp_data %>%
      count(departament) %>%
      mutate(pct = round(100 * n / sum(n)),
             departament = reorder(departament, -n))
    ggplot(dept_counts, aes(x = departament, y = n)) +
      geom_col(fill = col_bad, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 4.5) +
      labs(title = "Rozklad pracownikow wedlug dzialu",
           x = "Dzial", y = "Liczba pracownikow") +
      theme_minimal(base_size = 14)
  })

  output$tab5_plot_staz <- renderPlot({
    med_staz <- median(corp_data$staz_pracy)
    sd_staz  <- round(sd(corp_data$staz_pracy), 2)
    ggplot(corp_data, aes(x = staz_pracy)) +
      geom_histogram(bins = 15, fill = col_mixed, color = "white", alpha = 0.85) +
      geom_vline(xintercept = med_staz, color = col_dark, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_staz, y = Inf, label = paste0("mediana = ", med_staz),
               vjust = 2, hjust = -0.1, size = 4, color = col_dark) +
      labs(
        title = paste0("Staz pracy  |  zakres: ", min(corp_data$staz_pracy),
                       "\u2013", max(corp_data$staz_pracy), " lat  |  SD = ", sd_staz),
        x = "Staz pracy (lata)", y = "Liczba pracownikow"
      ) +
      theme_minimal(base_size = 14)
  })

  output$tab5_plot_wynagrodzenie <- renderPlot({
    med_wyn <- median(corp_data$wynagrodzenie)
    sd_wyn  <- round(sd(corp_data$wynagrodzenie))
    ggplot(corp_data, aes(x = wynagrodzenie)) +
      geom_histogram(bins = 15, fill = col_mixed, color = "white", alpha = 0.85) +
      geom_vline(xintercept = med_wyn, color = col_dark, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_wyn, y = Inf, label = paste0("mediana = ", med_wyn, " PLN"),
               vjust = 2, hjust = -0.1, size = 4, color = col_dark) +
      labs(
        title = paste0("Wynagrodzenie  |  zakres: ", min(corp_data$wynagrodzenie),
                       "\u2013", max(corp_data$wynagrodzenie), " PLN  |  SD = ", sd_wyn, " PLN"),
        x = "Wynagrodzenie (PLN)", y = "Liczba pracownikow"
      ) +
      theme_minimal(base_size = 14)
  })

  output$tab5_plot_plec <- renderPlot({
    plec_counts <- corp_data %>%
      count(plec) %>%
      mutate(pct = round(100 * n / sum(n)))
    ggplot(plec_counts, aes(x = plec, y = n)) +
      geom_col(fill = col_bad, alpha = 0.85) +
      geom_text(aes(label = paste0(pct, "%  (n=", n, ")")), vjust = -0.4, size = 4.5) +
      labs(title = "Rozklad pracownikow wedlug plci",
           x = "Plec", y = "Liczba pracownikow") +
      theme_minimal(base_size = 14)
  })

  output$tab5_scatter <- renderPlot({
    ggplot(corp_data, aes(x = staz_pracy, y = wynagrodzenie)) +
      geom_point(alpha = 0.5, size = 3, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      labs(title = "Staz pracy vs wynagrodzenie",
           subtitle = paste0("r = ", round(cor(corp_data$staz_pracy, corp_data$wynagrodzenie), 3)),
           x = "Staz pracy (lata)", y = "Wynagrodzenie (PLN)") +
      theme_minimal(base_size = 14)
  })

  output$tab5_scatter_sim <- renderPlot({
    mult <- input$tab5_sd_mult
    sim_staz <- mean(corp_data$staz_pracy) + (corp_data$staz_pracy - mean(corp_data$staz_pracy)) * mult
    sim_wyn <- mean(corp_data$wynagrodzenie) + (corp_data$wynagrodzenie - mean(corp_data$wynagrodzenie)) * mult
    # Add true correlation
    set.seed(42)
    sim_wyn <- sim_wyn + (sim_staz - mean(sim_staz)) * 200 + rnorm(corp_n, 0, 100 * mult)
    r <- round(cor(sim_staz, sim_wyn), 3)

    ggplot(data.frame(x = sim_staz, y = sim_wyn), aes(x, y)) +
      geom_point(alpha = 0.5, size = 3, color = col_dark) +
      geom_smooth(method = "lm", color = col_primary, se = TRUE) +
      labs(title = paste0("Symulacja z SD \u00d7 ", mult),
           subtitle = paste0("r = ", r),
           x = "Staz pracy (lata)", y = "Wynagrodzenie (PLN)") +
      theme_minimal(base_size = 14)
  })

  output$tab5_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Firma: hipoteza ok, n ok, mix ok, brak zmiennosci, struktura nie (niezbalansowane grupy), niezaleznosc ok
    render_verdict(c("yes", "yes", "yes", "no", "no", "yes", "yes", "yes", "yes"), "bad")
  })

  # ==========================================================================
  # TAB 6: WYNAGRODZENIA (Wage)
  # ==========================================================================

  output$tab6_table <- DT::renderDataTable({
    datatable(round_df(Wage[, c("year", "age", "maritl", "race", "education", "jobclass", "health", "wage")]),
              options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab6_hist <- renderPlot({
    req(input$tab6_var)
    var <- input$tab6_var
    if (var %in% c("wage", "age")) {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_histogram(bins = 30, fill = col_primary, color = "white", alpha = 0.8) +
        labs(title = paste("Rozklad:", var), x = var, y = "Liczebnosc") +
        theme_minimal(base_size = 14)
    } else {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_bar(fill = col_primary, alpha = 0.8) +
        labs(title = paste("Rozklad:", var), x = var, y = "Liczebnosc") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  })

  output$tab6_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    render_verdict(rep("yes", 9), "good")
  })

  # ==========================================================================
  # TAB 7: TRUDNA ANKIETA
  # ==========================================================================

  output$tab7_table <- DT::renderDataTable({
    datatable(round_df(messy_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab7_mean_result <- renderUI({
    req(input$tab7_mean > 0)
    isolate({
      var <- input$tab7_var
      vals <- messy_data[[var]]

      if (var == "samopoczucie") {
        # This one is numeric already
        div(class = "callout-info", style = "margin-top: 10px;",
          paste0("Srednia samopoczucia: ", round(mean(vals), 1),
                 " (ale uwaga: wszyscy zaokraglaja do 10 - to nie jest prawdziwa skala ciagla)")
        )
      } else if (var == "ulubiony_kolor") {
        div(class = "callout-warning", style = "margin-top: 10px;",
          "Ulubiony kolor to zmienna nominalna - srednia nie ma sensu. ",
          "A poza tym: jak ta zmienna wiaze sie z Twoim pytaniem badawczym?"
        )
      } else {
        nums <- safe_numeric(vals)
        n_na <- sum(is.na(nums))
        pct_na <- round(n_na / length(nums) * 100, 1)

        if (n_na == 0) {
          div(class = "callout-info", style = "margin-top: 10px;",
            paste0("Srednia: ", round(mean(nums, na.rm = TRUE), 2))
          )
        } else {
          div(class = "callout-danger", style = "margin-top: 10px;",
            tags$strong(paste0(n_na, " z ", length(nums), " wartosci (", pct_na, "%) nie dalo sie przekonwertowac na liczby!")),
            tags$br(),
            "Przyklady problematycznych wartosci: ",
            paste(head(vals[is.na(nums)], 5), collapse = ", "),
            tags$br(), tags$br(),
            "R nie wie, co zrobic z tekstem jak '3-4h' albo 'dobrze'."
          )
        }
      }
    })
  })

  output$tab7_clean_table <- DT::renderDataTable({
    if (input$tab7_toggle == "Surowe") {
      datatable(round_df(messy_data), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    } else {
      # Cleaned version
      clean <- data.frame(
        czas_na_studia_h = c(NA, 3.5, 5, NA, NA, 2, NA, 3, 6, NA)[
          match(messy_data$czas_na_studia,
                c("duzo", "3-4h", "5", "caly dzien", "malo", "ok. 2 godziny",
                  "nie wiem", "3", "6h dziennie", "weekendy"))],
        ocena_kursu_1_10 = c(8, NA, 4, NA, 7.5, NA, 9, NA, 6, NA, 10, NA)[
          match(messy_data$ocena_kursu,
                c("8/10", "dobrze", "4", "B+", "7.5", "srednia", "9", "bardzo dobrze",
                  "6/10", "slabo", "10", "ok"))],
        aktywnosc_razy_tyg = c(NA, 0, NA, 3, NA, 7, 2, NA)[
          match(messy_data$aktywnosc,
                c("tak", "nie", "czasami", "3 razy w tygodniu", "rzadko",
                  "codziennie", "2x", "nie wiem"))],
        samopoczucie_1_10 = round(messy_data$samopoczucie / 10),
        stringsAsFactors = FALSE
      )
      datatable(round_df(clean), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    }
  })

  output$tab7_clean_info <- renderUI({
    if (input$tab7_toggle == "Oczyszczone") {
      div(class = "callout-info", style = "margin-top: 10px;",
        tags$strong("Zmiany:"),
        tags$br(), "- czas_na_studia: zamieniono na godziny (ale duzo wartosci to NA - niejednoznaczne odpowiedzi)",
        tags$br(), "- ocena_kursu: ujednolicono do skali 1-10 (tekst -> NA)",
        tags$br(), "- aktywnosc: zamieniono na razy/tydzien (duzo NA)",
        tags$br(), "- samopoczucie: przeskalowano 1-100 -> 1-10",
        tags$br(), "- ulubiony_kolor: USUNIETO (irrelewantna zmienna)",
        tags$br(), tags$br(),
        tags$em("Wniosek: czyszczenie jest mozliwe, ale tracimy duzo danych. Lepiej zaprojektowac ankiete poprawnie od poczatku.")
      )
    }
  })

  output$tab7_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Trudna ankieta: hipoteza ok, n ok, brak mix (bo nic nie jest liczbowe), zmiennosc ok, struktura nie, niezaleznosc ok | braki ok, definicje NO, bledy ok
    render_verdict(c("yes", "yes", "no", "yes", "no", "yes", "yes", "no", "yes"), "bad")
  })

  # ==========================================================================
  # TAB 8: CENY MIESZKAN
  # ==========================================================================

  output$tab8_table <- DT::renderDataTable({
    datatable(round_df(apt_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # Identify error rows
  error_rows <- c(3, 17, 42, 28, 55, 71)

  apt_clean <- reactive({
    if (input$tab8_clean) {
      apt_data[-error_rows, ]
    } else {
      apt_data
    }
  })

  output$tab8_scatter_raw <- renderPlot({
    model <- lm(cena ~ powierzchnia, data = apt_data)
    r2 <- round(summary(model)$r.squared, 3)
    ggplot(apt_data, aes(x = powierzchnia, y = cena)) +
      geom_point(alpha = 0.5, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      labs(title = paste0("Cena vs powierzchnia (R\u00b2 = ", r2, ")"),
           x = "Powierzchnia (m\u00b2)", y = "Cena (PLN)") +
      theme_minimal(base_size = 14)
  })

  output$tab8_boxplot <- renderPlot({
    req(input$tab8_var)
    ggplot(apt_data, aes(y = .data[[input$tab8_var]])) +
      geom_boxplot(fill = col_mixed, alpha = 0.7, width = 0.5) +
      labs(title = paste("Boxplot:", input$tab8_var), y = input$tab8_var) +
      theme_minimal(base_size = 14)
  })

  output$tab8_scatter_clean <- renderPlot({
    d <- apt_clean()
    model <- lm(cena ~ powierzchnia, data = d)
    r2 <- round(summary(model)$r.squared, 3)
    ggplot(d, aes(x = powierzchnia, y = cena)) +
      geom_point(alpha = 0.5, color = col_dark) +
      geom_smooth(method = "lm", color = col_good, se = TRUE) +
      labs(title = paste0("Po czyszczeniu (R\u00b2 = ", r2, ")"),
           x = "Powierzchnia (m\u00b2)", y = "Cena (PLN)") +
      theme_minimal(base_size = 14)
  })

  # Quiz
  output$tab8_quiz <- renderUI({
    tagList(
      h4("Sklasyfikuj kazda podejrzana obserwacje:"),
      div(style = "margin: 10px 0;",
        tags$strong("1. Cena = 45 PLN"), tags$br(),
        radioButtons("tab8_q1", NULL, choices = c("Blad danych", "Prawdziwy outlier"), inline = TRUE)
      ),
      div(style = "margin: 10px 0;",
        tags$strong("2. Cena = 5 500 000 PLN"), tags$br(),
        radioButtons("tab8_q2", NULL, choices = c("Blad danych", "Prawdziwy outlier"), inline = TRUE)
      ),
      div(style = "margin: 10px 0;",
        tags$strong("3. Cena = -300 000 PLN"), tags$br(),
        radioButtons("tab8_q3", NULL, choices = c("Blad danych", "Prawdziwy outlier"), inline = TRUE)
      ),
      div(style = "margin: 10px 0;",
        tags$strong("4. Powierzchnia = 1200 m\u00b2"), tags$br(),
        radioButtons("tab8_q4", NULL, choices = c("Blad danych", "Prawdziwy outlier"), inline = TRUE)
      ),
      div(style = "margin: 10px 0;",
        tags$strong("5. Cena = 780 000, powierzchnia = 120 m\u00b2"), tags$br(),
        radioButtons("tab8_q5", NULL, choices = c("Blad danych", "Prawdziwy outlier"), inline = TRUE)
      )
    )
  })

  output$tab8_quiz_result <- renderUI({
    req(input$tab8_check_quiz > 0)
    isolate({
      answers <- c(input$tab8_q1, input$tab8_q2, input$tab8_q3, input$tab8_q4, input$tab8_q5)
      correct <- c("Blad danych", "Blad danych", "Blad danych", "Blad danych", "Prawdziwy outlier")
      explanations <- c(
        "Cena 45 PLN = brak zer (powinno byc ~450 000)",
        "5 500 000 = dodatkowe zero (powinno byc ~550 000)",
        "-300 000 = blad znaku (cena nie moze byc ujemna)",
        "1200 m\u00b2 = dodatkowe zero (powinno byc ~120 m\u00b2)",
        "To drogie mieszkanie, ale realne - duze, w dobrej lokalizacji. Prawdziwy outlier!"
      )

      items <- sapply(1:5, function(i) {
        ok <- answers[i] == correct[i]
        icon <- if (ok) "\u2705" else "\u274c"
        paste0("<div style='padding: 5px 0;'>", icon, " ", explanations[i], "</div>")
      })

      score <- sum(answers == correct)
      div(class = if (score >= 4) "callout-success" else "callout-warning", style = "margin-top: 15px;",
        tags$strong(paste0("Wynik: ", score, "/5")),
        HTML(paste(items, collapse = ""))
      )
    })
  })

  output$tab8_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Mieszkania: wszystko krytyczne ok, ale bledy w danych (naprawialny problem)
    render_verdict(c("yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "no"), "mixed")
  })

  # ==========================================================================
  # TAB 9: ANKIETA STUDENCKA
  # ==========================================================================

  output$tab9_table <- DT::renderDataTable({
    datatable(round_df(survey_data), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab9_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    render_verdict(rep("yes", 9), "good")
  })

  # ==========================================================================
  # TAB 10: JAKOSC POWIETRZA
  # ==========================================================================

  aq <- airquality

  output$tab10_table <- DT::renderDataTable({
    datatable(round_df(aq), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab10_missing <- renderPlot({
    miss_pct <- sapply(aq, function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss$color <- ifelse(df_miss$pct > 20, col_bad, ifelse(df_miss$pct > 5, col_mixed, col_good))

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct, fill = color)) +
      geom_col() +
      scale_fill_identity() +
      geom_hline(yintercept = 5, linetype = "dashed", color = col_mixed) +
      geom_hline(yintercept = 20, linetype = "dashed", color = col_bad) +
      annotate("text", x = 1, y = 22, label = "20% - powazny problem", color = col_bad, size = 4) +
      annotate("text", x = 1, y = 7, label = "5% - akceptowalne", color = col_mixed, size = 4) +
      labs(title = "Procent brakow danych", x = NULL, y = "% brakow") +
      theme_minimal(base_size = 14)
  })

  output$tab10_missing_info <- renderUI({
    ozone_na <- sum(is.na(aq$Ozone))
    solar_na <- sum(is.na(aq$Solar.R))
    n_complete <- sum(complete.cases(aq))
    div(class = "callout-warning",
      paste0("Ozone: ", ozone_na, " brakow (", round(ozone_na / nrow(aq) * 100, 1), "%), ",
             "Solar.R: ", solar_na, " brakow (", round(solar_na / nrow(aq) * 100, 1), "%). ",
             "Kompletne obserwacje: ", n_complete, " z ", nrow(aq), ".")
    )
  })

  output$tab10_lineplot <- renderPlot({
    aq$row <- 1:nrow(aq)
    ggplot(aq, aes(x = row, y = Ozone)) +
      geom_line(color = col_primary, alpha = 0.7) +
      geom_point(color = col_primary, size = 1.5, alpha = 0.5) +
      labs(title = "Ozone w kolejnosci obserwacji",
           subtitle = "Widac wyrazna sezonowosc - to nie sa niezalezne pomiary!",
           x = "Numer obserwacji (= dzien)", y = "Ozone (ppb)") +
      theme_minimal(base_size = 14)
  })

  output$tab10_lag <- renderPlot({
    oz <- aq$Ozone
    oz_clean <- oz[!is.na(oz)]
    n <- length(oz_clean)
    lag_df <- data.frame(x = oz_clean[-n], y = oz_clean[-1])
    r <- round(cor(lag_df$x, lag_df$y), 3)

    ggplot(lag_df, aes(x = x, y = y)) +
      geom_point(alpha = 0.4, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      labs(title = paste0("Autokorelacja lag-1 (r = ", r, ")"),
           subtitle = "Jesli obserwacje sa niezalezne, nie powinno byc korelacji",
           x = "Ozone(t)", y = "Ozone(t+1)") +
      theme_minimal(base_size = 14)
  })

  output$tab10_autocorr_info <- renderUI({
    oz <- aq$Ozone[!is.na(aq$Ozone)]
    n <- length(oz)
    r <- cor(oz[-n], oz[-1])
    div(class = "callout-danger",
      paste0("Autokorelacja lag-1: r = ", round(r, 3), ". ",
             "Gdyby obserwacje byly niezalezne, oczekiwalismy r bliskiego 0. ",
             "Wartosc ", round(r, 2), " oznacza silna zaleznosc miedzy kolejnymi dniami.")
    )
  })

  output$tab10_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Powietrze: hipoteza ok, n ok, mix ok(ish), zmiennosc ok, struktura warn, niezaleznosc NO | braki NO, definicje ok, bledy ok
    render_verdict(c("yes", "yes", "yes", "yes", "warn", "no", "no", "yes", "yes"), "bad")
  })

  # ==========================================================================
  # TAB 11: SCIAGA
  # ==========================================================================

  output$tab11_summary <- renderTable({
    data.frame(
      Nr = 1:10,
      Zbior = c("Szkoly w Kalifornii", "Ankieta na grupie", "Pingwiny",
                "Filmy Tarantino", "Ankieta firmowa", "Wynagrodzenia USA",
                "Trudna ankieta", "Ceny mieszkan", "Ankieta studencka", "Jakosc powietrza"),
      n = c(420, 8, 344, "~1800 zdarzen", 80, 3000, 90, 150, 150, 153),
      Werdykt = c("DOBRY", "ZLY", "DOBRY", "ZLY", "ZLY", "DOBRY", "ZLY", "MIESZANY", "DOBRY", "ZLY"),
      Problem = c("Brak", "Za mala proba", "Niewielkie braki", "Zla struktura, n=7 po agregacji",
                  "Brak zmiennosci", "Brak", "Zle zdefiniowane zmienne",
                  "Outliery i bledy", "Brak", "Braki danych + szereg czasowy"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$tab11_analysis_table <- renderTable({
    data.frame(
      Analiza = c("Test t", "Korelacja Pearsona", "Regresja liniowa", "Test chi-kwadrat"),
      Min_n = c("20-30 na grupe", "30 ogolnie", "10k + 50 (k = predyktory)", "5 w kazdej komorce tabeli"),
      Zmienne = c("1 ilosciowa + 1 jakosciowa (2 grupy)", "2 ilosciowe (ciagle)",
                  "1 ilosciowa (Y) + k ilosciowych/jakosciowych (X)", "2 jakosciowe"),
      Dodatkowe = c("Normalnosc, rownosc wariancji", "Liniowosc, normalnosc",
                    "Liniowosc, normalnosc reszt, homoskedastycznosc", "Niezaleznosc obserwacji"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

} # end server

shinyApp(ui = ui, server = server)
