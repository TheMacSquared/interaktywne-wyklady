# Co czyni dobry zbiór danych?
# Interaktywny wykład oparty o case studies - ocena jakości danych do analiz statystycznych

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
  wynagrodzenie = round(rnorm(corp_n, mean = 5500, sd = 1000)),
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

# Tab 7b: Dane do uratowania przez kategoryzację (n=12)
fixable_data <- data.frame(
  id = 1:12,
  rok_studiow = c("1", "pierwszy", "I rok", "2", "drugi", "2", "3", "III", "trzeci", "1", "2", "3"),
  tryb = c("stacjonarny", "s", "S", "niestacjonarny", "N", "stacjonarny",
           "zaoczny", "niestacjonarny", "stacjonarny", "s", "niestacjonarny", "Stacjonarny"),
  godziny_nauki = c("5", "ok. 5", "4-6h", "8", "duzo", "3h", "10", "7-8h", "malo", "6", "5h", "9"),
  stringsAsFactors = FALSE
)
fixable_data_cat <- data.frame(
  id = 1:12,
  rok_studiow = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 1L, 2L, 3L),
  tryb = c("stacjonarny","stacjonarny","stacjonarny",
           "niestacjonarny","niestacjonarny","stacjonarny",
           "niestacjonarny","niestacjonarny","stacjonarny",
           "stacjonarny","niestacjonarny","stacjonarny"),
  nauka_kat = c("srednie (4-6h)","srednie (4-6h)","srednie (4-6h)",
                "duzo (7h+)", NA, "malo (1-3h)",
                "duzo (7h+)","duzo (7h+)", NA,
                "srednie (4-6h)","srednie (4-6h)","duzo (7h+)"),
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
# KATALOG PROBLEMOW - mini-zbiory demonstracyjne
# ============================================================================

# Problem 1: Za malo danych (n=6)
cat_small <- data.frame(
  id = 1:6,
  plec = c("K", "M", "K", "K", "M", "K"),
  wiek = c(21, 23, 20, 22, 24, 21),
  stres = c(7, 4, 8, 6, 3, 7),
  oceny = c(4.2, 3.5, 4.8, 3.9, 3.1, 4.0),
  stringsAsFactors = FALSE
)

# Problem 2: Brak zmiennosci
cat_novar <- data.frame(
  id = 1:12,
  zadowolenie = c(5, 5, 4, 5, 5, 5, 4, 5, 5, 5, 4, 5),
  wynagrodzenie = c(3200, 6800, 4500, 7200, 3800, 5500, 4100, 6200, 3600, 7500, 4800, 5100),
  staz = c(3.1, 3.0, 2.9, 3.2, 3.0, 3.1, 2.8, 3.0, 3.1, 3.0, 2.9, 3.2),
  dzial = c("IT","IT","IT","IT","IT","IT","IT","IT","IT","IT","IT","IT"),
  stringsAsFactors = FALSE
)

# Problem 3: Bledy i literowki
cat_errors <- data.frame(
  id = 1:12,
  cena = c(350000, 420000, 45, 510000, 280000, 390000,
           -300000, 470000, 5500000, 310000, 440000, 360000),
  powierzchnia = c(55, 72, 48, 85, 40, 64, 52, 78, 90, 1200, 68, 58),
  pokoje = c(2, 3, 2, 4, 1, 3, 2, 3, 42, 2, 3, 2),
  dzielnica = c("Mokotow","Wola","Praga","Srodmiescie","Ursynow","Bielany",
                "Mokotow","Wola","Srodmiescie","Praga","Mokotow","Ursynow"),
  stringsAsFactors = FALSE
)
cat_errors_clean <- data.frame(
  id = 1:12,
  cena = c(350000, 420000, 450000, 510000, 280000, 390000,
           300000, 470000, 550000, 310000, 440000, 360000),
  powierzchnia = c(55, 72, 48, 85, 40, 64, 52, 78, 90, 120, 68, 58),
  pokoje = c(2, 3, 2, 4, 1, 3, 2, 3, 4, 2, 3, 2),
  dzielnica = c("Mokotow","Wola","Praga","Srodmiescie","Ursynow","Bielany",
                "Mokotow","Wola","Srodmiescie","Praga","Mokotow","Ursynow"),
  stringsAsFactors = FALSE
)

# Problem 4: Zle zdefiniowane zmienne
cat_messy <- data.frame(
  id = 1:10,
  czas_nauki = c("duzo", "3-4h", "5", "caly dzien", "malo",
                 "ok. 2 godziny", "nie wiem", "3", "6h dziennie", "weekendy"),
  ocena_kursu = c("8/10", "dobrze", "4", "B+", "7.5",
                  "srednia", "9", "bardzo dobrze", "6/10", "slabo"),
  aktywnosc = c("tak", "nie", "czasami", "3 razy/tyg", "rzadko",
                "codziennie", "2x", "nie wiem", "tak", "nie"),
  stringsAsFactors = FALSE
)
cat_messy_clean <- data.frame(
  id = 1:10,
  czas_nauki_h = c(NA, 3.5, 5, NA, NA, 2, NA, 3, 6, NA),
  ocena_kursu_1_10 = c(8, NA, 4, NA, 7.5, NA, 9, NA, 6, NA),
  aktywnosc_razy_tyg = c(NA, 0, NA, 3, NA, 7, 2, NA, NA, 0),
  stringsAsFactors = FALSE
)

# Problem 5: Braki danych
cat_missing <- data.frame(
  id = 1:12,
  wiek = c(21, 23, NA, 22, 24, 21, NA, 25, 22, 20, NA, 23),
  stres = c(7, NA, 8, 6, NA, 7, 5, NA, 4, 8, 6, NA),
  oceny = c(4.2, 3.5, NA, 3.9, 3.1, NA, 4.5, 3.8, NA, 4.0, 3.6, NA),
  kierunek = c("Bio", NA, "Psych", "Ekon", "Info", "Bio", NA, "Info", "Psych", NA, "Ekon", "Bio"),
  stringsAsFactors = FALSE
)

# Problem 6: Brak niezaleznosci (dane dzienne)
cat_timeseries <- data.frame(
  dzien = 1:20,
  data = format(seq(as.Date("2024-06-01"), by = "day", length.out = 20), "%d.%m"),
  temperatura = c(22.1, 23.5, 24.8, 25.2, 26.1, 27.3, 28.0, 27.5,
                  26.8, 25.1, 23.4, 22.0, 21.5, 22.8, 24.1, 25.6,
                  27.2, 28.5, 29.1, 28.3),
  ozon_ppb = c(35, 42, 51, 58, 67, 78, 85, 80,
               72, 55, 40, 33, 28, 38, 48, 60,
               75, 88, 95, 87),
  stringsAsFactors = FALSE
)

# Problem 7: Zla struktura (event-level) — oceny uczniow
cat_events <- data.frame(
  uczen = c("Ania","Ania","Ania","Bartek","Bartek","Celina","Celina","Celina"),
  przedmiot = c("Mat","Pol","Ang","Mat","Pol","Mat","Pol","Ang"),
  ocena = c(4, 5, 3, 2, 3, 5, 4, 5),
  stringsAsFactors = FALSE
)
cat_events_agg <- data.frame(
  uczen = c("Ania", "Bartek", "Celina"),
  srednia = c(4.0, 2.5, 4.67),
  n_ocen = c(3L, 2L, 3L),
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
  "Co czyni dobry zbiór danych?",
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

    /* Jamovi-style data table */
    .jamovi-table .dataTables_wrapper { font-family: 'Segoe UI', Roboto, sans-serif; }
    .jamovi-table table.dataTable thead th {
      background: #f0f0f0; border-bottom: 2px solid #bbb;
      font-weight: 600; font-size: 13px; padding: 8px 10px;
      text-align: center; vertical-align: bottom;
    }
    .jamovi-table table.dataTable thead th .var-type {
      display: block; font-size: 10px; font-weight: 400;
      color: #888; margin-top: 2px; font-style: italic;
    }
    .jamovi-table table.dataTable tbody td {
      padding: 6px 10px; font-size: 13px; text-align: center;
      border-right: 1px solid #eee;
    }
    .jamovi-table table.dataTable tbody tr:nth-child(odd) { background: #fafafa; }
    .jamovi-table table.dataTable tbody tr:nth-child(even) { background: #fff; }
    .cell-error { background: #fdedec !important; color: #c0392b; font-weight: 600; }
    .cell-na { background: #f5f5f5 !important; color: #bbb; font-style: italic; }
    .cell-messy { background: #fef9e7 !important; color: #7d6608; }
    .cell-ok { background: #eafaf1 !important; }

    /* Problem card in catalog */
    .problem-card {
      background: #fff; border: 1px solid #dee2e6; border-radius: 10px;
      padding: 25px; margin: 25px 0;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06);
    }
    .problem-card .problem-header {
      display: flex; align-items: center; gap: 12px; margin-bottom: 15px;
    }
    .problem-card .problem-number {
      display: inline-flex; width: 36px; height: 36px; border-radius: 50%;
      background: #e74c3c; color: white; font-weight: 700; font-size: 16px;
      align-items: center; justify-content: center; flex-shrink: 0;
    }
    .problem-card .problem-name {
      font-size: 20px; font-weight: 700; color: #2c3e50; margin: 0;
    }
    .problem-card .problem-desc {
      font-size: 15px; color: #555; line-height: 1.6; margin-bottom: 15px;
    }
    .dual-view { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 15px 0; }
    .dual-view .view-panel { min-width: 0; }
    .view-label {
      font-size: 12px; font-weight: 600; color: #7f8c8d;
      text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;
    }
    @media (max-width: 992px) { .dual-view { grid-template-columns: 1fr; } }

    /* Toggle pill buttons */
    .toggle-pills { display: inline-flex; border: 2px solid #3498db; border-radius: 20px; overflow: hidden; margin: 10px 0; }
    .toggle-pills .pill-btn {
      border: none; background: white; color: #3498db; padding: 6px 18px;
      font-size: 13px; font-weight: 600; cursor: pointer; transition: all 0.2s;
    }
    .toggle-pills .pill-btn.active { background: #3498db; color: white; }
    .toggle-pills .pill-btn:hover:not(.active) { background: #eaf4fc; }

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
    // Custom message handler for toggle button styling
    Shiny.addCustomMessageHandler('shinyjs-runjs', function(message) {
      eval(message.code);
    });
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

    div(class = "section-title", "Od pomysłu do danych"),

    div(class = "narrative",
      p("Każda analiza zaczyna się od pomysłu i pytań \u2014 jeszcze zanim otworzycie
        jakikolwiek plik. Musicie wiedzieć co chcecie zbadać i jak to opisać:
        czy szukamy związku między dwiema rzeczami? Porównujemy grupy? Sprawdzamy
        czy coś się zmienia w czasie? To nie musi być formalna hipoteza statystyczna
        \u2014 wystarczy jasny pomysł w języku potocznym."),
      p(tags$strong("Zachęcam do wybierania tematów, które Was naprawdę interesują."),
        " Jeśli piszecie pracę o czymś, na czym Wam zależy, naturalnie zadajecie
        lepsze pytania, szybciej wyłapujecie absurdalne wyniki, łatwiej tworzycie
        sensowne hipotezy. Analiza zyska niuans i dojrzałość, której nie da żaden
        podręcznik \u2014 bo będziecie rozumieć kontekst.")
    ),

    div(class = "section-title", "Drugi krok: dane"),

    div(class = "narrative",
      p("Kiedy macie już pomysł, trzeba znaleźć (albo zebrać) dane. I tu zaczyna się
        pierwsza pułapka: nie każdy zbiór danych nadaje się do planowanej analizy.
        Na tym wykładzie pokażę Wam na co zwracać uwagę \u2014 co dyskwalifikuje dane
        od razu, a co można naprawić.")
    ),

    div(class = "callout-warning",
      tags$strong("Pytanie do grupy:"),
      " Wyobraźcie sobie, że otworzyliście zbiór danych w jamovi.",
      " Na co zwracacie uwagę? Co może pójść nie tak?",
      tags$br(), tags$br(),
      tags$em("(Porozmawiajmy o tym, a potem pokażę Wam katalog typowych problemów.)")
    ),

    div(class = "callout-info",
      tags$strong("Plan wykładu:"),
      tags$br(),
      "1. Katalog problemów \u2014 7 typów błędów w danych (jak wyglądają w tabeli i na wykresie)",
      tags$br(),
      "2. Case studies \u2014 10 zbiorów do samodzielnej oceny",
      tags$br(),
      "3. Ściąga \u2014 checklist i podsumowanie"
    ),

    div(class = "chapter-transition",
      p("Zobaczmy, co może pójść nie tak z danymi."),
      actionButton("ch0_next", "Dalej: 1. Katalog problemów \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 1: KATALOG PROBLEMOW
  # ==========================================================================
  tabPanel("1. Katalog",
  fluidRow(column(10, offset = 1,

    div(class = "section-title", "Katalog problemów w danych"),

    div(class = "narrative",
      p("Poniżej zobaczysz 7 typowych problemów, które mogą dyskwalifikować zbiór danych.
        Każdy problem pokazujemy tak, jak wyglądałoby to w jamovi lub Excelu (tabela)
        oraz na wykresie. Gdzie to możliwe - pokaz surowe vs oczyszczone dane.")
    ),

    # --- Problem 1: Za mało danych ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "1"),
        h3(class = "problem-name", "Za mało danych")
      ),
      div(class = "problem-desc",
        "Kolega przepytał 6 znajomych i chce robić test t. Czy to wystarczy?"
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat1_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Co widać na wykresie"),
          plotOutput("cat1_plot", height = "280px")
        )
      ),
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Problem:"), " Przy n = 6 histogram ma ogromne dziury, ",
        "przedział ufności jest bardzo szeroki, a moc testu < 10%. ",
        "Nawet duży efekt będzie nieistotny statystycznie.",
        tags$br(),
        tags$strong("Zasada:"), " Minimum 20-30 obserwacji na grupę."
      )
    ),

    # --- Problem 2: Brak zmienności ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "2"),
        h3(class = "problem-name", "Brak zmienności")
      ),
      div(class = "problem-desc",
        "Firma przeprowadziła ankietę zadowolenia. Ale wszyscy wiedzą, że szef ją czyta..."
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat2_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Co widać na wykresach"),
          plotOutput("cat2_plot_zadowolenie", height = "200px"),
          plotOutput("cat2_plot", height = "200px")
        )
      ),
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Problem: staż pracy jest prawie stały"), " (zakres 2.8\u20133.2 lata). ",
        "Wynagrodzenia się różnią, ale nie widać żadnego wzorca \u2014 punkty tworzą pionową chmurę.",
        tags$br(),
        "Gdy jedna zmienna nie ma żadnego rozrzutu, nie da się ocenić czy i jak wpływa na drugą."
      )
    ),

    # --- Problem 3: Błędy i literówki ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "3"),
        h3(class = "problem-name", "Błędy i literówki w danych")
      ),
      div(class = "problem-desc",
        "Dane z portalu nieruchomości skopiowane do Excela. Wszystko wygląda OK... na pierwszy rzut oka."
      ),
      div(class = "toggle-pills",
        actionButton("cat3_raw", "Surowe", class = "pill-btn active"),
        actionButton("cat3_clean", "Oczyszczone", class = "pill-btn")
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat3_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Cena vs powierzchnia"),
          plotOutput("cat3_plot", height = "280px")
        )
      ),
      div(class = "callout-warning", style = "margin-top: 10px;",
        tags$strong("Typowe błędy:"),
        " brak zer (45 zamiast 450 000), dodatkowe zero (5 500 000 zamiast 550 000), ",
        "ujemna cena (-300 000), literówka w pokojach (42 zamiast 4).",
        tags$br(),
        tags$strong("Zasada:"), " Zawsze sprawdź zakresy zmiennych (min, max) zanim zaczniesz analizę."
      )
    ),

    # --- Problem 4: Źle zdefiniowane zmienne ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "4"),
        h3(class = "problem-name", "Źle zdefiniowane zmienne")
      ),
      div(class = "problem-desc",
        "Student zrobił ankietę z pytaniami otwartymi. Każdy odpowiedział po swojemu."
      ),
      div(class = "toggle-pills",
        actionButton("cat4_raw", "Surowe", class = "pill-btn active"),
        actionButton("cat4_clean", "Oczyszczone", class = "pill-btn")
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat4_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Próba zrobienia histogramu"),
          plotOutput("cat4_plot", height = "280px")
        )
      ),
      div(class = "callout-warning", style = "margin-top: 10px;",
        tags$strong("Problem:"), " R/jamovi nie wie, co zrobić z '3-4h' albo 'dobrze'. ",
        "Czyszczenie jest możliwe, ale tracimy dużo danych (NA).",
        tags$br(),
        tags$strong("Zasada:"), " Zamknięte pytania + spójne skale + pilotaż ankiety."
      )
    ),

    # --- Problem 5: Braki danych ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "5"),
        h3(class = "problem-name", "Braki danych (NA)")
      ),
      div(class = "problem-desc",
        "Ankieta ze 12 odpowiedziami. Nie każdy odpowiedział na wszystkie pytania."
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat5_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Procent brakow na zmienna"),
          plotOutput("cat5_plot", height = "280px")
        )
      ),
      div(class = "callout-info", style = "margin-top: 10px;",
        tags$strong("Progi:"),
        " < 5% braków = OK (usuń wiersze). 5-20% = ostrożnie (rozważ imputację). ",
        "> 20% = zmienna może odpaść z analizy.",
        tags$br(),
        tags$strong("Uwaga:"), " Braki rzadko są losowe! Może ludzie pomijali trudne pytania?"
      )
    ),

    # --- Problem 6: Brak niezależności ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "6"),
        h3(class = "problem-name", "Brak niezależności obserwacji")
      ),
      div(class = "problem-desc",
        "Dane o temperaturze i ozonie - 20 dni pomiarów. W tabeli wygląda normalnie..."
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat6_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Dane w kolejności (liniowy)"),
          plotOutput("cat6_plot", height = "280px")
        )
      ),
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Problem:"), " W tabeli te dane wyglądają jak 20 niezależnych pomiarów. ",
        "Ale wykres liniowy zdradza sezonowość - każdy dzień zależy od poprzedniego.",
        tags$br(),
        tags$strong("Konsekwencja:"), " Test t i korelacja Pearsona zakładają niezależność. ",
        "Złam to założenie = fałszywie istotne wyniki."
      )
    ),

    # --- Problem 7: Zła struktura ---
    div(class = "problem-card",
      div(class = "problem-header",
        span(class = "problem-number", "7"),
        h3(class = "problem-name", "Zła struktura danych")
      ),
      div(class = "problem-desc",
        "Dziennik szkolny: każdy wiersz to jedna ocena ucznia, nie jeden uczeń."
      ),
      div(class = "toggle-pills",
        actionButton("cat7_events", "Oceny (surowe)", class = "pill-btn active"),
        actionButton("cat7_agg", "Zagregowane", class = "pill-btn")
      ),
      div(class = "dual-view",
        div(class = "view-panel",
          div(class = "view-label", "Widok danych"),
          div(class = "jamovi-table", DT::dataTableOutput("cat7_table"))
        ),
        div(class = "view-panel",
          div(class = "view-label", "Ile masz obserwacji?"),
          plotOutput("cat7_plot", height = "280px")
        )
      ),
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Problem:"), " 8 wierszy wygląda jak n = 8, ale to oceny, nie uczniowie. ",
        "Po agregacji do poziomu uczniów masz n = 3. Test t na n = 3?",
        tags$br(),
        tags$strong("Zasada:"), " Zawsze pytaj: co jest jednostką obserwacji? ",
        "Osoba? Firma? Dzień? Wiersz w tabeli \u2260 obserwacja."
      )
    ),

    # --- Podsumowanie: Checklist ---
    div(class = "section-title", "Podsumowanie: Checklist jakości danych"),

    div(class = "narrative",
      p("Teraz już znasz typowe problemy. Użyj poniższego checklistu,
        żeby systematycznie oceniać każdy zbiór danych.")
    ),

    div(class = "widget-block",
      h4("Checklist jakości danych"),
      tags$p(tags$strong(style = "color: #e74c3c;", "KRYTYCZNE"),
        " - jeśli nie spełniasz, szukaj innego zbioru:"),
      checkboxGroupInput("intro_critical", NULL,
        choices = c(
          "Dane odpowiadają hipotezie badawczej (mierzą to, co chcesz badać)" = "hyp",
          "Wystarczająca liczba obserwacji (n \u2265 20-30 na grupę/podgrupę)" = "n",
          "Mix typów zmiennych (ilościowe + jakościowe)" = "mix",
          "Zmienność w danych (nie wszystko takie samo)" = "var",
          "Struktura danych pasuje do planowanych analiz" = "fit",
          "Niezależność obserwacji (lub możliwość agregacji)" = "indep"
        )
      ),
      tags$p(tags$strong(style = "color: #f39c12;", "NAPRAWIALNE"),
        " - wymagają pracy, ale się da:"),
      checkboxGroupInput("intro_fixable", NULL,
        choices = c(
          "Mało braków danych (< 5%)" = "missing",
          "Jednoznaczne definicje zmiennych" = "def",
          "Brak błędów i podejrzanych wartości" = "errors"
        )
      ),
      uiOutput("intro_thermometer")
    ),

    div(class = "chapter-transition",
      p("Pora przetestować tę wiedzę na prawdziwych zbiorach danych."),
      actionButton("cat_next", "Dalej: 2. Szkoły w Kalifornii \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 2: SZKOLY W KALIFORNII (CASchool) - DOBRY
  # ==========================================================================
  tabPanel("2. Szkoły",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Szkoły w Kalifornii"),

    div(class = "narrative",
      p("Zbiór danych z 420 okręgów szkolnych w Kalifornii. Zawiera wyniki testów
        standaryzowanych, wydatki na ucznia, dochody w okręgu i dane demograficzne."),
      p("Źródło: pakiet AER w R (Academic Economic Research).")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab1_table")
    ),

    div(class = "callout-info",
      tags$strong("Zmienne:"), " district, school (identyfikatory), ",
      "students, teachers (liczebności), expenditure (wydatki/ucznia $), ",
      "income (średni dochód w okręgu $tys.), english (% uczniów uczących się angielskiego), ",
      "lunch (% uczniów z darmowym lunchem), calworks (% rodzin na zasiłku), ",
      "read, math (wyniki testów Stanford 9)."
    ),

    div(class = "section-title", "Eksploracja zmiennych"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab1_var", "Wybierz zmienną:",
          choices = c("read", "math", "expenditure", "income", "english", "lunch",
                      "students", "teachers", "calworks"))),
        column(8, plotOutput("tab1_hist", height = "300px"))
      ),
      verbatimTextOutput("tab1_summary")
    ),

    div(class = "section-title", "Zależności między zmiennymi"),

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
      p("To był wzorcowy zbiór. Następny będzie... inny."),
      actionButton("ch1_next", "Dalej: 3. Ankieta na grupie \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 3: ZA MALO DANYCH - ZLY
  # ==========================================================================
  tabPanel("3. Grupa",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ankieta na grupie"),

    div(class = "narrative",
      p("Kolega zbiera dane do projektu. Dzień przed deadline'em pyta 8 znajomych
        ze swojej grupy. Oto co uzyskał:")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab2_table")
    ),

    div(class = "section-title", "Ile obserwacji naprawdę potrzebujesz?"),

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
      tags$strong("Problem:"), " n = 8 to zdecydowanie za mało.",
      tags$br(),
      "Przy tak małej próbie moc testu wynosi ok. 10-15% - nawet duża różnica ",
      "między grupami będzie nieistotna statystycznie.",
      tags$br(), tags$br(),
      tags$strong("Zasada:"), " Liczy się n na grupę, nie n ogólne! ",
      "Jeśli porównujesz 3 grupy i masz n = 30, to tylko 10 na grupę - wciąż za mało.",
      tags$br(),
      "Minimum 20-30 obserwacji w każdej podgrupie, którą chcesz analizować. ",
      "Regresja z k predyktorami potrzebuje n > 10k + 50."
    ),

    uiOutput("tab2_verdict"),

    div(class = "chapter-transition",
      p("Zobaczmy teraz zbiór, który radzi sobie lepiej."),
      actionButton("ch2_next", "Dalej: 4. Pingwiny \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 4: PINGWINY (palmerpenguins) - DOBRY
  # ==========================================================================
  tabPanel("4. Pingwiny",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Pingwiny z Antarktydy"),

    div(class = "narrative",
      p("Dane z badania 344 pingwinów trzech gatunków (Adelie, Chinstrap, Gentoo)
        na trzech wyspach archipelagu Palmera na Antarktydzie.
        Pomiary ciała: dziób, płetwy, masa."),
      p("Źródło: pakiet palmerpenguins w R (Horst, Hill & Gorman, 2020).")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab3_table")
    ),

    div(class = "section-title", "Czy są braki danych?"),

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
      tags$strong("Dobry zbiór!"),
      " n = 344, trzy zbalansowane grupy gatunków, jasno zdefiniowane zmienne pomiarowe.",
      tags$br(),
      "Niewielkie braki danych (< 3%) - można je bezpiecznie usunąć (listwise deletion).",
      tags$br(),
      "Możliwe analizy: test t, ANOVA, korelacja, regresja, chi-kwadrat."
    ),

    uiOutput("tab3_verdict"),

    div(class = "chapter-transition",
      p("Następny zbiór wygląda ciekawie... ale czy nadaje się do analizy?"),
      actionButton("ch3_next", "Dalej: 5. Filmy Tarantino \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 5: FILMY TARANTINO - ZLY
  # ==========================================================================
  tabPanel("5. Tarantino",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Filmy Tarantino"),

    div(class = "narrative",
      p("Kolega znalazł ciekawy zbiór danych o filmach Quentina Tarantino.
        Zawiera informacje o każdym przekleństwie i każdej śmierci w jego filmach.
        'Super temat na projekt!' - mówi."),
      p("Źródło: pakiet fivethirtyeight w R.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab4_table")
    ),

    div(class = "callout-info",
      tags$strong("Zmienne:"),
      " movie (tytuł filmu), type ('word' lub 'death'), ",
      "word (konkretne słowo, jeśli type='word'), minutes_in (minuta filmu)."
    ),

    div(class = "section-title", "Eksploracja"),

    div(class = "widget-block",
      fluidRow(
        column(6, actionButton("tab4_hist", "Histogram: minutes_in", class = "btn-outline-primary", width = "100%")),
        column(6, actionButton("tab4_bar", "Porównanie filmów", class = "btn-outline-primary", width = "100%"))
      ),
      plotOutput("tab4_explore_plot", height = "350px")
    ),

    div(class = "section-title", "Próba analiz"),

    div(class = "widget-block",
      h4("Jaka analiza tu pasuje?"),
      uiOutput("tab4_quiz_options"),
      uiOutput("tab4_quiz_result")
    ),

    div(class = "widget-block",
      h4("Może agregacja pomoże?"),
      div(class = "narrative",
        p("Każdy wiersz to jedno zdarzenie (przekleństwo lub śmierć). Aby używać klasycznej
          statystyki, musielibyśmy zagregować dane do poziomu filmów.")
      ),
      actionButton("tab4_aggregate", "Zagreguj dane", class = "btn-warning"),
      uiOutput("tab4_agg_result")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("Zły zbiór do klasycznej statystyki!"),
      tags$br(),
      tags$strong("Problem 1:"), " Dane eventowe - każdy wiersz to zdarzenie, nie obserwacja w sensie statystycznym.",
      tags$br(),
      tags$strong("Problem 2:"), " Po agregacji do poziomu filmów mamy n = 7. To za mało na jakąkolwiek analizę.",
      tags$br(),
      tags$strong("Problem 3:"), " Brak zmiennych ilościowych do korelacji/regresji."
    ),

    uiOutput("tab4_verdict"),

    div(class = "chapter-transition",
      p("Czasem dane mają odpowiednią wielkość, ale inny problem..."),
      actionButton("ch4_next", "Dalej: 6. Ankieta firmowa \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 6: ANKIETA FIRMOWA - ZŁY (brak zmienności)
  # ==========================================================================
  tabPanel("6. Firma",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ankieta firmowa"),

    div(class = "narrative",
      p("Firma przeprowadza anonimową ankietę zadowolenia pracowników.
        Problem w tym, że wszyscy wiedzą, że szef ją czyta...
        Zebrano dane od 80 pracowników.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab5_table")
    ),

    div(class = "section-title", "Zmienna 1: Zadowolenie z pracy"),

    div(class = "widget-block",
      plotOutput("tab5_plot_zadowolenie", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem: brak zróżnicowania odpowiedzi."),
      " 95% pracowników zaznaczyło 4 lub 5. Skala 1\u20135 w praktyce działa tu jak skala 1\u20132 \u2014
      kiedy wszyscy odpowiadają tak samo, zmienna nic nie mówi."
    ),

    div(class = "section-title", "Zmienna 2: Dział"),

    div(class = "widget-block",
      plotOutput("tab5_plot_departament", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem: niezbalansowane grupy."),
      " 94% respondentów to dział IT. Pozostałe działy mają po 1\u20132 osoby \u2014
      jakiekolwiek porównanie między działami będzie niemożliwe."
    ),

    div(class = "section-title", "Zmienna 3: Staż pracy"),

    div(class = "toggle-pills",
      actionButton("tab5_staz_normal", "Dane", class = "pill-btn active"),
      actionButton("tab5_staz_wide", "Pełna skala (1\u201310 lat)", class = "pill-btn")
    ),
    div(class = "widget-block",
      plotOutput("tab5_plot_staz", height = "300px")
    ),
    div(class = "callout-warning",
      tags$strong("Uwaga: wąska rozpiętość wartości."),
      " Wszyscy pracownicy mają staż w przedziale 2.8\u20133.5 roku. Sama w sobie mała zmienność
      nie jest błędem \u2014 zdarzają się takie dane. Ale gdy ",
      tags$em("cały zbiór"), " wygląda podobnie, wykrycie jakichkolwiek zależności staje się
      bardzo trudne."
    ),

    div(class = "section-title", "Zmienna 4: Wynagrodzenie"),

    div(class = "widget-block",
      plotOutput("tab5_plot_wynagrodzenie", height = "300px")
    ),
    div(class = "callout-success",
      tags$strong("Wynagrodzenia mają normalny rozrzut."),
      " To dobra wiadomość \u2014 ta zmienna wydaje się użyteczna.
      Zobaczmy więc, czy możemy ją powiązać z czymś innym w tym zbiorze."
    ),

    div(class = "section-title", "Zmienna 5: Płeć"),

    div(class = "widget-block",
      plotOutput("tab5_plot_plec", height = "300px")
    ),
    div(class = "callout-danger",
      tags$strong("Problem: niezbalansowane grupy."),
      " 90% respondentów to mężczyźni (ok. 72 os.), kobiet jest ok. 8. Porównanie
      według płci nie ma sensu przy takiej dysproporcji."
    ),

    div(class = "section-title", "Co się dzieje gdy próbujemy szukać zależności?"),

    div(class = "callout-info",
      "Wynagrodzenie ma dobry rozrzut. Czy możemy powiązać je ze stażem pracy? ",
      "Sprawdźmy \u2014 pamiętaj, że staż mieści się w bardzo wąskim przedziale."
    ),

    div(class = "widget-block",
      plotOutput("tab5_scatter", height = "300px")
    ),

    div(class = "section-title", "Co by było, gdyby dane miały normalną zmienność?"),

    div(class = "callout-info",
      "Co by było, gdyby pracownicy różnili się stażem bardziej \u2014 np. od 1 do 15 lat?
      Przesuń suwak i obserwuj jak pojawia się związek między stażem a wynagrodzeniem."
    ),

    div(class = "widget-block",
      sliderInput("tab5_sd_mult", "Mnożnik rozrzutu danych:", min = 1, max = 5, value = 1, step = 0.5),
      plotOutput("tab5_scatter_sim", height = "300px")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("Ten zbiór danych nie nadaje się do analizy."),
      tags$br(),
      "Wynagrodzenia mają dobry rozrzut, ale trudno to wykorzystać: odpowiedzi o zadowoleniu
      są skupione przy maksimum, działy i płeć skrajnie niezbalansowane, a staż pracy
      jest prawie stały. Nie ma zmiennej, którą można sensownie powiązać z wynagrodzeniem."
    ),

    uiOutput("tab5_verdict"),

    div(class = "chapter-transition",
      p("Pora na duży, dobry zbiór danych."),
      actionButton("ch5_next", "Dalej: 7. Wynagrodzenia \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 7: WYNAGRODZENIA (Wage) - DOBRY
  # ==========================================================================
  tabPanel("7. Wynagrodzenia",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Wynagrodzenia w USA"),

    div(class = "narrative",
      p("Dane z Current Population Survey: 3000 mężczyzn z regionu Mid-Atlantic.
        Informacje o zarobkach, wykształceniu, zawodzie, wieku i zdrowiu."),
      p("Źródło: pakiet ISLR w R (Introduction to Statistical Learning).")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab6_table")
    ),

    div(class = "section-title", "Eksploracja"),

    div(class = "widget-block",
      fluidRow(
        column(4, selectInput("tab6_var", "Wybierz zmienną:",
          choices = c("wage", "age", "education", "jobclass", "health", "maritl", "race"))),
        column(8, plotOutput("tab6_hist", height = "300px"))
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Bardzo dobry zbiór!"),
      " n = 3000, kompletne dane, bogaty mix zmiennych ilościowych i jakościowych.",
      tags$br(),
      "Możliwe analizy: test t, ANOVA, korelacja, regresja wieloraka, chi-kwadrat.",
      tags$br(),
      tags$em("Ale uwaga: dane tylko dla mężczyzn z jednego regionu USA - ",
              "ograniczona generalizowalność.")
    ),

    uiOutput("tab6_verdict"),

    div(class = "chapter-transition",
      p("Następny zbiór to przykład złej ankiety."),
      actionButton("ch6_next", "Dalej: 8. Trudna ankieta \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 8: TRUDNA ANKIETA - ZŁY (złe zmienne)
  # ==========================================================================
  tabPanel("8. Ankieta",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Trudna ankieta"),

    div(class = "narrative",
      p("Student zaprojektował ankietę bez konsultacji z prowadzącym i bez pilotażu.
        Rozesłał ją na grupie i zebrała 90 odpowiedzi. Oto wynik:")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab7_table")
    ),

    div(class = "section-title", "Spróbuj policzyć średnią"),

    div(class = "widget-block",
      selectInput("tab7_var", "Wybierz zmienną:",
        choices = c("czas_na_studia", "ocena_kursu", "aktywnosc", "samopoczucie", "ulubiony_kolor")),
      actionButton("tab7_mean", "Policz średnią", class = "btn-primary"),
      uiOutput("tab7_mean_result")
    ),

    div(class = "section-title", "Jak to naprawić?"),

    div(class = "widget-block",
      radioButtons("tab7_toggle", "Widok danych:", choices = c("Surowe", "Oczyszczone"), inline = TRUE),
      DT::dataTableOutput("tab7_clean_table"),
      uiOutput("tab7_clean_info")
    ),

    div(class = "callout-info",
      tags$strong("Jak tego uniknąć:"),
      tags$br(),
      "1. Zamknięte pytania (gotowe opcje do wyboru)",
      tags$br(),
      "2. Spójne skale (np. zawsze 1-10 albo zawsze 1-5)",
      tags$br(),
      "3. Pilotaż ankiety (przetestuj na 5 osobach przed rozesłaniem)",
      tags$br(),
      "4. Jasna instrukcja (np. 'podaj liczbę godzin tygodniowo')"
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("Dane wymagają gruntownego czyszczenia!"),
      tags$br(),
      "Zmienne tekstowe zamiast liczbowych, niespójne skale, brak kodowania.",
      tags$br(),
      "Zmienna 'ulubiony_kolor' jest nieistotna - nie wiąże się z żadnym pytaniem badawczym.",
      tags$br(),
      "R nie wie, co zrobić z '3-4h' albo 'dobrze' jako wartością liczbową."
    ),

    uiOutput("tab7_verdict"),

    div(class = "section-title", "Drugi przykład: dane do uratowania"),

    div(class = "narrative",
      p("Inna ankieta o nawykach studenckich, podobny problem \u2014 respondenci odpowiadali
        różnie na te same pytania. Ale tym razem prawie każdą odpowiedź można przypisać
        do kategorii. Porównaj surowe dane z wersją po kategoryzacji.")
    ),

    div(class = "toggle-pills",
      actionButton("tab7b_raw", "Surowe", class = "pill-btn active"),
      actionButton("tab7b_cat", "Po kategoryzacji", class = "pill-btn")
    ),

    div(class = "widget-block",
      DT::dataTableOutput("tab7b_table")
    ),

    div(class = "callout-success",
      tags$strong("10 z 12 wierszy można uratować (83%)."),
      tags$br(),
      tags$b("rok_studiow:"), " \"pierwszy\", \"I rok\", \"1\" \u2192 wszystkie to rok 1.",
      tags$br(),
      tags$b("tryb:"), " \"s\", \"S\", \"zaoczny\" \u2192 \"stacjonarny\" lub \"niestacjonarny\".",
      tags$br(),
      tags$b("godziny_nauki:"), " \"ok. 5\", \"4-6h\", \"5h\" \u2192 kategoria \"srednie (4-6h)\".
        Straty: \"duzo\" i \"malo\" \u2014 za mało informacji żeby przypisać do kategorii."
    ),

    div(class = "chapter-transition",
      p("Następny zbiór ma inny rodzaj problemów - błędy w danych."),
      actionButton("ch7_next", "Dalej: 9. Ceny mieszkań \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 9: CENY MIESZKAN - MIESZANY (outliery)
  # ==========================================================================
  tabPanel("9. Mieszkania",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ceny mieszkań"),

    div(class = "narrative",
      p("Dane z portalu z ogłoszeniami nieruchomości - 150 ofert skopiowanych do Excela.
        Chcemy zbadać zależność ceny od powierzchni.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab8_table")
    ),

    div(class = "section-title", "Cena vs powierzchnia"),

    div(class = "widget-block",
      plotOutput("tab8_scatter_raw", height = "350px")
    ),

    div(class = "section-title", "Szukanie outlierów"),

    div(class = "widget-block",
      selectInput("tab8_var", "Zmienna do boxplotu:",
        choices = c("cena", "powierzchnia", "pokoje", "rok_budowy")),
      plotOutput("tab8_boxplot", height = "300px")
    ),

    div(class = "widget-block",
      checkboxInput("tab8_clean", "Usuń podejrzane obserwacje", value = FALSE),
      conditionalPanel("input.tab8_clean",
        plotOutput("tab8_scatter_clean", height = "350px")
      )
    ),

    div(class = "section-title", "Quiz: błąd czy prawdziwy outlier?"),

    div(class = "widget-block",
      uiOutput("tab8_quiz"),
      actionButton("tab8_check_quiz", "Sprawdź odpowiedzi", class = "btn-primary"),
      uiOutput("tab8_quiz_result")
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-warning",
      tags$strong("Dane dobre po czyszczeniu!"),
      tags$br(),
      "Podstawowa struktura zbioru jest dobra (n=150, zróżnicowane zmienne, jasne definicje).",
      tags$br(),
      "Ale błędy wprowadzania danych drastycznie zaburzają wyniki (R\u00b2 skacze po ich usunięciu).",
      tags$br(),
      tags$strong("Klucz:"), " Rozróżnij błąd danych (usuń) od prawdziwego outliera (przemyśl zachowanie)."
    ),

    uiOutput("tab8_verdict"),

    div(class = "chapter-transition",
      p("Następny zbiór to przykład dobrze zaprojektowanej ankiety."),
      actionButton("ch8_next", "Dalej: 10. Ankieta studencka \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 10: ANKIETA STUDENCKA - DOBRY
  # ==========================================================================
  tabPanel("10. Studenci",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ankieta studencka"),

    div(class = "narrative",
      p("Wyobraź sobie, że projektujesz ankietę do projektu końcowego.
        Oto przykład dobrze zaprojektowanej ankiety z 150 respondentami.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab9_table")
    ),

    div(class = "callout-info",
      tags$strong("Zmienne i ich typy:"),
      tags$br(),
      "plec (nominalna) | kierunek (nominalna) | rok_studiow (porządkowa)",
      tags$br(),
      "godziny_nauki (ciągła) | stres (porządkowa/Likert 1-10) | srednia_ocen (ciągła) | liczba_kursow (dyskretna)"
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-success",
      tags$strong("Wzorcowa ankieta!"),
      tags$br(),
      "Zamknięte pytania, spójne skale, jasne kodowanie.",
      tags$br(),
      "n = 150, mix typów zmiennych, każda analiza z kursu jest możliwa.",
      tags$br(),
      tags$em("Porównaj z Trudną ankietą (tab 7) - te same tematy, ale świat różnic w jakości!")
    ),

    uiOutput("tab9_verdict"),

    div(class = "chapter-transition",
      p("Ostatni zbiór - wygląda dobrze, ale ma ukryty problem..."),
      actionButton("ch9_next", "Dalej: 11. Jakość powietrza \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 11: JAKOSC POWIETRZA (airquality) - ZLY
  # ==========================================================================
  tabPanel("11. Powietrze",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Jakość powietrza w Nowym Jorku"),

    div(class = "narrative",
      p("Dane o jakości powietrza w Nowym Jorku. 153 pomiary z lata 1973.
        Zmienne: Ozone (ppb), Solar.R (promieniowanie), Wind (mph), Temp (F)."),
      p("Źródło: wbudowany zbiór 'airquality' w R.")
    ),

    div(class = "section-title", "Podgląd danych"),

    div(class = "widget-block",
      DT::dataTableOutput("tab10_table")
    ),

    div(class = "section-title", "Czy są braki danych?"),

    div(class = "widget-block",
      plotOutput("tab10_missing", height = "300px"),
      uiOutput("tab10_missing_info")
    ),

    div(class = "section-title", "Odkryj ukryty problem"),

    div(class = "widget-block",
      actionButton("tab10_reveal", "Pokaż dane w kolejności", class = "btn-warning btn-lg", width = "100%"),
      conditionalPanel("input.tab10_reveal > 0",
        plotOutput("tab10_lineplot", height = "350px"),
        div(class = "callout-danger",
          tags$strong("To nie są niezależne obserwacje!"),
          " To pomiary dzienne - widać wyraźną sezonowość.",
          tags$br(),
          "Temperatura i ozon zmieniają się sezonowo - każdy dzień zależy od poprzedniego."
        )
      )
    ),

    conditionalPanel("input.tab10_reveal > 0",
      div(class = "widget-block",
        h4("Autokorelacja - dowód braku niezależności"),
        plotOutput("tab10_lag", height = "300px"),
        uiOutput("tab10_autocorr_info")
      )
    ),

    div(class = "section-title", "Werdykt"),

    div(class = "callout-danger",
      tags$strong("DWA poważne problemy:"),
      tags$br(),
      tags$strong("1. Braki danych:"), " Ozone ma 24% braków (37 z 153). ",
      "Po usunięciu braków zostaje 111 obserwacji.",
      tags$br(),
      tags$strong("2. Brak niezależności:"), " To szereg czasowy! ",
      "Obserwacje dzienne są silnie autokorelowane.",
      tags$br(),
      "Klasyczne testy (t-test, korelacja Pearsona) zakładają niezależność obserwacji - ",
      "tutaj to założenie jest złamane."
    ),

    uiOutput("tab10_verdict"),

    div(class = "chapter-transition",
      p("To był ostatni zbiór danych. Zobaczmy podsumowanie."),
      actionButton("ch10_next", "Dalej: 12. Ściąga \u2192",
                   class = "btn-primary btn-lg")
    ),

    div(style = "height: 40px;")
  ))),

  # ==========================================================================
  # TAB 12: SCIAGA
  # ==========================================================================
  tabPanel("12. Ściąga",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Ściąga - jak ocenić zbiór danych"),

    div(class = "section-title", "Podsumowanie 10 zbiorów"),

    div(class = "widget-block",
      tableOutput("tab11_summary")
    ),

    div(class = "section-title", "Checklist jakości danych"),

    div(class = "callout-danger",
      HTML("
        <strong style='font-size: 15px;'>KRYTYCZNE - jeśli nie spełniasz, szukaj innego zbioru:</strong>
        <ol>
          <li><strong>Czy dane odpowiadają hipotezie badawczej?</strong> Najpierw sformułuj co chcesz badać, potem sprawdź czy dane to mierzą.</li>
          <li><strong>Czy masz n &ge; 20-30 na grupę?</strong> Liczy się n w każdej podgrupie. Porównujesz 3 grupy? Potrzebujesz 3 &times; 30 = 90.</li>
          <li><strong>Czy masz mix typów zmiennych?</strong> Ilościowe do korelacji/regresji, jakościowe do t-testów i chi-kwadrat.</li>
          <li><strong>Czy jest zmienność?</strong> SD &asymp; 0 oznacza brak możliwości analizy.</li>
          <li><strong>Czy struktura danych pasuje do analiz?</strong> Sprawdź czy masz odpowiednie zmienne do każdej planowanej analizy.</li>
          <li><strong>Czy obserwacje są niezależne?</strong> Dane czasowe lub z klastrów wymagają specjalnych metod (lub agregacji).</li>
        </ol>
      ")
    ),

    div(class = "callout-warning",
      HTML("
        <strong style='font-size: 15px;'>NAPRAWIALNE - wymagają pracy, ale się da:</strong>
        <ol start='7'>
          <li><strong>Czy braki &lt; 5%?</strong> Można usunąć obserwacje z brakami lub imputować. Powyżej 20-30% w zmiennej - ta zmienna może odpaść.</li>
          <li><strong>Czy zmienne są jednoznacznie zdefiniowane?</strong> Można rekodować, przejść na rangi - ale każda decyzja ma konsekwencje.</li>
          <li><strong>Czy nie ma błędów i outlierów?</strong> Sprawdź zakresy, literówki. Odróżniaj błędy (usuń) od prawdziwych outlierów (przemyśl).</li>
        </ol>
      ")
    ),

    div(class = "section-title", "Dopasowanie analizy do danych"),

    div(class = "widget-block",
      tableOutput("tab11_analysis_table")
    ),

    div(class = "callout-info",
      tags$strong("Wskazówka:"),
      " Użyj tego checklistu oceniając dane do swojego projektu końcowego.",
      tags$br(),
      "Jeśli nie spełniasz kryteriów krytycznych - szukaj innego zbioru.",
      tags$br(),
      "Jeśli masz problemy naprawialne - możesz pracować z tymi danymi, ale zaplanuj czas na czyszczenie."
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

  observeEvent(input$ch0_next, { updateNavbarPage(session, "main_nav", selected = "1. Katalog") })
  observeEvent(input$cat_next, { updateNavbarPage(session, "main_nav", selected = "2. Szkoły") })
  observeEvent(input$ch1_next, { updateNavbarPage(session, "main_nav", selected = "3. Grupa") })
  observeEvent(input$ch2_next, { updateNavbarPage(session, "main_nav", selected = "4. Pingwiny") })
  observeEvent(input$ch3_next, { updateNavbarPage(session, "main_nav", selected = "5. Tarantino") })
  observeEvent(input$ch4_next, { updateNavbarPage(session, "main_nav", selected = "6. Firma") })
  observeEvent(input$ch5_next, { updateNavbarPage(session, "main_nav", selected = "7. Wynagrodzenia") })
  observeEvent(input$ch6_next, { updateNavbarPage(session, "main_nav", selected = "8. Ankieta") })
  observeEvent(input$ch7_next, { updateNavbarPage(session, "main_nav", selected = "9. Mieszkania") })
  observeEvent(input$ch8_next, { updateNavbarPage(session, "main_nav", selected = "10. Studenci") })
  observeEvent(input$ch9_next, { updateNavbarPage(session, "main_nav", selected = "11. Powietrze") })
  observeEvent(input$ch10_next, { updateNavbarPage(session, "main_nav", selected = "12. Ściąga") })

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
      label <- "Dane wymagają pracy - problemy krytyczne!"
    } else if (n_critical <= 4 || n_total <= 6) {
      color <- col_mixed
      label <- "Dane OK z zastrzeżeniami"
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
          "Naprawialne kryteria nie ratują krytycznych problemów!")
    )
  })

  # ==========================================================================
  # TAB 1: KATALOG PROBLEMOW
  # ==========================================================================

  # --- Problem 1: Za malo danych ---
  output$cat1_table <- DT::renderDataTable({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("id", br(span(class = "var-type", "id"))),
        th("plec", br(span(class = "var-type", "nominalna"))),
        th("wiek", br(span(class = "var-type", "ciagla"))),
        th("stres", br(span(class = "var-type", "porzadkowa"))),
        th("oceny", br(span(class = "var-type", "ciagla")))
      ))
    ))
    datatable(cat_small, container = sketch, rownames = FALSE,
              options = list(dom = 't', ordering = FALSE, pageLength = 10))
  })

  output$cat1_plot <- renderPlot({
    ggplot(cat_small, aes(x = oceny)) +
      geom_histogram(bins = 4, fill = col_bad, color = "white", alpha = 0.8) +
      geom_vline(xintercept = mean(cat_small$oceny), linetype = "dashed", color = col_dark, linewidth = 1) +
      annotate("text", x = mean(cat_small$oceny) + 0.15, y = 2.2,
               label = paste0("M = ", round(mean(cat_small$oceny), 2)), hjust = 0, size = 4.5) +
      scale_y_continuous(breaks = 0:3) +
      labs(title = "Histogram ocen (n = 6)", x = "Średnia ocen", y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  # --- Problem 2: Brak zmiennosci ---
  output$cat2_table <- DT::renderDataTable({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("id", br(span(class = "var-type", "id"))),
        th("zadowolenie", br(span(class = "var-type", "porzadkowa"))),
        th("wynagrodzenie", br(span(class = "var-type", "ciagla"))),
        th("staz", br(span(class = "var-type", "ciagla"))),
        th("dzial", br(span(class = "var-type", "nominalna")))
      ))
    ))
    datatable(cat_novar, container = sketch, rownames = FALSE,
              options = list(dom = 't', ordering = FALSE, pageLength = 12))
  })

  output$cat2_plot_zadowolenie <- renderPlot({
    pct_45 <- round(100 * mean(cat_novar$zadowolenie >= 4))
    ggplot(cat_novar, aes(x = factor(zadowolenie))) +
      geom_bar(fill = col_bad, alpha = 0.85) +
      scale_x_discrete(limits = c("1","2","3","4","5")) +
      labs(title = paste0("Zadowolenie: ", pct_45, "% odpowiedzi to 4 lub 5"),
           x = "Ocena (1\u20135)", y = "Liczba") +
      theme_minimal(base_size = 13)
  })

  output$cat2_plot <- renderPlot({
    ggplot(cat_novar, aes(x = staz, y = wynagrodzenie)) +
      geom_point(size = 3, alpha = 0.6, color = col_bad) +
      scale_x_continuous(limits = c(1, 10)) +
      labs(title = paste0("Sta\u017c vs wynagrodzenie (r = ",
                          round(cor(cat_novar$staz, cat_novar$wynagrodzenie), 3), ")"),
           subtitle = "Sta\u017c skupiony w w\u0105skim fragmencie osi",
           x = "Sta\u017c pracy (lata)", y = "Wynagrodzenie (PLN)") +
      theme_minimal(base_size = 13)
  })

  # --- Problem 3: Bledy i literowki (toggle) ---
  cat3_view <- reactiveVal("raw")
  observeEvent(input$cat3_raw, {
    cat3_view("raw")
    shinyjs_js <- paste0(
      "$('#cat3_raw').addClass('active'); $('#cat3_clean').removeClass('active');"
    )
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code = shinyjs_js))
  })
  observeEvent(input$cat3_clean, {
    cat3_view("clean")
    shinyjs_js <- paste0(
      "$('#cat3_clean').addClass('active'); $('#cat3_raw').removeClass('active');"
    )
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code = shinyjs_js))
  })

  output$cat3_table <- DT::renderDataTable({
    if (cat3_view() == "raw") {
      d <- cat_errors
    } else {
      d <- cat_errors_clean
    }
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("id", br(span(class = "var-type", "id"))),
        th("cena", br(span(class = "var-type", "ciagla"))),
        th("powierzchnia", br(span(class = "var-type", "ciagla"))),
        th("pokoje", br(span(class = "var-type", "dyskretna"))),
        th("dzielnica", br(span(class = "var-type", "nominalna")))
      ))
    ))
    dt <- datatable(d, container = sketch, rownames = FALSE,
                    options = list(dom = 't', ordering = FALSE, pageLength = 12))
    if (cat3_view() == "raw") {
      dt <- dt %>%
        formatStyle("cena", backgroundColor = styleInterval(
          c(0, 1000000), c("#fdedec", "white", "#fdedec"))) %>%
        formatStyle("powierzchnia", backgroundColor = styleInterval(
          c(500), c("white", "#fdedec"))) %>%
        formatStyle("pokoje", backgroundColor = styleInterval(
          c(10), c("white", "#fdedec")))
    }
    dt
  })

  output$cat3_plot <- renderPlot({
    if (cat3_view() == "raw") {
      d <- cat_errors
      title_txt <- "Z błędami"
      col <- col_bad
    } else {
      d <- cat_errors_clean
      title_txt <- "Po oczyszczeniu"
      col <- col_good
    }
    model <- lm(cena ~ powierzchnia, data = d)
    r2 <- round(summary(model)$r.squared, 3)
    ggplot(d, aes(x = powierzchnia, y = cena)) +
      geom_point(size = 3, alpha = 0.7, color = col_dark) +
      geom_smooth(method = "lm", color = col, se = TRUE) +
      labs(title = paste0(title_txt, " (R\u00b2 = ", r2, ")"),
           x = "Powierzchnia (m\u00b2)", y = "Cena (PLN)") +
      theme_minimal(base_size = 14)
  })

  # --- Problem 4: Zle zdefiniowane zmienne (toggle) ---
  cat4_view <- reactiveVal("raw")
  observeEvent(input$cat4_raw, {
    cat4_view("raw")
    session$sendCustomMessage(type = "shinyjs-runjs",
      message = list(code = "$('#cat4_raw').addClass('active'); $('#cat4_clean').removeClass('active');"))
  })
  observeEvent(input$cat4_clean, {
    cat4_view("clean")
    session$sendCustomMessage(type = "shinyjs-runjs",
      message = list(code = "$('#cat4_clean').addClass('active'); $('#cat4_raw').removeClass('active');"))
  })

  output$cat4_table <- DT::renderDataTable({
    if (cat4_view() == "raw") {
      d <- cat_messy
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(tr(
          th("id", br(span(class = "var-type", "id"))),
          th("czas_nauki", br(span(class = "var-type", "tekst?!"))),
          th("ocena_kursu", br(span(class = "var-type", "tekst?!"))),
          th("aktywnosc", br(span(class = "var-type", "tekst?!")))
        ))
      ))
      datatable(d, container = sketch, rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = 10)) %>%
        formatStyle(c("czas_nauki", "ocena_kursu", "aktywnosc"),
                    backgroundColor = "#fef9e7")
    } else {
      d <- cat_messy_clean
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(tr(
          th("id", br(span(class = "var-type", "id"))),
          th("czas_nauki_h", br(span(class = "var-type", "ciagla"))),
          th("ocena_kursu_1_10", br(span(class = "var-type", "ciagla"))),
          th("aktywnosc_razy_tyg", br(span(class = "var-type", "ciagla")))
        ))
      ))
      datatable(d, container = sketch, rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = 10))
    }
  })

  output$cat4_plot <- renderPlot({
    if (cat4_view() == "raw") {
      nums <- suppressWarnings(as.numeric(cat_messy$czas_nauki))
      n_ok <- sum(!is.na(nums))
      n_fail <- sum(is.na(nums))
      df <- data.frame(
        status = c("Rozpoznane\njako liczba", "Nie da się\nprzeczytać"),
        n = c(n_ok, n_fail)
      )
      ggplot(df, aes(x = status, y = n, fill = status)) +
        geom_col(width = 0.6) +
        scale_fill_manual(values = c(col_good, col_bad)) +
        geom_text(aes(label = n), vjust = -0.5, size = 6, fontface = "bold") +
        labs(title = "czas_nauki: próba konwersji na liczby",
             subtitle = paste0(n_fail, " z ", nrow(cat_messy), " odpowiedzi nie da się użyć"),
             x = NULL, y = "Liczba odpowiedzi") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none") +
        ylim(0, max(df$n) + 1)
    } else {
      d <- cat_messy_clean[!is.na(cat_messy_clean$czas_nauki_h), ]
      ggplot(d, aes(x = czas_nauki_h)) +
        geom_histogram(bins = 5, fill = col_good, color = "white", alpha = 0.8) +
        labs(title = paste0("Histogram (n = ", nrow(d), " z ", nrow(cat_messy_clean), ")"),
             subtitle = "Po oczyszczeniu - ale straciliśmy połowę danych",
             x = "Godziny nauki/tydzień", y = "Liczebność") +
        theme_minimal(base_size = 14)
    }
  })

  # --- Problem 5: Braki danych ---
  output$cat5_table <- DT::renderDataTable({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("id", br(span(class = "var-type", "id"))),
        th("wiek", br(span(class = "var-type", "ciagla"))),
        th("stres", br(span(class = "var-type", "porzadkowa"))),
        th("oceny", br(span(class = "var-type", "ciagla"))),
        th("kierunek", br(span(class = "var-type", "nominalna")))
      ))
    ))
    # Replace NA with styled text for visibility
    d <- cat_missing
    datatable(d, container = sketch, rownames = FALSE,
              options = list(dom = 't', ordering = FALSE, pageLength = 12)) %>%
      formatStyle(names(d)[-1],
        backgroundColor = styleEqual(NA, "#f5f5f5"),
        color = styleEqual(NA, "#bbb"))
  })

  output$cat5_plot <- renderPlot({
    miss_pct <- sapply(cat_missing[, -1], function(x) mean(is.na(x)) * 100)
    df_miss <- data.frame(variable = names(miss_pct), pct = miss_pct)
    df_miss$color <- ifelse(df_miss$pct > 20, col_bad, ifelse(df_miss$pct > 5, col_mixed, col_good))

    ggplot(df_miss, aes(x = reorder(variable, -pct), y = pct, fill = color)) +
      geom_col(width = 0.6) +
      scale_fill_identity() +
      geom_text(aes(label = paste0(round(pct), "%")), vjust = -0.5, size = 5, fontface = "bold") +
      geom_hline(yintercept = 5, linetype = "dashed", color = col_mixed) +
      geom_hline(yintercept = 20, linetype = "dashed", color = col_bad) +
      annotate("text", x = 3.5, y = 7, label = "5% = OK", color = col_mixed, size = 3.5) +
      annotate("text", x = 3.5, y = 22, label = "20% = problem", color = col_bad, size = 3.5) +
      labs(title = "Procent braków na zmienną", x = NULL, y = "% braków (NA)") +
      theme_minimal(base_size = 14) +
      ylim(0, 35)
  })

  # --- Problem 6: Brak niezaleznosci ---
  output$cat6_table <- DT::renderDataTable({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(tr(
        th("dzien", br(span(class = "var-type", "id"))),
        th("data", br(span(class = "var-type", "data"))),
        th("temperatura", br(span(class = "var-type", "ciagla"))),
        th("ozon_ppb", br(span(class = "var-type", "ciagla")))
      ))
    ))
    datatable(cat_timeseries, container = sketch, rownames = FALSE,
              options = list(dom = 't', ordering = FALSE, pageLength = 20, scrollY = "260px"))
  })

  output$cat6_plot <- renderPlot({
    ggplot(cat_timeseries, aes(x = dzien, y = temperatura)) +
      geom_line(color = col_bad, linewidth = 1.2) +
      geom_point(color = col_bad, size = 2.5) +
      labs(title = "Temperatura w kolejności pomiarów",
           subtitle = "Wyraźna fala - każdy dzień zależy od poprzedniego!",
           x = "Dzień pomiaru", y = "Temperatura (\u00b0C)") +
      theme_minimal(base_size = 14)
  })

  # --- Problem 7: Zla struktura (toggle) ---
  cat7_view <- reactiveVal("events")
  observeEvent(input$cat7_events, {
    cat7_view("events")
    session$sendCustomMessage(type = "shinyjs-runjs",
      message = list(code = "$('#cat7_events').addClass('active'); $('#cat7_agg').removeClass('active');"))
  })
  observeEvent(input$cat7_agg, {
    cat7_view("agg")
    session$sendCustomMessage(type = "shinyjs-runjs",
      message = list(code = "$('#cat7_agg').addClass('active'); $('#cat7_events').removeClass('active');"))
  })

  output$cat7_table <- DT::renderDataTable({
    if (cat7_view() == "events") {
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(tr(
          th("uczen", br(span(class = "var-type", "nominalna"))),
          th("przedmiot", br(span(class = "var-type", "nominalna"))),
          th("ocena", br(span(class = "var-type", "dyskretna")))
        ))
      ))
      datatable(cat_events, container = sketch, rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = 10))
    } else {
      sketch <- htmltools::withTags(table(
        class = "display",
        thead(tr(
          th("uczen", br(span(class = "var-type", "nominalna"))),
          th("srednia", br(span(class = "var-type", "ciagla"))),
          th("n_ocen", br(span(class = "var-type", "dyskretna")))
        ))
      ))
      datatable(cat_events_agg, container = sketch, rownames = FALSE,
                options = list(dom = 't', ordering = FALSE, pageLength = 10))
    }
  })

  output$cat7_plot <- renderPlot({
    if (cat7_view() == "events") {
      df <- data.frame(label = "Wiersze\nw tabeli", n = nrow(cat_events))
      ggplot(df, aes(x = label, y = n)) +
        geom_col(fill = col_mixed, width = 0.4) +
        geom_text(aes(label = paste0("n = ", n)), vjust = -0.5, size = 7, fontface = "bold") +
        labs(title = "Ile masz 'obserwacji'?",
             subtitle = "8 wierszy, ale to oceny, nie uczniowie",
             x = NULL, y = NULL) +
        ylim(0, 10) +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    } else {
      df <- data.frame(label = "Uczniowie\n(obserwacje)", n = nrow(cat_events_agg))
      ggplot(df, aes(x = label, y = n)) +
        geom_col(fill = col_bad, width = 0.4) +
        geom_text(aes(label = paste0("n = ", n)), vjust = -0.5, size = 7, fontface = "bold",
                  color = col_bad) +
        labs(title = "Po agregacji",
             subtitle = "n = 3 uczni\u00f3w. Test t? Zdecydowanie za ma\u0142o.",
             x = NULL, y = NULL) +
        ylim(0, 5) +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
  })

  # ==========================================================================
  # TAB 2: SZKOLY W KALIFORNII
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
      labs(title = paste("Rozkład:", input$tab1_var), x = input$tab1_var, y = "Liczebność") +
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
  # TAB 3: ZA MALO DANYCH
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
      geom_histogram(bins = max(5L, round(input$tab2_n / 5)), fill = col_primary, color = "white", alpha = 0.8) +
      labs(title = paste0("Histogram (n = ", input$tab2_n, ")"), x = "Średnia ocen", y = "Liczebność") +
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
      annotate("text", x = 150, y = 0.55, label = "Akceptowalna szerokość", color = col_good, size = 4) +
      labs(title = "Szerokość 95% CI", x = "Liczba obserwacji (n)", y = "Szerokość CI") +
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
    render_verdict(c("yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes"), "bad")
  })

  # ==========================================================================
  # TAB 4: PINGWINY
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
      annotate("text", x = 2, y = 6, label = "Próg 5%", color = col_bad, size = 4) +
      labs(title = "Procent braków danych", x = NULL, y = "% braków") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  output$tab3_missing_info <- renderUI({
    n_complete <- sum(complete.cases(penguins))
    n_total <- nrow(penguins)
    div(class = "callout-info",
      paste0("Kompletne obserwacje: ", n_complete, " z ", n_total,
             " (", round(n_complete / n_total * 100, 1), "%). ",
             "Braki dotyczą głównie zmiennej sex (", sum(is.na(penguins$sex)), " NA).")
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
  # TAB 5: FILMY TARANTINO
  # ==========================================================================

  output$tab4_table <- DT::renderDataTable({
    datatable(round_df(tarantino), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$tab4_hist, {
    output$tab4_explore_plot <- renderPlot({
      ggplot(tarantino, aes(x = minutes_in)) +
        geom_histogram(bins = 30, fill = col_primary, color = "white", alpha = 0.8) +
        labs(title = "Rozkład minutes_in", x = "Minuta filmu", y = "Liczba zdarzeń") +
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
        tags$strong("Dokładnie!"),
        " Dane eventowe nie nadają się do klasycznych testów.",
        " Każdy wiersz to zdarzenie, nie niezależna obserwacja."
      )
    } else {
      div(class = "callout-danger", style = "margin-top: 10px;",
        tags$strong("Nie do końca."),
        paste0(" ", answer, " wymaga zmiennych odpowiedniego typu i niezależnych obserwacji. "),
        "Tutaj mamy dane eventowe - każdy wiersz to jedno przekleństwo lub śmierć w filmie. ",
        "Poprawna odpowiedź: 'Żadna z klasycznych'."
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
        paste0(" Po agregacji mamy n = ", nrow(agg), " filmów. "),
        "To zdecydowanie za mało na jakąkolwiek analizę statystyczną.",
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
  # TAB 6: ANKIETA FIRMOWA
  # ==========================================================================

  output$tab5_table <- DT::renderDataTable({
    datatable(round_df(corp_data), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$tab5_plot_zadowolenie <- renderPlot({
    pct_45 <- round(100 * mean(corp_data$zadowolenie >= 4))
    ggplot(corp_data, aes(x = factor(zadowolenie))) +
      geom_bar(fill = col_bad, alpha = 0.85) +
      scale_x_discrete(limits = c("1","2","3","4","5")) +
      labs(
        title = paste0("Zadowolenie z pracy (skala 1\u20135): ", pct_45, "% odpowiedzi to 4 lub 5"),
        x = "Ocena zadowolenia", y = "Liczba pracowników"
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
      labs(title = "Rozkład pracowników według działu",
           x = "Dział", y = "Liczba pracowników") +
      theme_minimal(base_size = 14)
  })

  tab5_staz_view <- reactiveVal("normal")
  observeEvent(input$tab5_staz_normal, {
    tab5_staz_view("normal")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab5_staz_normal').addClass('active'); $('#tab5_staz_wide').removeClass('active');"))
  })
  observeEvent(input$tab5_staz_wide, {
    tab5_staz_view("wide")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab5_staz_wide').addClass('active'); $('#tab5_staz_normal').removeClass('active');"))
  })

  output$tab5_plot_staz <- renderPlot({
    med_staz <- median(corp_data$staz_pracy)
    sd_staz  <- round(sd(corp_data$staz_pracy), 2)
    p <- ggplot(corp_data, aes(x = staz_pracy)) +
      geom_histogram(bins = 15, fill = col_mixed, color = "white", alpha = 0.85) +
      geom_vline(xintercept = med_staz, color = col_dark, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_staz, y = Inf, label = paste0("mediana = ", med_staz),
               vjust = 2, hjust = -0.1, size = 4, color = col_dark) +
      labs(
        title = paste0("Staż pracy  |  zakres: ", min(corp_data$staz_pracy),
                       "\u2013", max(corp_data$staz_pracy), " lat  |  SD = ", sd_staz),
        x = "Staż pracy (lata)", y = "Liczba pracowników"
      ) +
      theme_minimal(base_size = 14)
    if (tab5_staz_view() == "wide") p <- p + scale_x_continuous(limits = c(1, 10))
    p
  })

  output$tab5_plot_wynagrodzenie <- renderPlot({
    med_wyn <- median(corp_data$wynagrodzenie)
    sd_wyn  <- round(sd(corp_data$wynagrodzenie))
    ggplot(corp_data, aes(x = wynagrodzenie)) +
      geom_histogram(bins = 15, fill = col_primary, color = "white", alpha = 0.85) +
      geom_vline(xintercept = med_wyn, color = col_dark, linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_wyn, y = Inf, label = paste0("mediana = ", med_wyn, " PLN"),
               vjust = 2, hjust = -0.1, size = 4, color = col_dark) +
      labs(
        title = paste0("Wynagrodzenie  |  zakres: ", min(corp_data$wynagrodzenie),
                       "\u2013", max(corp_data$wynagrodzenie), " PLN  |  SD = ", sd_wyn, " PLN"),
        x = "Wynagrodzenie (PLN)", y = "Liczba pracowników"
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
      labs(title = "Rozkład pracowników według płci",
           x = "Płeć", y = "Liczba pracowników") +
      theme_minimal(base_size = 14)
  })

  output$tab5_scatter <- renderPlot({
    ggplot(corp_data, aes(x = staz_pracy, y = wynagrodzenie)) +
      geom_point(alpha = 0.5, size = 3, color = col_dark) +
      geom_smooth(method = "lm", color = col_bad, se = TRUE) +
      scale_x_continuous(limits = c(1, 10)) +
      labs(title = "Staż pracy vs wynagrodzenie",
           subtitle = paste0("r = ", round(cor(corp_data$staz_pracy, corp_data$wynagrodzenie), 3),
                             "  \u2014  staż w wąskim przedziale, wynagrodzenia zróżnicowane"),
           x = "Staż pracy (lata)", y = "Wynagrodzenie (PLN)") +
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
           x = "Staż pracy (lata)", y = "Wynagrodzenie (PLN)") +
      theme_minimal(base_size = 14)
  })

  output$tab5_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Firma: jedyny problem to brak zmiennosci (zadowolenie skupione, staz wąski, grupy niezbalansowane)
    render_verdict(c("yes", "yes", "yes", "no", "yes", "yes", "yes", "yes", "yes"), "bad")
  })

  # ==========================================================================
  # TAB 7: WYNAGRODZENIA (Wage)
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
        labs(title = paste("Rozkład:", var), x = var, y = "Liczebność") +
        theme_minimal(base_size = 14)
    } else {
      ggplot(Wage, aes(x = .data[[var]])) +
        geom_bar(fill = col_primary, alpha = 0.8) +
        labs(title = paste("Rozkład:", var), x = var, y = "Liczebność") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  })

  output$tab6_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    render_verdict(rep("yes", 9), "good")
  })

  # ==========================================================================
  # TAB 8: TRUDNA ANKIETA
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
          paste0("Średnia samopoczucia: ", round(mean(vals), 1),
                 " (ale uwaga: wszyscy zaokrąglają do 10 - to nie jest prawdziwa skala ciągła)")
        )
      } else if (var == "ulubiony_kolor") {
        div(class = "callout-warning", style = "margin-top: 10px;",
          "Ulubiony kolor to zmienna nominalna - średnia nie ma sensu. ",
          "A poza tym: jak ta zmienna wiąże się z Twoim pytaniem badawczym?"
        )
      } else {
        nums <- safe_numeric(vals)
        n_na <- sum(is.na(nums))
        pct_na <- round(n_na / length(nums) * 100, 1)

        if (n_na == 0) {
          div(class = "callout-info", style = "margin-top: 10px;",
            paste0("Średnia: ", round(mean(nums, na.rm = TRUE), 2))
          )
        } else {
          div(class = "callout-danger", style = "margin-top: 10px;",
            tags$strong(paste0(n_na, " z ", length(nums), " wartości (", pct_na, "%) nie dało się przekonwertować na liczby!")),
            tags$br(),
            "Przykłady problematycznych wartości: ",
            paste(head(vals[is.na(nums)], 5), collapse = ", "),
            tags$br(), tags$br(),
            "R nie wie, co zrobić z tekstem jak '3-4h' albo 'dobrze'."
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
        tags$br(), "- czas_na_studia: zamieniono na godziny (ale dużo wartości to NA - niejednoznaczne odpowiedzi)",
        tags$br(), "- ocena_kursu: ujednolicono do skali 1-10 (tekst -> NA)",
        tags$br(), "- aktywnosc: zamieniono na razy/tydzień (dużo NA)",
        tags$br(), "- samopoczucie: przeskalowano 1-100 -> 1-10",
        tags$br(), "- ulubiony_kolor: USUNIĘTO (nieistotna zmienna)",
        tags$br(), tags$br(),
        tags$em("Wniosek: czyszczenie jest możliwe, ale tracimy dużo danych. Lepiej zaprojektować ankietę poprawnie od początku.")
      )
    }
  })

  output$tab7_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Trudna ankieta: hipoteza ok, n ok, brak mix (bo nic nie jest liczbowe), zmiennosc ok, struktura nie, niezaleznosc ok | braki ok, definicje NO, bledy ok
    render_verdict(c("yes", "yes", "no", "yes", "no", "yes", "yes", "no", "yes"), "bad")
  })

  tab7b_view <- reactiveVal("raw")
  observeEvent(input$tab7b_raw, {
    tab7b_view("raw")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab7b_raw').addClass('active'); $('#tab7b_cat').removeClass('active');"))
  })
  observeEvent(input$tab7b_cat, {
    tab7b_view("cat")
    session$sendCustomMessage(type = "shinyjs-runjs", message = list(code =
      "$('#tab7b_cat').addClass('active'); $('#tab7b_raw').removeClass('active');"))
  })

  output$tab7b_table <- DT::renderDataTable({
    if (tab7b_view() == "raw") {
      datatable(fixable_data,
                options = list(dom = 't', ordering = FALSE, pageLength = 12),
                rownames = FALSE)
    } else {
      datatable(fixable_data_cat,
                options = list(dom = 't', ordering = FALSE, pageLength = 12),
                rownames = FALSE) %>%
        DT::formatStyle("nauka_kat",
          backgroundColor = DT::styleEqual(NA, "#fdedec"),
          target = "cell")
    }
  })

  # ==========================================================================
  # TAB 9: CENY MIESZKAN
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
      h4("Sklasyfikuj każdą podejrzaną obserwację:"),
      div(style = "margin: 10px 0;",
        tags$strong("1. Cena = 45 PLN"), tags$br(),
        radioButtons("tab8_q1", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      ),
      div(style = "margin: 10px 0;",
        tags$strong("2. Cena = 5 500 000 PLN"), tags$br(),
        radioButtons("tab8_q2", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      ),
      div(style = "margin: 10px 0;",
        tags$strong("3. Cena = -300 000 PLN"), tags$br(),
        radioButtons("tab8_q3", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      ),
      div(style = "margin: 10px 0;",
        tags$strong("4. Powierzchnia = 1200 m\u00b2"), tags$br(),
        radioButtons("tab8_q4", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      ),
      div(style = "margin: 10px 0;",
        tags$strong("5. Cena = 780 000, powierzchnia = 120 m\u00b2"), tags$br(),
        radioButtons("tab8_q5", NULL, choices = c("Błąd danych", "Prawdziwy outlier"), inline = TRUE)
      )
    )
  })

  output$tab8_quiz_result <- renderUI({
    req(input$tab8_check_quiz > 0)
    isolate({
      answers <- c(input$tab8_q1, input$tab8_q2, input$tab8_q3, input$tab8_q4, input$tab8_q5)
      correct <- c("Błąd danych", "Błąd danych", "Błąd danych", "Błąd danych", "Prawdziwy outlier")
      explanations <- c(
        "Cena 45 PLN = brak zer (powinno być ~450 000)",
        "5 500 000 = dodatkowe zero (powinno być ~550 000)",
        "-300 000 = błąd znaku (cena nie może być ujemna)",
        "1200 m\u00b2 = dodatkowe zero (powinno być ~120 m\u00b2)",
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
  # TAB 10: ANKIETA STUDENCKA
  # ==========================================================================

  output$tab9_table <- DT::renderDataTable({
    datatable(round_df(survey_data), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$tab9_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    render_verdict(rep("yes", 9), "good")
  })

  # ==========================================================================
  # TAB 11: JAKOSC POWIETRZA
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
      annotate("text", x = 1, y = 22, label = "20% - poważny problem", color = col_bad, size = 4) +
      annotate("text", x = 1, y = 7, label = "5% - akceptowalne", color = col_mixed, size = 4) +
      labs(title = "Procent braków danych", x = NULL, y = "% braków") +
      theme_minimal(base_size = 14)
  })

  output$tab10_missing_info <- renderUI({
    ozone_na <- sum(is.na(aq$Ozone))
    solar_na <- sum(is.na(aq$Solar.R))
    n_complete <- sum(complete.cases(aq))
    div(class = "callout-warning",
      paste0("Ozone: ", ozone_na, " braków (", round(ozone_na / nrow(aq) * 100, 1), "%), ",
             "Solar.R: ", solar_na, " braków (", round(solar_na / nrow(aq) * 100, 1), "%). ",
             "Kompletne obserwacje: ", n_complete, " z ", nrow(aq), ".")
    )
  })

  output$tab10_lineplot <- renderPlot({
    aq$row <- 1:nrow(aq)
    ggplot(aq, aes(x = row, y = Ozone)) +
      geom_line(color = col_primary, alpha = 0.7) +
      geom_point(color = col_primary, size = 1.5, alpha = 0.5) +
      labs(title = "Ozone w kolejności obserwacji",
           subtitle = "Widać wyraźną sezonowość - to nie są niezależne pomiary!",
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
           subtitle = "Jeśli obserwacje są niezależne, nie powinno być korelacji",
           x = "Ozone(t)", y = "Ozone(t+1)") +
      theme_minimal(base_size = 14)
  })

  output$tab10_autocorr_info <- renderUI({
    oz <- aq$Ozone[!is.na(aq$Ozone)]
    n <- length(oz)
    r <- cor(oz[-n], oz[-1])
    div(class = "callout-danger",
      paste0("Autokorelacja lag-1: r = ", round(r, 3), ". ",
             "Gdyby obserwacje były niezależne, oczekiwaliśmy r bliskiego 0. ",
             "Wartość ", round(r, 2), " oznacza silną zależność między kolejnymi dniami.")
    )
  })

  output$tab10_verdict <- renderUI({
    # hipoteza, n, mix, zmiennosc, struktura, niezaleznosc | braki, definicje, bledy
    # Powietrze: hipoteza ok, n ok, mix ok(ish), zmiennosc ok, struktura warn, niezaleznosc NO | braki NO, definicje ok, bledy ok
    render_verdict(c("yes", "yes", "yes", "yes", "warn", "no", "no", "yes", "yes"), "bad")
  })

  # ==========================================================================
  # TAB 12: SCIAGA
  # ==========================================================================

  output$tab11_summary <- renderTable({
    data.frame(
      Nr = 2:11,
      Zbior = c("Szkoły w Kalifornii", "Ankieta na grupie", "Pingwiny",
                "Filmy Tarantino", "Ankieta firmowa", "Wynagrodzenia USA",
                "Trudna ankieta", "Ceny mieszkań", "Ankieta studencka", "Jakość powietrza"),
      n = c(420, 8, 344, "~1800 zdarzeń", 80, 3000, 90, 150, 150, 153),
      Werdykt = c("DOBRY", "ZŁY", "DOBRY", "ZŁY", "ZŁY", "DOBRY", "ZŁY", "MIESZANY", "DOBRY", "ZŁY"),
      Problem = c("Brak", "Za mała próba", "Niewielkie braki", "Zła struktura, n=7 po agregacji",
                  "Brak zmienności", "Brak", "Źle zdefiniowane zmienne",
                  "Outliery i błędy", "Brak", "Braki danych + szereg czasowy"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$tab11_analysis_table <- renderTable({
    data.frame(
      Analiza = c("Test t", "Korelacja Pearsona", "Regresja liniowa", "Test chi-kwadrat"),
      Min_n = c("20-30 na grupę", "30 ogólnie", "10k + 50 (k = predyktory)", "5 w każdej komórce tabeli"),
      Zmienne = c("1 ilościowa + 1 jakościowa (2 grupy)", "2 ilościowe (ciągłe)",
                  "1 ilościowa (Y) + k ilościowych/jakościowych (X)", "2 jakościowe"),
      Dodatkowe = c("Normalność, równość wariancji", "Liniowość, normalność",
                    "Liniowość, normalność reszt, homoskedastyczność", "Niezależność obserwacji"),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

} # end server

shinyApp(ui = ui, server = server)
