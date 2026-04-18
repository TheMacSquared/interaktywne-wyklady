# helpers.R — dane globalne i funkcje pomocnicze dla dobre-dane

# ============================================================================
# KOLORY LOKALNE (col_primary, col_dark -> R/shared.R)
# ============================================================================

col_good    <- "#27ae60"
col_mixed   <- "#f39c12"
col_bad     <- "#e74c3c"

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
