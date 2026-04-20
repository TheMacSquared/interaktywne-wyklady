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
# Pakiet nie dostarcza kolumny `type`, tylko `profane` (TRUE/FALSE).
# Dodajemy ja, by uzywac jednolitych etykiet "word"/"death" w analizach nizej.
tarantino$type <- ifelse(tarantino$profane, "word", "death")

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

# Tab 5: Oceny hotelu - brak zmiennosci (n=80)
hotel_n <- 80
hotel_data <- data.frame(
  ocena_ogolna   = sample(1:5, hotel_n, replace = TRUE,
                          prob = c(0.01, 0.01, 0.06, 0.32, 0.60)),
  typ_pokoju     = factor(sample(c("Apartament Premium", "Pokoj standardowy", "Pokoj ekonomiczny"),
                                 hotel_n, replace = TRUE, prob = c(0.84, 0.09, 0.07))),
  dlugosc_pobytu = sample(1:3, hotel_n, replace = TRUE, prob = c(0.50, 0.35, 0.15)),
  cena_za_noc    = round(rnorm(hotel_n, mean = 450, sd = 85)),
  kraj_goscia    = factor(sample(c("Polska", "Niemcy", "UK", "Francja", "Inne"),
                                 hotel_n, replace = TRUE, prob = c(0.87, 0.05, 0.03, 0.03, 0.02))),
  stringsAsFactors = FALSE
)

# Tab 7: Formularz rejestracyjny kursu - zmienne dobre i zle (n=90)
reg_n <- 90
reg_data <- data.frame(
  wiek = sample(19:36, reg_n, replace = TRUE),
  wyksztalcenie = sample(c("technikum", "licencjat", "inzynier", "magister"),
                         reg_n, replace = TRUE, prob = c(0.12, 0.38, 0.22, 0.28)),
  doswiadczenie = sample(c("troche", "5 lat", "tak mam", "licencjat znam",
                            "3", "ponad rok", "nie mam", "duzo", "pare miesiecy", "brak"),
                          reg_n, replace = TRUE),
  dostepnosc = sample(c("weekendy", "nie w piatki", "elastycznie", "kiedy trzeba",
                         "tylko rano", "po 16:00", "zawsze", "pon-sr"),
                       reg_n, replace = TRUE),
  ocena_umiejetnosci = sample(c("7", "dobry", "8/10", "6", "bardzo dobry",
                                 "4", "B+", "9", "sredni", "7.5"),
                               reg_n, replace = TRUE),
  stringsAsFactors = FALSE
)

# Tab 7b: Dane do uratowania przez kategoryzację (n=12) — formularz kursu
fixable_data <- data.frame(
  id = 1:12,
  poziom = c("podstawowy", "podst.", "PODSTAWOWY",
             "sredniozaawansowany", "srednio zaawans.", "srednio-zaawansowany",
             "zaawansowany", "zaawans.", "expert",
             "podstawowy", "sred. zaawans.", "Zaawansowany"),
  platnosc = c("przelew", "przel.", "przelew bankowy",
               "gotowka", "gotowka", "Gotowka",
               "karta", "karta kredytowa", "paypal",
               "przelew", "gotowka", "PRZELEW"),
  godziny_tyg = c("5", "ok. 5", "4-6h", "8", "duzo", "3h",
                  "10", "7-8h", "malo", "6", "5h", "9"),
  stringsAsFactors = FALSE
)
fixable_data_cat <- data.frame(
  id = 1:12,
  poziom_kat = c("podstawowy","podstawowy","podstawowy",
                 "srednio-zaaw.","srednio-zaaw.","srednio-zaaw.",
                 "zaawansowany","zaawansowany","zaawansowany",
                 "podstawowy","srednio-zaaw.","zaawansowany"),
  platnosc_kat = c("przelew","przelew","przelew",
                   "gotowka","gotowka","gotowka",
                   "karta","karta","karta",
                   "przelew","gotowka","przelew"),
  nauka_kat = c("srednie (4-6h)","srednie (4-6h)","srednie (4-6h)",
                "duzo (7h+)", NA, "malo (1-3h)",
                "duzo (7h+)","duzo (7h+)", NA,
                "srednie (4-6h)","srednie (4-6h)","duzo (7h+)"),
  stringsAsFactors = FALSE
)

# Tab 8: Badania laboratoryjne - bledy danych (n=150)
lab_n   <- 150
lab_wiek <- sample(22:77, lab_n, replace = TRUE)
lab_hem  <- round(pmax(10.5, pmin(19.0,
              15.8 - (lab_wiek - 22) * 0.045 + rnorm(lab_n, 0, 1.4))), 1)
lab_gluk <- round(pmax(60, pmin(180, rnorm(lab_n, 90, 20))), 0)
lab_cisc <- round(pmax(85, pmin(175, rnorm(lab_n, 122, 17))), 0)
lab_data <- data.frame(
  wiek        = lab_wiek,
  plec        = factor(sample(c("K", "M"), lab_n, replace = TRUE)),
  hemoglobina = lab_hem,
  glukoza     = lab_gluk,
  cisnienie   = lab_cisc,
  stringsAsFactors = FALSE
)
# Prawdziwy outlier (nie blad): pacjent z niekontrolowana cukrzyca
lab_data$glukoza[100] <- 310
# Bledy wprowadzania danych
lab_data$hemoglobina[3]  <- -14.2   # ujemna — niemozliwa
lab_data$hemoglobina[17] <- 1420    # brak przecinka (powinno byc 14.20)
lab_data$cisnienie[42]   <- -70     # ujemne cisnienie — niemozliwe
lab_data$glukoza[28]     <- 11000   # 3 zera za duzo (powinno byc 110)
lab_data$hemoglobina[55] <- 0       # 0 = blad kodowania braków
lab_data$wiek[71]        <- 108     # prawdopodobny blad wpisania

# Tab 10: Kawiarnia studencka - sprzedaz dzienna (~rok akademicki = 245 dni)
cafe_n     <- 245
cafe_dates <- seq(as.Date("2023-10-01"), by = "day", length.out = cafe_n)
cafe_dow   <- as.integer(format(cafe_dates, "%u"))  # 1=Pon, 7=Nd
cafe_dow_effect <- c(75, 70, 80, 68, 58, 28, 20)[cafe_dow]
cafe_sem_idx    <- seq_len(cafe_n)
cafe_sem_effect <- 15 * sin(2 * pi * cafe_sem_idx / 245)
cafe_base <- 60 + cafe_dow_effect + cafe_sem_effect
cafe_kawy <- round(cafe_base + rnorm(cafe_n, 0, 10))
cafe_kawy <- pmax(8, cafe_kawy)
cafe_kawy[sort(sample(cafe_n, round(0.13 * cafe_n)))] <- NA   # ~13% brakow
cafe_temp <- round(12 - 10 * cos(2 * pi * cafe_sem_idx / 365) + rnorm(cafe_n, 0, 3.5), 1)
cafe_temp[sample(cafe_n, round(0.03 * cafe_n))] <- NA         # ~3% brakow
cafe_data <- data.frame(
  dzien       = cafe_sem_idx,
  data        = format(cafe_dates, "%d.%m"),
  dzien_tyg   = c("Pon", "Wt", "Sr", "Czw", "Pt", "Sob", "Nd")[cafe_dow],
  kawy        = cafe_kawy,
  temperatura = cafe_temp,
  stringsAsFactors = FALSE
)

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

# Problem 6: Brak niezaleznosci (temperatura dzienna, pol roku + agregat miesieczny)
local({
  set.seed(42)
  daty <- seq(as.Date("2023-10-01"), as.Date("2024-03-31"), by = "day")
  n <- length(daty)
  doy <- as.numeric(format(daty, "%j"))
  trend <- 8 + 12 * cos(2 * pi * (doy - 196) / 365)  # max ~lipiec, min ~styczen
  temp <- round(trend + rnorm(n, 0, 2.2), 1)
  cat_timeseries <<- data.frame(
    data = daty,
    miesiac = format(daty, "%Y-%m"),
    temperatura = temp,
    stringsAsFactors = FALSE
  )
})
cat_timeseries_monthly <- cat_timeseries |>
  dplyr::group_by(miesiac) |>
  dplyr::summarise(
    srednia_temp = round(mean(temperatura), 1),
    n_dni = dplyr::n(),
    .groups = "drop"
  ) |>
  as.data.frame()

# Problem 7: Zla struktura (pacjenci vs wizyty)
local({
  set.seed(123)
  n_pacjentow <- 30
  # Kazdy pacjent ma 3-6 wizyt (srednio 4) -> okolo 120 wizyt
  liczby_wizyt <- sample(3:6, n_pacjentow, replace = TRUE, prob = c(0.15, 0.35, 0.35, 0.15))
  plec_pacjenta <- sample(c("K", "M"), n_pacjentow, replace = TRUE, prob = c(0.55, 0.45))
  # Bazowe cisnienie pacjenta (kobiety nieco nizsze, ale z duzym rozrzutem)
  baseline <- ifelse(plec_pacjenta == "K", 128, 134) + round(rnorm(n_pacjentow, 0, 10))

  id_v <- integer(0); plec_v <- character(0); data_v <- as.Date(character(0)); cisn_v <- integer(0)
  for (i in seq_len(n_pacjentow)) {
    k <- liczby_wizyt[i]
    daty_wizyt <- sort(sample(seq(as.Date("2024-01-15"), as.Date("2024-09-30"), by = "day"), k))
    szum <- round(rnorm(k, 0, 5))
    cisnienia <- pmax(95, pmin(185, baseline[i] + szum))
    id_v <- c(id_v, rep(i, k))
    plec_v <- c(plec_v, rep(plec_pacjenta[i], k))
    data_v <- c(data_v, daty_wizyt)
    cisn_v <- c(cisn_v, cisnienia)
  }
  cat_patients_visits <<- data.frame(
    id_pacjenta = id_v,
    plec = plec_v,
    data_wizyty = data_v,
    cisnienie_skurczowe = cisn_v,
    stringsAsFactors = FALSE
  )
})
cat_patients_agg <- cat_patients_visits |>
  dplyr::group_by(id_pacjenta, plec) |>
  dplyr::summarise(
    srednie_cisnienie = round(mean(cisnienie_skurczowe), 1),
    n_wizyt = dplyr::n(),
    .groups = "drop"
  ) |>
  as.data.frame()

# ============================================================================
# FUNKCJE POMOCNICZE
# ============================================================================

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
