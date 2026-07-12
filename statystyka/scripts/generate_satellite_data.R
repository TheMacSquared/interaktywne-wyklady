#!/usr/bin/env Rscript

# Syntetyczny zbiór dydaktyczny dla kierunku Inżynieria danych satelitarnych
# i kosmicznych. Jeden wiersz = jedna lokalizacja obserwowana jednego dnia.
# Dane przypominają tabelę po wstępnym przetworzeniu produktu satelitarnego;
# nie są surowym obrazem ani rzeczywistym produktem konkretnej misji.

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

stat_root <- normalizePath(file.path(script_dir(), ".."), mustWork = TRUE)
output_dir <- file.path(stat_root, "dane")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260712)
n <- 180L

typ_pokrycia <- sample(
  c("zwarta_zabudowa", "zabudowa_luzna", "park", "pole", "las"),
  n, replace = TRUE, prob = c(0.20, 0.22, 0.18, 0.22, 0.18)
)
strefa <- ifelse(typ_pokrycia %in% c("zwarta_zabudowa", "zabudowa_luzna"),
                 "miejska", "zielona")
region <- sample(c("zachod", "centrum", "wschod"), n, replace = TRUE,
                 prob = c(0.30, 0.42, 0.28))
data_obserwacji <- sample(as.Date(c("2026-05-18", "2026-06-03", "2026-06-19")),
                          n, replace = TRUE)

wysokosc_m <- round(pmax(80, rnorm(n, 165, 42)))
zachmurzenie_pct <- round(pmin(100, pmax(0, stats::rbeta(n, 1.5, 4.5) * 100)), 1)

ndvi_mean <- c(
  zwarta_zabudowa = 0.12,
  zabudowa_luzna = 0.28,
  park = 0.62,
  pole = 0.51,
  las = 0.76
)
ndvi <- ndvi_mean[typ_pokrycia] + rnorm(n, 0, 0.075) - 0.0012 * zachmurzenie_pct
ndvi <- round(pmin(0.92, pmax(-0.10, ndvi)), 3)

cover_effect <- c(
  zwarta_zabudowa = 4.2,
  zabudowa_luzna = 2.4,
  park = -0.4,
  pole = 0.5,
  las = -1.5
)
date_effect <- as.numeric(data_obserwacji - min(data_obserwacji)) * 0.055
grunt_temp_c <- 24.5 + cover_effect[typ_pokrycia] + date_effect -
  0.008 * (wysokosc_m - 150) + rnorm(n, 0, 1.25)

# Sensor jest dość precyzyjny, ale ma niewielkie dodatnie obciążenie.
sat_temp_c <- grunt_temp_c + 0.75 + rnorm(n, 0, 0.85) +
  0.012 * zachmurzenie_pct
roznica_temp_c <- sat_temp_c - grunt_temp_c

jakosc_pomiaru <- cut(
  zachmurzenie_pct,
  breaks = c(-Inf, 20, 50, Inf),
  labels = c("dobra", "ograniczona", "odrzucona"),
  ordered_result = TRUE
)
pomiar_dostepny <- ifelse(jakosc_pomiaru == "odrzucona", "nie", "tak")

df <- data.frame(
  id_lokalizacji = sprintf("P%03d", seq_len(n)),
  data_obserwacji = format(data_obserwacji),
  region = region,
  strefa = strefa,
  typ_pokrycia = typ_pokrycia,
  szerokosc_geo = round(runif(n, 50.85, 51.22), 5),
  dlugosc_geo = round(runif(n, 16.75, 17.35), 5),
  wysokosc_m = wysokosc_m,
  zachmurzenie_pct = zachmurzenie_pct,
  ndvi = ndvi,
  grunt_temp_c = round(grunt_temp_c, 2),
  sat_temp_c = round(sat_temp_c, 2),
  roznica_temp_c = round(roznica_temp_c, 2),
  jakosc_pomiaru = as.character(jakosc_pomiaru),
  pomiar_dostepny = pomiar_dostepny,
  stringsAsFactors = FALSE
)

output <- file.path(output_dir, "satelitarne_obserwacje.csv")
utils::write.csv(df, output, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Zapisano %d obserwacji: %s\n", nrow(df), output))
