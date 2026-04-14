# Generowanie danych do ćwiczeń z rozkładów prawdopodobieństwa
# Kontekst: Technologia Żywności i Żywienie Człowieka
# Uruchom: source("generate_data.R") lub Rscript generate_data.R

set.seed(2026)

output_dir <- "dane"
dir.create(output_dir, showWarnings = FALSE)

# ============================================================================
# 1. Reklamacje tygodniowo — Poisson(λ = 4)
# ============================================================================
# Liczba reklamacji konsumentów na tydzień (2 lata danych, 52 tygodnie/rok)

reklamacje <- data.frame(
  tydzien = rep(1:52, 2),
  rok = rep(c("2024", "2025"), each = 52),
  liczba_reklamacji = rpois(104, lambda = 4)
)

write.csv(reklamacje, file.path(output_dir, "reklamacje_tygodniowo.csv"), row.names = FALSE)
cat("Reklamacje: mean =", round(mean(reklamacje$liczba_reklamacji), 2),
    ", var =", round(var(reklamacje$liczba_reklamacji), 2), "\n")

# ============================================================================
# 2. Masa netto na dwóch liniach produkcyjnych — normalny vs skośny
# ============================================================================
# Linia A: dobrze skalibrowana — pomiary bliskie normalnemu N(1000, 5)
# Linia B: stara głowica — sporadyczne przepełnienia (skośna prawo)

masa <- data.frame(
  linia = rep(c("A_nowa", "B_stara"), each = 80),
  masa_netto_g = c(
    rnorm(80, mean = 1000, sd = 5),                         # A: normalny
    rlnorm(80, meanlog = log(999), sdlog = 0.008) + rexp(80, rate = 0.3)  # B: skośny prawo
  )
)

write.csv(masa, file.path(output_dir, "masa_netto_linie.csv"), row.names = FALSE)
cat("Masa A: mean =", round(mean(masa$masa_netto_g[1:80]), 2),
    ", sd =", round(sd(masa$masa_netto_g[1:80]), 2), "\n")
cat("Masa B: mean =", round(mean(masa$masa_netto_g[81:160]), 2),
    ", sd =", round(sd(masa$masa_netto_g[81:160]), 2), "\n")

# ============================================================================
# 3. Kontrola wadliwości opakowań — Binomial(n = 40, p = 0.045)
# ============================================================================
# Partie 40 opakowań, wadliwość ~4.5%, 50 kontrolowanych partii

kontrola <- data.frame(
  partia = 1:50,
  liczba_wadliwych = rbinom(50, size = 40, prob = 0.045)
)

write.csv(kontrola, file.path(output_dir, "kontrola_opakowania.csv"), row.names = FALSE)
cat("Opakowania wadliwe: mean =", round(mean(kontrola$liczba_wadliwych), 2),
    ", var =", round(var(kontrola$liczba_wadliwych), 2), "\n")

# ============================================================================
# 4. Czas między awariami linii — Exp(rate = 1/4)
# ============================================================================
# Średnio awaria co 4 godziny na linii pakującej

czas_awarie <- data.frame(
  numer_awarii = 1:80,
  godziny_od_poprzedniej = rexp(80, rate = 1/4)
)

write.csv(czas_awarie, file.path(output_dir, "czas_miedzy_awariami.csv"), row.names = FALSE)
cat("Czas między awariami: mean =", round(mean(czas_awarie$godziny_od_poprzedniej), 2),
    ", sd =", round(sd(czas_awarie$godziny_od_poprzedniej), 2), "\n")

cat("\nWszystkie pliki zapisane w folderze:", output_dir, "\n")
cat("Pliki:", paste(list.files(output_dir), collapse = ", "), "\n")
