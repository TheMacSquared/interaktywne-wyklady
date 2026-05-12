# Generowanie danych do ćwiczeń z rozkładów prawdopodobieństwa
# Kontekst: Rolnictwo
# Uruchom: source("generate_data.R") lub Rscript generate_data.R

set.seed(2026)

output_dir <- "dane"
dir.create(output_dir, showWarnings = FALSE)

# ============================================================================
# 1. Szkodniki tygodniowo — Poisson(λ = 2.8)
# ============================================================================
# Liczba wystąpień szkodników na polu kukurydzy tygodniowo (3 sezony, 15 tyg.)

szkodniki <- data.frame(
  tydzien = rep(1:15, 3),
  sezon = rep(c("2023", "2024", "2025"), each = 15),
  liczba_wystapien = rpois(45, lambda = 2.8)
)

write.csv(szkodniki, file.path(output_dir, "szkodniki_tygodniowo.csv"), row.names = FALSE)
cat("Szkodniki: mean =", mean(szkodniki$liczba_wystapien),
    ", var =", var(szkodniki$liczba_wystapien), "\n")

# ============================================================================
# 2. Plony na dwóch odmianach pszenicy — normalny vs skośny
# ============================================================================
# Odmiana A: stabilna, plon bliski normalnemu
# Odmiana B: wrażliwa na suszę — skośna lewa (większość dobrze, ale zdarzają się klęski)

plony <- data.frame(
  odmiana = rep(c("A_stabilna", "B_wrazliwa"), each = 60),
  plon_tha = c(
    rnorm(60, mean = 6.2, sd = 0.8),                        # A: normalny
    pmax(2.0, rnorm(60, mean = 5.8, sd = 1.5) - rexp(60, rate = 2))  # B: skośny lewo
  )
)

write.csv(plony, file.path(output_dir, "plony_odmiany.csv"), row.names = FALSE)
cat("Plony A: mean =", round(mean(plony$plon_tha[1:60]), 2),
    ", sd =", round(sd(plony$plon_tha[1:60]), 2), "\n")
cat("Plony B: mean =", round(mean(plony$plon_tha[61:120]), 2),
    ", sd =", round(sd(plony$plon_tha[61:120]), 2), "\n")

# ============================================================================
# 3. Kontrola jakości nasion — Binomial(n = 50, p = 0.03)
# ============================================================================
# Partie 50 nasion, wadliwość 3%, 40 kontrolowanych partii

kontrola <- data.frame(
  partia = 1:40,
  liczba_wadliwych = rbinom(40, size = 50, prob = 0.03)
)

write.csv(kontrola, file.path(output_dir, "kontrola_nasion.csv"), row.names = FALSE)
cat("Nasiona wadliwe: mean =", round(mean(kontrola$liczba_wadliwych), 2),
    ", var =", round(var(kontrola$liczba_wadliwych), 2), "\n")

# ============================================================================
# 4. Czas między deszczami — Exp(rate = 1/10)
# ============================================================================
# Średnio deszcz co 10 dni w lipcu w regionie rolniczym

czas_deszcze <- data.frame(
  numer_opadu = 1:60,
  dni_od_poprzedniego = rexp(60, rate = 1/10)
)

write.csv(czas_deszcze, file.path(output_dir, "czas_miedzy_deszczami.csv"), row.names = FALSE)
cat("Czas między deszczami: mean =", round(mean(czas_deszcze$dni_od_poprzedniego), 2),
    ", sd =", round(sd(czas_deszcze$dni_od_poprzedniego), 2), "\n")

cat("\nWszystkie pliki zapisane w folderze:", output_dir, "\n")
cat("Pliki:", paste(list.files(output_dir), collapse = ", "), "\n")
