# Generowanie danych do ćwiczeń z rozkładów prawdopodobieństwa
# Kontekst: Inżynieria Bezpieczeństwa / BHP
# Uruchom: source("generate_data.R") lub Rscript generate_data.R

set.seed(2026)

output_dir <- "dane"
dir.create(output_dir, showWarnings = FALSE)

# ============================================================================
# 1. Wypadki miesięcznie — Poisson(λ = 2.5)
# ============================================================================
# Liczba wypadków przy pracy na miesiąc w zakładzie produkcyjnym (5 lat danych)

wypadki <- data.frame(
  miesiac = rep(1:12, 5),
  rok = rep(2021:2025, each = 12),
  liczba_wypadkow = rpois(60, lambda = 2.5)
)

write.csv(wypadki, file.path(output_dir, "wypadki_miesiecznie.csv"), row.names = FALSE)
cat("Wypadki: mean =", mean(wypadki$liczba_wypadkow),
    ", var =", var(wypadki$liczba_wypadkow), "\n")

# ============================================================================
# 2. Hałas na stanowiskach — normalny vs skośny
# ============================================================================
# Stanowisko A: hala montażowa — pomiary bliskie normalnemu
# Stanowisko B: spawalnia — pomiary skośne (sporadyczne szczyty)

halas <- data.frame(
  stanowisko = rep(c("A_montaz", "B_spawalnia"), each = 50),
  halas_dB = c(
    rnorm(50, mean = 82, sd = 4),             # A: normalny
    rlnorm(50, meanlog = log(78), sdlog = 0.12) # B: log-normalny (skośny prawo)
  )
)

write.csv(halas, file.path(output_dir, "halas_stanowiska.csv"), row.names = FALSE)
cat("Hałas A: mean =", mean(halas$halas_dB[1:50]),
    ", sd =", sd(halas$halas_dB[1:50]), "\n")
cat("Hałas B: mean =", mean(halas$halas_dB[51:100]),
    ", sd =", sd(halas$halas_dB[51:100]), "\n")

# ============================================================================
# 3. Kontrola kasków — Binomial(n = 30, p = 0.05)
# ============================================================================
# Inspekcja partii kasków ochronnych: 30 kasków w partii, 5% wadliwości
# 40 partii zbadanych

kontrola <- data.frame(
  partia = 1:40,
  liczba_wadliwych = rbinom(40, size = 30, prob = 0.05)
)

write.csv(kontrola, file.path(output_dir, "kontrola_kaskow.csv"), row.names = FALSE)
cat("Kaski wadliwe: mean =", mean(kontrola$liczba_wadliwych),
    ", var =", var(kontrola$liczba_wadliwych), "\n")

# ============================================================================
# 4. Czas między incydentami BHP — Exp(rate = 1/15)
# ============================================================================
# Średnio 1 incydent co 15 dni

czas_incydenty <- data.frame(
  numer_incydentu = 1:80,
  dni_od_poprzedniego = rexp(80, rate = 1/15)
)

write.csv(czas_incydenty, file.path(output_dir, "czas_miedzy_incydentami.csv"), row.names = FALSE)
cat("Czas między incydentami: mean =", mean(czas_incydenty$dni_od_poprzedniego),
    ", sd =", sd(czas_incydenty$dni_od_poprzedniego), "\n")

cat("\nWszystkie pliki zapisane w folderze:", output_dir, "\n")
cat("Pliki:", paste(list.files(output_dir), collapse = ", "), "\n")
