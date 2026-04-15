# Generowanie danych do ćwiczeń z przedziałów ufności
# Trzy kierunki: BHP, Rolnictwo, Technologia Żywności
# Uruchom: Rscript generate_kierunki.R z folderu dane/

set.seed(2026)

# ============================================================================
# BHP — zakłady produkcyjne
# ============================================================================
# 320 zakładów produkcyjnych; zmienne:
#   wskaznik_wypadkow  — liczba wypadków na 1000 pracowników/rok (ciągła)
#   absencja_dni       — średnia liczba dni absencji chorobowej na pracownika/rok
#   zmianowosci        — "jedna" / "dwie" / "trzy" zmiany
#   branza             — "spozywcza" / "metalowa" / "chemiczna"
#   wielkosc           — "maly" / "sredni" / "duzy" (wg liczby pracowników)
#   szkolenia_rocznie  — liczba szkoleń BHP rocznie na pracownika
#   naruszen_proc      — procent kontroli z naruszeniami
#   ponad_norma_halas  — logiczny: czy hałas > 85 dB na stanowiskach pracy

n_bhp <- 320

branza    <- sample(c("spozywcza","metalowa","chemiczna"), n_bhp, replace=TRUE, prob=c(.35,.40,.25))
wielkosc  <- sample(c("maly","sredni","duzy"), n_bhp, replace=TRUE, prob=c(.40,.40,.20))
zmiany    <- sample(c("jedna","dwie","trzy"), n_bhp, replace=TRUE, prob=c(.30,.45,.25))

# Wskaźnik wypadków — zależny od branży i zmianowości
mu_wyp <- ifelse(branza == "chemiczna", 8.5,
           ifelse(branza == "metalowa", 11.2, 6.3)) +
          ifelse(zmiany == "trzy", 2.1,
           ifelse(zmiany == "dwie", 0.8, 0))
wskaznik_wypadkow <- pmax(0, rnorm(n_bhp, mean = mu_wyp, sd = 3.2))

# Absencja — zależna od wskaźnika wypadków + szum
absencja_dni <- pmax(0, 8.5 + 0.6 * wskaznik_wypadkow + rnorm(n_bhp, 0, 4.1))

# Szkolenia i naruszenia
szkolenia_rocznie <- pmax(0, round(rnorm(n_bhp, mean = 3.2, sd = 1.8)))
naruszen_proc <- pmax(0, pmin(100, 18 - 2.1 * szkolenia_rocznie + rnorm(n_bhp, 0, 8)))

# Hałas > 85 dB: częstsze w metalowej i przy 3 zmianach
p_halas <- ifelse(branza == "metalowa", 0.52,
            ifelse(branza == "chemiczna", 0.31, 0.18))
ponad_norma_halas <- rbinom(n_bhp, 1, p_halas) == 1

bhp <- data.frame(
  id = 1:n_bhp,
  branza, wielkosc, zmiany,
  wskaznik_wypadkow = round(wskaznik_wypadkow, 2),
  absencja_dni      = round(absencja_dni, 1),
  szkolenia_rocznie,
  naruszen_proc     = round(naruszen_proc, 1),
  ponad_norma_halas
)

write.csv(bhp, "bhp_zaklady.csv", row.names = FALSE)
cat("BHP: n =", n_bhp, "\n")
cat("  wskaznik_wypadkow: mean =", round(mean(bhp$wskaznik_wypadkow),2),
    "sd =", round(sd(bhp$wskaznik_wypadkow),2), "\n")
cat("  absencja_dni: mean =", round(mean(bhp$absencja_dni),2),
    "sd =", round(sd(bhp$absencja_dni),2), "\n")
cat("  prop(halas): p =", round(mean(bhp$ponad_norma_halas),3), "\n")
cat("  prop(naruszen > 20%): p =", round(mean(bhp$naruszen_proc > 20),3), "\n\n")

# ============================================================================
# ROLNICTWO — pola uprawne
# ============================================================================
# 280 pól uprawnych; zmienne:
#   plon_pszenicy   — t/ha (ciągła)
#   plon_rzepa      — t/ha (ciągła)
#   klasa_gleby     — "I" / "II" / "III" (klasa bonitacyjna)
#   nawozenie       — "niskie" / "srednie" / "wysokie" (kg N/ha)
#   powierzchnia_ha — powierzchnia pola w ha
#   wilgotnosc_proc — % wilgotności gleby
#   ph_gleby        — odczyn gleby (ciągła)
#   plon_ponizej_5  — logiczny: czy plon pszenicy < 5 t/ha
#   wilg_powyzej_70 — logiczny: czy wilgotność > 70%

n_rol <- 280

klasa     <- sample(c("I","II","III"), n_rol, replace=TRUE, prob=c(.25,.45,.30))
nawozenie <- sample(c("niskie","srednie","wysokie"), n_rol, replace=TRUE, prob=c(.25,.50,.25))

# Plon pszenicy — zależny od klasy gleby i nawożenia
mu_psz <- ifelse(klasa == "I", 7.1,
           ifelse(klasa == "II", 6.1, 4.9)) +
          ifelse(nawozenie == "wysokie", 0.8,
           ifelse(nawozenie == "srednie", 0.3, -0.4))
plon_pszenicy <- pmax(1.5, rnorm(n_rol, mean = mu_psz, sd = 0.85))

# Plon rzepaku — mniejsza zmienność
mu_rzep <- 0.55 * mu_psz + 0.8 + rnorm(n_rol, 0, 0.5)
plon_rzepa <- pmax(0.8, mu_rzep)

# Wilgotność i pH
wilgotnosc_proc <- pmax(20, pmin(95, rnorm(n_rol, 58, 14)))
ph_gleby <- round(pmax(4.5, pmin(8.0, rnorm(n_rol, 6.4, 0.7))), 1)
powierzchnia_ha <- round(pmax(1, rlnorm(n_rol, meanlog = log(18), sdlog = 0.8)), 1)

rolnictwo <- data.frame(
  id = 1:n_rol,
  klasa_gleby = klasa, nawozenie,
  powierzchnia_ha,
  plon_pszenicy = round(plon_pszenicy, 2),
  plon_rzepa    = round(plon_rzepa, 2),
  wilgotnosc_proc = round(wilgotnosc_proc, 1),
  ph_gleby,
  plon_ponizej_5  = plon_pszenicy < 5,
  wilg_powyzej_70 = wilgotnosc_proc > 70
)

write.csv(rolnictwo, "rolnictwo_pola.csv", row.names = FALSE)
cat("Rolnictwo: n =", n_rol, "\n")
cat("  plon_pszenicy: mean =", round(mean(rolnictwo$plon_pszenicy),2),
    "sd =", round(sd(rolnictwo$plon_pszenicy),2), "\n")
cat("  plon_rzepa: mean =", round(mean(rolnictwo$plon_rzepa),2),
    "sd =", round(sd(rolnictwo$plon_rzepa),2), "\n")
cat("  prop(plon < 5): p =", round(mean(rolnictwo$plon_ponizej_5),3), "\n")
cat("  prop(wilg > 70): p =", round(mean(rolnictwo$wilg_powyzej_70),3), "\n\n")

# ============================================================================
# TECHNOLOGIA ŻYWNOŚCI — partie produkcyjne
# ============================================================================
# 350 partii produkcyjnych; zmienne:
#   zawartosc_bialka  — % białka w produkcie (ciągła)
#   zawartosc_tluszczu— % tłuszczu (ciągła)
#   linia             — "A" / "B" / "C" (linia produkcyjna)
#   dostawca          — "lokalny" / "krajowy" / "importowany"
#   temperatura_proc  — temperatura procesu (°C)
#   czas_procesu_min  — czas procesu w minutach
#   bialko_ponizej_normy — logiczny: zawartosc_bialka < 26%
#   tluszcz_powyzej_normy— logiczny: zawartosc_tluszczu > 3.0%

n_zyw <- 350

linia    <- sample(c("A","B","C"), n_zyw, replace=TRUE, prob=c(.40,.35,.25))
dostawca <- sample(c("lokalny","krajowy","importowany"), n_zyw, replace=TRUE, prob=c(.35,.45,.20))

# Białko — zależne od dostawcy i linii
mu_bialko <- ifelse(dostawca == "importowany", 27.8,
              ifelse(dostawca == "krajowy", 26.9, 26.1)) +
             ifelse(linia == "A", 0.3,
              ifelse(linia == "B", 0.0, -0.5))
zawartosc_bialka <- rnorm(n_zyw, mean = mu_bialko, sd = 1.1)

# Tłuszcz — mniejsza zmienność
mu_tluszcz <- 2.7 + ifelse(linia == "C", 0.25, 0) + rnorm(n_zyw, 0, 0.22)
zawartosc_tluszczu <- pmax(1.5, mu_tluszcz)

# Parametry procesu
temperatura_proc <- round(rnorm(n_zyw, 78, 4.5), 1)
czas_procesu_min <- round(pmax(10, rnorm(n_zyw, 45, 8)))

zywnosc <- data.frame(
  id = 1:n_zyw,
  linia, dostawca,
  zawartosc_bialka   = round(zawartosc_bialka, 2),
  zawartosc_tluszczu = round(zawartosc_tluszczu, 3),
  temperatura_proc,
  czas_procesu_min,
  bialko_ponizej_normy   = zawartosc_bialka < 26,
  tluszcz_powyzej_normy  = zawartosc_tluszczu > 3.0
)

write.csv(zywnosc, "zywnosc_partie.csv", row.names = FALSE)
cat("Żywność: n =", n_zyw, "\n")
cat("  zawartosc_bialka: mean =", round(mean(zywnosc$zawartosc_bialka),2),
    "sd =", round(sd(zywnosc$zawartosc_bialka),2), "\n")
cat("  zawartosc_tluszczu: mean =", round(mean(zywnosc$zawartosc_tluszczu),3),
    "sd =", round(sd(zywnosc$zawartosc_tluszczu),3), "\n")
cat("  prop(bialko < 26): p =", round(mean(zywnosc$bialko_ponizej_normy),3), "\n")
cat("  prop(tluszcz > 3): p =", round(mean(zywnosc$tluszcz_powyzej_normy),3), "\n\n")

cat("Wszystkie pliki zapisane.\n")
