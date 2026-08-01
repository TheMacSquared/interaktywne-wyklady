# 📊 Metody bayesowskie

Interaktywny wykład porównujący podejście bayesowskie ze częstościowym. Każdy rozdział merytoryczny pokazuje ten sam problem w obu paradygmatach — strona w stronę, te same dane.

## 📋 Wymagania

- R (wersja ≥ 4.1)
- Pakiety R: `shiny`, `bslib`, `ggplot2`, `dplyr`, `tidyr`, `rstatix`, `broom`, `BayesFactor`, `rstanarm`

## 🚀 Instalacja

```r
install.packages(c(
  "shiny", "bslib", "ggplot2", "dplyr", "tidyr",
  "rstatix", "broom",
  "BayesFactor", "rstanarm"
))
```

> `rstanarm` wymaga kompilatora C++ (zawiera Stan). Pierwsza instalacja może zająć 10–15 minut.

## ▶️ Uruchamianie

```r
shiny::runApp("statystyka/08-metody-bayesowskie")
```

## 📚 Struktura wykładu (12 rozdziałów)

| # | Rozdział | Metoda freq | Metoda Bayes | Porównanie |
|---|----------|-------------|---------------|------------|
| 1 | Intuicja | — | prior/likelihood/posterior (beta-binomial) | — (wprowadzenie) |
| 2 | BF vs p-value | `t.test()` | `ttestBF()` | dwukolumnowe |
| 3 | HDI vs CI | 95% CI (t) | 95% HDI | dwukolumnowe |
| 4 | Jedna próba | `t.test(x, mu)` | `ttestBF(x, mu)` + posterior μ | dwukolumnowe |
| 5 | Dwie grupy | Welch t-test | `ttestBF(formula)` + posterior różnicy | dwukolumnowe |
| 6 | ANOVA | `aov()` / F-test | `anovaBF()` | dwukolumnowe |
| 7 | Tabele krzyżowe | `chisq.test()` | `contingencyTableBF()` + posterior OR | dwukolumnowe |
| 8 | Korelacja | `cor.test()` | `correlationBF()` + posterior ρ | dwukolumnowe |
| 9 | Regresja liniowa | `lm()` | `rstanarm::stan_glm()` | forest plot: CI vs HDI |
| 10 | Regresja logistyczna | `glm(binomial)` | `stan_glm(binomial)` + posterior OR | forest plot: OR + HDI |
| 11 | Ściąga | — | — | tabela paradygmat↔paradygmat |
| 12 | Ćwiczenia | — | — | dropdown kierunku (Rolnictwo / TŻ / BHP / Edukacja) |

## 🎯 Koncepcje pedagogiczne

### Co ilustruje to narzędzie?

- **Różnica interpretacyjna**: co naprawdę znaczy p-value, a co BF; co znaczy 95% CI, a co 95% HDI
- **Paradoks Lindleya**: duża próba + mały efekt → niskie p, ale BF mówi „słaby dowód"
- **Dowód za H₀**: bayesowsko można go kwantyfikować (BF < 1/3), częstościowo nie
- **Regularyzacja priorem**: jak silny prior stabilizuje estymację przy małych próbach
- **Pytania praktyczne**: P(różnica > próg), P(OR > 2) — bezpośrednie odpowiedzi z posterior

### Dwukolumnowy układ

W rozdziałach merytorycznych (ch2–ch10) ten sam dataset jest analizowany obiema metodami jednocześnie. Lewa kolumna (panel czerwony) — podejście częstościowe. Prawa (panel fioletowy) — bayesowskie. Pod spodem akapit „werdykt": co mówią oba paradygmaty o tym konkretnym wyniku.

## 💡 Scenariusze na zajęciach

1. **Wprowadzenie do Bayesa (ch1–ch3)**: 30–45 min. Prowadzący pokazuje, jak prior aktualizuje się do posterior; pokazuje paradoks Lindleya z ch2.
2. **Praktyczne testy (ch4–ch8)**: dla każdego z bazowych testów (t, ANOVA, tabele, korelacja) — porównanie na jednej próbie. 60–90 min.
3. **Regresja bayesowska (ch9–ch10)**: osobne zajęcia, bo `stan_glm` wymaga chwili na fit. 60 min.
4. **Synteza (ch11)**: ściąga-decyzja do powtórki.
5. **Samodzielnie (ch12)**: ćwiczenia kierunkowe jako zadanie domowe.

## 🛠️ Techniczne szczegóły

### Silniki obliczeniowe

- `BayesFactor` (dla ch2–ch8): szybkie obliczenia analityczne/numeryczne, brak MCMC w trakcie klikania. Priory Cauchy/Jeffreys domyślne.
- `rstanarm` (dla ch9–ch10): MCMC pod maską. W aplikacji `chains = 2, iter = 1000, refresh = 0` — dla szybkości kosztem precyzji posteriora (wystarczające dydaktycznie). Przycisk „Dopasuj modele" uruchamia fit świadomie (nie przy każdej zmianie suwaka).

### Beta-binomial w ch1

Zaimplementowany ręcznie (`beta_binomial_posterior` w helpers.R) — dydaktycznie czytelny, żeby student widział, że posterior wynika analitycznie z priora × likelihood.

### HDI

`hdi_from_samples()` używa algorytmu Chen-Shao — najkrótsze okno z próbek pokrywające zadany % gęstości. Działa dla dowolnego rozkładu posteriora (także skośnego, np. OR).

## 🐛 Rozwiązywanie problemów

### „Nie mogę zainstalować rstanarm"

```r
# Windows: potrzebny Rtools
# macOS: xcode-select --install
# Linux: sudo apt install r-base-dev build-essential

# Alternatywnie: pakiet prekompilowany
install.packages("rstanarm", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

### „Fit stan_glm jest wolny"

Pierwszy fit po starcie aplikacji = ~5–15 s (kompilacja modelu Stan). Kolejne fity tego samego modelu są znacznie szybsze. Jeśli chcesz szybciej, zmniejsz `iter` w helpers.R:

```r
fit_bayes_lm(..., chains = 1, iter = 500)
```

### „BF₁₀ wyświetla ∞"

Bardzo silny dowód przy dużej próbie — obliczenia numeryczne dochodzą do granicy precyzji. Interpretacyjnie: dowód „ekstremalny". Zmniejsz n lub zbliż μ do μ₀, żeby zobaczyć konkretne liczby.

## 📝 Licencja

Projekt edukacyjny. Wolne do użytku i modyfikacji w celach dydaktycznych.
