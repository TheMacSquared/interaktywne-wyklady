# 📊 Założenia Testów Statystycznych - Przewodnik Interaktywny

Interaktywna aplikacja Shiny do nauczania założeń testów statystycznych.

## Uruchomienie

```r
shiny::runApp("dydaktyka/04-szablony/narzedzia/Interaktywne/zalozenia-testow")
```

## Struktura aplikacji

### 📈 Tab 1: Założenie normalności
- **Cel:** Pokazać wizualnie DLACZEGO testy parametryczne wymagają normalności
- **Scenariusze:** Normalny, lekko skośny, silnie skośny, bimodalny, z outlierami
- **Wizualizacje:** Histogram z overlay, QQ-plot, test Shapiro-Wilka

### 📊 Tab 2: Jednorodność wariancji
- **Cel:** Pokazać DLACZEGO testy wymagają równych wariancji
- **Scenariusze:** Równe, lekko różne, bardzo różne, różne n + różne wariancje
- **Wizualizacje:** Boxploty, statystyki opisowe, test Levene'a

### 🔍 Tab 3: Porównanie testów parametrycznych vs nieparametrycznych

#### 3a. t-test vs Wilcoxon/Mann-Whitney
- **Scenariusze:** Normalny, skośny, z outlierami, małe n
- **Wizualizacje:** Boxploty, rozkłady, tabela wyników

#### 3b. Pearson vs Spearman
- **Scenariusze:** Liniowy, monotoniczny nieliniowy, z outlierami, brak związku
- **Wizualizacje:** Scatterplot z linią trendu, tabela wyników korelacji

#### 3c. ANOVA vs Kruskal-Wallis
- **Scenariusze:** Normalny, skośny, różne wariancje, z outlierami
- **Wizualizacje:** Boxploty 3 grup, tabela wyników testów

### 📉 Tab 4: Założenia regresji

#### 4a. Normalność reszt
- **Scenariusze:** Reszty normalne, skośne, z outlierami
- **Wizualizacje:** Scatterplot, histogram reszt, QQ-plot reszt, test Shapiro-Wilka
- **Kluczowe:** Normalność reszt ≠ normalność Y!

#### 4b. Homoskedastyczność reszt
- **Scenariusze:** Stała wariancja, rozrzut rośnie, rozrzut maleje
- **Wizualizacje:** Scatterplot, **Residual plot** (kluczowy!), test Breusch-Pagan
- **Kluczowe:** Residual plot powinien być chmurą punktów wokół y=0

#### 4c. Wpływ outlierów na regresję
- **Scenariusze:** Bez outlierów, outlier w Y, outlier w X i Y, kilka outlierów
- **Wizualizacje:** Scatterplot z 2 liniami (z/bez outlierów), tabela porównawcza modeli
- **Kluczowe:** Outliery mogą drastycznie zmienić linię regresji!

## Pakiety wymagane

```r
library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
library(rstatix)
library(lmtest)
```

## Użycie dydaktyczne

Aplikacja została zaprojektowana do wsparcia wykładów o założeniach testów:

1. **Demonstracja na żywo:** Pokaż jak naruszenie założeń wpływa na wyniki testów
2. **Interaktywność:** Studenci mogą samodzielnie eksplorować scenariusze
3. **Wizualizacja idei:** Nacisk na zrozumienie DLACZEGO, nie tylko wzory
4. **Porównania:** Bezpośrednie porównanie testów parametrycznych vs nieparametrycznych

## Przykładowe ścieżki użycia

### Ścieżka 1: "Kiedy używać testów nieparametrycznych?"
1. Tab 1 → Wybierz "Silnie skośny" → Pokaż QQ-plot (wyraźne odchylenie)
2. Tab 3 → t-test vs Wilcoxon → Wybierz "Skośne rozkłady" → Porównaj wyniki
3. **Wniosek:** Wilcoxon jest bezpieczniejszy przy naruszeniu normalności

### Ścieżka 2: "Dlaczego sprawdzać założenia regresji?"
1. Tab 4a → "Reszty skośne" → Pokaż QQ-plot reszt (odchylenie)
2. Tab 4b → "Rozrzut rośnie" → Pokaż residual plot (kształt lejka)
3. Tab 4c → "Outlier w X i Y" → Pokaż jak zmienia się linia
4. **Wniosek:** Naruszenia założeń = błędne wnioski!

### Ścieżka 3: "Pearson vs Spearman - który wybrać?"
1. Tab 3b → "Liniowy związek" → Oba podobne wyniki
2. Tab 3b → "Monotoniczny nieliniowy" → Spearman lepszy
3. Tab 3b → "Z outlierami" → Spearman odporniejszy
4. **Wniosek:** Spearman bezpieczniejszy, Pearson silniejszy przy liniowości

## Rozszerzenia (future work)

- Export danych do CSV
- Upload własnych danych użytkownika
- Symulacje mocy testów (Monte Carlo)
- Więcej testów (Chi-kwadrat, Fisher)
- Shinylive export (uruchomienie w przeglądarce bez R)

## Autor

Maciej - Dydaktyka 2024/2025
