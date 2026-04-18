# Log eksperymentów wizualnych

Piaskownica do testowania zmian graficznych bez ryzyka dla produkcji.
Po akceptacji eksperymentu przenosimy do `R/shared_styles.css` / `R/shared.R`.

## Struktura

```
sandbox/
├── testowy-wyklad/          # kopia dobre-dane/ z podpiętym experimental.css
│   ├── app.R
│   └── www/experimental.css # główne miejsce eksperymentów
├── fonts-comparison/        # mini-apka: 4 fonty obok siebie
│   └── app.R
└── EKSPERYMENTY.md          # ten plik
```

## Uruchamianie

```r
# Porównanie fontów (szybki podgląd obok siebie)
shiny::runApp("sandbox/fonts-comparison")

# Pełny testowy wykład ze zmianami
shiny::runApp("sandbox/testowy-wyklad")
```

## Jak eksperymentować z fontami

W `testowy-wyklad/www/experimental.css` są 4 bloki `@import` — odkomentuj
jeden, zakomentuj pozostałe, przeładuj apkę.

Aktualnie aktywna: **Source Sans 3 + Source Serif 4** (rekomendacja).

---

## Dziennik zmian

### 2026-04-18 — początek eksperymentu

**Cel sesji:** wybrać font, przetestować hierarchię typograficzną H1/H2/H3,
`max-width` narracji, ikony w callout-ach.

**Zastosowane zmiany (experimental.css):**
- Font: Source Sans 3 (UI/nagłówki) + Source Serif 4 (narracja)
- Kod: JetBrains Mono
- Skala H1 32px → H2 24px → H3 18px → body 16px → small 14px
- `max-width: 72ch` dla `.narrative`
- `widget-block` z lewym niebieskim paskiem i labelem "⚙ Widget interaktywny"
- Callouts z ikonami emoji (ℹ ⚠ ✕ ✓) przez `::before`

**Alternatywy do przetestowania:**
- [ ] Inter (neutralny, wszędzie jeden font)
- [ ] Atkinson Hyperlegible (dostępność)
- [ ] Lato (polski autor)

**Do przetestowania po wybraniu fontu:**
- [ ] Bootstrap Icons (`bsicons::bs_icon()`) zamiast emoji w callouts
- [ ] Widget-block z ikoną SVG zamiast tekstu "⚙"
- [ ] Dark mode via `bs_theme(preset="zephyr")` + zmienne CSS
- [ ] Landing page `landing/app.R` z `shinyAppDir()` w tabach

**Decyzje / notatki:**
- Uruchomione `sandbox/fonts-comparison` na porcie 3850 — wszystkie 4 fonty renderują polskie znaki poprawnie (ą, ć, ę, ł, ń, ó, ś, ź, ż).
- **Uwaga Rscript preview:** domyślny Rscript Claude preview (R 4.5.0 w Program Files) NIE ma zainstalowanych pakietów `AER`, `palmerpenguins`, `ISLR`, `fivethirtyeight`. Aby uruchomić `sandbox/testowy-wyklad` lokalnie, potrzeba:
  ```r
  install.packages(c("AER","palmerpenguins","ISLR","fivethirtyeight"))
  ```
  Albo użyć tego samego RStudio/R w którym działa produkcyjne `dobre-dane`.
- Screenshot fonts-comparison (1600x1400): czytelny, fonty wyraźnie różne. Source Serif daje najsilniejszy "podręcznikowy" feel, Inter najneutralniejszy, Atkinson najszerszy (więcej oddechu), Lato pomiędzy.

**Wrażenia porównawcze:**
- Inter: kompaktowy, czysty, UI-friendly. Trochę "SaaS-owy".
- Source Serif: ciepłe, poważne, pasuje do wykładu akademickiego.
- Atkinson Hyperlegible: świetna czytelność, szerszy — zabiera więcej miejsca. **WYBRANY.**
- Lato: bezpieczny i profesjonalny. Polski autor.

**DECYZJA 2026-04-18:** Atkinson Hyperlegible jako domyślny font projektu.
Powód: priorytet dostępności (studenci z dysleksją / niedowidzeniem), wyraźne
rozróżnienie znaków podobnych (I/l/1, O/0), humanistyczny charakter pasujący do
celu edukacyjnego.

**Następny krok:** przenieść Atkinson do produkcyjnego `R/shared_styles.css`
(sekcja fontu), usunąć/zakomentować pozostałe opcje w `experimental.css`.

---

### Pomysły do zrealizowania później

**Rozwijane callouty à la Tufte (collapsible sidenotes)**
- Nowy typ `callout-note` (dygresje/uzupełnienia), domyślnie zwinięty, ikona do kliknięcia.
- Desktop (>1200px): po kliknięciu rozwija się na prawym marginesie jako sidenote (absolute position).
- Mobile (<1200px): inline push-down akordeon, tap target ≥44px, animacja max-height 0.2s.
- NIE konwertować istniejących info/warning/danger/success — one mają natychmiastowe znaczenie sygnalizacyjne. Tylko nowy typ dla treści rozszerzającej.
- Ryzyko: jeśli >5 ikon blisko, tekst staje się "dziurawy" — używać oszczędnie.
- Prototyp: `sandbox/testowy-wyklad/` przed wdrożeniem do `shared_styles.css`.
