# Handout: Usprawnienia wykładu 12-projekt-badawczy

## Kontekst

Wykład `statystyka/12-projekt-badawczy/` to interaktywna aplikacja Shiny prowadząca studentów przez mock-badanie empiryczne na danych `AER::TeachingRatings` (oceny ewaluacyjne nauczycieli akademickich, n=463). Celem jest nauka iteracyjnego myślenia badawczego — statystyka jest narzędziem, nie celem.

Istniejąca struktura (8 rozdziałów, pliki w `modules/`):
- `ch1_ciekawosc.R` — wybór obszaru zainteresowania
- `ch2_pytanie.R` — różne "ramki" dla pytania badawczego
- `ch3_hipotezy.R` — hipotezy + alternatywne wyjaśnienia
- `ch4_pomiar.R` — operacjonalizacja, construct map
- `ch5_sprawdzenia.R` — pierwsze testy statystyczne
- `ch6_iteracja.R` — co zrobić z wynikami
- `ch7_checklist.R` — checklist projektu grupowego
- `ch8_dodatek_model.R` — regresja wielowymiarowa (oznaczona jako "opcjonalna")

Kod jest dobry. Potrzebuje 4 konkretnych usprawnień opisanych poniżej.

---

## Wzorce UI których musisz przestrzegać

Projekt używa shared layout z `statystyka/R/`. **Nie wolno** używać `fluidPage`, `navbarPage`, `bslib::page_*`. Kanon:

```r
lc_feedback("tekst", type = "info|ok|warning|danger")
lc_stat_grid(lc_stat_box("label", value, color = proj_col_xxx))
figure_panel(label = "...", title = "...", zoom_plot_ui("id"))
margin_callout("tekst", color = "uwaga|wskazowka|ok")
lc_h2(id = "...", num = "N", title = "...")
lc_p("akapit narracyjny")
```

Kolory aplikacji są zdefiniowane w `modules/helpers.R` jako `proj_col_*`. Używaj ich, nie definiuj nowych.

Wzorce interaktywne do reuse z `statystyka/10-case-studies/modules/ch1_caschools.R`:
- **Checkbox model builder**: linie 636–643 (reactiveVal + observeEvent + dynamic formula)
- **Coefficient plot**: linie 675–698 (broom::tidy conf.int + geom_errorbarh + significance color)
- **Model comparison series**: linie 554–579 (4 pre-fit models, extract β, styled table)

---

## Zmiana 1: Confound checker w ch5_sprawdzenia.R

### Problem
Rozdział 5 przeskakuje od hipotez do testów statystycznych. Brakuje kroku gdzie student sprawdza *czy dana zmienna w ogóle jest zakłócaczem* — zanim uruchomi test.

### Co dodać
Na początku ch5, przed istniejącą sekcją z wyborem testu, dodaj nową sekcję `lc_h2`:

**"Zanim uruchomisz test: sprawdź zakłócacze"**

Zawartość:
1. Krótki tekst: zmienna Z jest zakłócaczem jeśli spełnia oba warunki jednocześnie: (a) koreluje z predyktorem X (`beauty`), (b) koreluje z wynikiem Y (`eval`).
2. `selectInput` — wybierz zmienną do sprawdzenia: `gender`, `age`, `tenure`, `minority`, `native`, `credits`, `division`
3. Dwa wykresy obok siebie (użyj CSS grid lub `lc_grid()`):
   - Lewy: `beauty ~ wybrana_zmienna` (violin lub boxplot)
   - Prawy: `eval ~ wybrana_zmienna` (violin lub boxplot)
4. Pod wykresami: `lc_feedback` reagujące dynamicznie na wybraną zmienną:
   - Jeśli zmienna spełnia oba warunki → `type="warning"`, tekst: "Kandydat na zakłócacz — warto kontrolować w modelu"
   - Jeśli tylko jeden warunek → `type="info"`, tekst: "Koreluje tylko z [X/Y] — nie zakłóca głównej relacji"

### Logika "spełnia warunki"
Oblicz w serverze korelacje (lub różnice median dla zmiennych kategorycznych) i zdefiniuj próg dla "koreluje":
- Ciągłe: `abs(cor(beauty, z, use="complete.obs")) > 0.10`
- Kategoryczne: różnica median między grupami > 0.05 punktu w skali

Prekalkuluj wyniki dla wszystkich zmiennych w `helpers.R` żeby serwer był szybki.

---

## Zmiana 2: Iteracyjne mosty na końcu ch5 i ch8

### Problem
Wykład jest linearny. Studenci przechodzą rozdziały jak kroki procedury, zamiast czuć że wyniki jednej analizy *generują* następne pytania.

### Co dodać w ch5_sprawdzenia.R (na końcu)

Po ostatniej sekcji z wynikami testu, dodaj nową sekcję `lc_h2` pt. **"Co ten wynik mówi nam dalej?"**:

```r
lc_feedback(
  tags$p(strong("Wynik sugeruje dwa nowe pytania:")),
  tags$ol(
    tags$li("Czy efekt beauty utrzymuje się gdy kontrolujemy inne czynniki (wiek, płeć, typ kursu)?"),
    tags$li("Czy response rate wpływa na wynik — czyli czy słyszmy tylko część studentów?")
  ),
  tags$p("Te pytania prowadzą do kolejnego kroku: modelu z kontrolami."),
  type = "info"
)
```

### Co dodać w ch8_dodatek_model.R (na końcu)

Po sekcji z własnym modelem studenta, dodaj sekcję **"Co model mówi nam dalej?"**:

```r
lc_feedback(
  tags$p(strong("Efekt beauty przeżył kontrolę — ale to rodzi kolejne pytanie:")),
  tags$p("Czy to przyczynowość? Czy atrakcyjność *powoduje* wyższe oceny, czy tylko z nimi współwystępuje?"),
  tags$p("Dane obserwacyjne nie mogą odpowiedzieć na to pytanie. Żeby odpowiedzieć, potrzebujemy innego projektu badania."),
  type = "warning"
)
```

---

## Zmiana 3: Wynieś ch8 z "bonusu" do głównego toku

### Problem
`ch8_dodatek_model.R` jest oznaczony jako opcjonalny. Regresja wielowymiarowa to jednak najważniejszy krok analityczny w całym wykładzie — bez niej nie widać jak β_beauty zmienia się pod kontrolą.

### Co zmienić

**W app.R**: Przesuń ch8 przed ch7. Nowa kolejność rozdziałów:
```
ch1, ch2, ch3, ch4, ch5, ch6, ch8_model, ch7_checklist
```
(Iteracja po wynikach → model → checklist to sensowniejszy flow niż model po checkliście)

Jeśli zmiana kolejności wymaga przelogowania numerów w UI (np. "Rozdział 8" staje się "Rozdział 7"), zrób to w argumentach `lecture_num` w `lc_chapter_hero()` każdego pliku.

**W ch8_dodatek_model.R**: Usuń wszelkie oznaczenia "opcjonalny", "bonus", "dodatek" z kicker/lead/tytułu. Zmień kicker na coś w stylu: `"Testujemy stabilność efektu"`.

**W ch7_checklist.R**: Zaktualizuj referencje — checklist powinien odwoływać się do modelu z poprzedniego rozdziału jako ostatniego kroku przed wnioskami.

---

## Zmiana 4: Nowy rozdział — projektowanie lepszego badania

### Problem
Wykład kończy się checklistem. Brakuje domknięcia które pyta: "skoro dane obserwacyjne mają ograniczenia — jak wyglądałoby badanie które naprawdę odpowie na pytanie?"

### Nowy plik: `modules/ch_projekt_badania.R`

Wstaw go między obecnym ch6_iteracja.R a ch8_model (w nowej kolejności: ch1–ch5, ch6_iter, **ch_projekt**, ch8_model, ch7_checklist).

**Kicker**: Od obserwacji do eksperymentu  
**Tytuł**: "Jak zaprojektować lepsze badanie?"

Zawartość:

**Sekcja 1** — krótki tekst (2 akapity):
- Dane obserwacyjne pokazują *współwystępowanie*, nie *przyczynowość*. Nie możemy losować urody nauczycielom.
- Żeby ustalić czy efekt jest przyczynowy, potrzebujemy innego projektu.

**Sekcja 2** — interaktywna: `radioButtons` z 4 propozycjami:
1. Eksperyment z manipulowanym opisem prowadzącego (fikcyjne profile, różna "uroda" na zdjęciu)
2. Ślepa ocena materiałów dydaktycznych (studenci oceniają anonimowe slajdy)
3. Pomiar efektów uczenia się (wyniki egzaminów zamiast ewaluacji)
4. Replikacja w różnych kulturach i uczelniach

Dla każdej propozycji: `lc_feedback` z oceną:
- Co by to pokazało?
- Jakie ma ograniczenia?
- Jak silny byłby dowód?

Użyj `type="ok"` dla mocnych propozycji, `type="info"` dla słabszych.

**Sekcja 3** — `margin_callout(color="wskazowka")`:
"W swoim projekcie grupowym też nie możesz losować — zastanów się co to oznacza dla siły twoich wniosków."

---

## Weryfikacja po zmianach

1. `shiny::runApp("statystyka/12-projekt-badawczy")` — przejść przez wszystkie rozdziały w nowej kolejności
2. Sprawdzić confound checker (zmiana 1): czy wykresy się renderują, czy feedback zmienia się przy wyborze zmiennej
3. Sprawdzić mosty iteracyjne (zmiana 2): czy są widoczne na końcu ch5 i ch8
4. Sprawdzić nowy rozdział (zmiana 4): czy radio buttons wyświetlają właściwy feedback per propozycja
5. Sprawdzić że `lc_chapter_next()` na końcu każdego rozdziału wskazuje właściwy następny rozdział (po zmianie kolejności)
