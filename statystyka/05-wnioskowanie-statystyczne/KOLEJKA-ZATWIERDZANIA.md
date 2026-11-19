# Wykład 05 — kolejka zatwierdzania prototypów

Stan: 19 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port):

```r
system("Rscript scripts/build_static.R statystyka/05-wnioskowanie-statystyczne")   # → dist/statystyka/05-wnioskowanie-statystyczne.html
```

## Inwentarz

| Rozdz. | Element | Status |
|---|---|---|
| 03 | Prototyp 5A „Czujka dymu w akademiku” | przyjęty (9.11); wdrożony 19.11 jako Ryc. 3.1 po poprawie grafiki (czujka nad progiem, toster, płomień, strefa alarmu); `type-error.jpg` zostaje |
| 03 | Prototyp 5B „Ile osób zaprosić do powtórki badania?” | odrzucony (9.11), usunięty |
| 06 | Prototyp 5C „Sen i ostatnia cyfra telefonu” | odrzucony (9.11), usunięty |
| 07 | Prototyp 5E „Mandaty: jak by było, gdyby płeć nie miała znaczenia?” | przyjęty (9.11) |
| 07 | Prototyp 5E-2: 5E w wyglądzie ludzików z karteczkami (scena „stop” z 02) | odrzucony (9.11), usunięty |
| 09 | Prototyp 5D „Żelki i trądzik” | przyjęty (9.11) po przebudowie, z cytowaniem xkcd 882 |
| 09 | Sceny `jars`, `worlds`, `stress` (8.10) | czekają na ocenę (5F) |
| 01 | Prototyp 5S „Eksperyment: telefon a koncentracja” (kadry) | wdrożony 19.11 jako Ryc. 1.1 (dawna Ryc. 1.1 to teraz 1.2) |
| 01 | Prototyp 5T „Sąd i test statystyczny” (kadry) | wdrożony 19.11 jako Ryc. 1.3; zastąpił tabelę analogii |
| 02 | Prototyp 5P „O co naprawdę pyta pytanie?” (trzy pytania z tekstu: wzrost, korepetycje, lody) | wdrożony 19.11 jako Ryc. 2.1 |
| 02 | Prototyp 5L „Rozbiór pytania” (kadry: pytanie → parametr → relacja → hipotezy) | wdrożony 19.11 jako Ryc. 2.2 |
| 02 | Prototyp 5M „Dwie hipotezy dzielą oś parametru” (trzy pary znaków) | wdrożony 19.11 jako Ryc. 2.4 |
| 02 | Prototyp 5N „Jedno słowo zmienia hipotezy” (zmienił / skrócił / wydłużył) | złączony z 5R (19.11), usunięty |
| 02 | Prototyp 5O „Popraw hipotezę” (pięć błędnych zapisów) | wdrożony 19.11 jako Ryc. 2.6 |
| 02 | Prototyp 5Q „Przepis na parę hipotez” (panel statyczny: trzy kroki, pary znaków, dwie reguły) | wdrożony 19.11 jako Ryc. 2.3 |
| 02 | Prototyp 5R „Jedno słowo w pytaniu, inny test” (5N + 5R: słowo → hipotezy słownie → sygnał na osi wyniku; bez α) | wdrożony 19.11 jako Ryc. 2.5 |

## Kolejka

### 1. 5A i 5B — błędy i moc (rozdz. 03)

Decyzja (9.11): 5A przyjęty, mechanika i puenta bez zmian. Do zrobienia:
dopracować grafiki. Czujka pod sufitem jest mała i mało czytelna,
a ikony tostera i płomienia są szkicowe. `type-error.jpg` zostaje obok 5A,
bo jest humorystyczny (decyzja 9.11).

5B odrzucony i usunięty (9.11): dubel Ryc. 3.1, tak samo abstrakcyjny,
z innego punktu widzenia, ale bez nowej puenty. Ryc. 3.1 zostaje.

Otwarty pomysł zamiast 5B: rozbudować 5A o przełącznik „ile odczytów
czujka uśrednia przed decyzją: 1 / 4 / 16”, widoczny dopiero w drugim kroku.
Krzywe się zwężają, przy tym samym progu maleją oba błędy, a czułość czujki
to moc. Wtedy Ryc. 3.1 tylko nazywa to, co sala zobaczyła na czujce. Do
sprawdzenia: czy uśrednianie odczytów brzmi realnie i czy druga puenta nie
rozmywa 5A.

### 2. 5E i 5E-2 — mandaty (rozdz. 07)

5E podobał się (9.11). 5E-2 to ten sam scenariusz, te same dane (200
kontroli, 30 i 50 mandatów, χ² = 8.33) i te same kadry i teksty, ale
wygląd ze sceny „Kto dostaje mandat?” z wykładu 02 (Ryc. 2.1), którą
studenci już znają: ludziki w kolorach płci z karteczką mandatu albo
pouczenia; oczekiwany mandat to karteczka z przerywanym brzegiem.

Pierwsza wersja 5E-2 (dane 02: 70 kierowców, χ² = 1.30) odrzucona 9.11:
miała zostać tylko oprawa, nie dane.

Decyzja (9.11): zostaje 5E, bo jest czytelniejszy; 5E-2 miał za dużo
szczegółów i został usunięty. Do zrobienia przy wdrożeniu: usunąć Ryc. 7.1
(trzy tabele) i przenieść wzory E i χ² za scenę.

### 3. 5C — korelacja (rozdz. 06)

Odrzucony i usunięty (9.11). Historia była sztuczna, a stos r z kolejnych
grupek to rozkład próbkowy w nowym kostiumie: sugeruje, że trzeba zbierać
wiele prób po 40 osób. Rozkład pod H₀ ma już Ryc. 3.2 (p-wartość) i krok po
kroku Ryc. 6.1. Zdanie o ±0.31 w tekście zostaje.

Otwarty pomysł: przypadek z jedną próbą. Kierownik Bananpolu ma 10 dni
pomiarów temperatury w hali i liczby braków, r = 0.5, i chce kupić
klimatyzację; test daje p = 0.14. To samo r z 40 dni daje p = 0.001.
Forma przykładów, bez powtarzania prób.

### 4. 5D — żelki (rozdz. 09)

Przebudowany 9.11 dwa razy. Najpierw lampki z licznikiem serii zastąpiły
kadry ze stałymi danymi. Potem, na prośbę o pokazanie mechanizmu, scena
pokazuje cięcie jednej zmiennej na podgrupy:

1. Pytanie: 100 niejedzących i 400 jedzących (kropka = osoba, liczba
   wyprysków), średnia jedzących w pasie zakresu przypadku, p = 0.61.
2. Podział na kolory: te same 400 kropek rozjeżdża się na 20 wierszy po
   20 osób; pas dla grupy 20 osób jest szerszy; średnia zielonych wypada
   poza niego w górę (p = 0.031).
3. Gazeta: tylko zielone żelki.
4. Cała seria: 19 średnich w pasie, 1 poza; 20 × 5% = średnio 1 alarm.

Decyzja (9.11): przyjęty w tej wersji. Pod panelem podpis ze źródłem
(Randall Munroe, „Significant”, xkcd 882, CC BY-NC 2.5).

Dane: `p5_jelly_data()` w `proto05.R`, ziarno 833 (jedyny alarm: zielone, średnio więcej wyprysków).
Motyw z xkcd 882 („Significant”), grafika własna. Ryc. 9.1 po scenie
daje wzór 64%.

### 5. Decyzje bez prototypu

Pozycje 5F–5K z propozycji: sceny ANOVA, kolejność 3.2 i wzoru p-wartości,
parowanie, siła efektu, szablon rozdz. 04–08, porządki numeracji.

### 6. 5L–5P — rozdział 02 (19.11)

Rozdział 02 miał jedną rycinę (2.4) na siedem sekcji tekstu. Pięć paneli bez
liczenia i bez własnego JS (`modules/proto05_ch2.R`, `proto05_ch2.css`), każdy
stoi obok tekstu, który mógłby zastąpić; tekstu na razie nie ruszano.

- 5P: sekcja „Zasada”, po trzech potocznych pytaniach.
- 5L: sekcja „Rozbiór przykładu”, przed akapitami „Krok 1–3”. Kadr z symbolami
  usunięty 19.11 (za wcześnie); zapis formalny zostaje w następnej sekcji.
- 5M: po szkielecie pary hipotez, przed akapitem „Znaki łączą się w pary”.
- 5N: po galerii jednostronnej, przed akapitem o słowach zdradzających kierunek.
  Ta sama oś co w 5M; do decyzji, czy zostają oba.
- 5O: sekcja „Typowe błędy”, przed listą pięciu pomyłek.
- 5Q: przed szkieletem „parametr = / ≤ / ≥ wartość odniesienia”; wyróżnia równość
  w H₀ i rozłączność hipotez, które dotąd były tylko w tekście.
- 5R: sekcja „Test jednostronny a dwustronny”, po tabeli typów. Powód (19.11):
  Ryc. 2.4 z α, rozkładem statystyki i wartościami krytycznymi stoi za wcześnie,
  bo błędy wprowadza dopiero rozdział 03. Decyzja 19.11 (wariant B): Ryc. 2.4
  z akapitami o mechanizmie przeniesiona do rozdz. 03 jako Ryc. 3.5, w nowej sekcji
  „Jeden ogon czy dwa?” po p-wartości; dawne Ryc. 3.5 i 3.6 to teraz 3.6 i 3.7.
  Rozdział 02 nie używa już α.

Decyzja 19.11: 5M, 5N i 5R były zbyt podobne. Zostają dwa: 5M (oś parametru, pary
znaków, przy zapisie formalnym obok 5Q) i 5R (słowo w pytaniu → hipotezy słownie →
oś wyniku z próby, w sekcji o teście jednostronnym). 5N usunięty; przykład w 5R
to na razie nawóz i plon.

Rozdział 02 zamknięty 19.11: przyjęte 5P, 5L, 5M, 5O, 5Q, 5R oraz przeniesienie
dawnej Ryc. 2.4 do rozdz. 03. Do zrobienia przy wdrożeniu: numery rycin zamiast
plakietek „Prototyp” i usunięcie tekstu, który panele zastępują (akapity „Krok 1–3”,
szkielet pary hipotez, lista błędów).

### 7. 5S–5T — rozdział 01 (19.11)

Kod: `modules/proto05_ch1.R`, `proto05_ch1.css`, bez JS. Oba przyjęte 19.11;
5T zastąpił tabelę analogii (usunięta z tekstu).

- 5S „Eksperyment: telefon a koncentracja” (kadry): 80 studentów → losowanie →
  dwie grupy → test. Stoi po liście opisującej eksperyment, przed Ryc. 1.1.
- 5T „Sąd i test statystyczny” (kadry): założenie → dowody → mocne dowody →
  słabe dowody, z przekreślonym „niewinny” i „telefon nie wpływa”. Stoi przed
  tabelą analogii.

### 8. Wdrożenie rozdziałów 01 i 02 (19.11)

Prototypy 5S, 5T, 5P, 5L, 5Q, 5M, 5R, 5O są stałymi rycinami z numerami
(tabela na górze). Kod: `modules/ch1_panels.R`, `modules/ch2_panels.R`,
`modules/panels_ch12.css` (dawne `proto05_ch1.*`, `proto05_ch2.*`). W `proto05.*`
zostają prototypy 5A, 5D, 5E (rozdz. 03, 07, 09).

Tekst obok paneli (zasada z 19.11: powtórzenie panelu znika, rozwinięcie zostaje).
Usunięte: lista opisująca eksperyment (Ryc. 1.1), „Krok 3” z ramką hipotez słownych
(Ryc. 2.2), szkielet pary hipotez i akapit o parach znaków (Ryc. 2.3 i 2.4).
Zostają jako rozwinięcie: akapity „Krok 1” i „Krok 2”, lista pięciu błędów
(nazwy błędów, HARKing, uzasadnienie zamiany ról), tabela typów testu.

### 9. Numeracja rozdziału 03 po wdrożeniu 5A (19.11)

Czujka dymu to Ryc. 3.1, pozostałe ryciny przesunięte o jeden: moc i błędy 3.2,
symulacja pod H₀ 3.3, pole ogonów 3.4, pytanie o p-wartość 3.5, jeden ogon czy
dwa (dawna 2.4) 3.6, p-wartość a α 3.7, quiz 3.8. Wcześniejsze wpisy w tym pliku
i w PUNKTY-STOP-PROPOZYCJA.md używają numerów sprzed zmiany.

### 10. 5U–5Z — rozdział 03 (19.11)

Kod: `modules/proto05_ch3.R`, `proto05_ch3.css`; rycina `s05_power2` w `figures.js`,
opcja `k` w scenie `smoke` (`proto05.js`). Rozstrzygnięte 19.11, patrz pod tabelą.

| ID | Co | Gdzie | Zastępuje / uzupełnia |
|---|---|---|---|
| 5Z | Czujka, która uśrednia odczyty (1 / 4 / 16) | pod Ryc. 3.1 | wariant Ryc. 3.1 |
| 5U | Cztery sytuacje: czujka / sąd / test | pod 5Z, przed tabelą błędów | tabela błędów |
| 5X | Moc testu i błędy dla dwóch grup | pod Ryc. 3.2 | Ryc. 3.2 i akapit o √2 |
| 5Y | p-wartość i α na jednym wykresie (trzy widoki) | pod Ryc. 3.4 | Ryc. 3.4 i 3.7 |
| 5V | Odwrócony warunek: koszykarze i wysocy | przed akapitem o odwróceniu warunku | uzupełnia akapit |
| 5W | Od p-wartości do zdania w raporcie (kadry, dwa wyniki) | sekcja „Decyzja w praktyce” | ramki werdyktów |

Bez prototypu, zmiana w tekście (dawna pozycja 5G): opis symulacji i Ryc. 3.3 stoją
teraz przed definicją i wzorem p-wartości. Do cofnięcia, jeśli kolejność nie pasuje.

Decyzje 19.11 i wdrożenie:

- 5Z odrzucony (za dużo niuansu naraz), usunięty razem z opcją `k` w scenie czujki.
- 5U i 5V odrzucone (niepotrzebne), usunięte; tabela błędów i akapit o koszykarzach zostają.
- 5X przyjęty i zastąpił Ryc. 3.2: moc liczona dla dwóch grup po n osób (różnica
  średnich „plecak − biurko”, H₀: różnica = 0). Tekst sekcji przeliczony (moc 67.3%
  zamiast 92.6%), akapit o √2 zastąpiony jednym zdaniem o odchyleniach 12 i 14 pkt.
- 5Y przyjęty jako nowa Ryc. 3.7 (trzy widoki); Ryc. 3.4 bez zmian.
- 5W przyjęty jako Ryc. 3.8 (`modules/ch3_panels.R`); quiz to teraz Ryc. 3.9.
- Kolejność „symulacja przed wzorem p-wartości” została w tekście.

W rozdziale 03 nie ma już prototypów. `panels_ch12.css` to teraz `panels.css`.
