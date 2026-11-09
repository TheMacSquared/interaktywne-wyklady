# Wykład 05 — propozycja punktów stop

Stan: 9 listopada 2026. Podstawa: `PUNKTY-STOP.md`, `SCENY.md` (z lekcjami
z odrzuconych prototypów: „sto pracowni”, „porównaj wszystkie pary”, B8),
`statystyka/R/DESIGN_CONTRACT.md`, treść 12 rozdziałów. Prototypy priorytetu A
są w tekście ze złotą plakietką „Prototyp 5X · rodzaj”. Decyzje zbiera
[KOLEJKA-ZATWIERDZANIA.md](KOLEJKA-ZATWIERDZANIA.md).

## Diagnoza

- Pojęcia przychodzą jako tekst lub wzór przed obrazem:
  - α, β i moc: analogia, tabela i statyczny `type-error.jpg`;
  - p-wartość: wzór przed symulacją z Ryc. 3.2;
  - χ²: wzory przed tabelaryczną Ryc. 7.1;
  - F: wzór przed scenami słoików;
  - d: wzór przed Ryc. 10.1;
  - zmienność r pod H₀ i zysk z parowania: tylko liczby w tekście.
- Rozdziały 04–08 mają ten sam szablon: galeria hipotez → wzory → krok po
  kroku → wersja jednostronna. Powtórzone puenty:
  - „brak odrzucenia ≠ brak efektu” (ok. 8 razy);
  - „jednostronny” (4 panele);
  - „dokładny vs przybliżony” (Ryc. 5.3 i 7.3);
  - galerie 10.2–10.5 są prawie identyczne.
- Mocne obrazy: 1.1 (telefon na biurku / w plecaku), 3.2 (symulacja pod H₀),
  8.3 (wykruszanie, przypadek).
- Ryc. 3.1 ma trzy suwaki i test jednej próby (μ₀ = 70), choć historia dotyczy
  dwóch grup.
- Trzy prototypy scen z 8.10 w ANOVA (`jars`, `worlds`, `stress`) czekają
  na ocenę i stoją po wzorze F.
- Porządki:
  - podwójna „Ryc. 10.1” (drzewo i siła efektu);
  - rozdział 02 ma tylko Ryc. 2.4;
  - sprzeczne odsyłacze do wykładów 01 i 02 (rozdz. 07, 08, 09).

## Prototypy w tekście (priorytet A)

Kod: `modules/proto05.R`, `modules/proto05.js`, `modules/proto05.css`. To
osobny silnik z kontenerem `.p5-sc`, żeby nie ruszać scen ANOVA w `scenes.js`.
Wszystko działa bez serwera R. Suwaki to zwykłe `input type=range`.

| ID | Gdzie | Rodzaj / moment | Puenta | Co robi obraz |
|---|---|---|---|---|
| 5A | rozdz. 03, po akapicie o czujniku, przed tabelą błędów | scena (jeden obraz z „co, jeśli”) | Każdy próg alarmu wymienia fałszywe alarmy na przeoczenia. | Czujka dymu: odczyty w zwykłe dni N(20, 6) i przy pożarze N(45, 8), suwak progu. Odczyt: % fałszywych alarmów (i ile dni w roku) oraz % przeoczonych pożarów. Kandydat na zastąpienie `type-error.jpg`. |
| 5B | rozdz. 03, przed Ryc. 3.1 | piaskownica | — | Odrzucony 9.11 jako dubel Ryc. 3.1 i usunięty. Otwarty pomysł: uśrednianie odczytów w 5A (kolejka, poz. 1). |
| 5C | rozdz. 06, przed „Od współczynnika do testu” | scena | — | Odrzucony 9.11 i usunięty: sztuczna historia, a stos r z powtórzonych grupek to rozkład próbkowy (CTG) w kostiumie. Otwarty pomysł: przypadek z jedną próbą (kolejka, poz. 3). |
| 5D | rozdz. 09, na początku „Dlaczego nie kilka testów t?” | scena (kadry) | Pocięcie jednej zmiennej na wiele podgrup daje przypadkowe „sukcesy”; gazeta pokazuje tylko ten jeden. | Żelki i trądzik (motyw xkcd 882, własna grafika), dane stałe: 400 jedzących dzielonych na 20 kolorów, średnie podgrup wobec pasa zakresu przypadku; jedna wypada poza pas → gazeta → cała seria. Ryc. 9.1 zostaje jako pojęcie (wzór). |
| 5E | rozdz. 07, po hipotezach, przed wzorami E i χ² | scena (kadry) | Liczebności oczekiwane to „jak by było, gdyby nic nie zależało”. | 200 kontroli (dane jak w Ryc. 7.1): kierowcy jako kropki → świat bez związku (po 40) → obrysy braków i nadwyżek → wkłady (O − E)²/E, suma χ² = 8.33. Kandydat na zastąpienie tabelarycznej Ryc. 7.1. |

5D ma stałe dane (`p5_jelly_data()` w `proto05.R`, ziarno 833): poza pasem
jest dokładnie jedna podgrupa, więc prowadzący wie, co pokaże każdy kadr.

## Do decyzji bez prototypu (priorytet B)

| ID | Co | Rekomendacja |
|---|---|---|
| 5F | Sceny ANOVA `jars`, `worlds`, `stress` (rozdz. 09, 8.10) | `jars` przed wzór F (wprowadzenie), `worlds` jako galeria po F, `stress` przed Games-Howellem; nadać numery po decyzji. |
| 5G | Ryc. 3.2 a wzór p-wartości | Przestawić: symulacja 3.2 przed wzorem. Ryc. 3.3 zostaje jako pojęcie. |
| 5H | Zysk z parowania (rozdz. 08) | Dwa światy na Ryc. 8.2: te same dane bez linii i z liniami par, odczyt p obu testów. |
| 5I | Siła efektu (rozdz. 10) | Ryc. 10.1: jeden suwak n przy stałym d, bez selektora scenariuszy. Ryc. 10.2–10.5 scalić w jedną galerię jako domknięcie. |
| 5J | Szablon rozdz. 04–08 | Jednostronne panele 4.2, 5.2, 6.2 zastąpić odsyłaczem do Ryc. 2.4. Z Ryc. 5.3 i 7.3 zostawić jedną. |
| 5K | Porządki | Numer ryciny drzewa, odsyłacze do wykładów 01 i 02, numeracja ćwiczeń. |
