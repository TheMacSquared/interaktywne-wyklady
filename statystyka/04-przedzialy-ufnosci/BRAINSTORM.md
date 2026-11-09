# Warianty B3–B8 w tekście wykładu

Szkice do porównania obrazów i flow wykładu. Każdy stoi bezpośrednio po
odpowiadającej mu rycinie Claude’a; nie usuwają obecnych wersji ani nie
zmieniają statusu zaakceptowanych rycin. Kod bazowy Claude’a pochodzi
z `a8f6a03`, pobranego z remote. Nowe warianty są w `modules/brainstorm.*`.

## Oglądanie

Z katalogu repo:

```r
shiny::runApp("statystyka/04-przedzialy-ufnosci", port = 8744)
```

W istniejącym podglądzie trzeba odświeżyć sesję po restarcie aplikacji.
Warianty mają plakietki „Wariant B3”–„Wariant B8” i znajdują się w treści,
bez osobnego rozdziału czy aplikacji. Fonty i Chart.js wymagają połączenia
z internetem, jak pozostałe wykłady korzystające z tej biblioteki.

## Mapa porównania

B1 (Cztery miarki na jednym torze) i B2 (Ta sama ocena, inna stabilność)
odrzucone 2026-11-09; w rozdziale 1 zostają Ryc. 1.2 i 1.4.
B3 i B4 przyjęte 2026-11-09 zamiast prototypów 2b, 2c i 2d (usunięte);
B4 dostał przycisk n = 5. Do zrobienia: wygląd i port na ECharts.
B5 przyjęty 2026-11-09 zamiast prototypu 3a (usunięty; obliczenia
`s4p_3a_stats()` zostają w stop_proto_ch3.R).
B7 odrzucony 2026-11-09 razem z prototypem 4b; prototyp 4a przepisany na
ECharts (`modules/s4p_ch4.js`) i przeniesiony za Ryc. 4.1 jako Ryc. 4.2.

| Gdzie | Wariant | Co warto spróbować | Pomysł na przepływ opowieści |
|---|---|---|---|
| Rozdz. 2, po Suwaku ufności (2c) | B3: Stos i mapa 100 badań | Zmień 80% na 99% bez zmiany prób. Porównaj kolor mapy ze stosami. | Mapa daje obraz proporcji trafień, a stos wyjaśnia mechanizm. To kandydat do scalenia 2b i 2c. |
| Rozdz. 2, po porównaniu zakresu ludzi i CI (2d) | B4: Dwa znaczenia 95% | 25 → 100 → 400; oba obiekty mają wspólną skalę. | Modelowy obraz izoluje zależność od n. Obecne 2d pokazuje prawdziwie zmieniającą się próbę — można porównać obie pedagogiczne wersje. |
| Rozdz. 3, po Rozsuń linie (3a) | B5: Rozsuń grupy | Przeciągnij środek linii 1 albo wybierz „Pułapka”; uchwyt działa też z klawiatury. | Stała skala i trzy wyrównane wiersze pozwalają śledzić równocześnie CI grup i różnicy. Grupy wyrażone względem 9.08, różnica w naturalnej skali. |
| Rozdz. 3, po Istotne, ale nieważne (3c) | B6: Użyteczna oszczędność czasu | Przełącz trzy celowe warianty, potem zmieniaj efekt i n niezależnie. | Jasny próg 2 min zamiast umownego progu IQ; osobno odczyt kierunku i użyteczności. |
| Rozdz. 5, po Poziomie dobranym po fakcie (5c) | B8: Selekcja wyników | Dodaj 1000 badań i przełącz wszystkie/wybrane. | Alternatywny eksperyment: stały poziom 95%, zmienia się wyłącznie reguła pokazywania wyników. Może też otwierać temat selekcji w kolejnym wykładzie. |

## Co daje Chart.js

Kryterium z 5 listopada 2026 (`R/DESIGN_CONTRACT.md`): port do Chart.js
pozwala wersji offline (bez serwera R) liczyć wykres na żywo i zwiększa
responsywność, więc wierne odtworzenie
wykresu R też jest zyskiem. Kandydatami są wszystkie wykresy R, które
przetrwają decyzje o prototypach — również zaakceptowane Ryc. 1.1–1.4
(tarcza, cztery miarki, 4× więcej osób, pizzerie) oraz oryginalne panele
rozdziałów 3–5. Sceny SVG i Ryc. 2.1 już działają bez serwera.

Najwięcej dydaktycznie zyskują 3a (przeciąganie średniej), 2c (płynna zmiana
szerokości tych samych CI), 2d (natychmiastowe porównanie n) i pizzerie
(dokładanie punktów). Pierwsze trzy mają tu działające warianty; pizzerie
(B2 odrzucony) czekają na port Ryc. 1.4.

B6 dostaje z R
kwantyle t dla każdego dostępnego n. Nie implementujemy przybliżenia kwantyla
t w JavaScripcie. Każdy wariant korzysta ze wspólnego `lcCharts` i jego
sprzątania zasobów, motywów, powiększania i uchwytów klawiaturowych.

## Założenia szkiców

- B3: osiem serii po 100 prób, po nich cykl wraca do pierwszej serii;
  poziom zmienia kwantyl t, nie dane. Odsetek nie musi być dokładnie nominalny.
- B4: znane σ = 10 i umownie stały środek 170. Ilustrujemy szerokość
  z przedziału normalnego; nie jest to losowanie CI gwarantującego trafienie.
- B5: ustalone s i n z oryginału 3a, wersja Welcha. Przesuwamy środek,
  nigdy nie wybieramy granic CI niezależnie od danych i metody.
- B6: dwie niezależne grupy o takim samym n i s = 10; t z 2n − 2 df.
  Próg ±2 min to założenie użyteczności, nie własność rozkładu.
- B8: pula 4000 badań z N(26, 8), n = 40, dwustronne 95% CI t.
  Wybrane wyniki mają dolną granicę >26, więc ich pokrycie μ=26 wynosi 0%
  z definicji filtra. To pokrycie warunkowe w wybranym podzbiorze, a nie
  twierdzenie, że poprawna procedura 95% ma zerowe pokrycie bez selekcji.

Po wyczerpaniu puli B8 przyciski dodawania są wyłączone; „Od nowa” wraca
do początku. Zmiana rozdziału zaczyna wariant od początkowego stanu.

## Sprawdzenie

Sprawdzono wczytanie aplikacji i składnię JS, wszystkie osiem wariantów
w przeglądarce, odczyty przy zmianie kontrolek, zaznaczanie wad, filtr wyników,
powiększenie bez zmiany danych oraz ponowne wejście do rozdziału. Sprawdzono
uchwyt B5 z klawiatury i myszą, ujemny efekt w B6 oraz układ na wąskim ekranie z dużym krojem i ciemnym
motywem. To prototypy do rozmowy, nie deklaracja pełnej zgodności dostępności.

## Trzy wersje plastyczne sceny „Zmierz grupkę” — S1–S3

Aktualizacja: S2 (2.5D) wycofany z aktywnego UI. Pozostały S1 i S3;
szkic markerem wdrożono w sześciu oryginalnych scenach opisanych w
[PROTOTYPY-MARKER.md](../PROTOTYPY-MARKER.md). Opis trzech prób poniżej
dokumentuje wcześniejszy etap porównania.

W rozdziale 1, sekcja „Estymator w akcji”, bezpośrednio pod starszą sceną,
znajdują się trzy dodatkowe warianty do porównania:

- S1 — ilustracja redakcyjna: miękkie sylwetki, zróżnicowane ubrania,
  fryzury, twarze, plecaki i spokojne tło korytarza.
- S2 — makieta 2.5D: perspektywa ścian i podłogi, bryły przedmiotów,
  cienie i bardziej geometryczne postacie.
- S3 — szkic markerem: odręczny kontur, lekkie kreskowanie i oszczędny kolor.

To trzy zatwierdzone kierunki wizualne tej samej historii, a nie nowe
obowiązkowe przystanki. Użytkownik zlecił pozostawienie wariantów w tekście,
by oceniać również flow. Oryginał pozostaje dostępny powyżej.

Każda wersja mierzy grupkę 25 osób z modelu N(170, 10). Na ilustracji widać
fragment kolejki, a na osi komplet pomiarów. Po zakończeniu pomiarów średnia
spada do historii. +10/+100/+1000 działają seryjnie; „Pokaż μ” odsłania
wspólną wartość populacji. Wszystkie trzy wersje mają ten sam deterministyczny
strumień prób, więc po tych samych akcjach pokazują identyczne wyniki.
Reset przywraca początek strumienia i anuluje trwającą animację.

Implementacja: `modules/visual_scenes.R`, `visual_scenes.js`,
`visual_scenes.css`. Rysunki są SVG, bez bitmap i dodatkowych bibliotek.
Na telefonie sam rysunek można przewijać poziomo, zachowując rozmiar postaci;
kontrolki i odczyty mieszczą się w panelu. Obszar rysunku jest dostępny przez Tab.

Zweryfikowano trzy style w przeglądarce i obejrzano zrzuty ekranu. Sprawdzono
identyczność wyników po 1 i 1011 grupkach, reset podczas animacji, ograniczenie
ruchu przez `prefers-reduced-motion`, ciemny motyw, wąski ekran oraz ponowne
wejście do rozdziału. Składnia JS, wczytanie aplikacji i kontrola kontraktu
designu przechodzą bez błędów.
