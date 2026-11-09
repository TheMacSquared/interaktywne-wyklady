# Wykład 05 — kolejka zatwierdzania prototypów

Stan: 9 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port):

```r
shiny::runApp("statystyka/05-wnioskowanie-statystyczne", port = 8755)
```

## Inwentarz

| Rozdz. | Element | Status |
|---|---|---|
| 03 | Prototyp 5A „Czujka dymu w akademiku” | przyjęty (9.11), do poprawy grafiki; `type-error.jpg` zostaje |
| 03 | Prototyp 5B „Ile osób zaprosić do powtórki badania?” | odrzucony (9.11), usunięty |
| 06 | Prototyp 5C „Sen i ostatnia cyfra telefonu” | odrzucony (9.11), usunięty |
| 07 | Prototyp 5E „Mandaty: jak by było, gdyby płeć nie miała znaczenia?” | do oceny; obok Ryc. 7.1 |
| 07 | Prototyp 5E-2: 5E w wyglądzie ludzików z karteczkami (scena „stop” z 02) | do oceny; wariant wyglądu 5E |
| 09 | Prototyp 5D „Żelki i trądzik” | przyjęty (9.11) po przebudowie, z cytowaniem xkcd 882 |
| 09 | Sceny `jars`, `worlds`, `stress` (8.10) | czekają na ocenę (5F) |

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

Do decyzji: który wygląd zostaje. Po decyzji usunąć drugi wariant
i Ryc. 7.1, przenieść wzory E i χ² za scenę.

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
