# Wykład 06 — propozycja punktów stop

Stan: 9 listopada 2026. Podstawa: `PUNKTY-STOP.md`, `SCENY.md`,
`statystyka/R/DESIGN_CONTRACT.md`, treść rozdziałów 01–04. Prototypy priorytetu A
są w tekście wykładu ze złotą plakietką „Prototyp 6X · rodzaj”; decyzje
zbiera [KOLEJKA-ZATWIERDZANIA.md](KOLEJKA-ZATWIERDZANIA.md).

## Diagnoza

- Wszystkie wykresy to ggplot. Jedyny obraz JS to scena hali (prototyp
  z 8 października). Brak przypadku na prawdziwych danych.
- Dużo liczb z symulacji stoi tylko w tekście: 29% / 0.1% fałszywych alarmów
  testu Studenta, moc Levene'a, χ² i Fisher przy rzadkich kategoriach.
  Ryc. 1.3 to zamrożony wynik symulacji.
- Bez obrazu: odporność testu t na skośność, budowa wykresu Q-Q (opis
  słowny), niezależność obserwacji i pseudoreplikacja.
- Ryc. 1.2, 2.2 i 2.3 to kalkulatory „przycisk → liczba” dla danych
  z innego panelu.
- Rozdział 04: cztery tabele i Sprawdzarka 4.1 powtarzają te same treści.
- Powtórzone puenty: „test formalny zależy od n” (5 miejsc), „Welch
  domyślnie” (5), „Mann-Whitney nie leczy nierównych wariancji” (4).
- Scena hali stała po liczbach, które miała pokazać, i nie pokazywała
  mechanizmu. Student i Welch nie dawali się porównać na tych samych danych.

## Prototypy w tekście (priorytet A)

Wszystkie sceny są w `modules/scenes.js` (rodzaje `welch`, `repair`, `plots`,
`qq`, `sw`) i działają bez serwera R. Pula losowań 6C i kwantyle 6B pochodzą
z R (`p6_sw_pool()`, `p6_qq_config()` w `modules/helpers.R`).

| ID | Gdzie | Rodzaj / moment | Puenta | Co robi obraz |
|---|---|---|---|---|
| 6A | rozdz. 1, po akapicie o odporności | symulacja, wprowadzenie | Skośność pojedynczych czasów szkodzi testowi t przy małej próbie, a z rosnącym n przestaje. | Kierownik serwisu porównuje średni czas napraw z normą 30 min (prawdziwa średnia = 30). Czasy: 10 min + wykładniczy o średniej 20. Liczniki osobno dla n = 5 / 15 / 50: ok. 12% / 9% / 6% alarmów. |
| 6B | rozdz. 1, po opisie budowy Q-Q | pojęcie (kadry) | Wykres Q-Q to kolejka danych postawiona obok kolejki idealnej. | 20 osób → według wzrostu → kontury idealnej kolejki (x̄ + s·z) → punkty Q-Q; najniższa i najwyższa osoba wyróżnione na każdym kadrze. |
| 6C | rozdz. 1, po Ryc. 1.3 | przypadek brzegowy (dwa światy) | Ten sam lekki odchył, a werdykt Shapiro-Wilka zależy od n. | Masa jabłek z jednego sadu (120 + 6·Gamma(10)): przy n = 20 mediana p ≈ 0.34, przy n = 2000 p < 0.001 zawsze. Wspólne skale obu wykresów. |
| 6D | rozdz. 2, przed akapitem z 29% / 0.1% | scena (przebudowa hali) | Przy nierównych n wspólny rozrzut Studenta kłamie, Welch nie. | Te same pomiary idą do obu testów: dwie lampki, dwa liczniki. Klamry pod pasami: s każdej zmiany i przerywana klamra s_p. Usunięte: przełącznik testu zerujący licznik i histogram p (druga puenta). |
| 6E | rozdz. 4, po akapicie o niezależności | scena, wprowadzenie | n to liczba niezależnych jednostek, nie pomiarów. | 2 nawozy × 4 poletka × 25 roślin, bez efektu nawozu (τ = 3, σ = 5 cm). Analiza „100 roślin” alarmuje w ok. 49% doświadczeń, „4 poletka” w ok. 5%. |

Liczby sprawdzone w R (20 000 powtórzeń; 6E i 6C po 5000 i 2000).

## Do decyzji bez prototypu (priorytet B)

| ID | Co | Rekomendacja |
|---|---|---|
| 6F | Ryc. 3.1 χ² vs Fisher | Zbierać powtórzenia (+100 / +1000, dwa liczniki obok siebie) zamiast paczki 500 bez akumulacji; port na scenę albo ECharts. |
| 6G | Ryc. 2.1 (cztery suwaki) + kalkulatory 2.2, 2.3 | Jeden suwak „ile razy większy rozrzut grupy B”; wyniki Levene'a i obu testów t jako odczyty pod 2.1. |
| 6H | Rozdz. 4 | Zostawić Sprawdzarkę 4.1 jako materiał, usunąć tabele, które ją powielają; ewentualnie mapa „pytanie → test → co sprawdzić” jako domknięcie. |
| 6I | Przypadek na prawdziwych danych | Jedno śledztwo przez wszystkie założenia, na `caschools.csv` albo `dane/satelitarne_obserwacje.csv`. |
| — | Ryc. 1.2 i 1.3 | Jeśli 6C zostanie przyjęty, Ryc. 1.3 i kalkulator 1.2 można usunąć. |
| — | Ryc. 1.1 | Po 6B zamienić w galerię wzorców (stałe, celowe warianty) bez suwaka n. |

Świadomie pominięte: wpływ wartości odstającej na r (wykład 02, Ryc. 5.3),
animacja CTG (wykład 03, rozdz. 6).
