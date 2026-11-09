# Wykład 06 — kolejka zatwierdzania prototypów

Stan: 9 listopada 2026. Opis i uzasadnienie w
[PUNKTY-STOP-PROPOZYCJA.md](PUNKTY-STOP-PROPOZYCJA.md).

Oglądanie (z katalogu repo, osobny port):

```r
shiny::runApp("statystyka/06-zalozenia-testow", port = 8756)
```

## Inwentarz

| Rozdz. | Element | Status |
|---|---|---|
| 1 | Prototyp 6A „Serwis: czy naprawy trzymają normę?” | odrzucony (9.11), usunięty |
| 1 | Prototyp 6B „Wykres Q-Q to dwie kolejki obok siebie” | do oceny |
| 1 | Prototyp 6C „Ten sam kształt, inny werdykt” | do oceny |
| 2 | Prototyp 6D „Dwie zmiany w hali” (przebudowa sceny z 8.10) | do oceny |
| 4 | Prototyp 6E „Sto roślin czy cztery poletka?” | do oceny |

## Kolejka

### 1. 6D — hala (rozdz. 2)

Przebudowa istniejącej sceny. Do oceny: czy klamry s i s_p czytelnie
pokazują mechanizm i czy dwie lampki nie są za małe na rzutniku. Po przyjęciu
akapit „Pierwszy z tych układów można ustawić na Ryc. 2.1…” można skrócić.

### 2. 6A — serwis (rozdz. 1)

Odrzucony i usunięty (9.11): puenta „odporność testu t zależy od n” nie
jest dobra, a obraz nie mówił, kiedy zapala się lampka. Akapit o
odporności w rozdz. 1 zostaje bez obrazu.

### 3. 6E — poletka (rozdz. 4)

Jedyny obraz niezależności w wykładzie. Alternatywne miejsce: osobna sekcja
w rozdz. 1 albo 3.

### 4. 6B i 6C — Q-Q i Shapiro-Wilk (rozdz. 1)

Jeśli przyjęte: Ryc. 1.1 jako galeria wzorców, usunąć Ryc. 1.2 i 1.3
(6C je zastępuje).

### 5. Decyzje bez prototypu

6F–6I z propozycji: χ² i Fisher z akumulacją, uproszczenie Ryc. 2.1,
odchudzenie rozdz. 4, przypadek na prawdziwych danych.
