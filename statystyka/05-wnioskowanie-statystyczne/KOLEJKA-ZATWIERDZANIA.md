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
| 06 | Prototyp 5C „Sen i ostatnia cyfra telefonu” | do oceny |
| 07 | Prototyp 5E „Mandaty: jak by było, gdyby płeć nie miała znaczenia?” | do oceny; obok Ryc. 7.1 |
| 09 | Prototyp 5D „Żelki i trądzik” | do oceny; przed Ryc. 9.1 |
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

### 2. 5E — mandaty (rozdz. 07)

Kadry stoją przed wzorami, a stara Ryc. 7.1 (trzy tabele) po nich. Po
przyjęciu: usunąć Ryc. 7.1 i przenieść wzory E i χ² za scenę.

### 3. 5C — korelacja (rozdz. 06)

Zastępuje zdanie z liczbą ±0.31 w tekście. Po przyjęciu: skrócić akapit
„Od współczynnika do testu”.

### 4. 5D — żelki (rozdz. 09)

Puenta jest bliska Ryc. 9.1, ale 9.1 liczy wzór, a 5D pokazuje historię.
Do decyzji: czy scena nie jest za „gazetowa” i czy nagłówek ma zostać.

### 5. Decyzje bez prototypu

Pozycje 5F–5K z propozycji: sceny ANOVA, kolejność 3.2 i wzoru p-wartości,
parowanie, siła efektu, szablon rozdz. 04–08, porządki numeracji.
