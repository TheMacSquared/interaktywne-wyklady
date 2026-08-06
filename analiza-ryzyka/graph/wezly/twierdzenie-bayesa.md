---
id: twierdzenie-bayesa
typ: pojecie
status: rozumiem
powstanie: "1763"
tagi: [wyklad-03]
relacje:
  - typ: wymaga
    cel: prawdopodobienstwo-warunkowe
  - typ: rozwiazuje
    cel: problem-falszywego-alarmu
---

# Twierdzenie Bayesa

**Definicja.** P(H|D) = P(D|H)·P(H) / P(D). Odwraca kierunek warunkowania:
z prawdopodobieństwa danych przy hipotezie na prawdopodobieństwo hipotezy przy danych.

**Intuicja.** Waga świadectwa zależy od tego, jak rzadka jest hipoteza.
Alarm „prawie zawsze działa", ale jeśli awarie są rzadkie, większość alarmów
i tak jest fałszywa — bo fałszywe alarmy losujemy z ogromnej puli stanów normalnych.

**Przykład.** Czujnik: czułość 99%, fałszywe alarmy 5%, awarie 1% czasu.
P(awaria|alarm) = 0,99·0,01 / (0,99·0,01 + 0,05·0,99) ≈ 0,17. Alarm to głównie szum.

**Pułapki / niezrozumienia.** Mylenie P(D|H) z P(H|D) — błąd prokuratora.
Zaniedbanie częstości bazowej to nie „błąd rachunkowy", tylko domyślny tryb intuicji.
