---
id: fta
typ: metoda
status: zalazek
powstanie: "1962"
tagi: [wyklad-09]
relacje:
  - typ: wymaga
    cel: zdarzenie-losowe
  - typ: wymaga
    cel: niezawodnosc-systemu
    nota: "bramki AND/OR to inna notacja układów równoległych/szeregowych"
---

# Analiza drzewa błędów (FTA)

**Definicja.** Dedukcyjny rozkład zdarzenia szczytowego na kombinacje przyczyn przez bramki logiczne; rachunek na zbiorach minimalnych przekrojów.

**Intuicja.** Odwrócenie perspektywy niezawodnościowej: zamiast „czy system działa" — „którędy dokładnie może zawieść". Pułapka: wspólne przyczyny łamią założenie niezależności.
