---
id: funkcja-hazardu
typ: pojecie
status: w-trakcie
tagi: [wyklad-07]
relacje:
  - typ: wymaga
    cel: czas-do-zdarzenia
---

# Funkcja hazardu

**Definicja.** h(t) = f(t)/R(t) — chwilowa intensywność awarii pod warunkiem
dożycia do t. Nie jest prawdopodobieństwem (może przekraczać 1).

**Intuicja.** Odpowiada na pytanie „skoro dotrwało do teraz, jak bardzo grozi mu
awaria *w tej chwili*". Kształt h(t) to diagnoza charakteru zużycia: stała —
awarie czysto losowe (brak pamięci), rosnąca — starzenie, malejąca — śmiertelność
niemowlęca (wady ujawniają się wcześnie). Krzywa wannowa = złożenie wszystkich trzech.

**Przykład.** Elektronika po okresie docierania: h(t) ≈ const → model wykładniczy.
Element mechaniczny ze zmęczeniem materiału: h(t) rosnąca → Weibull z k > 1.

**Pułapki / niezrozumienia.** Mylę h(t) z f(t) — gęstość opisuje populację startową,
hazard tylko tych, którzy przeżyli. Do wyjaśnienia: dlaczego R(t) = exp(−∫h).
