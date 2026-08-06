---
id: identyfikator-ascii-bez-spacji
typ: pojecie            # jeden z typy_wezlow w ontologia.yaml
status: zalazek         # zalazek | w-trakcie | rozumiem | wymaga-powrotu
powstanie: "1763"       # OPCJONALNE, format EDTF ("1951~" = około; usuń pole, gdy bez sensu)
tagi: [wyklad-01]       # OPCJONALNE, dowolne etykiety
relacje:                # OPCJONALNE; cel musi istnieć jako id innego pliku
  - typ: wymaga
    cel: inny-wezel
  - typ: uogolnia
    cel: jeszcze-inny-wezel
    nota: "krótki komentarz, gdy relacja nieoczywista"
---

# Pełna nazwa węzła

**Definicja.** Jedno–dwa zdania możliwie ścisłe.

**Intuicja.** Własnymi słowami, obrazowo — to pole jest dla Ciebie, nie dla rygoru.

**Przykład.** Najlepiej z kontekstu ryzyka/niezawodności.

**Pułapki / niezrozumienia.** Co mnie myliło, czego nie łapię (aktualizować przy statusie wymaga-powrotu).

---
Zasada minimalnego tarcia: nowy węzeł to skopiowany szablon, wypełnione TYLKO
frontmatter + nagłówek + jedna linijka definicji. Reszta może czekać — od tego
jest status "zalazek". Sekcje poniżej nagłówka są konwencją, nie wymogiem;
kompilator czyta wyłącznie frontmatter i pierwszy nagłówek H1.
