# Baza wiedzy: analiza ryzyka

Graf wiedzy budowany plikami Markdown. Jeden plik w `wezly/` = jeden węzeł;
relacje i metadane w nagłówku YAML, treść poniżej — patrz `SZABLON.md`.

- `ontologia.yaml` — typy węzłów, typy relacji z dziedzinami, statusy.
- `kompiluj.py` — skleja `wezly/*.md` w `graf.json` i waliduje
  (istnienie celów, dziedziny relacji, acykliczność `wymaga`/`uogolnia`).
  Wymaga `pyyaml`. Uruchomienie: `python3 kompiluj.py`.
- `graf.json` — wynik kompilacji; wejście dla przyszłej wizualizacji
  (konwencje zgodne z projektem historycznym: source/target, typy, EDTF).

Konwencja pracy: nowe pojęcie dopisuj od razu jako `zalazek` (30 sekund:
id, typ, nagłówek, jedno zdanie), pogłębiaj przy zmianie statusu.
Pole `status` czyni z grafu mapę własnego rozumienia, nie tylko mapę dziedziny.
