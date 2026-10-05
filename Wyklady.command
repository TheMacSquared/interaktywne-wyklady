#!/usr/bin/env bash
# Dwuklik w Finderze uruchamia hub wykładów i otwiera go w przeglądarce.
#
# To okno Terminala musi zostać otwarte przez całe zajęcia — trzyma huba
# i wszystkie uruchomione wykłady. Zamknięcie okna kończy wszystko naraz.
set -euo pipefail
cd "$(dirname "$0")"

# hub/start.R sprawdza pakiety i nie startuje drugiego huba, gdy jeden już działa.
exec Rscript hub/start.R
