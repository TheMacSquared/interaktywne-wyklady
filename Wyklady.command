#!/usr/bin/env bash
# Dwuklik w Finderze uruchamia hub wykładów i otwiera go w przeglądarce.
#
# To okno Terminala musi zostać otwarte przez całe zajęcia — trzyma huba
# i wszystkie uruchomione wykłady. Zamknięcie okna kończy wszystko naraz.
set -euo pipefail
cd "$(dirname "$0")"

PORT="${PORT:-7700}"

echo "Hub wykładów — http://127.0.0.1:${PORT}"
echo "Zatrzymanie: Ctrl+C albo zamknięcie tego okna."
echo

exec Rscript -e "shiny::runApp('hub', port = ${PORT}, host = '127.0.0.1', launch.browser = TRUE)"
