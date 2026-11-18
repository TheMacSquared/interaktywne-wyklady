#!/usr/bin/env bash
# Publikacja stron statycznych na GitHub Pages (gałąź gh-pages).
#
# Bierze zbudowane strony z dist/ (spis dist/index.html, spisy kursów i pliki
# wykładów dist/<kurs>/*.html), kopiuje je na gałąź gh-pages i ją wypycha.
# Bieżący katalog roboczy zostaje bez zmian: gałąź gh-pages obsługuje
# tymczasowy git worktree. Strony mają <meta name="robots" content="noindex">
# (scripts/build_static.R, scripts/build_index.R) — nie trafiają do wyszukiwarek,
# ale są dostępne dla każdego, kto zna adres.
#
# Użycie (z katalogu głównego repozytorium):
#   scripts/publish_pages.sh            # publikuje to, co jest w dist/
#   scripts/publish_pages.sh --build    # najpierw buduje wszystkie wykłady i spis
#   scripts/publish_pages.sh --dry-run  # przygotowuje commit, bez wypychania
#
# Jednorazowo w GitHubie: Settings → Pages → Build and deployment →
# Source: Deploy from a branch, Branch: gh-pages, folder / (root).
set -euo pipefail
cd "$(dirname "$0")/.."

BUILD=0; DRY=0
for a in "$@"; do
  case "$a" in
    --build) BUILD=1 ;;
    --dry-run) DRY=1 ;;
    *) echo "Nieznana opcja: $a" >&2; exit 2 ;;
  esac
done

COURSES="statystyka analiza-ryzyka"

if [ "$BUILD" = 1 ]; then
  lectures=()
  for c in $COURSES; do
    for d in "$c"/[0-9][0-9]-*/; do [ -f "$d/app.R" ] && lectures+=("${d%/}"); done
  done
  Rscript scripts/build_static.R "${lectures[@]}"
  Rscript scripts/build_index.R
fi

[ -f dist/index.html ] || { echo "Brak dist/index.html — uruchom z --build albo scripts/build_index.R." >&2; exit 1; }
for c in $COURSES; do
  ls dist/"$c"/[0-9][0-9]-*.html >/dev/null 2>&1 || { echo "Brak stron w dist/$c/." >&2; exit 1; }
  grep -q 'name="robots" content="noindex' dist/"$c"/index.html || { echo "dist/$c/index.html bez noindex — przebuduj spis." >&2; exit 1; }
done
for f in dist/*/[0-9][0-9]-*.html; do
  grep -q 'name="robots" content="noindex' "$f" || { echo "$f bez noindex — przebuduj wykład." >&2; exit 1; }
done
# Zakładki górnego paska: każda (poza bieżącym wykładem) prowadzi do istniejącego pliku.
LINK_ERR=0
for f in dist/*/[0-9][0-9]-*.html; do
  dir=$(dirname "$f")
  while read -r href; do
    if [ "$href" = "#" ]; then
      echo "$f: zakładka górnego paska bez linku (href=\"#\")." >&2; LINK_ERR=1
    elif [ ! -f "$dir/$href" ]; then
      echo "$f: zakładka prowadzi do brakującego pliku $href." >&2; LINK_ERR=1
    fi
  done < <(grep -oE '<a class="(lc-tab|lc-tabs-menu-item) ?" href="[^"]*"' "$f" | sed -E 's/.*href="([^"]*)"/\1/')
done
[ "$LINK_ERR" = 0 ] || { echo "Przebuduj wykłady (scripts/publish_pages.sh --build)." >&2; exit 1; }

SRC_COMMIT=$(git rev-parse --short HEAD)
SRC_BRANCH=$(git rev-parse --abbrev-ref HEAD)
DIRTY=""
git diff --quiet HEAD -- statystyka analiza-ryzyka shared scripts || DIRTY=" (z niezatwierdzonymi zmianami)"

WT=$(mktemp -d "${TMPDIR:-/tmp}/gh-pages.XXXXXX")
cleanup() { git worktree remove --force "$WT" >/dev/null 2>&1 || true; rm -rf "$WT"; }
trap cleanup EXIT

if git ls-remote --exit-code --heads origin gh-pages >/dev/null 2>&1; then
  git fetch -q origin gh-pages
  git worktree add -q -B gh-pages "$WT" origin/gh-pages
elif git show-ref --verify --quiet refs/heads/gh-pages; then
  # Gałąź jest tylko lokalnie (np. po --dry-run przed pierwszą publikacją).
  git worktree add -q "$WT" gh-pages
else
  # Pierwsza publikacja: osierocona gałąź bez historii kodu.
  git worktree add -q --detach "$WT"
  git -C "$WT" checkout -q --orphan gh-pages
fi

# Zawartość gałęzi to wyłącznie strony: stare pliki znikają, nowe wchodzą.
git -C "$WT" rm -rqf --ignore-unmatch . >/dev/null
find "$WT" -mindepth 1 -maxdepth 1 ! -name .git -exec rm -rf {} +
cp dist/index.html "$WT"/
for c in $COURSES; do
  mkdir -p "$WT/$c"
  cp dist/"$c"/index.html dist/"$c"/[0-9][0-9]-*.html "$WT/$c"/
done
touch "$WT/.nojekyll"   # bez przetwarzania przez Jekyll

git -C "$WT" add -A
if git -C "$WT" diff --cached --quiet; then
  echo "Strony bez zmian względem ostatniego commitu gałęzi gh-pages."
else
  git -C "$WT" commit -q -m "Publikacja stron z $SRC_BRANCH@$SRC_COMMIT$DIRTY"
fi
echo "Gałąź gh-pages: $(git -C "$WT" log -1 --format='%h %s')"
# W worktree .git to mały plik ze wskaźnikiem, więc du bez wykluczeń (GNU i BSD/macOS).
echo "Pliki: $(git -C "$WT" ls-files | wc -l | tr -d ' '), rozmiar: $(du -sh "$WT" | cut -f1)"

if [ "$DRY" = 1 ]; then
  echo "--dry-run: bez wypychania."
  exit 0
fi
git -C "$WT" push -q origin gh-pages
echo "Wypchnięto gałąź gh-pages. Strona: https://themacsquared.github.io/interaktywne-wyklady/"
