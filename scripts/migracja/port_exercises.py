#!/usr/bin/env python3
"""Przenosi rozdział „Ćwiczenia” z wariantami kierunków z serwera do dokumentu.

Wzorzec wejściowy (statystyka 03, 04, …):
  <ui> <- list(… selectInput("<prefix>_kierunek", …) … uiOutput("<prefix>_content") …)
  .<prefix>_content_<k> <- function() tagList(… lc_more("Rozwiązanie", uiOutput("<prefix>_solN")) …)
  .<prefix>_solutions <- list(<k> = list(sol1 = …, …))
  <prefix>_server: renderUI(switch(k, <k> = .<prefix>_content_<k>(), …)) + renderUI dla rozwiązań

Wynik: definicja <ui> na końcu pliku, treść wszystkich wariantów w dokumencie
(lc_variants() + lc_static_fill()), pusty serwer.

Użycie: port_exercises.py <plik.R> <prefix>      np. … ch7_cwiczenia.R ch7
"""
import pathlib
import re
import sys
sys.path.insert(0, str(pathlib.Path(__file__).parent))
from move_scene_texts import match_paren


def main(path, prefix):
    p = pathlib.Path(path)
    t = p.read_text(encoding="utf-8")
    ui = prefix + "_ui"
    server = prefix + "_server"

    # kierunki i funkcje treści z switch() w serwerze
    s = t.index("%s <- function(input, output, session) {" % server)
    pairs = re.findall(r"^\s*(\w+)\s*=\s*(\.%s_content_\w+)\(\)" % re.escape(prefix), t[s:], re.M)
    assert pairs, "nie znaleziono switch() z wariantami"
    directions = [k for k, _ in pairs]

    # definicja UI wędruje na koniec
    a = t.index("%s <- list(" % ui)
    close_i = match_paren(t, t.index("(", a))
    ui_block = t[a:close_i + 1]
    t = t[:a] + t[close_i + 1:].lstrip("\n")
    old = 'uiOutput("%s_content")' % prefix
    assert ui_block.count(old) == 1, "brak uiOutput treści"
    switch = ",\n".join("        %s = %s()" % (k, fn) for k, fn in pairs)
    ui_block = ui_block.replace(old, '''# Wszystkie warianty są w dokumencie; widoczny jest wariant wybranego kierunku.
    lc_variants("%(p)s_kierunek", selected = "%(first)s",
      stats::setNames(lapply(.%(p)s_directions, .%(p)s_static_content), .%(p)s_directions))''' %
                                {"p": prefix, "first": directions[0]})
    m = re.search(r'selectInput\("%s_kierunek",' % re.escape(prefix), ui_block)
    assert m, "brak listy kierunków"
    sel_close = match_paren(ui_block, ui_block.index("(", m.start()))
    ui_block = ui_block[:sel_close].rstrip() + ",\n          selectize = FALSE   # zwykła lista: działa bez skryptów Shiny\n        " + ui_block[sel_close:]

    s = t.index("%s <- function(input, output, session) {" % server)
    head = t[:s].rstrip("\n")
    # ramka nagłówka „SERVER” tuż przed funkcją, jeśli jest
    frame = re.search(r"\n# =+\n# SERVER[^\n]*\n# =+\s*$", head)
    if frame:
        head = head[:frame.start()].rstrip("\n")
    t = head + '''

# ============================================================================
# WARIANTY W DOKUMENCIE (bez serwera)
# ============================================================================

.%(p)s_directions <- c(%(dirs)s)

# Treść wariantu z rozwiązaniami wstawionymi w zwijane bloki (dawniej renderował
# je serwer). Nagłówki sekcji dostają przyrostek kierunku, bo w dokumencie są
# wszystkie warianty naraz.
.%(p)s_static_content <- function(k) {
  content <- switch(k,
%(switch)s
  )
  solutions <- .%(p)s_solutions[[k]]
  lc_static_fill(content, heading_suffix = k,
    fills = stats::setNames(solutions, paste0("%(p)s_", names(solutions))))
}

# ============================================================================
# UI
# ============================================================================

%(ui)s

# ============================================================================
# SERVER
# ============================================================================

%(server)s <- function(input, output, session) {
  # Rozdział nie ma logiki serwera: warianty i rozwiązania są w dokumencie
  # (lc_variants(), lc_static_fill()).
}
''' % {"p": prefix, "dirs": ", ".join('"%s"' % k for k in directions), "switch": switch,
       "ui": ui_block, "server": server}
    p.write_text(t, encoding="utf-8")
    print("przeniesiono warianty:", ", ".join(directions))


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
