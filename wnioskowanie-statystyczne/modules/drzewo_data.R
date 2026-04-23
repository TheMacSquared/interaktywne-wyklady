# ============================================================================
# DRZEWO DECYZYJNE: dane i konstruktory widgetów
# ============================================================================
# Ten plik zawiera:
#   - drzewo_dot           : kod Graphviz/DOT (wariant A)
#   - drzewo_nodes/edges   : data.frames dla visNetwork (wariant B)
#   - build_drzewo_visnet(): konstruktor widgetu visNetwork (konfiguracja
#                            w jednym miejscu - używana i w Shiny, i w
#                            skrypcie eksportującym).
#
# Źródło danych dla obu wariantów to screenshot diagramu z zajęć
# (wierna kopia). Każda zmiana treści - tu, w jednym pliku.

# ----------------------------------------------------------------------------
# WARIANT A: Graphviz (DOT)
# ----------------------------------------------------------------------------
drzewo_dot <- '
digraph drzewo {
  graph [rankdir = TB, bgcolor = "transparent", splines = ortho,
         nodesep = 0.5, ranksep = 0.6, fontname = "Helvetica"]
  node  [fontname = "Helvetica", fontsize = 11, margin = "0.18,0.10"]
  edge  [fontname = "Helvetica", fontsize = 10, color = "#2c3e50"]

  /* ==================== DRZEWO 1: JEDNA ZMIENNA ==================== */

  subgraph cluster_jedna {
    label = "Testy jednej zmiennej"
    labeljust = "c"
    fontsize = 16
    fontname = "Helvetica-Bold"
    color = "#bdc3c7"
    style = "rounded"
    margin = 20

    root1 [label = "testy jednej zmiennej",
           shape = box, style = "rounded,filled", fillcolor = "#ffffff",
           penwidth = 2, fontsize = 13, fontname = "Helvetica-Bold"]

    dec1 [label = "Jaki typ\\ndanych?",
          shape = oval, style = filled, fillcolor = "#ffffff",
          penwidth = 1.5, fontsize = 12]

    ilo1 [label = "Ilościowe / ciągłe",
          shape = box, style = "rounded,filled", fillcolor = "#d5f5e3",
          penwidth = 1.2]

    nom1 [label = "Nominalne lub porządkowe",
          shape = box, style = "rounded,filled", fillcolor = "#d6eaf8",
          penwidth = 1.2]

    box_ilo1 [label = <
      <table border="0" cellborder="0" cellspacing="3">
        <tr><td align="left"><i>Przykład problemu:</i></td></tr>
        <tr><td align="left">Czy średnia zawartość soli<br/>w posiłkach X jest równa 20 g?</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Hipoteza (3 warianty)</b></td></tr>
        <tr><td align="left">H0: μ = liczba &#160;&#160;Ha: μ ≠ liczba</td></tr>
        <tr><td align="left">H0: μ ≥ liczba &#160;&#160;Ha: μ &lt; liczba</td></tr>
        <tr><td align="left">H0: μ ≤ liczba &#160;&#160;Ha: μ &gt; liczba</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Test statystyczny</b></td></tr>
        <tr><td align="left">Test t dla jednej próby</td></tr>
      </table>>,
      shape = box, style = "filled", fillcolor = "#eafaf1",
      penwidth = 2, color = "#27ae60"]

    box_nom1 [label = <
      <table border="0" cellborder="0" cellspacing="3">
        <tr><td align="left"><i>Przykład problemu:</i></td></tr>
        <tr><td align="left">Czy procent osób popierających nowego<br/>premiera przekracza 50% (0.5)?</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Hipoteza (3 warianty)</b></td></tr>
        <tr><td align="left">H0: p = liczba &#160;&#160;Ha: p ≠ liczba</td></tr>
        <tr><td align="left">H0: p ≥ liczba &#160;&#160;Ha: p &lt; liczba</td></tr>
        <tr><td align="left">H0: p ≤ liczba &#160;&#160;Ha: p &gt; liczba</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Test statystyczny</b></td></tr>
        <tr><td align="left">Test proporcji (dwumianowy)</td></tr>
      </table>>,
      shape = box, style = "filled", fillcolor = "#eaf3fa",
      penwidth = 2, color = "#2980b9"]

    root1 -> dec1
    dec1  -> ilo1
    dec1  -> nom1
    ilo1  -> box_ilo1
    nom1  -> box_nom1
  }

  /* ==================== DRZEWO 2: DWIE ZMIENNE ==================== */

  subgraph cluster_dwie {
    label = "Testy dwóch zmiennych"
    labeljust = "c"
    fontsize = 16
    fontname = "Helvetica-Bold"
    color = "#bdc3c7"
    style = "rounded"
    margin = 20

    root2 [label = "testy dwóch zmiennych",
           shape = box, style = "rounded,filled", fillcolor = "#ffffff",
           penwidth = 2, fontsize = 13, fontname = "Helvetica-Bold"]

    dec2 [label = "Jaki typ\\ndanych?",
          shape = oval, style = filled, fillcolor = "#ffffff",
          penwidth = 1.5, fontsize = 12]

    nn   [label = "Dwie nominalne",
          shape = box, style = "rounded,filled", fillcolor = "#d5f5e3",
          penwidth = 1.2]

    ni   [label = "Jedna nominalna,\\njedna ciągła",
          shape = box, style = "rounded,filled", fillcolor = "#d6eaf8",
          penwidth = 1.2]

    cc   [label = "Dwie ciągłe",
          shape = box, style = "rounded,filled", fillcolor = "#d5f5e3",
          penwidth = 1.2]

    box_nn [label = <
      <table border="0" cellborder="0" cellspacing="3">
        <tr><td align="left"><i>Przykład problemu:</i></td></tr>
        <tr><td align="left">Czy istnieje związek pomiędzy<br/>prywatnym ubezpieczeniem a płcią?</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Hipoteza (1 wariant)</b></td></tr>
        <tr><td align="left">H0: brak związku pomiędzy zmiennymi</td></tr>
        <tr><td align="left">Ha: istnieje istotny związek</td></tr>
        <tr><td align="left">&#160;&#160;&#160;&#160;pomiędzy zmiennymi</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Test statystyczny</b></td></tr>
        <tr><td align="left">Test chi-kwadrat niezależności</td></tr>
      </table>>,
      shape = box, style = "filled", fillcolor = "#eafaf1",
      penwidth = 2, color = "#27ae60"]

    box_ni2 [label = <
      <table border="0" cellborder="0" cellspacing="3">
        <tr><td align="left"><i>Przykład problemu:</i></td></tr>
        <tr><td align="left">Czy średnia zawartość soli w posiłkach firmy X<br/>jest równa średniej zawartości soli firmy Y?</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Hipoteza (3 warianty)</b></td></tr>
        <tr><td align="left">H0: μ₁ = μ₂ &#160;&#160;Ha: μ₁ ≠ μ₂</td></tr>
        <tr><td align="left">H0: μ₁ ≥ μ₂ &#160;&#160;Ha: μ₁ &lt; μ₂</td></tr>
        <tr><td align="left">H0: μ₁ ≤ μ₂ &#160;&#160;Ha: μ₁ &gt; μ₂</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Test statystyczny</b></td></tr>
        <tr><td align="left">Test t dla dwóch prób</td></tr>
      </table>>,
      shape = box, style = "filled", fillcolor = "#eafaf1",
      penwidth = 2, color = "#27ae60"]

    box_ni3 [label = <
      <table border="0" cellborder="0" cellspacing="3">
        <tr><td align="left"><i>Przykład problemu:</i></td></tr>
        <tr><td align="left">Czy średnia zawartość soli w posiłkach firmy X<br/>jest równa średniej zawartości soli firmy Y?</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Hipoteza (1 wariant)</b></td></tr>
        <tr><td align="left">H0: μ₁ = μ₂ = … = μₙ</td></tr>
        <tr><td align="left">Ha: przynajmniej jedna para różna</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Test statystyczny</b></td></tr>
        <tr><td align="left">Analiza ANOVA</td></tr>
      </table>>,
      shape = box, style = "filled", fillcolor = "#eafaf1",
      penwidth = 2, color = "#27ae60"]

    box_cc [label = <
      <table border="0" cellborder="0" cellspacing="3">
        <tr><td align="left"><i>Przykład problemu:</i></td></tr>
        <tr><td align="left">Czy wraz ze wzrostem średniej<br/>zawartości soli rośnie średnia<br/>kaloryczność posiłków?</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Hipoteza (1 wariant)</b></td></tr>
        <tr><td align="left">H0: r = 0</td></tr>
        <tr><td align="left">&#160;&#160;&#160;&#160;brak związku pomiędzy zmiennymi</td></tr>
        <tr><td align="left">Ha: r ≠ 0</td></tr>
        <tr><td align="left">&#160;&#160;&#160;&#160;istnieje istotny związek pomiędzy</td></tr>
        <tr><td align="left">&#160;&#160;&#160;&#160;zmiennymi</td></tr>
        <tr><td> </td></tr>
        <tr><td align="left"><b>Test statystyczny</b></td></tr>
        <tr><td align="left">Współczynnik korelacji Pearsona</td></tr>
      </table>>,
      shape = box, style = "filled", fillcolor = "#eafaf1",
      penwidth = 2, color = "#27ae60"]

    root2 -> dec2
    dec2  -> nn
    dec2  -> ni
    dec2  -> cc
    nn    -> box_nn
    ni    -> box_ni2 [label = "Jeśli 2 grupy"]
    ni    -> box_ni3 [label = "Jeśli ponad 2 grupy"]
    cc    -> box_cc
  }
}
'

# ----------------------------------------------------------------------------
# WARIANT B: visNetwork - dane
# ----------------------------------------------------------------------------
# Pomocnik: składa pełen label boxa terminalnego (przykład + hipotezy + test).
# visNetwork z font.multi="html" obsługuje TYLKO <b>, <i>, <code>, i TYLKO
# gdy cały tag mieści się w jednej linii. Nowa linia = \n. Znaki ≥ ≤ ≠ ≠
# wpisujemy wprost jako Unicode (<br>, &nbsp;, &lt; pokazałyby się dosłownie).
.box_label <- function(przyklad, hipotezy, test) {
  paste0(
    "<i>Przykład problemu:</i>\n", przyklad, "\n\n",
    "<b>Hipoteza</b>\n", hipotezy, "\n\n",
    "<b>Test statystyczny</b>\n", test
  )
}

drzewo_nodes <- data.frame(
  id = c(
    # wspólny korzeń
    "root", "dec_ile",
    # gałąź 1: jedna zmienna
    "jedna", "d1", "i1", "n1", "b_i1", "b_n1",
    # gałąź 2: dwie zmienne
    "dwie", "d2", "nn", "ni", "cc", "b_nn", "b_ni2", "b_ni3", "b_cc"
  ),
  label = c(
    # root
    "<b>Jaki test wybrać?</b>",
    "<b>Ile mamy</b>\n<b>zmiennych?</b>",

    # gałąź 1
    "<b>Testy jednej</b>\n<b>zmiennej</b>",
    "<b>Jaki typ</b>\n<b>danych?</b>",
    "Ilościowe / ciągłe",
    "Nominalne lub\nporządkowe",
    .box_label(
      "Czy średnia zawartość soli\nw posiłkach X jest równa 20 g?",
      paste(
        "H0: μ = liczba    Ha: μ ≠ liczba",
        "H0: μ ≥ liczba    Ha: μ < liczba",
        "H0: μ ≤ liczba    Ha: μ > liczba",
        sep = "\n"
      ),
      "Test t dla jednej próby"
    ),
    .box_label(
      "Czy procent osób popierających nowego\npremiera przekracza 50% (0.5)?",
      paste(
        "H0: p = liczba    Ha: p ≠ liczba",
        "H0: p ≥ liczba    Ha: p < liczba",
        "H0: p ≤ liczba    Ha: p > liczba",
        sep = "\n"
      ),
      "Test proporcji (dwumianowy)"
    ),

    # gałąź 2
    "<b>Testy dwóch</b>\n<b>zmiennych</b>",
    "<b>Jaki typ</b>\n<b>danych?</b>",
    "Dwie nominalne",
    "Jedna nominalna,\njedna ciągła",
    "Dwie ciągłe",
    .box_label(
      "Czy istnieje związek pomiędzy\nprywatnym ubezpieczeniem a płcią?",
      paste(
        "H0: brak związku pomiędzy zmiennymi",
        "Ha: istnieje istotny związek",
        "      pomiędzy zmiennymi",
        sep = "\n"
      ),
      "Test chi-kwadrat niezależności"
    ),
    .box_label(
      "Czy średnia zawartość soli w posiłkach firmy X\njest równa średniej zawartości soli firmy Y?",
      paste(
        "H0: μ₁ = μ₂    Ha: μ₁ ≠ μ₂",
        "H0: μ₁ ≥ μ₂    Ha: μ₁ < μ₂",
        "H0: μ₁ ≤ μ₂    Ha: μ₁ > μ₂",
        sep = "\n"
      ),
      "Test t dla dwóch prób"
    ),
    .box_label(
      "Czy średnia zawartość soli w posiłkach firmy X\njest równa średniej zawartości soli firmy Y\noraz firmy Z (i kolejnych)?",
      paste(
        "H0: μ₁ = μ₂ = … = μₙ",
        "Ha: przynajmniej jedna para różna",
        sep = "\n"
      ),
      "Analiza ANOVA"
    ),
    .box_label(
      "Czy wraz ze wzrostem średniej zawartości\nsoli rośnie średnia kaloryczność posiłków?",
      paste(
        "H0: r = 0 (brak związku)",
        "Ha: r ≠ 0 (istotny związek)",
        sep = "\n"
      ),
      "Współczynnik korelacji Pearsona"
    )
  ),
  group = c(
    "root", "decision",
    "branch", "decision", "cat_ilo", "cat_nom", "test_ilo", "test_nom",
    "branch", "decision", "cat_ilo", "cat_nom", "cat_ilo",
    "test_ilo", "test_ilo", "test_ilo", "test_ilo"
  ),
  level = c(
    1, 2,
    3, 4, 5, 5, 6, 6,
    3, 4, 5, 5, 5, 6, 6, 6, 6
  ),
  stringsAsFactors = FALSE
)

drzewo_edges <- data.frame(
  from = c(
    "root", "dec_ile", "dec_ile",
    "jedna", "d1", "d1", "i1", "n1",
    "dwie", "d2", "d2", "d2", "nn", "ni", "ni", "cc"
  ),
  to = c(
    "dec_ile", "jedna", "dwie",
    "d1", "i1", "n1", "b_i1", "b_n1",
    "d2", "nn", "ni", "cc", "b_nn", "b_ni2", "b_ni3", "b_cc"
  ),
  label = c(
    "", "jedna", "dwie",
    "", "", "", "", "",
    "", "", "", "", "", "2 grupy", "3+ grup", ""
  ),
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------------------------
# KONSTRUKTOR WIDGETU visNetwork
# ----------------------------------------------------------------------------
# Używany i w Shiny (ch_drzewo.R), i w skrypcie eksportu (export_drzewo.R),
# dzięki czemu konfiguracja stylów / layoutu żyje w jednym miejscu.
build_drzewo_visnet <- function() {
  visNetwork::visNetwork(drzewo_nodes, drzewo_edges) |>
    visNetwork::visNodes(
      shape = "box",
      margin = 12,
      widthConstraint = list(minimum = 180, maximum = 320),
      font = list(multi = "html", size = 15,
                  face = "Helvetica", align = "left")
    ) |>
    visNetwork::visHierarchicalLayout(
      direction = "UD",
      sortMethod = "directed",
      levelSeparation = 160,
      nodeSpacing = 360,
      treeSpacing = 260,
      blockShifting = TRUE,
      edgeMinimization = TRUE,
      parentCentralization = TRUE
    ) |>
    visNetwork::visGroups(groupname = "root",
                          color = list(background = "#ffffff",
                                       border = "#2c3e50"),
                          font = list(multi = "html", size = 20,
                                      face = "Helvetica", align = "center")) |>
    visNetwork::visGroups(groupname = "decision",
                          shape = "ellipse",
                          color = list(background = "#ffffff",
                                       border = "#7f8c8d"),
                          font = list(multi = "html", size = 16,
                                      face = "Helvetica", align = "center")) |>
    visNetwork::visGroups(groupname = "branch",
                          color = list(background = "#ecf0f1",
                                       border = "#2c3e50"),
                          font = list(multi = "html", size = 17,
                                      face = "Helvetica", align = "center")) |>
    visNetwork::visGroups(groupname = "cat_ilo",
                          color = list(background = "#d5f5e3",
                                       border = "#27ae60"),
                          font = list(multi = "html", size = 15,
                                      face = "Helvetica", align = "center")) |>
    visNetwork::visGroups(groupname = "cat_nom",
                          color = list(background = "#d6eaf8",
                                       border = "#2980b9"),
                          font = list(multi = "html", size = 15,
                                      face = "Helvetica", align = "center")) |>
    visNetwork::visGroups(groupname = "test_ilo",
                          color = list(background = "#eafaf1",
                                       border = "#27ae60"),
                          font = list(multi = "html", size = 14,
                                      face = "Helvetica", align = "left")) |>
    visNetwork::visGroups(groupname = "test_nom",
                          color = list(background = "#eaf3fa",
                                       border = "#2980b9"),
                          font = list(multi = "html", size = 14,
                                      face = "Helvetica", align = "left")) |>
    visNetwork::visEdges(arrows = "to",
                         color = list(color = "#2c3e50"),
                         smooth = list(enabled = TRUE, type = "cubicBezier",
                                       roundness = 0.4),
                         font = list(size = 13, align = "middle",
                                     background = "#ffffff")) |>
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE,
                                                   degree = 2,
                                                   hover = TRUE),
                           nodesIdSelection = FALSE) |>
    visNetwork::visPhysics(enabled = FALSE) |>
    visNetwork::visInteraction(navigationButtons = TRUE,
                               zoomView = TRUE,
                               dragView = TRUE,
                               tooltipDelay = 200)
}
