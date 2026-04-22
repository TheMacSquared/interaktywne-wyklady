# ============================================================================
# CHAPTER: Drzewo decyzyjne - mapa wyboru testu statystycznego
# ============================================================================
# Dwie równoległe implementacje tego samego diagramu:
#   - Wariant A: DiagrammeR (Graphviz) - statyczny, wierne odwzorowanie
#                screenshota z zajęć (dwa drzewka)
#   - Wariant B: visNetwork - interaktywny, jedno drzewo z wspólnym
#                korzeniem "Ile mamy zmiennych?"
#
# Dane grafu i konstruktor widgetu visNetwork są w modules/drzewo_data.R,
# żeby móc ich używać także ze skryptu eksportu (../export_drzewo.R).

source(file.path(app_dir, "modules", "drzewo_data.R"), local = TRUE)

# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------
ch_drzewo_ui <- list(
  id = "ch-drzewo", num = "09", title = "Drzewo decyzyjne",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 09 · Wnioskowanie statystyczne",
      num    = "09",
      title  = "Drzewo decyzyjne.",
      lead   = "„Mam dane — jaki test zastosować?” Mapa decyzji: od typu zmiennych
                i liczby grup do konkretnego testu. Jedno spojrzenie na cały wykład."
    ),

    # ----------------------- WARIANT A: Graphviz -----------------------
    h2(id = "ch-drzewo-graphviz", class = "section-title",
       "Wariant A — statyczny diagram (DiagrammeR / Graphviz)"),

    div(class = "narrative",
      p("Wierne odwzorowanie diagramu z zajęć. Dwa osobne drzewka: dla testów
         ", tags$b("jednej zmiennej"), " (na górze) oraz ", tags$b("dwóch zmiennych"),
        " (poniżej). Kolor zielony = dane ilościowe/ciągłe, niebieski = nominalne/porządkowe.")
    ),

    figure_panel(
      label = "Ryc. 9.1",
      title = "Drzewo decyzyjne — wariant statyczny",
      full_width = TRUE,
      DiagrammeR::grVizOutput("drzewo_graphviz", height = "1400px", width = "100%")
    ),

    # ----------------------- WARIANT B: visNetwork ---------------------
    h2(id = "ch-drzewo-visnet", class = "section-title",
       "Wariant B — diagram interaktywny (visNetwork)"),

    div(class = "narrative",
      p("Ten sam zestaw decyzji, ale jako ", tags$b("jedno drzewo"),
        " zaczynające się od pytania „ile mamy zmiennych?”. Diagram jest
         interaktywny — możesz przeciągać węzły, powiększać scrollem, a także
         ", tags$b("kliknąć"), " dowolny węzeł, żeby podświetlić jego ścieżkę decyzyjną.")
    ),

    figure_panel(
      label = "Ryc. 9.2",
      title = "Drzewo decyzyjne — wariant interaktywny",
      full_width = TRUE,
      visNetwork::visNetworkOutput("drzewo_visnet", height = "1000px", width = "100%")
    ),

    # ----------------------- Legenda / kiedy który ---------------------
    margin_callout(
      label = "Jak używać",
      tagList(
        tags$ul(
          tags$li("Zacznij od pytania ", tags$b("ile zmiennych?")),
          tags$li("Zapytaj ", tags$b("jaki typ danych?")),
          tags$li("Jeśli jedna nominalna + druga ciągła, ", tags$b("ile grup?")),
          tags$li("Box na końcu gałęzi: przykład, hipotezy, test.")
        )
      )
    ),

    lc_chapter_next(
      num       = "10",
      title     = "Ściąga",
      lead      = "kompaktowe podsumowanie wszystkich testów w tabelach.",
      target_id = "ch-sciaga"
    )
  )
)

# ----------------------------------------------------------------------------
# SERVER
# ----------------------------------------------------------------------------
ch_drzewo_server <- function(input, output, session) {

  output$drzewo_graphviz <- DiagrammeR::renderGrViz({
    DiagrammeR::grViz(drzewo_dot)
  })

  output$drzewo_visnet <- visNetwork::renderVisNetwork({
    build_drzewo_visnet()
  })
}
