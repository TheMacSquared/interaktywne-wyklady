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
ch_drzewo_ui <- tabPanel("9. Drzewo decyzyjne",
  fluidRow(column(10, offset = 1,

    div(class = "chapter-recap",
      "Mapa wyboru testu statystycznego - jedno spojrzenie na cały wykład.
       Zaczynasz u góry pytaniem \"ile zmiennych?\", a każde kolejne pytanie
       o typ danych prowadzi do konkretnego testu, przykładowego problemu
       i zestawu hipotez."
    ),

    # ----------------------- WARIANT A: Graphviz -----------------------
    div(class = "section-title", "Wariant A — statyczny diagram (DiagrammeR / Graphviz)"),

    div(class = "narrative",
      p("Wierne odwzorowanie diagramu z zajęć. Dwa osobne drzewka: dla testów
         ", tags$b("jednej zmiennej"), " (na górze) oraz ", tags$b("dwóch zmiennych"),
        " (poniżej). Kolor zielony = dane ilościowe/ciągłe, niebieski = nominalne/porządkowe.")
    ),

    div(class = "widget-block",
      DiagrammeR::grVizOutput("drzewo_graphviz", height = "1400px", width = "100%")
    ),

    # ----------------------- WARIANT B: visNetwork ---------------------
    div(class = "section-title", "Wariant B — diagram interaktywny (visNetwork)"),

    div(class = "narrative",
      p("Ten sam zestaw decyzji, ale jako ", tags$b("jedno drzewo"),
        " zaczynające się od pytania \"ile mamy zmiennych?\". Diagram jest
         interaktywny — możesz przeciągać węzły, powiększać scrollem, używać
         guzików nawigacyjnych w lewym dolnym rogu oraz ", tags$b("kliknąć"),
        " dowolny węzeł, żeby podświetlić jego ścieżkę decyzyjną. Przydatne,
         gdy drzewo rozrośnie się o kolejne gałęzie (np. testy założeń, testy
         nieparametryczne).")
    ),

    div(class = "widget-block",
      visNetwork::visNetworkOutput("drzewo_visnet", height = "1000px", width = "100%")
    ),

    # ----------------------- Legenda / kiedy który ---------------------
    div(class = "callout-info",
      tags$strong("Jak używać:"),
      tags$ul(
        tags$li("Zacznij od pytania ", tags$b("ile zmiennych?"),
                " — jedna (np. średnia soli wobec wartości referencyjnej)
                 czy dwie (np. sól vs. kaloryczność)?"),
        tags$li("Następnie zapytaj ", tags$b("jaki typ danych?"),
                " — ilościowe/ciągłe czy nominalne/porządkowe?"),
        tags$li("Gdy jedna zmienna jest nominalna, a druga ciągła, rozstrzygnij ", tags$b("ile grup"),
                " — 2 grupy to test t, 3+ grup to ANOVA."),
        tags$li("Box na końcu gałęzi podaje: przykład problemu, warianty hipotez
                 i nazwę testu statystycznego.")
      )
    ),

    div(class = "text-center", style = "margin-top: 30px;",
      actionButton("ch_drzewo_next", "Dalej → Ściąga",
                   class = "btn-primary", style = "padding: 10px 24px;")
    )

  ))
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
