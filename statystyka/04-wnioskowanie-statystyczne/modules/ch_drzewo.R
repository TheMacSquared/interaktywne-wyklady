# ============================================================================
# CHAPTER: Drzewo decyzyjne - mapa wyboru testu statystycznego
# ============================================================================
# Interaktywne drzewo przez visNetwork. Dane grafu i konstruktor widgetu
# w modules/drzewo_data.R — używane też przez ../export_drzewo.R.

source(file.path(app_dir, "modules", "drzewo_data.R"), local = TRUE)

# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------
ch_drzewo_ui <- list(
  id = "ch-drzewo", num = "10", title = "Drzewo decyzyjne",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 10 · Testowanie hipotez",
      num    = "10",
      title  = "Drzewo decyzyjne.",
      lead   = "„Mam pytanie i dane — jaki test zastosować?” Najpierw sprawdź plan
                badania, potem przejdź od typu zmiennych i liczby grup do konkretnego testu."
    ),

    tagList(
      lc_feedback(type = "warning",
        tags$strong("Zanim wejdziesz do drzewa:"),
        tags$ul(
          tags$li("Nazwij pytanie: opis, porównanie czy związek?"),
          tags$li("Ustal jednostkę obserwacji i sprawdź, czy pomiary są niezależne."),
          tags$li("Jeśli te same obiekty mierzono kilka razy, wybierz analizę sparowaną."),
          tags$li("Wniosek o przyczynowości wymaga odpowiedniego planu badania — sam test go nie zapewnia.")
        )
      ),
      p("Jedno drzewo zaczynające się od pytania „ile mamy zmiennych?”.
        Diagram jest interaktywny — możesz przeciągać węzły, powiększać scrollem,
        a także ", tags$b("kliknąć"),
        " dowolny węzeł, żeby podświetlić jego ścieżkę decyzyjną."),
      p("Gdy drzewo robi się za ciasne w kolumnie treści, kliknij przycisk ",
        tags$b("Pełny ekran"), " — rozszerzy widget na całe okno przeglądarki.
        Wyjście: przycisk ", tags$em("Zamknij"), " lub klawisz Esc.")
    ),

    figure_panel(
      label = "Ryc. 10.1",
      title = "Drzewo decyzyjne — wariant interaktywny",
      full_width = TRUE,
      div(id = "drzewo-fullscreen-wrap", class = "drzewo-wrap",
        div(class = "drzewo-toolbar",
          actionButton("drzewo_fullscreen", HTML("&#x26F6; Pełny ekran"),
            class = "lc-btn-secondary-outline lc-btn-sm",
            onclick = paste0(
              "var el = document.getElementById('drzewo-fullscreen-wrap');",
              "if (document.fullscreenElement) { document.exitFullscreen(); }",
              "else if (el.requestFullscreen) { el.requestFullscreen(); }",
              "else if (el.webkitRequestFullscreen) { el.webkitRequestFullscreen(); }"
            )
          )
        ),
        visNetwork::visNetworkOutput("drzewo_visnet", height = "1000px", width = "100%")
      )
    ),

    inline_callout(
      label = "Jak używać",
      tagList(
        tags$ul(
          tags$li("Przed drzewem sprawdź ", tags$b("pytanie i plan badania")),
          tags$li("Następnie zapytaj ", tags$b("ile zmiennych?")),
          tags$li("Zapytaj ", tags$b("jaki typ danych?")),
          tags$li("Jeśli jedna nominalna + druga ciągła, ", tags$b("ile grup?")),
          tags$li("Box na końcu gałęzi: przykład, hipotezy, test.")
        )
      )
    ),

    lc_chapter_next(
      num       = "11",
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

  output$drzewo_visnet <- visNetwork::renderVisNetwork({
    build_drzewo_visnet()
  })
}
