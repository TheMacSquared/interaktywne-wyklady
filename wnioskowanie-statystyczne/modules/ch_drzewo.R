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
  id = "ch-drzewo", num = "09", title = "Drzewo decyzyjne",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 09 · Testowanie hipotez",
      num    = "09",
      title  = "Drzewo decyzyjne.",
      lead   = "„Mam dane — jaki test zastosować?” Mapa decyzji: od typu zmiennych
                i liczby grup do konkretnego testu. Jedno spojrzenie na cały wykład."
    ),

    div(class = "narrative",
      p("Jedno drzewo zaczynające się od pytania „ile mamy zmiennych?”.
        Diagram jest interaktywny — możesz przeciągać węzły, powiększać scrollem,
        a także ", tags$b("kliknąć"),
        " dowolny węzeł, żeby podświetlić jego ścieżkę decyzyjną."),
      p("Gdy drzewo robi się za ciasne w kolumnie treści, kliknij przycisk ",
        tags$b("Pełny ekran"), " — rozszerzy widget na całe okno przeglądarki.
        Wyjście: przycisk ", tags$em("Zamknij"), " lub klawisz Esc.")
    ),

    figure_panel(
      label = "Ryc. 9.1",
      title = "Drzewo decyzyjne — wariant interaktywny",
      full_width = TRUE,
      div(id = "drzewo-fullscreen-wrap", class = "drzewo-wrap",
        div(class = "drzewo-toolbar",
          tags$button(type = "button", class = "btn btn-outline-secondary btn-sm",
            onclick = paste0(
              "var el = document.getElementById('drzewo-fullscreen-wrap');",
              "if (document.fullscreenElement) { document.exitFullscreen(); }",
              "else if (el.requestFullscreen) { el.requestFullscreen(); }",
              "else if (el.webkitRequestFullscreen) { el.webkitRequestFullscreen(); }"
            ),
            HTML("&#x26F6; Pełny ekran")
          )
        ),
        visNetwork::visNetworkOutput("drzewo_visnet", height = "1000px", width = "100%")
      )
    ),

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

  output$drzewo_visnet <- visNetwork::renderVisNetwork({
    build_drzewo_visnet()
  })
}
