# Sterowanie wykresem i czytelność na rzutniku
# Osobny podgląd minimalistycznego widgetu z wykładu 03.

library(shiny)
library(ggplot2)
library(dplyr)

.find_app_dir <- function() {
  candidates <- c(lapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) dirname(normalizePath(frame$ofile))
  }), list(getwd()))
  candidates <- unique(unlist(candidates))
  valid <- candidates[file.exists(file.path(candidates, "prototype.css"))]
  if (!length(valid)) stop("Uruchom aplikację przez shiny::runApp().")
  valid[[1]]
}
app_dir <- .find_app_dir()
project_root <- normalizePath(file.path(app_dir, "..", ".."))

source(file.path(project_root, "R", "palette.R"), local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"), local = TRUE)
source(file.path(project_root, "R", "shared.R"), local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
lc_apply_ggplot_defaults()

area_readouts <- function() {
  lc_readouts(
    lc_readout("Od", tags$span(`data-role` = "a", "165.0")),
    lc_readout("Do", tags$span(`data-role` = "b", "175.0")),
    lc_readout("Prawdopodobieństwo", tags$span(`data-role` = "P", "0.468"),
               color = "var(--upwr-accent)")
  )
}

.chapters <- list(list(
  id = "a11y-prototype", num = "01", title = "Minimalistyczne sterowanie",
  content = tagList(
    lc_chapter_hero(kicker = "Podgląd wdrożonej wersji", num = "01",
      title = "Granice przedziału i prawdopodobieństwo.",
      lead = "Przeciągaj granice lub przesuwaj je klawiaturą. Mała lupa po prawej powiększa wykres i jego napisy."),
    figure_panel("Ryc. 4.3", title = "Ile osób ma „około 170 cm”?", width_mode = "text",
      tags$div(class = "lc-area", `data-mu` = "170", `data-sd` = "8",
        `data-a` = "165", `data-b` = "175", `data-lo` = "140", `data-hi` = "200",
        lc_toolbar(lc_caption("Przeciągnij granice lub użyj Tab i strzałek."), area_readouts(), lc_chart_zoom_button()),
        tags$div(class = "lc-area-canvas",
          tags$canvas(`data-role` = "pdf", role = "img",
            `aria-label` = "Gęstość wzrostu N(170, 8) z polem między granicami a i b"))
      )
    ),
    lc_more("Sterowanie klawiaturą",
      lc_p("Tab przechodzi do lupy i uchwytów granic. Strzałki przesuwają granicę o 0.5 cm,
        Shift ze strzałką o 5 cm. Home i End przesuwają ją do krańca dostępnego zakresu.
        Przycisk lupy działa również klawiszem Enter lub spacją.")
    )
  )
))

ui <- lecture_page(
  lecture_id = "chart-dostepnosc-prototyp", lecture_num = "P", lecture_title = "Sterowanie i rzutnik",
  module_label = "Prototyp", chapters = .chapters,
  header_extras = tagList(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"),
    includeScript(file.path(project_root, "R", "lc_charts.js")),
    includeScript(file.path(project_root, "03-rozklady-prawdopodobienstwa", "modules", "area_chart.js")),
    includeCSS(file.path(app_dir, "prototype.css"))
  )
)

server <- function(input, output, session) {
  lecture_server(.chapters, input, output, session)
}

shinyApp(ui, server)
