# 05-do-zdarzenia — pełny szkic wykładu

library(shiny)
library(ggplot2)
library(dplyr)

.find_app_dir <- function() {
  # Katalog aplikacji rozpoznajemy po tym, że jego rodzic zawiera R/lecture_layout.R.
  has_project_root <- function(dir) {
    file.exists(file.path(dirname(dir), "R", "lecture_layout.R"))
  }

  candidates <- character(0)
  # 1) ofile w stosie wywołań (source())
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) candidates <- c(candidates, dirname(normalizePath(ofile)))
  }
  # 2) Rscript --file=...
  file_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    candidates <- c(candidates, dirname(normalizePath(sub("--file=", "", file_arg[[1]]))))
  }
  # 3) Katalog roboczy — shiny::runApp() ustawia go na katalog aplikacji.
  candidates <- c(candidates, getwd())

  # Pierwszy kandydat leżący w projekcie; gdy żaden nie pasuje, zachowaj stare zachowanie.
  # Bez tego uruchomienie przez wrapper (np. rozszerzenie Shiny dla VS Code) trafia do
  # katalogu wrappera, bo --file= wskazuje jego skrypt, a nie app.R.
  valid <- Filter(has_project_root, candidates)
  if (length(valid) > 0) valid[[1]] else candidates[[1]]
}

app_dir <- .find_app_dir()
project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"), local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"), local = TRUE)
source(file.path(project_root, "R", "shared.R"), local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)
source(file.path(project_root, "R", "bananpol.R"), local = TRUE)
source(file.path(project_root, "R", "risk_math.R"), local = TRUE)
source(file.path(project_root, "R", "risk_block.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_common.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_tasma.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_geo.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_suma.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_oddzialy.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_nb.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_rezerwa.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_planex.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_mix.R"), local = TRUE)
source(file.path(app_dir, "modules", "stop_proto_dostawcy.R"), local = TRUE)
source(file.path(app_dir, "modules", "block.R"), local = TRUE)

lc_apply_ggplot_defaults()

.chapters <- dozd_chapters

ui <- lecture_page(
  lecture_id = "do-zdarzenia",
  lecture_num = "05",
  lecture_title = "Ile prób do zdarzenia",
  module_label = "Analiza ryzyka · Bananpol",
  chapters = .chapters,
  # Chart.js dla punktów stop P7 (nb_chart.js) i „Limit, średnia i rezerwa” (plan_chart.js)
  header_extras = tagList(
    # Zapisy matematyczne w zwykłym tekście (\\( … \\) w akapicie albo punkcie listy)
    # dostają jasne tło z ramką, jak w wykładzie 04. Wzory numerowane stoją w <div>.
    tags$style(HTML("
      :is(p, li) > .MathJax {
        background: var(--upwr-surface);
        border: 1px solid var(--upwr-rule);
        border-radius: 4px;
        padding: .05em .35em;
        margin: 0 .1em;
      }
      /* Ramki rozpychają wyjustowany tekst, więc akapity z matematyką wyrównujemy do lewej. */
      :is(p, li):has(> .MathJax) { text-align: left; }
    ")),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"),
    includeScript(file.path(project_root, "R", "lc_charts.js")),
    includeScript(file.path(app_dir, "modules", "nb_chart.js")),
    includeScript(file.path(app_dir, "modules", "plan_chart.js")),
    includeScript(file.path(app_dir, "modules", "mix_chart.js"))
  )
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  dozd_server(input, output, session)
}

shinyApp(ui = ui, server = server)
