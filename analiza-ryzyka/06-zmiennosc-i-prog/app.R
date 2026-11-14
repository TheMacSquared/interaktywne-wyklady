# 06-zmiennosc-i-prog — pełny szkic wykładu

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
source(file.path(project_root, "R", "lc_stat_figures.R"), local = TRUE)
# Prototypy punktów stop (PUNKTY-STOP-PROPOZYCJA.md): wspólne narzędzia, potem
# pliki rozdziałów; block.R wpina obiekty przez z6p_get().
source(file.path(app_dir, "modules", "stop_proto_common.R"), local = TRUE)
for (.f in c("ra", "rb", "rc", "rd", "re", "rf", "rg")) {
  source(file.path(app_dir, "modules", paste0("stop_proto_", .f, ".R")), local = TRUE)
}
source(file.path(app_dir, "modules", "block.R"), local = TRUE)

lc_apply_ggplot_defaults()

.chapters <- prog_chapters

ui <- lecture_page(
  lecture_id = "zmiennosc-i-prog",
  lecture_num = "06",
  lecture_title = "Zmienność i próg",
  module_label = "Analiza ryzyka · Bananpol",
  chapters = .chapters,
  # Prototypy punktów stop działają w przeglądarce, bez serwera R: sceny
  # (modules/sceny.js, window.lcZ6) i ryciny lcSF na ECharts; pliki
  # modules/proto_*.js rejestrują sceny i ryciny poszczególnych rozdziałów.
  header_extras = tagList(
    includeScript(file.path(app_dir, "modules", "sceny.js")),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/echarts/6.1.0/echarts.min.js"),
    includeScript(file.path(project_root, "R", "lc_charts.js")),
    includeScript(file.path(project_root, "R", "lc_dist.js")),
    includeScript(file.path(project_root, "R", "lc_stat_figures.js")),
    lc_sf_styles(),
    lapply(c("ra", "rb", "rc", "rd", "re", "rf"), function(f)
      includeScript(file.path(app_dir, "modules", paste0("proto_", f, ".js"))))
  )
)

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)
  prog_server(input, output, session)
}

shinyApp(ui = ui, server = server)
