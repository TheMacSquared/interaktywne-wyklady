# Co czyni dobry zbiór danych?
# Interaktywny wykład oparty o case studies - ocena jakości danych do analiz statystycznych

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(AER)
library(palmerpenguins)
library(ISLR)
library(fivethirtyeight)

# ============================================================================
# MODUŁY
# ============================================================================

.find_app_dir <- function() {
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg))))
  }
  getwd()
}
app_dir <- .find_app_dir()
project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"),        local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),     local = TRUE)
source(file.path(project_root, "R", "shared.R"),         local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"), local = TRUE)

lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch0_wprowadzenie.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch1_katalog.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch2_szkoly.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch3_grupa.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch4_pingwiny.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch5_tarantino.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch6_hotel.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch7_wynagrodzenia.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch8_ankieta.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch9_laboratorium.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch10_studenci.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch11_kawiarnia.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch12_sciaga.R"),        local = TRUE)

# ============================================================================
# LOKALNE STYLE I JS
# ============================================================================

header_extras <- tagList(
  tags$style(HTML("
    /* Verdict badge */
    .verdict-badge {
      display: inline-block; padding: 4px 12px; border-radius: 12px;
      color: white; font-weight: bold; font-size: 13px;
    }
    .verdict-good { background: var(--upwr-szalwia); }
    .verdict-mixed { background: var(--upwr-bursztyn); }
    .verdict-bad { background: var(--upwr-accent); }

    /* Reveal sections */
    .reveal-section {
      border: 2px dashed var(--upwr-niebo); border-radius: 8px;
      padding: 15px; margin: 15px 0; background: var(--upwr-panel);
    }

    /* Jamovi-style data table */
    .jamovi-table .dataTables_wrapper { font-family: 'Segoe UI', Roboto, sans-serif; }
    .jamovi-table table.dataTable thead th {
      background: var(--upwr-panel); border-bottom: 2px solid var(--upwr-rule);
      font-weight: 600; font-size: 13px; padding: 8px 10px;
      text-align: center; vertical-align: bottom;
    }
    .jamovi-table table.dataTable thead th .var-type {
      display: block; font-size: 10px; font-weight: 400;
      color: var(--upwr-reference); margin-top: 2px; font-style: italic;
    }
    .jamovi-table table.dataTable tbody td {
      padding: 6px 10px; font-size: 13px; text-align: center;
      border-right: 1px solid var(--upwr-rule);
    }
    .jamovi-table table.dataTable tbody tr:nth-child(odd) { background: var(--upwr-panel); }
    .jamovi-table table.dataTable tbody tr:nth-child(even) { background: white; }
    .cell-error { background: #f7ded8 !important; color: var(--upwr-accent); font-weight: 600; }
    .cell-na { background: var(--upwr-bg) !important; color: var(--upwr-reference); font-style: italic; }
    .cell-messy { background: #f3e6c7 !important; color: #7a5a1a; }
    .cell-ok { background: #dce9dc !important; }

    /* Problem card in catalog */
    .problem-card {
      background: white; border: 1px solid var(--upwr-rule); border-radius: 8px;
      padding: 25px; margin: 25px 0;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06);
    }
    .problem-card .problem-header {
      display: flex; align-items: center; gap: 12px; margin-bottom: 15px;
    }
    .problem-card .problem-number {
      display: inline-flex; width: 36px; height: 36px; border-radius: 50%;
      background: var(--upwr-accent); color: white; font-weight: 700; font-size: 16px;
      align-items: center; justify-content: center; flex-shrink: 0;
    }
    .problem-card .problem-name {
      font-size: 20px; font-weight: 700; color: var(--upwr-ink); margin: 0;
    }
    .problem-card .problem-desc {
      font-size: 15px; color: var(--upwr-ink-soft); line-height: 1.6; margin-bottom: 15px;
    }
    .dual-view { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 15px 0; }
    .dual-view .view-panel { min-width: 0; }
    .view-label {
      font-size: 12px; font-weight: 600; color: var(--upwr-reference);
      text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;
    }
    @media (max-width: 992px) { .dual-view { grid-template-columns: 1fr; } }

    /* Toggle pill buttons */
    .toggle-pills { display: inline-flex; border: 2px solid var(--upwr-accent); border-radius: 20px; overflow: hidden; margin: 10px 0; }
    .toggle-pills .pill-btn {
      border: none; background: white; color: var(--upwr-accent); padding: 6px 18px;
      font-size: 13px; font-weight: 600; cursor: pointer; transition: all 0.2s;
    }
    .toggle-pills .pill-btn.active { background: var(--upwr-accent); color: white; }
    .toggle-pills .pill-btn:hover:not(.active) { background: var(--upwr-panel); }
  ")),
    tags$script(HTML("
    // Custom message handler for toggle button styling
    Shiny.addCustomMessageHandler('shinyjs-runjs', function(message) {
      eval(message.code);
    });
  "))
)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch0_ui, ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui,
                  ch7_ui, ch8_ui, ch9_ui, ch10_ui, ch11_ui, ch12_ui)

ui <- lecture_page(
  lecture_id    = "dobre-dane",
  lecture_num   = "08",
  lecture_title = "Co czyni dobry zbiór danych?",
  module_label  = "Moduł VIII",
  chapters      = .chapters,
  header_extras = header_extras
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  lc <- lecture_server(.chapters, input, output, session)

  ch0_server(input, output, session)
  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
  ch8_server(input, output, session)
  ch9_server(input, output, session)
  ch10_server(input, output, session)
  ch11_server(input, output, session)
  ch12_server(input, output, session)
}

shinyApp(ui = ui, server = server)
