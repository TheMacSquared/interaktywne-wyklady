# Co czyni dobry zbiór danych?
# Interaktywny wykład oparty o case studies - ocena jakości danych do analiz statystycznych

library(shiny)
library(bslib)
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

source(file.path(project_root, "R", "shared.R"),              local = TRUE)
source(file.path(app_dir, "modules", "helpers.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch0_wprowadzenie.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch1_katalog.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch2_szkoly.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch3_grupa.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch4_pingwiny.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch5_tarantino.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch6_firma.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch7_wynagrodzenia.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch8_ankieta.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch9_mieszkania.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch10_studenci.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch11_powietrze.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch12_sciaga.R"),        local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  tags$head(
    includeCSS(file.path(project_root, "R", "shared_styles.css")),
    includeScript(file.path(project_root, "R", "shared_toc.js")),
    tags$style(HTML("
    /* Verdict badge */
    .verdict-badge {
      display: inline-block; padding: 4px 12px; border-radius: 12px;
      color: white; font-weight: bold; font-size: 13px;
    }
    .verdict-good { background: #27ae60; }
    .verdict-mixed { background: #f39c12; }
    .verdict-bad { background: #e74c3c; }

    /* Reveal sections */
    .reveal-section {
      border: 2px dashed #3498db; border-radius: 8px;
      padding: 15px; margin: 15px 0; background: #f0f8ff;
    }

    /* Jamovi-style data table */
    .jamovi-table .dataTables_wrapper { font-family: 'Segoe UI', Roboto, sans-serif; }
    .jamovi-table table.dataTable thead th {
      background: #f0f0f0; border-bottom: 2px solid #bbb;
      font-weight: 600; font-size: 13px; padding: 8px 10px;
      text-align: center; vertical-align: bottom;
    }
    .jamovi-table table.dataTable thead th .var-type {
      display: block; font-size: 10px; font-weight: 400;
      color: #888; margin-top: 2px; font-style: italic;
    }
    .jamovi-table table.dataTable tbody td {
      padding: 6px 10px; font-size: 13px; text-align: center;
      border-right: 1px solid #eee;
    }
    .jamovi-table table.dataTable tbody tr:nth-child(odd) { background: #fafafa; }
    .jamovi-table table.dataTable tbody tr:nth-child(even) { background: #fff; }
    .cell-error { background: #fdedec !important; color: #c0392b; font-weight: 600; }
    .cell-na { background: #f5f5f5 !important; color: #bbb; font-style: italic; }
    .cell-messy { background: #fef9e7 !important; color: #7d6608; }
    .cell-ok { background: #eafaf1 !important; }

    /* Problem card in catalog */
    .problem-card {
      background: #fff; border: 1px solid #dee2e6; border-radius: 10px;
      padding: 25px; margin: 25px 0;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06);
    }
    .problem-card .problem-header {
      display: flex; align-items: center; gap: 12px; margin-bottom: 15px;
    }
    .problem-card .problem-number {
      display: inline-flex; width: 36px; height: 36px; border-radius: 50%;
      background: #e74c3c; color: white; font-weight: 700; font-size: 16px;
      align-items: center; justify-content: center; flex-shrink: 0;
    }
    .problem-card .problem-name {
      font-size: 20px; font-weight: 700; color: #2c3e50; margin: 0;
    }
    .problem-card .problem-desc {
      font-size: 15px; color: #555; line-height: 1.6; margin-bottom: 15px;
    }
    .dual-view { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 15px 0; }
    .dual-view .view-panel { min-width: 0; }
    .view-label {
      font-size: 12px; font-weight: 600; color: #7f8c8d;
      text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;
    }
    @media (max-width: 992px) { .dual-view { grid-template-columns: 1fr; } }

    /* Toggle pill buttons */
    .toggle-pills { display: inline-flex; border: 2px solid #3498db; border-radius: 20px; overflow: hidden; margin: 10px 0; }
    .toggle-pills .pill-btn {
      border: none; background: white; color: #3498db; padding: 6px 18px;
      font-size: 13px; font-weight: 600; cursor: pointer; transition: all 0.2s;
    }
    .toggle-pills .pill-btn.active { background: #3498db; color: white; }
    .toggle-pills .pill-btn:hover:not(.active) { background: #eaf4fc; }
  ")),
    tags$script(HTML("
    // Custom message handler for toggle button styling
    Shiny.addCustomMessageHandler('shinyjs-runjs', function(message) {
      eval(message.code);
    });
    $(function() {
      var tocEl = $('<div id=\"sticky-toc\"></div>').appendTo('body');
      function buildToc() {
        var activeTab = $('.tab-pane.active');
        if (!activeTab.length) return;
        var sections = activeTab.find('.section-title');
        if (sections.length < 2) { tocEl.hide(); return; }
        var html = '<div class=\"toc-title\">Spis tresci</div>';
        sections.each(function(i) {
          var el = $(this); var id = 'toc-sec-' + i; el.attr('id', id);
          var text = el.text().trim();
          if (text.length > 35) text = text.substring(0, 33) + '...';
          html += '<a href=\"#' + id + '\">' + text + '</a>';
        });
        tocEl.html(html).show();
      }
      function updateActive() {
        var scrollTop = $(window).scrollTop(); var current = null;
        $('.tab-pane.active .section-title').each(function() {
          if ($(this).offset().top - 100 <= scrollTop) current = $(this).attr('id');
        });
        tocEl.find('a').removeClass('toc-active');
        if (current) tocEl.find('a[href=\"#' + current + '\"]').addClass('toc-active');
      }
      tocEl.on('click', 'a', function(e) {
        e.preventDefault();
        var target = $($(this).attr('href'));
        if (target.length) $('html, body').animate({ scrollTop: target.offset().top - 60 }, 300);
      });
      $(document).on('shown.bs.tab', function() { setTimeout(buildToc, 150); });
      $(window).on('scroll', updateActive);
      setTimeout(buildToc, 500);
    });
  "))
  )
)

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Co czyni dobry zbiór danych?",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch0_ui,
  ch1_ui,
  ch2_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui,
  ch6_ui,
  ch7_ui,
  ch8_ui,
  ch9_ui,
  ch10_ui,
  ch11_ui,
  ch12_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  observeEvent(input$ch0_next,  { updateNavbarPage(session, "main_nav", selected = "1. Katalog") })
  observeEvent(input$cat_next,  { updateNavbarPage(session, "main_nav", selected = "2. Szkoły") })
  observeEvent(input$ch1_next,  { updateNavbarPage(session, "main_nav", selected = "3. Grupa") })
  observeEvent(input$ch2_next,  { updateNavbarPage(session, "main_nav", selected = "4. Pingwiny") })
  observeEvent(input$ch3_next,  { updateNavbarPage(session, "main_nav", selected = "5. Tarantino") })
  observeEvent(input$ch4_next,  { updateNavbarPage(session, "main_nav", selected = "6. Hotel") })
  observeEvent(input$ch5_next,  { updateNavbarPage(session, "main_nav", selected = "7. Wynagrodzenia") })
  observeEvent(input$ch6_next,  { updateNavbarPage(session, "main_nav", selected = "8. Formularz") })
  observeEvent(input$ch7_next,  { updateNavbarPage(session, "main_nav", selected = "9. Laboratorium") })
  observeEvent(input$ch8_next,  { updateNavbarPage(session, "main_nav", selected = "10. Studenci") })
  observeEvent(input$ch9_next,  { updateNavbarPage(session, "main_nav", selected = "11. Kawiarnia") })
  observeEvent(input$ch10_next, { updateNavbarPage(session, "main_nav", selected = "12. \u015aci\u0105ga") })

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
