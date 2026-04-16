# Zalozenia testow - interaktywny przewodnik
# Scrollowalny skrypt: zalozenia wszystkich poznanych metod i alternatywy

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(lmtest)

# ============================================================================
# KOLORY
# ============================================================================

col_primary    <- "#3498db"
col_secondary  <- "#e74c3c"
col_success    <- "#27ae60"
col_warning    <- "#f39c12"
col_dark       <- "#2c3e50"
col_purple     <- "#9b59b6"
col_teal       <- "#1abc9c"

col_ok         <- "#27ae60"    # zielony - zalozenie spelnione
col_fail       <- "#e74c3c"    # czerwony - zalozenie naruszone
col_test       <- "#3498db"    # niebieski - dane/test
col_alt        <- "#9b59b6"    # fioletowy - alternatywa

# ============================================================================
# MODULY
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

source(file.path(app_dir, "modules", "helpers.R"),          local = TRUE)
source(file.path(app_dir, "modules", "ch1_normalnosc.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch2_wariancje.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch3_regresja.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch4_chi_fisher.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch5_mapa.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),       local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
  tags$style(HTML("
  .narrative { font-size: 16px; line-height: 1.7; color: #2c3e50; margin-bottom: 15px; }
  .narrative p { margin-bottom: 12px; }
  .widget-block {
    background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px;
    padding: 20px; margin: 25px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  }
  .section-title {
    font-size: 22px; font-weight: bold; color: #34495e;
    margin-top: 35px; margin-bottom: 15px;
    border-left: 4px solid #3498db; padding-left: 12px;
  }
  .step-buttons { display: flex; gap: 6px; margin-bottom: 15px; flex-wrap: wrap; }
  .step-buttons .btn { flex: 1; min-width: 120px; }
  .stat-box {
    display: inline-block; padding: 8px 16px; margin: 4px;
    border-radius: 6px; font-weight: bold; font-size: 16px;
    color: white; min-width: 100px; text-align: center;
  }
  .callout-info {
    background: #eaf4fc; border-left: 4px solid #3498db;
    padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
  }
  .callout-warning {
    background: #fef9e7; border-left: 4px solid #f39c12;
    padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
  }
  .callout-danger {
    background: #fdedec; border-left: 4px solid #e74c3c;
    padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
  }
  .callout-success {
    background: #eafaf1; border-left: 4px solid #27ae60;
    padding: 12px 16px; margin: 15px 0; border-radius: 0 6px 6px 0;
  }
  .chapter-transition {
    background: linear-gradient(135deg, #eaf4fc, #f0f7ee);
    border: 1px solid #b8d4e8; border-radius: 8px;
    padding: 20px; margin: 30px 0 15px 0; text-align: center;
  }
  .chapter-transition p { font-size: 16px; color: #2c3e50; margin-bottom: 12px; }
  .chapter-transition .btn { font-size: 16px; padding: 10px 30px; }
  .chapter-recap { font-size: 14px; color: #7f8c8d; font-style: italic; margin-bottom: 5px; }
  .formula-box {
    background: #f5f0ff; border: 1px solid #d5c8f0; border-radius: 6px;
    padding: 12px 16px; margin: 10px 0;
  }
  .preset-buttons { display: flex; gap: 6px; margin-bottom: 15px; flex-wrap: wrap; }
  .preset-buttons .btn { flex: 1; min-width: 100px; font-size: 13px; }
  #sticky-toc {
    position: fixed; top: 70px; left: 10px; width: 180px;
    max-height: calc(100vh - 90px); overflow-y: auto;
    background: rgba(255,255,255,0.95); border: 1px solid #dee2e6;
    border-radius: 8px; padding: 10px 8px; font-size: 12px;
    z-index: 1000; box-shadow: 0 2px 8px rgba(0,0,0,0.08);
  }
  #sticky-toc .toc-title {
    font-weight: bold; color: #2c3e50; margin-bottom: 6px;
    font-size: 13px; padding-bottom: 4px; border-bottom: 1px solid #dee2e6;
  }
  #sticky-toc a {
    display: block; padding: 3px 6px; color: #7f8c8d; text-decoration: none;
    border-radius: 4px; line-height: 1.3; margin-bottom: 2px;
  }
  #sticky-toc a:hover { color: #3498db; background: #eaf4fc; }
  #sticky-toc a.toc-active { color: #3498db; font-weight: bold; background: #eaf4fc; }
  @media (max-width: 1400px) { #sticky-toc { display: none !important; } }
  /* Mobile TOC toggle */
  #toc-mobile-btn {
    display: none; position: fixed; bottom: 20px; right: 20px;
    width: 48px; height: 48px; border-radius: 50%; background: #3498db;
    color: white; border: none; font-size: 22px; line-height: 48px;
    text-align: center; cursor: pointer; z-index: 1001;
    box-shadow: 0 2px 10px rgba(0,0,0,0.2);
  }
  #toc-mobile-btn:hover { background: #2980b9; }
  #toc-overlay {
    display: none; position: fixed; top: 0; left: 0; right: 0; bottom: 0;
    background: rgba(0,0,0,0.4); z-index: 1000;
  }
  @media (max-width: 1400px) {
    #toc-mobile-btn { display: block; }
    #sticky-toc.toc-open {
      display: block !important; position: fixed;
      top: 50%; left: 50%; transform: translate(-50%, -50%);
      width: 85%; max-width: 320px; max-height: 70vh;
      z-index: 1002; box-shadow: 0 4px 20px rgba(0,0,0,0.3);
    }
    #toc-overlay.toc-open { display: block; }
  }
  ")),
  tags$script(HTML("
    $(function() {
      var tocEl = $('<div id=\"sticky-toc\"></div>').appendTo('body');
      function buildToc() {
        var activeTab = $('.tab-pane.active');
        if (!activeTab.length) return;
        var sections = activeTab.find('.section-title');
        if (sections.length < 2) { tocEl.hide(); return; }
        var html = '<div class=\"toc-title\">Spis tre\u015bci</div>';
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
  ))

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Za\u0142o\u017cenia test\u00f3w",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. Jednorodne wariancje")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav", selected = "3. Za\u0142o\u017cenia regresji")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Za\u0142o\u017cenia \u03c7\u00b2 i Fishera")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "5. Mapa metod")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav", selected = "6. \u015aci\u0105ga")
  })

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
}

shinyApp(ui = ui, server = server)
