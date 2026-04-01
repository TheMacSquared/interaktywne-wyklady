# Przedzialy ufnosci - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania przedzialow ufnosci

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)

# ============================================================================
# KOLORY
# ============================================================================

col_primary    <- "#3498db"    # niebieski
col_secondary  <- "#e74c3c"    # czerwony
col_success    <- "#27ae60"    # zielony
col_warning    <- "#f39c12"    # pomaranczowy
col_dark       <- "#2c3e50"    # ciemny
col_purple     <- "#9b59b6"    # fioletowy
col_teal       <- "#1abc9c"    # morski

# Kolory specyficzne dla przedzialow ufnosci
col_ci         <- "#3498db"    # niebieski - przedzial ufnosci
col_miss       <- "#e74c3c"    # czerwony - przedzial nie trafil
col_hit        <- "#27ae60"    # zielony - przedzial trafil
col_estimate   <- "#f39c12"    # pomaranczowy - estymata punktowa
col_true       <- "#9b59b6"    # fioletowy - prawdziwy parametr

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

source(file.path(app_dir, "modules", "helpers.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch1_estymacja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch2_idea.R"),      local = TRUE)
source(file.path(app_dir, "modules", "ch3_srednia.R"),   local = TRUE)
source(file.path(app_dir, "modules", "ch4_proporcja.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch5_czynniki.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),    local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
  tags$style(HTML("
  /* Narrative text */
  .narrative { font-size: 16px; line-height: 1.7; color: #2c3e50; margin-bottom: 15px; }
  .narrative p { margin-bottom: 12px; }

  /* Widget containers */
  .widget-block {
    background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px;
    padding: 20px; margin: 25px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  }

  /* Headings */
  .section-title {
    font-size: 22px; font-weight: bold; color: #34495e;
    margin-top: 35px; margin-bottom: 15px;
    border-left: 4px solid #3498db; padding-left: 12px;
  }

  /* Step buttons row */
  .step-buttons { display: flex; gap: 6px; margin-bottom: 15px; flex-wrap: wrap; }
  .step-buttons .btn { flex: 1; min-width: 120px; }

  /* Inline stats */
  .stat-box {
    display: inline-block; padding: 8px 16px; margin: 4px;
    border-radius: 6px; font-weight: bold; font-size: 16px;
    color: white; min-width: 100px; text-align: center;
  }

  /* Alert/callout boxes */
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

  /* Chapter navigation */
  .chapter-transition {
    background: linear-gradient(135deg, #eaf4fc, #f0f7ee);
    border: 1px solid #b8d4e8; border-radius: 8px;
    padding: 20px; margin: 30px 0 15px 0; text-align: center;
  }
  .chapter-transition p {
    font-size: 16px; color: #2c3e50; margin-bottom: 12px;
  }
  .chapter-transition .btn {
    font-size: 16px; padding: 10px 30px;
  }

  /* Chapter opening reference */
  .chapter-recap {
    font-size: 14px; color: #7f8c8d; font-style: italic;
    margin-bottom: 5px;
  }

  /* Formula callout */
  .formula-box {
    background: #f5f0ff; border: 1px solid #d5c8f0; border-radius: 6px;
    padding: 12px 16px; margin: 10px 0;
  }

  /* Preset buttons */
  .preset-buttons { display: flex; gap: 6px; margin-bottom: 15px; flex-wrap: wrap; }
  .preset-buttons .btn { flex: 1; min-width: 100px; font-size: 13px; }

  /* Sticky TOC */
  #sticky-toc {
    position: fixed;
    top: 70px;
    left: 10px;
    width: 180px;
    max-height: calc(100vh - 90px);
    overflow-y: auto;
    background: rgba(255,255,255,0.95);
    border: 1px solid #dee2e6;
    border-radius: 8px;
    padding: 10px 8px;
    font-size: 12px;
    z-index: 1000;
    box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    transition: opacity 0.3s;
  }
  #sticky-toc .toc-title {
    font-weight: bold;
    color: #2c3e50;
    margin-bottom: 6px;
    font-size: 13px;
    padding-bottom: 4px;
    border-bottom: 1px solid #dee2e6;
  }
  #sticky-toc a {
    display: block;
    padding: 3px 6px;
    color: #7f8c8d;
    text-decoration: none;
    border-radius: 4px;
    line-height: 1.3;
    margin-bottom: 2px;
    transition: all 0.2s;
  }
  #sticky-toc a:hover { color: #3498db; background: #eaf4fc; }
  #sticky-toc a.toc-active { color: #3498db; font-weight: bold; background: #eaf4fc; }
  @media (max-width: 1400px) { #sticky-toc { display: none; } }

  /* Quiz tiles */
  .quiz-tiles { display: grid; gap: 12px; margin: 15px 0; }
  .quiz-cols-2 { grid-template-columns: repeat(2, 1fr); }
  .quiz-cols-3 { grid-template-columns: repeat(3, 1fr); }
  .quiz-cols-4 { grid-template-columns: repeat(2, 1fr); }

  .quiz-tiles .quiz-tile {
    background: white; border: 2px solid #dee2e6; border-radius: 12px;
    padding: 20px 12px; text-align: center; cursor: pointer; transition: all 0.3s;
    display: block; width: 100%; font-family: inherit; color: inherit;
  }
  .quiz-tiles .quiz-tile:hover {
    border-color: #3498db; transform: translateY(-4px);
    box-shadow: 0 8px 25px rgba(52,152,219,0.2);
  }
  .quiz-tiles .quiz-tile:focus { outline: none; }
  .quiz-tiles .quiz-tile .tile-letter {
    display: inline-block; width: 36px; height: 36px; line-height: 36px;
    border-radius: 50%; background: #3498db; color: white;
    font-weight: 700; font-size: 16px; margin-bottom: 8px;
  }
  .quiz-tiles .quiz-tile .tile-text { font-size: 13px; color: #2c3e50; }
  .quiz-cols-4 .quiz-tile .tile-text { font-size: 12px; }
  .quiz-tile.correct { border-color: #27ae60 !important; background: #eafaf1 !important; }
  .quiz-tile.wrong { border-color: #e74c3c !important; background: #fdedec !important; }
  .quiz-tile.disabled { pointer-events: none; opacity: 0.7; }
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
          var el = $(this);
          var id = 'toc-sec-' + i;
          el.attr('id', id);
          var text = el.text().trim();
          if (text.length > 35) text = text.substring(0, 33) + '...';
          html += '<a href=\"#' + id + '\" data-idx=\"' + i + '\">' + text + '</a>';
        });
        tocEl.html(html).show();
      }

      function updateActive() {
        var scrollTop = $(window).scrollTop();
        var current = null;
        $('.tab-pane.active .section-title').each(function() {
          if ($(this).offset().top - 100 <= scrollTop) current = $(this).attr('id');
        });
        tocEl.find('a').removeClass('toc-active');
        if (current) tocEl.find('a[href=\"#' + current + '\"]').addClass('toc-active');
      }

      tocEl.on('click', 'a', function(e) {
        e.preventDefault();
        var target = $($(this).attr('href'));
        if (target.length) {
          $('html, body').animate({ scrollTop: target.offset().top - 60 }, 300);
        }
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
  "Przedzia\u0142y ufno\u015bci",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui,
  ch2_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui,
  ch6_ui
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA MIEDZY ROZDZIALAMI
  # ==========================================================================

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. Idea przedzia\u0142\u00f3w")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav", selected = "3. Przedzia\u0142 dla \u015bredniej")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Przedzia\u0142 dla proporcji")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "5. Co wp\u0142ywa na szeroko\u015b\u0107?")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav", selected = "6. \u015aci\u0105ga")
  })

  # ==========================================================================
  # CHAPTER SERVERS
  # ==========================================================================

  ch1_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)

}

shinyApp(ui = ui, server = server)
