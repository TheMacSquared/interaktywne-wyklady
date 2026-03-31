# Statystyka opisowa - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania statystyk opisowych

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(e1071)  # for skewness, kurtosis
library(gridExtra)

# ============================================================================
# KOLORY
# ============================================================================

col_nominal    <- "#e74c3c"
col_ordinal    <- "#f39c12"
col_discrete   <- "#3498db"
col_continuous <- "#27ae60"
col_dark       <- "#2c3e50"

type_colors <- c(
  "nominalna"           = col_nominal,
  "porzadkowa"          = col_ordinal,
  "ilosciowa_dyskretna" = col_discrete,
  "ilosciowa_ciagla"    = col_continuous
)

type_labels <- c(
  "nominalna"           = "Jakościowa nominalna",
  "porzadkowa"          = "Jakościowa porządkowa",
  "ilosciowa_dyskretna" = "Ilościowa dyskretna",
  "ilosciowa_ciagla"    = "Ilościowa ciągła"
)

# ============================================================================
# ZBIOR DANYCH: ANKIETA STUDENCKA
# ============================================================================

set.seed(2024)
n <- 200

plec <- sample(c("Kobieta", "Mezczyzna"), n, replace = TRUE, prob = c(0.55, 0.45))

student_data <- data.frame(
  plec = factor(plec),
  kierunek = factor(sample(
    c("Informatyka", "Biologia", "Psychologia", "Ekonomia"),
    n, replace = TRUE, prob = c(0.3, 0.2, 0.25, 0.25)
  )),
  grupa_krwi = factor(sample(
    c("A", "B", "AB", "0"),
    n, replace = TRUE, prob = c(0.35, 0.2, 0.08, 0.37)
  )),
  rok_studiow = factor(
    sample(1:5, n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
    levels = 1:5, ordered = TRUE
  ),
  zadowolenie = factor(
    sample(
      c("Bardzo niezadowolony", "Niezadowolony", "Neutralny", "Zadowolony", "Bardzo zadowolony"),
      n, replace = TRUE, prob = c(0.05, 0.15, 0.30, 0.35, 0.15)
    ),
    levels = c("Bardzo niezadowolony", "Niezadowolony", "Neutralny", "Zadowolony", "Bardzo zadowolony"),
    ordered = TRUE
  ),
  liczba_kursow = sample(3:9, n, replace = TRUE),
  liczba_nieobecnosci = rpois(n, lambda = 3),
  wzrost = round(ifelse(
    plec == "Kobieta",
    rnorm(n, mean = 166, sd = 6),
    rnorm(n, mean = 178, sd = 7)
  ), 1),
  srednia_ocen = round(pmin(5.0, pmax(2.0, rnorm(n, mean = 3.8, sd = 0.6))), 2),
  czas_dojazdu = round(rgamma(n, shape = 3, scale = 10) + 5, 1),
  waga = round(ifelse(
    plec == "Kobieta",
    rnorm(n, mean = 62, sd = 8),
    rnorm(n, mean = 80, sd = 10)
  ), 1),
  ocena_wykładowcy = factor(
    sample(1:10, n, replace = TRUE,
           prob = c(0.02, 0.03, 0.05, 0.08, 0.12, 0.15, 0.20, 0.18, 0.12, 0.05)),
    levels = 1:10, ordered = TRUE
  ),
  stringsAsFactors = FALSE
)

# ============================================================================
# METADANE ZMIENNYCH
# ============================================================================

variable_meta <- list(
  plec = list(label = "P\u0142e\u0107", type = "nominalna"),
  kierunek = list(label = "Kierunek studi\u00f3w", type = "nominalna"),
  grupa_krwi = list(label = "Grupa krwi", type = "nominalna"),
  rok_studiow = list(label = "Rok studi\u00f3w", type = "porzadkowa"),
  zadowolenie = list(label = "Zadowolenie ze studi\u00f3w", type = "porzadkowa"),
  liczba_kursow = list(label = "Liczba kurs\u00f3w", type = "ilosciowa_dyskretna"),
  liczba_nieobecnosci = list(label = "Liczba nieobecno\u015bci", type = "ilosciowa_dyskretna"),
  wzrost = list(label = "Wzrost (cm)", type = "ilosciowa_ciagla"),
  srednia_ocen = list(label = "\u015arednia ocen", type = "ilosciowa_ciagla"),
  czas_dojazdu = list(label = "Czas dojazdu (min)", type = "ilosciowa_ciagla"),
  waga = list(label = "Waga (kg)", type = "ilosciowa_ciagla"),
  ocena_wykladowcy = list(label = "Ocena wyk\u0142adowcy (1-10)", type = "porzadkowa")
)

# ============================================================================
# MODULY
# ============================================================================

# Ustal katalog aplikacji niezaleznie od sposobu uruchomienia
.find_app_dir <- function() {
  # 1) Szukaj ofile w stosie wywolan (source())
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  # 2) Rscript --file=...
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("--file=", "", file_arg))))
  }
  # 3) Fallback: working directory
  getwd()
}
app_dir <- .find_app_dir()

source(file.path(app_dir, "modules", "helpers.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch1_typy.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch2_jakosciowe.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_polozenie.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch4_rozrzut.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch5_ksztalt.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),     local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS, Chart.js)
# ============================================================================

global_header <- tagList(
  withMathJax(),
  tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"),
  tags$script(HTML("
    var pieChartJS = null, barChartJS = null;
    var _pendingScenario = null;

    function _doRenderScenario(msg) {
      if (pieChartJS) pieChartJS.destroy();
      if (barChartJS) barChartJS.destroy();

      var pieCtx = document.getElementById('ch2_pie_canvas');
      var barCtx = document.getElementById('ch2_bar_canvas');

      if (!pieCtx || !barCtx) {
        // Canvas not mounted yet -- retry up to 10 times
        _pendingScenario = msg;
        return;
      }
      _pendingScenario = null;

      pieChartJS = new Chart(pieCtx, {
        type: 'pie',
        data: {
          labels: msg.labels,
          datasets: [{
            data: msg.data,
            backgroundColor: msg.colors,
            borderWidth: 1,
            borderColor: 'rgba(255,255,255,0.6)'
          }]
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          animation: { duration: 500 },
          plugins: {
            legend: { display: false },
            tooltip: {
              callbacks: {
                label: function(ctx) { return ctx.label + ': ' + ctx.parsed + '%'; }
              }
            }
          }
        }
      });

      barChartJS = new Chart(barCtx, {
        type: 'bar',
        data: {
          labels: msg.labels,
          datasets: [{
            data: msg.data,
            backgroundColor: msg.colors,
            borderRadius: 4,
            barPercentage: 0.7
          }]
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          indexAxis: 'y',
          animation: { duration: 500 },
          scales: {
            x: {
              min: 0, max: 50,
              grid: { color: 'rgba(128,128,128,0.1)' },
              ticks: {
                callback: function(v) { return v + '%'; },
                color: 'rgba(128,128,128,0.5)',
                font: { size: 12 }
              }
            },
            y: {
              grid: { display: false },
              ticks: { color: 'rgba(128,128,128,0.6)', font: { size: 13 } }
            }
          },
          plugins: {
            legend: { display: false },
            tooltip: {
              callbacks: {
                label: function(ctx) { return ctx.parsed.x + '%'; }
              }
            }
          }
        }
      });
    }

    Shiny.addCustomMessageHandler('render_scenario', function(msg) {
      _doRenderScenario(msg);
    });

    // Retry rendering when tab becomes visible (canvas gets mounted)
    $(document).on('shown.bs.tab', function(e) {
      if (_pendingScenario) {
        setTimeout(function() { _doRenderScenario(_pendingScenario); }, 100);
      }
    });
  ")),
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

  /* Type badges */
  .type-badge {
    display: inline-block; padding: 6px 14px; border-radius: 20px;
    color: white; font-weight: bold; font-size: 14px; margin: 2px;
  }

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

  /* Gallery grid */
  .example-card {
    border: 2px solid #dee2e6; border-radius: 8px;
    padding: 12px; margin-bottom: 15px; background: white;
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

  /* Variable tracker panel */
  .tracker-panel {
    background: #eaf4fc; border: 1px solid #3498db; border-radius: 6px;
    padding: 10px 14px; margin-bottom: 15px; font-size: 13px;
  }
  .tracker-panel strong { color: #2c3e50; }

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
  ")),
  tags$script(HTML("
    $(function() {
      var tocEl = $('<div id=\"sticky-toc\"></div>').appendTo('body');

      function buildToc() {
        var activeTab = $('.tab-pane.active');
        if (!activeTab.length) return;
        var sections = activeTab.find('.section-title');
        if (sections.length < 2) { tocEl.hide(); return; }

        var html = '<div class=\"toc-title\">Spis treści</div>';
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
  "Statystyka opisowa",
  id = "main_nav",
  theme = bs_theme(bootswatch = "sandstone"),
  header = global_header,
  ch1_ui,
  ch2_ui,
  ch3_ui,
  ch4_ui,
  ch5_ui,
  ch6_ui
) # end navbarPage

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA MIEDZY ROZDZIALAMI
  # ==========================================================================

  observeEvent(input$ch1_next, {
    updateNavbarPage(session, "main_nav", selected = "2. Zmienne jakościowe")
  })
  observeEvent(input$ch2_next, {
    updateNavbarPage(session, "main_nav", selected = "3. Statystyki polozenia")
  })
  observeEvent(input$ch3_next, {
    updateNavbarPage(session, "main_nav", selected = "4. Statystyki rozrzutu")
  })
  observeEvent(input$ch4_next, {
    updateNavbarPage(session, "main_nav", selected = "5. Kształt rozkładu")
  })
  observeEvent(input$ch5_next, {
    updateNavbarPage(session, "main_nav", selected = "6. \u015aci\u0105ga")
  })

  # ==========================================================================
  # VARIABLE TRACKER
  # ==========================================================================

  output$tracker_ch3 <- renderUI({
    var_name <- input$tracked_var
    req(var_name)
    vals <- student_data[[var_name]]
    label <- variable_meta[[var_name]]$label

    div(class = "tracker-panel",
      tags$strong(paste0("\U0001F50D Sledzona zmienna: ", label)),
      " | Położenie: ",
      paste0("średnia = ", round(mean(vals), 2),
             ", mediana = ", round(median(vals), 2))
    )
  })

  output$tracker_ch4 <- renderUI({
    var_name <- input$tracked_var
    req(var_name)
    vals <- student_data[[var_name]]
    label <- variable_meta[[var_name]]$label

    div(class = "tracker-panel",
      tags$strong(paste0("\U0001F50D Sledzona zmienna: ", label)),
      " | Położenie: ",
      paste0("x\u0304 = ", round(mean(vals), 2), ", Me = ", round(median(vals), 2)),
      " | Rozrzut: ",
      paste0("SD = ", round(sd(vals), 2), ", IQR = ", round(IQR(vals), 2))
    )
  })

  output$tracker_ch5 <- renderUI({
    var_name <- input$tracked_var
    req(var_name)
    vals <- student_data[[var_name]]
    label <- variable_meta[[var_name]]$label
    sk <- round(e1071::skewness(vals), 2)
    ku <- round(e1071::kurtosis(vals), 2)

    div(class = "tracker-panel",
      tags$strong(paste0("\U0001F50D Sledzona zmienna: ", label)),
      " | x\u0304 = ", round(mean(vals), 2),
      ", SD = ", round(sd(vals), 2),
      " | Kształt: skośność = ", sk, ", kurtoza = ", ku
    )
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

} # end server

shinyApp(ui = ui, server = server)
