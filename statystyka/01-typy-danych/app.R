# Statystyka opisowa - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania statystyk opisowych

library(shiny)
library(ggplot2)
library(dplyr)
library(e1071)  # for skewness, kurtosis
library(gridExtra)
library(jsonlite)

# ============================================================================
# ETYKIETY TYPÓW ZMIENNYCH
# (kolory: type_colors z R/palette.R)
# ============================================================================

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
  plec = list(label = "Płeć", type = "nominalna"),
  kierunek = list(label = "Kierunek studiów", type = "nominalna"),
  grupa_krwi = list(label = "Grupa krwi", type = "nominalna"),
  rok_studiow = list(label = "Rok studiów", type = "porzadkowa"),
  zadowolenie = list(label = "Zadowolenie ze studiów", type = "porzadkowa"),
  liczba_kursow = list(label = "Liczba kursów", type = "ilosciowa_dyskretna"),
  liczba_nieobecnosci = list(label = "Liczba nieobecności", type = "ilosciowa_dyskretna"),
  wzrost = list(label = "Wzrost (cm)", type = "ilosciowa_ciagla"),
  srednia_ocen = list(label = "Średnia ocen", type = "ilosciowa_ciagla"),
  czas_dojazdu = list(label = "Czas dojazdu (min)", type = "ilosciowa_ciagla"),
  waga = list(label = "Waga (kg)", type = "ilosciowa_ciagla"),
  ocena_wykladowcy = list(label = "Ocena wykładowcy (1-10)", type = "porzadkowa")
)

# ============================================================================
# MODULY
# ============================================================================

# Ustal katalog aplikacji niezaleznie od sposobu uruchomienia
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

source(file.path(project_root, "R", "palette.R"),          local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),       local = TRUE)
source(file.path(project_root, "R", "shared.R"),           local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"),   local = TRUE)

# Globalne defaulty ggplot2 — motyw + kolory geom-ów (upwr_single = burgund)
lc_apply_ggplot_defaults()

source(file.path(app_dir, "modules", "helpers.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch1_typy.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch2_jakosciowe.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch3_polozenie.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch4_rozrzut.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch5_ksztalt.R"),    local = TRUE)
source(file.path(app_dir, "modules", "ch6_sciaga.R"),     local = TRUE)
source(file.path(app_dir, "modules", "ch7_quiz.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch8_cwiczenia.R"),  local = TRUE)

# ============================================================================
# GLOBAL UI HEADER (CSS, JS, Chart.js)
# ============================================================================

# App-specyficzne extras — przekazywane do lecture_page() jako header_extras
# (CSS i JS layoutu są już inkludowane przez lecture_page)
app_extras <- tagList(
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

    $(document).on('shown.bs.tab', function(e) {
      if (_pendingScenario) {
        setTimeout(function() { _doRenderScenario(_pendingScenario); }, 100);
      }
    });
  ")),
  tags$style(HTML("
  /* Type badges */
  .type-badge {
    display: inline-block; padding: 6px 14px; border-radius: 20px;
    color: white; font-weight: bold; font-size: 14px; margin: 2px;
  }

  /* Taxonomy tree (HTML/CSS) */
  .taxonomy-tree { overflow-x: auto; padding: 10px 0; }
  .taxonomy-tree, .taxonomy-tree ul, .taxonomy-tree li {
    list-style: none; margin: 0; padding: 0;
  }
  .taxonomy-tree > ul { padding-top: 0; }
  .taxonomy-tree ul {
    display: flex; justify-content: center;
    padding-top: 20px; position: relative;
  }
  .taxonomy-tree ul ul::before {
    content: ''; position: absolute; top: 0; left: 50%;
    border-left: 2px solid var(--upwr-rule); height: 20px;
  }
  .taxonomy-tree li {
    display: flex; flex-direction: column; align-items: center;
    position: relative; padding: 20px 8px 0; text-align: center;
  }
  .taxonomy-tree > ul > li { padding-top: 0; }
  .taxonomy-tree li::before, .taxonomy-tree li::after {
    content: ''; position: absolute; top: 0;
    border-top: 2px solid var(--upwr-rule); width: 50%; height: 20px;
  }
  .taxonomy-tree li::before { right: 50%; }
  .taxonomy-tree li::after { left: 50%; border-left: 2px solid var(--upwr-rule); }
  .taxonomy-tree li:first-child::before { border: none; }
  .taxonomy-tree li:last-child::after { border: none; }
  .taxonomy-tree li:last-child::before {
    border-right: 2px solid var(--upwr-rule); border-radius: 0 5px 0 0;
  }
  .taxonomy-tree li:first-child::after { border-radius: 5px 0 0 0; }
  .taxonomy-tree li:only-child::before,
  .taxonomy-tree li:only-child::after { display: none; }
  .taxonomy-tree li:only-child { padding-top: 0; }

  .tax-node {
    background: var(--upwr-surface-sunken); border: 2px solid var(--upwr-ink-soft); border-radius: 8px;
    padding: 10px 18px; font-weight: 700; font-size: 15px; color: var(--upwr-ink);
    white-space: nowrap;
  }
  .tax-node small { font-weight: 400; font-size: 12px; color: var(--upwr-reference); }
  .tax-leaf {
    border-radius: 8px; padding: 12px 16px; font-weight: 700; font-size: 14px;
    color: white; cursor: pointer; transition: all 0.2s; white-space: nowrap;
  }
  .tax-leaf:hover {
    transform: translateY(-3px); box-shadow: 0 4px 12px rgba(0,0,0,0.2);
  }
  .tax-example {
    font-size: 12px; color: var(--upwr-reference); margin-top: 6px; font-style: italic;
    white-space: normal; max-width: 130px; line-height: 1.3;
  }
  .tax-hint {
    font-size: 11px; color: var(--upwr-ink-subtle); margin-top: 4px; font-style: italic;
  }
  @media (max-width: 600px) {
    .taxonomy-tree li { padding: 16px 2px 0; }
    .tax-node { padding: 6px 8px; font-size: 11px; }
    .tax-node small { font-size: 9px; }
    .tax-leaf {
      padding: 8px 6px; font-size: 11px;
      transform: rotate(-25deg); margin-top: 6px;
    }
    .tax-leaf:hover { transform: rotate(-25deg) translateY(-3px); }
    .tax-example { font-size: 9px; max-width: 80px; margin-top: 10px; }
    .tax-hint { font-size: 9px; margin-top: 10px; }
  }

  /* Taxonomy detail panel */
  .tax-detail {
    background: var(--upwr-surface-sunken); border-radius: 6px;
    padding: 14px 18px; margin-top: 12px;
    animation: taxDetailFade 0.25s ease-out;
  }
  @keyframes taxDetailFade {
    from { opacity: 0; transform: translateY(-6px); }
    to   { opacity: 1; transform: translateY(0); }
  }

  /* Gallery grid */
  .example-card {
    border: 2px solid var(--upwr-rule); border-radius: 8px;
    padding: 12px; margin-bottom: 15px; background: white;
  }

  /* Variable tracker panel */
  .tracker-panel {
    background: color-mix(in srgb, var(--upwr-cat-niebo) 16%, var(--upwr-surface)); border: 1px solid var(--upwr-cat-niebo); border-radius: 6px;
    padding: 10px 14px; margin-bottom: 15px; font-size: 13px;
  }
  .tracker-panel strong { color: var(--upwr-ink); }
  "))
) # end app_extras

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2_ui, ch3_ui, ch4_ui,
                  ch5_ui, ch6_ui, ch7_ui, ch8_ui)

ui <- lecture_page(
  lecture_id    = "typy-danych",
  lecture_num   = "01",
  lecture_title = "Statystyka opisowa",
  module_label  = "Moduł I",
  chapters      = .chapters,
  header_extras = app_extras
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # NAWIGACJA MIEDZY ROZDZIALAMI
  # ==========================================================================

  lc <- lecture_server(.chapters, input, output, session)

  # Nawigacja między rozdziałami idzie przez lc_chapter_next() w modułach
  # (sendCustomMessage("switchToChapter", ...) → lc__switch_chapter).
  # Stare observeEvent(input$chN_next) zostały usunięte razem z buttonami.

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
      paste0("x̄ = ", round(mean(vals), 2), ", Me = ", round(median(vals), 2)),
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
      " | x̄ = ", round(mean(vals), 2),
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
  ch7_server(input, output, session)
  ch8_server(input, output, session)

} # end server

shinyApp(ui = ui, server = server)
