# Testowanie hipotez - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania testow hipotez

library(shiny)
library(ggplot2)
library(dplyr)
library(rstatix)
library(broom)
library(tidyr)

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
project_root <- dirname(app_dir)

source(file.path(project_root, "R", "palette.R"),          local = TRUE)
source(file.path(project_root, "R", "theme_upwr.R"),       local = TRUE)
source(file.path(project_root, "R", "shared.R"),           local = TRUE)
source(file.path(project_root, "R", "lecture_layout.R"),   local = TRUE)

# Globalne defaulty ggplot2 — motyw upwr + Atkinson + kolory geom-ów
lc_apply_ggplot_defaults()

addResourcePath("assets", file.path(app_dir, "assets"))

source(file.path(app_dir, "modules", "helpers.R"),              local = TRUE)
source(file.path(app_dir, "modules", "ch1_logika.R"),           local = TRUE)
source(file.path(app_dir, "modules", "ch2_hipotezy.R"),         local = TRUE)
source(file.path(app_dir, "modules", "ch2_jedna_ilosciowa.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch3_jedna_jakosciowa.R"), local = TRUE)
source(file.path(app_dir, "modules", "ch4_korelacja.R"),        local = TRUE)
source(file.path(app_dir, "modules", "ch5_dwie_jakosciowe.R"),  local = TRUE)
source(file.path(app_dir, "modules", "ch6_dwie_grupy.R"),       local = TRUE)
source(file.path(app_dir, "modules", "ch7_anova.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch_drzewo.R"),            local = TRUE)
source(file.path(app_dir, "modules", "ch8_sciaga.R"),           local = TRUE)
source(file.path(app_dir, "modules", "ch9_cwiczenia.R"),        local = TRUE)

# ============================================================================
# LOKALNE STYLE I JS
# ============================================================================

header_extras <- tagList(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/4.4.1/chart.umd.js"),
  tags$style(HTML("
    .ws-chart-wrap {
      position: relative;
      width: 100%;
      height: 320px;
    }

    .ch2-animated-widget .ch2-step-btn {
      transition: transform 160ms ease, box-shadow 180ms ease,
                  background-color 180ms ease, color 180ms ease;
    }

    .ch2-animated-widget .ch2-step-btn:hover {
      transform: translateY(-1px);
      box-shadow: 0 4px 12px rgba(107, 26, 42, 0.14);
    }

    .ch2-animated-widget .ch2-step-btn.ch2-step-active {
      background: var(--upwr-accent) !important;
      color: #fff !important;
      box-shadow: 0 0 0 3px rgba(107, 26, 42, 0.14);
    }

    .ch2-animated-widget .ch2-step-stage {
      transition: background-color 220ms ease, box-shadow 220ms ease;
    }

    .ch2-animated-widget .ch2-step-stage.ch2-stage-refresh {
      animation: ch2StageRefresh 420ms ease-out;
    }

    .ch2-step-panel {
      animation: ch2StepEnter 360ms cubic-bezier(.2, .75, .25, 1);
      transform-origin: top center;
    }

    .ch2-step-panel .lc-feedback,
    .ch2-step-panel .lc-stat-box,
    .ch2-step-panel .lc-formula-box {
      animation: ch2SoftPop 320ms ease-out both;
    }

    .ch2-step-panel .lc-stat-box:nth-of-type(2) { animation-delay: 45ms; }
    .ch2-step-panel .lc-stat-box:nth-of-type(3) { animation-delay: 90ms; }

    @keyframes ch2StepEnter {
      from { opacity: 0; transform: translateY(10px); }
      to   { opacity: 1; transform: translateY(0); }
    }

    @keyframes ch2SoftPop {
      from { opacity: 0; transform: translateY(6px) scale(.985); }
      to   { opacity: 1; transform: translateY(0) scale(1); }
    }

    @keyframes ch2StageRefresh {
      0%   { box-shadow: inset 0 0 0 0 rgba(180, 138, 42, 0); }
      35%  { box-shadow: inset 0 0 0 999px rgba(180, 138, 42, 0.08); }
      100% { box-shadow: inset 0 0 0 0 rgba(180, 138, 42, 0); }
    }

    @media (prefers-reduced-motion: reduce) {
      .ch2-animated-widget .ch2-step-btn,
      .ch2-animated-widget .ch2-step-stage,
      .ch2-step-panel,
      .ch2-step-panel .lc-feedback,
      .ch2-step-panel .lc-stat-box,
      .ch2-step-panel .lc-formula-box {
        animation: none !important;
        transition: none !important;
        transform: none !important;
      }
    }
  ")),
  tags$script(HTML("
    window.wsCharts = window.wsCharts || {};
    window.wsPendingCharts = window.wsPendingCharts || {};

    function wsCss(name, fallback) {
      var value = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
      return value || fallback;
    }

    function wsNormPdf(x) {
      return Math.exp(-0.5 * x * x) / Math.sqrt(2 * Math.PI);
    }

    function wsDestroyChart(id) {
      if (window.wsCharts[id]) {
        window.wsCharts[id].destroy();
        delete window.wsCharts[id];
      }
    }

    function wsRenderWhenReady(type, msg) {
      var canvas = document.getElementById(msg.id);
      if (!canvas || !window.Chart) {
        window.wsPendingCharts[msg.id] = { type: type, msg: msg };
        return false;
      }
      delete window.wsPendingCharts[msg.id];
      if (type === 'sided') wsRenderSidedChart(msg);
      if (type === 'pvalue') wsRenderPValueChart(msg);
      if (type === 'anova') wsRenderAnovaSignalChart(msg);
      return true;
    }

    function wsFlushPendingCharts() {
      Object.keys(window.wsPendingCharts).forEach(function(id) {
        var pending = window.wsPendingCharts[id];
        wsRenderWhenReady(pending.type, pending.msg);
      });
    }

    function wsDistributionData(msg) {
      var xs = [];
      var line = [];
      var shade = [];
      var alpha = msg.alpha || 0.05;
      var sided = msg.sided || 'two.sided';
      var critHigh = sided === 'two.sided' ? msg.crit : (sided === 'greater' ? msg.crit : null);
      var critLow = sided === 'two.sided' ? -msg.crit : (sided === 'less' ? msg.crit : null);

      for (var i = 0; i <= 240; i++) {
        var x = -4 + i * (8 / 240);
        var y = wsNormPdf(x);
        var inTail = false;
        if (sided === 'two.sided') inTail = x <= critLow || x >= critHigh;
        if (sided === 'greater') inTail = x >= critHigh;
        if (sided === 'less') inTail = x <= critLow;
        xs.push(x.toFixed(2));
        line.push({ x: x, y: y });
        shade.push({ x: x, y: inTail ? y : null });
      }
      return { line: line, shade: shade, critLow: critLow, critHigh: critHigh, alpha: alpha };
    }

    function wsRenderSidedChart(msg) {
      var canvas = document.getElementById(msg.id);
      if (!canvas || !window.Chart) return;
      wsDestroyChart(msg.id);
      var accent = wsCss('--upwr-accent', '#6b1a2a');
      var h0 = wsCss('--upwr-niebo', '#6a9dc4');
      var reference = wsCss('--upwr-reference', '#8b8175');
      var data = wsDistributionData(msg);
      var datasets = [
        { label: 'Obszar odrzucenia', data: data.shade, borderColor: 'transparent',
          backgroundColor: 'rgba(107, 26, 42, 0.30)', fill: 'origin', pointRadius: 0,
          tension: 0.25, spanGaps: false },
        { label: 'Rozkład pod H0', data: data.line, borderColor: h0, borderWidth: 3,
          backgroundColor: 'transparent', pointRadius: 0, tension: 0.25 }
      ];

      if (data.critLow !== null) {
        datasets.push({
          label: 'Dolny punkt krytyczny',
          data: [{ x: data.critLow, y: 0 }, { x: data.critLow, y: wsNormPdf(data.critLow) }],
          borderColor: accent, borderWidth: 2, borderDash: [6, 5],
          pointRadius: 0, showLine: true
        });
      }
      if (data.critHigh !== null) {
        datasets.push({
          label: 'Górny punkt krytyczny',
          data: [{ x: data.critHigh, y: 0 }, { x: data.critHigh, y: wsNormPdf(data.critHigh) }],
          borderColor: accent, borderWidth: 2, borderDash: [6, 5],
          pointRadius: 0, showLine: true
        });
      }

      window.wsCharts[msg.id] = new Chart(canvas, {
        type: 'line',
        data: { datasets: datasets },
        options: {
          responsive: true, maintainAspectRatio: false, parsing: false,
          animation: { duration: 650, easing: 'easeOutQuart' },
          scales: {
            x: { type: 'linear', min: -4, max: 4, grid: { color: 'rgba(139,129,117,.12)' },
                 ticks: { color: reference } },
            y: { min: 0, max: 0.43, grid: { color: 'rgba(139,129,117,.12)' },
                 ticks: { color: reference } }
          },
          plugins: {
            legend: { display: false },
            title: { display: false },
            tooltip: { enabled: false }
          }
        }
      });
    }

    function wsRenderPValueChart(msg) {
      wsRenderSidedChart(Object.assign({
        sided: 'two.sided',
        alpha: 0.03,
        crit: msg.stat || 2.17
      }, msg));
    }

    function wsRenderAnovaSignalChart(msg) {
      var canvas = document.getElementById(msg.id);
      if (!canvas || !window.Chart) return;
      wsDestroyChart(msg.id);
      var accent = wsCss('--upwr-accent', '#6b1a2a');
      var sage = wsCss('--upwr-szalwia', '#4a8a6a');
      var reference = wsCss('--upwr-reference', '#8b8175');
      var between = Number(msg.between || 3);
      var within = Number(msg.within || 1.6);
      var fApprox = Math.max(0.05, (between * between) / (within * within));

      window.wsCharts[msg.id] = new Chart(canvas, {
        type: 'bar',
        data: {
          labels: ['Sygnał między grupami', 'Szum wewnątrz grup', 'Przybliżone F'],
          datasets: [{
            data: [between, within, fApprox],
            backgroundColor: [accent, sage, wsCss('--upwr-bursztyn', '#c08540')],
            borderRadius: 6,
            barPercentage: 0.68
          }]
        },
        options: {
          indexAxis: 'y',
          responsive: true, maintainAspectRatio: false,
          animation: { duration: 650, easing: 'easeOutQuart' },
          scales: {
            x: { beginAtZero: true, grid: { color: 'rgba(139,129,117,.12)' },
                 ticks: { color: reference } },
            y: { grid: { display: false }, ticks: { color: reference, font: { size: 13 } } }
          },
          plugins: {
            legend: { display: false },
            title: { display: false },
            tooltip: {
              callbacks: { label: function(ctx) { return ' ' + Number(ctx.parsed.x).toFixed(2); } }
            }
          }
        }
      });
    }

    if (window.Shiny) {
      Shiny.addCustomMessageHandler('ws_sided_chart', function(msg) {
        wsRenderWhenReady('sided', msg);
      });
      Shiny.addCustomMessageHandler('ws_pvalue_chart', function(msg) {
        wsRenderWhenReady('pvalue', msg);
      });
      Shiny.addCustomMessageHandler('ws_anova_signal_chart', function(msg) {
        wsRenderWhenReady('anova', msg);
      });
    }

    document.addEventListener('DOMContentLoaded', function() {
      wsFlushPendingCharts();
      var observer = new MutationObserver(wsFlushPendingCharts);
      observer.observe(document.body, { childList: true, subtree: true });
    });

    document.addEventListener('click', function(event) {
      var btn = event.target.closest('.ch2-step-btn');
      if (!btn) return;

      var widget = btn.closest('.ch2-animated-widget');
      if (!widget) return;

      widget.querySelectorAll('.ch2-step-btn').forEach(function(other) {
        other.classList.remove('ch2-step-active');
      });
      btn.classList.add('ch2-step-active');

      var stage = widget.querySelector('.ch2-step-stage');
      if (stage) {
        stage.classList.remove('ch2-stage-refresh');
        void stage.offsetWidth;
        stage.classList.add('ch2-stage-refresh');
      }
    });

    document.addEventListener('click', function(event) {
      var reset = event.target.closest('.ch2-sample-reset');
      if (!reset) return;
      document.querySelectorAll('.ch2-animated-widget .ch2-step-btn').forEach(function(btn) {
        btn.classList.remove('ch2-step-active');
      });
    });

    document.addEventListener('change', function(event) {
      if (!event.target || !['ch2_scenario', 'ch2_n'].includes(event.target.id)) return;
      document.querySelectorAll('.ch2-animated-widget .ch2-step-btn').forEach(function(btn) {
        btn.classList.remove('ch2-step-active');
      });
    });
  "))
)

# ============================================================================
# UI
# ============================================================================

.chapters <- list(ch1_ui, ch2h_ui, ch1d_ui, ch2_ui, ch3_ui, ch4_ui,
                  ch5_ui, ch6_ui, ch7_ui, ch_drzewo_ui, ch8_ui, ch9_ui)

ui <- lecture_page(
  lecture_id    = "wnioskowanie-statystyczne",
  lecture_num   = "04",
  lecture_title = "Testowanie hipotez",
  module_label  = "Moduł IV",
  chapters      = .chapters,
  header_extras = header_extras
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  lc <- lecture_server(.chapters, input, output, session)

  # Nawigacja między rozdziałami idzie przez lc_chapter_next() w modułach
  # (sendCustomMessage("switchToChapter", ...) → lc__switch_chapter).

  # ==========================================================================
  # CHAPTER SERVERS
  # ==========================================================================

  ch1_server(input, output, session)
  ch2h_server(input, output, session)
  ch2_server(input, output, session)
  ch3_server(input, output, session)
  ch4_server(input, output, session)
  ch5_server(input, output, session)
  ch6_server(input, output, session)
  ch7_server(input, output, session)
  ch_drzewo_server(input, output, session)
  ch8_server(input, output, session)
  ch9_server(input, output, session)

}

shinyApp(ui = ui, server = server)
