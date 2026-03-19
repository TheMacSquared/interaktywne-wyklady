# Statystyka opisowa - interaktywny przewodnik
# Scrollowalny skrypt z osadzonymi widgetami do nauczania statystyk opisowych

library(shiny)
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
# FUNKCJE POMOCNICZE
# ============================================================================

render_taxonomy <- function(highlight = NULL, revealed = character(0)) {
  nodes <- data.frame(
    id = c("dane", "ilościowe", "jakościowe",
           "ciagla", "dyskretna", "porzadkowa", "nominalna"),
    label = c("Dane", "Ilosciowe\n(liczbowe)", "Jakosciowe\n(kategoryczne)",
              "Ciagle", "Dyskretne", "Porzadkowe", "Nominalne"),
    x = c(5, 2.5, 7.5, 1.25, 3.75, 6.25, 8.75),
    y = c(3, 2, 2, 1, 1, 1, 1),
    type = c(NA, NA, NA, "ilosciowa_ciagla", "ilosciowa_dyskretna", "porzadkowa", "nominalna"),
    example = c("", "", "",
                "np. wzrost,\nczas dojazdu",
                "np. liczba kursow,\nliczba nieobecnosci",
                "np. rok studiow,\nzadowolenie",
                "np. plec, kierunek,\ngrupa krwi"),
    stringsAsFactors = FALSE
  )

  nodes$fill <- sapply(nodes$type, function(t) {
    if (is.na(t)) return("#ecf0f1")
    type_colors[t]
  })
  nodes$alpha <- sapply(nodes$type, function(t) {
    if (is.null(highlight) || is.na(t)) return(1)
    if (t == highlight) return(1) else return(0.3)
  })

  edges <- data.frame(
    x = c(5, 5, 2.5, 2.5, 7.5, 7.5),
    xend = c(2.5, 7.5, 1.25, 3.75, 6.25, 8.75),
    y = c(2.75, 2.75, 1.75, 1.75, 1.75, 1.75),
    yend = c(2.25, 2.25, 1.25, 1.25, 1.25, 1.25)
  )

  ggplot() +
    geom_segment(data = edges,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 color = "#bdc3c7", linewidth = 1.2) +
    geom_tile(data = nodes,
              aes(x = x, y = y, width = 2, height = 0.45),
              fill = nodes$fill, alpha = nodes$alpha,
              color = col_dark, linewidth = 0.5) +
    geom_text(data = nodes,
              aes(x = x, y = y, label = label),
              size = 5, fontface = "bold", color = col_dark) +
    geom_text(data = nodes %>% filter(id %in% revealed),
              aes(x = x, y = y - 0.35, label = example),
              size = 3.5, color = "#7f8c8d", lineheight = 0.9) +
    geom_text(data = nodes %>% filter(example != "", !id %in% revealed),
              aes(x = x, y = y - 0.35, label = "kliknij aby odkryc"),
              size = 3, color = "#bdc3c7", fontface = "italic") +
    coord_cartesian(xlim = c(-0.2, 10.2), ylim = c(0.3, 3.5)) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10))
}

render_good_plot <- function(x, label, type) {
  col <- type_colors[type]
  df <- data.frame(x = x)

  if (type %in% c("nominalna", "porzadkowa")) {
    ggplot(df, aes(x = x)) +
      geom_bar(fill = col, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Wykres słupkowy: ", label), x = label, y = "Liczebność") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = if (nlevels(factor(x)) > 4) 30 else 0, hjust = 1))
  } else if (type == "ilosciowa_dyskretna") {
    ggplot(df, aes(x = factor(x))) +
      geom_bar(fill = col, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Wykres słupkowy: ", label), x = label, y = "Liczebność") +
      theme_minimal(base_size = 14)
  } else {
    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20, fill = col, color = "white", alpha = 0.7) +
      geom_density(color = col, linewidth = 1.2) +
      labs(title = paste0("Histogram z gęstościa: ", label), x = label, y = "Gęstość") +
      theme_minimal(base_size = 14)
  }
}

render_bad_plot <- function(x, label, type) {
  df <- data.frame(x = x)
  if (type %in% c("nominalna", "porzadkowa")) {
    df$x_num <- as.numeric(factor(x))
    ggplot(df, aes(x = x_num)) +
      geom_histogram(bins = 10, fill = "#95a5a6", color = "white") +
      labs(title = paste0("Histogram (NIEODPOWIEDNI): ", label),
           subtitle = "Histogram wymaga danych liczbowych - tu mamy kategorie!",
           x = paste0(label, " (zakodowane jako liczby)"), y = "Liczebność") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(color = "#e74c3c"),
            plot.subtitle = element_text(color = "#e74c3c", face = "italic"))
  } else {
    n_unique <- length(unique(x))
    ggplot(df, aes(x = x)) +
      geom_bar(fill = "#95a5a6", width = 0.3) +
      labs(title = paste0("Wykres słupkowy (NIEODPOWIEDNI): ", label),
           subtitle = paste0(n_unique, " unikalnych wartości - wykres słupkowy jest nieczytelny!"),
           x = label, y = "Liczebność") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(color = "#e74c3c"),
            plot.subtitle = element_text(color = "#e74c3c", face = "italic"),
            axis.text.x = element_text(size = 5, angle = 90))
  }
}

pie_vs_bar_scenarios <- list(
  list(
    name = "Duze różnice",
    labels = c("Produkt A", "Produkt B", "Produkt C", "Produkt D", "Produkt E"),
    data = c(45, 25, 15, 10, 5),
    colors = c("#3266ad", "#1D9E75", "#BA7517", "#D85A30", "#993556"),
    pie_verdict = "Różnice widoczne, ale porównanie kątów jest trudniejsze niż długości",
    bar_verdict = "Natychmiastowe porównanie -- różnice czytelne od razu",
    pie_ok = TRUE
  ),
  list(
    name = "Podobne wartości",
    labels = c("Produkt A", "Produkt B", "Produkt C", "Produkt D", "Produkt E"),
    data = c(22, 21, 20, 19, 18),
    colors = c("#3266ad", "#1D9E75", "#BA7517", "#D85A30", "#993556"),
    pie_verdict = "Wycinki prawie identyczne -- nie widać która kategoria prowadzi",
    bar_verdict = "Różnice 1-2 pp. wciąż czytelne dzięki wspólnej osi",
    pie_ok = FALSE
  ),
  list(
    name = "Podobne + zle kolory",
    labels = c("Produkt A", "Produkt B", "Produkt C", "Produkt D", "Produkt E"),
    data = c(22, 21, 20, 19, 18),
    colors = c("#3266ad", "#4a7abf", "#6290d0", "#7aa6e0", "#93bcf0"),
    pie_verdict = "Zblizone wielkości + zbliżone kolory = nieczytelny wykres",
    bar_verdict = "Nawet przy podobnych kolorach pozycja na osi ratuje czytelnosc",
    pie_ok = FALSE
  )
)

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Statystyka opisowa",
  id = "main_nav",

  header = tagList(
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
  )),

  # ==========================================================================
  # CHAPTER 1 UI
  # ==========================================================================
  tabPanel("1. Typy danych",
  fluidRow(column(8, offset = 2,

    # --- Section: Introduction ---
    div(class = "section-title", "Typy danych - fundament analizy statystycznej"),

    div(class = "narrative",
      p("Pierwszym krokiem w analizie danych jest rozpoznanie typu zmiennej,
        z która mamy do czynienia. To nie jest formalność - typ zmiennej
        determinuje jakie statystyki mozemy obliczyć, jakie wykresy narysowac
        i jakie testy statystyczne zastosować."),
      p("Błędne rozpoznanie typu zmiennej prowadzi do błędnych analiz.
        Na przykład, obliczanie średniej z kodow pocztowych nie ma sensu,
        mimo ze są to liczby.")
    ),

    div(class = "callout-info",
      tags$strong("Zasada:"),
      " Zanim zaczniesz analize - zawsze okresl typ każdej zmiennej."
    ),

    # --- Widget 1: Taxonomy tree ---
    div(class = "section-title", "Taksonomia typow danych"),

    div(class = "narrative",
      p("Ponizszy diagram przedstawia hierarchie typow danych.
        Kliknij na liscie drzewa (najnizszy poziom), aby odkryc
        przyklady zmiennych każdego typu z naszego zbioru danych.")
    ),

    div(class = "widget-block",
      plotOutput("ch1_taxonomy_plot", click = "ch1_taxonomy_click",
                 height = "350px"),
      div(style = "text-align: center; margin-top: 10px;",
        actionButton("ch1_reveal_all", "Odkryj wszystkie",
                     class = "btn-outline-primary", style = "margin-right: 8px;"),
        actionButton("ch1_hide_all", "Ukryj wszystkie",
                     class = "btn-outline-secondary")
      )
    ),

    div(class = "callout-warning",
      tags$strong("Uwaga:"),
      " Granica miedzy typami nie zawsze jest ostra. Na przykład ocena wykładowcy
      w skali 1-10 moze byc traktowana jako porządkowa lub dyskretna,
      w zależności od kontekstu i celu analizy."
    ),

    # --- Widget 2: Examples gallery ---
    div(class = "section-title", "Przyklady typow zmiennych"),

    div(class = "narrative",
      p("Zobaczmy jak wyglada każdy typ zmiennej w praktyce.
        Kazdy typ ma swoje charakterystyczne cechy i wymaga
        odpowiednich narzedzi wizualizacji."),
      p("Wlacz opcje poniżej, aby zobaczyc co sie stanie,
        gdy uzyjemy nieodpowiedniego wykresu.")
    ),

    div(class = "widget-block",
      checkboxInput("ch1_show_bad",
                    "Pokaz nieodpowiednie wykresy (przykład czego NIE robic)",
                    value = FALSE),
      fluidRow(
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", col_nominal, ";"),
            span(class = "type-badge",
                 style = paste0("background: ", col_nominal, ";"),
                 "Jakościowa nominalna"),
            tags$h4("Płeć"),
            tags$p(style = "color: #7f8c8d; font-size: 13px;",
              "Kategorie bez naturalnego porzadku. Mozemy liczyc ile jest
               obserwacji w każdej kategorii, ale nie mozemy ich uporządkowac
               ani uśredniać."),
            plotOutput("ch1_ex1_plot", height = "280px")
          )
        ),
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", col_ordinal, ";"),
            span(class = "type-badge",
                 style = paste0("background: ", col_ordinal, ";"),
                 "Jakościowa porządkowa"),
            tags$h4("Zadowolenie ze studiów"),
            tags$p(style = "color: #7f8c8d; font-size: 13px;",
              "Kategorie z naturalnym porzadkiem. Wiemy ze 'Bardzo zadowolony'
               jest wyzej niż 'Zadowolony', ale nie znamy dokladnych odleglosci
               miedzy kategoriami."),
            plotOutput("ch1_ex2_plot", height = "280px")
          )
        )
      ),
      fluidRow(
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", col_discrete, ";"),
            span(class = "type-badge",
                 style = paste0("background: ", col_discrete, ";"),
                 "Ilościowa dyskretna"),
            tags$h4("Liczba kursów"),
            tags$p(style = "color: #7f8c8d; font-size: 13px;",
              "Wartości liczbowe, ale tylko całkowite. Mozemy obliczać srednia
               i odchylenie standardowe. Wykres słupkowy jest tu odpowiedni,
               bo mamy skończoną liczbę wartości."),
            plotOutput("ch1_ex3_plot", height = "280px")
          )
        ),
        column(6,
          div(class = "example-card",
              style = paste0("border-color: ", col_continuous, ";"),
            span(class = "type-badge",
                 style = paste0("background: ", col_continuous, ";"),
                 "Ilościowa ciągła"),
            tags$h4("Wzrost (cm)"),
            tags$p(style = "color: #7f8c8d; font-size: 13px;",
              "Wartości liczbowe, ktore mogą przyjmowac dowolne wartości
               z pewnego przedzialu (takze ulamkowe). Histogram grupuje
               wartości w przedziały, gęstość wygładza rozkład."),
            plotOutput("ch1_ex4_plot", height = "280px")
          )
        )
      )
    ),

    # --- Widget 4: Dataset preview ---
    div(class = "section-title", "Nasze dane - ankieta studencka"),

    div(class = "narrative",
      p("A tak wyglądają nasze dane - ankieta 200 studentow.
        To z tego zbioru beda pochodzic wszystkie przyklady
        w dalszej czesci kursu. Ponizej pierwszych 10 obserwacji.")
    ),

    div(class = "widget-block",
      div(style = "overflow-x: auto; font-size: 12px;",
        tableOutput("ch1_data_preview")
      )
    ),

    div(class = "callout-info",
      tags$strong("Zwroc uwage:"),
      " Zbior zawiera zmienne wszystkich czterech typow. W kolejnych
      rozdzialach nauczymy sie jak je podsumowywać i wizualizować."
    ),

    # --- Variable tracker selector ---
    div(class = "widget-block", style = "background: #eaf4fc; border: 2px solid #3498db;",
      h4("\U0001F50D Sledz zmienna przez caly kurs"),
      p(style = "font-size: 14px; color: #2c3e50;",
        "Wybierz jedna zmienna ilościowa. W każdym kolejnym rozdziale zobaczysz,
         jakie nowe informacje daja Ci kolejne narzedzia statystyczne zastosowane
         do tej samej zmiennej."),
      selectInput("tracked_var", "Wybierz zmienna do sledzenia:",
        choices = c("Wzrost (cm)" = "wzrost",
                    "Waga (kg)" = "waga",
                    "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost", width = "300px"
      )
    ),

    div(class = "chapter-transition",
      p("Wiemy jakie typy zmiennych mamy w naszych danych. Zaczynamy od zmiennych
        jakościowych -- są prostsze i stanowia naturalny punkt wyjscia."),
      actionButton("ch1_next", "Dalej: 2. Zmienne jakościowe \u2192",
                   class = "btn-primary btn-lg")
    ),

    # Bottom spacing
    div(style = "height: 40px;")

  ))
  ), # end ch1 tabPanel

  # ==========================================================================
  # CHAPTER 2 UI
  # ==========================================================================
  tabPanel("2. Zmienne jakościowe",
  fluidRow(column(8, offset = 2,

    # --- Introduction ---
    div(class = "chapter-recap",
      "W poprzednim rozdziale poznalismy cztery typy zmiennych. Teraz zajmiemy sie
       pierwszym z nich -- zmiennymi jakościowymi."
    ),
    div(class = "section-title", "Zmienne jakościowe"),

    div(class = "narrative",
      p("Zmienne jakościowe opisują cechy, nie liczby. Podstawowym narzędziem
        ich opisu jest tabela częstości. Zobaczmy krok po kroku, jak ja
        zbudować na przykładzie zmiennej ", tags$b("kierunek studiow"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Frequency table step-by-step
    # ========================================================================
    div(class = "widget-block",
      h4("Tabela cz\u0119sto\u015bci - krok po kroku"),
      radioButtons("ch2_freq_var", "Wybierz zmienn\u0105:",
        choices = c(
          "Kierunek studi\u00f3w (nominalna)" = "kierunek",
          "Zadowolenie ze studi\u00f3w (porz\u0105dkowa)" = "zadowolenie"
        ),
        selected = "kierunek", inline = TRUE
      ),
      div(class = "step-buttons",
        actionButton("ch2_freq_s1", "1. Surowe dane",
                     class = "btn-outline-primary"),
        actionButton("ch2_freq_s2", "2. Zliczanie",
                     class = "btn-outline-primary"),
        actionButton("ch2_freq_s3", "3. Cz\u0119sto\u015bci wzgl\u0119dne",
                     class = "btn-outline-primary"),
        actionButton("ch2_freq_s4", "4. Skumulowane",
                     class = "btn-outline-primary")
      ),
      actionButton("ch2_freq_reset", "Reset", class = "btn-secondary btn-sm"),
      uiOutput("ch2_freq_explanation"),
      tableOutput("ch2_freq_table")
    ),

    # ========================================================================
    # WIDGET 1b: Nominal vs Ordinal comparison
    # ========================================================================
    div(class = "section-title", "Nominalna vs porządkowa -- czy kolejność ma znaczenie?"),

    div(class = "narrative",
      p("Zanim przejdziemy do wizualizacji, zatrzymajmy sie na waznym rozróżnieniu.
        Zmienne jakościowe dzielimy na ", tags$b("nominalne"), " (kategorie bez naturalnej
        kolejnośći) i ", tags$b("porzadkowe"), " (kategorie z logiczna kolejnośćia).
        Ta roznica ma praktyczne konsekwencje.")
    ),

    div(class = "widget-block",
      h4("Czy kolejność kategorii ma znaczenie?"),

      checkboxInput("ch2_ord_shuffle", "Losowa kolejność kategorii", value = FALSE),

      fluidRow(
        column(6,
          h5(style = "text-align: center; color: #e74c3c;", "Nominalna: Kierunek studiów"),
          plotOutput("ch2_ord_nom_plot", height = "300px")
        ),
        column(6,
          h5(style = "text-align: center; color: #f39c12;", "Porzadkowa: Zadowolenie"),
          plotOutput("ch2_ord_ord_plot", height = "300px")
        )
      ),

      uiOutput("ch2_ord_explanation"),

    ),

    # --- Narrative before Widget 2 ---
    div(class = "section-title", "Wykres kołowy vs słupkowy"),

    div(class = "narrative",
      p("Jak wizualizować zmienne jakościowe? Porównajmy wykres kołowy ze słupkowym
        w trzech scenariuszach -- od latwego do trudnego. Zobaczysz, dlaczego
        wykres słupkowy jest ", tags$b("zawsze"), " co najmniej tak samo czytelny.")
    ),

    # ========================================================================
    # WIDGET 2: Pie vs Bar -- scenario comparison
    # ========================================================================
    div(class = "widget-block",
      h4("Trzy scenariusze porównawcze"),
      div(style = "display: flex; gap: 8px; margin-bottom: 15px; flex-wrap: wrap;",
        actionButton("ch2_sc1", "1. Duze różnice",
                     class = "btn-outline-primary"),
        actionButton("ch2_sc2", "2. Podobne wartości",
                     class = "btn-outline-primary"),
        actionButton("ch2_sc3", "3. Podobne + zle kolory",
                     class = "btn-outline-primary")
      ),
      fluidRow(
        column(6,
          h5(style = "text-align: center; color: #5f5e5a;", "Wykres kołowy"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_pie_canvas")
          ),
          uiOutput("ch2_scenario_pie_verdict")
        ),
        column(6,
          h5(style = "text-align: center; color: #5f5e5a;", "Wykres słupkowy -- te same dane"),
          div(style = "position: relative; width: 100%; height: 320px;",
            tags$canvas(id = "ch2_bar_canvas")
          ),
          uiOutput("ch2_scenario_bar_verdict")
        )
      ),
      div(style = "display: flex; flex-wrap: wrap; gap: 14px; font-size: 12px; color: #5f5e5a; margin-top: 8px;",
        id = "ch2_legend",
        uiOutput("ch2_scenario_legend")
      )
    ),

    # --- Narrative before Widget 4 ---
    div(class = "section-title", "Manipulacja kolorami"),

    div(class = "narrative",
      p("Kolory na wykresie mogą manipulowac odbiorem danych. Zobaczmy,
        jak ten sam zestaw danych moze wyglądać zupełnie inaczej w
        zależności od doboru palety kolorow.")
    ),

    # ========================================================================
    # WIDGET 4: Color manipulation demo
    # ========================================================================
    div(class = "widget-block",
      h4("Jak kolory zmieniaja percepcje danych"),
      fluidRow(
        column(4,
          selectInput("ch2_color_palette", "Paleta kolor\u00f3w:",
            choices = c(
              "Neutralna (szara)" = "neutral",
              "Ciep\u0142a (podkre\u015bla Informatyk\u0119)" = "warm",
              "Zimna (podkre\u015bla Biologi\u0119)" = "cool",
              "Stronnicza" = "biased",
              "--- Klasyczne palety R ---" = "sep1",
              "Viridis" = "viridis",
              "Set2 (ColorBrewer)" = "set2",
              "Okabe-Ito (colorblind-safe)" = "okabe_ito",
              "Tableau 10" = "tableau"
            ),
            selected = "neutral"
          ),
          actionButton("ch2_color_random", "Losowe kolory",
                       class = "btn-outline-secondary", width = "100%")
        ),
        column(8, plotOutput("ch2_color_plot", height = "380px"))
      ),
      div(class = "callout-warning",
        tags$b("Pami\u0119taj: "),
        "Wyb\u00f3r kolor\u00f3w nie jest neutralny. Intensywne, cieplejsze barwy
         przyci\u0105gaj\u0105 uwag\u0119, a jasne/szare marginalizuj\u0105 kategorie.",
        tags$br(), tags$br(),
        tags$b("Dobre praktyki: "),
        tags$ul(
          tags$li(tags$b("Viridis"), " -- percepcyjnie r\u00f3wnomierna (r\u00f3\u017cnice
            warto\u015bci = r\u00f3\u017cnice w kolorze), czytelna w skali szaro\u015bci
            i bezpieczna dla daltonist\u00f3w. Domy\u015blna w wielu pakietach R."),
          tags$li(tags$b("Okabe-Ito"), " -- paleta zaprojektowana specjalnie
            pod k\u0105tem daltoni\u015bt\u00f3w (ok. 8% m\u0119\u017cczyzn). Klasyczny wyb\u00f3r
            w publikacjach naukowych."),
          tags$li(tags$b("ColorBrewer (Set2, Set3, Paired...)"), " -- rodzina palet
            stworzonych przez kartograf\u0119 Cynthia Brewer. W R dost\u0119pne przez ",
            tags$code("scale_fill_brewer()"), "."),
          tags$li(tags$b("Tableau 10"), " -- standard w narz\u0119dziach BI,
            zbalansowana jasno\u015b\u0107 i kontrast.")
        )
      )
    ),

    # ========================================================================
    # WIDGET 4b: Cross-tabulation
    # ========================================================================
    div(class = "section-title", "Tabela krzyzowa -- dwie zmienne jednoczesnie"),

    div(class = "narrative",
      p("Dotychczas analizowalismy po jednej zmiennej. Ale często chcemy
        zbadac ", tags$b("zaleznosc miedzy dwiema zmiennymi jakościowymi"),
        ". Sluzy do tego tabela krzyzowa (kontyngencji).")
    ),

    div(class = "widget-block",
      h4("Tabela krzyzowa"),
      fluidRow(
        column(4,
          selectInput("ch2_cross_row", "Zmienna w wierszach:",
            choices = c("Płeć" = "plec", "Kierunek" = "kierunek",
                        "Grupa krwi" = "grupa_krwi"),
            selected = "plec"
          )
        ),
        column(4,
          selectInput("ch2_cross_col", "Zmienna w kolumnach:",
            choices = c("Kierunek" = "kierunek", "Płeć" = "plec",
                        "Grupa krwi" = "grupa_krwi"),
            selected = "kierunek"
          )
        ),
        column(4,
          radioButtons("ch2_cross_type", "Pokaz:",
            choices = c("Liczebnośći" = "counts",
                        "% wierszowe" = "row_pct",
                        "% kolumnowe" = "col_pct"),
            selected = "counts", inline = TRUE
          )
        )
      ),
      tableOutput("ch2_cross_table"),
      fluidRow(
        column(6,
          radioButtons("ch2_cross_chart", NULL,
            choices = c("Wykres slupkowy" = "bar", "Heatmapa" = "heatmap"),
            selected = "bar", inline = TRUE
          )
        )
      ),
      plotOutput("ch2_cross_plot", height = "350px")
    ),

    # --- Narrative before Widget 5 ---
    div(class = "section-title", "Dominanta (moda)"),

    div(class = "narrative",
      p("Dominanta (moda) to jedyna miara tendencji centralnej dla
        zmiennych nominalnych. Jest to wartość (kategoria), ktora
        występuje najczęściej w zbiorze danych.")
    ),

    # ========================================================================
    # WIDGET 5: Mode (dominanta)
    # ========================================================================
    div(class = "widget-block",
      h4("Dominanta - najcz\u0119\u015bciej wyst\u0119puj\u0105ca kategoria"),
      actionButton("ch2_mode_resample", "Losuj nowe proporcje",
                   class = "btn-primary"),
      plotOutput("ch2_mode_plot", height = "350px"),
      uiOutput("ch2_mode_text")
    ),

    div(class = "chapter-transition",
      p("Zmienne jakościowe opisaliśmy tabelami częstości i dominanta.
        A co ze zmiennymi ilościowymi? Potrzebujemy nowych narzedzi -- statystyk polozenia."),
      actionButton("ch2_next", "Dalej: 3. Statystyki polozenia \u2192",
                   class = "btn-primary btn-lg")
    ),

    # Bottom spacer
    div(style = "height: 60px;")

  )) # end column / fluidRow
  ), # end ch2 tabPanel

  # ==========================================================================
  # CHAPTER 3 UI
  # ==========================================================================
  tabPanel("3. Statystyki polozenia",
  fluidRow(column(8, offset = 2,

    # --- Introduction ---
    div(class = "chapter-recap",
      "Zmienne jakościowe podsumowaliśmy tabelami częstości i wykresami słupkowymi.
       Teraz przechodzimy do zmiennych ilościowych -- zaczynajac od pytan o 'środek' danych."
    ),
    uiOutput("tracker_ch3"),
    div(class = "section-title", "Statystyki polozenia"),

    div(class = "narrative",
      p("Zmienne ilo\u015bciowe wymagaj\u0105 nowych narz\u0119dzi. Zanim przejdziemy do
        statystyk, poznajmy podstawow\u0105 wizualizacj\u0119 \u2014 ", tags$b("histogram"),
        ". Potem zbadamy miary po\u0142o\u017cenia: ", tags$b("\u015bredni\u0105"), ", ",
        tags$b("median\u0119"), " i ", tags$b("percentyle"), ".")
    ),

    # ========================================================================
    # WIDGET: Histogram krok po kroku
    # ========================================================================
    div(class = "section-title", "Histogram \u2014 krok po kroku"),

    div(class = "narrative",
      p("Histogram to podstawowy wykres dla zmiennych ci\u0105g\u0142ych. Pokazuje
        jak cz\u0119sto wyst\u0119puj\u0105 warto\u015bci w poszczeg\u00f3lnych ",
        tags$b("przedzia\u0142ach (binach)"),
        ". Zbudujmy go krok po kroku.")
    ),

    div(class = "widget-block",
      h4("Budowa histogramu"),
      fluidRow(
        column(4,
          selectInput("ch3_hist_var", "Zmienna:",
            choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                        "Czas dojazdu (min)" = "czas_dojazdu",
                        "\u015arednia ocen" = "srednia_ocen"),
            selected = "wzrost"
          ),
          uiOutput("ch3_hist_bin_slider"),
          actionButton("ch3_hist_step1", "1. Surowe dane",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step2", "2. Posortuj dane",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step3", "3. Podziel na przedzia\u0142y",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step4", "4. Przypisz do bin\u00f3w",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step5", "5. Zlicz obserwacje",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step6", "6. Zbuduj s\u0142upki",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step7", "7. Gotowy histogram",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step8", "8. Wp\u0142yw szeroko\u015bci binu",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_reset", "Reset",
                       class = "btn-outline-secondary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_hist_plot", height = "400px"),
          uiOutput("ch3_hist_text"),
          tableOutput("ch3_hist_table")
        )
      )
    ),

    # ========================================================================
    # Transition to location statistics
    # ========================================================================
    div(class = "section-title", "Statystyki po\u0142o\u017cenia"),

    div(class = "narrative",
      p("Histogram pokazuje kszta\u0142t rozk\u0142adu, ale nie daje jednej liczby
        opisuj\u0105cej '\u015brodek'. Do tego s\u0142u\u017c\u0105 statystyki po\u0142o\u017cenia:
        ", tags$b("\u015brednia"), ", ", tags$b("mediana"), " i ",
        tags$b("percentyle"), ". Ka\u017cda odpowiada na to pytanie inaczej.")
    ),

    # ========================================================================
    # WIDGET 0a: Mean introduction
    # ========================================================================
    div(class = "section-title", "\u015arednia arytmetyczna"),

    div(class = "narrative",
      p("\u015arednia arytmetyczna to suma wszystkich warto\u015bci podzielona
        przez ich liczb\u0119. Jest to 'punkt r\u00f3wnowagi' danych -- gdyby\u015bmy
        po\u0142o\u017cyli dane na wadze, \u015brednia by\u0142aby punktem podparcia."),
      withMathJax(helpText(
        "$$\\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i = \\frac{x_1 + x_2 + \\ldots + x_n}{n}$$"
      ))
    ),

    div(class = "widget-block",
      h4("\u015arednia jako punkt r\u00f3wnowagi"),
      selectInput("ch3_mean_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                    "\u015arednia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      plotOutput("ch3_mean_plot", height = "300px"),
      uiOutput("ch3_mean_text")
    ),

    # ========================================================================
    # WIDGET 0b: Median introduction
    # ========================================================================
    div(class = "section-title", "Mediana"),

    div(class = "narrative",
      p("Mediana to warto\u015b\u0107, kt\u00f3ra dzieli posortowane dane na dwie
        r\u00f3wne po\u0142owy: 50% obserwacji le\u017cy poni\u017cej, 50% powy\u017cej.
        Nie zale\u017cy od tego, jak bardzo skrajne s\u0105 warto\u015bci
        na ko\u0144cach -- liczy si\u0119 tylko pozycja \u015brodkowa.")
    ),

    div(class = "widget-block",
      h4("Mediana dzieli dane na p\u00f3\u0142"),
      selectInput("ch3_median_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Czas dojazdu (min)" = "czas_dojazdu",
                    "\u015arednia ocen" = "srednia_ocen"),
        selected = "czas_dojazdu"
      ),
      plotOutput("ch3_median_plot", height = "300px"),
      uiOutput("ch3_median_text")
    ),

    # ========================================================================
    # WIDGET 1: Mean vs Median -- comparison
    # ========================================================================
    div(class = "section-title", "\u015arednia vs mediana -- kiedy si\u0119 r\u00f3\u017cni\u0105?"),

    div(class = "narrative",
      p("Dla danych symetrycznych \u015brednia i mediana s\u0105 blisko siebie.
        Ale co si\u0119 dzieje, gdy rozk\u0142ad jest sko\u015bny lub pojawi si\u0119
        warto\u015b\u0107 odstaj\u0105ca?"),
      p("Wyobra\u017amy sobie zarobki w pewnej firmie. Wi\u0119kszo\u015b\u0107 pracownik\u00f3w
        zarabia umiarkowanie, ale s\u0105 te\u017c osoby z bardzo wysokimi pensjami.
        Zobaczmy, jak \u015brednia i mediana reaguj\u0105 na nowe warto\u015bci.")
    ),

    div(class = "widget-block",
      h4("Zarobki w firmie: \u015brednia vs mediana"),

      fluidRow(
        column(5,
          sliderInput("ch3_svm_new_value", "Nowa warto\u015b\u0107:",
                      min = 2000, max = 25000, value = 5000, step = 500,
                      pre = "", post = " z\u0142", width = "100%")
        ),
        column(7,
          div(style = "display: flex; gap: 8px; margin-top: 25px;",
            actionButton("ch3_svm_add", "Dodaj warto\u015b\u0107",
                         class = "btn-primary"),
            actionButton("ch3_svm_outlier", "Dodaj outlier (CEO)",
                         class = "btn-danger"),
            actionButton("ch3_svm_reset", "Reset",
                         class = "btn-default")
          )
        )
      ),

      hr(),

      plotOutput("ch3_svm_hist", height = "280px"),
      plotOutput("ch3_svm_strip", height = "120px"),

      div(style = "text-align: center; margin-top: 10px;",
        uiOutput("ch3_svm_stats")
      )
    ),

    div(class = "callout-info",
      tags$strong("Obserwacja:"),
      " Dodaj kilka 'normalnych' zarobk\u00f3w -- \u015brednia i mediana b\u0119d\u0105 blisko
        siebie. Teraz kliknij 'Dodaj outlier (CEO)' -- zobacz, jak \u015brednia
        skacze w g\u00f3r\u0119, a mediana prawie si\u0119 nie zmienia!"
    ),

    # ========================================================================
    # WIDGET 2: Robustness mini-demo
    # ========================================================================
    div(class = "section-title", "Odporno\u015b\u0107 miar na outliery"),

    div(class = "narrative",
      p("Kt\u00f3ra statystyka jest bardziej odporna na outliery? \u015arednia
        arytmetyczna bierze pod uwag\u0119 ka\u017cd\u0105 warto\u015b\u0107 -- wi\u0119c jedna
        ekstremalna obserwacja mo\u017ce j\u0105 znacz\u0105co przesun\u0105\u0107. Mediana
        ignoruje skrajne warto\u015bci, patrz\u0105c tylko na '\u015brodek' danych."),
      p("\u015arednia ucinana (trimmed mean) to kompromis: odrzuca pewien
        procent najbardziej skrajnych obserwacji z obu stron, a nast\u0119pnie
        oblicza \u015bredni\u0105 z pozosta\u0142ych. Dodajmy kilka ekstremalnych
        zarobk\u00f3w i zobaczmy, co si\u0119 stanie.")
    ),

    div(class = "widget-block",
      h4("Odporność: średnia vs mediana vs średnia ucinana"),

      div(style = "display: flex; gap: 8px; margin-bottom: 15px;",
        actionButton("ch3_rob_add1", "Dodaj outlier (+50 000 zl)",
                     class = "btn-warning"),
        actionButton("ch3_rob_add5", "Dodaj 5 outlierow",
                     class = "btn-danger"),
        actionButton("ch3_rob_reset", "Reset",
                     class = "btn-default")
      ),

      plotOutput("ch3_rob_plot", height = "320px"),

      div(style = "margin-top: 15px;",
        tableOutput("ch3_rob_table")
      )
    ),

    div(class = "callout-warning",
      tags$strong("Wniosek:"),
      " Średnia arytmetyczna jest bardzo wrażliwa na wartości odstające.
        Mediana jest najbardziej odporna. Średnia ucinana oferuje
        kompromis - jest mniej wrażliwa niż średnia, ale bardziej niż mediana.
        Dlatego przy skośnych rozkładach (np. zarobki) mediana jest często
        lepsza miara 'typowej' wartości."
    ),

    # ========================================================================
    # WIDGET 2b: Discrete variables
    # ========================================================================
    div(class = "section-title", "Zmienne dyskretne -- te same statystyki, inne wykresy"),

    div(class = "narrative",
      p("Dotychczas uzywalismy zmiennych ciągłych (wzrost, zarobki). Ale co ze
        zmiennymi ", tags$b("dyskretnymi"), " -- takimi jak liczba kursow czy
        liczba nieobecnosci? Statystyki polozenia (średnia, mediana) obliczamy
        tak samo, ale ", tags$b("wizualizacja"), " wymaga uwagi.")
    ),

    div(class = "widget-block",
      h4("Dyskretna vs ciągła -- porównanie wizualizacji"),
      selectInput("ch3_disc_var", "Wybierz zmienna dyskretna:",
        choices = c("Liczba nieobecności" = "liczba_nieobecnosci",
                    "Liczba kursów" = "liczba_kursow"),
        selected = "liczba_nieobecnosci"
      ),
      fluidRow(
        column(6,
          h5(style = "text-align: center; color: #27ae60;", "Wykres słupkowy (poprawny)"),
          plotOutput("ch3_disc_bar", height = "300px")
        ),
        column(6,
          h5(style = "text-align: center; color: #e74c3c;", "Histogram (problematyczny)"),
          plotOutput("ch3_disc_hist", height = "300px")
        )
      ),
      tableOutput("ch3_disc_stats"),
      uiOutput("ch3_disc_explanation")
    ),

    # ========================================================================
    # WIDGET 2c: Multimodality in continuous distributions
    # ========================================================================
    div(class = "section-title", "Modalno\u015b\u0107 rozk\u0142adu -- ile 'g\u00f3rek' ma histogram?"),

    div(class = "narrative",
      p("W rozdziale o zmiennych jako\u015bciowych poznali\u015bmy dominant\u0119 -- najcz\u0119stsz\u0105
        kategori\u0119. Dla danych ci\u0105g\u0142ych dominanta pojedynczej warto\u015bci nie ma sensu
        (prawie ka\u017cda warto\u015b\u0107 jest unikatowa). Ale poj\u0119cie ",
        tags$b("mody"), " dzia\u0142a na ", tags$b("przedzia\u0142ach"),
        " -- szukamy, kt\u00f3ry bin histogramu jest najwy\u017cszy."),
      p("Co wa\u017cniejsze, rozk\u0142ad mo\u017ce mie\u0107 ",
        tags$b("wi\u0119cej ni\u017c jeden szczyt"), " (mod\u0119). To cz\u0119sto
        sygna\u0142, \u017ce dane pochodz\u0105 z kilku r\u00f3\u017cnych grup.")
    ),

    div(class = "widget-block",
      h4("Unimodalny vs bimodalny vs wielomodalny"),
      radioButtons("ch3_modal_scenario", "Scenariusz:",
        choices = c(
          "Unimodalny -- wzrost kobiet" = "unimodal",
          "Bimodalny -- wzrost (kobiety + m\u0119\u017cczy\u017ani)" = "bimodal",
          "Wielomodalny -- czas dojazdu (autobus vs rower vs auto)" = "multimodal"
        ),
        selected = "unimodal"
      ),
      plotOutput("ch3_modal_plot", height = "350px"),
      uiOutput("ch3_modal_text")
    ),

    # ========================================================================
    # WIDGET 3: Percentile explorer
    # ========================================================================
    div(class = "section-title", "Percentyle i kwantyle"),

    div(class = "narrative",
      p("Kwantyle i percentyle dziela dane na czesci. Percentyl mówi nam,
        jaki procent obserwacji jest poniżej danej wartości. Na przykład
        percentyl 75. oznacza, ze 75% obserwacji ma wartość mniejsza
        lub rowna tej wartości."),
      p("Trzy najwazniejsze kwantyle to kwartyle:"),
      tags$ul(
        tags$li(tags$strong("Q1 (25. percentyl)"), " - pierwsza cwiartka danych"),
        tags$li(tags$strong("Q2 (50. percentyl)"), " - mediana, czyli środek"),
        tags$li(tags$strong("Q3 (75. percentyl)"), " - trzecia cwiartka danych")
      ),
      p("Przesuwaj suwak, aby zobaczyc rozne percentyle wzrostu studentow
        z naszej ankiety.")
    ),

    div(class = "widget-block",
      h4("Explorer percentyli: wzrost studentow"),

      fluidRow(
        column(6,
          sliderInput("ch3_q_pct", "Percentyl:",
                      min = 0, max = 100, value = 50, step = 1,
                      post = "%", width = "100%")
        ),
        column(6,
          div(style = "display: flex; gap: 8px; margin-top: 25px;",
            actionButton("ch3_q_q1", "Q1 (25%)", class = "btn-outline-primary"),
            actionButton("ch3_q_med", "Mediana (50%)", class = "btn-outline-primary"),
            actionButton("ch3_q_q3", "Q3 (75%)", class = "btn-outline-primary")
          )
        )
      ),

      hr(),

      plotOutput("ch3_q_hist", height = "280px"),
      plotOutput("ch3_q_box", height = "120px"),

      div(style = "text-align: center; margin-top: 10px;",
        uiOutput("ch3_q_text")
      )
    ),

    # ====================================================================
    # WIDGET 4: Guess the statistic game
    # ====================================================================
    div(class = "section-title", "Gra: Zgadnij średnia i mediane!"),

    div(class = "narrative",
      p("Sprawdzmy Twoją intuicję! Na histogramie zobaczysz rozkład danych.
        Kliknij na wykres, aby postawić swój typ: najpierw ", tags$b("średnia"),
        ", potem ", tags$b("mediana"), ". Czy potrafisz je odroznic?")
    ),

    div(class = "widget-block",
      h4("Kliknij na wykres, aby umie\u015bci\u0107 \u015bredni\u0105 i median\u0119"),
      div(style = "margin-bottom: 10px;",
        actionButton("ch3_game_new", "Nowa runda",
                     class = "btn-primary", style = "margin-right: 6px;"),
        actionButton("ch3_game_reveal", "Poka\u017c odpowied\u017a",
                     class = "btn-success", style = "margin-right: 6px;")
      ),
      uiOutput("ch3_game_status_banner"),
      plotOutput("ch3_game_plot", height = "350px", click = "ch3_game_click"),
      uiOutput("ch3_game_feedback")
    ),

    div(class = "callout-info",
      tags$strong("Rozstęp międzykwartylowy (IQR):"),
      " Roznica miedzy Q3 a Q1 to IQR - miara rozrzutu, która jest odporna
        na outliery. Boxplot (wykres pudełkowy) uzywa wlasnie kwartyli
        do wizualizacji rozkładu danych. Więcej o tym w kolejnym rozdziale!"
    ),

    div(class = "chapter-transition",
      p("Wiemy gdzie jest 'środek' danych. Ale dwie grupy z ta sama srednia
        mogą wyglądać zupełnie inaczej -- rozni je rozrzut. Jak go mierzyc i wizualizować?"),
      actionButton("ch3_next", "Dalej: 4. Statystyki rozrzutu \u2192",
                   class = "btn-primary btn-lg")
    ),

    # Spacer at bottom
    div(style = "height: 60px;")

  ))
  ), # end ch3 tabPanel

  # ==========================================================================
  # CHAPTER 4 UI
  # ==========================================================================
  tabPanel("4. Statystyki rozrzutu",
  fluidRow(column(8, offset = 2,

    # --- Introduction ---
    div(class = "chapter-recap",
      "Średnia, mediana, percentyle -- wiemy jak znalezc 'środek'. Ale środek to nie wszystko.
       Pora zmierzyc, jak bardzo dane są rozproszone wokol tego srodka."
    ),
    uiOutput("tracker_ch4"),
    div(class = "section-title", "Statystyki rozrzutu"),

    div(class = "narrative",
      p("Średnia mówi gdzie jest środek, ale nic o tym jak bardzo dane sa
        rozproszone. Dwie grupy mogą mieć te sama średnia, a wyglądać
        zupełnie inaczej."),
      p("W tym rozdziale poznamy miary rozrzutu: odchylenie standardowe,
        wariancje, rozstęp, rozstęp międzykwartylowy (IQR) oraz
        współczynnik zmienności. Nauczymy sie tez budowac boxplot od podstaw.")
    ),

    # ====================================================================
    # WIDGET 1: Bus scenario - "Mean is not everything"
    # ====================================================================
    div(class = "section-title", "\u015arednia to nie wszystko"),

    div(class = "narrative",
      p("Wyobra\u017a sobie dwie linie autobusowe. Obie maj\u0105 takie samo
        \u015brednie sp\u00f3\u017anienie -- oko\u0142o 2 minuty. Kt\u00f3r\u0105 wybierzesz?"),
      p("Wi\u0119kszo\u015b\u0107 autobus\u00f3w jest blisko rozk\u0142adu (0-4 min sp\u00f3\u017anienia),
        rzadko kt\u00f3ry przyje\u017cd\u017ca za wcze\u015bnie, a od czasu do czasu
        zdarza si\u0119 du\u017ce sp\u00f3\u017anienie. Ale rozrzut tych sp\u00f3\u017anie\u0144
        mo\u017ce by\u0107 bardzo r\u00f3\u017cny.")
    ),

    div(class = "widget-block",
      div(class = "step-buttons",
        actionButton("ch4_spread_s1", "1. Dwie linie",
                     class = "btn-outline-primary"),
        actionButton("ch4_spread_s2", "2. Ta sama \u015brednia, ale...",
                     class = "btn-outline-primary"),
        actionButton("ch4_spread_s3", "3. Wychodzisz wcze\u015bniej",
                     class = "btn-outline-primary"),
        actionButton("ch4_spread_s4", "4. Konsekwencje",
                     class = "btn-outline-primary")
      ),
      sliderInput("ch4_spread_buffer", "Wychodzisz wcze\u015bniej o (minuty):",
                  min = 0, max = 10, value = 0, step = 1, width = "100%"),
      plotOutput("ch4_spread_plot", height = "450px"),
      uiOutput("ch4_spread_text")
    ),

    # ====================================================================
    # WIDGET 2: SD step-by-step
    # ====================================================================
    div(class = "section-title", "Odchylenie standardowe krok po kroku"),

    div(class = "narrative",
      p("Jak obliczamy odchylenie standardowe? Krok po kroku.
        Zobaczmy to na przykładzie 10 pomiarów wzrostu.")
    ),

    div(class = "widget-block",
      div(class = "step-buttons",
        actionButton("ch4_sd_s1", "1. Dane",
                     class = "btn-outline-primary"),
        actionButton("ch4_sd_s2", "2. Odchylenia od średniej",
                     class = "btn-outline-primary"),
        actionButton("ch4_sd_s3", "3. Wariancja i SD",
                     class = "btn-outline-primary")
      ),
      div(style = "margin-bottom: 10px;",
        actionButton("ch4_sd_new", "Losuj nowy zestaw",
                     class = "btn-success btn-sm", style = "margin-right: 6px;"),
        actionButton("ch4_sd_reset", "Reset",
                     class = "btn-secondary btn-sm")
      ),
      plotOutput("ch4_sd_plot", height = "400px"),
      tableOutput("ch4_sd_table"),
      uiOutput("ch4_sd_text")
    ),

    # ====================================================================
    # WIDGET 2b: Empirical rule (68-95-99.7)
    # ====================================================================
    div(class = "section-title", "Regula empiryczna (68-95-99.7)"),

    div(class = "narrative",
      p("Wiemy juz jak obliczyć odchylenie standardowe. Ale co ono oznacza
        w praktyce? Dla rozkładow zbliżonych do normalnego obowiązuje ",
        tags$b("regula empiryczna"), ": okolo 68% danych miesci sie w zakresie
        średnia \u00B11 SD, 95% w \u00B12 SD, a 99.7% w \u00B13 SD.")
    ),

    div(class = "widget-block",
      h4("Regula 68-95-99.7 -- czy zawsze dziala?"),
      selectInput("ch4_emp_var", "Wybierz zmienna:",
        choices = c("Wzrost (cm)" = "wzrost",
                    "Waga (kg)" = "waga",
                    "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      plotOutput("ch4_emp_plot", height = "400px"),
      uiOutput("ch4_emp_text")
    ),

    # ====================================================================
    # WIDGET 3: Boxplot builder
    # ====================================================================
    div(class = "section-title", "Budujemy boxplot od podstaw"),

    div(class = "narrative",
      p("Boxplot to wizualne podsumowanie rozkładu oparte na kwartylach.
        Zbudujmy go od podstaw, krok po kroku, aby zrozumieć co
        oznacza każdy element tego wykresu.")
    ),

    div(class = "widget-block",
      div(class = "step-buttons",
        actionButton("ch4_bp_s1", "1. Surowe dane",
                     class = "btn-outline-primary"),
        actionButton("ch4_bp_s2", "2. Mediana",
                     class = "btn-outline-primary"),
        actionButton("ch4_bp_s3", "3. Kwartyle i pudełko",
                     class = "btn-outline-primary"),
        actionButton("ch4_bp_s4", "4. Wąsy i outliers",
                     class = "btn-outline-primary"),
        actionButton("ch4_bp_s5", "5. Gotowy boxplot",
                     class = "btn-outline-primary")
      ),
      div(style = "margin-bottom: 10px;",
        actionButton("ch4_bp_new", "Losuj nowe dane",
                     class = "btn-success btn-sm", style = "margin-right: 6px;"),
        actionButton("ch4_bp_reset", "Reset",
                     class = "btn-secondary btn-sm")
      ),
      plotOutput("ch4_bp_plot", height = "350px"),
      uiOutput("ch4_bp_text")
    ),

    # ====================================================================
    # WIDGET 3b: Group comparison -- side-by-side boxplots
    # ====================================================================
    div(class = "section-title", "Porównanie grup"),

    div(class = "narrative",
      p("Dotychczas analizowalismy caly zbior danych naraz. Ale jednym z
        najczestszych pytan w statystyce jest: ", tags$b("czy grupy sie roznia?"),
        " Boxploty obok siebie to doskonałe narzędzie do porównywania rozkładow
        miedzy grupami.")
    ),

    div(class = "widget-block",
      h4("Boxploty grupowane"),
      fluidRow(
        column(4,
          selectInput("ch4_grp_var", "Zmienna ilościowa:",
            choices = c("Wzrost (cm)" = "wzrost",
                        "Waga (kg)" = "waga",
                        "Czas dojazdu (min)" = "czas_dojazdu",
                        "Średnia ocen" = "srednia_ocen"),
            selected = "wzrost"
          )
        ),
        column(4,
          selectInput("ch4_grp_by", "Grupuj wg:",
            choices = c("Płeć" = "plec",
                        "Kierunek" = "kierunek"),
            selected = "plec"
          )
        ),
        column(4,
          checkboxInput("ch4_grp_violin", "Pokaz violin plot", value = FALSE),
          checkboxInput("ch4_grp_points", "Pokaz punkty", value = TRUE)
        )
      ),
      plotOutput("ch4_grp_plot", height = "400px"),
      tableOutput("ch4_grp_table")
    ),

    # ====================================================================
    # WIDGET 4: Spread measures comparison
    # ====================================================================
    div(class = "section-title", "Porównanie miar rozrzutu"),

    div(class = "narrative",
      p("Porównajmy rozne miary rozrzutu i ich odporność na wartości
        odstające. Dodaj outliera i obserwuj, ktore miary sie zmieniaja,
        a ktore pozostaja stabilne.")
    ),

    div(class = "widget-block",
      div(style = "margin-bottom: 10px;",
        actionButton("ch4_comp_add1", "Dodaj outlier (+30 cm)",
                     class = "btn-warning", style = "margin-right: 6px;"),
        actionButton("ch4_comp_add5", "Dodaj 5 outlierow",
                     class = "btn-danger", style = "margin-right: 6px;"),
        actionButton("ch4_comp_reset", "Reset",
                     class = "btn-secondary")
      ),
      plotOutput("ch4_comp_plot", height = "350px"),
      tableOutput("ch4_comp_table")
    ),

    div(class = "callout-info",
      tags$strong("Wniosek:"),
      " Rozstęp jest bardzo wrażliwy na outlierow - wystarczy jedna wartosc
      odstająca, aby go zmienić. IQR i odchylenie standardowe są bardziej
      odporne, a IQR jest z nich najbardziej stabilne."
    ),

    # ====================================================================
    # WIDGET 5: Coefficient of Variation
    # ====================================================================
    div(class = "section-title", "Współczynnik zmienności (CV)"),

    div(class = "narrative",
      p("Odchylenie standardowe mówi o rozrzucie, ale w jakich jednostkach?
        SD wzrostu (w cm) i SD wagi (w kg) nie są porownywalne!
        Aby porownac zmiennosc zmiennych w roznych skalach, uzywamy ",
        tags$b("współczynnika zmienności"), " (CV = SD / średnia \u00D7 100%).")
    ),

    div(class = "widget-block",
      h4("Porównanie zmienności miedzy zmiennymi"),
      fluidRow(
        column(6, plotOutput("ch4_sd_compare_plot", height = "350px")),
        column(6, plotOutput("ch4_cv_plot", height = "350px"))
      ),
      tableOutput("ch4_cv_table"),
      div(class = "callout-info",
        tags$strong("Interpretacja: "),
        "Lewy wykres pokazuje SD w oryginalnych jednostkach -- wartości są nieporównywalne,
         bo każda zmienna ma inna skalę. Prawy wykres pokazuje CV (%), które normalizuje
         rozrzut wzgledem średniej -- teraz widać, że ", tags$b("czas dojazdu"),
        " ma największa względna zmienność, choć jego SD nie jest największe."
      )
    ),

    div(class = "chapter-transition",
      p("Położenie i rozrzut to nie wszystko. Dwa rozkłady z ta sama średnia i
        odchyleniem standardowym mogą mieć zupełnie inny kształt -- asymetrię
        i różna 'ciężkość' ogonów."),
      actionButton("ch4_next", "Dalej: 5. Kształt rozkładu \u2192",
                   class = "btn-primary btn-lg")
    ),

    # Bottom spacing
    div(style = "height: 40px;")

  ))
  ), # end ch4 tabPanel

  # ==========================================================================
  # CHAPTER 5 UI
  # ==========================================================================
  tabPanel("5. Kształt rozkładu",
  fluidRow(column(8, offset = 2,

    # --- Introduction ---
    div(class = "chapter-recap",
      "Znamy juz położenie i rozrzut. Ostatni element układanki: jaki kształt ma rozkład?
       Czy jest symetryczny, czy moze ma 'długi ogon' w jedna strone?"
    ),
    uiOutput("tracker_ch5"),
    div(class = "section-title", h2("5. Kształt rozkładu")),

    div(class = "narrative",
      p("Dwa rozkłady mogą mieć ta sama średnia i odchylenie standardowe,
        a wyglądać zupełnie inaczej. Kształt rozkładu mówi nam o asymetrii
        i 'ciężkości' ogonów."),
      p("W tym rozdziale poznasz dwie miary kształtu:",
        tags$strong("skośność"), "(asymetria) i",
        tags$strong("kurtoze"), "(ciężkość ogonów).")
    ),

    # --- Widget 1: Skewness ---
    div(class = "section-title", h3("5.1 Skośność (asymetria)")),

    div(class = "narrative",
      p("Skośność mierzy asymetrię rozkładu. Prosta regula: jeśli srednia
        jest większa od mediany, rozkład jest prawostronnie skośny.")
    ),

    div(class = "widget-block",
      selectInput("ch5_skew_var", "Wybierz zmienna:",
        choices = c(
          "Wzrost" = "wzrost",
          "Czas dojazdu" = "czas_dojazdu",
          "Średnia ocen" = "srednia_ocen",
          "Liczba nieobecności" = "liczba_nieobecnosci"
        ),
        selected = "czas_dojazdu"
      ),
      plotOutput("ch5_skew_plot", height = "350px"),
      uiOutput("ch5_skew_info")
    ),

    # --- Widget 2: Kurtosis ---
    div(class = "section-title", h3("5.2 Kurtoza (ci\u0119\u017cko\u015b\u0107 ogon\u00f3w)")),

    div(class = "narrative",
      p("Kurtoza mierzy, jak 'ci\u0119\u017ckie' s\u0105 ogony rozk\u0142adu \u2014 czyli
        jak cz\u0119sto pojawiaj\u0105 si\u0119 warto\u015bci ekstremalne. Nie chodzi
        o 'sp\u0142aszczenie' szczytu, lecz o to, ile obserwacji le\u017cy daleko
        od \u015bredniej.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnaj rozk\u0142ady o r\u00f3\u017cnej kurtozie"),
      fluidRow(
        column(8,
          sliderInput("ch5_kurt_val", "Nadwy\u017ckowa kurtoza:",
            min = -1.2, max = 6, value = 0, step = 0.2
          )
        ),
        column(4,
          div(style = "margin-top: 25px; display: flex; gap: 4px; flex-wrap: wrap;",
            actionButton("ch5_kurt_platy", "Platykurtyczny",
                         class = "btn-outline-primary btn-sm"),
            actionButton("ch5_kurt_mezo", "Mezokurtyczny",
                         class = "btn-outline-primary btn-sm"),
            actionButton("ch5_kurt_lepto", "Leptokurtyczny",
                         class = "btn-outline-primary btn-sm")
          )
        )
      ),
      plotOutput("ch5_kurt_plot", height = "350px"),
      plotOutput("ch5_kurt_tails", height = "220px"),
      uiOutput("ch5_kurt_text")
    ),

    # --- Widget 3: Full picture ---
    div(class = "section-title", h3("5.3 Pelny obraz")),

    div(class = "narrative",
      p("Na koniec - pelny obraz. Dla każdej zmiennej ilościowej mozemy
        opisać jej położenie, rozrzut i kształt.")
    ),

    div(class = "widget-block",
      selectInput("ch5_full_var", "Wybierz zmienna:",
        choices = c(
          "Wzrost" = "wzrost",
          "Średnia ocen" = "srednia_ocen",
          "Czas dojazdu" = "czas_dojazdu",
          "Waga" = "waga"
        ),
        selected = "wzrost"
      ),
      plotOutput("ch5_full_hist", height = "350px"),
      plotOutput("ch5_full_box", height = "120px"),
      tableOutput("ch5_full_table"),
      uiOutput("ch5_full_interpretation")
    ),

    # --- Transition to ch6 ---
    div(class = "chapter-transition",
      p("Teraz potrafisz opisac rozk\u0142ad zmiennej ilo\u015bciowej w trzech wymiarach:
        po\u0142o\u017cenie, rozrzut i kszta\u0142t. Czas na podsumowanie -- \u015bci\u0105ga
        ze wszystkimi narz\u0119dziami w jednym miejscu."),
      actionButton("ch5_next", "Dalej: 6. \u015aci\u0105ga \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
  ), # end ch5 tabPanel

  # ==========================================================================
  # CHAPTER 6 UI: Cheat sheet
  # ==========================================================================
  tabPanel("6. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "section-title", "Podsumowanie -- narz\u0119dzia statystyki opisowej"),

    div(class = "narrative",
      p("Poni\u017cej kompletna \u015bci\u0105ga ze wszystkimi poj\u0119ciami i narz\u0119dziami
        om\u00f3wionymi w trakcie wyk\u0142adu. Warto j\u0105 mie\u0107 pod r\u0119k\u0105 podczas analiz.")
    ),

    # --- Cheat sheet 1: Tools by variable type ---
    div(class = "section-title", "Narz\u0119dzia wg typu zmiennej"),

    div(class = "widget-block",
      tableOutput("ch6_ref_table")
    ),

    div(class = "callout-danger",
      tags$strong("Najcz\u0119stszy b\u0142\u0105d:"),
      " Obliczanie \u015bredniej z danych nominalnych lub porz\u0105dkowych
      (np. \u015brednia z kod\u00f3w kierunk\u00f3w). Wynik b\u0119dzie liczb\u0105, ale
      nie b\u0119dzie mia\u0142 \u017cadnego sensu!"
    ),

    # --- Cheat sheet 2: Measures summary ---
    div(class = "section-title", "Miary statystyczne -- kiedy co stosowa\u0107"),

    div(class = "widget-block",
      h4("Miary po\u0142o\u017cenia (rozdzia\u0142 3)"),
      tableOutput("ch6_location_table"),
      hr(),
      h4("Miary rozrzutu (rozdzia\u0142 4)"),
      tableOutput("ch6_spread_table"),
      hr(),
      h4("Miary kszta\u0142tu (rozdzia\u0142 5)"),
      tableOutput("ch6_shape_table")
    ),

    # --- Cheat sheet 3: Visualization guide ---
    div(class = "section-title", "Przewodnik po wykresach"),

    div(class = "widget-block",
      tableOutput("ch6_plots_table")
    ),

    # --- Cheat sheet 4: Common mistakes ---
    div(class = "section-title", "Typowe b\u0142\u0119dy"),

    div(class = "widget-block",
      tableOutput("ch6_mistakes_table")
    ),

    # --- Formulas ---
    div(class = "section-title", "Wzory"),

    div(class = "widget-block",
      withMathJax(
        h4("Miary po\u0142o\u017cenia"),
        helpText("$$\\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i$$"),
        p("Mediana: warto\u015b\u0107 \u015brodkowa po posortowaniu danych"),
        hr(),
        h4("Miary rozrzutu"),
        helpText("$$s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2$$"),
        helpText("$$s = \\sqrt{s^2}$$"),
        helpText("$$CV = \\frac{s}{\\bar{x}} \\cdot 100\\%$$"),
        helpText("$$IQR = Q_3 - Q_1$$"),
        hr(),
        h4("Miary kszta\u0142tu"),
        helpText("$$\\text{Sko\u015bno\u015b\u0107} = \\frac{n}{(n-1)(n-2)} \\sum \\left(\\frac{x_i - \\bar{x}}{s}\\right)^3$$"),
        helpText("$$\\text{Kurtoza (excess)} = \\frac{n(n+1)}{(n-1)(n-2)(n-3)} \\sum \\left(\\frac{x_i - \\bar{x}}{s}\\right)^4 - \\frac{3(n-1)^2}{(n-2)(n-3)}$$")
      )
    ),

    br(), br()
  ))
  ) # end ch6 tabPanel

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

    skew_word <- if (abs(sk) < 0.3) "symetryczny" else if (sk > 0) "skośny prawo" else "skośny lewo"

    div(class = "tracker-panel",
      tags$strong(paste0("\U0001F50D Sledzona zmienna: ", label)),
      " | x\u0304 = ", round(mean(vals), 2),
      ", SD = ", round(sd(vals), 2),
      " | Kształt: skośność = ", sk, " (", skew_word, "), kurtoza = ", ku
    )
  })

  # ==========================================================================
  # CHAPTER 1 SERVER
  # ==========================================================================

  ch1_revealed <- reactiveVal(character(0))

  # --- Widget 1: Taxonomy tree ---

  output$ch1_taxonomy_plot <- renderPlot({
    render_taxonomy(highlight = NULL, revealed = ch1_revealed())
  })

  observeEvent(input$ch1_taxonomy_click, {
    click <- input$ch1_taxonomy_click
    if (is.null(click)) return()

    leaf_nodes <- data.frame(
      id = c("ciagla", "dyskretna", "porzadkowa", "nominalna"),
      x = c(1.25, 3.75, 6.25, 8.75),
      y = c(1, 1, 1, 1),
      stringsAsFactors = FALSE
    )

    distances <- sqrt((leaf_nodes$x - click$x)^2 + (leaf_nodes$y - click$y)^2)
    nearest_idx <- which.min(distances)

    if (distances[nearest_idx] < 1.5) {
      nearest_id <- leaf_nodes$id[nearest_idx]
      current <- ch1_revealed()
      if (nearest_id %in% current) {
        ch1_revealed(setdiff(current, nearest_id))
      } else {
        ch1_revealed(c(current, nearest_id))
      }
    }
  })

  observeEvent(input$ch1_reveal_all, {
    ch1_revealed(c("ciagla", "dyskretna", "porzadkowa", "nominalna"))
  })

  observeEvent(input$ch1_hide_all, {
    ch1_revealed(character(0))
  })

  # --- Widget 2: Examples gallery ---

  output$ch1_ex1_plot <- renderPlot({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$plec, "Płeć", "nominalna")
    } else {
      render_good_plot(student_data$plec, "Płeć", "nominalna")
    }
  })

  output$ch1_ex2_plot <- renderPlot({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$zadowolenie, "Zadowolenie", "porzadkowa")
    } else {
      render_good_plot(student_data$zadowolenie, "Zadowolenie", "porzadkowa")
    }
  })

  output$ch1_ex3_plot <- renderPlot({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$liczba_kursow, "Liczba kursów", "ilosciowa_dyskretna")
    } else {
      render_good_plot(student_data$liczba_kursow, "Liczba kursów", "ilosciowa_dyskretna")
    }
  })

  output$ch1_ex4_plot <- renderPlot({
    if (input$ch1_show_bad) {
      render_bad_plot(student_data$wzrost, "Wzrost (cm)", "ilosciowa_ciagla")
    } else {
      render_good_plot(student_data$wzrost, "Wzrost (cm)", "ilosciowa_ciagla")
    }
  })

  # --- Widget 3: Reference table (moved to Chapter 6) ---

  # --- Widget 4: Dataset preview ---

  output$ch1_data_preview <- renderTable({
    head(student_data, 10)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  # ==========================================================================
  # CHAPTER 2 SERVER
  # ==========================================================================

  ch2_freq_step <- reactiveVal(0)
  ch2_scenario_idx <- reactiveVal(1)
  ch2_random_colors <- reactiveVal(NULL)
  ch2_mode_data <- reactiveVal(NULL)

  # --- Initialise reactive values that need data ---
  observe({
    if (is.null(ch2_mode_data())) {
      ch2_mode_data(student_data$kierunek)
    }
  })

  # ========================================================================
  # Widget 1: Frequency table step-by-step
  # ========================================================================

  observeEvent(input$ch2_freq_s1, { ch2_freq_step(1) })
  observeEvent(input$ch2_freq_s2, { ch2_freq_step(2) })
  observeEvent(input$ch2_freq_s3, { ch2_freq_step(3) })
  observeEvent(input$ch2_freq_s4, { ch2_freq_step(4) })
  observeEvent(input$ch2_freq_reset, { ch2_freq_step(0) })
  observeEvent(input$ch2_freq_var, { ch2_freq_step(0) })

  output$ch2_freq_explanation <- renderUI({
    step <- ch2_freq_step()
    var_name <- input$ch2_freq_var
    is_ord <- (!is.null(var_name) && var_name == "zadowolenie")
    var_label <- if (is_ord) "zadowolenie" else "kierunek"

    if (step == 0) {
      div(class = "callout-info",
          "Kliknij kolejne przyciski, aby zbudowa\u0107 tabel\u0119 cz\u0119sto\u015bci krok po kroku.")
    } else if (step == 1) {
      div(class = "callout-info",
          tags$b("Krok 1: Surowe dane. "),
          "Tak wygl\u0105daj\u0105 pierwsze obserwacje zmiennej ",
          tags$code(var_label), ". Ka\u017cdy wiersz to odpowied\u017a jednego studenta.",
          if (is_ord) tagList(
            tags$br(),
            tags$em("Uwaga: kategorie maj\u0105 naturaln\u0105 kolejno\u015b\u0107 -- od
                    'Bardzo niezadowolony' do 'Bardzo zadowolony'.")
          )
      )
    } else if (step == 2) {
      div(class = "callout-info",
          tags$b("Krok 2: Zliczanie. "),
          "Liczymy, ile razy wyst\u0119puje ka\u017cda kategoria. To s\u0105 ",
          tags$b("cz\u0119sto\u015bci bezwzgl\u0119dne"), " (liczebno\u015bci).",
          if (is_ord) tagList(
            tags$br(),
            tags$em("Kategorie s\u0105 uporz\u0105dkowane -- ich kolejno\u015b\u0107 w tabeli
                    ma znaczenie.")
          )
      )
    } else if (step == 3) {
      div(class = "callout-info",
          tags$b("Krok 3: Cz\u0119sto\u015bci wzgl\u0119dne. "),
          "Dzielimy ka\u017cd\u0105 liczebno\u015b\u0107 przez ca\u0142kowit\u0105 liczb\u0119 obserwacji (n = ",
          nrow(student_data), "). Wynik mo\u017cemy wyrazi\u0107 jako u\u0142amek lub procent.")
    } else if (step == 4) {
      if (is_ord) {
        div(class = "callout-success",
          tags$b("Krok 4: Cz\u0119sto\u015bci skumulowane. "),
          "Sumujemy cz\u0119sto\u015bci narastaj\u0105co. ",
          tags$b("Dla zmiennej porz\u0105dkowej to ma g\u0142\u0119boki sens!"),
          tags$br(), tags$br(),
          "Mo\u017cemy powiedzie\u0107 np.: ",
          tags$em("'X% student\u00f3w jest neutralnych lub bardziej zadowolonych'"),
          " albo ",
          tags$em("'Y% student\u00f3w jest niezadowolonych lub bardzo niezadowolonych'"),
          ".",
          tags$br(), tags$br(),
          "Skumulowany procent daje sensown\u0105 interpretacj\u0119 ",
          tags$b("tylko wtedy, gdy kategorie maj\u0105 naturaln\u0105 kolejno\u015b\u0107."))
      } else {
        div(class = "callout-warning",
          tags$b("Krok 4: Cz\u0119sto\u015bci skumulowane. "),
          "Sumujemy cz\u0119sto\u015bci narastaj\u0105co. ",
          tags$b("Ale uwaga!"), " Dla zmiennej ",
          tags$b("nominalnej"), " kolejno\u015b\u0107 kategorii jest umowna.",
          tags$br(), tags$br(),
          "Stwierdzenie '72% student\u00f3w studiuje Informatyk\u0119 lub wcze\u015bniej'
           nie ma sensu -- bo co znaczy 'wcze\u015bniej' w li\u015bcie kierunk\u00f3w?",
          tags$br(), tags$br(),
          tags$em("Prze\u0142\u0105cz na zmienn\u0105 porz\u0105dkow\u0105 (Zadowolenie), \u017ceby
                  zobaczy\u0107, kiedy skumulowany procent jest naprawd\u0119 przydatny."))
      }
    }
  })

  output$ch2_freq_table <- renderTable({
    step <- ch2_freq_step()
    if (step == 0) return(NULL)

    var_name <- input$ch2_freq_var
    is_ord <- (!is.null(var_name) && var_name == "zadowolenie")
    x <- if (is_ord) student_data$zadowolenie else student_data$kierunek
    col_label <- if (is_ord) "Zadowolenie" else "Kierunek"

    if (step == 1) {
      sample_vals <- head(x, 20)
      df <- data.frame(Nr = 1:20, V = as.character(sample_vals))
      names(df) <- c("Nr", col_label)
      return(df)
    }

    counts <- table(x)
    df <- data.frame(
      Kategoria = names(counts),
      Liczebnosc = as.integer(counts)
    )
    names(df) <- c("Kategoria", "Liczebno\u015b\u0107")

    if (step >= 3) {
      df[["Cz\u0119st. wzgl\u0119dna"]] <- round(df[["Liczebno\u015b\u0107"]] / sum(df[["Liczebno\u015b\u0107"]]), 3)
      df[["Procent (%)"]] <- round(df[["Cz\u0119st. wzgl\u0119dna"]] * 100, 1)
    }

    if (step >= 4) {
      df[["Skumul. liczebno\u015b\u0107"]] <- cumsum(df[["Liczebno\u015b\u0107"]])
      df[["Skumul. procent (%)"]] <- round(cumsum(df[["Cz\u0119st. wzgl\u0119dna"]]) * 100, 1)
    }

    df
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")


  # ========================================================================
  # Widget 1b: Nominal vs Ordinal comparison
  # ========================================================================

  output$ch2_ord_nom_plot <- renderPlot({
    df <- data.frame(kierunek = student_data$kierunek)
    lvls <- levels(df$kierunek)
    if (isTRUE(input$ch2_ord_shuffle)) {
      lvls <- sample(lvls)
    }
    df$kierunek <- factor(df$kierunek, levels = lvls)
    ggplot(df, aes(x = kierunek)) +
      geom_bar(fill = col_nominal, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Kierunek", y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  output$ch2_ord_ord_plot <- renderPlot({
    df <- data.frame(zadowolenie = student_data$zadowolenie)
    lvls <- levels(df$zadowolenie)
    if (isTRUE(input$ch2_ord_shuffle)) {
      lvls <- sample(lvls)
    }
    df$zadowolenie <- factor(df$zadowolenie, levels = lvls)
    ggplot(df, aes(x = zadowolenie)) +
      geom_bar(fill = col_ordinal, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)),
                vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
      labs(x = "Zadowolenie", y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  output$ch2_ord_explanation <- renderUI({
    if (isTRUE(input$ch2_ord_shuffle)) {
      div(class = "callout-warning",
        tags$strong("Losowa kolejność: "),
        "Dla ", tags$b("kierunku studiow"), " (zmienna nominalna) zmiana kolejnośći
         nie zmienia interpretacji -- kategorie nie maja naturalnego porzadku.
         Ale dla ", tags$b("zadowolenia"), " (zmienna porządkowa) losowa kolejność
         jest mylaca! Tracimy informacje o naturalnym porzadku od 'bardzo niezadowolony'
         do 'bardzo zadowolony'."
      )
    } else {
      div(class = "callout-info",
        tags$strong("Domyslna kolejność: "),
        "Kierunek studiów pokazujemy w kolejnośći alfabetycznej (umownej) --
         moglibymy uzyc dowolnej innej. Zadowolenie natomiast ma naturalny
         porzadek: od 'bardzo niezadowolony' do 'bardzo zadowolony'. ",
        tags$em("Wlacz 'Losowa kolejność', zeby zobaczyc różnice!")
      )
    }
  })

  # ========================================================================
  # Widget 2: Pie vs Bar -- scenario comparison (Chart.js)
  # ========================================================================

  observeEvent(input$ch2_sc1, { ch2_scenario_idx(1) })
  observeEvent(input$ch2_sc2, { ch2_scenario_idx(2) })
  observeEvent(input$ch2_sc3, { ch2_scenario_idx(3) })

  ch2_current_scenario <- reactive({
    pie_vs_bar_scenarios[[ch2_scenario_idx()]]
  })

  # Send scenario data to Chart.js via custom message
  observe({
    s <- ch2_current_scenario()
    session$sendCustomMessage("render_scenario", list(
      labels = as.list(s$labels),
      data   = as.list(s$data),
      colors = as.list(s$colors)
    ))
  })

  output$ch2_scenario_pie_verdict <- renderUI({
    s <- ch2_current_scenario()
    badge_style <- if (s$pie_ok) "background: #EAF3DE; color: #3B6D11;" else
                                 "background: #FCEBEB; color: #A32D2D;"
    badge_text  <- if (s$pie_ok) "OK" else "Problem"
    div(style = "text-align: center; font-size: 13px; color: #5f5e5a; margin-top: 6px;",
      tags$span(style = paste0("display: inline-block; font-size: 11px; padding: 2px 8px;
                                 border-radius: 6px; font-weight: 500; margin-right: 4px; ",
                                badge_style), badge_text),
      s$pie_verdict
    )
  })

  output$ch2_scenario_bar_verdict <- renderUI({
    s <- ch2_current_scenario()
    div(style = "text-align: center; font-size: 13px; color: #5f5e5a; margin-top: 6px;",
      tags$span(style = "display: inline-block; font-size: 11px; padding: 2px 8px;
                         border-radius: 6px; font-weight: 500; margin-right: 4px;
                         background: #EAF3DE; color: #3B6D11;", "OK"),
      s$bar_verdict
    )
  })

  output$ch2_scenario_legend <- renderUI({
    s <- ch2_current_scenario()
    legend_items <- mapply(function(label, color, value) {
      tags$span(style = "display: flex; align-items: center; gap: 4px;",
        tags$span(style = paste0("width: 10px; height: 10px; border-radius: 2px;
                                   flex-shrink: 0; background: ", color, ";")),
        paste0(label, " ", value, "%")
      )
    }, s$labels, s$colors, s$data, SIMPLIFY = FALSE)
    tagList(legend_items)
  })

  # ========================================================================
  # Widget 4: Color manipulation demo
  # ========================================================================

  observeEvent(input$ch2_color_random, {
    # Paleta o gwarantowanym kontra\u015bcie na bia\u0142ym tle
    safe_colors <- c(
      "#e6194B", "#3cb44b", "#4363d8", "#f58231", "#911eb4",
      "#42d4f4", "#f032e6", "#bfef45", "#fabed4", "#469990",
      "#dcbeff", "#9A6324", "#800000", "#aaffc3", "#808000",
      "#000075", "#a9a9a9", "#e6beff", "#ffd8b1", "#fffac8"
    )
    ch2_random_colors(sample(safe_colors, 4))
  })

  # Reset random colors when palette selector changes
  observeEvent(input$ch2_color_palette, {
    ch2_random_colors(NULL)
  })

  output$ch2_color_plot <- renderPlot({
    df <- data.frame(kierunek = student_data$kierunek)
    df_counts <- as.data.frame(table(df$kierunek))
    names(df_counts) <- c("Kierunek", "n")

    levels_order <- levels(student_data$kierunek)
    if (is.null(levels_order)) levels_order <- unique(as.character(student_data$kierunek))

    rand_cols <- ch2_random_colors()
    palette_choice <- input$ch2_color_palette

    if (!is.null(rand_cols)) {
      fill_colors <- setNames(rand_cols, levels_order)
      subtitle <- "Losowa paleta kolorow"
    } else if (palette_choice == "neutral") {
      fill_colors <- setNames(rep("#95a5a6", 4), levels_order)
      subtitle <- "Neutralna - wszystkie kategorie rowne"
    } else if (palette_choice == "warm") {
      fill_colors <- setNames(
        ifelse(levels_order == "Informatyka", "#e74c3c", "#d5d8dc"),
        levels_order
      )
      subtitle <- "Ciepla paleta - uwaga przyciagana do Informatyki"
    } else if (palette_choice == "cool") {
      fill_colors <- setNames(
        ifelse(levels_order == "Biologia", "#2980b9", "#d5d8dc"),
        levels_order
      )
      subtitle <- "Zimna paleta - uwaga przyciagana do Biologii"
    } else if (palette_choice == "biased") {
      biggest <- df_counts$Kierunek[which.max(df_counts$n)]
      smallest <- df_counts$Kierunek[which.min(df_counts$n)]
      cols <- setNames(rep("#bdc3c7", 4), levels_order)
      cols[as.character(biggest)]  <- "#e74c3c"
      cols[as.character(smallest)] <- "#2c3e50"
      fill_colors <- cols
      subtitle <- paste0("Stronnicza - ", biggest,
                         " wyr\u00f3\u017cniona, ", smallest, " wyciszona")
    } else if (palette_choice == "viridis") {
      fill_colors <- setNames(
        c("#440154", "#31688e", "#35b779", "#fde725")[1:length(levels_order)],
        levels_order)
      subtitle <- "Viridis -- percepcyjnie r\u00f3wnomierna, colorblind-safe"
    } else if (palette_choice == "set2") {
      fill_colors <- setNames(
        c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")[1:length(levels_order)],
        levels_order)
      subtitle <- "Set2 (ColorBrewer) -- popularny domy\u015blny wyb\u00f3r"
    } else if (palette_choice == "okabe_ito") {
      fill_colors <- setNames(
        c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")[1:length(levels_order)],
        levels_order)
      subtitle <- "Okabe-Ito -- zaprojektowana specjalnie dla daltonist\u00f3w"
    } else if (palette_choice == "tableau") {
      fill_colors <- setNames(
        c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")[1:length(levels_order)],
        levels_order)
      subtitle <- "Tableau 10 -- standard w wizualizacji danych"
    } else {
      fill_colors <- setNames(rep("#95a5a6", length(levels_order)), levels_order)
      subtitle <- ""
    }

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = Kierunek)) +
      geom_col(color = "white", width = 0.7) +
      geom_text(aes(label = n), vjust = -0.5, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(values = fill_colors, guide = "none") +
      labs(title = "Kierunek studiów",
           subtitle = subtitle,
           x = "Kierunek", y = "Liczebność") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(color = "#7f8c8d", face = "italic"))
  })


  # ========================================================================
  # Widget 4b: Cross-tabulation

  output$ch2_cross_table <- renderTable({
    row_var <- input$ch2_cross_row
    col_var <- input$ch2_cross_col
    req(row_var, col_var, row_var != col_var)

    tbl <- table(student_data[[row_var]], student_data[[col_var]])

    if (input$ch2_cross_type == "counts") {
      df <- as.data.frame.matrix(tbl)
      df <- cbind(data.frame(` ` = rownames(df), check.names = FALSE), df)
    } else if (input$ch2_cross_type == "row_pct") {
      pct <- round(prop.table(tbl, margin = 1) * 100, 1)
      df <- as.data.frame.matrix(pct)
      df[] <- lapply(df, function(x) paste0(x, "%"))
      df <- cbind(data.frame(` ` = rownames(df), check.names = FALSE), df)
    } else {
      pct <- round(prop.table(tbl, margin = 2) * 100, 1)
      df <- as.data.frame.matrix(pct)
      df[] <- lapply(df, function(x) paste0(x, "%"))
      df <- cbind(data.frame(` ` = rownames(df), check.names = FALSE), df)
    }
    df
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  output$ch2_cross_plot <- renderPlot({
    row_var <- input$ch2_cross_row
    col_var <- input$ch2_cross_col
    chart_type <- input$ch2_cross_chart
    req(row_var, col_var, row_var != col_var)

    df <- data.frame(
      row = student_data[[row_var]],
      col = student_data[[col_var]]
    )

    row_label <- c("plec" = "P\u0142e\u0107", "kierunek" = "Kierunek", "grupa_krwi" = "Grupa krwi")
    col_label <- row_label

    if (!is.null(chart_type) && chart_type == "heatmap") {
      # Heatmap (geom_tile)
      tbl <- table(df$row, df$col)
      if (input$ch2_cross_type == "row_pct") {
        tbl <- prop.table(tbl, margin = 1) * 100
        fill_label <- "% wierszowy"
        fmt <- function(x) paste0(round(x, 1), "%")
      } else if (input$ch2_cross_type == "col_pct") {
        tbl <- prop.table(tbl, margin = 2) * 100
        fill_label <- "% kolumnowy"
        fmt <- function(x) paste0(round(x, 1), "%")
      } else {
        fill_label <- "Liczebno\u015b\u0107"
        fmt <- function(x) as.character(x)
      }
      heat_df <- as.data.frame(as.table(tbl))
      names(heat_df) <- c("Wiersz", "Kolumna", "Wartosc")

      ggplot(heat_df, aes(x = Kolumna, y = Wiersz, fill = Wartosc)) +
        geom_tile(color = "white", linewidth = 1.5) +
        geom_text(aes(label = fmt(Wartosc)), size = 5, fontface = "bold") +
        scale_fill_gradient(low = "#eaf2f8", high = "#2980b9", name = fill_label) +
        labs(x = col_label[col_var], y = row_label[row_var]) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid = element_blank(),
          axis.text = element_text(size = 12)
        )
    } else {
      # Grouped bar chart
      ggplot(df, aes(x = row, fill = col)) +
        geom_bar(position = "dodge", alpha = 0.85, color = "white") +
        scale_fill_brewer(palette = "Set2") +
        labs(x = row_label[row_var], y = "Liczebno\u015b\u0107", fill = col_label[col_var]) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    }
  })

  # Widget 5: Mode (dominanta)
  # ========================================================================

  observeEvent(input$ch2_mode_resample, {
    probs <- runif(4)
    probs <- probs / sum(probs)
    new_data <- sample(
      c("Informatyka", "Biologia", "Psychologia", "Ekonomia"),
      200, replace = TRUE, prob = probs
    )
    ch2_mode_data(factor(new_data,
      levels = c("Informatyka", "Biologia", "Psychologia", "Ekonomia")))
  })

  output$ch2_mode_plot <- renderPlot({
    req(ch2_mode_data())
    x <- ch2_mode_data()
    df_counts <- as.data.frame(table(x))
    names(df_counts) <- c("Kierunek", "n")
    mode_cat <- df_counts$Kierunek[which.max(df_counts$n)]

    df_counts$is_mode <- ifelse(df_counts$Kierunek == mode_cat,
                                "Dominanta", "Inne")

    ggplot(df_counts, aes(x = Kierunek, y = n, fill = is_mode)) +
      geom_col(color = "white", width = 0.7, alpha = 0.9) +
      geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      scale_fill_manual(
        values = c("Dominanta" = col_nominal, "Inne" = "#d5d8dc"),
        guide = "none"
      ) +
      labs(title = "Kierunek studiów - dominanta",
           x = "Kierunek", y = "Liczebność") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$ch2_mode_text <- renderUI({
    req(ch2_mode_data())
    x <- ch2_mode_data()
    counts <- table(x)
    mode_cat <- names(counts)[which.max(counts)]
    mode_n   <- max(counts)
    total_n  <- sum(counts)
    mode_pct <- round(mode_n / total_n * 100, 1)

    div(class = "callout-info",
      tags$b("Dominanta: "), mode_cat,
      tags$br(),
      paste0("Wystepuje ", mode_n, " razy (", mode_pct, "% z ", total_n,
             " obserwacji)."),
      tags$br(),
      tags$em("Dla zmiennych nominalnych dominanta to jedyna sensowna miara
              tendencji centralnej - nie mozemy obliczyć średniej ani mediany
              z nazw kategorii.")
    )
  })

  # ==========================================================================
  # CHAPTER 3 SERVER
  # ==========================================================================

  # --------------------------------------------------------------------------
  # Widget: Histogram krok po kroku
  # --------------------------------------------------------------------------

  ch3_hist_step <- reactiveVal(0)

  observeEvent(input$ch3_hist_var, { ch3_hist_step(0) })
  observeEvent(input$ch3_hist_reset, { ch3_hist_step(0) })
  observeEvent(input$ch3_hist_step1, { ch3_hist_step(1) })
  observeEvent(input$ch3_hist_step2, { ch3_hist_step(2) })
  observeEvent(input$ch3_hist_step3, { ch3_hist_step(3) })
  observeEvent(input$ch3_hist_step4, { ch3_hist_step(4) })
  observeEvent(input$ch3_hist_step5, { ch3_hist_step(5) })
  observeEvent(input$ch3_hist_step6, { ch3_hist_step(6) })
  observeEvent(input$ch3_hist_step7, { ch3_hist_step(7) })
  observeEvent(input$ch3_hist_step8, { ch3_hist_step(8) })

  # Default bin widths per variable
  ch3_hist_defaults <- list(
    wzrost = list(min = 1, max = 15, value = 3, step = 1, unit = "cm"),
    waga = list(min = 2, max = 20, value = 5, step = 1, unit = "kg"),
    czas_dojazdu = list(min = 2, max = 20, value = 5, step = 1, unit = "min"),
    srednia_ocen = list(min = 0.1, max = 1, value = 0.3, step = 0.05, unit = "pkt")
  )

  output$ch3_hist_bin_slider <- renderUI({
    d <- ch3_hist_defaults[[input$ch3_hist_var]]
    sliderInput("ch3_hist_bin_width",
                "Szeroko\u015b\u0107 binu:",
                min = d$min, max = d$max,
                value = d$value, step = d$step)
  })

  # Compute bin breaks
  ch3_hist_breaks <- reactive({
    req(input$ch3_hist_bin_width)
    x <- student_data[[input$ch3_hist_var]]
    w <- input$ch3_hist_bin_width
    start <- floor(min(x) / w) * w
    end <- ceiling(max(x) / w) * w + w
    seq(start, end, by = w)
  })

  # Data with bin assignments
  ch3_hist_binned <- reactive({
    req(input$ch3_hist_bin_width)
    x <- student_data[[input$ch3_hist_var]]
    breaks <- ch3_hist_breaks()
    df <- data.frame(value = x)
    df$bin <- cut(df$value, breaks = breaks, include.lowest = TRUE, right = FALSE)
    df$bin_num <- as.numeric(df$bin)
    df
  })

  # Bin statistics
  ch3_hist_stats <- reactive({
    df <- ch3_hist_binned()
    breaks <- ch3_hist_breaks()
    all_bins <- data.frame(
      bin_start = breaks[-length(breaks)],
      bin_end = breaks[-1]
    )
    all_bins$bin_mid <- (all_bins$bin_start + all_bins$bin_end) / 2
    all_bins$bin_num <- seq_len(nrow(all_bins))

    counts <- df %>%
      filter(!is.na(bin)) %>%
      group_by(bin_num) %>%
      summarise(count = n(), .groups = "drop")
    all_bins <- all_bins %>% left_join(counts, by = "bin_num")
    all_bins$count[is.na(all_bins$count)] <- 0

    # Trim to relevant range
    min_d <- min(all_bins$bin_num[all_bins$count > 0])
    max_d <- max(all_bins$bin_num[all_bins$count > 0])
    all_bins %>%
      filter(bin_num >= max(1, min_d - 1),
             bin_num <= min(nrow(all_bins), max_d + 1))
  })

  # Variable labels
  ch3_hist_var_labels <- c(
    "wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
    "czas_dojazdu" = "Czas dojazdu (min)",
    "srednia_ocen" = "\u015arednia ocen"
  )

  output$ch3_hist_plot <- renderPlot({
    step <- ch3_hist_step()
    var_name <- input$ch3_hist_var
    req(var_name)
    x <- student_data[[var_name]]
    x_label <- ch3_hist_var_labels[var_name]
    n <- length(x)

    x_lo <- min(x) - diff(range(x)) * 0.05
    x_hi <- max(x) + diff(range(x)) * 0.05

    strip_theme <- theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    if (step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij Krok 1", size = 6, color = "gray50") +
        theme_void() + xlim(0, 1) + ylim(0, 1)

    } else if (step == 1) {
      df <- data.frame(value = x)
      ggplot(df, aes(x = value, y = 0)) +
        geom_jitter(height = 0.3, size = 3, alpha = 0.6, color = "#3498db") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 2) {
      df <- data.frame(value = sort(x))
      ggplot(df, aes(x = value, y = 0)) +
        geom_point(size = 3, alpha = 0.7, color = "#27ae60") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 3) {
      breaks <- ch3_hist_breaks()
      df <- data.frame(value = sort(x))
      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)], xmax = breaks[-1]
      ) %>% filter(xmax > x_lo, xmin < x_hi)

      ggplot() +
        geom_rect(data = bin_rects,
                  aes(xmin = xmin, xmax = xmax, ymin = -0.35, ymax = 0.35),
                  fill = NA, color = "#2c3e50", linewidth = 0.8,
                  linetype = "dashed") +
        geom_point(data = df, aes(x = value, y = 0),
                   size = 2.5, alpha = 0.5, color = "#95a5a6") +
        geom_text(data = bin_rects,
                  aes(x = (xmin + xmax) / 2, y = -0.45,
                      label = paste0("[", xmin, ", ", xmax, ")")),
                  size = 2.8, color = "#2c3e50") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.55, 0.5))

    } else if (step == 4) {
      df <- ch3_hist_binned()
      breaks <- ch3_hist_breaks()
      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)], xmax = breaks[-1],
        bin_num = seq_len(length(breaks) - 1)
      ) %>% filter(xmax > x_lo, xmin < x_hi)

      ggplot() +
        geom_rect(data = bin_rects,
                  aes(xmin = xmin, xmax = xmax, ymin = -0.35, ymax = 0.35,
                      fill = factor(bin_num)),
                  alpha = 0.15, color = "#2c3e50", linewidth = 0.5) +
        geom_jitter(data = df %>% filter(!is.na(bin)),
                    aes(x = value, y = 0, color = factor(bin_num)),
                    height = 0.2, size = 3, alpha = 0.8) +
        scale_fill_viridis_d(guide = "none") +
        scale_color_viridis_d(guide = "none") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 5) {
      df <- ch3_hist_binned()
      stats <- ch3_hist_stats()

      ggplot() +
        geom_rect(data = stats,
                  aes(xmin = bin_start, xmax = bin_end,
                      ymin = -0.35, ymax = 0.35,
                      fill = factor(bin_num)),
                  alpha = 0.15, color = "#2c3e50", linewidth = 0.5) +
        geom_jitter(data = df %>% filter(!is.na(bin)),
                    aes(x = value, y = 0, color = factor(bin_num)),
                    height = 0.2, size = 2, alpha = 0.6) +
        geom_text(data = stats,
                  aes(x = bin_mid, y = 0.45,
                      label = ifelse(count > 0, paste0("n=", count), "")),
                  size = 4, fontface = "bold", color = "#2c3e50") +
        scale_fill_viridis_d(guide = "none") +
        scale_color_viridis_d(guide = "none") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.6))

    } else if (step == 6) {
      stats <- ch3_hist_stats()
      w <- input$ch3_hist_bin_width

      ggplot(stats, aes(x = bin_mid, y = count)) +
        geom_col(aes(fill = factor(bin_num)),
                 width = w * 0.95, alpha = 0.7,
                 color = "#2c3e50", linewidth = 0.3) +
        geom_text(aes(label = count), vjust = -0.5, size = 4, fontface = "bold") +
        scale_fill_viridis_d(guide = "none") +
        labs(x = x_label, y = "Liczba obserwacji") +
        theme_minimal(base_size = 14) +
        coord_cartesian(xlim = c(x_lo, x_hi))

    } else if (step == 7) {
      df <- data.frame(value = x)
      w <- input$ch3_hist_bin_width

      ggplot(df, aes(x = value)) +
        geom_histogram(binwidth = w, fill = "#3498db", alpha = 0.7,
                       color = "#2c3e50", linewidth = 0.3) +
        labs(x = x_label, y = "Liczba obserwacji",
             title = paste0("Histogram: ", x_label),
             subtitle = paste0("n = ", n, " | szeroko\u015b\u0107 binu = ", w)) +
        theme_minimal(base_size = 14)

    } else if (step == 8) {
      df <- data.frame(value = x)
      w <- input$ch3_hist_bin_width
      widths <- c(w / 2, w, w * 2)
      unit <- ch3_hist_defaults[[var_name]]$unit
      labels <- paste0("Bin = ", widths, " ", unit)

      plots <- lapply(seq_along(widths), function(i) {
        ggplot(df, aes(x = value)) +
          geom_histogram(binwidth = widths[i],
                         fill = c("#e74c3c", "#3498db", "#27ae60")[i],
                         alpha = 0.7, color = "#2c3e50", linewidth = 0.3) +
          labs(x = if (i == 2) x_label else "",
               y = if (i == 1) "Liczba obs." else "",
               title = labels[i]) +
          theme_minimal(base_size = 11) +
          theme(plot.title = element_text(
            size = 12, face = "bold",
            color = c("#e74c3c", "#3498db", "#27ae60")[i]))
      })
      gridExtra::grid.arrange(grobs = plots, ncol = 3)
    }
  })

  output$ch3_hist_text <- renderUI({
    step <- ch3_hist_step()
    var_name <- input$ch3_hist_var
    req(var_name)
    x <- student_data[[var_name]]
    n <- length(x)
    unit <- ch3_hist_defaults[[var_name]]$unit

    txt <- switch(as.character(step),
      "0" = "Kliknij Krok 1, aby rozpocz\u0105\u0107 budow\u0119 histogramu.",
      "1" = paste0("Mamy ", n, " obserwacji \u2014 ka\u017cdy punkt to jedna warto\u015b\u0107. ",
                   "Trudno z tego odczyta\u0107 rozk\u0142ad, prawda?"),
      "2" = paste0("Sortujemy od min = ", round(min(x), 1),
                   " do max = ", round(max(x), 1), " ", unit,
                   ". Wida\u0107 zag\u0119szczenia, ale wci\u0105\u017c nieczytelne."),
      "3" = paste0("Dzielimy o\u015b na r\u00f3wne przedzia\u0142y (biny) o szeroko\u015bci ",
                   input$ch3_hist_bin_width, " ", unit,
                   ". Ka\u017cdy bin to 'koszyk' na obserwacje."),
      "4" = "Ka\u017cda obserwacja trafia do swojego binu \u2014 kolor = przynale\u017cno\u015b\u0107.",
      "5" = "Liczymy obserwacje w ka\u017cdym binie. Te liczby stan\u0105 si\u0119 wysoko\u015bci\u0105 s\u0142upk\u00f3w.",
      "6" = "Zamieniamy punkty na s\u0142upki \u2014 wysoko\u015b\u0107 = liczba obserwacji. To ju\u017c prawie histogram!",
      "7" = paste0("Gotowy histogram (n = ", n, ", bin = ", input$ch3_hist_bin_width,
                   " ", unit, "). Spr\u00f3buj zmieni\u0107 szeroko\u015b\u0107 binu suwakiem!"),
      "8" = paste0("Te same dane z trzema szeroko\u015bciami binu. ",
                   "Za w\u0105skie \u2192 szum. Za szerokie \u2192 utrata szczeg\u00f3\u0142\u00f3w.")
    )
    div(class = "callout-info", p(txt))
  })

  output$ch3_hist_table <- renderTable({
    if (ch3_hist_step() < 5) return(NULL)
    stats <- ch3_hist_stats()
    n <- length(student_data[[input$ch3_hist_var]])

    result <- stats %>% filter(count > 0) %>%
      mutate(pct = round(count / n * 100, 1))
    out <- data.frame(
      a = paste0("[", result$bin_start, ", ", result$bin_end, ")"),
      b = result$count,
      c = paste0(result$pct, "%")
    )
    names(out) <- c("Przedzia\u0142", "Liczba obs.", "Procent")
    out
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # --------------------------------------------------------------------------
  # Widget 0a: Mean introduction
  # --------------------------------------------------------------------------

  output$ch3_mean_plot <- renderPlot({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    var_labels <- c("wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
                    "srednia_ocen" = "\u015arednia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = "#d5d8dc", color = "white", alpha = 0.8) +
      geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.5, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("\u015arednia = ", round(m, 2)),
               vjust = 2, hjust = -0.1, color = "#e74c3c", size = 5, fontface = "bold") +
      annotate("segment", x = min(x), xend = m, y = -0.5, yend = -0.5,
               color = "#3498db", linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      annotate("segment", x = max(x), xend = m, y = -0.5, yend = -0.5,
               color = "#3498db", linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      labs(x = var_labels[var_name], y = "Liczebno\u015b\u0107",
           title = "\u015arednia jako punkt r\u00f3wnowagi") +
      theme_minimal(base_size = 14)
  })

  output$ch3_mean_text <- renderUI({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    s <- sum(x)
    n <- length(x)
    div(class = "callout-info",
      withMathJax(paste0(
        "$$\\bar{x} = \\frac{", round(s, 1), "}{", n, "} = ", round(m, 2), "$$"
      )),
      tags$em("\u015arednia uwzgl\u0119dnia ka\u017cd\u0105 warto\u015b\u0107 -- jest wra\u017cliwa
              na warto\u015bci skrajne, bo przeci\u0105ga j\u0105 w ich stron\u0119.")
    )
  })

  # --------------------------------------------------------------------------
  # Widget 0b: Median introduction
  # --------------------------------------------------------------------------

  output$ch3_median_plot <- renderPlot({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    x_sorted <- sort(x)
    n <- length(x_sorted)
    n_below <- sum(x_sorted < med)
    n_above <- sum(x_sorted > med)
    var_labels <- c("wzrost" = "Wzrost (cm)", "czas_dojazdu" = "Czas dojazdu (min)",
                    "srednia_ocen" = "\u015arednia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = "#d5d8dc", color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = "#2980b9", linewidth = 1.5) +
      annotate("rect", xmin = min(x) - 1, xmax = med, ymin = -Inf, ymax = Inf,
               fill = "#3498db", alpha = 0.08) +
      annotate("rect", xmin = med, xmax = max(x) + 1, ymin = -Inf, ymax = Inf,
               fill = "#e74c3c", alpha = 0.08) +
      annotate("text", x = (min(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_below, " obs.)"),
               vjust = 2, color = "#2c3e50", size = 5, fontface = "bold") +
      annotate("text", x = (max(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_above, " obs.)"),
               vjust = 2, color = "#2c3e50", size = 5, fontface = "bold") +
      annotate("text", x = med, y = Inf, label = paste0("Me = ", round(med, 1)),
               vjust = 4, hjust = -0.1, color = "#2980b9", size = 5, fontface = "bold") +
      geom_histogram(bins = 25, fill = "#d5d8dc", color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = "#2980b9", linewidth = 1.5) +
      labs(x = var_labels[var_name], y = "Liczebno\u015b\u0107",
           title = "Mediana dzieli dane na dwie r\u00f3wne po\u0142owy") +
      theme_minimal(base_size = 14)
  })

  output$ch3_median_text <- renderUI({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    m <- mean(x)
    diff <- abs(m - med)
    skew_dir <- if (m > med) "prawostronna (d\u0142ugi ogon w prawo)" else
                if (m < med) "lewostronna (d\u0142ugi ogon w lewo)" else "symetryczny"

    div(class = "callout-info",
      tags$b("Mediana = ", round(med, 1)),
      " | \u015arednia = ", round(m, 2),
      " | R\u00f3\u017cnica = ", round(diff, 2),
      tags$br(), tags$br(),
      if (diff < 0.5) {
        tags$em("Mediana \u2248 \u015brednia -- rozk\u0142ad jest zbli\u017cony do symetrycznego.")
      } else {
        tags$em(paste0("Mediana \u2260 \u015brednia -- sko\u015bno\u015b\u0107 ", skew_dir,
                       ". Mediana lepiej opisuje 'typow\u0105' warto\u015b\u0107."))
      }
    )
  })

  # --------------------------------------------------------------------------
  # Widget 1: Mean vs Median comparison
  # --------------------------------------------------------------------------

  ch3_svm_generate <- function() {
    round(rgamma(30, shape = 3, scale = 1500) + 2000)
  }

  ch3_svm_data <- reactiveVal(NULL)

  observe({
    if (is.null(ch3_svm_data())) {
      set.seed(NULL)
      ch3_svm_data(ch3_svm_generate())
    }
  })

  observeEvent(input$ch3_svm_add, {
    ch3_svm_data(c(ch3_svm_data(), input$ch3_svm_new_value))
  })

  observeEvent(input$ch3_svm_outlier, {
    ch3_svm_data(c(ch3_svm_data(), 50000))
  })

  observeEvent(input$ch3_svm_reset, {
    set.seed(NULL)
    ch3_svm_data(ch3_svm_generate())
  })

  output$ch3_svm_hist <- renderPlot({
    req(ch3_svm_data())
    d <- data.frame(x = ch3_svm_data())
    m <- mean(d$x)
    med <- median(d$x)

    ggplot(d, aes(x = x)) +
      geom_histogram(fill = "#bdc3c7", color = "white", bins = 25) +
      geom_vline(aes(xintercept = m, color = "Srednia"),
                 linewidth = 1.2, linetype = "solid") +
      geom_vline(aes(xintercept = med, color = "Mediana"),
                 linewidth = 1.2, linetype = "dashed") +
      scale_color_manual(
        name = NULL,
        breaks = c("Srednia", "Mediana"),
        values = c("Srednia" = "#e74c3c", "Mediana" = "#3498db")
      ) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zl)", y = "Liczba osob",
           title = "Rozkład zarobkow") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"))
  })

  output$ch3_svm_strip <- renderPlot({
    req(ch3_svm_data())
    d <- data.frame(x = ch3_svm_data())
    m <- mean(d$x)
    med <- median(d$x)

    ggplot(d, aes(x = x, y = 0)) +
      geom_jitter(height = 0.3, width = 0, size = 2.5,
                  alpha = 0.6, color = "#2c3e50") +
      geom_point(aes(x = m), y = 0, color = "#e74c3c",
                 size = 5, shape = 18) +
      geom_point(aes(x = med), y = 0, color = "#3498db",
                 size = 5, shape = 18) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zl)", y = NULL) +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })

  output$ch3_svm_stats <- renderUI({
    req(ch3_svm_data())
    d <- ch3_svm_data()
    m <- mean(d)
    med <- median(d)
    diff_val <- m - med

    diff_color <- if (abs(diff_val) < 500) "#27ae60" else "#f39c12"

    tagList(
      div(class = "stat-box", style = "background: #e74c3c;",
          paste0("Srednia: ", format(round(m), big.mark = " "), " zl")),
      div(class = "stat-box", style = "background: #3498db;",
          paste0("Mediana: ", format(round(med), big.mark = " "), " zl")),
      div(class = "stat-box", style = paste0("background: ", diff_color, ";"),
          paste0("Roznica: ", format(round(diff_val), big.mark = " "), " zl"))
    )
  })

  # --------------------------------------------------------------------------
  # Widget 2: Robustness mini-demo
  # --------------------------------------------------------------------------

  ch3_rob_generate <- function() {
    round(rgamma(40, shape = 3, scale = 1500) + 2000)
  }

  ch3_rob_base <- reactiveVal(NULL)
  ch3_rob_outliers <- reactiveVal(numeric(0))

  observe({
    if (is.null(ch3_rob_base())) {
      set.seed(NULL)
      ch3_rob_base(ch3_rob_generate())
    }
  })

  ch3_rob_all <- reactive({
    c(ch3_rob_base(), ch3_rob_outliers())
  })

  # Store baseline stats for comparison
  ch3_rob_base_stats <- reactive({
    req(ch3_rob_base())
    d <- ch3_rob_base()
    list(
      mean = mean(d),
      median = median(d),
      trimmed = mean(d, trim = 0.1)
    )
  })

  observeEvent(input$ch3_rob_add1, {
    new_outlier <- 50000 + runif(1, -5000, 5000)
    ch3_rob_outliers(c(ch3_rob_outliers(), round(new_outlier)))
  })

  observeEvent(input$ch3_rob_add5, {
    new_outliers <- round(50000 + runif(5, -5000, 5000))
    ch3_rob_outliers(c(ch3_rob_outliers(), new_outliers))
  })

  observeEvent(input$ch3_rob_reset, {
    set.seed(NULL)
    ch3_rob_base(ch3_rob_generate())
    ch3_rob_outliers(numeric(0))
  })

  output$ch3_rob_plot <- renderPlot({
    req(ch3_rob_all())
    d <- data.frame(x = ch3_rob_all())
    m <- mean(d$x)
    med <- median(d$x)
    tr <- mean(d$x, trim = 0.1)

    line_data <- data.frame(
      xval = c(m, med, tr),
      Statystyka = factor(
        c("Srednia", "Mediana", "Sr. ucinana (10%)"),
        levels = c("Srednia", "Mediana", "Sr. ucinana (10%)")
      ),
      ltype = c("solid", "dashed", "dotted")
    )

    n_outliers <- length(ch3_rob_outliers())
    subtitle_text <- if (n_outliers == 0) {
      "Brak outlierow"
    } else {
      paste0("Liczba dodanych outlierow: ", n_outliers)
    }

    ggplot(d, aes(x = x)) +
      geom_histogram(fill = "#bdc3c7", color = "white", bins = 30) +
      geom_vline(data = line_data,
                 aes(xintercept = xval, color = Statystyka,
                     linetype = Statystyka),
                 linewidth = 1.2) +
      scale_color_manual(
        name = NULL,
        breaks = c("Srednia", "Mediana", "Sr. ucinana (10%)"),
        values = c("Srednia" = "#e74c3c",
                   "Mediana" = "#3498db",
                   "Sr. ucinana (10%)" = "#27ae60")
      ) +
      scale_linetype_manual(
        name = NULL,
        breaks = c("Srednia", "Mediana", "Sr. ucinana (10%)"),
        values = c("Srednia" = "solid",
                   "Mediana" = "dashed",
                   "Sr. ucinana (10%)" = "dotted")
      ) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zl)", y = "Liczba osob",
           title = "Porównanie miar polozenia",
           subtitle = subtitle_text) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"))
  })

  output$ch3_rob_table <- renderTable({
    req(ch3_rob_all())
    req(ch3_rob_base_stats())

    d <- ch3_rob_all()
    base <- ch3_rob_base_stats()

    current_mean <- mean(d)
    current_med <- median(d)
    current_tr <- mean(d, trim = 0.1)

    data.frame(
      Statystyka = c("Srednia", "Mediana", "Średnia ucinana (10%)"),
      Wartość = paste0(
        format(round(c(current_mean, current_med, current_tr)),
               big.mark = " "), " zl"),
      `Zmiana vs bazowa` = paste0(
        ifelse(c(current_mean - base$mean,
                 current_med - base$median,
                 current_tr - base$trimmed) >= 0, "+", ""),
        format(round(c(current_mean - base$mean,
                       current_med - base$median,
                       current_tr - base$trimmed)),
               big.mark = " "), " zl"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lcr")

  # --------------------------------------------------------------------------
  # Widget 2b: Discrete variables

  output$ch3_disc_bar <- renderPlot({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]
    df <- data.frame(x = factor(vals))

    ggplot(df, aes(x = x)) +
      geom_bar(fill = col_discrete, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  output$ch3_disc_hist <- renderPlot({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(bins = 15, fill = "#e74c3c", color = "white", alpha = 0.6) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  output$ch3_disc_stats <- renderTable({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    mode_val <- as.numeric(names(sort(table(vals), decreasing = TRUE))[1])

    data.frame(
      Statystyka = c("Srednia", "Mediana", "Dominanta (moda)", "SD", "Rozstep"),
      Wartość = c(round(mean(vals), 2), median(vals), mode_val,
                  round(sd(vals), 2), paste0(min(vals), " - ", max(vals))),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  output$ch3_disc_explanation <- renderUI({
    div(class = "callout-info",
      tags$strong("Dlaczego wykres słupkowy jest lepszy? "),
      "Zmienna dyskretna przyjmuje skończenie wiele wartości calkowitych.
       Wykres słupkowy pokazuje każdą wartość osobno i poprawnie oddaje liczebnośći.
       Histogram natomiast grupuje dane w 'kubly' (bins), co moze niepoprawnie
       rozbic lub polaczyc wartości całkowite. ",
      tags$em("Statystyki (średnia, mediana, SD) liczymy tak samo jak dla zmiennych ciągłych.")
    )
  })

  # Widget 2c: Multimodality in continuous distributions
  # --------------------------------------------------------------------------

  output$ch3_modal_plot <- renderPlot({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    set.seed(42)
    if (scenario == "unimodal") {
      x <- rnorm(500, mean = 165, sd = 6)
      df <- data.frame(val = x)
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = "#3498db", color = "white", alpha = 0.7) +
        geom_density(linewidth = 1.2, color = "#2c3e50") +
        geom_vline(xintercept = mean(x), color = "#e74c3c", linewidth = 1, linetype = "dashed") +
        annotate("text", x = mean(x) + 1, y = Inf, label = "moda \u2248 \u015brednia \u2248 mediana",
                 hjust = 0, vjust = 2, color = "#e74c3c", size = 4.5, fontface = "bold") +
        labs(title = "Unimodalny -- jeden wyra\u017any szczyt",
             x = "Wzrost kobiet (cm)", y = "G\u0119sto\u015b\u0107") +
        theme_minimal(base_size = 14)

    } else if (scenario == "bimodal") {
      x_k <- rnorm(250, mean = 162, sd = 5)
      x_m <- rnorm(250, mean = 182, sd = 5)
      df <- data.frame(val = c(x_k, x_m),
                       grupa = rep(c("Kobiety", "M\u0119\u017cczy\u017ani"), each = 250))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 35,
                       fill = "#95a5a6", color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = "#2c3e50") +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Kobiety" = "#e74c3c", "M\u0119\u017cczy\u017ani" = "#3498db")) +
        labs(title = "Bimodalny -- dwa szczyty (dwie grupy!)",
             x = "Wzrost (cm)", y = "G\u0119sto\u015b\u0107", color = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")

    } else {
      x1 <- rnorm(150, mean = 12, sd = 3)
      x2 <- rnorm(120, mean = 25, sd = 4)
      x3 <- rnorm(130, mean = 40, sd = 5)
      df <- data.frame(val = c(x1, x2, x3),
                       grupa = c(rep("Rower", 150), rep("Autobus", 120), rep("Auto", 130)))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 40,
                       fill = "#95a5a6", color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = "#2c3e50") +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Rower" = "#27ae60", "Autobus" = "#f39c12", "Auto" = "#e74c3c")) +
        labs(title = "Wielomodalny -- trzy szczyty (trzy \u015brodki transportu)",
             x = "Czas dojazdu (min)", y = "G\u0119sto\u015b\u0107", color = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    }
  })

  output$ch3_modal_text <- renderUI({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    if (scenario == "unimodal") {
      div(class = "callout-info",
        tags$b("Rozk\u0142ad unimodalny: "), "jeden szczyt, jedna 'g\u00f3rka'. ",
        "Dla rozk\u0142adu symetrycznego moda \u2248 \u015brednia \u2248 mediana. ",
        "Wi\u0119kszo\u015b\u0107 statystyk opisowych zak\u0142ada w\u0142a\u015bnie taki rozk\u0142ad.")
    } else if (scenario == "bimodal") {
      div(class = "callout-warning",
        tags$b("Rozk\u0142ad bimodalny: "), "dwa szczyty! ",
        "To sygna\u0142, \u017ce dane prawdopodobnie pochodz\u0105 z ",
        tags$b("dw\u00f3ch r\u00f3\u017cnych grup"), ". ",
        "Podawanie jednej \u015bredniej dla ca\u0142o\u015bci jest mylace -- ",
        "\u015brednia wyl\u0105duje mi\u0119dzy szczytami, gdzie prawie nikt nie jest!",
        tags$br(), tags$br(),
        tags$em("Praktyka: rozdziel grupy i analizuj osobno."))
    } else {
      div(class = "callout-warning",
        tags$b("Rozk\u0142ad wielomodalny: "), "trzy szczyty = trzy podgrupy. ",
        "Ka\u017cda podgrupa (rowerzy\u015bci, pasa\u017cerowie autobus\u00f3w, kierowcy) ",
        "ma w\u0142asn\u0105 'typow\u0105' warto\u015b\u0107. ",
        tags$br(), tags$br(),
        tags$em("Wielomodalno\u015b\u0107 to jeden z najwa\u017cniejszych sygna\u0142\u00f3w w danych -- ",
                "m\u00f3wi, \u017ce patrzenie na ca\u0142o\u015b\u0107 bez podzia\u0142u na grupy ",
                "mo\u017ce prowadzi\u0107 do b\u0142\u0119dnych wniosk\u00f3w."))
    }
  })

  # Widget 3: Percentile explorer
  # --------------------------------------------------------------------------

  # Quick-select buttons
  observeEvent(input$ch3_q_q1, {
    updateSliderInput(session, "ch3_q_pct", value = 25)
  })

  observeEvent(input$ch3_q_med, {
    updateSliderInput(session, "ch3_q_pct", value = 50)
  })

  observeEvent(input$ch3_q_q3, {
    updateSliderInput(session, "ch3_q_pct", value = 75)
  })

  output$ch3_q_hist <- renderPlot({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- quantile(wzrost, probs = pct)

    d <- data.frame(x = wzrost)
    d$below <- d$x <= q_val

    ggplot(d, aes(x = x)) +
      geom_histogram(aes(fill = below), color = "white", bins = 25,
                     boundary = q_val, show.legend = FALSE) +
      geom_vline(xintercept = q_val, color = "#2c3e50",
                 linewidth = 1.2, linetype = "solid") +
      annotate("text", x = q_val, y = Inf,
               label = paste0(round(q_val, 1), " cm"),
               vjust = -0.5, hjust = -0.1,
               fontface = "bold", size = 5, color = "#2c3e50") +
      scale_fill_manual(values = c("TRUE" = "#3498db", "FALSE" = "#bdc3c7")) +
      labs(x = "Wzrost (cm)", y = "Liczba studentow",
           title = paste0(input$ch3_q_pct, ". percentyl wzrostu studentow")) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$ch3_q_box <- renderPlot({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- quantile(wzrost, probs = pct)

    d <- data.frame(x = wzrost)

    ggplot(d, aes(x = x, y = 0)) +
      geom_boxplot(fill = "#ecf0f1", color = "#2c3e50",
                   width = 0.5, outlier.alpha = 0.4) +
      geom_point(aes(x = q_val), y = 0,
                 color = "#e74c3c", size = 5, shape = 18) +
      annotate("text", x = q_val, y = 0.35,
               label = paste0("P", input$ch3_q_pct),
               fontface = "bold", size = 4.5, color = "#e74c3c") +
      labs(x = "Wzrost (cm)", y = NULL) +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })

  output$ch3_q_text <- renderUI({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- round(quantile(wzrost, probs = pct), 1)

    actual_pct <- round(100 * mean(wzrost <= q_val), 1)

    div(style = "font-size: 18px; color: #2c3e50; padding: 10px;",
      tags$strong(paste0(input$ch3_q_pct, "% studentow")),
      paste0(" ma wzrost poniżej ", q_val, " cm."),
      br(),
      tags$span(style = "font-size: 14px; color: #7f8c8d;",
        paste0("(Dokladnie ", actual_pct, "% obserwacji <= ", q_val, " cm)"))
    )
  })

  # ========================================================================
  # Widget 4: Guess the statistic game
  # ========================================================================

  ch3_game_data <- reactiveVal(NULL)
  ch3_game_guesses <- reactiveVal(list(mean = NULL, median = NULL))
  ch3_game_revealed <- reactiveVal(FALSE)
  ch3_game_round <- reactiveVal(0)
  ch3_game_score <- reactiveVal(list(total = 0, good = 0))

  generate_game_distribution <- function() {
    type <- sample(c("symmetric", "right_skew", "left_skew"), 1)
    n <- 200
    if (type == "symmetric") {
      vals <- rnorm(n, mean = sample(40:80, 1), sd = sample(8:15, 1))
    } else if (type == "right_skew") {
      vals <- rgamma(n, shape = sample(2:4, 1), scale = sample(5:12, 1)) + sample(10:30, 1)
    } else {
      vals <- 100 - rgamma(n, shape = sample(2:4, 1), scale = sample(5:12, 1))
    }
    round(vals, 1)
  }

  observe({
    if (is.null(ch3_game_data())) {
      ch3_game_data(generate_game_distribution())
    }
  })

  observeEvent(input$ch3_game_new, {
    ch3_game_data(generate_game_distribution())
    ch3_game_guesses(list(mean = NULL, median = NULL))
    ch3_game_revealed(FALSE)
    ch3_game_round(ch3_game_round() + 1)
  })

  observeEvent(input$ch3_game_click, {
    if (ch3_game_revealed()) return()
    g <- ch3_game_guesses()
    if (is.null(g$mean)) {
      g$mean <- input$ch3_game_click$x
    } else if (is.null(g$median)) {
      g$median <- input$ch3_game_click$x
    }
    ch3_game_guesses(g)
  })

  observeEvent(input$ch3_game_reveal, {
    g <- ch3_game_guesses()
    req(g$mean, g$median)
    ch3_game_revealed(TRUE)
    vals <- ch3_game_data()
    real_mean <- mean(vals)
    real_med <- median(vals)
    rng <- diff(range(vals))
    mean_err <- abs(g$mean - real_mean) / rng
    med_err <- abs(g$median - real_med) / rng
    sc <- ch3_game_score()
    sc$total <- sc$total + 1
    if (mean_err < 0.08 && med_err < 0.08) sc$good <- sc$good + 1
    ch3_game_score(sc)
  })

  output$ch3_game_status_banner <- renderUI({
    g <- ch3_game_guesses()
    if (is.null(g$mean)) {
      div(style = "text-align: center; padding: 12px; margin-bottom: 10px;
                    background: #fdedec; border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: #e74c3c;",
        "\u2193 Kliknij na wykres, aby postawi\u0107 \u015aREDNI\u0104"
      )
    } else if (is.null(g$median)) {
      div(style = "text-align: center; padding: 12px; margin-bottom: 10px;
                    background: #eaf4fc; border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: #3498db;",
        "\u2193 Teraz kliknij, aby postawi\u0107 MEDIAN\u0118"
      )
    } else if (!ch3_game_revealed()) {
      div(style = "text-align: center; padding: 12px; margin-bottom: 10px;
                    background: #eafaf1; border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: #27ae60;",
        "Gotowe! Kliknij 'Poka\u017c odpowied\u017a'"
      )
    } else {
      sc <- ch3_game_score()
      div(style = "text-align: center; padding: 8px; margin-bottom: 10px;
                    background: #f8f9fa; border-radius: 8px; font-size: 14px;
                    color: #2c3e50;",
        paste0("Wynik: ", sc$good, "/", sc$total, " trafionych rund")
      )
    }
  })

  output$ch3_game_plot <- renderPlot({
    vals <- ch3_game_data()
    req(vals)
    g <- ch3_game_guesses()
    revealed <- ch3_game_revealed()

    p <- ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(bins = 25, fill = "grey70", color = "white", alpha = 0.7) +
      theme_minimal(base_size = 14) +
      labs(x = "Wartość", y = "Liczebność", title = "Gdzie jest srednia? Gdzie mediana?")

    if (!is.null(g$mean)) {
      p <- p + geom_vline(xintercept = g$mean, color = "#e74c3c",
                          linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = g$mean, y = Inf, label = "Twoja\nśrednia",
                 vjust = 2, color = "#e74c3c", fontface = "bold", size = 3.5)
    }
    if (!is.null(g$median)) {
      p <- p + geom_vline(xintercept = g$median, color = "#3498db",
                          linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = g$median, y = Inf, label = "Twoja\nmediana",
                 vjust = 3.5, color = "#3498db", fontface = "bold", size = 3.5)
    }

    if (revealed) {
      real_mean <- mean(vals)
      real_med <- median(vals)
      p <- p +
        geom_vline(xintercept = real_mean, color = "#e74c3c", linewidth = 1.5) +
        annotate("text", x = real_mean, y = Inf, label = paste0("Srednia\n", round(real_mean, 1)),
                 vjust = 1, color = "#e74c3c", fontface = "bold", size = 4) +
        geom_vline(xintercept = real_med, color = "#3498db", linewidth = 1.5) +
        annotate("text", x = real_med, y = Inf, label = paste0("Mediana\n", round(real_med, 1)),
                 vjust = 2.5, color = "#3498db", fontface = "bold", size = 4)
    }

    p
  })

  output$ch3_game_feedback <- renderUI({
    if (!ch3_game_revealed()) return(NULL)
    vals <- ch3_game_data()
    g <- ch3_game_guesses()
    real_mean <- mean(vals)
    real_med <- median(vals)

    mean_err <- round(abs(g$mean - real_mean), 1)
    med_err <- round(abs(g$median - real_med), 1)
    rng <- diff(range(vals))

    if (real_mean > real_med + rng * 0.02) {
      skew_text <- "Rozkład jest skośny w prawo (średnia > mediana)."
    } else if (real_mean < real_med - rng * 0.02) {
      skew_text <- "Rozkład jest skośny w lewo (średnia < mediana)."
    } else {
      skew_text <- "Rozkład jest w przyblizeniu symetryczny (średnia \u2248 mediana)."
    }

    overall_err <- (abs(g$mean - real_mean) + abs(g$median - real_med)) / rng
    if (overall_err < 0.08) {
      grade <- "Doskonale!"
      cls <- "callout-info"
    } else if (overall_err < 0.15) {
      grade <- "Nieźle!"
      cls <- "callout-warning"
    } else {
      grade <- "Mozna lepiej!"
      cls <- "callout-danger"
    }

    div(class = cls,
      tags$strong(paste0(grade, " ")),
      paste0("Blad średniej: ", mean_err, ", blad mediany: ", med_err, ". "),
      skew_text
    )
  })

  # ==========================================================================
  # CHAPTER 4 SERVER
  # ==========================================================================

  # --- Widget 1: Bus scenario ---

  ch4_spread_step <- reactiveVal(0)

  observeEvent(input$ch4_spread_s1, { ch4_spread_step(1) })
  observeEvent(input$ch4_spread_s2, { ch4_spread_step(2) })
  observeEvent(input$ch4_spread_s3, { ch4_spread_step(3) })
  observeEvent(input$ch4_spread_s4, { ch4_spread_step(4) })

  # Helper: generate bus delay data (deterministic seed)
  ch4_bus_data <- function() {
    set.seed(123)
    data_a <- rgamma(1000, shape = 8, scale = 0.25) - 0.3
    data_b <- rgamma(1000, shape = 0.4, scale = 5)  - 0.3
    data_a <- data_a - mean(data_a) + 2
    data_b <- data_b - mean(data_b) + 2
    list(a = data_a, b = data_b,
         sd_a = round(sd(data_a), 1), sd_b = round(sd(data_b), 1))
  }

  output$ch4_spread_plot <- renderPlot({
    step <- ch4_spread_step()
    if (step == 0) return(NULL)

    buffer <- input$ch4_spread_buffer
    bus <- ch4_bus_data()

    dens_a <- density(bus$a, from = -3, to = 30, n = 500)
    dens_b <- density(bus$b, from = -3, to = 30, n = 500)
    df_a <- data.frame(x = dens_a$x, y = dens_a$y,
                       linia = paste0("Linia A (SD = ", bus$sd_a, ")"))
    df_b <- data.frame(x = dens_b$x, y = dens_b$y,
                       linia = paste0("Linia B (SD = ", bus$sd_b, ")"))
    df_all <- rbind(df_a, df_b)

    col_a <- "#3498db"; col_b <- "#e74c3c"
    lbl_a <- paste0("Linia A (SD = ", bus$sd_a, ")")
    lbl_b <- paste0("Linia B (SD = ", bus$sd_b, ")")

    p <- ggplot(df_all, aes(x = x, y = y, color = linia, fill = linia)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = setNames(c(col_a, col_b), c(lbl_a, lbl_b))) +
      scale_fill_manual(values = setNames(c(col_a, col_b), c(lbl_a, lbl_b))) +
      geom_vline(xintercept = 2, linetype = "dashed", color = "#2c3e50",
                 linewidth = 0.8) +
      annotate("text", x = 2, y = max(df_a$y) * 1.08,
               label = "\u015arednie sp\u00f3\u017anienie = 2 min",
               hjust = 0.5, size = 4.5, color = "#2c3e50", fontface = "bold") +
      geom_vline(xintercept = 0, linetype = "solid", color = "#95a5a6",
                 linewidth = 0.5, alpha = 0.5) +
      annotate("text", x = -0.3, y = max(df_a$y) * 0.3,
               label = "punktualny", angle = 90,
               size = 3.5, color = "#95a5a6") +
      labs(x = "Sp\u00f3\u017anienie (minuty)    \u2190 za wcze\u015bnie | za p\u00f3\u017ano \u2192",
           y = "G\u0119sto\u015b\u0107",
           title = "Rozk\u0142ad sp\u00f3\u017anie\u0144 dw\u00f3ch linii autobusowych",
           color = NULL, fill = NULL) +
      coord_cartesian(xlim = c(-3, 25)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")

    if (step >= 2) {
      p <- p +
        annotate("text", x = -1.5, y = max(df_a$y) * 0.85,
                 label = paste0("SD = ", bus$sd_a, " min\n(niezawodna)"),
                 size = 4, color = col_a, fontface = "bold") +
        annotate("text", x = 15, y = max(df_b$y) * 1.8,
                 label = paste0("SD = ", bus$sd_b, " min\n(nieprzewidywalna)"),
                 size = 4, color = col_b, fontface = "bold")
    }

    if (step >= 3) {
      cutoff <- -buffer  # wychodzisz buffer min wczesniej = jestes na -buffer
      # Zdazysz na autobus jesli delay >= -buffer (jeszcze nie odjecha\u0142)
      shade_a <- df_a[df_a$x >= cutoff, ]
      shade_b <- df_b[df_b$x >= cutoff, ]

      lbl <- if (buffer == 0) "Wychodzisz\nna st\u00f3wk\u0119"
             else paste0("Wychodzisz\n", buffer, " min wcze\u015bniej")

      p <- p +
        geom_area(data = shade_a, aes(x = x, y = y), alpha = 0.25) +
        geom_area(data = shade_b, aes(x = x, y = y), alpha = 0.15) +
        geom_vline(xintercept = cutoff, linetype = "dotted",
                   color = "#27ae60", linewidth = 1) +
        annotate("text", x = cutoff, y = max(df_a$y) * 0.95,
                 label = lbl,
                 hjust = 1.1, size = 3.8, color = "#27ae60", fontface = "bold")
    }

    if (step >= 4) {
      prob_a <- mean(bus$a >= -buffer)
      prob_b <- mean(bus$b >= -buffer)

      p <- p +
        annotate("label", x = 18, y = max(df_a$y) * 0.85,
                 label = paste0("P(zd\u0105\u017cysz) A = ",
                                round(prob_a * 100, 1), "%"),
                 size = 4.5, fill = "#eaf4fc", color = col_a,
                 fontface = "bold", label.size = 0.5) +
        annotate("label", x = 18, y = max(df_a$y) * 0.70,
                 label = paste0("P(zd\u0105\u017cysz) B = ",
                                round(prob_b * 100, 1), "%"),
                 size = 4.5, fill = "#fdedec", color = col_b,
                 fontface = "bold", label.size = 0.5)
    }

    p
  })

  output$ch4_spread_text <- renderUI({
    step <- ch4_spread_step()
    buffer <- input$ch4_spread_buffer
    bus <- ch4_bus_data()

    if (step == 0) {
      div(class = "callout-info",
          "Kliknij przycisk kroku, aby rozpocz\u0105\u0107.")
    } else if (step == 1) {
      div(class = "callout-info",
          tags$strong("Krok 1:"),
          " Obie linie maj\u0105 \u015brednie sp\u00f3\u017anienie oko\u0142o 2 minut.
          Patrz\u0105c tylko na \u015bredni\u0105, s\u0105 identyczne.
          Warto\u015bci ujemne = przyjazd przed czasem (rzadko si\u0119 zdarza).")
    } else if (step == 2) {
      pct_10_a <- round(mean(bus$a > 10) * 100, 1)
      pct_10_b <- round(mean(bus$b > 10) * 100, 1)
      mean_late_a <- if (any(bus$a > 10)) round(mean(bus$a[bus$a > 10]), 1) else 0
      mean_late_b <- if (any(bus$b > 10)) round(mean(bus$b[bus$b > 10]), 1) else 0
      div(class = "callout-info",
          tags$strong("Krok 2:"),
          paste0(" Linia A ma SD = ", bus$sd_a, " min (sp\u00f3\u017anienia skupione 0-4 min),
          a linia B ma SD = ", bus$sd_b, " min (zdarza si\u0119 i punktualnie,
          i 10+ min sp\u00f3\u017anienia)."),
          tags$br(), tags$br(),
          tags$strong("Sp\u00f3\u017anienia >10 min: "),
          paste0("Linia A: ", pct_10_a, "% kurs\u00f3w",
                 if (pct_10_a > 0) paste0(" (\u015br. ", mean_late_a, " min)") else "",
                 "; Linia B: ", pct_10_b, "% kurs\u00f3w",
                 if (pct_10_b > 0) paste0(" (\u015br. ", mean_late_b, " min)") else "",
                 "."))
    } else if (step == 3) {
      lbl <- if (buffer == 0) "na st\u00f3wk\u0119 (0 min zapasu)"
             else paste0(buffer, " min wcze\u015bniej")
      div(class = "callout-info",
          tags$strong("Krok 3:"),
          paste0(" Wychodzisz ", lbl,
                 ". Jeste\u015b na przystanku o ", buffer,
                 " min przed rozk\u0142adem. Zd\u0105\u017cysz na ka\u017cdy autobus,
                 kt\u00f3ry nie odjedzie wcze\u015bniej ni\u017c ", buffer,
                 " min przed rozk\u0142adem. Zacieniowany obszar = kursy,
                 na kt\u00f3re zd\u0105\u017cysz. Przesu\u0144 suwak!"))
    } else if (step == 4) {
      prob_a <- mean(bus$a >= -buffer)
      prob_b <- mean(bus$b >= -buffer)
      pct_10_a <- round(mean(bus$a > 10) * 100, 1)
      pct_10_b <- round(mean(bus$b > 10) * 100, 1)
      mean_late_b <- if (any(bus$b > 10)) round(mean(bus$b[bus$b > 10]), 1) else 0
      lbl <- if (buffer == 0) "na st\u00f3wk\u0119" else paste0(buffer, " min wcze\u015bniej")
      div(class = "callout-info",
          tags$strong("Krok 4: Konsekwencje"),
          tags$br(),
          paste0("Wychodzisz ", lbl, ":"),
          tags$br(),
          tags$strong(paste0("Linia A: zd\u0105\u017cysz na ", round(prob_a * 100, 1),
                             "% kurs\u00f3w.")),
          tags$br(),
          tags$strong(paste0("Linia B: zd\u0105\u017cysz na ", round(prob_b * 100, 1),
                             "% kurs\u00f3w.")),
          tags$br(), tags$br(),
          if (pct_10_b > 0) tagList(
            tags$em(paste0("A gdy linia B si\u0119 sp\u00f3\u017ani powa\u017cnie (>10 min, ",
                           pct_10_b, "% kurs\u00f3w), \u015brednie czekasz ",
                           mean_late_b, " min. ",
                           "Linia A praktycznie nigdy tak si\u0119 nie sp\u00f3\u017ania.")),
            tags$br(), tags$br()
          ),
          "To dlatego sama \u015brednia nie wystarczy -- rozrzut danych
          ma realne konsekwencje!")
    }
  })

  # --- Widget 2: SD step-by-step ---

  ch4_sd_step <- reactiveVal(0)
  ch4_sd_data <- reactiveVal(round(rnorm(10, mean = 170, sd = 8), 1))

  observeEvent(input$ch4_sd_s1, { ch4_sd_step(1) })
  observeEvent(input$ch4_sd_s2, { ch4_sd_step(2) })
  observeEvent(input$ch4_sd_s3, { ch4_sd_step(3) })

  observeEvent(input$ch4_sd_new, {
    set.seed(NULL)
    ch4_sd_data(round(rnorm(10, mean = 170, sd = 8), 1))
    ch4_sd_step(0)
  })

  observeEvent(input$ch4_sd_reset, {
    ch4_sd_step(0)
  })

  output$ch4_sd_plot <- renderPlot({
    step <- ch4_sd_step()
    if (step == 0) return(NULL)

    vals <- ch4_sd_data()
    n <- length(vals)
    x_bar <- mean(vals)
    s <- sd(vals)

    if (step == 1) {
      # Krok 1: punkty na osi liczbowej
      df <- data.frame(x = vals)
      p <- ggplot(df, aes(x = x, y = 0)) +
        geom_point(size = 4, color = "#3498db") +
        labs(x = "Wzrost (cm)", y = "",
             title = "Pomiary wzrostu (n = 10)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.3, 0.3))

    } else {
      # Kroki 2-3: punkty jedna pod drug\u0105, posortowane wg odleg\u0142o\u015bci od \u015bredniej
      deviations <- vals - x_bar
      ord <- order(abs(deviations), decreasing = TRUE)
      df <- data.frame(
        x = vals[ord],
        dev = deviations[ord],
        y = seq(n, 1)  # najdalszy na g\u00f3rze
      )

      p <- ggplot(df, aes(x = x, y = y)) +
        geom_vline(xintercept = x_bar, linetype = "dashed", color = "#e74c3c",
                   linewidth = 1) +
        geom_segment(aes(x = x_bar, xend = x, y = y, yend = y),
                     color = "#f39c12", linewidth = 0.8,
                     arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
        geom_point(size = 4, color = "#3498db") +
        geom_text(aes(label = paste0(ifelse(dev > 0, "+", ""),
                                     round(dev, 1))),
                  hjust = ifelse(df$dev >= 0, -0.3, 1.3),
                  size = 3.5, color = "#7f8c8d") +
        annotate("text", x = x_bar, y = n + 0.8,
                 label = paste0("\u015brednia = ", round(x_bar, 2)),
                 color = "#e74c3c", size = 5, fontface = "bold") +
        labs(x = "Wzrost (cm)", y = "",
             title = "Odchylenia od \u015bredniej (posortowane wg odleg\u0142o\u015bci)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(0, n + 1.2))

      if (step >= 3) {
        p <- p +
          annotate("rect", xmin = x_bar - s, xmax = x_bar + s,
                   ymin = 0, ymax = n + 0.3, fill = "#27ae60", alpha = 0.08) +
          geom_vline(xintercept = x_bar - s, linetype = "dotted",
                     color = "#27ae60", linewidth = 0.8) +
          geom_vline(xintercept = x_bar + s, linetype = "dotted",
                     color = "#27ae60", linewidth = 0.8) +
          annotate("text", x = x_bar - s, y = 0.3,
                   label = paste0("\u015br. - SD\n", round(x_bar - s, 1)),
                   color = "#27ae60", size = 3.5, fontface = "bold", vjust = 0) +
          annotate("text", x = x_bar + s, y = 0.3,
                   label = paste0("\u015br. + SD\n", round(x_bar + s, 1)),
                   color = "#27ae60", size = 3.5, fontface = "bold", vjust = 0) +
          annotate("text", x = x_bar, y = 0.5,
                   label = paste0("SD = ", round(s, 2), " cm"),
                   color = "#27ae60", size = 4.5, fontface = "bold")
      }
    }

    p
  })

  output$ch4_sd_table <- renderTable({
    step <- ch4_sd_step()
    if (step < 2) return(NULL)

    vals <- ch4_sd_data()
    n <- length(vals)
    x_bar <- mean(vals)

    deviations <- vals - x_bar
    sq_deviations <- deviations^2

    df <- data.frame(
      i = 1:n,
      `xi` = vals,
      `xi - x_bar` = round(deviations, 2),
      `(xi - x_bar)^2` = round(sq_deviations, 2),
      check.names = FALSE
    )

    if (step >= 3) {
      variance <- sum(sq_deviations) / (n - 1)
      s <- sqrt(variance)
      summary_row <- data.frame(
        i = NA,
        `xi` = NA,
        `xi - x_bar` = NA,
        `(xi - x_bar)^2` = round(sum(sq_deviations), 2),
        check.names = FALSE
      )
      # Mark the summary row
      summary_row$i <- "SUMA"
      summary_row$`xi` <- ""
      summary_row$`xi - x_bar` <- ""
      df$i <- as.character(df$i)
      df$`xi` <- as.character(df$`xi`)
      df$`xi - x_bar` <- as.character(round(deviations, 2))
      summary_row$`(xi - x_bar)^2` <- as.character(round(sum(sq_deviations), 2))
      df$`(xi - x_bar)^2` <- as.character(round(sq_deviations, 2))
      df <- rbind(df, summary_row)
    }

    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%",
     align = "cccc")

  output$ch4_sd_text <- renderUI({
    step <- ch4_sd_step()

    if (step == 0) {
      div(class = "callout-info",
          "Kliknij przycisk kroku, aby rozpoczac obliczanie odchylenia standardowego.")
    } else if (step == 1) {
      div(class = "callout-info",
          tags$strong("Krok 1:"),
          " Mamy 10 pomiarów wzrostu. Na osi liczbowej każdy punkt to jedna
          obserwacja. Jak bardzo są rozproszone?")
    } else if (step == 2) {
      vals <- ch4_sd_data()
      x_bar <- mean(vals)
      div(class = "callout-info",
          tags$strong("Krok 2:"),
          paste0(" Obliczamy srednia: x\u0304 = ", round(x_bar, 2),
                 " cm. Nastepnie liczymy odchylenie każdego punktu od średniej
                 (strzalki na wykresie). W tabeli widzisz odchylenia i ich kwadraty.
                 Kwadraty gwarantuja, ze odchylenia dodatnie i ujemne sie nie
                 znosa."))
    } else if (step == 3) {
      vals <- ch4_sd_data()
      n <- length(vals)
      x_bar <- mean(vals)
      deviations <- vals - x_bar
      sq_deviations <- deviations^2
      variance <- sum(sq_deviations) / (n - 1)
      s <- sqrt(variance)
      div(class = "callout-info",
          tags$strong("Krok 3:"),
          tags$br(), tags$br(),
          withMathJax(helpText(
            "$$s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2}$$"
          )),
          paste0("Suma kwadrat\u00f3w odchyle\u0144 = ", round(sum(sq_deviations), 2)),
          tags$br(),
          paste0("Wariancja \\(s^2\\) = suma / (n-1) = ",
                 round(sum(sq_deviations), 2), " / ", n - 1, " = ",
                 round(variance, 2)),
          tags$br(),
          tags$strong(paste0("Odchylenie standardowe \\(s = \\sqrt{",
                             round(variance, 2), "} = ", round(s, 2), "\\) cm")),
          tags$br(), tags$br(),
          "Zielony pas na wykresie oznacza przedzia\u0142 \\(\\bar{x} \\pm s\\).
          W rozk\u0142adzie normalnym ok. 68% danych le\u017cy w tym przedziale.")
    }
  })

  # --- Widget 2b: Empirical rule (68-95-99.7) ---

  output$ch4_emp_plot <- renderPlot({
    var_name <- input$ch4_emp_var
    req(var_name)
    vals <- student_data[[var_name]]
    m <- mean(vals)
    s <- sd(vals)

    band_colors <- c("#3498db", "#f39c12", "#e74c3c")
    band_alphas <- c(0.25, 0.15, 0.10)
    band_labels <- c("\u00B11 SD", "\u00B12 SD", "\u00B13 SD")

    pct_in <- sapply(1:3, function(k) {
      round(mean(vals >= m - k * s & vals <= m + k * s) * 100, 1)
    })

    p <- ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 25, fill = "grey70", color = "white", alpha = 0.7)

    for (k in 3:1) {
      p <- p + annotate("rect",
        xmin = m - k * s, xmax = m + k * s,
        ymin = -Inf, ymax = Inf,
        fill = band_colors[k], alpha = band_alphas[k]
      )
    }

    p <- p +
      geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.2, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("x\u0304 = ", round(m, 1)),
               vjust = -0.5, color = "#e74c3c", fontface = "bold", size = 4.5) +
      labs(
        title = paste0("Regula empiryczna: ", pct_in[1], "% / ",
                        pct_in[2], "% / ", pct_in[3], "%",
                        "  (teoria: 68% / 95% / 99.7%)"),
        x = variable_meta[[var_name]]$label,
        y = "Gęstość"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 13))

    p
  })

  output$ch4_emp_text <- renderUI({
    var_name <- input$ch4_emp_var
    req(var_name)
    vals <- student_data[[var_name]]
    m <- mean(vals)
    s <- sd(vals)

    pct_in <- sapply(1:3, function(k) {
      round(mean(vals >= m - k * s & vals <= m + k * s) * 100, 1)
    })

    diff_1sd <- abs(pct_in[1] - 68)

    if (diff_1sd < 5) {
      div(class = "callout-info",
        tags$strong("Dobra zgodno\u015b\u0107 z regu\u0142\u0105! "),
        paste0("W przedziale \u00B11 SD le\u017cy ", pct_in[1], "% danych (teoria: 68%). "),
        "To oznacza, \u017ce rozk\u0142ad tej zmiennej jest zbli\u017cony do normalnego. ",
        "Odchylenie standardowe dobrze podsumowuje rozrzut."
      )
    } else {
      div(class = "callout-warning",
        tags$strong("S\u0142aba zgodno\u015b\u0107 z regu\u0142\u0105! "),
        paste0("W przedziale \u00B11 SD le\u017cy ", pct_in[1], "% danych (teoria: 68%). "),
        tags$b("Dlaczego?"), " Regu\u0142a 68-95-99.7 zak\u0142ada rozk\u0142ad symetryczny ",
        "(zbli\u017cony do normalnego). Gdy rozk\u0142ad jest sko\u015bny, dane koncentruj\u0105 si\u0119 ",
        "asymetrycznie wok\u00f3\u0142 \u015bredniej -- wi\u0119cej obserwacji le\u017cy po jednej stronie ",
        "ni\u017c po drugiej, co \u0142amie za\u0142o\u017cenie regu\u0142y. ",
        "W takim przypadku IQR lepiej opisuje rozrzut ni\u017c odchylenie standardowe."
      )
    }
  })

  # --- Widget 3: Boxplot builder ---

  ch4_bp_step <- reactiveVal(0)
  ch4_bp_data <- reactiveVal(round(c(rnorm(27, 170, 8), 145, 198, 200), 1))

  observeEvent(input$ch4_bp_s1, { ch4_bp_step(1) })
  observeEvent(input$ch4_bp_s2, { ch4_bp_step(2) })
  observeEvent(input$ch4_bp_s3, { ch4_bp_step(3) })
  observeEvent(input$ch4_bp_s4, { ch4_bp_step(4) })
  observeEvent(input$ch4_bp_s5, { ch4_bp_step(5) })

  observeEvent(input$ch4_bp_new, {
    set.seed(NULL)
    ch4_bp_data(round(c(rnorm(27, 170, 8), 145, 198, 200), 1))
    ch4_bp_step(0)
  })

  observeEvent(input$ch4_bp_reset, {
    ch4_bp_step(0)
  })

  output$ch4_bp_plot <- renderPlot({
    step <- ch4_bp_step()
    if (step == 0) return(NULL)

    vals <- ch4_bp_data()
    sorted_vals <- sort(vals)
    med <- median(vals)
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr_val <- q3 - q1
    lower_fence <- q1 - 1.5 * iqr_val
    upper_fence <- q3 + 1.5 * iqr_val
    whisker_low <- min(vals[vals >= lower_fence])
    whisker_high <- max(vals[vals <= upper_fence])
    outliers <- vals[vals < lower_fence | vals > upper_fence]

    if (step == 5) {
      # Final: clean boxplot + histogram for comparison
      df <- data.frame(x = vals)
      p_box <- ggplot(df, aes(y = x, x = "")) +
        geom_boxplot(fill = "#3498db", alpha = 0.5, color = "#2c3e50",
                     outlier.color = "#e74c3c", outlier.size = 3,
                     width = 0.4) +
        geom_jitter(width = 0.05, alpha = 0.4, size = 2, color = "#2c3e50") +
        coord_flip() +
        labs(x = "", y = "Wzrost (cm)", title = "Gotowy boxplot (geom_boxplot)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

      p_hist <- ggplot(df, aes(x = x)) +
        geom_histogram(bins = 15, fill = "#3498db", color = "white", alpha = 0.7) +
        geom_vline(xintercept = med, color = "#e74c3c", linewidth = 1,
                   linetype = "dashed") +
        geom_vline(xintercept = q1, color = "#f39c12", linewidth = 0.8,
                   linetype = "dotted") +
        geom_vline(xintercept = q3, color = "#f39c12", linewidth = 0.8,
                   linetype = "dotted") +
        labs(x = "Wzrost (cm)", y = "Liczebność",
             title = "Histogram (do porównania)") +
        theme_minimal(base_size = 14)

      gridExtra::grid.arrange(p_box, p_hist, nrow = 2, heights = c(1, 1.2))
      return()
    }

    # Steps 1-4: manual construction
    df <- data.frame(x = vals, y = 0)

    if (step == 1) {
      # Jittered raw data
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        geom_point(size = 3, color = "#3498db", alpha = 0.7) +
        labs(x = "Wzrost (cm)", y = "",
             title = "Krok 1: Surowe dane (n = 30)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.8, 0.8))

    } else if (step == 2) {
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        geom_point(size = 3, color = "#3498db", alpha = 0.7) +
        geom_vline(xintercept = med, color = "#e74c3c", linewidth = 1.5) +
        annotate("text", x = med, y = 0.65,
                 label = paste0("Mediana = ", round(med, 1)),
                 color = "#e74c3c", size = 5, fontface = "bold") +
        labs(x = "Wzrost (cm)", y = "",
             title = "Krok 2: Mediana dzieli dane na polowy") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.8, 0.8))

    } else if (step == 3) {
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        # IQR box
        annotate("rect", xmin = q1, xmax = q3, ymin = -0.5, ymax = 0.5,
                 fill = "#3498db", alpha = 0.2, color = "#3498db",
                 linewidth = 1) +
        geom_point(size = 3, color = "#3498db", alpha = 0.7) +
        geom_vline(xintercept = med, color = "#e74c3c", linewidth = 1.5) +
        geom_vline(xintercept = q1, color = "#f39c12", linewidth = 1,
                   linetype = "dashed") +
        geom_vline(xintercept = q3, color = "#f39c12", linewidth = 1,
                   linetype = "dashed") +
        annotate("text", x = med, y = 0.7,
                 label = paste0("Me = ", round(med, 1)),
                 color = "#e74c3c", size = 4.5, fontface = "bold") +
        annotate("text", x = q1, y = -0.65,
                 label = paste0("Q1 = ", round(q1, 1)),
                 color = "#f39c12", size = 4, fontface = "bold") +
        annotate("text", x = q3, y = -0.65,
                 label = paste0("Q3 = ", round(q3, 1)),
                 color = "#f39c12", size = 4, fontface = "bold") +
        annotate("text", x = (q1 + q3) / 2, y = 0.7,
                 label = paste0("IQR = ", round(iqr_val, 1)),
                 color = "#3498db", size = 4, fontface = "bold",
                 hjust = ifelse(abs(med - (q1 + q3) / 2) < 3, 2, 0.5)) +
        labs(x = "Wzrost (cm)", y = "",
             title = "Krok 3: Kwartyle Q1, Q3 i pudełko (IQR)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.9, 0.9))

    } else if (step == 4) {
      is_outlier <- vals < lower_fence | vals > upper_fence
      df$outlier <- is_outlier
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.2, 0.2)

      p <- ggplot(df) +
        # IQR box
        annotate("rect", xmin = q1, xmax = q3, ymin = -0.4, ymax = 0.4,
                 fill = "#3498db", alpha = 0.2, color = "#3498db",
                 linewidth = 1) +
        # Median line inside box
        geom_segment(aes(x = med, xend = med, y = -0.4, yend = 0.4),
                     color = "#e74c3c", linewidth = 1.5) +
        # Left whisker
        geom_segment(aes(x = whisker_low, xend = q1, y = 0, yend = 0),
                     color = "#2c3e50", linewidth = 0.8) +
        geom_segment(aes(x = whisker_low, xend = whisker_low, y = -0.2, yend = 0.2),
                     color = "#2c3e50", linewidth = 0.8) +
        # Right whisker
        geom_segment(aes(x = q3, xend = whisker_high, y = 0, yend = 0),
                     color = "#2c3e50", linewidth = 0.8) +
        geom_segment(aes(x = whisker_high, xend = whisker_high, y = -0.2, yend = 0.2),
                     color = "#2c3e50", linewidth = 0.8) +
        # Points: normal
        geom_point(data = df[!df$outlier, ], aes(x = x, y = y_jit),
                   size = 2.5, color = "#3498db", alpha = 0.5) +
        # Points: outliers
        geom_point(data = df[df$outlier, ], aes(x = x, y = y_jit),
                   size = 4, color = "#e74c3c", shape = 18) +
        # Fence annotations
        annotate("text", x = lower_fence, y = -0.55,
                 label = paste0("Q1 - 1.5*IQR\n= ", round(lower_fence, 1)),
                 color = "#7f8c8d", size = 3.5) +
        annotate("text", x = upper_fence, y = -0.55,
                 label = paste0("Q3 + 1.5*IQR\n= ", round(upper_fence, 1)),
                 color = "#7f8c8d", size = 3.5) +
        labs(x = "Wzrost (cm)", y = "",
             title = "Krok 4: Wąsy i wartości odstające") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.8, 0.7))

      if (length(outliers) > 0) {
        p <- p +
          annotate("label", x = mean(outliers), y = 0.55,
                   label = paste0(length(outliers), " outlier(s)"),
                   fill = "#fdedec", color = "#e74c3c", size = 4,
                   fontface = "bold", label.size = 0.5)
      }

      p
    }
  })

  output$ch4_bp_text <- renderUI({
    step <- ch4_bp_step()
    if (step == 0) {
      div(class = "callout-info",
          "Kliknij przycisk kroku, aby zacząć budowę boxplota.")
    } else if (step == 1) {
      div(class = "callout-info",
          tags$strong("Krok 1:"),
          " Zaczynamy od surowych danych. 30 pomiarów wzrostu rozrzuconych
          na osi liczbowej. Widać ogolny zakres, ale ciężko wyciagnac
          szybkie wnioski.")
    } else if (step == 2) {
      vals <- ch4_bp_data()
      div(class = "callout-info",
          tags$strong("Krok 2:"),
          paste0(" Sortujemy dane i wyznaczamy mediane = ", round(median(vals), 1),
                 " cm. Mediana dzieli posortowane dane na dwie rowne polowy."))
    } else if (step == 3) {
      vals <- ch4_bp_data()
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      div(class = "callout-info",
          tags$strong("Krok 3:"),
          paste0(" Wyznaczamy kwartyle: Q1 = ", round(q1, 1),
                 " (25% danych poniżej), Q3 = ", round(q3, 1),
                 " (75% danych poniżej). Pudelko (box) rozciaga sie od Q1 do Q3
                 i zawiera srodkowe 50% danych. IQR = Q3 - Q1 = ",
                 round(q3 - q1, 1), " cm."))
    } else if (step == 4) {
      vals <- ch4_bp_data()
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      iqr_val <- q3 - q1
      outliers <- vals[vals < q1 - 1.5 * iqr_val | vals > q3 + 1.5 * iqr_val]
      div(class = "callout-info",
          tags$strong("Krok 4:"),
          paste0(" Wąsy siagaja do najdalszych punktow w granicach
                 1.5 * IQR od pudełka. Wszystko poza wąsami to wartości
                 odstające (outliers). "),
          if (length(outliers) > 0) {
            paste0("Znaleziono ", length(outliers),
                   " wartosc(i) odstająca(e): ",
                   paste(round(outliers, 1), collapse = ", "), " cm.")
          } else {
            "Brak wartości odstających."
          })
    } else if (step == 5) {
      div(class = "callout-info",
          tags$strong("Krok 5:"),
          " Gotowy boxplot (gora) w porownaniu z histogramem (dol).
          Boxplot kompaktowo podsumowuje rozkład: mediana, kwartyle,
          rozstęp i outlierow - wszystko w jednym wykresie. Histogram
          pokazuje więcej szczegółów o kształcie rozkładu.")
    }
  })

  # --- Widget 3b: Group comparison ---

  output$ch4_grp_plot <- renderPlot({
    var_name <- input$ch4_grp_var
    grp_name <- input$ch4_grp_by
    req(var_name, grp_name)

    df <- data.frame(
      value = student_data[[var_name]],
      group = student_data[[grp_name]]
    )

    var_label <- names(which(c("wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
      "czas_dojazdu" = "Czas dojazdu (min)", "srednia_ocen" = "Średnia ocen") == var_name))
    if (length(var_label) == 0) var_label <- var_name

    grp_label <- ifelse(grp_name == "plec", "Płeć", "Kierunek")

    p <- ggplot(df, aes(x = group, y = value, fill = group))

    if (isTRUE(input$ch4_grp_violin)) {
      p <- p + geom_violin(alpha = 0.4, color = NA) +
        geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA)
    } else {
      p <- p + geom_boxplot(alpha = 0.7, outlier.color = "#e74c3c", outlier.size = 3)
    }

    if (isTRUE(input$ch4_grp_points)) {
      p <- p + geom_jitter(width = 0.15, alpha = 0.3, size = 1.5)
    }

    p + scale_fill_brewer(palette = "Set2") +
      labs(x = grp_label, y = var_label) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })

  output$ch4_grp_table <- renderTable({
    var_name <- input$ch4_grp_var
    grp_name <- input$ch4_grp_by
    req(var_name, grp_name)

    df <- data.frame(
      value = student_data[[var_name]],
      group = student_data[[grp_name]]
    )

    df %>%
      group_by(Grupa = group) %>%
      summarise(
        n = n(),
        Średnia = round(mean(value), 2),
        Mediana = round(median(value), 2),
        SD = round(sd(value), 2),
        IQR = round(IQR(value), 2),
        .groups = "drop"
      )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  # --- Widget 4: Spread measures comparison ---

  ch4_comp_data <- reactiveVal(NULL)

  observe({
    if (is.null(ch4_comp_data())) {
      ch4_comp_data(student_data$wzrost)
    }
  })

  observeEvent(input$ch4_comp_add1, {
    set.seed(NULL)
    current <- ch4_comp_data()
    outlier <- max(current) + 30 + runif(1, -5, 5)
    ch4_comp_data(c(current, round(outlier, 1)))
  })

  observeEvent(input$ch4_comp_add5, {
    set.seed(NULL)
    current <- ch4_comp_data()
    outliers <- sapply(1:5, function(i) max(current) + 30 + runif(1, -5, 5))
    ch4_comp_data(c(current, round(outliers, 1)))
  })

  observeEvent(input$ch4_comp_reset, {
    ch4_comp_data(student_data$wzrost)
  })

  output$ch4_comp_plot <- renderPlot({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)

    df <- data.frame(x = vals)
    data_range <- range(vals)
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr_val <- q3 - q1

    ggplot(df, aes(x = x)) +
      geom_histogram(bins = 30, fill = "#3498db", color = "white", alpha = 0.7) +
      # Range
      annotate("segment", x = data_range[1], xend = data_range[2],
               y = -2, yend = -2, color = "#e74c3c", linewidth = 2) +
      annotate("text",
               x = (data_range[1] + data_range[2]) / 2, y = -3.5,
               label = paste0("Rozstęp = ", round(diff(data_range), 1)),
               color = "#e74c3c", size = 4, fontface = "bold") +
      # IQR
      annotate("segment", x = q1, xend = q3, y = -6, yend = -6,
               color = "#27ae60", linewidth = 2) +
      annotate("text", x = (q1 + q3) / 2, y = -7.5,
               label = paste0("IQR = ", round(iqr_val, 1)),
               color = "#27ae60", size = 4, fontface = "bold") +
      labs(x = "Wzrost (cm)", y = "Liczebność",
           title = paste0("Histogram wzrostu (n = ", length(vals), ")")) +
      theme_minimal(base_size = 14) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(10, 10, 50, 10))
  })

  output$ch4_comp_table <- renderTable({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)

    data.frame(
      Miara = c("Rozstep", "IQR (rozstęp międzykwartylowy)",
                "Odchylenie standardowe (SD)",
                "Współczynnik zmienności (CV)"),
      Wartość = c(
        paste0(round(diff(range(vals)), 1), " cm"),
        paste0(round(IQR(vals), 1), " cm"),
        paste0(round(sd(vals), 2), " cm"),
        paste0(round(sd(vals) / mean(vals) * 100, 1), "%")
      ),
      Wlasnosci = c(
        "Bardzo wrażliwy na outlierow - zależy tylko od min i max",
        "Odporny na outlierow - oparty na kwartylach",
        "Umiarkowanie wrażliwy - bierze pod uwage wszystkie dane",
        "Bezjednostkowy - pozwala porownywac zmiennosc roznych zmiennych"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  # --- Widget 5: Coefficient of Variation ---

  output$ch4_sd_compare_plot <- renderPlot({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "\u015arednia ocen")

    stats <- data.frame(
      Zmienna = factor(labels, levels = rev(labels)),
      SD = sapply(vars, function(v) sd(student_data[[v]]))
    )

    ggplot(stats, aes(x = Zmienna, y = SD, fill = SD)) +
      geom_col(alpha = 0.85, width = 0.6) +
      geom_text(aes(label = round(SD, 2)), hjust = -0.1, size = 5) +
      scale_fill_gradient(low = "#85c1e9", high = "#2980b9", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      labs(x = NULL, y = "Odchylenie standardowe (oryg. jednostki)",
           title = "SD -- nieporównywalne") +
      theme_minimal(base_size = 14)
  })

  output$ch4_cv_plot <- renderPlot({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "\u015arednia ocen")

    stats <- data.frame(
      Zmienna = factor(labels, levels = rev(labels)),
      CV = sapply(vars, function(v) sd(student_data[[v]]) / mean(student_data[[v]]) * 100)
    )

    ggplot(stats, aes(x = Zmienna, y = CV, fill = CV)) +
      geom_col(alpha = 0.85, width = 0.6) +
      geom_text(aes(label = paste0(round(CV, 1), "%")), hjust = -0.1, size = 5) +
      scale_fill_gradient(low = "#3498db", high = "#e74c3c", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      labs(x = NULL, y = "Współczynnik zmienności (%)",
           title = "CV -- porównywalne") +
      theme_minimal(base_size = 14)
  })

  output$ch4_cv_table <- renderTable({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    data.frame(
      Zmienna = labels,
      Średnia = sapply(vars, function(v) round(mean(student_data[[v]]), 2)),
      SD = sapply(vars, function(v) round(sd(student_data[[v]]), 2)),
      `CV (%)` = sapply(vars, function(v) round(sd(student_data[[v]]) / mean(student_data[[v]]) * 100, 1)),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  # ==========================================================================
  # CHAPTER 5 SERVER
  # ==========================================================================

  # --------------------------------------------------------------------------
  # Widget 1: Skewness
  # --------------------------------------------------------------------------

  ch5_skew_data <- reactive({
    req(input$ch5_skew_var)
    vals <- student_data[[input$ch5_skew_var]]
    vals <- vals[!is.na(vals)]
    list(
      values = vals,
      label = variable_meta[[input$ch5_skew_var]]$label,
      var_name = input$ch5_skew_var
    )
  })

  output$ch5_skew_plot <- renderPlot({
    d <- ch5_skew_data()
    vals <- d$values
    m <- mean(vals)
    med <- median(vals)
    sk <- e1071::skewness(vals)

    if (abs(m - med) / sd(vals) < 0.1) {
      annotation <- "Średnia \u2248 Mediana \u2192 rozkład symetryczny"
    } else if (m > med) {
      annotation <- "Średnia > Mediana \u2192 skośność prawostronna"
    } else {
      annotation <- "Średnia < Mediana \u2192 skośność lewostronna"
    }

    df <- data.frame(x = vals)

    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
        bins = 20, fill = "#3498db", color = "white", alpha = 0.6) +
      geom_density(color = "#2c3e50", linewidth = 1) +
      geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.2, linetype = "solid") +
      geom_vline(xintercept = med, color = "#3498db", linewidth = 1.2, linetype = "dashed") +
      annotate("text",
        x = max(vals) - 0.05 * diff(range(vals)),
        y = Inf, vjust = 2, hjust = 1,
        label = annotation, size = 4.5, fontface = "italic", color = "#2c3e50") +
      annotate("text",
        x = m, y = -Inf, vjust = -0.5,
        label = paste0("Średnia = ", round(m, 2)),
        color = "#e74c3c", size = 3.5, fontface = "bold") +
      annotate("text",
        x = med, y = -Inf, vjust = -2,
        label = paste0("Mediana = ", round(med, 2)),
        color = "#3498db", size = 3.5, fontface = "bold") +
      labs(
        title = paste0(d$label, " \u2014 skośność = ", round(sk, 3)),
        x = d$label,
        y = "Gęstość"
      ) +
      theme_minimal(base_size = 14)
  })

  output$ch5_skew_info <- renderUI({
    d <- ch5_skew_data()
    vals <- d$values
    sk <- e1071::skewness(vals)

    if (sk > 0.5) {
      interp <- "Rozkład jest wyraźnie prawostronnie skośny (skośność > 0.5). Większość wartości skupia sie po lewej stronie, z długim ogonem w prawo."
    } else if (sk < -0.5) {
      interp <- "Rozkład jest wyraźnie lewostronnie skośny (skośność < -0.5). Większość wartości skupia sie po prawej stronie, z długim ogonem w lewo."
    } else if (sk > 0.1) {
      interp <- "Rozkład jest lekko prawostronnie skośny."
    } else if (sk < -0.1) {
      interp <- "Rozkład jest lekko lewostronnie skośny."
    } else {
      interp <- "Rozkład jest w przyblizeniu symetryczny."
    }

    tagList(
      div(class = "callout-info",
        tags$strong("Regula kciuka: "),
        "Jeśli średnia jest większa od mediany, to rozkład jest prawostronnie skośny
        (ogon ciagnie sie w prawo). Jeśli średnia jest mniejsza od mediany \u2014
        lewostronnie skośny. Wartość skośnośći = 0 oznacza idealna symetrię."
      ),
      p(tags$strong("Skośność = ", round(sk, 3)), " \u2014 ", interp)
    )
  })

  # --------------------------------------------------------------------------
  # Widget 2: Kurtosis (suwak kurtozy, bez t-rozk\u0142adu)
  # --------------------------------------------------------------------------

  observeEvent(input$ch5_kurt_platy, { updateSliderInput(session, "ch5_kurt_val", value = -1.0) })
  observeEvent(input$ch5_kurt_mezo,  { updateSliderInput(session, "ch5_kurt_val", value = 0) })
  observeEvent(input$ch5_kurt_lepto, { updateSliderInput(session, "ch5_kurt_val", value = 4) })

  # Generate density with target excess kurtosis
  # Leptokurtic: t-distribution scaled to sd=1 (higher peak, heavier tails)
  # Platykurtic: beta(a,a) scaled to sd=1 (flatter peak, no tails)
  ch5_kurt_density <- reactive({
    ek <- input$ch5_kurt_val
    req(!is.null(ek))
    x_seq <- seq(-5, 5, length.out = 500)

    if (ek < -0.01) {
      # Platykurtyczny: beta(a,a) scaled to sd=1
      # excess_kurt of beta(a,a) = -6/(2a+3)
      # So a = -(6/ek + 3) / 2, but ek is negative
      # ek = -6/(2a+3) -> 2a+3 = -6/ek -> a = (-6/ek - 3)/2
      a <- max(1.01, (-6 / ek - 3) / 2)
      scale_b <- sqrt(2 * a + 1)
      # beta(a,a) on [0,1] mapped to [-scale_b, scale_b] for sd=1
      dens <- dbeta((x_seq / scale_b + 1) / 2, a, a) / (2 * scale_b)
      # Zero out beyond support
      dens[abs(x_seq) > scale_b] <- 0
    } else if (ek <= 0.01) {
      dens <- dnorm(x_seq)
    } else {
      # Leptokurtyczny: t-distribution scaled to sd=1
      # excess_kurt = 6 / (df - 4) -> df = 6/ek + 4
      df_mapped <- max(4.5, 6 / ek + 4)
      sd_t <- sqrt(df_mapped / (df_mapped - 2))
      dens <- dt(x_seq * sd_t, df = df_mapped) * sd_t
    }
    data.frame(x = x_seq, dens = dens, norm = dnorm(x_seq))
  })

  output$ch5_kurt_plot <- renderPlot({
    df <- ch5_kurt_density()
    ek <- input$ch5_kurt_val

    type_name <- if (ek < -0.1) "Platykurtyczny" else if (ek > 0.1) "Leptokurtyczny" else "Mezokurtyczny"
    type_color <- if (ek < -0.1) "#f39c12" else if (ek > 0.1) "#e74c3c" else "#27ae60"

    ggplot(df, aes(x = x)) +
      geom_line(aes(y = norm), color = "#95a5a6", linewidth = 1, linetype = "dashed") +
      geom_area(aes(y = dens), fill = type_color, alpha = 0.35) +
      geom_line(aes(y = dens), color = type_color, linewidth = 1.2) +
      annotate("text", x = -4.5, y = max(df$dens) * 0.95,
        label = paste0(type_name, " (kurtoza = ", round(ek, 1), ")"),
        color = type_color, hjust = 0, size = 5, fontface = "bold") +
      annotate("text", x = -4.5, y = max(df$dens) * 0.85,
        label = "Rozk\u0142ad normalny (kurtoza = 0)",
        color = "#95a5a6", hjust = 0, size = 4) +
      labs(x = "x", y = "G\u0119sto\u015b\u0107",
        title = "Jak kurtoza wp\u0142ywa na kszta\u0142t rozk\u0142adu?") +
      theme_minimal(base_size = 14)
  })

  output$ch5_kurt_tails <- renderPlot({
    df <- ch5_kurt_density()
    ek <- input$ch5_kurt_val
    type_color <- if (ek < -0.1) "#f39c12" else if (ek > 0.1) "#e74c3c" else "#27ae60"

    tail_df <- df[df$x >= 2.5, ]

    ggplot(tail_df, aes(x = x)) +
      geom_area(aes(y = norm), fill = "#95a5a6", alpha = 0.15) +
      geom_line(aes(y = norm), color = "#95a5a6", linewidth = 1, linetype = "dashed") +
      geom_area(aes(y = dens), fill = type_color, alpha = 0.3) +
      geom_line(aes(y = dens), color = type_color, linewidth = 1.2) +
      labs(x = "x", y = "G\u0119sto\u015b\u0107",
        title = "Powi\u0119kszenie prawego ogona (x > 2.5)") +
      theme_minimal(base_size = 14)
  })

  output$ch5_kurt_text <- renderUI({
    ek <- input$ch5_kurt_val
    req(!is.null(ek))

    if (ek < -0.5) {
      type_class <- "callout-warning"
      type_name <- "Platykurtyczny"
      desc <- "Rozk\u0142ad ma l\u017cejsze ogony ni\u017c normalny \u2014 warto\u015bci ekstremalne
               s\u0105 rzadsze. Dane s\u0105 bardziej 'skoncentrowane' w okolicy \u015bredniej,
               bez dalekich obserwacji."
    } else if (ek < 0.5) {
      type_class <- "callout-info"
      type_name <- "Mezokurtyczny"
      desc <- "Rozk\u0142ad jest zbli\u017cony do normalnego \u2014 ogony maj\u0105 'typow\u0105'
               ci\u0119\u017cko\u015b\u0107. To punkt odniesienia, wzgl\u0119dem kt\u00f3rego por\u00f3wnujemy
               inne rozk\u0142ady."
    } else {
      type_class <- "callout-danger"
      type_name <- "Leptokurtyczny"
      desc <- paste0("Rozk\u0142ad ma ci\u0119\u017csze ogony ni\u017c normalny \u2014 warto\u015bci
               ekstremalne pojawiaj\u0105 si\u0119 cz\u0119\u015bciej ni\u017c by\u015bmy oczekiwali.
               W finansach to oznacza wi\u0119ksze ryzyko ekstremalnych strat
               ('czarne \u0142ab\u0119dzie').")
    }

    div(class = type_class,
      tags$strong(paste0(type_name, " (nadwy\u017ckowa kurtoza = ", round(ek, 1), ")")),
      p(desc),
      tags$ul(
        tags$li(tags$b("Platykurtyczny"), " (kurtoza < 0): lekkie ogony, mniej ekstrema\u0142\u00f3w"),
        tags$li(tags$b("Mezokurtyczny"), " (kurtoza \u2248 0): rozk\u0142ad normalny \u2014 punkt odniesienia"),
        tags$li(tags$b("Leptokurtyczny"), " (kurtoza > 0): ci\u0119\u017ckie ogony, wi\u0119cej ekstrema\u0142\u00f3w")
      )
    )
  })

  # --------------------------------------------------------------------------
  # Widget 3: Full picture (Capstone)
  # --------------------------------------------------------------------------

  ch5_full_data <- reactive({
    req(input$ch5_full_var)
    vals <- student_data[[input$ch5_full_var]]
    vals <- vals[!is.na(vals)]
    list(
      values = vals,
      label = variable_meta[[input$ch5_full_var]]$label,
      var_name = input$ch5_full_var
    )
  })

  output$ch5_full_hist <- renderPlot({
    d <- ch5_full_data()
    vals <- d$values
    m <- mean(vals)
    med <- median(vals)
    df <- data.frame(x = vals)

    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
        bins = 20, fill = "#3498db", color = "white", alpha = 0.5) +
      geom_density(color = "#2c3e50", linewidth = 1) +
      geom_rug(color = "#2c3e50", alpha = 0.5) +
      geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.1) +
      geom_vline(xintercept = med, color = "#3498db", linewidth = 1.1, linetype = "dashed") +
      annotate("text", x = m, y = Inf, vjust = 2, hjust = -0.1,
        label = paste0("Średnia = ", round(m, 2)),
        color = "#e74c3c", size = 3.8, fontface = "bold") +
      annotate("text", x = med, y = Inf, vjust = 3.5, hjust = -0.1,
        label = paste0("Mediana = ", round(med, 2)),
        color = "#3498db", size = 3.8, fontface = "bold") +
      labs(
        title = paste0("Rozkład zmiennej: ", d$label),
        x = d$label,
        y = "Gęstość"
      ) +
      theme_minimal(base_size = 14)
  })

  output$ch5_full_box <- renderPlot({
    d <- ch5_full_data()
    df <- data.frame(x = d$values)

    ggplot(df, aes(x = x)) +
      geom_boxplot(fill = "#3498db", alpha = 0.4, color = "#2c3e50",
        outlier.color = "#e74c3c", outlier.size = 3) +
      labs(x = d$label) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })

  output$ch5_full_table <- renderTable({
    d <- ch5_full_data()
    vals <- d$values

    n <- length(vals)
    m <- mean(vals)
    med <- median(vals)
    s <- sd(vals)
    v <- var(vals)
    rng <- diff(range(vals))
    q1 <- quantile(vals, 0.25, names = FALSE)
    q3 <- quantile(vals, 0.75, names = FALSE)
    iqr_val <- IQR(vals)
    cv <- (s / m) * 100
    sk <- e1071::skewness(vals)
    ku <- e1071::kurtosis(vals)
    trimmed <- mean(vals, trim = 0.1)

    # Mode: bin with highest frequency
    h <- hist(vals, breaks = 20, plot = FALSE)
    mode_bin_idx <- which.max(h$counts)
    mode_val <- (h$breaks[mode_bin_idx] + h$breaks[mode_bin_idx + 1]) / 2

    stats_df <- data.frame(
      Statystyka = c(
        "n", "Srednia", "Mediana", "Dominanta (środek przedziałowy)",
        "Sr. ucinana 10%",
        "Odch. std.", "Wariancja", "Rozstep", "IQR", "CV (%)",
        "Minimum", "Q1", "Q3", "Maksimum",
        "Skośność", "Kurtoza"
      ),
      Wartość = c(
        as.character(n),
        formatC(m, format = "f", digits = 2),
        formatC(med, format = "f", digits = 2),
        formatC(mode_val, format = "f", digits = 2),
        formatC(trimmed, format = "f", digits = 2),
        formatC(s, format = "f", digits = 2),
        formatC(v, format = "f", digits = 2),
        formatC(rng, format = "f", digits = 2),
        formatC(iqr_val, format = "f", digits = 2),
        formatC(cv, format = "f", digits = 1),
        formatC(min(vals), format = "f", digits = 2),
        formatC(q1, format = "f", digits = 2),
        formatC(q3, format = "f", digits = 2),
        formatC(max(vals), format = "f", digits = 2),
        formatC(sk, format = "f", digits = 3),
        formatC(ku, format = "f", digits = 3)
      ),
      stringsAsFactors = FALSE
    )
    stats_df
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lr")

  output$ch5_full_interpretation <- renderUI({
    d <- ch5_full_data()
    vals <- d$values
    label <- d$label

    m <- mean(vals)
    med <- median(vals)
    s <- sd(vals)
    q1 <- quantile(vals, 0.25, names = FALSE)
    q3 <- quantile(vals, 0.75, names = FALSE)
    iqr_val <- IQR(vals)
    sk <- e1071::skewness(vals)

    # Skewness description
    if (sk > 0.5) {
      skew_text <- "prawostronnie skośny"
    } else if (sk < -0.5) {
      skew_text <- "lewostronnie skośny"
    } else if (sk > 0.1) {
      skew_text <- "lekko prawostronnie skośny"
    } else if (sk < -0.1) {
      skew_text <- "lekko lewostronnie skośny"
    } else {
      skew_text <- "w przyblizeniu symetryczny"
    }

    # Mean vs median
    rel_diff <- abs(m - med) / s
    if (rel_diff < 0.1) {
      mean_med_text <- paste0(
        "Średnia (", round(m, 2), ") jest bliska medianie (", round(med, 2), ")."
      )
    } else if (m > med) {
      mean_med_text <- paste0(
        "Średnia (", round(m, 2), ") jest większa od mediany (", round(med, 2), ")."
      )
    } else {
      mean_med_text <- paste0(
        "Średnia (", round(m, 2), ") jest mniejsza od mediany (", round(med, 2), ")."
      )
    }

    # Outliers
    lower_fence <- q1 - 1.5 * iqr_val
    upper_fence <- q3 + 1.5 * iqr_val
    outliers_low <- vals[vals < lower_fence]
    outliers_high <- vals[vals > upper_fence]
    n_outliers <- length(outliers_low) + length(outliers_high)

    if (n_outliers > 0) {
      outlier_text <- paste0(
        "Wykryto ", n_outliers, " wartości odstających ",
        "(poza przedzialem [", round(lower_fence, 2), ", ",
        round(upper_fence, 2), "])."
      )
    } else {
      outlier_text <- "Brak wartości odstających (wg kryterium 1.5 * IQR)."
    }

    div(class = "callout-info",
      p(tags$strong("Automatyczna interpretacja:")),
      tags$ul(
        tags$li(paste0("Rozkład zmiennej ", label, " jest ", skew_text, ".")),
        tags$li(paste0("Typowy student ma ", tolower(label),
          " miedzy ", round(q1, 2), " a ", round(q3, 2), ".")),
        tags$li(mean_med_text),
        tags$li(outlier_text)
      )
    )
  })

  # ==========================================================================
  # CHAPTER 6 SERVER: Cheat sheet tables
  # ==========================================================================

  output$ch6_ref_table <- renderTable({
    df <- data.frame(
      a = c("Jako\u015bciowa nominalna", "Jako\u015bciowa porz\u0105dkowa",
            "Ilo\u015bciowa dyskretna", "Ilo\u015bciowa ci\u0105g\u0142a"),
      b = c("Moda, cz\u0119sto\u015bci, proporcje",
            "Moda, cz\u0119sto\u015bci, proporcje, cz\u0119sto\u015bci skumulowane",
            "\u015arednia, mediana, odch. std., kwartyle",
            "\u015arednia, mediana, odch. std., kwartyle, sko\u015bno\u015b\u0107, kurtoza"),
      c = c("S\u0142upkowy, ko\u0142owy (ostro\u017cnie!)",
            "S\u0142upkowy (z zachowaniem kolejno\u015bci)",
            "S\u0142upkowy, punktowy",
            "Histogram, g\u0119sto\u015bci, pude\u0142kowy, skrzypcowy"),
      d = c("Nie obliczaj \u015bredniej ani mediany",
            "Nie obliczaj \u015bredniej (sporne!), nie rysuj histogramu",
            "Nie rysuj wykresu g\u0119sto\u015bci (sko\u0144czona l. warto\u015bci)",
            "Nie rysuj wykresu s\u0142upkowego (zbyt wiele warto\u015bci)"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Typ zmiennej", "Odpowiednie statystyki",
                   "Odpowiednie wykresy", "Czego NIE robi\u0107")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_location_table <- renderTable({
    df <- data.frame(
      a = c("\u015arednia arytmetyczna", "Mediana",
            "\u015arednia ucinana", "Dominanta (moda)"),
      b = c("Dane symetryczne, bez outlier\u00f3w",
            "Dane sko\u015bne lub z outlierami",
            "Kompromis mi\u0119dzy \u015bredni\u0105 a median\u0105",
            "Zmienne nominalne; szukanie najcz\u0119stszej warto\u015bci"),
      c = c("Niska -- jeden outlier mo\u017ce silnie przesun\u0105\u0107",
            "Wysoka -- zale\u017cy tylko od rang",
            "\u015arednia -- ucina skrajne obserwacje",
            "Nie dotyczy (kategorie)"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Kiedy stosowa\u0107", "Odporno\u015b\u0107 na outliery")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_spread_table <- renderTable({
    df <- data.frame(
      a = c("Odchylenie standardowe (s)", "Wariancja (s\u00b2)",
            "Rozst\u0119p (Range)", "IQR", "Wsp\u00f3\u0142czynnik zmienno\u015bci (CV)"),
      b = c("Przeci\u0119tne odchylenie od \u015bredniej",
            "Kwadrat odchylenia -- w jednostkach\u00b2",
            "Max - Min (wra\u017cliwy na outliery)",
            "Rozrzut \u015brodkowych 50% danych (Q3 - Q1)",
            "Rozrzut wzgl\u0119dem \u015bredniej (%), pozwala por\u00f3wnywa\u0107 zmienne"),
      c = c("Dane symetryczne, zbli\u017cone do normalnego",
            "We wzorach (rzadko raportowana wprost)",
            "Szybka orientacja, ma\u0142e zbiory",
            "Dane sko\u015bne, z outlierami, razem z boxplotem",
            "Por\u00f3wnanie rozrzutu zmiennych o r\u00f3\u017cnych jednostkach"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Co mierzy", "Kiedy stosowa\u0107")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_shape_table <- renderTable({
    df <- data.frame(
      a = c("Sko\u015bno\u015b\u0107", "Kurtoza (excess)"),
      b = c("0 = symetryczny, >0 prawostronny, <0 lewostronny",
            "0 = normalny (mezokurtyczny), >0 ci\u0119\u017ckie ogony, <0 lekkie ogony"),
      c = c("|\u015bko\u015bno\u015b\u0107| > 1: silna asymetria, rozwa\u017c median\u0119 zamiast \u015bredniej",
            "Kurtoza > 2: cz\u0119ste warto\u015bci ekstremalne, uwa\u017caj na outliery"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Miara", "Warto\u015b\u0107 referencyjna", "Praktyczna regu\u0142a")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_plots_table <- renderTable({
    df <- data.frame(
      a = c("S\u0142upkowy (bar)", "Ko\u0142owy (pie)", "Histogram",
            "G\u0119sto\u015bci (density)", "Pude\u0142kowy (boxplot)",
            "Skrzypcowy (violin)", "Heatmapa"),
      b = c("Jako\u015bciowe (nominalne, porz\u0105dkowe)",
            "Jako\u015bciowe (tylko du\u017ce r\u00f3\u017cnice!)",
            "Ilo\u015bciowe ci\u0105g\u0142e",
            "Ilo\u015bciowe ci\u0105g\u0142e (g\u0142adka wersja histogramu)",
            "Ilo\u015bciowe (por\u00f3wnanie grup)",
            "Ilo\u015bciowe (pe\u0142ny kszta\u0142t rozk\u0142adu + por\u00f3wnanie)",
            "Tabela krzy\u017cowa (dwie zmienne jako\u015bciowe)"),
      c = c("Liczebno\u015bci / proporcje kategorii",
            "Proporcje (tylko gdy kategorie bardzo si\u0119 r\u00f3\u017cni\u0105)",
            "Kszta\u0142t rozk\u0142adu, sko\u015bno\u015b\u0107, modalno\u015b\u0107",
            "Kszta\u0142t rozk\u0142adu (bez zale\u017cno\u015bci od bin\u00f3w)",
            "Median\u0119, IQR, outliery -- kompaktowo",
            "Pe\u0142ny kszta\u0142t + median\u0119/IQR",
            "Zale\u017cno\u015bci mi\u0119dzy zmiennymi jako\u015bciowymi"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("Wykres", "Typ danych", "Pokazuje")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  output$ch6_mistakes_table <- renderTable({
    df <- data.frame(
      a = c("\u015arednia z danych nominalnych",
            "Histogram dla zmiennej dyskretnej (ma\u0142o warto\u015bci)",
            "Tylko \u015brednia bez miary rozrzutu",
            "Pie chart dla podobnych warto\u015bci",
            "Ignorowanie outlier\u00f3w",
            "Dominanta dla danych ci\u0105g\u0142ych",
            "SD bez sprawdzenia symetrii"),
      b = c("Kategorie nie maj\u0105 warto\u015bci liczbowej -- wynik jest bezsensowny",
            "Biny \u0142\u0105cz\u0105 warto\u015bci, kt\u00f3re powinny by\u0107 osobno -- u\u017cyj wykresu s\u0142upkowego",
            "Dwie grupy z t\u0105 sam\u0105 \u015bredni\u0105 mog\u0105 mie\u0107 zupe\u0142nie r\u00f3\u017cny rozrzut",
            "Ludzkie oko nie odr\u00f3\u017cnia k\u0105t\u00f3w r\u00f3\u017cni\u0105cych si\u0119 o 2-3%",
            "Jeden outlier mo\u017ce przesun\u0105\u0107 \u015bredni\u0105 i zwi\u0119kszy\u0107 SD",
            "Prawie ka\u017cda warto\u015b\u0107 wyst\u0119puje 1-2 razy -- moda jest przypadkowa",
            "Przy silnej sko\u015bno\u015bci SD s\u0142abo opisuje rozrzut -- lepsza jest IQR"),
      c = c("U\u017cyj dominanty (mody) i tabeli cz\u0119sto\u015bci",
            "U\u017cyj wykresu s\u0142upkowego (geom_col / geom_bar)",
            "Zawsze raportuj \u015bredni\u0105 + SD lub median\u0119 + IQR",
            "U\u017cyj wykresu s\u0142upkowego",
            "Raportuj median\u0119 + IQR obok \u015bredniej + SD",
            "U\u017cyj \u015bredniej i mediany",
            "Sprawd\u017a sko\u015bno\u015b\u0107; je\u015bli |skew| > 1, raportuj median\u0119 + IQR"),
      stringsAsFactors = FALSE
    )
    names(df) <- c("B\u0142\u0105d", "Dlaczego to b\u0142\u0105d", "Co zrobi\u0107 zamiast")
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

} # end server

shinyApp(ui = ui, server = server)
