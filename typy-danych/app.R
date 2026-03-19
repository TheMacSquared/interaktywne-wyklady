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
  )) # end column / fluidRow
  ) # end ch2 tabPanel

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

} # end server

shinyApp(ui = ui, server = server)
