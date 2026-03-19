# Statystyki opisowe - interaktywny przewodnik
# Narzedzie do nauczania typow danych, statystyk opisowych i wizualizacji

library(shiny)
library(ggplot2)
library(dplyr)
library(e1071)  # for skewness, kurtosis

# ============================================================================
# KOLORY
# ============================================================================

col_nominal    <- "#e74c3c"
col_ordinal    <- "#f39c12"
col_discrete   <- "#3498db"
col_continuous <- "#27ae60"
col_dark       <- "#2c3e50"

type_colors <- c(
  "nominalna"          = col_nominal,
  "porzadkowa"         = col_ordinal,
  "ilosciowa_dyskretna" = col_discrete,
  "ilosciowa_ciagla"   = col_continuous
)

type_labels <- c(
  "nominalna"          = "Jakosciowa nominalna",
  "porzadkowa"         = "Jakosciowa porzadkowa",
  "ilosciowa_dyskretna" = "Ilosciowa dyskretna",
  "ilosciowa_ciagla"   = "Ilosciowa ciagla"
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
  ocena_wykladowcy = factor(
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
  plec = list(
    label = "Plec",
    type = "nominalna",
    description = "Plec to zmienna nominalna - kategorie nie maja naturalnego porzadku. Nie mozna powiedziec, ze 'Kobieta' > 'Mezczyzna' ani odwrotnie.",
    bad_stats_explanation = "Srednia plci nie ma sensu - kategorie nie sa liczbami. Nawet jesli zakodujemy je jako 1 i 2, wynik (np. 1.45) nie ma interpretacji."
  ),
  kierunek = list(
    label = "Kierunek studiow",
    type = "nominalna",
    description = "Kierunek studiow to zmienna nominalna - rozne kategorie bez naturalnego porzadku. Mozemy liczyc czestosci, ale nie srednia.",
    bad_stats_explanation = "Srednia kierunku? To tak jakby policzyc srednia z 'Informatyki' i 'Biologii'. Bezsensowne."
  ),
  grupa_krwi = list(
    label = "Grupa krwi",
    type = "nominalna",
    description = "Grupa krwi to klasyczna zmienna nominalna - etykiety kategorii (A, B, AB, 0) nie maja porzadku ani wartosci liczbowej.",
    bad_stats_explanation = "Nie mozna policzyc sredniej grupy krwi. Mediana tez nie ma sensu - nie ma porzadku miedzy grupami."
  ),
  rok_studiow = list(
    label = "Rok studiow",
    type = "porzadkowa",
    description = "Rok studiow ma naturalny porzadek (1 < 2 < 3 < 4 < 5), ale roznice miedzy latami nie musza byc 'rowne' pod wzgledem doswiadczenia.",
    bad_stats_explanation = "Srednia roku studiow (np. 2.3) jest dyskusyjna. Mozna ja policzyc, ale czy roznica miedzy 1. a 2. rokiem jest taka sama jak miedzy 4. a 5.?"
  ),
  zadowolenie = list(
    label = "Zadowolenie ze studiow",
    type = "porzadkowa",
    description = "Skala Likerta (5 poziomow) - jest porzadek od 'Bardzo niezadowolony' do 'Bardzo zadowolony', ale odleglosci miedzy kategoriami nie sa rowne.",
    bad_stats_explanation = "Srednia z Likerta to czesty blad! Czy roznica miedzy 'Niezadowolony' a 'Neutralny' jest taka sama jak miedzy 'Zadowolony' a 'Bardzo zadowolony'?"
  ),
  liczba_kursow = list(
    label = "Liczba kursow w semestrze",
    type = "ilosciowa_dyskretna",
    description = "Liczba kursow to zmienna ilosciowa dyskretna - przyjmuje wartosci calkowite, mozna liczyc srednia i odchylenie standardowe.",
    bad_stats_explanation = "Tu wlasciwie wszystkie statystyki maja sens! Ale tabela czestosci jest lepsza niz histogram z wieloma przedzialami."
  ),
  liczba_nieobecnosci = list(
    label = "Liczba nieobecnosci",
    type = "ilosciowa_dyskretna",
    description = "Zliczenia (count data) - klasyczna zmienna dyskretna. Przyjmuje wartosci 0, 1, 2, ... Mozna liczyc srednia, ale rozklad jest czesto skosny.",
    bad_stats_explanation = "Statystyki maja sens, ale uwaga - rozklad jest skosny, wiec mediana moze byc lepsza niz srednia."
  ),
  wzrost = list(
    label = "Wzrost (cm)",
    type = "ilosciowa_ciagla",
    description = "Wzrost to zmienna ilosciowa ciagla - moze przyjac dowolna wartosc z pewnego zakresu. Wszystkie statystyki opisowe maja sens.",
    bad_stats_explanation = "Dla zmiennych ciaglych wszystkie statystyki sa poprawne. Tabela czestosci jest bezuzyteczna - prawie kazda wartosc jest unikalna."
  ),
  srednia_ocen = list(
    label = "Srednia ocen",
    type = "ilosciowa_ciagla",
    description = "Srednia ocen to zmienna ciagla (mimo ze ograniczona do 2.0-5.0). Mozna liczyc srednia, mediane, odchylenie standardowe.",
    bad_stats_explanation = "Statystyki opisowe sa tu jak najbardziej na miejscu. Mozna tez wizualizowac histogramem lub boxplotem."
  ),
  czas_dojazdu = list(
    label = "Czas dojazdu (min)",
    type = "ilosciowa_ciagla",
    description = "Czas dojazdu to zmienna ciagla, czesto o skosnym rozkladzie. Ciekawy przyklad, gdzie mediana moze byc lepsza niz srednia.",
    bad_stats_explanation = "Wszystkie statystyki poprawne, ale warto zwrocic uwage na skosnosc - srednia moze zawyzyc typowy czas dojazdu."
  ),
  waga = list(
    label = "Waga (kg)",
    type = "ilosciowa_ciagla",
    description = "Waga to zmienna ilosciowa ciagla, skorelowana ze wzrostem. Moze przyjac dowolna wartosc z pewnego zakresu. Wszystkie statystyki opisowe maja sens.",
    bad_stats_explanation = "Dla zmiennych ciaglych wszystkie statystyki sa poprawne. Tabela czestosci jest bezuzyteczna - prawie kazda wartosc jest unikalna."
  ),
  ocena_wykladowcy = list(
    label = "Ocena wykladowcy (1-10)",
    type = "porzadkowa",
    description = "Ocena wykladowcy na skali 1-10 to zmienna porzadkowa - jest naturalny porzadek, ale odleglosci miedzy ocenami nie musza byc rowne.",
    bad_stats_explanation = "Srednia z ocen na skali porzadkowej jest dyskusyjna. Czy roznica miedzy 3 a 4 jest taka sama jak miedzy 8 a 9?"
  )
)

# ============================================================================
# FUNKCJE POMOCNICZE
# ============================================================================

# Diagram taksonomii typow danych
render_taxonomy <- function(highlight = NULL, revealed = character(0)) {
  # Pozycje wezlow
  nodes <- data.frame(
    id = c("dane", "ilosciowe", "jakosciowe",
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

  # Kolory
  nodes$fill <- sapply(nodes$type, function(t) {
    if (is.na(t)) return("#ecf0f1")
    type_colors[t]
  })
  nodes$alpha <- sapply(nodes$type, function(t) {
    if (is.null(highlight) || is.na(t)) return(1)
    if (t == highlight) return(1) else return(0.3)
  })

  # Krawedzie
  edges <- data.frame(
    x = c(5, 5, 2.5, 2.5, 7.5, 7.5),
    xend = c(2.5, 7.5, 1.25, 3.75, 6.25, 8.75),
    # Ilosciowe (lewo): ciagla(1.25), dyskretna(3.75)
    # Jakosciowe (prawo): porzadkowa(6.25), nominalna(8.75)
    y = c(2.75, 2.75, 1.75, 1.75, 1.75, 1.75),
    yend = c(2.25, 2.25, 1.25, 1.25, 1.25, 1.25)
  )

  box_w <- 1.1
  box_h <- 0.2

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
    # Przyklady - tylko odkryte wezly
    geom_text(data = nodes %>% filter(id %in% revealed),
              aes(x = x, y = y - 0.35, label = example),
              size = 3.5, color = "#7f8c8d", lineheight = 0.9) +
    # Znak "?" dla zakrytych wezlow
    geom_text(data = nodes %>% filter(example != "", !id %in% revealed),
              aes(x = x, y = y - 0.35, label = "kliknij aby odkryc"),
              size = 3, color = "#bdc3c7", fontface = "italic") +
    coord_cartesian(xlim = c(-0.2, 10.2), ylim = c(0.3, 3.5)) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10))
}

# Statystyki opisowe odpowiednie dla danego typu
compute_good_stats <- function(x, type) {
  if (type %in% c("nominalna", "porzadkowa")) {
    freq <- as.data.frame(table(x))
    names(freq) <- c("Wartosc", "Liczebnosc")
    freq$Procent <- round(100 * freq$Liczebnosc / sum(freq$Liczebnosc), 1)
    mode_val <- as.character(freq$Wartosc[which.max(freq$Liczebnosc)])

    stats_text <- paste0(
      "Liczba obserwacji: ", length(x), "\n",
      "Liczba kategorii: ", nlevels(factor(x)), "\n",
      "Dominanta (moda): ", mode_val
    )
    if (type == "porzadkowa") {
      stats_text <- paste0(stats_text, "\n",
                           "Mediana: ", median(as.numeric(x)))
    }
    list(table = freq, text = stats_text)
  } else {
    x_num <- as.numeric(x)
    stats <- data.frame(
      Statystyka = c("Liczba obserwacji", "Srednia", "Mediana",
                     "Odchylenie std.", "Minimum", "Maksimum",
                     "Q1 (25%)", "Q3 (75%)", "IQR"),
      Wartosc = c(
        length(x_num),
        round(mean(x_num, na.rm = TRUE), 2),
        round(median(x_num, na.rm = TRUE), 2),
        round(sd(x_num, na.rm = TRUE), 2),
        round(min(x_num, na.rm = TRUE), 2),
        round(max(x_num, na.rm = TRUE), 2),
        round(quantile(x_num, 0.25, na.rm = TRUE), 2),
        round(quantile(x_num, 0.75, na.rm = TRUE), 2),
        round(IQR(x_num, na.rm = TRUE), 2)
      ),
      stringsAsFactors = FALSE
    )
    list(table = stats, text = NULL)
  }
}

# Wykres odpowiedni dla typu
render_good_plot <- function(x, label, type) {
  col <- type_colors[type]
  df <- data.frame(x = x)

  if (type %in% c("nominalna", "porzadkowa")) {
    ggplot(df, aes(x = x)) +
      geom_bar(fill = col, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4.5) +
      labs(title = paste0("Wykres slupkowy: ", label),
           x = label, y = "Liczebnosc") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = if (nlevels(factor(x)) > 4) 30 else 0,
                                        hjust = 1))
  } else if (type == "ilosciowa_dyskretna") {
    ggplot(df, aes(x = factor(x))) +
      geom_bar(fill = col, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4.5) +
      labs(title = paste0("Wykres slupkowy: ", label),
           x = label, y = "Liczebnosc") +
      theme_minimal(base_size = 14)
  } else {
    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 20, fill = col, color = "white", alpha = 0.7) +
      geom_density(color = col, linewidth = 1.2) +
      labs(title = paste0("Histogram z gestosc: ", label),
           x = label, y = "Gestosc") +
      theme_minimal(base_size = 14)
  }
}

# Boxplot dla zmiennych ilosciowych
render_boxplot <- function(x, label, type) {
  col <- type_colors[type]
  df <- data.frame(x = as.numeric(x))
  ggplot(df, aes(y = x)) +
    geom_boxplot(fill = col, alpha = 0.5, color = col_dark, width = 0.4) +
    coord_flip() +
    labs(title = paste0("Boxplot: ", label), y = label, x = "") +
    theme_minimal(base_size = 14) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

# "Zly" wykres - nieodpowiedni dla danego typu
render_bad_plot <- function(x, label, type) {
  df <- data.frame(x = x)

  if (type %in% c("nominalna", "porzadkowa")) {
    # Histogram zmiennej kategorycznej - bezsens
    df$x_num <- as.numeric(factor(x))
    ggplot(df, aes(x = x_num)) +
      geom_histogram(bins = 10, fill = "#95a5a6", color = "white") +
      labs(title = paste0("Histogram (NIEODPOWIEDNI): ", label),
           subtitle = "Histogram wymaga danych liczbowych - tu mamy kategorie!",
           x = paste0(label, " (zakodowane jako liczby)"), y = "Liczebnosc") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(color = "#e74c3c"),
            plot.subtitle = element_text(color = "#e74c3c", face = "italic"))
  } else {
    # Wykres slupkowy zmiennej ciaglej - za duzo kategorii
    df$x_cut <- as.character(x)
    n_unique <- length(unique(x))
    ggplot(df, aes(x = x)) +
      geom_bar(fill = "#95a5a6", color = "white") +
      labs(title = paste0("Wykres slupkowy (NIEODPOWIEDNI): ", label),
           subtitle = paste0(n_unique, " unikalnych wartosci - wykres slupkowy jest nieczytelny!"),
           x = label, y = "Liczebnosc") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(color = "#e74c3c"),
            plot.subtitle = element_text(color = "#e74c3c", face = "italic"),
            axis.text.x = element_text(size = 5, angle = 90))
  }
}

# "Zle" statystyki
compute_bad_stats <- function(x, type) {
  if (type == "nominalna") {
    x_num <- as.numeric(factor(x))
    paste0(
      "Srednia (po zakodowaniu jako liczby): ", round(mean(x_num), 2), "\n",
      "Odchylenie std.: ", round(sd(x_num), 2), "\n",
      "Mediana: ", round(median(x_num), 2), "\n\n",
      "Te liczby NIE MAJA SENSU! Zaleza od arbitralnego\n",
      "kodowania kategorii (np. A=1, B=2, ...)."
    )
  } else if (type == "porzadkowa") {
    x_num <- as.numeric(x)
    paste0(
      "Srednia (po zakodowaniu jako liczby): ", round(mean(x_num), 2), "\n",
      "Odchylenie std.: ", round(sd(x_num), 2), "\n\n",
      "Dyskusyjne! Zaklada rowne odleglosci miedzy\n",
      "kategoriami, co nie musi byc prawda."
    )
  } else if (type == "ilosciowa_ciagla") {
    freq <- as.data.frame(table(x))
    paste0(
      "Tabela czestosci: ", nrow(freq), " unikalnych wartosci\n",
      "(prawie kazda wartosc wystepuje tylko raz)\n\n",
      "Tabela czestosci dla zmiennej ciaglej jest\n",
      "bezuzyteczna - lepiej uzyc histogramu."
    )
  } else {
    "Dla tej zmiennej wiekszosc statystyk jest poprawna."
  }
}

# ============================================================================
# UI
# ============================================================================

ui <- navbarPage(
  "Statystyki opisowe - interaktywny przewodnik",
  id = "main_nav",

  # CSS in header tag
  header = tags$head(tags$style(HTML("
    .type-badge {
      display: inline-block;
      padding: 8px 16px;
      border-radius: 20px;
      color: white;
      font-weight: bold;
      font-size: 16px;
      margin: 10px 0;
    }
    .bad-box {
      border: 2px solid #e74c3c;
      border-radius: 8px;
      padding: 15px;
      margin-top: 15px;
      background-color: #fdf2f2;
    }
    .bad-box h4 { color: #e74c3c; margin-top: 0; }
    .good-box {
      border: 2px solid #27ae60;
      border-radius: 8px;
      padding: 15px;
      background-color: #f2fdf5;
    }
    .good-box h4 { color: #27ae60; margin-top: 0; }
    .desc-box {
      background-color: #f8f9fa;
      padding: 12px;
      border-radius: 5px;
      border-left: 4px solid #3498db;
      margin: 10px 0;
    }
    .summary-cell {
      padding: 8px 12px;
      border-radius: 5px;
      margin: 3px;
      color: white;
      font-weight: bold;
    }
    .tab-content { padding-top: 15px; }
  "))),

  # ========================================================================
  # SECTION 1: TYPY DANYCH
  # ========================================================================
  tabPanel("1. Typy danych",
    tabsetPanel(id = "s1_tabs",

      # --------------------------------------------------------------------
      # Tab 1.1: Taksonomia
      # --------------------------------------------------------------------
      tabPanel("Taksonomia",
        br(),
        fluidRow(
          column(12,
            plotOutput("taxonomy_plot", height = "420px", click = "taxonomy_click"),
            fluidRow(
              column(6, actionButton("reveal_all", "Odkryj wszystkie", class = "btn-outline-primary btn-sm")),
              column(6, actionButton("hide_all", "Ukryj wszystkie", class = "btn-outline-secondary btn-sm"))
            )
          )
        )
      ),

      # --------------------------------------------------------------------
      # Tab 1.2: Eksploracja zmiennej
      # --------------------------------------------------------------------
      tabPanel("Eksploracja zmiennej",
        sidebarLayout(
          sidebarPanel(
            h4("Wybierz zmienna"),
            selectInput("selected_var", "Zmienna:",
                        choices = setNames(
                          names(variable_meta),
                          sapply(variable_meta, function(m) m$label)
                        ),
                        selected = "plec"),

            uiOutput("type_badge"),

            div(class = "desc-box",
              textOutput("var_description")
            ),

            hr(),

            checkboxInput("show_bad", "Pokaz NIEODPOWIEDNIE statystyki/wykresy", value = FALSE),

            hr(),

            h5("Dane (pierwsze 10 wartosci):"),
            verbatimTextOutput("data_preview"),

            width = 3
          ),
          mainPanel(
            div(class = "good-box",
              h4("Odpowiedni wykres"),
              plotOutput("good_plot", height = "350px"),
              conditionalPanel(
                condition = "output.show_boxplot",
                plotOutput("boxplot", height = "150px")
              )
            ),

            br(),

            fluidRow(
              column(6,
                div(class = "good-box",
                  h4("Odpowiednie statystyki"),
                  verbatimTextOutput("good_stats_text"),
                  tableOutput("good_stats_table")
                )
              ),
              column(6,
                conditionalPanel(
                  condition = "input.show_bad == true",
                  div(class = "bad-box",
                    h4("NIEODPOWIEDNIE statystyki"),
                    verbatimTextOutput("bad_stats_text")
                  )
                )
              )
            ),

            conditionalPanel(
              condition = "input.show_bad == true",
              br(),
              div(class = "bad-box",
                h4("NIEODPOWIEDNI wykres"),
                plotOutput("bad_plot", height = "300px")
              )
            ),

            width = 9
          )
        )
      ),

      # --------------------------------------------------------------------
      # Tab 1.3: Porownanie zmiennych
      # --------------------------------------------------------------------
      tabPanel("Porownanie zmiennych",
        br(),
        fluidRow(
          column(6,
            wellPanel(
              selectInput("compare_var1", "Zmienna 1:",
                          choices = setNames(
                            names(variable_meta),
                            sapply(variable_meta, function(m) m$label)
                          ),
                          selected = "plec"),
              uiOutput("compare_badge1"),
              plotOutput("compare_plot1", height = "300px"),
              tableOutput("compare_stats1")
            )
          ),
          column(6,
            wellPanel(
              selectInput("compare_var2", "Zmienna 2:",
                          choices = setNames(
                            names(variable_meta),
                            sapply(variable_meta, function(m) m$label)
                          ),
                          selected = "wzrost"),
              uiOutput("compare_badge2"),
              plotOutput("compare_plot2", height = "300px"),
              tableOutput("compare_stats2")
            )
          )
        ),
        fluidRow(
          column(12,
            div(class = "desc-box",
              h4("Porownanie"),
              uiOutput("compare_summary")
            )
          )
        )
      ),

      # --------------------------------------------------------------------
      # Tab 1.4: Sciaga
      # --------------------------------------------------------------------
      tabPanel("Sciaga",
        br(),
        h4("Sciaga: typ danych -> narzedzia analizy"),
        tableOutput("summary_table"),
        hr(),
        h4("Podglad zbioru danych: Ankieta studencka (n = 200)"),
        div(style = "overflow-x: auto;",
          tableOutput("dataset_preview")
        )
      )
    )
  ),

tabPanel("2. Zmienne jakosciowe",
  tabsetPanel(id = "s2_tabs",

    # ====================================================================
    # Tab 2.1: Tabela czestosci
    # ====================================================================
    tabPanel("Tabela czestosci",
      sidebarLayout(
        sidebarPanel(
          h4("Budowanie tabeli czestosci"),
          selectInput("s2_freq_var", "Wybierz zmienna:",
                      choices = c(
                        "Plec" = "plec",
                        "Kierunek studiow" = "kierunek",
                        "Grupa krwi" = "grupa_krwi",
                        "Rok studiow" = "rok_studiow",
                        "Zadowolenie" = "zadowolenie",
                        "Ocena wykladowcy" = "ocena_wykladowcy"
                      ),
                      selected = "kierunek"),
          hr(),
          h5("Kroki budowania tabeli:"),
          actionButton("s2_freq_step1", "1. Surowe dane",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("s2_freq_step2", "2. Zliczanie",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("s2_freq_step3", "3. Czestosci wzgledne",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("s2_freq_step4", "4. Skumulowane",
                       class = "btn-outline-primary", width = "100%"),
          hr(),
          actionButton("s2_freq_reset", "Reset",
                       class = "btn-outline-secondary", width = "100%"),
          width = 3
        ),
        mainPanel(
          uiOutput("s2_freq_explanation"),
          br(),
          tableOutput("s2_freq_table"),
          width = 9
        )
      )
    ),

    # ====================================================================
    # Tab 2.2: Wizualizacje
    # ====================================================================
    tabPanel("Wizualizacje",
      sidebarLayout(
        sidebarPanel(
          selectInput("s2_viz_var", "Wybierz zmienna:",
                      choices = c(
                        "Plec" = "plec",
                        "Kierunek studiow" = "kierunek",
                        "Grupa krwi" = "grupa_krwi",
                        "Rok studiow" = "rok_studiow",
                        "Zadowolenie" = "zadowolenie",
                        "Ocena wykladowcy" = "ocena_wykladowcy"
                      ),
                      selected = "kierunek"),
          radioButtons("s2_viz_sort", "Sortowanie:",
                       choices = c("Oryginalna kolejnosc" = "original",
                                   "Posortowane malejaco" = "descending"),
                       selected = "original"),
          radioButtons("s2_viz_orient", "Orientacja slupkow:",
                       choices = c("Pionowe" = "vertical",
                                   "Poziome" = "horizontal"),
                       selected = "vertical"),
          hr(),
          h4("Manipulacja kolorami"),
          selectInput("s2_color_palette", "Paleta kolorow:",
                      choices = c(
                        "Neutralna" = "neutral",
                        "Ciepla (manipulacja)" = "warm",
                        "Zimna (manipulacja)" = "cold",
                        "Jaskrawa vs szara" = "biased",
                        "Losowa" = "random"
                      ),
                      selected = "neutral"),
          actionButton("s2_randomize_colors", "Losuj kolory",
                       class = "btn-outline-warning", width = "100%"),
          hr(),
          checkboxInput("s2_show_quiz", "Pokaz quiz", value = FALSE),
          width = 3
        ),
        mainPanel(
          h4("Porownanie wizualizacji"),
          fluidRow(
            column(4, plotOutput("s2_bar_plot", height = "350px")),
            column(4, plotOutput("s2_pie_plot", height = "350px")),
            column(4, plotOutput("s2_lollipop_plot", height = "350px"))
          ),
          br(),
          div(class = "bad-box",
            h4("Dlaczego pie chart klamie"),
            uiOutput("s2_pie_critique")
          ),
          br(),
          conditionalPanel(
            condition = "input.s2_show_quiz == true",
            div(class = "desc-box",
              h4("Quiz: Ktora kategoria jest najwieksza?"),
              fluidRow(
                column(6, plotOutput("s2_quiz_pie", height = "300px")),
                column(6, plotOutput("s2_quiz_bar", height = "300px"))
              ),
              uiOutput("s2_quiz_answer")
            )
          ),
          width = 9
        )
      )
    ),

    # ====================================================================
    # Tab 2.3: Tabela krzyzowa
    # ====================================================================
    tabPanel("Tabela krzyzowa",
      sidebarLayout(
        sidebarPanel(
          selectInput("s2_cross_var1", "Zmienna 1 (wiersze):",
                      choices = c(
                        "Plec" = "plec",
                        "Kierunek studiow" = "kierunek",
                        "Grupa krwi" = "grupa_krwi",
                        "Rok studiow" = "rok_studiow",
                        "Zadowolenie" = "zadowolenie",
                        "Ocena wykladowcy" = "ocena_wykladowcy"
                      ),
                      selected = "plec"),
          selectInput("s2_cross_var2", "Zmienna 2 (kolumny):",
                      choices = c(
                        "Plec" = "plec",
                        "Kierunek studiow" = "kierunek",
                        "Grupa krwi" = "grupa_krwi",
                        "Rok studiow" = "rok_studiow",
                        "Zadowolenie" = "zadowolenie",
                        "Ocena wykladowcy" = "ocena_wykladowcy"
                      ),
                      selected = "kierunek"),
          radioButtons("s2_cross_type", "Typ tabeli:",
                       choices = c(
                         "Liczebnosci" = "count",
                         "% wierszowe" = "row",
                         "% kolumnowe" = "col",
                         "% calkowite" = "total"
                       ),
                       selected = "count"),
          width = 3
        ),
        mainPanel(
          div(class = "desc-box",
            uiOutput("s2_cross_explanation")
          ),
          br(),
          tableOutput("s2_cross_table"),
          br(),
          plotOutput("s2_cross_plot", height = "400px"),
          width = 9
        )
      )
    ),

    # ====================================================================
    # Tab 2.4: Moda (dominanta)
    # ====================================================================
    tabPanel("Moda",
      sidebarLayout(
        sidebarPanel(
          selectInput("s2_mode_var", "Wybierz zmienna:",
                      choices = setNames(
                        names(variable_meta),
                        sapply(variable_meta, function(m) m$label)
                      ),
                      selected = "kierunek"),
          actionButton("s2_mode_generate", "Generuj nowe dane",
                       class = "btn-primary", width = "100%"),
          width = 3
        ),
        mainPanel(
          div(class = "desc-box",
            uiOutput("s2_mode_explanation")
          ),
          br(),
          plotOutput("s2_mode_plot", height = "350px"),
          br(),
          verbatimTextOutput("s2_mode_stats"),
          width = 9
        )
      )
    )

  ) # end tabsetPanel
), # end tabPanel
tabPanel("3. Statystyki polozenia",
  tabsetPanel(id = "s3_tabs",

    # --- Tab 3.1: Srednia vs Mediana ---
    tabPanel("Srednia vs Mediana",
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Wybierz scenariusz"),
          selectInput("svm_scenario", "Scenariusz:",
            choices = c("Zarobki w firmie" = "zarobki",
                        "Wyniki egzaminu" = "egzamin",
                        "Czas dojazdu do pracy" = "dojazd",
                        "Ceny mieszkan" = "mieszkania",
                        "Czas odpowiedzi email" = "email")),
          hr(),
          h4("Dodaj nowa obserwacje"),
          uiOutput("svm_value_slider"),
          actionButton("svm_add_value", "Dodaj wartosc", class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton("svm_add_outlier", "Dodaj outlier", class = "btn-warning", width = "100%"),
          br(), br(),
          actionButton("svm_reset", "Reset do poczatku", class = "btn-danger", width = "100%"),
          hr(),
          h4("Statystyki"),
          div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
            h5(textOutput("svm_mean_text"), style = "color: #e74c3c; margin: 5px 0;"),
            h5(textOutput("svm_median_text"), style = "color: #3498db; margin: 5px 0;"),
            h5(textOutput("svm_diff_text"), style = "color: #2c3e50; margin: 5px 0; font-weight: bold;")
          )
        ),
        mainPanel(width = 9,
          plotOutput("svm_histogram", height = "400px"),
          br(),
          plotOutput("svm_stripplot", height = "150px")
        )
      )
    ),

    # --- Tab 3.2: Odpornosc statystyk ---
    tabPanel("Odpornosc statystyk",
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Dane bazowe"),
          selectInput("s3_rob_source", "Zrodlo danych:",
            choices = c("Zarobki (skosne)" = "salary",
                        "Egzamin (normalne)" = "exam",
                        "Z ankiety: wzrost" = "survey_wzrost",
                        "Z ankiety: czas dojazdu" = "survey_czas")),
          hr(),
          h4("Dodaj wartosci odstajace"),
          sliderInput("s3_rob_outlier_val", "Wartosc outlieru:",
            min = 0, max = 100, value = 50),
          actionButton("s3_rob_add", "Dodaj outlier", class = "btn-warning", width = "100%"),
          actionButton("s3_rob_add5", "Dodaj 5 outlierow", class = "btn-danger", width = "100%"),
          actionButton("s3_rob_reset", "Reset", class = "btn-outline-secondary", width = "100%"),
          hr(),
          h4("Srednia ucinana"),
          sliderInput("s3_rob_trim", "Procent ucinania:",
            min = 0, max = 0.4, value = 0.1, step = 0.05),
          hr(),
          div(class = "desc-box", uiOutput("s3_rob_info"))
        ),
        mainPanel(width = 9,
          plotOutput("s3_rob_plot", height = "400px"),
          br(),
          tableOutput("s3_rob_table")
        )
      )
    ),

    # --- Tab 3.3: Kwantyle i percentyle ---
    tabPanel("Kwantyle i percentyle",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("s3_q_var", "Zmienna:",
            choices = c("Wzrost" = "wzrost",
                        "Srednia ocen" = "srednia_ocen",
                        "Czas dojazdu" = "czas_dojazdu",
                        "Waga" = "waga")),
          hr(),
          sliderInput("s3_q_percentile", "Percentyl:",
            min = 0, max = 100, value = 50, step = 1, post = "%"),
          hr(),
          h4("Szybkie wybory"),
          actionButton("s3_q_q1", "Q1 (25%)", class = "btn-outline-primary btn-sm", width = "100%"),
          actionButton("s3_q_med", "Mediana (50%)", class = "btn-outline-danger btn-sm", width = "100%"),
          actionButton("s3_q_q3", "Q3 (75%)", class = "btn-outline-primary btn-sm", width = "100%"),
          hr(),
          div(class = "desc-box", uiOutput("s3_q_info"))
        ),
        mainPanel(width = 9,
          plotOutput("s3_q_hist_plot", height = "350px"),
          plotOutput("s3_q_box_plot", height = "120px"),
          uiOutput("s3_q_interpretation")
        )
      )
    )
  )
),
tabPanel("4. Statystyki rozrzutu",
  tabsetPanel(id = "s4_tabs",

    # ---- Tab 4.1: Srednia to nie wszystko ----
    tabPanel("Srednia to nie wszystko",
      sidebarLayout(
        sidebarPanel(
          h4("Narracja"),
          actionButton("spread_step1", "1. Dwie linie autobusowe", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("spread_step2", "2. Ta sama srednia, ale...", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("spread_step3", "3. Wychodzisz wczesniej", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("spread_step4", "4. Konsekwencje", class = "btn-outline-primary", width = "100%"),
          hr(),
          uiOutput("spread_controls"),
          hr(),
          selectInput("spread_scenario", "Scenariusz:",
                      choices = c("Autobusy (spoznie)" = "autobusy",
                                  "Produkcja (srubki)" = "produkcja",
                                  "Egzamin (wyniki)" = "egzamin")),
          actionButton("spread_reset", "Od poczatku", class = "btn-outline-secondary btn-sm", width = "100%"),
          width = 3
        ),
        mainPanel(
          uiOutput("spread_story"),
          plotOutput("spread_density_plot", height = "450px"),
          uiOutput("spread_bottom"),
          width = 9
        )
      )
    ),

    # ---- Tab 4.2: SD krok po kroku ----
    tabPanel("SD krok po kroku",
      sidebarLayout(
        sidebarPanel(
          h4("Wybor danych"),
          selectInput("sd_scenario", "Typ danych:",
                      choices = c("Bardzo skupione (SD ~ 2)" = "skupione",
                                  "Umiarkowanie rozproszone (SD ~ 5)" = "umiarkowane",
                                  "Bardzo rozproszone (SD ~ 10)" = "rozproszone")),
          actionButton("sd_regenerate", "Losuj nowy zestaw", class = "btn-success", width = "100%"),
          hr(),
          h4("Kroki obliczania SD"),
          actionButton("sd_step1", "1. Pokaz surowe dane", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("sd_step2", "2. Oblicz srednia", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("sd_step3", "3. Odleglosci i kwadraty", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("sd_step4", "4. Odchylenie standardowe", class = "btn-outline-primary", width = "100%"),
          hr(),
          actionButton("sd_reset", "Reset", class = "btn-danger btn-sm", width = "100%"),
          hr(),
          h4("Statystyki"),
          div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
            h5(textOutput("sd_n_text")),
            h5(textOutput("sd_mean_text"), style = "color: #e74c3c;"),
            h5(textOutput("sd_sd_text"), style = "color: #3498db; font-weight: bold;")
          ),
          width = 3
        ),
        mainPanel(
          div(
            style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
            h4(textOutput("sd_step_title"), style = "color: #2c3e50;"),
            plotOutput("sd_main_plot", height = "400px"),
            div(
              style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
              textOutput("sd_step_explanation")
            )
          ),
          div(
            style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
            h4("Obliczenia", style = "color: #7f8c8d;"),
            tableOutput("sd_calculations_table")
          ),
          width = 9
        )
      )
    ),

    # ---- Tab 4.3: Budowa boxplota ----
    tabPanel("Budowa boxplota",
      sidebarLayout(
        sidebarPanel(
          h4("Wybor danych"),
          selectInput("bp_scenario", "Scenariusz:",
                      choices = c("Autobusy" = "autobusy",
                                  "Kac po alkoholu" = "kac",
                                  "Zakupy w sklepie" = "sklep")),
          sliderInput("bp_n_obs", "Liczba obserwacji:",
                      min = 10, max = 100, value = 30, step = 5),
          actionButton("bp_draw_new", "Losuj nowe dane", class = "btn-success", width = "100%"),
          hr(),
          h4("Kroki budowy box plotu"),
          actionButton("bp_step1", "1. Surowe dane", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("bp_step2", "2. Sortuj", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("bp_step3", "3. Mediana Q2", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("bp_step4", "4. Q1 i Q3", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("bp_step5", "5. IQR", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("bp_step6", "6. Wasy", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("bp_step7", "7. Outliery", class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("bp_step8", "8. Box plot", class = "btn-outline-primary", width = "100%"),
          hr(),
          actionButton("bp_reset", "Reset", class = "btn-danger btn-sm", width = "100%"),
          hr(),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; font-size: 12px;",
            p(strong("Q1:"), "25. percentyl"),
            p(strong("Q2 (mediana):"), "50. percentyl"),
            p(strong("Q3:"), "75. percentyl"),
            p(strong("IQR:"), "Q3 - Q1"),
            p(strong("Wasy:"), "1.5 x IQR"),
            p(strong("Outliery:"), "Poza wasami")
          ),
          width = 3
        ),
        mainPanel(
          div(
            style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
            h4(textOutput("bp_step_title"), style = "color: #2c3e50;"),
            plotOutput("bp_main_plot", height = "300px"),
            div(
              style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
              textOutput("bp_step_explanation")
            )
          ),
          div(
            style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
            h4("Histogram tych samych danych", style = "color: #7f8c8d;"),
            plotOutput("bp_histogram", height = "250px")
          ),
          width = 9
        )
      )
    ),

    # ---- Tab 4.4: Porownanie miar rozrzutu ----
    tabPanel("Porownanie miar",
      sidebarLayout(
        sidebarPanel(
          selectInput("s4_comp_var", "Zmienna:",
                      choices = c("Wzrost (cm)" = "wzrost",
                                  "Waga (kg)" = "waga",
                                  "Czas dojazdu (min)" = "czas_dojazdu",
                                  "Srednia ocen" = "srednia_ocen")),
          hr(),
          h4("Dodaj outlier"),
          sliderInput("s4_comp_outlier", "Wartosc:", min = 0, max = 300, value = 200),
          actionButton("s4_comp_add", "Dodaj", class = "btn-warning", width = "100%"),
          actionButton("s4_comp_reset", "Reset", class = "btn-outline-secondary btn-sm", width = "100%"),
          hr(),
          checkboxInput("s4_comp_show_cv", "Pokaz wspolczynnik zmiennosci (CV)", FALSE),
          width = 3
        ),
        mainPanel(
          plotOutput("s4_comp_plot", height = "350px"),
          br(),
          tableOutput("s4_comp_table"),
          conditionalPanel(
            condition = "input.s4_comp_show_cv == true",
            br(),
            div(class = "desc-box",
              h4("Wspolczynnik zmiennosci (CV)"),
              uiOutput("s4_comp_cv_explanation")
            ),
            plotOutput("s4_comp_cv_plot", height = "300px")
          ),
          width = 9
        )
      )
    )

  ) # end tabsetPanel
), # end tabPanel
tabPanel("5. Ksztalt rozkladu",
  tabsetPanel(id = "s5_tabs",

    # ------------------------------------------------------------------
    # Tab 5.1: Skosnosc
    # ------------------------------------------------------------------
    tabPanel("Skosnosc",
      sidebarLayout(
        sidebarPanel(
          h4("Zrodlo danych"),
          radioButtons("s5_skew_source", "Typ:",
                       choices = c("Z ankiety" = "survey",
                                   "Generowane" = "generated")),
          conditionalPanel(
            condition = "input.s5_skew_source == 'survey'",
            selectInput("s5_skew_var", "Zmienna:",
                        choices = c("Wzrost" = "wzrost",
                                    "Czas dojazdu" = "czas_dojazdu",
                                    "Srednia ocen" = "srednia_ocen",
                                    "Waga" = "waga",
                                    "Liczba nieobecnosci" = "liczba_nieobecnosci"))
          ),
          conditionalPanel(
            condition = "input.s5_skew_source == 'generated'",
            selectInput("s5_skew_dist", "Rozklad:",
                        choices = c("Normalny" = "normal",
                                    "Prawoskosny (gamma)" = "gamma",
                                    "Lewoskosny" = "left_skew",
                                    "Silnie prawoskosny (exp)" = "exponential")),
            sliderInput("s5_skew_param", "Parametr ksztaltu:",
                        min = 0.5, max = 10, value = 3, step = 0.5),
            actionButton("s5_skew_regen", "Losuj ponownie",
                         class = "btn-success btn-sm", width = "100%")
          ),
          hr(),
          div(class = "desc-box",
            h4("Interpretacja"),
            uiOutput("s5_skew_interpretation")
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("s5_skew_plot", height = "400px"),
          br(),
          div(class = "desc-box",
            h4("Regula kciuka: polozenie sredniej vs mediany"),
            uiOutput("s5_skew_rule")
          ),
          width = 9
        )
      )
    ),

    # ------------------------------------------------------------------
    # Tab 5.2: Kurtoza
    # ------------------------------------------------------------------
    tabPanel("Kurtoza",
      sidebarLayout(
        sidebarPanel(
          h4("Porownanie rozkladow"),
          sliderInput("s5_kurt_df", "Stopnie swobody rozkladu t:",
                      min = 1, max = 50, value = 5, step = 1),
          hr(),
          checkboxInput("s5_kurt_show_normal", "Pokaz rozklad normalny", TRUE),
          checkboxInput("s5_kurt_show_uniform", "Pokaz rozklad jednostajny", FALSE),
          hr(),
          div(class = "desc-box",
            h4("Interpretacja"),
            uiOutput("s5_kurt_interpretation")
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("s5_kurt_plot", height = "400px"),
          br(),
          plotOutput("s5_kurt_tails_plot", height = "250px"),
          br(),
          div(class = "desc-box",
            uiOutput("s5_kurt_explanation")
          ),
          width = 9
        )
      )
    ),

    # ------------------------------------------------------------------
    # Tab 5.3: Pelny obraz rozkladu (capstone)
    # ------------------------------------------------------------------
    tabPanel("Pelny obraz",
      sidebarLayout(
        sidebarPanel(
          selectInput("s5_full_var", "Zmienna:",
                      choices = c("Wzrost (cm)" = "wzrost",
                                  "Waga (kg)" = "waga",
                                  "Czas dojazdu (min)" = "czas_dojazdu",
                                  "Srednia ocen" = "srednia_ocen",
                                  "Liczba kursow" = "liczba_kursow",
                                  "Liczba nieobecnosci" = "liczba_nieobecnosci")),
          hr(),
          h4("Wszystkie statystyki"),
          tableOutput("s5_full_stats_table"),
          width = 3
        ),
        mainPanel(
          plotOutput("s5_full_hist_plot", height = "400px"),
          plotOutput("s5_full_box_plot", height = "120px"),
          br(),
          div(class = "desc-box",
            h4("Co ten rozklad nam mowi?"),
            uiOutput("s5_full_interpretation")
          ),
          width = 9
        )
      )
    )
  )
)

) # end navbarPage
# end UI

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # SECTION 1: Typy danych
  # --------------------------------------------------------------------------

  # --- Tab 1.1: Taksonomia ---

  # Wezly z przykladami i ich pozycje (x, y) na diagramie
  taxonomy_nodes <- data.frame(
    id = c("ciagla", "dyskretna", "porzadkowa", "nominalna"),
    x = c(1.25, 3.75, 6.25, 8.75),
    y = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  revealed_nodes <- reactiveVal(character(0))

  observeEvent(input$taxonomy_click, {
    click <- input$taxonomy_click
    if (is.null(click)) return()
    # Sprawdz ktory wezel kliknieto (tolerancja: +-1 x, +-0.25 y)
    dists <- abs(taxonomy_nodes$x - click$x) < 1 & abs(taxonomy_nodes$y - click$y) < 0.4
    if (any(dists)) {
      clicked_id <- taxonomy_nodes$id[which(dists)[1]]
      current <- revealed_nodes()
      if (clicked_id %in% current) {
        revealed_nodes(setdiff(current, clicked_id))
      } else {
        revealed_nodes(c(current, clicked_id))
      }
    }
  })

  observeEvent(input$reveal_all, {
    revealed_nodes(c("ciagla", "dyskretna", "porzadkowa", "nominalna"))
  })

  observeEvent(input$hide_all, {
    revealed_nodes(character(0))
  })

  output$taxonomy_plot <- renderPlot({
    render_taxonomy(revealed = revealed_nodes())
  })

  # --- Tab 1.2: Eksploracja zmiennej ---

  current_meta <- reactive({
    variable_meta[[input$selected_var]]
  })

  current_data <- reactive({
    student_data[[input$selected_var]]
  })

  current_type <- reactive({
    current_meta()$type
  })

  output$type_badge <- renderUI({
    type <- current_type()
    col <- type_colors[type]
    lbl <- type_labels[type]
    div(class = "type-badge", style = paste0("background-color:", col), lbl)
  })

  output$var_description <- renderText({
    current_meta()$description
  })

  output$data_preview <- renderPrint({
    x <- current_data()
    head(x, 10)
  })

  output$good_plot <- renderPlot({
    render_good_plot(current_data(), current_meta()$label, current_type())
  })

  # Kontrola widocznosci boxplota
  output$show_boxplot <- reactive({
    current_type() %in% c("ilosciowa_dyskretna", "ilosciowa_ciagla")
  })
  outputOptions(output, "show_boxplot", suspendWhenHidden = FALSE)

  output$boxplot <- renderPlot({
    req(current_type() %in% c("ilosciowa_dyskretna", "ilosciowa_ciagla"))
    render_boxplot(current_data(), current_meta()$label, current_type())
  })

  good_stats <- reactive({
    compute_good_stats(current_data(), current_type())
  })

  output$good_stats_text <- renderPrint({
    s <- good_stats()
    if (!is.null(s$text)) cat(s$text)
  })

  output$good_stats_table <- renderTable({
    good_stats()$table
  })

  output$bad_stats_text <- renderPrint({
    cat(compute_bad_stats(current_data(), current_type()))
  })

  output$bad_plot <- renderPlot({
    req(input$show_bad)
    render_bad_plot(current_data(), current_meta()$label, current_type())
  })

  # --- Tab 1.3: Porownanie ---

  render_compare <- function(var_name, plot_id, stats_id, badge_id) {
    meta <- variable_meta[[var_name]]
    data <- student_data[[var_name]]
    type <- meta$type

    list(meta = meta, data = data, type = type)
  }

  output$compare_badge1 <- renderUI({
    meta <- variable_meta[[input$compare_var1]]
    col <- type_colors[meta$type]
    lbl <- type_labels[meta$type]
    div(class = "type-badge", style = paste0("background-color:", col), lbl)
  })

  output$compare_badge2 <- renderUI({
    meta <- variable_meta[[input$compare_var2]]
    col <- type_colors[meta$type]
    lbl <- type_labels[meta$type]
    div(class = "type-badge", style = paste0("background-color:", col), lbl)
  })

  output$compare_plot1 <- renderPlot({
    meta <- variable_meta[[input$compare_var1]]
    render_good_plot(student_data[[input$compare_var1]], meta$label, meta$type)
  })

  output$compare_plot2 <- renderPlot({
    meta <- variable_meta[[input$compare_var2]]
    render_good_plot(student_data[[input$compare_var2]], meta$label, meta$type)
  })

  output$compare_stats1 <- renderTable({
    meta <- variable_meta[[input$compare_var1]]
    compute_good_stats(student_data[[input$compare_var1]], meta$type)$table
  })

  output$compare_stats2 <- renderTable({
    meta <- variable_meta[[input$compare_var2]]
    compute_good_stats(student_data[[input$compare_var2]], meta$type)$table
  })

  output$compare_summary <- renderUI({
    m1 <- variable_meta[[input$compare_var1]]
    m2 <- variable_meta[[input$compare_var2]]
    c1 <- type_colors[m1$type]
    c2 <- type_colors[m2$type]

    good_charts <- function(type) {
      switch(type,
        "nominalna" = "wykres slupkowy",
        "porzadkowa" = "wykres slupkowy",
        "ilosciowa_dyskretna" = "wykres slupkowy, boxplot",
        "ilosciowa_ciagla" = "histogram, boxplot"
      )
    }

    good_stats_list <- function(type) {
      switch(type,
        "nominalna" = "czestosci, dominanta",
        "porzadkowa" = "czestosci, dominanta, mediana",
        "ilosciowa_dyskretna" = "srednia, mediana, odch. std., IQR",
        "ilosciowa_ciagla" = "srednia, mediana, odch. std., IQR"
      )
    }

    tags$div(
      tags$p(
        tags$span(style = paste0("color:", c1, ";font-weight:bold;"), m1$label),
        paste0(" (", type_labels[m1$type], ") -> statystyki: ", good_stats_list(m1$type),
               "; wykresy: ", good_charts(m1$type))
      ),
      tags$p(
        tags$span(style = paste0("color:", c2, ";font-weight:bold;"), m2$label),
        paste0(" (", type_labels[m2$type], ") -> statystyki: ", good_stats_list(m2$type),
               "; wykresy: ", good_charts(m2$type))
      )
    )
  })

  # --- Tab 1.4: Sciaga / Podsumowanie ---

  output$summary_table <- renderTable({
    data.frame(
      `Typ danych` = c("Nominalna", "Porzadkowa", "Ilosciowa dyskretna", "Ilosciowa ciagla"),
      `Przyklady` = c(
        "Plec, kierunek, grupa krwi",
        "Rok studiow, zadowolenie (Likert)",
        "Liczba kursow, liczba nieobecnosci",
        "Wzrost, srednia ocen, czas dojazdu"
      ),
      `Statystyki opisowe` = c(
        "Czestosci, dominanta (moda)",
        "Czestosci, dominanta, mediana",
        "Srednia, mediana, odch. std., min/max, IQR",
        "Srednia, mediana, odch. std., min/max, IQR"
      ),
      `Wykresy` = c(
        "Wykres slupkowy, wykres kolowy",
        "Wykres slupkowy",
        "Wykres slupkowy, boxplot",
        "Histogram, boxplot, wykres gestosci"
      ),
      `Czego NIE robic` = c(
        "NIE licz sredniej ani mediany",
        "Srednia jest DYSKUSYJNA",
        "Tabela czestosci OK, ale histogram lepszy",
        "NIE rob tabeli czestosci (za duzo wartosci)"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$dataset_preview <- renderTable({
    head(student_data, 15)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

# SECTION 2: Zmienne jakosciowe

# ========================================================================
# Zmienne pomocnicze sekcji 2
# ========================================================================

s2_categorical_vars <- c("plec", "kierunek", "grupa_krwi",
                          "rok_studiow", "zadowolenie", "ocena_wykladowcy")

# ========================================================================
# Tab 2.1: Tabela czestosci
# ========================================================================

s2_freq_step <- reactiveVal(0)

observeEvent(input$s2_freq_step1, { s2_freq_step(1) })
observeEvent(input$s2_freq_step2, { s2_freq_step(2) })
observeEvent(input$s2_freq_step3, { s2_freq_step(3) })
observeEvent(input$s2_freq_step4, { s2_freq_step(4) })
observeEvent(input$s2_freq_reset, { s2_freq_step(0) })
observeEvent(input$s2_freq_var, { s2_freq_step(0) })

output$s2_freq_explanation <- renderUI({
  step <- s2_freq_step()
  var_name <- input$s2_freq_var
  meta <- variable_meta[[var_name]]
  var_type <- meta$type

  if (step == 0) {
    div(class = "desc-box",
      h4("Tabela czestosci - krok po kroku"),
      p("Tabela czestosci to podstawowe narzedzie do opisu zmiennych jakosciowych."),
      p("Kliknij kolejne przyciski, aby zbudowac tabele krok po kroku."),
      p(strong("Wybrana zmienna: "), meta$label,
        " (", type_labels[var_type], ")")
    )
  } else if (step == 1) {
    div(class = "desc-box",
      h4("Krok 1: Surowe dane"),
      p("Patrzymy na surowe dane - to lista wszystkich wartosci zmiennej ",
        strong(meta$label), "."),
      p("Mamy ", strong(nrow(student_data)), " obserwacji. Trudno cos z tego wyczytac - ",
        "potrzebujemy podsumowania!")
    )
  } else if (step == 2) {
    div(class = "desc-box",
      h4("Krok 2: Zliczanie (czestosci bezwzgledne)"),
      p("Liczymy ile razy wystapila kazda kategoria. ",
        "To sa tzw. ", strong("czestosci bezwzgledne"), " (liczebnosci)."),
      p("Juz widac, ktora kategoria jest najczesstsza!")
    )
  } else if (step == 3) {
    div(class = "desc-box",
      h4("Krok 3: Czestosci wzgledne (procenty)"),
      p("Dzielimy kazda liczebnosc przez calkowita liczbe obserwacji ",
        "i mnozmy przez 100%."),
      p("Procenty sa latwiejsze do interpretacji i ",
        strong("umozliwiaja porownanie grup o roznych liczebnosciach"), ".")
    )
  } else if (step == 4) {
    is_ordinal <- var_type == "porzadkowa"
    if (is_ordinal) {
      div(class = "desc-box",
        h4("Krok 4: Czestosci skumulowane"),
        p("Dla zmiennej porzadkowej mozemy policzyc czestosci skumulowane - ",
          "sumujemy procenty od poczatku do kazdej kategorii."),
        p("Mozemy powiedziec np. '", strong("X% studentow ma rok studiow <= 3"),
          "' - to ma sens, bo kategorie maja porzadek!"),
        p(em("Czestosci skumulowane maja sens TYLKO dla zmiennych porzadkowych, ",
             "nie dla nominalnych."))
      )
    } else {
      div(class = "bad-box",
        h4("Krok 4: Czestosci skumulowane - UWAGA!"),
        p("Zmienna ", strong(meta$label), " jest ", strong("nominalna"),
          " - jej kategorie nie maja naturalnego porzadku."),
        p("Czestosci skumulowane ", strong("nie maja sensu"),
          " dla zmiennych nominalnych! ",
          "Co by oznaczalo 'skumulowana czestosc do kategorii Biologia'?"),
        p(em("Czestosci skumulowane liczymy tylko dla zmiennych porzadkowych."))
      )
    }
  }
})

output$s2_freq_table <- renderTable({
  step <- s2_freq_step()
  var_name <- input$s2_freq_var
  x <- student_data[[var_name]]
  meta <- variable_meta[[var_name]]
  var_type <- meta$type

  if (step == 0) return(NULL)

  if (step == 1) {
    # Surowe dane - pokaz pierwsze 20 wartosci w tabeli
    df <- data.frame(
      Nr = 1:min(20, length(x)),
      Wartosc = as.character(x[1:min(20, length(x))]),
      stringsAsFactors = FALSE
    )
    if (length(x) > 20) {
      df <- rbind(df, data.frame(Nr = NA, Wartosc = paste0("... (lacznie ", length(x), " obserwacji)")))
    }
    return(df)
  }

  # Krok 2+: tabela czestosci
  freq <- as.data.frame(table(x), stringsAsFactors = FALSE)
  names(freq) <- c("Wartosc", "Liczebnosc")
  freq$Wartosc <- as.character(freq$Wartosc)

  if (step >= 3) {
    freq$Procent <- round(100 * freq$Liczebnosc / sum(freq$Liczebnosc), 1)
  }

  if (step >= 4 && var_type == "porzadkowa") {
    freq$Skumulowane <- cumsum(freq$Procent)
  }

  # Dodaj wiersz sumy
  sum_row <- data.frame(
    Wartosc = "SUMA",
    Liczebnosc = sum(freq$Liczebnosc),
    stringsAsFactors = FALSE
  )
  if (step >= 3) {
    sum_row$Procent <- 100.0
  }
  if (step >= 4 && var_type == "porzadkowa") {
    sum_row$Skumulowane <- NA
  }

  freq <- rbind(freq, sum_row)
  freq

}, striped = TRUE, bordered = TRUE, hover = TRUE, na = "")

# ========================================================================
# Tab 2.2: Wizualizacje (pie chart critique + color manipulation)
# ========================================================================

s2_color_vals <- reactiveVal(NULL)

observeEvent(input$s2_randomize_colors, {
  n_cols <- 12  # enough for any variable
  random_cols <- sprintf("#%02X%02X%02X",
                         sample(50:220, n_cols, replace = TRUE),
                         sample(50:220, n_cols, replace = TRUE),
                         sample(50:220, n_cols, replace = TRUE))
  s2_color_vals(random_cols)
})

s2_current_colors <- reactive({
  palette_choice <- input$s2_color_palette
  n_max <- 12

  if (palette_choice == "neutral") {
    c("#7f8c8d", "#95a5a6", "#bdc3c7", "#5d6d7e", "#85929e",
      "#aab7b8", "#808b96", "#a6acaf", "#b2babb", "#d5d8dc",
      "#717d7e", "#99a3a4")
  } else if (palette_choice == "warm") {
    c("#e74c3c", "#e67e22", "#f1c40f", "#d35400", "#c0392b",
      "#f39c12", "#e74c3c", "#e67e22", "#f1c40f", "#d35400",
      "#c0392b", "#f39c12")
  } else if (palette_choice == "cold") {
    c("#2980b9", "#3498db", "#1abc9c", "#16a085", "#2c3e50",
      "#27ae60", "#2980b9", "#3498db", "#1abc9c", "#16a085",
      "#2c3e50", "#27ae60")
  } else if (palette_choice == "biased") {
    # First category bright red, rest gray
    c("#e74c3c", "#bdc3c7", "#bdc3c7", "#bdc3c7", "#bdc3c7",
      "#bdc3c7", "#bdc3c7", "#bdc3c7", "#bdc3c7", "#bdc3c7",
      "#bdc3c7", "#bdc3c7")
  } else if (palette_choice == "random") {
    rv <- s2_color_vals()
    if (is.null(rv)) {
      # Generate initial random
      sprintf("#%02X%02X%02X",
              sample(50:220, n_max, replace = TRUE),
              sample(50:220, n_max, replace = TRUE),
              sample(50:220, n_max, replace = TRUE))
    } else {
      rv
    }
  } else {
    rep("#95a5a6", n_max)
  }
})

s2_viz_data <- reactive({
  var_name <- input$s2_viz_var
  x <- student_data[[var_name]]
  freq <- as.data.frame(table(x), stringsAsFactors = FALSE)
  names(freq) <- c("Kategoria", "Liczebnosc")
  freq$Procent <- round(100 * freq$Liczebnosc / sum(freq$Liczebnosc), 1)

  if (input$s2_viz_sort == "descending") {
    freq <- freq[order(-freq$Liczebnosc), ]
    freq$Kategoria <- factor(freq$Kategoria, levels = freq$Kategoria)
  } else {
    freq$Kategoria <- factor(freq$Kategoria, levels = freq$Kategoria)
  }

  freq
})

output$s2_bar_plot <- renderPlot({
  df <- s2_viz_data()
  colors <- s2_current_colors()[1:nrow(df)]
  is_horizontal <- input$s2_viz_orient == "horizontal"

  p <- ggplot(df, aes(x = Kategoria, y = Liczebnosc, fill = Kategoria)) +
    geom_col(color = "white", show.legend = FALSE) +
    geom_text(aes(label = Liczebnosc),
              vjust = if (is_horizontal) 0.5 else -0.5,
              hjust = if (is_horizontal) -0.2 else 0.5,
              size = 4) +
    scale_fill_manual(values = colors) +
    labs(title = "Wykres slupkowy", x = NULL, y = "Liczebnosc") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(
      angle = if (!is_horizontal && nrow(df) > 4) 30 else 0,
      hjust = 1))

  if (is_horizontal) {
    p <- p + coord_flip()
  }
  p
})

output$s2_pie_plot <- renderPlot({
  df <- s2_viz_data()
  colors <- s2_current_colors()[1:nrow(df)]

  df$ypos <- cumsum(df$Liczebnosc) - 0.5 * df$Liczebnosc

  ggplot(df, aes(x = "", y = Liczebnosc, fill = Kategoria)) +
    geom_col(width = 1, color = "white", show.legend = TRUE) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colors) +
    labs(title = "Pie chart", fill = "Kategoria") +
    theme_void(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
})

output$s2_lollipop_plot <- renderPlot({
  df <- s2_viz_data()
  colors <- s2_current_colors()[1:nrow(df)]

  ggplot(df, aes(x = Kategoria, y = Liczebnosc, color = Kategoria)) +
    geom_segment(aes(x = Kategoria, xend = Kategoria,
                     y = 0, yend = Liczebnosc),
                 linewidth = 1.2, show.legend = FALSE) +
    geom_point(size = 5, show.legend = FALSE) +
    scale_color_manual(values = colors) +
    labs(title = "Lollipop chart", x = NULL, y = "Liczebnosc") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(
      angle = if (nrow(df) > 4) 30 else 0, hjust = 1))
})

output$s2_pie_critique <- renderUI({
  tags$div(
    tags$ul(
      tags$li(strong("Ludzie zle porownuja katy"), " - wykres slupkowy uzywa dlugosci, ",
              "co jest znacznie latwiejsze do percepcji niz katy i pole powierzchni."),
      tags$li(strong("Przy >5 kategoriach pie chart staje sie nieczytelny"),
              " - wycinki sa za male, etykiety nachodza na siebie."),
      tags$li(strong("Nie mozna latwo porownac dwoch pie chartow"),
              " - porownanie dwoch wykresow slupkowych jest naturalne, ",
              "ale dwoch 'kolaczow'? Bardzo trudne!"),
      tags$li(strong("Kolory moga manipulowac percepcja"),
              " - jaskrawy, cieplejszy kolor przyciaga wzrok i sprawia, ",
              "ze wycinek wydaje sie wiekszy. Przetestuj palety powyzej!"),
      tags$li(strong("Efekt 3D i eksplodowania"),
              " - czeste 'ulepszenia' pie chartow jeszcze bardziej znieksztalcaja odbiory.")
    ),
    tags$p(em("Wniosek: uzywaj wykresow slupkowych lub lollipop zamiast pie chartow."))
  )
})

# Quiz data - very similar values, hard to distinguish on pie
s2_quiz_data <- reactive({
  input$s2_show_quiz  # re-trigger when quiz is shown
  set.seed(NULL)
  base <- sample(18:22, 1)
  vals <- base + sample(0:8, 5) / 10 * sample(c(-1, 1), 5, replace = TRUE)
  vals <- round(vals / sum(vals) * 200)
  # Make sure there's a clear winner but small differences
  vals[sample(1:5, 1)] <- max(vals) + sample(1:3, 1)
  data.frame(
    Kategoria = factor(LETTERS[1:5], levels = LETTERS[1:5]),
    Liczebnosc = vals,
    stringsAsFactors = FALSE
  )
})

output$s2_quiz_pie <- renderPlot({
  df <- s2_quiz_data()
  quiz_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6")

  ggplot(df, aes(x = "", y = Liczebnosc, fill = Kategoria)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = quiz_colors) +
    labs(title = "Pie chart - ktora kategoria jest najwieksza?") +
    theme_void(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
})

output$s2_quiz_bar <- renderPlot({
  df <- s2_quiz_data()
  quiz_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6")

  ggplot(df, aes(x = Kategoria, y = Liczebnosc, fill = Kategoria)) +
    geom_col(color = "white", show.legend = FALSE) +
    geom_text(aes(label = Liczebnosc), vjust = -0.5, size = 4.5) +
    scale_fill_manual(values = quiz_colors) +
    labs(title = "Wykres slupkowy - tu juz widac!",
         x = NULL, y = "Liczebnosc") +
    theme_minimal(base_size = 14)
})

output$s2_quiz_answer <- renderUI({
  df <- s2_quiz_data()
  winner <- as.character(df$Kategoria[which.max(df$Liczebnosc)])
  max_val <- max(df$Liczebnosc)
  min_val <- min(df$Liczebnosc)
  diff_pct <- round((max_val - min_val) / min_val * 100, 1)

  div(class = "desc-box", style = "margin-top: 15px;",
    p(strong("Odpowiedz: "), "Kategoria ", strong(winner),
      " jest najwieksza (", max_val, " obserwacji)."),
    p("Roznica miedzy najwieksza a najmniejsza kategoria to tylko ",
      strong(paste0(diff_pct, "%")), "."),
    p("Na pie charcie ta roznica jest praktycznie ",
      strong("niemozliwa do zauwazen"), ", a na wykresie slupkowym widac ja od razu.")
  )
})

# ========================================================================
# Tab 2.3: Tabela krzyzowa
# ========================================================================

output$s2_cross_explanation <- renderUI({
  cross_type <- input$s2_cross_type
  var1_label <- variable_meta[[input$s2_cross_var1]]$label
  var2_label <- variable_meta[[input$s2_cross_var2]]$label

  explanation <- switch(cross_type,
    "count" = paste0(
      "Tabela krzyzowa (kontyngencji) pokazuje liczebnosci dla kazdej ",
      "kombinacji kategorii zmiennych ", var1_label, " i ", var2_label, ". ",
      "Kazda komorka mowi ile obserwacji ma dana kombinacje."
    ),
    "row" = paste0(
      "Procenty wierszowe - kazdy wiersz sumuje sie do 100%. ",
      "Odpowiadaja na pytanie: 'Jaki jest rozklad ", var2_label,
      " wsrod osob z danej kategorii ", var1_label, "?'"
    ),
    "col" = paste0(
      "Procenty kolumnowe - kazda kolumna sumuje sie do 100%. ",
      "Odpowiadaja na pytanie: 'Jaki jest rozklad ", var1_label,
      " wsrod osob z danej kategorii ", var2_label, "?'"
    ),
    "total" = paste0(
      "Procenty calkowite - cala tabela sumuje sie do 100%. ",
      "Kazda komorka mowi jaki procent WSZYSTKICH obserwacji ",
      "ma dana kombinacje kategorii."
    )
  )

  tags$div(
    h4("Tabela krzyzowa: ", var1_label, " x ", var2_label),
    p(explanation)
  )
})

output$s2_cross_table <- renderTable({
  var1 <- input$s2_cross_var1
  var2 <- input$s2_cross_var2
  cross_type <- input$s2_cross_type

  x1 <- student_data[[var1]]
  x2 <- student_data[[var2]]

  ct <- table(x1, x2)

  if (cross_type == "count") {
    result <- as.data.frame.matrix(ct)
  } else if (cross_type == "row") {
    result <- as.data.frame.matrix(round(prop.table(ct, margin = 1) * 100, 1))
  } else if (cross_type == "col") {
    result <- as.data.frame.matrix(round(prop.table(ct, margin = 2) * 100, 1))
  } else {
    result <- as.data.frame.matrix(round(prop.table(ct) * 100, 1))
  }

  # Add row names as first column
  result <- cbind(data.frame(Kategoria = rownames(result), stringsAsFactors = FALSE), result)
  rownames(result) <- NULL
  result

}, striped = TRUE, bordered = TRUE, hover = TRUE)

output$s2_cross_plot <- renderPlot({
  var1 <- input$s2_cross_var1
  var2 <- input$s2_cross_var2
  cross_type <- input$s2_cross_type

  x1 <- student_data[[var1]]
  x2 <- student_data[[var2]]
  var1_label <- variable_meta[[var1]]$label
  var2_label <- variable_meta[[var2]]$label

  ct <- table(x1, x2)

  if (cross_type == "count") {
    mat <- ct
    fill_label <- "Liczebnosc"
  } else if (cross_type == "row") {
    mat <- round(prop.table(ct, margin = 1) * 100, 1)
    fill_label <- "% wierszowe"
  } else if (cross_type == "col") {
    mat <- round(prop.table(ct, margin = 2) * 100, 1)
    fill_label <- "% kolumnowe"
  } else {
    mat <- round(prop.table(ct) * 100, 1)
    fill_label <- "% calkowite"
  }

  # Convert to long format
  df_long <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(df_long) <- c("Var1", "Var2", "Wartosc")

  ggplot(df_long, aes(x = Var2, y = Var1, fill = Wartosc)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = round(Wartosc, 1)),
              size = 5, fontface = "bold", color = col_dark) +
    scale_fill_gradient(low = "#f8f9fa", high = "#3498db",
                        name = fill_label) +
    labs(title = paste0("Tabela krzyzowa: ", var1_label, " x ", var2_label),
         x = var2_label, y = var1_label) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(
      angle = if (length(unique(df_long$Var2)) > 5) 30 else 0,
      hjust = 1),
      panel.grid = element_blank())
})

# ========================================================================
# Tab 2.4: Moda (dominanta)
# ========================================================================

s2_mode_data <- reactiveVal(NULL)

observe({
  # Initialize with current student_data
  var_name <- input$s2_mode_var
  req(var_name)
  s2_mode_data(student_data[[var_name]])
})

observeEvent(input$s2_mode_generate, {
  var_name <- input$s2_mode_var
  meta <- variable_meta[[var_name]]
  n <- 200

  if (meta$type == "nominalna") {
    if (var_name == "plec") {
      new_data <- factor(sample(c("Kobieta", "Mezczyzna"), n, replace = TRUE))
    } else if (var_name == "kierunek") {
      # Randomly pick probabilities to sometimes get bimodal
      probs <- runif(4)
      probs <- probs / sum(probs)
      new_data <- factor(sample(c("Informatyka", "Biologia", "Psychologia", "Ekonomia"),
                                n, replace = TRUE, prob = probs))
    } else {
      probs <- runif(4)
      probs <- probs / sum(probs)
      new_data <- factor(sample(c("A", "B", "AB", "0"),
                                n, replace = TRUE, prob = probs))
    }
  } else if (meta$type == "porzadkowa") {
    if (var_name == "rok_studiow") {
      probs <- runif(5)
      probs <- probs / sum(probs)
      new_data <- factor(sample(1:5, n, replace = TRUE, prob = probs),
                         levels = 1:5, ordered = TRUE)
    } else if (var_name == "zadowolenie") {
      lvls <- c("Bardzo niezadowolony", "Niezadowolony", "Neutralny",
                "Zadowolony", "Bardzo zadowolony")
      probs <- runif(5)
      probs <- probs / sum(probs)
      new_data <- factor(sample(lvls, n, replace = TRUE, prob = probs),
                         levels = lvls, ordered = TRUE)
    } else {
      probs <- runif(10)
      probs <- probs / sum(probs)
      new_data <- factor(sample(1:10, n, replace = TRUE, prob = probs),
                         levels = 1:10, ordered = TRUE)
    }
  } else if (meta$type == "ilosciowa_dyskretna") {
    if (var_name == "liczba_kursow") {
      new_data <- sample(3:9, n, replace = TRUE)
    } else {
      new_data <- rpois(n, lambda = sample(1:6, 1))
    }
  } else {
    # ilosciowa_ciagla - generate with possible bimodal
    bimodal <- sample(c(TRUE, FALSE), 1)
    if (bimodal) {
      mu1 <- runif(1, 50, 80)
      mu2 <- mu1 + runif(1, 10, 30)
      sd_val <- runif(1, 3, 8)
      mix <- sample(c(0.4, 0.5, 0.6), 1)
      n1 <- round(n * mix)
      new_data <- round(c(rnorm(n1, mu1, sd_val), rnorm(n - n1, mu2, sd_val)), 1)
    } else {
      new_data <- round(rnorm(n, runif(1, 50, 180), runif(1, 5, 15)), 1)
    }
  }

  s2_mode_data(new_data)
})

output$s2_mode_explanation <- renderUI({
  var_name <- input$s2_mode_var
  meta <- variable_meta[[var_name]]

  tags$div(
    h4("Moda (dominanta) - ", meta$label),
    p(strong("Moda"), " (dominanta) to wartosc wystepujaca najczesciej w zbiorze danych."),
    p("Jest to jedyna miara tendencji centralnej, ktora mozna stosowac do ",
      strong("wszystkich typow zmiennych"), " - nominalnych, porzadkowych i ilosciowych."),
    if (meta$type == "nominalna") {
      p("Dla zmiennej nominalnej moda to ", strong("jedyna sensowna miara centralnosci"),
        " - nie mozna liczyc sredniej ani mediany.")
    } else if (meta$type == "porzadkowa") {
      p("Dla zmiennej porzadkowej mozna liczyc ", strong("mode i mediane"),
        ", ale srednia jest dyskusyjna.")
    } else {
      p("Dla zmiennej ilosciowej moda jest rzadziej uzywana - czesciej stosujemy ",
        "srednia lub mediane. Ale moze byc przydatna do wykrycia ",
        strong("rozkladow wielomodalnych"), ".")
    }
  )
})

output$s2_mode_plot <- renderPlot({
  x <- s2_mode_data()
  req(x)
  var_name <- input$s2_mode_var
  meta <- variable_meta[[var_name]]
  var_type <- meta$type

  if (var_type %in% c("nominalna", "porzadkowa", "ilosciowa_dyskretna")) {
    freq <- as.data.frame(table(x), stringsAsFactors = FALSE)
    names(freq) <- c("Wartosc", "Liczebnosc")
    max_count <- max(freq$Liczebnosc)
    freq$is_mode <- freq$Liczebnosc == max_count

    if (var_type == "porzadkowa" || var_type == "ilosciowa_dyskretna") {
      freq$Wartosc <- factor(freq$Wartosc, levels = freq$Wartosc)
    }

    type_col <- type_colors[var_type]

    ggplot(freq, aes(x = Wartosc, y = Liczebnosc, fill = is_mode)) +
      geom_col(color = "white", show.legend = FALSE) +
      geom_text(aes(label = Liczebnosc), vjust = -0.5, size = 4.5) +
      scale_fill_manual(values = c("FALSE" = "#bdc3c7", "TRUE" = type_col)) +
      labs(title = paste0("Moda: ", meta$label),
           subtitle = "Podswietlone slupki = moda (najwyzsza liczebnosc)",
           x = meta$label, y = "Liczebnosc") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(
        angle = if (nrow(freq) > 6) 30 else 0, hjust = 1))

  } else {
    # ciagla - histogram z zaznaczonym przedzialem mody
    df <- data.frame(x = as.numeric(x))
    hist_data <- hist(df$x, breaks = 20, plot = FALSE)
    max_bin <- which.max(hist_data$counts)
    bin_df <- data.frame(
      xmin = hist_data$breaks[-length(hist_data$breaks)],
      xmax = hist_data$breaks[-1],
      count = hist_data$counts
    )
    bin_df$is_mode <- bin_df$count == max(bin_df$count)

    type_col <- type_colors[var_type]

    ggplot(bin_df, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = count, fill = is_mode)) +
      geom_rect(color = "white", show.legend = FALSE) +
      scale_fill_manual(values = c("FALSE" = "#bdc3c7", "TRUE" = type_col)) +
      labs(title = paste0("Moda (przedzial): ", meta$label),
           subtitle = "Podswietlony przedzial = przedzial z najwyzsza czestoscia",
           x = meta$label, y = "Liczebnosc") +
      theme_minimal(base_size = 14)
  }
})

output$s2_mode_stats <- renderPrint({
  x <- s2_mode_data()
  req(x)
  var_name <- input$s2_mode_var
  meta <- variable_meta[[var_name]]
  var_type <- meta$type

  if (var_type %in% c("nominalna", "porzadkowa", "ilosciowa_dyskretna")) {
    freq <- as.data.frame(table(x), stringsAsFactors = FALSE)
    names(freq) <- c("Wartosc", "Liczebnosc")
    max_count <- max(freq$Liczebnosc)
    modes <- freq$Wartosc[freq$Liczebnosc == max_count]

    n_modes <- length(modes)
    modality <- if (n_modes == 1) {
      "unimodalny (jedna moda)"
    } else if (n_modes == 2) {
      "bimodalny (dwie mody)"
    } else {
      paste0("multimodalny (", n_modes, " mod)")
    }

    cat("Zmienna:", meta$label, "\n")
    cat("Typ:", type_labels[var_type], "\n")
    cat("Liczba obserwacji:", length(x), "\n")
    cat("Liczba kategorii:", nrow(freq), "\n\n")
    cat("Moda (dominanta):", paste(modes, collapse = ", "), "\n")
    cat("Liczebnosc mody:", max_count, "\n")
    cat("Rozklad:", modality, "\n")

    if (var_type == "porzadkowa") {
      cat("\nMediana:", as.character(median(as.numeric(x))), "\n")
      cat("(Dla porownania z moda)\n")
    }

  } else {
    x_num <- as.numeric(x)
    hist_data <- hist(x_num, breaks = 20, plot = FALSE)
    max_bin <- which.max(hist_data$counts)
    mode_range <- paste0("[", round(hist_data$breaks[max_bin], 1),
                         ", ", round(hist_data$breaks[max_bin + 1], 1), ")")
    max_count <- hist_data$counts[max_bin]

    # Check for multiple peaks (bins with count close to max)
    threshold <- max_count * 0.9
    peak_bins <- which(hist_data$counts >= threshold)
    n_peaks <- length(peak_bins)
    modality <- if (n_peaks == 1) {
      "unimodalny (jeden szczyt)"
    } else if (n_peaks == 2) {
      "bimodalny (dwa szczyty)"
    } else {
      paste0("multimodalny (", n_peaks, " szczytow)")
    }

    cat("Zmienna:", meta$label, "\n")
    cat("Typ:", type_labels[var_type], "\n")
    cat("Liczba obserwacji:", length(x_num), "\n\n")
    cat("Przedzial modowy:", mode_range, "\n")
    cat("Liczebnosc w przedziale:", max_count, "\n")
    cat("Rozklad:", modality, "\n\n")
    cat("Srednia:", round(mean(x_num, na.rm = TRUE), 2), "\n")
    cat("Mediana:", round(median(x_num, na.rm = TRUE), 2), "\n")
    cat("(Dla zmiennych ciaglych srednia i mediana sa czesciej uzywane niz moda)\n")
  }
})

# END SECTION 2
# SECTION 3: Statystyki polozenia

# ============================================================================
# Tab 3.1: Srednia vs Mediana
# ============================================================================

svm_scenarios <- list(
  zarobki = list(name = "Zarobki w firmie (PLN)", slider_min = 3000, slider_max = 25000, slider_value = 7000, outlier_factor = 3),
  egzamin = list(name = "Wyniki egzaminu (punkty)", slider_min = 0, slider_max = 100, slider_value = 70, outlier_factor = 0.3),
  dojazd = list(name = "Czas dojazdu do pracy (minuty)", slider_min = 5, slider_max = 120, slider_value = 30, outlier_factor = 2),
  mieszkania = list(name = "Ceny mieszkan (tys. PLN)", slider_min = 200, slider_max = 2000, slider_value = 500, outlier_factor = 2.5),
  email = list(name = "Czas odpowiedzi email (godziny)", slider_min = 1, slider_max = 48, slider_value = 8, outlier_factor = 3)
)

svm_generate_data <- function(scenario_name) {
  set.seed(NULL)
  if (scenario_name == "zarobki") { rgamma(80, shape = 2, scale = 2000) + 3000
  } else if (scenario_name == "egzamin") { pmax(pmin(rnorm(80, mean = 70, sd = 12), 100), 0)
  } else if (scenario_name == "dojazd") { rgamma(80, shape = 3, scale = 8) + 5
  } else if (scenario_name == "mieszkania") { c(rgamma(76, shape = 3, scale = 80) + 200, runif(4, 1200, 1800))
  } else if (scenario_name == "email") { rexp(80, rate = 0.2) + 1 }
}

svm_data <- reactiveVal(NULL)

observe({ if (is.null(svm_data())) { svm_data(svm_generate_data("zarobki")) } })

observeEvent(input$svm_scenario, { svm_data(svm_generate_data(input$svm_scenario)) })

observeEvent(input$svm_reset, { svm_data(svm_generate_data(input$svm_scenario)) })

output$svm_value_slider <- renderUI({
  s <- svm_scenarios[[input$svm_scenario]]
  sliderInput("svm_new_value", "Wartosc do dodania:",
    min = s$slider_min, max = s$slider_max, value = s$slider_value,
    step = (s$slider_max - s$slider_min) / 100)
})

observeEvent(input$svm_add_value, { req(input$svm_new_value); svm_data(c(svm_data(), input$svm_new_value)) })

observeEvent(input$svm_add_outlier, {
  current_max <- max(svm_data()); s <- svm_scenarios[[input$svm_scenario]]
  outlier <- min(current_max * s$outlier_factor, s$slider_max); svm_data(c(svm_data(), outlier))
})

svm_mean_val <- reactive({ mean(svm_data()) })
svm_median_val <- reactive({ median(svm_data()) })

output$svm_mean_text <- renderText({ paste0("Srednia: ", round(svm_mean_val(), 2)) })
output$svm_median_text <- renderText({ paste0("Mediana: ", round(svm_median_val(), 2)) })
output$svm_diff_text <- renderText({ paste0("Roznica: ", round(abs(svm_mean_val() - svm_median_val()), 2)) })

output$svm_histogram <- renderPlot({
  req(svm_data()); df <- data.frame(value = svm_data()); s <- svm_scenarios[[input$svm_scenario]]
  m <- svm_mean_val(); med <- svm_median_val()
  ggplot(df, aes(x = value)) +
    geom_histogram(bins = 15, fill = "#95a5a6", color = "white", alpha = 0.7) +
    geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.5) +
    geom_vline(xintercept = med, color = "#3498db", linewidth = 1.5, linetype = "dashed") +
    annotate("text", x = m, y = Inf, label = paste0("Srednia: ", round(m, 2)), vjust = 2, hjust = ifelse(m > med, -0.1, 1.1), color = "#e74c3c", size = 5, fontface = "bold") +
    annotate("text", x = med, y = Inf, label = paste0("Mediana: ", round(med, 2)), vjust = 3.5, hjust = ifelse(med > m, -0.1, 1.1), color = "#3498db", size = 5, fontface = "bold") +
    labs(title = s$name, subtitle = paste0("n = ", length(svm_data()), " obserwacji"), x = "Wartosc", y = "Liczba obserwacji") +
    theme_minimal(base_size = 14) + theme(plot.title = element_text(size = 18, face = "bold"))
})

output$svm_stripplot <- renderPlot({
  req(svm_data()); df <- data.frame(value = svm_data()); m <- svm_mean_val(); med <- svm_median_val()
  ggplot(df, aes(x = value, y = 0)) +
    geom_jitter(height = 0.15, alpha = 0.6, size = 3, color = "#95a5a6") +
    geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.5) +
    geom_vline(xintercept = med, color = "#3498db", linewidth = 1.5, linetype = "dashed") +
    labs(title = "Wszystkie obserwacje (kazdy punkt = jedna wartosc)", x = "Wartosc") +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    ylim(-0.5, 0.5)
})

# ============================================================================
# Tab 3.2: Odpornosc statystyk
# ============================================================================

s3_rob_base_data <- reactiveVal(NULL)
s3_rob_outliers <- reactiveVal(numeric(0))

observeEvent(input$s3_rob_source, {
  set.seed(NULL)
  base <- switch(input$s3_rob_source,
    "salary" = rgamma(100, shape = 2, scale = 2000) + 3000,
    "exam"   = rnorm(100, mean = 70, sd = 12),
    "survey_wzrost" = student_data$wzrost,
    "survey_czas"   = student_data$czas_dojazdu
  )
  s3_rob_base_data(base)
  s3_rob_outliers(numeric(0))

  # Update outlier slider range based on data
  data_range <- range(base, na.rm = TRUE)
  data_span <- data_range[2] - data_range[1]
  updateSliderInput(session, "s3_rob_outlier_val",
    min = round(data_range[1] - data_span * 0.5),
    max = round(data_range[2] + data_span * 2),
    value = round(data_range[2] + data_span * 0.5))
}, ignoreNULL = FALSE)

observeEvent(input$s3_rob_reset, {
  s3_rob_outliers(numeric(0))
})

observeEvent(input$s3_rob_add, {
  s3_rob_outliers(c(s3_rob_outliers(), input$s3_rob_outlier_val))
})

observeEvent(input$s3_rob_add5, {
  s3_rob_outliers(c(s3_rob_outliers(), rep(input$s3_rob_outlier_val, 5)))
})

s3_rob_all_data <- reactive({
  req(s3_rob_base_data())
  c(s3_rob_base_data(), s3_rob_outliers())
})

output$s3_rob_plot <- renderPlot({
  req(s3_rob_all_data())

  all_data <- s3_rob_all_data()
  base_data <- s3_rob_base_data()
  outliers <- s3_rob_outliers()
  trim_pct <- input$s3_rob_trim

  m <- mean(all_data)
  med <- median(all_data)
  tm <- mean(all_data, trim = trim_pct)
  tm25 <- mean(all_data, trim = 0.25)

  df <- data.frame(value = all_data)

  # Build legend data
  legend_labels <- c(
    paste0("Srednia: ", round(m, 2)),
    paste0("Mediana: ", round(med, 2)),
    paste0("Sr. ucinana ", round(trim_pct * 100), "%: ", round(tm, 2)),
    paste0("Sr. ucinana 25%: ", round(tm25, 2))
  )
  legend_colors <- c("#e74c3c", "#3498db", "#27ae60", "#f39c12")
  legend_linetypes <- c("solid", "dashed", "dotted", "twodash")

  p <- ggplot(df, aes(x = value)) +
    geom_histogram(bins = 20, fill = "#95a5a6", color = "white", alpha = 0.7)

  # Add outlier points below histogram

if (length(outliers) > 0) {
    df_out <- data.frame(value = outliers)
    p <- p + geom_point(data = df_out, aes(x = value, y = -0.5),
      color = "#e74c3c", size = 4, shape = 17, alpha = 0.8)
  }

  p <- p +
    geom_vline(aes(xintercept = m, color = "Srednia", linetype = "Srednia"), linewidth = 1.3) +
    geom_vline(aes(xintercept = med, color = "Mediana", linetype = "Mediana"), linewidth = 1.3) +
    geom_vline(aes(xintercept = tm, color = "Sr. ucinana", linetype = "Sr. ucinana"), linewidth = 1.3) +
    geom_vline(aes(xintercept = tm25, color = "Sr. ucinana 25%", linetype = "Sr. ucinana 25%"), linewidth = 1.3) +
    scale_color_manual(name = "Statystyka",
      breaks = c("Srednia", "Mediana", "Sr. ucinana", "Sr. ucinana 25%"),
      values = c("Srednia" = "#e74c3c", "Mediana" = "#3498db",
                 "Sr. ucinana" = "#27ae60", "Sr. ucinana 25%" = "#f39c12"),
      labels = legend_labels) +
    scale_linetype_manual(name = "Statystyka",
      breaks = c("Srednia", "Mediana", "Sr. ucinana", "Sr. ucinana 25%"),
      values = c("Srednia" = "solid", "Mediana" = "dashed",
                 "Sr. ucinana" = "dotted", "Sr. ucinana 25%" = "twodash"),
      labels = legend_labels) +
    labs(
      title = "Porownanie odpornosci statystyk polozenia",
      subtitle = paste0("n = ", length(all_data), " (bazowe: ", length(base_data),
                        ", outliery: ", length(outliers), ")"),
      x = "Wartosc", y = "Liczba obserwacji"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      legend.position = "top",
      legend.text = element_text(size = 11)
    )

  p
})

output$s3_rob_table <- renderTable({
  req(s3_rob_all_data(), s3_rob_base_data())

  base <- s3_rob_base_data()
  current <- s3_rob_all_data()
  trim_pct <- input$s3_rob_trim
  trim_label <- paste0("Srednia ucinana ", round(trim_pct * 100), "%")

  base_mean <- mean(base)
  base_median <- median(base)
  base_tm <- mean(base, trim = trim_pct)
  base_tm25 <- mean(base, trim = 0.25)

  cur_mean <- mean(current)
  cur_median <- median(current)
  cur_tm <- mean(current, trim = trim_pct)
  cur_tm25 <- mean(current, trim = 0.25)

  data.frame(
    Statystyka = c("Srednia", "Mediana", trim_label, "Srednia ucinana 25%"),
    `Wartosc bazowa` = round(c(base_mean, base_median, base_tm, base_tm25), 2),
    `Wartosc aktualna` = round(c(cur_mean, cur_median, cur_tm, cur_tm25), 2),
    Zmiana = round(c(cur_mean - base_mean, cur_median - base_median,
                     cur_tm - base_tm, cur_tm25 - base_tm25), 2),
    check.names = FALSE
  )
}, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

output$s3_rob_info <- renderUI({
  trim_pct <- input$s3_rob_trim
  n_outliers <- length(s3_rob_outliers())

  tagList(
    h5("Odpornosc statystyk", style = "font-weight: bold; color: #2c3e50;"),
    p(style = "font-size: 13px;",
      strong("Srednia"), " - reaguje silnie na wartosci odstajace. ",
      "Kazda obserwacja wplywa na jej wartosc."),
    p(style = "font-size: 13px;",
      strong("Mediana"), " - odporna na outliery. Zmienia sie tylko gdy ",
      "obserwacje przesuwaja sie przez srodek rozkladu."),
    p(style = "font-size: 13px;",
      strong("Srednia ucinana"), " - kompromis miedzy srednia a mediana. ",
      "Odrzuca ", round(trim_pct * 100), "% skrajnych obserwacji z kazdej strony."),
    if (n_outliers > 0) {
      p(style = "font-size: 13px; color: #e74c3c; font-weight: bold;",
        paste0("Dodano ", n_outliers, " outlier",
               ifelse(n_outliers == 1, "", ifelse(n_outliers < 5, "y", "ow")),
               ". Obserwuj jak rozne statystyki reaguja!"))
    }
  )
})

# ============================================================================
# Tab 3.3: Kwantyle i percentyle
# ============================================================================

observeEvent(input$s3_q_q1, {
  updateSliderInput(session, "s3_q_percentile", value = 25)
})

observeEvent(input$s3_q_med, {
  updateSliderInput(session, "s3_q_percentile", value = 50)
})

observeEvent(input$s3_q_q3, {
  updateSliderInput(session, "s3_q_percentile", value = 75)
})

s3_q_selected_data <- reactive({
  req(input$s3_q_var)
  student_data[[input$s3_q_var]]
})

s3_q_percentile_value <- reactive({
  req(s3_q_selected_data())
  quantile(s3_q_selected_data(), probs = input$s3_q_percentile / 100, na.rm = TRUE)
})

s3_q_var_label <- reactive({
  switch(input$s3_q_var,
    "wzrost" = "Wzrost (cm)",
    "srednia_ocen" = "Srednia ocen",
    "czas_dojazdu" = "Czas dojazdu (min)",
    "waga" = "Waga (kg)")
})

output$s3_q_hist_plot <- renderPlot({
  req(s3_q_selected_data())

  vals <- s3_q_selected_data()
  pct <- input$s3_q_percentile / 100
  pct_val <- s3_q_percentile_value()
  var_label <- s3_q_var_label()

  df <- data.frame(value = vals)

  # Compute histogram breaks to shade below percentile
  ggplot(df, aes(x = value)) +
    geom_histogram(data = df[df$value <= pct_val, , drop = FALSE],
      aes(x = value), bins = 25, fill = "#3498db", color = "white", alpha = 0.8) +
    geom_histogram(data = df[df$value > pct_val, , drop = FALSE],
      aes(x = value), bins = 25, fill = "#bdc3c7", color = "white", alpha = 0.6) +
    geom_vline(xintercept = pct_val, color = "#e74c3c", linewidth = 1.5) +
    annotate("text", x = pct_val, y = Inf,
      label = paste0("P", input$s3_q_percentile, " = ", round(pct_val, 2)),
      vjust = 2, hjust = -0.1, color = "#e74c3c", size = 5.5, fontface = "bold") +
    annotate("text",
      x = min(vals) + (pct_val - min(vals)) / 2, y = 0,
      label = paste0(input$s3_q_percentile, "%"),
      vjust = -0.5, color = "#2c3e50", size = 6, fontface = "bold") +
    labs(
      title = paste0("Percentyl ", input$s3_q_percentile, " zmiennej: ", var_label),
      subtitle = paste0(input$s3_q_percentile, "% obserwacji ponizej zaznaczonej wartosci"),
      x = var_label, y = "Liczba obserwacji"
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 18, face = "bold"))
})

output$s3_q_box_plot <- renderPlot({
  req(s3_q_selected_data())

  vals <- s3_q_selected_data()
  pct_val <- s3_q_percentile_value()
  var_label <- s3_q_var_label()

  q1 <- quantile(vals, 0.25, na.rm = TRUE)
  q2 <- quantile(vals, 0.50, na.rm = TRUE)
  q3 <- quantile(vals, 0.75, na.rm = TRUE)

  df <- data.frame(value = vals)

  ggplot(df, aes(x = value, y = 0)) +
    geom_boxplot(fill = "#ecf0f1", color = "#2c3e50", width = 0.5, outlier.color = "#e74c3c") +
    geom_vline(xintercept = pct_val, color = "#e74c3c", linewidth = 1.5) +
    annotate("text", x = q1, y = -0.45, label = paste0("Q1\n", round(q1, 1)),
      color = "#3498db", size = 3.5, fontface = "bold") +
    annotate("text", x = q2, y = -0.45, label = paste0("Q2\n", round(q2, 1)),
      color = "#e74c3c", size = 3.5, fontface = "bold") +
    annotate("text", x = q3, y = -0.45, label = paste0("Q3\n", round(q3, 1)),
      color = "#3498db", size = 3.5, fontface = "bold") +
    labs(x = var_label) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    ylim(-0.7, 0.7)
})

output$s3_q_info <- renderUI({
  req(s3_q_selected_data())

  vals <- s3_q_selected_data()
  pct_val <- s3_q_percentile_value()
  pct <- input$s3_q_percentile
  var_label <- s3_q_var_label()

  q1 <- round(quantile(vals, 0.25, na.rm = TRUE), 2)
  q2 <- round(quantile(vals, 0.50, na.rm = TRUE), 2)
  q3 <- round(quantile(vals, 0.75, na.rm = TRUE), 2)
  iqr_val <- round(q3 - q1, 2)

  tagList(
    h5("Kwartyle", style = "font-weight: bold; color: #2c3e50;"),
    p(style = "font-size: 13px;",
      strong("Q1 (25%):"), q1, br(),
      strong("Q2 (50%):"), q2, br(),
      strong("Q3 (75%):"), q3, br(),
      strong("IQR:"), iqr_val),
    hr(),
    h5("Wybrany percentyl", style = "font-weight: bold; color: #e74c3c;"),
    p(style = "font-size: 13px;",
      strong(paste0("P", pct, ":")), round(pct_val, 2))
  )
})

output$s3_q_interpretation <- renderUI({
  req(s3_q_selected_data())

  pct <- input$s3_q_percentile
  pct_val <- round(s3_q_percentile_value(), 2)
  var_label <- s3_q_var_label()

  div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 10px;",
    h4(style = "color: #2c3e50; margin-top: 0;", "Interpretacja"),
    p(style = "font-size: 16px;",
      strong(paste0(pct, "%")),
      " obserwacji ma wartosc zmiennej ",
      em(var_label),
      " ponizej ",
      strong(pct_val), ".")
  )
})

# END SECTION 3
# SECTION 4: Statystyki rozrzutu

# ============================================================================
# Tab 4.1: Srednia to nie wszystko
# ============================================================================

spread_scenarios <- list(
  autobusy = list(
    name_a = "Linia A", name_b = "Linia B",
    mean_label = "Srednie spoznie (min):", mean_min = 0, mean_max = 10, mean_value = 3, mean_step = 0.5,
    sd_label_a = "Odch. std. linii A (min):", sd_label_b = "Odch. std. linii B (min):",
    sd_a = 1, sd_b = 5,
    buffer_label = "Ile minut wczesniej wychodzisz:", buffer_min = 0, buffer_max = 20, buffer_value = 5, buffer_step = 0.5,
    x_label = "Spoznie (minuty)", prob_label = "ze sie spoznisz",
    story = list(s1 = "Wyobraz sobie dwie linie autobusowe. Obie maja to samo srednie spoznie.", s2 = "Ale linia A jest punktualna (male odchylenie), a linia B — nieprzewidywalna (duze odchylenie). Srednia jest taka sama!", s3 = "Wychodzisz na przystanek kilka minut wczesniej. Zacieniowany obszar to ryzyko, ze autobus juz odjechal.", s4 = "Mimo tej samej sredniej, linia B daje znacznie wieksze ryzyko spoznienia. Sama srednia nie wystarczy do oceny sytuacji!")
  ),
  produkcja = list(
    name_a = "Maszyna A", name_b = "Maszyna B",
    mean_label = "Nominalna srednica (mm):", mean_min = 8, mean_max = 12, mean_value = 10, mean_step = 0.1,
    sd_label_a = "Odch. std. maszyny A (mm):", sd_label_b = "Odch. std. maszyny B (mm):",
    sd_a = 0.02, sd_b = 0.08,
    buffer_label = "Tolerancja +/- (mm):", buffer_min = 0.01, buffer_max = 0.2, buffer_value = 0.05, buffer_step = 0.01,
    x_label = "Srednica srubki (mm)", prob_label = "wadliwych produktow",
    story = list(s1 = "Dwie maszyny produkuja srubki. Obie celuja w te sama srednia srednice.", s2 = "Ale maszyna A jest precyzyjna, a maszyna B — rozrzutna. Srednia srednica jest taka sama!", s3 = "Klient wymaga tolerancji +/- pewna wartosc. Srubki poza zakresem to braki.", s4 = "Maszyna B produkuje duzo wiecej brakow. Srednia ta sama, ale koszty zupelnie inne!")
  ),
  egzamin = list(
    name_a = "Grupa A", name_b = "Grupa B",
    mean_label = "Sredni wynik (pkt):", mean_min = 30, mean_max = 80, mean_value = 60, mean_step = 1,
    sd_label_a = "Odch. std. grupy A (pkt):", sd_label_b = "Odch. std. grupy B (pkt):",
    sd_a = 5, sd_b = 15,
    buffer_label = "Prog zaliczenia (pkt):", buffer_min = 30, buffer_max = 80, buffer_value = 50, buffer_step = 1,
    x_label = "Wynik egzaminu (punkty)", prob_label = "ze obleje",
    story = list(s1 = "Dwie grupy studentow pisza egzamin. Sredni wynik w obu grupach jest taki sam.", s2 = "Ale w grupie A wyniki sa skupione, a w grupie B — bardzo rozproszone. Ta sama srednia!", s3 = "Ustalamy prog zaliczenia. Zacieniowany obszar to osoby, ktore nie zaliczyly.", s4 = "W grupie B oblewa wiecej osob, ale tez wiecej dostaje najwyzsze noty. Sama srednia tego nie pokaze!")
  )
)

spread_step <- reactiveVal(1)
observeEvent(input$spread_step1, { spread_step(1) })
observeEvent(input$spread_step2, { spread_step(2) })
observeEvent(input$spread_step3, { spread_step(3) })
observeEvent(input$spread_step4, { spread_step(4) })
observeEvent(input$spread_reset, { spread_step(1) })
observeEvent(input$spread_scenario, { spread_step(1) })

output$spread_story <- renderUI({
  s <- spread_scenarios[[input$spread_scenario]]
  step <- spread_step()
  story_key <- paste0("s", step)
  tags$div(class = "desc-box", style = "font-size: 16px; margin-bottom: 15px;",
           tags$b(paste0("Krok ", step, "/4: ")), s$story[[story_key]])
})

output$spread_controls <- renderUI({
  s <- spread_scenarios[[input$spread_scenario]]
  step <- spread_step()
  controls <- list()
  if (step >= 1) {
    controls <- c(controls, list(sliderInput("spread_mean", s$mean_label,
                                             min = s$mean_min, max = s$mean_max,
                                             value = s$mean_value, step = s$mean_step)))
  }
  if (step >= 2) {
    controls <- c(controls, list(
      sliderInput("spread_sd_a", s$sd_label_a,
                  min = s$sd_a * 0.2, max = s$sd_a * 10,
                  value = s$sd_a, step = s$sd_a * 0.2),
      sliderInput("spread_sd_b", s$sd_label_b,
                  min = s$sd_b * 0.2, max = s$sd_b * 10,
                  value = s$sd_b, step = s$sd_b * 0.2)
    ))
  }
  if (step >= 3) {
    controls <- c(controls, list(sliderInput("spread_buffer", s$buffer_label,
                                             min = s$buffer_min, max = s$buffer_max,
                                             value = s$buffer_value, step = s$buffer_step)))
  }
  do.call(tagList, controls)
})

output$spread_bottom <- renderUI({
  req(spread_step() >= 4)
  s <- spread_scenarios[[input$spread_scenario]]
  probs <- spread_probs()
  ratio_text <- ""
  if (probs$a > 0) {
    ratio <- round(probs$b / probs$a, 1)
    if (ratio > 1) {
      ratio_text <- paste0(s$name_b, " ma ", ratio, "x wieksze ryzyko niz ", s$name_a, "!")
    }
  }
  tags$div(
    br(),
    fluidRow(
      column(4, div(style = "background: #eaf2f8; padding: 20px; border-radius: 8px; text-align: center;",
                    tags$h3(style = "color: #3498db; margin: 0;", paste0(round(probs$a * 100, 1), "%")),
                    tags$p(style = "color: #3498db; margin: 5px 0 0 0;", paste0(s$name_a, ": ", s$prob_label)))),
      column(4, div(style = "background: #fdedec; padding: 20px; border-radius: 8px; text-align: center;",
                    tags$h3(style = "color: #e74c3c; margin: 0;", paste0(round(probs$b * 100, 1), "%")),
                    tags$p(style = "color: #e74c3c; margin: 5px 0 0 0;", paste0(s$name_b, ": ", s$prob_label)))),
      column(4, div(style = "background: #f8f9fa; padding: 20px; border-radius: 8px; text-align: center;",
                    tags$h3(style = "color: #2c3e50; margin: 0;", ratio_text),
                    tags$p(style = "margin: 5px 0 0 0;", "A srednia jest taka sama!")))
    ),
    br(),
    div(class = "desc-box",
        tags$h4("Wniosek"),
        tags$p("Srednia (i mediana) to ", tags$b("statystyki polozenia"), " — mowia, gdzie jest 'srodek' danych."),
        tags$p("Ale nie mowia nic o tym, ", tags$b("jak bardzo dane sa rozproszone"), " wokol tego srodka."),
        tags$p("Dlatego potrzebujemy ", tags$b("statystyk rozrzutu"), " (odchylenie standardowe, wariancja, IQR)."))
  )
})

spread_probs <- reactive({
  s <- spread_scenarios[[input$spread_scenario]]
  mu <- input$spread_mean %||% s$mean_value
  sd_a <- input$spread_sd_a %||% s$sd_a
  sd_b <- input$spread_sd_b %||% s$sd_b
  buf <- input$spread_buffer %||% s$buffer_value
  if (input$spread_scenario == "produkcja") {
    prob_a <- 1 - (pnorm(mu + buf, mu, sd_a) - pnorm(mu - buf, mu, sd_a))
    prob_b <- 1 - (pnorm(mu + buf, mu, sd_b) - pnorm(mu - buf, mu, sd_b))
  } else if (input$spread_scenario == "egzamin") {
    prob_a <- pnorm(buf, mu, sd_a)
    prob_b <- pnorm(buf, mu, sd_b)
  } else {
    prob_a <- 1 - pnorm(buf, mu, sd_a)
    prob_b <- 1 - pnorm(buf, mu, sd_b)
  }
  list(a = prob_a, b = prob_b)
})

output$spread_density_plot <- renderPlot({
  s <- spread_scenarios[[input$spread_scenario]]
  step <- spread_step()
  mu <- input$spread_mean %||% s$mean_value
  sd_a <- input$spread_sd_a %||% s$sd_a
  sd_b <- input$spread_sd_b %||% s$sd_b
  buf <- input$spread_buffer %||% s$buffer_value
  max_sd <- max(sd_a, sd_b)
  x_range <- seq(mu - 4 * max_sd, mu + 4 * max_sd, length.out = 500)

  if (step == 1) {
    df <- data.frame(x = x_range, density = dnorm(x_range, mu, sd_a))
    p <- ggplot(df, aes(x = x, y = density)) +
      geom_blank() +
      geom_vline(xintercept = mu, linewidth = 1.5, color = "#2c3e50") +
      annotate("text", x = mu, y = max(df$density) * 0.5,
               label = paste0("Srednia = ", mu), hjust = -0.15, size = 6,
               fontface = "bold", color = "#2c3e50") +
      labs(title = "Dwie grupy z ta sama srednia",
           x = s$x_label, y = "Gestosc prawdopodobienstwa") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(size = 18, face = "bold"))
    return(p)
  }

  df <- data.frame(
    x = rep(x_range, 2),
    density = c(dnorm(x_range, mu, sd_a), dnorm(x_range, mu, sd_b)),
    group = rep(c(s$name_a, s$name_b), each = 500)
  )

  p <- ggplot(df, aes(x = x, y = density, fill = group, color = group)) +
    geom_line(linewidth = 1.2) +
    geom_area(alpha = 0.2, position = "identity") +
    geom_vline(xintercept = mu, linewidth = 0.8, color = "#2c3e50", alpha = 0.5) +
    annotate("text", x = mu, y = max(df$density) * 0.95,
             label = paste0("Srednia = ", mu), hjust = -0.1, size = 4, color = "#2c3e50") +
    scale_fill_manual(values = c(setNames("#3498db", s$name_a), setNames("#e74c3c", s$name_b))) +
    scale_color_manual(values = c(setNames("#3498db", s$name_a), setNames("#e74c3c", s$name_b)))

  if (step >= 3) {
    if (input$spread_scenario == "produkcja") {
      df_shade_a <- df %>% filter(group == s$name_a, x < mu - buf | x > mu + buf)
      df_shade_b <- df %>% filter(group == s$name_b, x < mu - buf | x > mu + buf)
      p <- p +
        geom_area(data = df_shade_a, aes(x = x, y = density), fill = "#3498db", alpha = 0.5, inherit.aes = FALSE) +
        geom_area(data = df_shade_b, aes(x = x, y = density), fill = "#e74c3c", alpha = 0.5, inherit.aes = FALSE) +
        geom_vline(xintercept = c(mu - buf, mu + buf), linetype = "dashed", linewidth = 1, color = "#2c3e50") +
        annotate("text", x = mu - buf, y = max(df$density) * 1.05, label = paste0("-", buf, " mm"), hjust = 1.1, size = 4, fontface = "bold") +
        annotate("text", x = mu + buf, y = max(df$density) * 1.05, label = paste0("+", buf, " mm"), hjust = -0.1, size = 4, fontface = "bold")
    } else if (input$spread_scenario == "egzamin") {
      df_shade_a <- df %>% filter(group == s$name_a, x < buf)
      df_shade_b <- df %>% filter(group == s$name_b, x < buf)
      p <- p +
        geom_area(data = df_shade_a, aes(x = x, y = density), fill = "#3498db", alpha = 0.5, inherit.aes = FALSE) +
        geom_area(data = df_shade_b, aes(x = x, y = density), fill = "#e74c3c", alpha = 0.5, inherit.aes = FALSE) +
        geom_vline(xintercept = buf, linetype = "dashed", linewidth = 1, color = "#2c3e50") +
        annotate("text", x = buf, y = max(df$density) * 1.05, label = paste0("Prog: ", buf, " pkt"), hjust = -0.1, size = 4.5, fontface = "bold")
    } else {
      df_shade_a <- df %>% filter(group == s$name_a, x > buf)
      df_shade_b <- df %>% filter(group == s$name_b, x > buf)
      p <- p +
        geom_area(data = df_shade_a, aes(x = x, y = density), fill = "#3498db", alpha = 0.5, inherit.aes = FALSE) +
        geom_area(data = df_shade_b, aes(x = x, y = density), fill = "#e74c3c", alpha = 0.5, inherit.aes = FALSE) +
        geom_vline(xintercept = buf, linetype = "dashed", linewidth = 1, color = "#2c3e50") +
        annotate("text", x = buf, y = max(df$density) * 1.05, label = paste0("Bufor: ", buf, " min"), hjust = -0.1, size = 4.5, fontface = "bold")
    }
  }

  if (step >= 4) {
    probs <- spread_probs()
    p <- p +
      annotate("label", x = mu + 3 * max_sd, y = max(df$density) * 0.85,
               label = paste0(s$name_a, ": ", round(probs$a * 100, 1), "%"),
               fill = "#3498db", color = "white", size = 5, fontface = "bold") +
      annotate("label", x = mu + 3 * max_sd, y = max(df$density) * 0.70,
               label = paste0(s$name_b, ": ", round(probs$b * 100, 1), "%"),
               fill = "#e74c3c", color = "white", size = 5, fontface = "bold")
  }

  subtitle <- if (step >= 2) {
    paste0("Ta sama srednia (", mu, "), rozne odchylenia standardowe")
  } else {
    NULL
  }

  p + labs(title = paste0(s$name_a, " vs ", s$name_b),
           subtitle = subtitle,
           x = s$x_label, y = "Gestosc prawdopodobienstwa",
           fill = "Grupa", color = "Grupa") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 18, face = "bold"),
          legend.position = if (step >= 2) "bottom" else "none",
          legend.text = element_text(size = 12))
})

# ============================================================================
# Tab 4.2: SD krok po kroku
# ============================================================================

sd_scenarios <- list(
  skupione = list(name = "Bardzo skupione", mean = 50, sd = 2, color = "#27ae60"),
  umiarkowane = list(name = "Umiarkowanie rozproszone", mean = 50, sd = 5, color = "#f39c12"),
  rozproszone = list(name = "Bardzo rozproszone", mean = 50, sd = 10, color = "#e74c3c")
)

sd_data <- reactiveVal(rnorm(20, mean = 50, sd = 2))
sd_current_step <- reactiveVal(0)

observeEvent(input$sd_scenario, {
  scenario <- sd_scenarios[[input$sd_scenario]]
  sd_data(rnorm(20, mean = scenario$mean, sd = scenario$sd))
  sd_current_step(0)
})

observeEvent(input$sd_regenerate, {
  scenario <- sd_scenarios[[input$sd_scenario]]
  sd_data(rnorm(20, mean = scenario$mean, sd = scenario$sd))
  sd_current_step(0)
})

observeEvent(input$sd_reset, { sd_current_step(0) })
observeEvent(input$sd_step1, { sd_current_step(1) })
observeEvent(input$sd_step2, { sd_current_step(2) })
observeEvent(input$sd_step3, { sd_current_step(3) })
observeEvent(input$sd_step4, { sd_current_step(4) })

sd_mean_val <- reactive({ mean(sd_data()) })
sd_sd_val <- reactive({ sd(sd_data()) })

output$sd_n_text <- renderText({
  paste0("n = ", length(sd_data()), " obserwacji")
})

output$sd_mean_text <- renderText({
  paste0("Srednia: ", round(sd_mean_val(), 2))
})

output$sd_sd_text <- renderText({
  paste0("Odchylenie standardowe: ", round(sd_sd_val(), 2))
})

output$sd_step_title <- renderText({
  step <- sd_current_step()
  if (step == 0) return("Wylosuj dane i kliknij 'Krok 1', aby rozpoczac")
  if (step == 1) return("Krok 1: Surowe dane")
  if (step == 2) return("Krok 2: Obliczanie sredniej")
  if (step == 3) return("Krok 3: Odleglosci od sredniej i ich kwadraty")
  if (step == 4) return("Krok 4: Odchylenie standardowe")
})

output$sd_step_explanation <- renderText({
  step <- sd_current_step()
  n <- length(sd_data())
  if (step == 0) return(paste0("Wylosowano ", n, " obserwacji. Kliknij kolejne kroki, aby zobaczyc jak obliczamy odchylenie standardowe."))
  if (step == 1) return(paste0("To sa nasze surowe dane: ", n, " wylosowanych wartosci. Kazdy punkt to jedna obserwacja."))
  if (step == 2) return(paste0("Srednia to 'srodek ciezkosci' danych. Obliczamy ja jako sume wszystkich wartosci podzielona przez ich liczbe. Srednia = ", round(sd_mean_val(), 2)))
  if (step == 3) return("Dla kazdego punktu obliczamy odleglosc od sredniej (x - x_sr), a potem podnosimy ja do kwadratu. Zobacz tabele ponizej!")
  if (step == 4) return(paste0("Suma kwadratow / (n-1) = Wariancja. Pierwiastek z wariancji = SD = ", round(sd_sd_val(), 2), ". To 'typowa odleglosc' punktu od sredniej. Zobacz obliczenia w tabeli!"))
})

output$sd_main_plot <- renderPlot({
  scenario <- sd_scenarios[[input$sd_scenario]]
  step <- sd_current_step()

  df <- data.frame(
    value = sd_data(),
    index = seq_along(sd_data())
  )
  df$deviation <- df$value - sd_mean_val()
  df$squared_dev <- df$deviation^2
  df$distance <- abs(df$deviation)
  df$distance_normalized <- df$distance / max(df$distance + 0.001)

  if (step == 0) {
    ggplot() +
      annotate("text", x = 50, y = 0,
               label = "Kliknij 'Krok 1', aby rozpoczac",
               size = 6, color = "gray50") +
      theme_void() +
      xlim(20, 80) + ylim(-0.5, 0.5)

  } else if (step == 1) {
    ggplot(df, aes(x = value, y = 0)) +
      geom_jitter(height = 0.3, size = 4, alpha = 0.7, color = scenario$color) +
      labs(x = "Wartosc", y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(20, 80) + ylim(-0.5, 0.5)

  } else if (step == 2) {
    ggplot(df, aes(x = value, y = 0)) +
      geom_jitter(height = 0.3, size = 4, alpha = 0.7, color = scenario$color) +
      geom_vline(xintercept = sd_mean_val(), color = "#e74c3c", linewidth = 2, linetype = "solid") +
      annotate("text", x = sd_mean_val(), y = 0.45,
               label = paste0("x_sr = ", round(sd_mean_val(), 2)),
               color = "#e74c3c", size = 6, fontface = "bold") +
      labs(x = "Wartosc", y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(20, 80) + ylim(-0.5, 0.5)

  } else if (step == 3) {
    df_sorted <- df %>% arrange(value)
    n <- nrow(df_sorted)
    df_sorted$y_pos <- seq(from = -0.4, to = 0.4, length.out = n)

    ggplot(df_sorted, aes(x = value, y = y_pos)) +
      geom_segment(aes(x = value, xend = sd_mean_val(), y = y_pos, yend = y_pos),
                   color = "#9b59b6", linewidth = 1, alpha = 0.7,
                   arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
      geom_point(size = 4, alpha = 0.8, color = scenario$color) +
      geom_vline(xintercept = sd_mean_val(), color = "#e74c3c", linewidth = 2, linetype = "solid") +
      annotate("text", x = sd_mean_val(), y = 0.45,
               label = paste0("x_sr = ", round(sd_mean_val(), 2)),
               color = "#e74c3c", size = 5, fontface = "bold") +
      labs(x = "Wartosc", y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(20, 80) + ylim(-0.5, 0.5)

  } else if (step == 4) {
    ggplot(df, aes(x = value, y = 0)) +
      annotate("rect", xmin = sd_mean_val() - 2 * sd_sd_val(), xmax = sd_mean_val() + 2 * sd_sd_val(),
               ymin = -0.5, ymax = 0.5, alpha = 0.1, fill = "#3498db") +
      annotate("rect", xmin = sd_mean_val() - sd_sd_val(), xmax = sd_mean_val() + sd_sd_val(),
               ymin = -0.5, ymax = 0.5, alpha = 0.2, fill = "#3498db") +
      geom_jitter(height = 0.3, size = 4, alpha = 0.7, color = scenario$color) +
      geom_vline(xintercept = sd_mean_val(), color = "#e74c3c", linewidth = 2, linetype = "solid") +
      geom_vline(xintercept = sd_mean_val() - sd_sd_val(), color = "#3498db", linewidth = 1.5, linetype = "dashed") +
      geom_vline(xintercept = sd_mean_val() + sd_sd_val(), color = "#3498db", linewidth = 1.5, linetype = "dashed") +
      annotate("text", x = sd_mean_val(), y = 0.45,
               label = paste0("SD = ", round(sd_sd_val(), 2)),
               color = "#3498db", size = 6, fontface = "bold") +
      annotate("text", x = sd_mean_val(), y = -0.45,
               label = "+/-1SD (68% danych)",
               color = "#3498db", size = 4, fontface = "bold") +
      labs(x = "Wartosc", y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(20, 80) + ylim(-0.5, 0.5)
  }
})

output$sd_calculations_table <- renderTable({
  step <- sd_current_step()

  if (step < 1) {
    return(data.frame(Info = "Obliczenia pojawia sie od kroku 1"))
  }

  vals <- sd_data()
  mean_v <- sd_mean_val()
  n <- length(vals)
  show_n <- min(10, n)

  df <- data.frame(
    Nr = 1:show_n,
    Wartosc = round(vals[1:show_n], 2)
  )

  if (step >= 2) {
    df$Srednia <- round(mean_v, 2)
  }

  if (step >= 3) {
    df$Odleglosc <- round(vals[1:show_n] - mean_v, 2)
    df$Kwadrat <- round((vals[1:show_n] - mean_v)^2, 2)
  }

  if (step >= 4) {
    sum_squared <- sum((vals - mean_v)^2)
    variance <- sum_squared / (n - 1)
    summary_text <- data.frame(
      Info = c(
        paste0("Suma kwadratow = ", round(sum_squared, 2)),
        paste0("Wariancja = ", round(sum_squared, 2), " / ", (n - 1), " = ", round(variance, 2)),
        paste0("Odchylenie standardowe = sqrt(", round(variance, 2), ") = ", round(sd_sd_val(), 2))
      )
    )
    return(summary_text)
  }

  df
}, striped = TRUE, hover = TRUE, bordered = TRUE)

# ============================================================================
# Tab 4.3: Budowa boxplota
# ============================================================================

# Local data generators
generate_bp_autobusy_data <- function(n) {
  set.seed(NULL)
  grupa <- sample(c("rano", "popoludnie", "wieczor"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  opoznienie_min <- ifelse(grupa == "rano", rnorm(n, mean = 2, sd = 3),
                    ifelse(grupa == "popoludnie", rnorm(n, mean = 5, sd = 5),
                           rnorm(n, mean = 1, sd = 2)))
  round(pmax(opoznienie_min, -2), 1)
}

generate_bp_kac_data <- function(n) {
  set.seed(NULL)
  jednostki_alkoholu <- rpois(n, lambda = 5) + 1
  woda_wypita <- sample(c("malo", "srednio", "duzo"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
  jedzenie_przed <- sample(c("tak", "nie"), n, replace = TRUE, prob = c(0.6, 0.4))
  mieszanie_alkoholi <- sample(c("tak", "nie"), n, replace = TRUE, prob = c(0.3, 0.7))
  godziny_snu <- pmax(2, pmin(12, round(rnorm(n, mean = 6, sd = 2), 1)))
  plec <- sample(c("K", "M"), n, replace = TRUE)
  intensywnosc_kaca <- (
    jednostki_alkoholu * 1.2 +
    ifelse(woda_wypita == "malo", 3, ifelse(woda_wypita == "srednio", 1, -1)) +
    ifelse(jedzenie_przed == "nie", 2, -1) +
    ifelse(mieszanie_alkoholi == "tak", 2, 0) +
    (8 - godziny_snu) * 0.5 +
    ifelse(plec == "K", 1, 0) +
    rnorm(n, 0, 1)
  )
  round(pmax(0, pmin(10, intensywnosc_kaca)), 1)
}

generate_bp_sklep_data <- function(n) {
  set.seed(NULL)
  grupa <- sample(c("poranni", "dzienni", "wieczorni"), n, replace = TRUE, prob = c(0.17, 0.50, 0.33))
  godzina_zakupow <- ifelse(grupa == "poranni", rnorm(n, 8, 1),
                      ifelse(grupa == "dzienni", rnorm(n, 14, 3),
                             rnorm(n, 19, 1.5)))
  godzina_zakupow <- pmax(6, pmin(22, round(godzina_zakupow, 1)))
  dzien_tygodnia <- sample(c("1_pon", "2_wt", "3_sr", "4_czw", "5_pt", "6_sob", "7_nd"), n,
                           replace = TRUE, prob = c(0.12, 0.12, 0.12, 0.12, 0.18, 0.2, 0.14))
  liczba_produktow <- dplyr::case_when(
    dzien_tygodnia == "6_sob" ~ rpois(n, lambda = 25) + 5,
    godzina_zakupow < 10 ~ rpois(n, lambda = 5) + 1,
    TRUE ~ rpois(n, lambda = 12) + 3
  )
  kwota <- liczba_produktow * rnorm(n, mean = 8, sd = 3)
  round(pmax(10, kwota), 2)
}

bp_scenarios <- list(
  autobusy = list(
    title = "Spoznienia Autobusu",
    generator = generate_bp_autobusy_data,
    unit = "min",
    x_label = "Spoznienie (minuty)",
    x_min = -2, x_max = 20
  ),
  kac = list(
    title = "Intensywnosc Kaca",
    generator = generate_bp_kac_data,
    unit = "pkt",
    x_label = "Intensywnosc kaca (0-10)",
    x_min = 0, x_max = 10
  ),
  sklep = list(
    title = "Kwota Zakupow",
    generator = generate_bp_sklep_data,
    unit = "zl",
    x_label = "Kwota zakupow (zl)",
    x_min = 0, x_max = 500
  )
)

bp_data <- reactiveVal(numeric(0))
bp_current_step <- reactiveVal(0)

bp_sampled_data <- reactive({
  scenario <- bp_scenarios[[input$bp_scenario]]
  scenario$generator(input$bp_n_obs)
})

observeEvent(c(input$bp_scenario, input$bp_n_obs), {
  bp_data(bp_sampled_data())
  bp_current_step(0)
}, ignoreNULL = FALSE)

observeEvent(input$bp_draw_new, {
  bp_data(bp_sampled_data())
  bp_current_step(0)
})

observeEvent(input$bp_reset, { bp_current_step(0) })
observeEvent(input$bp_step1, { bp_current_step(1) })
observeEvent(input$bp_step2, { bp_current_step(2) })
observeEvent(input$bp_step3, { bp_current_step(3) })
observeEvent(input$bp_step4, { bp_current_step(4) })
observeEvent(input$bp_step5, { bp_current_step(5) })
observeEvent(input$bp_step6, { bp_current_step(6) })
observeEvent(input$bp_step7, { bp_current_step(7) })
observeEvent(input$bp_step8, { bp_current_step(8) })

bp_sorted_data <- reactive({ sort(bp_data()) })
bp_q1 <- reactive({ quantile(bp_data(), 0.25) })
bp_q2 <- reactive({ median(bp_data()) })
bp_q3 <- reactive({ quantile(bp_data(), 0.75) })
bp_iqr <- reactive({ bp_q3() - bp_q1() })
bp_lower_whisker <- reactive({ max(min(bp_data()), bp_q1() - 1.5 * bp_iqr()) })
bp_upper_whisker <- reactive({ min(max(bp_data()), bp_q3() + 1.5 * bp_iqr()) })
bp_outliers <- reactive({ bp_data()[bp_data() < bp_lower_whisker() | bp_data() > bp_upper_whisker()] })

output$bp_step_title <- renderText({
  scenario <- bp_scenarios[[input$bp_scenario]]
  step <- bp_current_step()
  if (step == 0) return(paste("Dane:", scenario$title, "- Kliknij 'Krok 1', aby rozpoczac"))
  if (step == 1) return("Krok 1: Surowe dane")
  if (step == 2) return("Krok 2: Posortowane dane")
  if (step == 3) return("Krok 3: Mediana (Q2)")
  if (step == 4) return("Krok 4: Kwartyle (Q1 i Q3)")
  if (step == 5) return("Krok 5: Rozstep miedzykwartylowy (IQR)")
  if (step == 6) return("Krok 6: Wasy")
  if (step == 7) return("Krok 7: Outliery")
  if (step == 8) return("Krok 8: Pelny Box Plot")
})

output$bp_step_explanation <- renderText({
  scenario <- bp_scenarios[[input$bp_scenario]]
  step <- bp_current_step()
  n <- length(bp_data())
  unit <- scenario$unit

  if (step == 0) return(paste0("Wylosowano ", n, " obserwacji. Rozpocznij budowe wykresu pudelkowego krok po kroku."))
  if (step == 1) return(paste0("To sa nasze surowe dane: ", n, " wylosowanych obserwacji. Kazdy punkt to jedna obserwacja."))
  if (step == 2) return(paste0("Sortujemy dane od najmniejszej do najwiekszej wartosci. To pozwala znalezc percentyle. Min = ", round(min(bp_data()), 1), " ", unit, ", Max = ", round(max(bp_data()), 1), " ", unit))
  if (step == 3) return(paste0("Mediana (Q2) to wartosc srodkowa - polowa danych jest ponizej, polowa powyzej. Mediana = ", round(bp_q2(), 1), " ", unit))
  if (step == 4) return(paste0("Q1 (25. percentyl) = ", round(bp_q1(), 1), " ", unit, " | Q3 (75. percentyl) = ", round(bp_q3(), 1), " ", unit, ". Srodkowe 50% danych znajduje sie miedzy Q1 a Q3."))
  if (step == 5) return(paste0("IQR (Inter-Quartile Range) = Q3 - Q1 = ", round(bp_q3(), 1), " - ", round(bp_q1(), 1), " = ", round(bp_iqr(), 1), " ", unit, ". To miara rozproszenia srodkowych 50% danych."))
  if (step == 6) return(paste0("Wasy pokazuja zakres 'typowych' wartosci. Dolny was = ", round(bp_lower_whisker(), 1), " ", unit, ", Gorny was = ", round(bp_upper_whisker(), 1), " ", unit, ". Obliczamy je jako Q1 - 1.5*IQR i Q3 + 1.5*IQR (ale nie dalej niz min/max)."))
  if (step == 7) {
    outlier_text <- if (length(bp_outliers()) > 0) paste(round(bp_outliers(), 1), collapse = ", ") else "brak"
    return(paste0("Outliery to wartosci poza wasami. Znaleziono ", length(bp_outliers()), " outlier(ow): ", outlier_text, ". To nietypowe obserwacje."))
  }
  if (step == 8) return("Pelny box plot (horizontal) - latwo porownac z histogramem ponizej. Pudelko = Q1 do Q3, linia = mediana, wasy = zakres typowych wartosci.")
})

output$bp_main_plot <- renderPlot({
  scenario <- bp_scenarios[[input$bp_scenario]]
  step <- bp_current_step()
  x_min <- scenario$x_min
  x_max <- scenario$x_max
  x_label <- scenario$x_label

  if (step == 0) {
    ggplot() +
      annotate("text", x = (x_min + x_max) / 2, y = 0.5,
               label = "Wylosuj dane i kliknij 'Krok 1'",
               size = 6, color = "gray50") +
      theme_void() +
      xlim(x_min, x_max) + ylim(0, 1)

  } else if (step == 1) {
    df <- data.frame(value = bp_data(), index = seq_along(bp_data()))
    ggplot(df, aes(x = value, y = 0)) +
      geom_jitter(height = 0.3, size = 4, alpha = 0.6, color = "#3498db") +
      labs(x = x_label, y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(x_min, x_max) + ylim(-0.5, 0.5)

  } else if (step == 2) {
    df <- data.frame(value = bp_sorted_data(), index = seq_along(bp_sorted_data()))
    ggplot(df, aes(x = value, y = 0)) +
      geom_point(size = 4, alpha = 0.7, color = "#27ae60") +
      labs(x = x_label, y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(x_min, x_max) + ylim(-0.5, 0.5)

  } else if (step == 3) {
    df <- data.frame(value = bp_sorted_data(), index = seq_along(bp_sorted_data()))
    ggplot(df, aes(x = value, y = 0)) +
      geom_point(size = 4, alpha = 0.5, color = "#95a5a6") +
      geom_vline(xintercept = bp_q2(), color = "#e74c3c", linewidth = 2, linetype = "solid") +
      annotate("text", x = bp_q2(), y = 0.4, label = paste0("Mediana\n", round(bp_q2(), 1)),
               color = "#e74c3c", size = 5, fontface = "bold") +
      labs(x = x_label, y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(x_min, x_max) + ylim(-0.5, 0.5)

  } else if (step == 4) {
    df <- data.frame(value = bp_sorted_data(), index = seq_along(bp_sorted_data()))
    ggplot(df, aes(x = value, y = 0)) +
      geom_point(size = 4, alpha = 0.5, color = "#95a5a6") +
      geom_vline(xintercept = bp_q1(), color = "#3498db", linewidth = 1.5, linetype = "dashed") +
      geom_vline(xintercept = bp_q2(), color = "#e74c3c", linewidth = 2, linetype = "solid") +
      geom_vline(xintercept = bp_q3(), color = "#3498db", linewidth = 1.5, linetype = "dashed") +
      annotate("text", x = bp_q1(), y = 0.4, label = paste0("Q1\n", round(bp_q1(), 1)),
               color = "#3498db", size = 4, fontface = "bold") +
      annotate("text", x = bp_q2(), y = -0.4, label = paste0("Q2\n", round(bp_q2(), 1)),
               color = "#e74c3c", size = 4, fontface = "bold") +
      annotate("text", x = bp_q3(), y = 0.4, label = paste0("Q3\n", round(bp_q3(), 1)),
               color = "#3498db", size = 4, fontface = "bold") +
      labs(x = x_label, y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(x_min, x_max) + ylim(-0.5, 0.5)

  } else if (step == 5) {
    df <- data.frame(value = bp_sorted_data(), index = seq_along(bp_sorted_data()))
    ggplot(df, aes(x = value, y = 0)) +
      annotate("rect", xmin = bp_q1(), xmax = bp_q3(), ymin = -0.2, ymax = 0.2,
               fill = "#3498db", alpha = 0.3) +
      geom_point(size = 4, alpha = 0.5, color = "#95a5a6") +
      geom_vline(xintercept = bp_q2(), color = "#e74c3c", linewidth = 2) +
      annotate("text", x = (bp_q1() + bp_q3()) / 2, y = 0.35,
               label = paste0("IQR = ", round(bp_iqr(), 1)),
               color = "#2c3e50", size = 5, fontface = "bold") +
      labs(x = x_label, y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(x_min, x_max) + ylim(-0.5, 0.5)

  } else if (step == 6) {
    df <- data.frame(value = bp_sorted_data(), index = seq_along(bp_sorted_data()))
    ggplot(df, aes(x = value, y = 0)) +
      geom_segment(aes(x = bp_lower_whisker(), xend = bp_q1(), y = 0, yend = 0),
                   linewidth = 1.5, color = "#2c3e50") +
      geom_segment(aes(x = bp_q3(), xend = bp_upper_whisker(), y = 0, yend = 0),
                   linewidth = 1.5, color = "#2c3e50") +
      annotate("rect", xmin = bp_q1(), xmax = bp_q3(), ymin = -0.2, ymax = 0.2,
               fill = "#3498db", alpha = 0.3, color = "#2c3e50", linewidth = 1) +
      geom_vline(xintercept = bp_q2(), color = "#e74c3c", linewidth = 2) +
      geom_point(size = 4, alpha = 0.5, color = "#95a5a6") +
      labs(x = x_label, y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(x_min, x_max) + ylim(-0.5, 0.5)

  } else if (step == 7) {
    df <- data.frame(value = bp_sorted_data(), index = seq_along(bp_sorted_data()))
    df$is_outlier <- df$value < bp_lower_whisker() | df$value > bp_upper_whisker()

    ggplot(df, aes(x = value, y = 0)) +
      geom_segment(aes(x = bp_lower_whisker(), xend = bp_q1(), y = 0, yend = 0),
                   linewidth = 1.5, color = "#2c3e50") +
      geom_segment(aes(x = bp_q3(), xend = bp_upper_whisker(), y = 0, yend = 0),
                   linewidth = 1.5, color = "#2c3e50") +
      annotate("rect", xmin = bp_q1(), xmax = bp_q3(), ymin = -0.2, ymax = 0.2,
               fill = "#3498db", alpha = 0.3, color = "#2c3e50", linewidth = 1) +
      geom_vline(xintercept = bp_q2(), color = "#e74c3c", linewidth = 2) +
      geom_point(aes(color = is_outlier), size = 4, alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#95a5a6", "TRUE" = "#e74c3c"), guide = "none") +
      labs(x = x_label, y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(x_min, x_max) + ylim(-0.5, 0.5)

  } else if (step == 8) {
    df <- data.frame(value = bp_data(), group = "Dane")
    p <- ggplot(df, aes(x = value, y = group)) +
      geom_boxplot(fill = "#3498db", alpha = 0.5, color = "#2c3e50", linewidth = 1,
                   outlier.shape = NA) +
      labs(x = x_label, y = "") +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      xlim(x_min, x_max)

    if (length(bp_outliers()) > 0) {
      df_outliers <- data.frame(value = bp_outliers(), group = "Dane")
      p <- p + geom_jitter(data = df_outliers, aes(x = value, y = group),
                           height = 0.1, size = 4, color = "#e74c3c", alpha = 0.7)
    }
    p
  }
})

output$bp_histogram <- renderPlot({
  scenario <- bp_scenarios[[input$bp_scenario]]
  df <- data.frame(value = bp_data())

  ggplot(df, aes(x = value)) +
    geom_histogram(bins = 15, fill = "#95a5a6", color = "white", alpha = 0.7) +
    labs(x = scenario$x_label, y = "Liczba obserwacji") +
    theme_minimal(base_size = 14) +
    xlim(scenario$x_min, scenario$x_max)
})

# ============================================================================
# Tab 4.4: Porownanie miar rozrzutu
# ============================================================================

s4_comp_base_data <- reactiveVal(NULL)
s4_comp_outliers <- reactiveVal(numeric(0))

observeEvent(input$s4_comp_var, {
  req(student_data)
  vals <- student_data[[input$s4_comp_var]]
  s4_comp_base_data(vals)
  s4_comp_outliers(numeric(0))

  # Update outlier slider range based on variable
  var_min <- floor(min(vals, na.rm = TRUE))
  var_max <- ceiling(max(vals, na.rm = TRUE))
  range_span <- var_max - var_min
  updateSliderInput(session, "s4_comp_outlier",
                    min = round(var_min - range_span),
                    max = round(var_max + range_span * 2),
                    value = round(var_max + range_span * 0.5))
}, ignoreNULL = FALSE)

observeEvent(input$s4_comp_add, {
  current <- s4_comp_outliers()
  s4_comp_outliers(c(current, input$s4_comp_outlier))
})

observeEvent(input$s4_comp_reset, {
  s4_comp_outliers(numeric(0))
})

s4_comp_all <- reactive({
  req(s4_comp_base_data())
  c(s4_comp_base_data(), s4_comp_outliers())
})

output$s4_comp_plot <- renderPlot({
  req(s4_comp_all())
  all_vals <- s4_comp_all()
  base_vals <- s4_comp_base_data()
  outlier_vals <- s4_comp_outliers()

  col_nominal <- "#e74c3c"
  col_discrete <- "#3498db"
  col_continuous <- "#27ae60"
  col_dark <- "#2c3e50"

  mu <- mean(all_vals)
  s <- sd(all_vals)
  q1 <- quantile(all_vals, 0.25)
  q3 <- quantile(all_vals, 0.75)
  iqr_val <- q3 - q1

  var_labels <- c(wzrost = "Wzrost (cm)", waga = "Waga (kg)",
                  czas_dojazdu = "Czas dojazdu (min)", srednia_ocen = "Srednia ocen")
  x_lab <- var_labels[input$s4_comp_var]

  df <- data.frame(value = all_vals)

  p <- ggplot(df, aes(x = value)) +
    geom_histogram(bins = 25, fill = "#95a5a6", color = "white", alpha = 0.7)

  # SD bands
  p <- p +
    annotate("rect", xmin = mu - s, xmax = mu + s,
             ymin = -Inf, ymax = Inf, alpha = 0.15, fill = col_discrete) +
    geom_vline(xintercept = mu, color = col_nominal, linewidth = 1.2, linetype = "solid") +
    geom_vline(xintercept = mu - s, color = col_discrete, linewidth = 0.8, linetype = "dashed") +
    geom_vline(xintercept = mu + s, color = col_discrete, linewidth = 0.8, linetype = "dashed")

  # IQR bracket annotations at top
  y_max <- max(ggplot_build(ggplot(df, aes(x = value)) + geom_histogram(bins = 25))$data[[1]]$count) * 1.05

  p <- p +
    annotate("segment", x = q1, xend = q3, y = y_max * 0.95, yend = y_max * 0.95,
             color = col_continuous, linewidth = 1.5) +
    annotate("segment", x = q1, xend = q1, y = y_max * 0.92, yend = y_max * 0.98,
             color = col_continuous, linewidth = 1.5) +
    annotate("segment", x = q3, xend = q3, y = y_max * 0.92, yend = y_max * 0.98,
             color = col_continuous, linewidth = 1.5) +
    annotate("text", x = (q1 + q3) / 2, y = y_max * 1.02,
             label = paste0("IQR = ", round(iqr_val, 2)),
             color = col_continuous, size = 4, fontface = "bold")

  # Range bracket
  p <- p +
    annotate("segment", x = min(all_vals), xend = max(all_vals),
             y = y_max * 0.82, yend = y_max * 0.82,
             color = col_dark, linewidth = 1, linetype = "dotted") +
    annotate("text", x = (min(all_vals) + max(all_vals)) / 2, y = y_max * 0.87,
             label = paste0("Rozstep = ", round(max(all_vals) - min(all_vals), 2)),
             color = col_dark, size = 3.5, fontface = "bold")

  # Mark outliers if any
  if (length(outlier_vals) > 0) {
    df_out <- data.frame(value = outlier_vals)
    p <- p + geom_rug(data = df_out, aes(x = value), color = col_nominal, linewidth = 1.2, alpha = 0.8)
  }

  # Legend annotations
  p <- p +
    annotate("text", x = mu, y = y_max * 0.72,
             label = paste0("Srednia = ", round(mu, 2)),
             color = col_nominal, size = 3.5, hjust = -0.1, fontface = "bold") +
    annotate("text", x = mu + s, y = y_max * 0.65,
             label = paste0("SD = ", round(s, 2)),
             color = col_discrete, size = 3.5, hjust = -0.1, fontface = "bold")

  p + labs(title = paste0("Rozklad zmiennej: ", x_lab),
           subtitle = if (length(outlier_vals) > 0) paste0("Dodano ", length(outlier_vals), " outlier(ow)") else NULL,
           x = x_lab, y = "Liczba obserwacji") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 16, face = "bold"))
})

output$s4_comp_table <- renderTable({
  req(s4_comp_all(), s4_comp_base_data())
  base_vals <- s4_comp_base_data()
  all_vals <- s4_comp_all()
  has_outliers <- length(s4_comp_outliers()) > 0

  base_stats <- data.frame(
    Miara = c("Rozstep (max - min)", "IQR (Q3 - Q1)", "Wariancja", "Odch. standardowe"),
    Bazowe = c(
      round(max(base_vals, na.rm = TRUE) - min(base_vals, na.rm = TRUE), 2),
      round(IQR(base_vals, na.rm = TRUE), 2),
      round(var(base_vals, na.rm = TRUE), 2),
      round(sd(base_vals, na.rm = TRUE), 2)
    )
  )

  if (has_outliers) {
    base_stats$Aktualne <- c(
      round(max(all_vals) - min(all_vals), 2),
      round(IQR(all_vals), 2),
      round(var(all_vals), 2),
      round(sd(all_vals), 2)
    )
    base_stats$Zmiana <- paste0(
      round((base_stats$Aktualne - base_stats$Bazowe) / base_stats$Bazowe * 100, 1), "%"
    )
  }

  if (input$s4_comp_show_cv) {
    cv_row <- data.frame(
      Miara = "Wsp. zmiennosci (CV)",
      Bazowe = round(sd(base_vals, na.rm = TRUE) / mean(base_vals, na.rm = TRUE) * 100, 2)
    )
    if (has_outliers) {
      cv_row$Aktualne <- round(sd(all_vals) / mean(all_vals) * 100, 2)
      cv_row$Zmiana <- paste0(round((cv_row$Aktualne - cv_row$Bazowe) / cv_row$Bazowe * 100, 1), "%")
    }
    base_stats <- rbind(base_stats, cv_row)
  }

  base_stats
}, striped = TRUE, hover = TRUE, bordered = TRUE)

output$s4_comp_cv_explanation <- renderUI({
  tags$div(
    tags$p("Odchylenie standardowe (SD) zalezy od jednostki mierzonej zmiennej — nie mozna wiec porownywac SD wzrostu (w cm) z SD wagi (w kg)."),
    tags$p("Wspolczynnik zmiennosci (CV) to SD podzielone przez srednia, wyrazone w procentach:"),
    tags$p(tags$b("CV = (SD / srednia) * 100%"), style = "text-align: center; font-size: 16px;"),
    tags$p("Dzieki temu mozna porownywac rozproszenie zmiennych o roznych skalach i jednostkach."),
    tags$p("Przyklad: CV = 5% oznacza, ze dane rozpraszaja sie o ok. 5% wartosci sredniej.")
  )
})

output$s4_comp_cv_plot <- renderPlot({
  req(student_data)

  # Compare wzrost and waga
  wzrost_vals <- student_data$wzrost
  waga_vals <- student_data$waga

  sd_wzrost <- sd(wzrost_vals, na.rm = TRUE)
  sd_waga <- sd(waga_vals, na.rm = TRUE)
  cv_wzrost <- sd_wzrost / mean(wzrost_vals, na.rm = TRUE) * 100
  cv_waga <- sd_waga / mean(waga_vals, na.rm = TRUE) * 100

  col_discrete <- "#3498db"
  col_continuous <- "#27ae60"

  df_sd <- data.frame(
    Zmienna = c("Wzrost", "Waga"),
    Wartosc = c(sd_wzrost, sd_waga),
    Miara = "SD (jednostki oryginalne)"
  )

  df_cv <- data.frame(
    Zmienna = c("Wzrost", "Waga"),
    Wartosc = c(cv_wzrost, cv_waga),
    Miara = "CV (%)"
  )

  df_all <- rbind(df_sd, df_cv)
  df_all$Miara <- factor(df_all$Miara, levels = c("SD (jednostki oryginalne)", "CV (%)"))

  ggplot(df_all, aes(x = Zmienna, y = Wartosc, fill = Zmienna)) +
    geom_col(width = 0.6, alpha = 0.8) +
    geom_text(aes(label = round(Wartosc, 2)), vjust = -0.5, size = 5, fontface = "bold") +
    facet_wrap(~ Miara, scales = "free_y") +
    scale_fill_manual(values = c("Wzrost" = col_discrete, "Waga" = col_continuous)) +
    labs(title = "Porownanie SD vs CV: wzrost (cm) vs waga (kg)",
         subtitle = "SD zalezy od skali, CV jest bezwymiarowy",
         x = "", y = "") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 16, face = "bold"),
          legend.position = "none",
          strip.text = element_text(size = 13, face = "bold"))
})

# END SECTION 4
# SECTION 5: Ksztalt rozkladu

# ==========================================================================
# Tab 5.1: Skosnosc
# ==========================================================================

# Store generated data so regen button works
s5_skew_gen_data <- reactiveVal(NULL)

# Generate new data when distribution type, parameter, or regen button changes
observeEvent(list(input$s5_skew_dist, input$s5_skew_param, input$s5_skew_regen), {
  req(input$s5_skew_source == "generated")
  param <- input$s5_skew_param
  dist <- input$s5_skew_dist

  set.seed(NULL)
  vals <- switch(dist,
    "normal"      = rnorm(500, 50, 10),
    "gamma"       = rgamma(500, shape = param, scale = 10),
    "left_skew"   = {
      raw <- rgamma(500, shape = param, scale = 10)
      max(raw) - raw + min(raw) + 5
    },
    "exponential" = rexp(500, rate = 1 / param),
    rnorm(500, 50, 10)
  )
  s5_skew_gen_data(vals)
}, ignoreNULL = FALSE)

s5_skew_data <- reactive({
  if (input$s5_skew_source == "survey") {
    as.numeric(student_data[[input$s5_skew_var]])
  } else {
    req(s5_skew_gen_data())
    s5_skew_gen_data()
  }
})

output$s5_skew_plot <- renderPlot({
  x <- s5_skew_data()
  req(length(x) > 1)

  df <- data.frame(x = x)
  m <- mean(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)

  ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30, fill = col_continuous, color = "white", alpha = 0.6) +
    geom_density(color = col_dark, linewidth = 1.2) +
    geom_vline(aes(xintercept = m, color = "Srednia"),
               linewidth = 1.1, linetype = "solid") +
    geom_vline(aes(xintercept = med, color = "Mediana"),
               linewidth = 1.1, linetype = "dashed") +
    scale_color_manual(name = "",
                       values = c("Srednia" = col_nominal, "Mediana" = col_discrete),
                       guide = guide_legend(override.aes = list(
                         linetype = c("dashed", "solid")))) +
    annotate("text", x = m, y = Inf, label = paste0("Srednia = ", round(m, 2)),
             vjust = 2, hjust = -0.1, color = col_nominal, fontface = "bold", size = 4.5) +
    annotate("text", x = med, y = Inf, label = paste0("Mediana = ", round(med, 2)),
             vjust = 3.5, hjust = -0.1, color = col_discrete, fontface = "bold", size = 4.5) +
    labs(title = "Histogram z gestoscia",
         subtitle = paste0("Skosnosc = ", round(e1071::skewness(x, na.rm = TRUE), 3)),
         x = "Wartosc", y = "Gestosc") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
})

output$s5_skew_interpretation <- renderUI({
  x <- s5_skew_data()
  req(length(x) > 1)

  sk <- e1071::skewness(x, na.rm = TRUE)

  if (sk > 0.5) {
    label <- "prawoskosny"
    badge_col <- col_nominal
  } else if (sk < -0.5) {
    label <- "lewoskosny"
    badge_col <- col_discrete
  } else {
    label <- "w przyblizeniu symetryczny"
    badge_col <- col_continuous
  }

  tags$div(
    tags$p(tags$strong("Skosnosc: "), round(sk, 3)),
    tags$span(class = "type-badge",
              style = paste0("background-color:", badge_col, ";font-size:14px;"),
              paste0("Rozklad ", label))
  )
})

output$s5_skew_rule <- renderUI({
  x <- s5_skew_data()
  req(length(x) > 1)

  m <- mean(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  diff_val <- m - med

  if (diff_val > 0.01 * sd(x, na.rm = TRUE)) {
    arrow <- "Srednia > Mediana -> rozklad prawoskosny (ogon w prawo)"
    icon_col <- col_nominal
  } else if (diff_val < -0.01 * sd(x, na.rm = TRUE)) {
    arrow <- "Srednia < Mediana -> rozklad lewoskosny (ogon w lewo)"
    icon_col <- col_discrete
  } else {
    arrow <- "Srednia ~ Mediana -> rozklad symetryczny"
    icon_col <- col_continuous
  }

  tags$div(
    tags$p(tags$strong("Srednia: "), round(m, 3)),
    tags$p(tags$strong("Mediana: "), round(med, 3)),
    tags$p(tags$strong("Roznica (srednia - mediana): "), round(diff_val, 3)),
    tags$p(style = paste0("color:", icon_col, ";font-weight:bold;font-size:15px;"), arrow)
  )
})

# ==========================================================================
# Tab 5.2: Kurtoza
# ==========================================================================

output$s5_kurt_plot <- renderPlot({
  df_val <- input$s5_kurt_df
  x_seq <- seq(-5, 5, length.out = 500)

  plot_df <- data.frame(x = x_seq, y = dt(x_seq, df = df_val),
                        Rozklad = paste0("t (df=", df_val, ")"))

  if (input$s5_kurt_show_normal) {
    norm_df <- data.frame(x = x_seq, y = dnorm(x_seq),
                          Rozklad = "Normalny")
    plot_df <- rbind(plot_df, norm_df)
  }

  if (input$s5_kurt_show_uniform) {
    # Uniform on [-sqrt(3), sqrt(3)] has variance 1 like standard normal
    a <- sqrt(3)
    unif_df <- data.frame(x = x_seq, y = dunif(x_seq, min = -a, max = a),
                          Rozklad = "Jednostajny")
    plot_df <- rbind(plot_df, unif_df)
  }

  color_map <- c(col_nominal, col_discrete, col_continuous)
  names(color_map) <- c(paste0("t (df=", df_val, ")"), "Normalny", "Jednostajny")

  ggplot(plot_df, aes(x = x, y = y, color = Rozklad)) +
    geom_line(linewidth = 1.3) +
    scale_color_manual(values = color_map) +
    labs(title = "Porownanie rozkladow - ksztalt i ogony",
         subtitle = paste0("Rozklad t z ", df_val, " stopniami swobody"),
         x = "Wartosc", y = "Gestosc") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
})

output$s5_kurt_tails_plot <- renderPlot({
  df_val <- input$s5_kurt_df
  x_left <- seq(-5, -2.5, length.out = 200)
  x_right <- seq(2.5, 5, length.out = 200)
  x_seq <- c(x_left, x_right)

  plot_df <- data.frame(x = x_seq, y = dt(x_seq, df = df_val),
                        Rozklad = paste0("t (df=", df_val, ")"))

  if (input$s5_kurt_show_normal) {
    norm_df <- data.frame(x = x_seq, y = dnorm(x_seq),
                          Rozklad = "Normalny")
    plot_df <- rbind(plot_df, norm_df)
  }

  if (input$s5_kurt_show_uniform) {
    a <- sqrt(3)
    unif_df <- data.frame(x = x_seq, y = dunif(x_seq, min = -a, max = a),
                          Rozklad = "Jednostajny")
    plot_df <- rbind(plot_df, unif_df)
  }

  color_map <- c(col_nominal, col_discrete, col_continuous)
  names(color_map) <- c(paste0("t (df=", df_val, ")"), "Normalny", "Jednostajny")

  ggplot(plot_df, aes(x = x, y = y, color = Rozklad)) +
    geom_line(linewidth = 1.3) +
    scale_color_manual(values = color_map) +
    labs(title = "Powiekszenie ogonow rozkladu",
         x = "Wartosc", y = "Gestosc") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
})

output$s5_kurt_interpretation <- renderUI({
  df_val <- input$s5_kurt_df

  if (df_val <= 4) {
    kurt_val <- Inf
    kurt_text <- "Inf (nieskonczona)"
  } else {
    kurt_val <- 6 / (df_val - 4)
    kurt_text <- round(kurt_val, 3)
  }

  if (is.infinite(kurt_val) || kurt_val > 1) {
    label <- "leptokurtyczny (kurtoza > 0)"
    badge_col <- col_nominal
    meaning <- "Wiecej wartosci ekstremalnych niz rozklad normalny"
  } else if (kurt_val > -0.5) {
    label <- "mezokurtyczny (kurtoza ~ 0)"
    badge_col <- col_discrete
    meaning <- "Ogony podobne do rozkladu normalnego"
  } else {
    label <- "platykurtyczny (kurtoza < 0)"
    badge_col <- col_continuous
    meaning <- "Mniej wartosci ekstremalnych niz rozklad normalny"
  }

  tags$div(
    tags$p(tags$strong("Nadmiarowa kurtoza rozkladu t: "), kurt_text),
    tags$span(class = "type-badge",
              style = paste0("background-color:", badge_col, ";font-size:13px;"),
              label),
    tags$p(style = "margin-top:10px;", tags$em(meaning))
  )
})

output$s5_kurt_explanation <- renderUI({
  tags$div(
    tags$h4("Czym jest kurtoza?"),
    tags$p(tags$strong("Kurtoza mowi o 'grubosci ogonow' rozkladu"),
           " - czyli o prawdopodobienstwie wystapienia wartosci ekstremalnych."),
    tags$ul(
      tags$li(tags$span(style = paste0("color:", col_nominal, ";font-weight:bold;"),
                        "Leptokurtyczny (kurtoza > 0):"),
              " ciezsze ogony, wieksze ryzyko wartosci ekstremalnych."),
      tags$li(tags$span(style = paste0("color:", col_discrete, ";font-weight:bold;"),
                        "Mezokurtyczny (kurtoza ~ 0):"),
              " podobne ogony do rozkladu normalnego."),
      tags$li(tags$span(style = paste0("color:", col_continuous, ";font-weight:bold;"),
                        "Platykurtyczny (kurtoza < 0):"),
              " lzejsze ogony, mniej ekstremalnych wartosci.")
    ),
    tags$p("Rozklad normalny ma nadmiarowa kurtoze rowna 0 (punkt odniesienia)."),
    tags$p("Rozklad t-Studenta z malymi stopniami swobody ma ciezkie ogony",
           " (wysoka kurtoza) - stad wieksze wartosci krytyczne niz dla rozkladu normalnego.")
  )
})

# ==========================================================================
# Tab 5.3: Pelny obraz rozkladu (capstone)
# ==========================================================================

s5_full_data <- reactive({
  as.numeric(student_data[[input$s5_full_var]])
})

output$s5_full_stats_table <- renderTable({
  x <- s5_full_data()
  req(length(x) > 1)

  # Mode: most frequent value rounded to 1 decimal
  x_rounded <- round(x, 1)
  freq_table <- table(x_rounded)
  mode_val <- as.numeric(names(freq_table)[which.max(freq_table)])

  sk <- e1071::skewness(x, na.rm = TRUE)
  ku <- e1071::kurtosis(x, na.rm = TRUE)
  cv <- sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE)) * 100

  stats_df <- data.frame(
    Statystyka = c("n", "Srednia", "Mediana", "Moda",
                   "Odch. std.", "Wariancja", "Rozstep", "IQR",
                   "Q1", "Q3", "Min", "Max",
                   "Skosnosc", "Kurtoza", "CV (%)"),
    Wartosc = c(
      length(x),
      round(mean(x, na.rm = TRUE), 2),
      round(median(x, na.rm = TRUE), 2),
      round(mode_val, 2),
      round(sd(x, na.rm = TRUE), 2),
      round(var(x, na.rm = TRUE), 2),
      round(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 2),
      round(IQR(x, na.rm = TRUE), 2),
      round(quantile(x, 0.25, na.rm = TRUE), 2),
      round(quantile(x, 0.75, na.rm = TRUE), 2),
      round(min(x, na.rm = TRUE), 2),
      round(max(x, na.rm = TRUE), 2),
      round(sk, 3),
      round(ku, 3),
      round(cv, 1)
    ),
    stringsAsFactors = FALSE
  )
  stats_df
}, striped = TRUE, bordered = TRUE, hover = TRUE, align = "lr")

output$s5_full_hist_plot <- renderPlot({
  x <- s5_full_data()
  req(length(x) > 1)

  df <- data.frame(x = x)
  m <- mean(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)

  var_label <- names(which(c("wzrost" = "wzrost", "waga" = "waga",
                              "czas_dojazdu" = "czas_dojazdu",
                              "srednia_ocen" = "srednia_ocen",
                              "liczba_kursow" = "liczba_kursow",
                              "liczba_nieobecnosci" = "liczba_nieobecnosci") == input$s5_full_var))

  ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 25, fill = col_continuous, color = "white", alpha = 0.6) +
    geom_density(color = col_dark, linewidth = 1.2) +
    geom_rug(color = col_dark, alpha = 0.3) +
    geom_vline(xintercept = m, color = col_nominal, linewidth = 1.1, linetype = "solid") +
    geom_vline(xintercept = med, color = col_discrete, linewidth = 1.1, linetype = "dashed") +
    annotate("text", x = m, y = Inf, label = paste0("Srednia = ", round(m, 2)),
             vjust = 2, hjust = -0.1, color = col_nominal, fontface = "bold", size = 4.5) +
    annotate("text", x = med, y = Inf, label = paste0("Mediana = ", round(med, 2)),
             vjust = 3.5, hjust = -0.1, color = col_discrete, fontface = "bold", size = 4.5) +
    labs(title = paste0("Rozklad zmiennej: ", input$s5_full_var),
         x = input$s5_full_var, y = "Gestosc") +
    theme_minimal(base_size = 14)
})

output$s5_full_box_plot <- renderPlot({
  x <- s5_full_data()
  req(length(x) > 1)

  df <- data.frame(x = x)

  ggplot(df, aes(y = x)) +
    geom_boxplot(fill = col_continuous, alpha = 0.5, color = col_dark,
                 width = 0.4, outlier.color = col_nominal, outlier.size = 2.5) +
    coord_flip() +
    labs(x = "", y = input$s5_full_var) +
    theme_minimal(base_size = 14) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(0, 10, 5, 10))
})

output$s5_full_interpretation <- renderUI({
  x <- s5_full_data()
  req(length(x) > 1)

  m <- mean(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  sk <- e1071::skewness(x, na.rm = TRUE)
  ku <- e1071::kurtosis(x, na.rm = TRUE)
  cv <- sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE)) * 100
  diff_pct <- abs(m - med) / sd(x, na.rm = TRUE) * 100

  items <- list()

  # Skewness interpretation
  if (sk > 0.5) {
    items <- c(items, list(
      tags$li(tags$span(style = paste0("color:", col_nominal, ";font-weight:bold;"),
                        "Rozklad jest prawoskosny"),
              " (skosnosc = ", round(sk, 2), ") - mediana lepiej opisuje typowa wartosc niz srednia.")
    ))
  } else if (sk < -0.5) {
    items <- c(items, list(
      tags$li(tags$span(style = paste0("color:", col_discrete, ";font-weight:bold;"),
                        "Rozklad jest lewoskosny"),
              " (skosnosc = ", round(sk, 2), ") - srednia jest zanizona wzgledem typowej wartosci.")
    ))
  } else {
    items <- c(items, list(
      tags$li(tags$span(style = paste0("color:", col_continuous, ";font-weight:bold;"),
                        "Rozklad jest w przyblizeniu symetryczny"),
              " (skosnosc = ", round(sk, 2), ") - srednia i mediana sa zblizone.")
    ))
  }

  # Kurtosis interpretation
  if (ku > 1) {
    items <- c(items, list(
      tags$li(tags$span(style = paste0("color:", col_nominal, ";font-weight:bold;"),
                        "Rozklad ma ciezkie ogony"),
              " (kurtoza = ", round(ku, 2), ") - uwaga na wartosci ekstremalne.")
    ))
  } else if (ku < -1) {
    items <- c(items, list(
      tags$li("Rozklad ma lekkie ogony (kurtoza = ", round(ku, 2),
              ") - mniej ekstremalnych wartosci niz rozklad normalny.")
    ))
  }

  # CV interpretation
  if (cv > 30) {
    items <- c(items, list(
      tags$li(tags$span(style = paste0("color:", col_ordinal, ";font-weight:bold;"),
                        "Duza zmiennosc danych"),
              " (CV = ", round(cv, 1), "%).")
    ))
  } else if (cv > 15) {
    items <- c(items, list(
      tags$li("Umiarkowana zmiennosc danych (CV = ", round(cv, 1), "%).")
    ))
  } else {
    items <- c(items, list(
      tags$li("Mala zmiennosc danych (CV = ", round(cv, 1), "%).")
    ))
  }

  # Mean vs median
  items <- c(items, list(
    tags$li("Roznica srednia - mediana: ", round(m - med, 2),
            if (diff_pct > 10) tags$span(style = paste0("color:", col_nominal, ";"),
                                          " (istotna roznica - rozklad wyraznie asymetryczny)")
            else " (niewielka roznica)")
  ))

  tags$div(
    tags$ul(items)
  )
})

# END SECTION 5

}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
