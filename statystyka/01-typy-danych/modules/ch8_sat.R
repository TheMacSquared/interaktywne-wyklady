# Wariant kierunkowy: Inżynieria danych satelitarnych i kosmicznych.

.ch8_sat_data <- read.csv(
  file.path(project_root, "dane", "satelitarne_obserwacje.csv"),
  stringsAsFactors = FALSE
)

.ch8_sat_panel <- function(id, title, ...) {
  figure_panel(
    label = "Ćwiczenie",
    h4(title),
    tagList(...),
    actionButton(paste0("ch8_ans", id), "Pokaż rozwiązanie",
                 class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput(paste0("ch8_sol", id))
  )
}

.ch8_content_sat <- function() tagList(
  lc_feedback(type = "info",
    p("Otwórz plik ", tags$code("../dane/satelitarne_obserwacje.csv"),
      " w Jamovi. To syntetyczna tabela dydaktyczna: jeden wiersz oznacza
       lokalizację obserwowaną jednego dnia, a nie pojedynczy surowy obraz."),
    p("NDVI traktujemy na tym etapie tylko jako liczbowy wskaźnik roślinności:
       wyższa wartość oznacza zwykle więcej aktywnej roślinności.")
  ),

  lc_h2("ch8-sat-blok1", "Blok 1: Od sensora do zmiennej (20 min)"),

  figure_panel(label = "Ćwiczenie",
    h4("Zadanie 1 — Jakiego typu są dane z obserwacji?"),
    p("Dla każdej zmiennej określ typ i sensowny wykres:"),
    uiOutput("ch8_table1"),
    actionButton("ch8_ans1", "Pokaż rozwiązanie",
                 class = "lc-btn-ok-outline lc-btn-sm")
  ),

  .ch8_sat_panel("2", "Zadanie 2 — Liczba nie zawsze oznacza ilość",
    tags$ol(
      tags$li("Czy współrzędne geograficzne są zwykłymi zmiennymi ilościowymi,
               które warto uśredniać bez kontekstu?"),
      tags$li("Czy kody P001, P002, ... tworzą skalę porządkową?"),
      tags$li("Dlaczego ", tags$code("jakosc_pomiaru"),
              " ma naturalny porządek, ale odstępy między kategoriami nie są liczbami?"),
      tags$li("Co tracimy, zamieniając dokładne zachmurzenie na kategorie
               dobra/ograniczona/odrzucona?")
    )
  ),

  lc_h2("ch8-sat-blok2", "Blok 2: Opis zmienności pomiarów (30 min)"),

  .ch8_sat_panel("3", "Zadanie 3 — Temperatura mierzona przez sensor",
    p("Dla ", tags$code("sat_temp_c"), " oblicz średnią, medianę, SD, IQR,
       minimum i maksimum. Narysuj histogram i boxplot."),
    tags$ol(
      tags$li("Czy średnia i mediana są podobne?"),
      tags$li("Czy widzisz obserwacje odstające?"),
      tags$li("Dlaczego sama średnia nie wystarcza do opisu pomiarów?")
    )
  ),

  .ch8_sat_panel("4", "Zadanie 4 — Te same dane, różne powierzchnie",
    p("Zbuduj boxplot ", tags$code("sat_temp_c ~ typ_pokrycia"),
      " oraz histogram NDVI rozdzielony według ", tags$code("strefa"), "."),
    p("Wskaż grupę o najwyższej medianie temperatury i grupę o najwyższym
       typowym NDVI. Nie formułuj jeszcze wniosku przyczynowego.")
  ),

  lc_h2("ch8-sat-blok3", "Blok 3: Pomiar i jakość danych (30 min)"),

  .ch8_sat_panel("5", "Zadanie 5 — Precyzja i obciążenie sensora",
    p("Porównaj ", tags$code("sat_temp_c"), " i ", tags$code("grunt_temp_c"),
      ". Następnie opisz zmienną ", tags$code("roznica_temp_c"), "."),
    tags$ol(
      tags$li("Czy różnice skupiają się blisko zera?"),
      tags$li("Czy sensor ma mały rozrzut różnic?"),
      tags$li("Czy dodatnia średnia różnica sugeruje błąd losowy, czy systematyczny?")
    )
  ),

  .ch8_sat_panel("6", "Zadanie 6 — Dużo pikseli ≠ dużo niezależnej informacji",
    p("Wyobraź sobie, że 180 lokalizacji tworzy regularną siatkę. Odpowiedz:"),
    tags$ol(
      tags$li("Czy sąsiednie lokalizacje mogą mieć podobną temperaturę i NDVI?"),
      tags$li("Czy 180 sąsiednich pikseli oznacza 180 całkowicie niezależnych pomiarów?"),
      tags$li("Jak zachmurzenie może sprawić, że braki pomiarów nie są przypadkowe?"),
      tags$li("Jak ostrożnie opisał(a)byś populację, do której odnoszą się wyniki?")
    )
  ),

  lc_h2("ch8-sat-podsumowanie", "Podsumowanie"),
  lc_feedback(type = "warning",
    p("Na tym etapie nie modelujemy zależności przestrzennej. Wystarczy zapamiętać:
       jednostką obserwacji jest lokalizacja i termin, a bliskie lokalizacje
       mogą nie dostarczać całkowicie niezależnej informacji.")
  ),
  actionButton("ch8_ans_summary", "Pokaż odpowiedzi",
               class = "lc-btn-ok-outline lc-btn-sm"),
  uiOutput("ch8_sol_summary")
)

.ch8_sat_solutions <- local({
  d <- .ch8_sat_data
  temp_mean <- mean(d$sat_temp_c)
  temp_med <- median(d$sat_temp_c)
  temp_sd <- sd(d$sat_temp_c)
  diff_mean <- mean(d$roznica_temp_c)
  diff_sd <- sd(d$roznica_temp_c)

  list(
    sol1 = NULL,
    sol2 = tagList(
      p("Współrzędne są liczbami, ale ich sens zależy od położenia i układu odniesienia.
         ID jest wyłącznie etykietą. Jakość ma porządek, lecz nie ma równych odstępów.
         Kategoryzacja zachmurzenia upraszcza decyzję, ale usuwa dokładną informację.")
    ),
    sol3 = tagList(
      p(sprintf("Dla sat_temp_c: średnia ≈ %.2f°C, mediana ≈ %.2f°C, SD ≈ %.2f°C.",
                temp_mean, temp_med, temp_sd)),
      p("Średnia opisuje położenie, a SD/IQR i wykres pokazują zmienność oraz
         możliwe wartości nietypowe.")
    ),
    sol4 = tagList(
      p("Najwyższych temperatur oczekujemy dla zwartej zabudowy, a najwyższego
         NDVI dla lasu. Jest to opis związku w danych, nie samodzielny dowód przyczynowy.")
    ),
    sol5 = tagList(
      p(sprintf("Średnia różnica satelita − grunt ≈ %.2f°C, SD różnic ≈ %.2f°C.",
                diff_mean, diff_sd)),
      p("Dodatnia średnia różnica wskazuje na systematyczne zawyżanie (obciążenie),
         a SD różnic opisuje rozrzut błędu, czyli precyzję porównań.")
    ),
    sol6 = tagList(
      p("Sąsiednie piksele zwykle są podobne, dlatego liczba wierszy może
         przeceniać liczbę niezależnych informacji. Zachmurzenie zależy od miejsca
         i czasu, więc niedostępne obserwacje mogą tworzyć systematyczny wzorzec.")
    ),
    sol_summary = tagList(
      p("Najważniejsze pytania: co jest jednostką obserwacji, co zmierzył sensor,
         skąd bierze się zmienność i czy wiersze można traktować jako niezależne?")
    )
  )
})
