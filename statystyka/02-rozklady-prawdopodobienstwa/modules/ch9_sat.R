# Wariant kierunkowy: Inżynieria danych satelitarnych i kosmicznych.

.ch9_sat_data <- read.csv(
  file.path(project_root, "dane", "satelitarne_obserwacje.csv"),
  stringsAsFactors = FALSE
)

.ch9_sat_panel <- function(id, title, ...) {
  figure_panel(
    label = "Ćwiczenie",
    h4(title), tagList(...),
    actionButton(paste0("ch9_ans", id), "Pokaż rozwiązanie",
                 class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput(paste0("ch9_sol", id))
  )
}

.ch9_content_sat <- function() tagList(
  lc_feedback(type = "info",
    p("Kontekst dotyczy pomiarów i zdarzeń związanych z sensorami. Nie trzeba znać
       budowy instrumentu — rozpoznajemy wyłącznie strukturę losowego zjawiska."),
    p("W bloku danych użyj pliku ",
      tags$code("../dane/satelitarne_obserwacje.csv"), ".")
  ),

  lc_h3("Blok 1: Dobierz rozkład (30 min)"),

  .ch9_sat_panel("1", "Zadanie 1 — Udane transmisje",
    p("Każda z 20 niezależnych prób transmisji pakietu ma prawdopodobieństwo
       powodzenia 0.9. Oblicz P(co najmniej 18 udanych), wartość oczekiwaną i SD.")),

  .ch9_sat_panel("2", "Zadanie 2 — Liczba wykrytych zdarzeń",
    p("W regionie wykrywa się średnio 3.2 wyładowania atmosferycznego na godzinę.
       Oblicz P(X=0), P(X=5) i P(X>4). Jaki warunek modelu przyjmujemy?")),

  .ch9_sat_panel("3", "Zadanie 3 — Błąd pomiaru temperatury",
    p("Powtarzane pomiary tej samej powierzchni opisujemy roboczo przez
       N(26.5°C, 1.2°C). Oblicz P(X>28), P(25<X<27) oraz 95. percentyl.")),

  .ch9_sat_panel("4", "Zadanie 4 — Czas między użytecznymi obserwacjami",
    p("Średni czas między użytecznymi obserwacjami wynosi 12 dni.
       Przyjmij rozkład wykładniczy i oblicz P(T>20) oraz P(T<5).")),

  lc_h3("Blok 2: Rozpoznawanie modeli (20 min)"),

  .ch9_sat_panel("5", "Zadanie 5 — Cztery sytuacje",
    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      tags$thead(tags$tr(tags$th(""), tags$th("Sytuacja"),
                         tags$th("Rozkład"), tags$th("Parametry"))),
      tags$tbody(
        .z5row("a", "Liczba pikseli poprawnie sklasyfikowanych w próbie 50 pikseli"),
        .z5row("b", "Liczba pożarów wykrytych w regionie w tygodniu"),
        .z5row("c", "Błąd położenia GNSS w jednej osi"),
        .z5row("d", "Czas do następnej awarii prostego komponentu")
      )
    )),

  .ch9_sat_panel("6", "Zadanie 6 — Założenie jest częścią modelu",
    tags$ol(
      tags$li("Dlaczego liczba zdarzeń w czasie nie zawsze ma rozkład Poissona?"),
      tags$li("Dlaczego błąd pomiaru nie zawsze jest normalny?"),
      tags$li("Czy kolejne piksele obrazu można automatycznie uznać za niezależne próby Bernoulliego?"),
      tags$li("Co może łamać bezpamięciowość czasu między obserwacjami?")
    )),

  lc_h3("Blok 3: Dane i krytyczne myślenie (30 min)"),

  .ch9_sat_panel("7", "Zadanie 7 — Czy temperatura wygląda normalnie?",
    p("Narysuj histogram i Q-Q plot ", tags$code("sat_temp_c"),
      ". Następnie rozdziel dane według ", tags$code("typ_pokrycia"), "."),
    p("Czy mieszanka różnych powierzchni może wyglądać mniej normalnie niż każda grupa osobno?")),

  .ch9_sat_panel("8", "Zadanie 8 — Dostępny pomiar jako zdarzenie",
    p("Oszacuj z danych p = odsetek ", tags$code("pomiar_dostepny == 'tak'"),
      ". Dla 10 nowych niezależnych lokalizacji policz P(co najmniej 8 dostępnych),
       używając oszacowanego p.")),

  .ch9_sat_panel("9", "Zadanie 9 — Rozkład różnic sensor–grunt",
    p("Opisz i narysuj ", tags$code("roznica_temp_c"), ". Czy środek rozkładu
       leży w pobliżu zera? Co oznaczałoby przesunięcie całego rozkładu?")),

  .ch9_sat_panel("10", "Zadanie 10 — Milion pikseli",
    p("Raport mówi: „mamy milion pikseli, więc dzięki CTG niepewność jest
       praktycznie zerowa”. Wskaż dwa problemy z tym zdaniem.")),

  lc_feedback(type = "warning",
    p("Rozkład jest modelem zjawiska, a nie etykietą dobieraną wyłącznie po
       kształcie histogramu. Zawsze pytaj, co jest pojedynczą próbą i czy próby
       można uznać za niezależne.")
  ),
  actionButton("ch9_ans_summary", "Pokaż odpowiedzi",
               class = "lc-btn-ok-outline lc-btn-sm"),
  uiOutput("ch9_sol_summary")
)

.ch9_sat_solutions <- local({
  d <- .ch9_sat_data
  p_available <- mean(d$pomiar_dostepny == "tak")
  list(
    sol1 = withMathJax(tagList(
      p("X ~ B(20, 0.9)."),
      p("P(X ≥ 18) = ", .fmt_p(1 - pbinom(17, 20, 0.9)),
        ", E(X)=18, SD(X)=", sprintf("%.2f", sqrt(20 * 0.9 * 0.1)), ".")
    )),
    sol2 = withMathJax(tagList(
      p("X ~ Pois(3.2). P(X=0) = ", .fmt_p(dpois(0, 3.2)),
        ", P(X=5) = ", .fmt_p(dpois(5, 3.2)),
        ", P(X>4) = ", .fmt_p(1 - ppois(4, 3.2)), "."),
      p("Zakładamy w przybliżeniu stałą intensywność i niezależność zdarzeń.")
    )),
    sol3 = withMathJax(tagList(
      p("P(X>28) = ", .fmt_p(1 - pnorm(28, 26.5, 1.2)),
        ", P(25<X<27) = ", .fmt_p(pnorm(27, 26.5, 1.2) - pnorm(25, 26.5, 1.2)),
        ", q95 = ", sprintf("%.2f°C", qnorm(0.95, 26.5, 1.2)), ".")
    )),
    sol4 = withMathJax(tagList(
      p("T ~ Exp(rate=1/12). P(T>20) = ", .fmt_p(pexp(20, 1/12, lower.tail = FALSE)),
        ", P(T<5) = ", .fmt_p(pexp(5, 1/12)), ".")
    )),
    sol5 = tagList(
      p("a) dwumianowy; b) Poissona; c) normalny jako model przybliżony;
         d) wykładniczy jako prosty model czasu oczekiwania.")
    ),
    sol6 = tagList(
      p("Zdarzenia mogą się grupować, błędy mogą mieć outliery lub asymetrię,
         sąsiednie piksele bywają podobne, a harmonogram i zachmurzenie sprawiają,
         że czas oczekiwania nie jest bezpamięciowy.")
    ),
    sol7 = tagList(
      p("Łączny rozkład miesza powierzchnie o różnych typowych temperaturach.
         Rozdzielenie według pokrycia może ujawnić prostsze rozkłady wewnątrz grup.")
    ),
    sol8 = withMathJax(tagList(
      p(sprintf("W danych p̂ = %.3f. Dla X ~ B(10, %.3f), P(X ≥ 8) = %s.",
                p_available, p_available,
                .fmt_p(1 - pbinom(7, 10, p_available))))
    )),
    sol9 = tagList(
      p(sprintf("Średnia różnica wynosi %.2f°C, a mediana %.2f°C.",
                mean(d$roznica_temp_c), median(d$roznica_temp_c))),
      p("Przesunięcie od zera sugeruje systematyczne obciążenie, nie tylko losowy szum.")
    ),
    sol10 = tagList(
      p("Piksele są przestrzennie zależne, więc milion wierszy nie oznacza miliona
         niezależnych informacji. Ponadto błąd systematyczny nie znika przez
         zwiększenie liczby pomiarów.")
    ),
    sol_summary = tagList(
      p("Najpierw definiujemy pojedynczą próbę i mechanizm zjawiska, dopiero potem
         wybieramy rozkład i liczymy prawdopodobieństwo.")
    )
  )
})
