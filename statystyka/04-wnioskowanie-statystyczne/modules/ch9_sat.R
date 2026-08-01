# Wariant kierunkowy: Inżynieria danych satelitarnych i kosmicznych.

.ch9_sat_data <- read.csv(
  file.path(project_root, "dane", "satelitarne_obserwacje.csv"),
  stringsAsFactors = FALSE
)

.ch9_sat_exercise <- function(id, title, ...) {
  figure_panel(
    label = paste("Ćwiczenie S", id), h4(title), tagList(...),
    actionButton(paste0("ch9_s_ans", id), "Pokaż rozwiązanie",
                 class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput(paste0("ch9_s_sol", id))
  )
}

ch9_sat_ui <- function() tagList(
  lc_h2("ch9-sat", "Wariant kierunkowy", "Dane satelitarne — pomiar powierzchni"),
  lc_feedback(type = "info",
    p(tags$b("Dane: "), tags$code("../dane/satelitarne_obserwacje.csv"),
      " — 180 syntetycznych obserwacji lokalizacji."),
    p("Zmienne: temperatura satelitarna i naziemna, ich różnica, NDVI,
       zachmurzenie, typ pokrycia, strefa oraz jakość pomiaru.")
  ),

  .ch9_sat_exercise("1", "Czy sensor systematycznie zawyża temperaturę?",
    p("Przetestuj, czy średnia ", tags$code("roznica_temp_c"),
      " różni się od 0. Użyj testu t jednej próby i podaj Cohen's d."),
    p("Wyjaśnij, dlaczego wcześniejsze obliczenie różnicy zachowuje parowanie
       pomiaru satelitarnego i naziemnego dla tej samej lokalizacji.")),

  .ch9_sat_exercise("2", "Czy co najmniej 75% pomiarów jest dostępnych?",
    p("Wykonaj prawostronny test dwumianowy dla ",
      tags$code("pomiar_dostepny == 'tak'"), " względem p₀=0.75.")
  ),

  .ch9_sat_exercise("3", "Czy zachmurzenie wiąże się z błędem pomiaru?",
    p("Oblicz korelację Pearsona między ", tags$code("zachmurzenie_pct"),
      " i ", tags$code("roznica_temp_c"), ". Narysuj wykres punktowy."),
    p("Czy związek statystyczny dowodzi, że zachmurzenie jest jedyną przyczyną błędu?")),

  .ch9_sat_exercise("4", "Miejska wyspa ciepła jako porównanie grup",
    p("Porównaj ", tags$code("sat_temp_c"), " między strefą miejską i zieloną
       testem t Welcha. Podaj różnicę średnich, CI i Cohen's d.")),

  .ch9_sat_exercise("5", "Strefa a dostępność pomiaru",
    p("Zbuduj tabelę ", tags$code("strefa × pomiar_dostepny"),
      " i wykonaj test χ² niezależności. Podaj Cramér's V.")),

  .ch9_sat_exercise("6", "Temperatura dla różnych typów pokrycia",
    p("Wykonaj ANOVA: ", tags$code("sat_temp_c ~ typ_pokrycia"),
      ". Podaj F, p, η² i wskaż, które średnie warto porównać post-hoc.")),

  lc_feedback(type = "warning",
    p(tags$b("Ograniczenie:"), " w tych ćwiczeniach traktujemy lokalizacje jako
       niezależne, aby przećwiczyć podstawowe testy. Sąsiednie piksele mogą być
       podobne, więc analiza rzeczywistej gęstej siatki wymagałaby ostrożniejszej
       oceny niepewności. Nie rozwiązujemy tego jeszcze formalnie.")
  )
)

ch9_sat_server <- function(input, output, session) {
  d <- .ch9_sat_data
  vis <- lapply(seq_len(6), function(i) reactiveVal(FALSE))

  observeEvent(input$ch9_kierunek, {
    lapply(vis, function(x) x(FALSE))
    for (i in seq_len(6)) {
      updateActionButton(session, paste0("ch9_s_ans", i), label = "Pokaż rozwiązanie")
    }
  }, ignoreInit = TRUE)

  exercise_solution_toggle_server(input, output, session, "ch9_s_ans1", function() {
    r <- .ch9_t1(d$roznica_temp_c, mu = 0)
    tagList(
      .ch9_sol_t1(r, "μ różnicy = 0", "μ różnicy ≠ 0",
                  "różnica temperatur", 0, "°C"),
      p(sprintf("Średnia różnica %.2f°C wskazuje na przeciętne zawyżanie pomiaru.", r$m))
    )
  }, visible = vis[[1]])

  exercise_solution_toggle_server(input, output, session, "ch9_s_ans2", function() {
    x <- sum(d$pomiar_dostepny == "tak"); n <- nrow(d)
    bt <- binom.test(x, n, p = 0.75, alternative = "greater")
    tagList(
      p(sprintf("Dostępnych %d/%d = %.1f%%; %s.", x, n, 100*x/n,
                .ch9_fmt_p(bt$p.value))),
      .ch9_decision(bt$p.value)
    )
  }, visible = vis[[2]])

  exercise_solution_toggle_server(input, output, session, "ch9_s_ans3", function() {
    r <- .ch9_cor_test(d$zachmurzenie_pct, d$roznica_temp_c)
    tagList(
      .ch9_sol_cor(r, "zachmurzenie", "różnicy temperatur"),
      p("Korelacja opisuje związek. Bez planu eksperymentalnego i kontroli innych
         warunków nie dowodzi wyłącznej przyczyny.")
    )
  }, visible = vis[[3]])

  exercise_solution_toggle_server(input, output, session, "ch9_s_ans4", function() {
    r <- .ch9_t2(d$sat_temp_c, d$strefa)
    tagList(
      .ch9_sol_t2(r, "°C"),
      p("Różnica dotyczy związku strefy z temperaturą w danych obserwacyjnych,
         nie izolowanego efektu przyczynowego zabudowy.")
    )
  }, visible = vis[[4]])

  exercise_solution_toggle_server(input, output, session, "ch9_s_ans5", function() {
    r <- .ch9_chi2(table(strefa = d$strefa, dostepny = d$pomiar_dostepny))
    .ch9_sol_chi2(r)
  }, visible = vis[[5]])

  exercise_solution_toggle_server(input, output, session, "ch9_s_ans6", function() {
    r <- .ch9_anova_f(d$sat_temp_c, d$typ_pokrycia)
    .ch9_sol_anova(r, "temperatura (°C)")
  }, visible = vis[[6]])
}
