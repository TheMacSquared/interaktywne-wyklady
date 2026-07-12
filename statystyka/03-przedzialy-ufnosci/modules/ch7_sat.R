# Wariant kierunkowy: Inżynieria danych satelitarnych i kosmicznych.

.ch7_sat_data <- read.csv(
  file.path(project_root, "dane", "satelitarne_obserwacje.csv"),
  stringsAsFactors = FALSE
)

.ch7_sat_panel <- function(id, title, ...) {
  figure_panel(
    label = "Ćwiczenie", h4(title), tagList(...),
    actionButton(paste0("ch7_ans", id), "Pokaż rozwiązanie",
                 class = "lc-btn-ok-outline lc-btn-sm"),
    uiOutput(paste0("ch7_sol", id))
  )
}

.ch7_content_sat <- function() tagList(
  lc_feedback(type = "info",
    p("Otwórz ", tags$code("../dane/satelitarne_obserwacje.csv"), " w Jamovi."),
    p("Jednostką obserwacji jest lokalizacja w określonym terminie. Przedziały
       liczymy tutaj klasycznie, traktując wiersze jako próbę; na końcu wrócimy
       do pytania, czy bliskie lokalizacje są rzeczywiście niezależne.")
  ),

  lc_h3("Blok 1: Przedziały dla średniej (30 min)"),
  .ch7_sat_panel("1", "Zadanie 1 — Średnia temperatura z sensora",
    p("Wyznacz 95% CI dla średniej ", tags$code("sat_temp_c"),
      ". Zapisz wynik w °C i zinterpretuj dla populacji podobnych lokalizacji
       obserwowanych w podobnych warunkach.")),

  .ch7_sat_panel("2", "Zadanie 2 — Sensor względem pomiaru naziemnego",
    p("Wyznacz 95% CI dla średniej ", tags$code("roznica_temp_c"),
      " = satelita − grunt. Czy przedział zawiera 0? Co mówi znak różnicy?"),
    p(tags$em("To analiza sparowana:"), " oba pomiary dotyczą tej samej lokalizacji.")),

  .ch7_sat_panel("3", "Zadanie 3 — Mniejsza podpróba",
    p("Wyznacz 95% CI dla ", tags$code("sat_temp_c"),
      " tylko w strefie miejskiej. Porównaj szerokość z zadaniem 1.")
  ),

  .ch7_sat_panel("4", "Zadanie 4 — Poziom ufności a szerokość",
    p("Dla ", tags$code("roznica_temp_c"),
      " policz CI 90%, 95% i 99%. Jak zmienia się margines błędu?")),

  lc_h3("Blok 2: Przedziały dla proporcji (25 min)"),
  .ch7_sat_panel("5", "Zadanie 5 — Dostępność pomiaru",
    p("Wyznacz 95% CI dla proporcji obserwacji z ",
      tags$code("pomiar_dostepny == 'tak'"), ".")),

  .ch7_sat_panel("6", "Zadanie 6 — Pomiary dobrej jakości",
    p("Wyznacz 95% CI dla proporcji obserwacji z ",
      tags$code("jakosc_pomiaru == 'dobra'"),
      ". Porównaj szerokość z zadaniem 5.")),

  .ch7_sat_panel("7", "Zadanie 7 — Dwie strefy",
    p("Policz odsetek pomiarów dobrej jakości osobno dla strefy miejskiej i
       zielonej. Wyznacz CI dla różnicy proporcji."),
    p("Czy dane wskazują na wyraźną różnicę dostępności między strefami?")),

  lc_h3("Blok 3: Interpretacja (20 min)"),
  .ch7_sat_panel("8", "Zadanie 8 — Co naprawdę mówi przedział?",
    tags$ol(type = "a",
      tags$li("95% wszystkich pikseli ma temperaturę wewnątrz CI dla średniej."),
      tags$li("Gdybyśmy powtarzali pobieranie próby, około 95% tak zbudowanych CI zawierałoby prawdziwą średnią."),
      tags$li("Węższy CI oznacza automatycznie, że sensor nie ma błędu systematycznego."),
      tags$li("Bardzo dużo sąsiednich pikseli może dać zbyt optymistyczny CI, jeśli potraktujemy je jako niezależne."),
      tags$li("CI dla średniej różnicy satelita−grunt służy do oceny przeciętnego obciążenia.")
    )),

  lc_feedback(type = "warning",
    p("Klasyczny CI zakłada niezależne obserwacje. Tutaj używamy go dydaktycznie,
       ale dla gęstej siatki przestrzennej należałoby uwzględnić podobieństwo
       sąsiednich lokalizacji. Na pierwszym semestrze wystarczy umieć nazwać to ograniczenie.")
  )
)

.ch7_sat_solutions <- local({
  d <- .ch7_sat_data
  ci1 <- .ci_mean(d$sat_temp_c)
  ci2 <- .ci_mean(d$roznica_temp_c)
  city <- .ci_mean(d$sat_temp_c[d$strefa == "miejska"])
  ci90 <- .ci_mean(d$roznica_temp_c, level = 0.90)
  ci99 <- .ci_mean(d$roznica_temp_c, level = 0.99)
  p_available <- .ci_prop(d$pomiar_dostepny == "tak")
  p_good <- .ci_prop(d$jakosc_pomiaru == "dobra")

  good <- d$jakosc_pomiaru == "dobra"
  urban <- d$strefa == "miejska"
  tab <- table(urban, good)
  pt <- prop.test(c(tab["TRUE", "TRUE"], tab["FALSE", "TRUE"]),
                  c(sum(tab["TRUE", ]), sum(tab["FALSE", ])), correct = FALSE)

  list(
    sol1 = tagList(
      p(sprintf("n=%d, średnia=%.2f°C, s=%.2f°C, 95%% CI=[%.2f, %.2f]°C.",
                ci1$n, ci1$mean, ci1$sd, ci1$lo, ci1$hi))
    ),
    sol2 = tagList(
      p(sprintf("Średnia różnica=%.2f°C, 95%% CI=[%.2f, %.2f]°C.",
                ci2$mean, ci2$lo, ci2$hi)),
      p("Przedział leży powyżej 0, więc dane wskazują na przeciętne zawyżanie
         temperatury przez sensor w tym syntetycznym zbiorze.")
    ),
    sol3 = tagList(
      p(sprintf("Strefa miejska: n=%d, średnia=%.2f°C, 95%% CI=[%.2f, %.2f]°C.",
                city$n, city$mean, city$lo, city$hi)),
      p("Mniejsza podpróba zwykle daje większy błąd standardowy, choć szerokość
         zależy także od rozrzutu wewnątrz grupy.")
    ),
    sol4 = tagList(
      p(sprintf("90%% CI=[%.2f, %.2f], 95%% CI=[%.2f, %.2f], 99%% CI=[%.2f, %.2f]°C.",
                ci90$lo, ci90$hi, ci2$lo, ci2$hi, ci99$lo, ci99$hi)),
      p("Większa ufność wymaga szerszego przedziału.")
    ),
    sol5 = tagList(
      p(sprintf("Dostępnych: %d/%d (%.1f%%), 95%% CI=[%.3f, %.3f].",
                p_available$k, p_available$n, 100 * p_available$p,
                p_available$lo, p_available$hi))
    ),
    sol6 = tagList(
      p(sprintf("Dobra jakość: %d/%d (%.1f%%), 95%% CI=[%.3f, %.3f].",
                p_good$k, p_good$n, 100 * p_good$p, p_good$lo, p_good$hi)),
      p("Szerokość zależy od n oraz od położenia proporcji względem 0.5.")
    ),
    sol7 = tagList(
      p(sprintf("95%% CI dla różnicy p(miejska)−p(zielona)=[%.3f, %.3f].",
                pt$conf.int[1], pt$conf.int[2])),
      p(if (pt$conf.int[1] <= 0 && pt$conf.int[2] >= 0) {
        "Przedział zawiera 0 — dane nie wskazują na wyraźną różnicę między strefami."
      } else {
        "Przedział nie zawiera 0 — dane wskazują na różnicę między strefami."
      })
    ),
    sol8 = tagList(
      tags$ul(
        tags$li("a) Fałsz — to interpretacja pojedynczych obserwacji, nie średniej."),
        tags$li("b) Prawda — interpretacja częstościowa procedury."),
        tags$li("c) Fałsz — duża precyzja nie usuwa obciążenia."),
        tags$li("d) Prawda — zależność przestrzenna zmniejsza efektywną ilość informacji."),
        tags$li("e) Prawda — różnice są sparowane według lokalizacji.")
      )
    )
  )
})
