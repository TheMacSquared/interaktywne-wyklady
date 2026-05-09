# ============================================================================
# ROZDZIAŁ 1: Od parametru do estymatora
# ============================================================================

ch1_ui <- lecture_chapter(
  id = "ch-estymatory",
  num = "01",
  title = "Od parametru do estymatora",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Estymacja",
      num = "01",
      title = "Od parametru do estymatora.",
      lead = "Parametr β₁ jest nieznany — opisuje całą populację. Estymator b₁ liczymy z konkretnej próby. Dwa różne zespoły, dwie próby, dwa różne b — który ma rację? Obaj."
    ),

    lc_h2("ch1-rozroznienie", "β czy b — co je odróżnia?"),
    lc_p("Statystyka rozróżnia dwa byty, które na pierwszy rzut oka wyglądają podobnie. Parametr populacyjny opisuje cały świat, którego nie widzimy w całości. Estymata to liczba, którą faktycznie obliczamy z próby — i która zmieniłaby się, gdyby próba była inna."),
    figure_panel(
      label = "Tabela 1.1",
      title = "Parametr populacji vs. estymata z próby",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Cecha"),
          tags$th("Parametr β"),
          tags$th("Estymata b")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Skąd pochodzi?"),
            tags$td("z całej populacji"),
            tags$td("z konkretnej próby")
          ),
          tags$tr(
            tags$td("Czy go znamy?"),
            tags$td("nie — to wartość, którą próbujemy odgadnąć"),
            tags$td("tak — to liczba, którą wyliczamy")
          ),
          tags$tr(
            tags$td("Czy jest losowy?"),
            tags$td("nie — to stała, choć nieznana"),
            tags$td("tak — zależy od tego, kogo trafiliśmy do próby")
          ),
          tags$tr(
            tags$td("Notacja"),
            tags$td("β₀, β₁ (litery greckie)"),
            tags$td("b₀, b₁ albo β̂₀, β̂₁ (z daszkiem)")
          )
        )
      )
    ),

    lc_h2("ch1-formula", "Wzór estymatora KMNK"),
    lc_formula_box(
      withMathJax(helpText("$$\\hat\\beta_1 = \\frac{\\sum_i (x_i - \\bar{x})(y_i - \\bar{y})}{\\sum_i (x_i - \\bar{x})^2}$$")),
      withMathJax(helpText("$$\\hat\\beta_0 = \\bar{y} - \\hat\\beta_1 \\bar{x}$$")),
      p("Licznik to kowariancja X i Y — czyli to, jak Y systematycznie zmienia się razem z X. Mianownik to wariancja X — czyli to, jak rozrzucone są same wartości X. Iloraz mówi, ile jednostek Y przypada średnio na jedną jednostkę X.")
    ),

    lc_h2("ch1-historia", "Historia z dwóch firm konsultingowych"),
    lc_p("Pewna sieć handlowa zleciła dwóm firmom konsultingowym to samo pytanie: jak metraż sklepu wpływa na miesięczną sprzedaż? Każda firma dostała inną próbę 50 sklepów z tego samego rynku."),
    lc_p("Firma A oszacowała b₁ = 1.42 (tysiąca zł sprzedaży na metr kwadratowy). Firma B — b₁ = 1.61. Klient wpadł w popłoch: kto się pomylił? Odpowiedź: nikt. Obie próby pochodzą z tej samej populacji sklepów, ale przez losowy dobór trafiły na nieco inny zestaw lokalizacji. Każda estymata jest poprawnym oszacowaniem prawdziwego, nieznanego β₁ — różnią się dlatego, że dane są zaszumione, a próby nie pokrywają się idealnie."),
    lc_p("Wniosek dla analityka: pojedyncza estymata to nie wyrok. Dopiero w połączeniu z miarą niepewności — błędem standardowym, do którego dojdziemy w następnym rozdziale — można powiedzieć, czy 1.42 i 1.61 to praktycznie ta sama odpowiedź, czy jednak istotnie różne wyniki."),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "Estymator to przepis (formuła). Estymata to konkretna liczba uzyskana po podstawieniu danych. b₁ jest losowy w sensie: zależy od próby, nie od populacji. Dwie próby z tej samej populacji dadzą dwie różne estymaty — i to nie jest błąd, tylko cecha pracy z danymi."
    ),

    lc_chapter_next(
      num = "02",
      title = "Błędy standardowe",
      lead = "miara niepewności estymatora",
      target_id = "ch-se"
    )
  )
)

ch1_server <- function(input, output, session) {}
