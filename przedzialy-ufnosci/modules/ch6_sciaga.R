# ============================================================================
# CHAPTER 6: Sciaga - podsumowanie przedzialow ufnosci
# ============================================================================

ch6_ui <- tabPanel("6. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Kompaktowe podsumowanie wszystkiego, co poznali\u015bmy o przedzia\u0142ach ufno\u015bci."
    ),

    div(class = "section-title", "Wzory przedzia\u0142\u00f3w ufno\u015bci"),

    div(class = "formula-box",
      h4("Przedzia\u0142 dla \u015bredniej (znane \u03c3)"),
      withMathJax(helpText(
        "$$\\bar{x} \\pm z^* \\cdot \\frac{\\sigma}{\\sqrt{n}}$$"
      )),
      p("Stosowany rzadko \u2014 wymaga znajomo\u015bci \u03c3 populacji.")
    ),

    div(class = "formula-box",
      h4("Przedzia\u0142 dla \u015bredniej (nieznane \u03c3) \u2014 STANDARDOWY"),
      withMathJax(helpText(
        "$$\\bar{x} \\pm t^*_{\\alpha/2,\\, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$"
      )),
      p("Najcz\u0119\u015bciej u\u017cywany. Wymaga: dane ilo\u015bciowe, w przybli\u017ceniu normalne (lub du\u017ce n).")
    ),

    div(class = "formula-box",
      h4("Przedzia\u0142 Walda dla proporcji"),
      withMathJax(helpText(
        "$$\\hat{p} \\pm z^* \\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}$$"
      )),
      p("Prosty, ale niedok\u0142adny przy skrajnych p lub ma\u0142ych n. Wymaga: np \u2265 10 i n(1\u2212p) \u2265 10.")
    ),

    div(class = "formula-box",
      h4("Przedzia\u0142 Wilsona dla proporcji (zalecany)"),
      p("Lepsze pokrycie ni\u017c Wald, szczeg\u00f3lnie dla ma\u0142ych pr\u00f3b i skrajnych proporcji."),
      p("R:", tags$code("prop.test(x, n, conf.level = 0.95)"), " \u2014 domy\u015blnie u\u017cywa metody Wilsona.")
    ),

    # ========================================================================
    div(class = "section-title", "Warto\u015bci krytyczne"),

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 15px;",
      tags$thead(
        tags$tr(
          tags$th("Poziom ufno\u015bci"),
          tags$th(withMathJax("\\(z^*\\)")),
          tags$th(withMathJax("\\(\\alpha\\)"))
        )
      ),
      tags$tbody(
        tags$tr(tags$td("90%"), tags$td("1.645"), tags$td("0.10")),
        tags$tr(tags$td("95%"), tags$td("1.960"), tags$td("0.05")),
        tags$tr(tags$td("99%"), tags$td("2.576"), tags$td("0.01"))
      )
    ),

    p(style = "color: #7f8c8d; font-size: 14px;",
      "Dla przedzia\u0142u t: u\u017cyj ", tags$code("qt(1 - \u03b1/2, df = n - 1)"),
      " zamiast z*."),

    # ========================================================================
    div(class = "section-title", "Drzewo decyzyjne: kt\u00f3ry przedzia\u0142?"),

    div(class = "callout-info",
      tags$strong("1."), " Co szacujesz?",
      tags$ul(
        tags$li(tags$b("\u015aredni\u0105"), " \u2192 przejd\u017a do pytania 2"),
        tags$li(tags$b("Proporcj\u0119"), " \u2192 przejd\u017a do pytania 3")
      ),
      tags$strong("2."), " Czy znasz \u03c3 populacji?",
      tags$ul(
        tags$li(tags$b("Tak"), " \u2192 z-interval (rzadko)"),
        tags$li(tags$b("Nie"), " \u2192 t-interval (prawie zawsze)")
      ),
      tags$strong("3."), " Czy np \u2265 10 i n(1\u2212p) \u2265 10?",
      tags$ul(
        tags$li(tags$b("Tak"), " \u2192 Wald lub Wilson"),
        tags$li(tags$b("Nie"), " \u2192 Wilson lub Clopper-Pearson (dok\u0142adny)")
      )
    ),

    # ========================================================================
    div(class = "section-title", "Co wp\u0142ywa na szeroko\u015b\u0107?"),

    tags$table(class = "table table-bordered",
      style = "font-size: 15px;",
      tags$thead(
        tags$tr(
          tags$th("Czynnik"),
          tags$th("Wzrost \u2192"),
          tags$th("Efekt na szeroko\u015b\u0107")
        )
      ),
      tags$tbody(
        tags$tr(tags$td("n"), tags$td("\u2191"), tags$td("\u2193 (w\u0119\u017cszy)")),
        tags$tr(tags$td("Poziom ufno\u015bci"), tags$td("\u2191"), tags$td("\u2191 (szerszy)")),
        tags$tr(tags$td("Zmienno\u015b\u0107 (s)"), tags$td("\u2191"), tags$td("\u2191 (szerszy)"))
      )
    ),

    p(withMathJax("Efekt n jest sub-liniowy: \\(ME \\propto 1/\\sqrt{n}\\).
      \u017ceby zmniejszy\u0107 ME o po\u0142ow\u0119, potrzebujesz 4\u00d7 wi\u0119cej danych.")),

    # ========================================================================
    div(class = "section-title", "Planowanie wielko\u015bci pr\u00f3by"),

    div(class = "formula-box",
      h4("Dla \u015bredniej (przybli\u017cenie z)"),
      withMathJax(helpText(
        "$$n = \\left(\\frac{z^* \\cdot s}{ME_{max}}\\right)^2$$"
      ))
    ),

    div(class = "formula-box",
      h4("Dla proporcji"),
      withMathJax(helpText(
        "$$n = \\frac{z^{*2} \\cdot \\hat{p}(1-\\hat{p})}{ME_{max}^2}$$"
      )),
      p("Gdy nie znamy p, u\u017cywamy p = 0.5 (daje maksymalne n).")
    ),

    # ========================================================================
    div(class = "section-title", "Funkcje R"),

    div(class = "formula-box",
      tags$pre(style = "background: #f8f9fa; padding: 12px; border-radius: 6px;",
        tags$code(
"# Przedzial dla sredniej
t.test(x, conf.level = 0.95)

# Lub z rstatix
library(rstatix)
data %>% t_test(variable ~ 1, mu = 0, conf.level = 0.95)

# Przedzial dla proporcji (metoda Wilsona)
prop.test(x = sukcesy, n = proba, conf.level = 0.95)

# Dokladny przedzial Cloppera-Pearsona
binom.test(x = sukcesy, n = proba, conf.level = 0.95)"
        )
      )
    ),

    # ========================================================================
    div(class = "section-title", "Typowe b\u0142\u0119dy interpretacji"),

    div(class = "callout-danger",
      tags$strong("B\u0141\u0118DNE:"),
      tags$ul(
        tags$li("\"\u03bc le\u017cy w tym przedziale z 95% prawdopodobie\u0144stwem\" \u2014 \u03bc jest sta\u0142e!"),
        tags$li("\"95% danych le\u017cy w tym przedziale\" \u2014 to nie prediction interval"),
        tags$li("\"\u015arednia z pr\u00f3by le\u017cy w przedziale\" \u2014 zawsze le\u017cy, z definicji")
      )
    ),

    div(class = "callout-success",
      tags$strong("POPRAWNE:"),
      tags$ul(
        tags$li("\"Gdyby\u015bmy powtarzali badanie, 95% tak skonstruowanych przedzia\u0142\u00f3w zawiera\u0142oby \u03bc\""),
        tags$li("\"Metoda, kt\u00f3r\u0105 u\u017cyli\u015bmy, daje poprawne przedzia\u0142y w 95% przypadk\u00f3w\""),
        tags$li("\"Mamy 95% ufno\u015bci w metod\u0119, kt\u00f3ra wytworzy\u0142a ten przedzia\u0142\"")
      )
    ),

    # ========================================================================
    div(class = "chapter-transition",
      p("Czas zastosowa\u0107 t\u0119 wiedz\u0119 w praktyce na rzeczywistym zbiorze danych."),
      actionButton("ch6_to_ch7", "Dalej: \u0106wiczenia \u2192", class = "btn-primary")
    )

  ))
)

# ============================================================================
# SERVER (brak interaktywnych widgetow)
# ============================================================================

ch6_server <- function(input, output, session) {
  # Sciaga nie wymaga logiki server
}
