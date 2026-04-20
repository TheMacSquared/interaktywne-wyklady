# ============================================================================
# CHAPTER 11: Sciaga - tabela porownawcza paradygmatow
# ============================================================================

ch11_ui <- tabPanel("11. \u015aci\u0105ga",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Podsumowanie na jednej stronie: kiedy u\u017cywa\u0107 kt\u00f3rego paradygmatu,
       kt\u00f3ra funkcja R odpowiada ktoremu testowi, i jak interpretowa\u0107 wyniki."
    ),

    div(class = "section-title", "Paradygmat \u2194 Paradygmat: czym si\u0119 r\u00f3\u017cni\u0105?"),

    tags$table(class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Pytanie"),
          tags$th("Cz\u0119sto\u015bciowo"),
          tags$th("Bayesowsko")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$b("Co szacujemy?")),
          tags$td("P(dane | H\u2080)"),
          tags$td("P(H | dane)")
        ),
        tags$tr(
          tags$td(tags$b("Miara dowodu")),
          tags$td("p-warto\u015b\u0107 (istotny/nieistotny)"),
          tags$td("BF\u2081\u2080 (skala si\u0142y: anekdot./umiark./silny/ekstr.)")
        ),
        tags$tr(
          tags$td(tags$b("Przedzia\u0142")),
          tags$td("95% CI \u2014 metoda, kt\u00f3ra w 95% przypadk\u00f3w pokrywa prawd\u0119"),
          tags$td("95% HDI \u2014 prawdopodobie\u0144stwo 95%, \u017ce parametr tu le\u017cy")
        ),
        tags$tr(
          tags$td(tags$b("Dow\u00f3d za H\u2080")),
          tags$td("Niemo\u017cliwy (brak odrzucenia \u2260 akceptacja)"),
          tags$td("Tak: BF\u2081\u2080 < 1/3 = dow\u00f3d za H\u2080")
        ),
        tags$tr(
          tags$td(tags$b("Prior")),
          tags$td("Brak \u2014 wnioskujemy tylko z danych"),
          tags$td("Wymagany \u2014 trzeba zdeklarowa\u0107 sprzed badania")
        ),
        tags$tr(
          tags$td(tags$b("Pytanie praktyczne")),
          tags$td("\u201ePrzy \u03b1 = 0.05: odrzuci\u0107 H\u2080?\u201f"),
          tags$td("\u201eJakie warto\u015bci parametru s\u0105 zgodne z danymi?\u201f")
        )
      )
    ),

    div(class = "section-title", "Tabela metod: co u\u017cywa\u0107 w R?"),

    tags$table(class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Problem"),
          tags$th("Cz\u0119sto\u015bciowo"),
          tags$th("Bayesowsko")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$b("Jedna pr\u00f3ba: \u03bc = \u03bc\u2080?")),
          tags$td(tags$code("t.test(x, mu=mu0)")),
          tags$td(tags$code("BayesFactor::ttestBF(x, mu=mu0)"))
        ),
        tags$tr(
          tags$td(tags$b("Dwie grupy")),
          tags$td(tags$code("t.test(y~grupa)"), " lub ",
                   tags$code("rstatix::t_test()")),
          tags$td(tags$code("ttestBF(formula=y~grupa, data=...)"))
        ),
        tags$tr(
          tags$td(tags$b("ANOVA (3+ grup)")),
          tags$td(tags$code("aov(y~grupa)"), " / ",
                   tags$code("rstatix::anova_test()")),
          tags$td(tags$code("anovaBF(y~grupa, data=...)"))
        ),
        tags$tr(
          tags$td(tags$b("Tabela krzy\u017cowa")),
          tags$td(tags$code("chisq.test(table)")),
          tags$td(tags$code("contingencyTableBF(table, sampleType=\"indepMulti\", fixedMargin=\"rows\")"))
        ),
        tags$tr(
          tags$td(tags$b("Korelacja")),
          tags$td(tags$code("cor.test(x, y)")),
          tags$td(tags$code("correlationBF(x, y)"))
        ),
        tags$tr(
          tags$td(tags$b("Regresja liniowa")),
          tags$td(tags$code("lm(y ~ x1 + x2)")),
          tags$td(tags$code("rstanarm::stan_glm(y~x1+x2, family=gaussian)"))
        ),
        tags$tr(
          tags$td(tags$b("Regresja logistyczna")),
          tags$td(tags$code("glm(y~x, family=binomial)")),
          tags$td(tags$code("stan_glm(y~x, family=binomial)"))
        )
      )
    ),

    div(class = "section-title", "Kiedy kt\u00f3ry paradygmat?"),

    div(class = "narrative",
      p(tags$b("Cz\u0119sto\u015bciowo jest wygodniejsze gdy:")),
      tags$ul(
        tags$li("Recenzenci/czasopismo oczekuj\u0105 p-warto\u015bci (wci\u0105\u017c standard w wielu dziedzinach)"),
        tags$li("Analiza jest rutynowa, bez silnych przekona\u0144 sprzed badania"),
        tags$li("Du\u017ca pr\u00f3ba + prosta hipoteza (daje te same liczby)")
      ),
      p(tags$b("Bayesowsko jest warto\u015bciowe gdy:")),
      tags$ul(
        tags$li("Ma\u0142a pr\u00f3ba \u2014 regularyzacja priorem stabilizuje estymacj\u0119"),
        tags$li("Chcesz powiedzie\u0107 \u201eefektu nie ma\u201f (BF < 1/3 pozwala)"),
        tags$li("Masz wiedz\u0119 sprzed badania, kt\u00f3r\u0105 chcesz w\u0142\u0105czy\u0107"),
        tags$li("Interesuje Ci\u0119 bezpo\u015brednio P(H | dane), nie P(dane | H\u2080)"),
        tags$li("Chcesz zada\u0107 pytanie o praktyczny pr\u00f3g: P(r\u00f3\u017cnica > 2 jednostki)")
      )
    ),

    div(class = "callout-success",
      tags$b("Podsumowanie w jednym zdaniu: "),
      "cz\u0119sto\u015bciowa statystyka odpowiada na pytanie \u201ejak zaskakuj\u0105ce s\u0105 moje dane, gdyby H\u2080 by\u0142o prawd\u0105?\u201f,
       bayesowska \u2014 \u201ejak prawdopodobne s\u0105 r\u00f3\u017cne warto\u015bci parametru, daj\u0105c moje dane i prior?\u201f.
       To dwa r\u00f3\u017cne pytania, oba legalne, cz\u0119sto komplementarne."
    ),

    div(class = "section-title", "Skala Jeffreysa dla BF"),

    tags$table(class = "table table-bordered",
      tags$thead(
        tags$tr(
          tags$th("BF\u2081\u2080"),
          tags$th("Interpretacja (dow\u00f3d za H\u2081)")
        )
      ),
      tags$tbody(
        tags$tr(tags$td("1 \u2013 3"), tags$td("Anekdotyczny (s\u0142aby) \u2014 prawie brak dowodu")),
        tags$tr(tags$td("3 \u2013 10"), tags$td("Umiarkowany")),
        tags$tr(tags$td("10 \u2013 30"), tags$td("Silny")),
        tags$tr(tags$td("30 \u2013 100"), tags$td("Bardzo silny")),
        tags$tr(tags$td("> 100"), tags$td("Ekstremalny"))
      )
    ),
    p(tags$em("Symetrycznie: BF\u2081\u2080 < 1 czytamy jako 1/BF\u2081\u2080 dow\u00f3d za H\u2080.")),

    div(class = "chapter-transition",
      p("Przejd\u017a do \u0107wicze\u0144 \u2014 praktyczne zadania w Twoim kierunku."),
      actionButton("ch11_to_ch12",
                   "\u0106wiczenia kierunkowe \u2192",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch11_server <- function(input, output, session) {
  # Brak reactivity - tylko statyczna tre\u015b\u0107
}
