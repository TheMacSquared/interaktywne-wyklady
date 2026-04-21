# ============================================================================
# CHAPTER 11: Sciaga - tabela porownawcza paradygmatow
# ============================================================================

ch11_ui <- tabPanel("11. Ściąga",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Podsumowanie na jednej stronie: kiedy używać którego paradygmatu,
       która funkcja R odpowiada ktoremu testowi, i jak interpretować wyniki."
    ),

    div(class = "section-title", "Paradygmat ↔ Paradygmat: czym się różnią?"),

    tags$table(class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Pytanie"),
          tags$th("Częstościowo"),
          tags$th("Bayesowsko")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$b("Co szacujemy?")),
          tags$td("P(dane | H₀)"),
          tags$td("P(H | dane)")
        ),
        tags$tr(
          tags$td(tags$b("Miara dowodu")),
          tags$td("p-wartość (istotny/nieistotny)"),
          tags$td("BF₁₀ (skala siły: anekdot./umiark./silny/ekstr.)")
        ),
        tags$tr(
          tags$td(tags$b("Przedział")),
          tags$td("95% CI — metoda, która w 95% przypadków pokrywa prawdę"),
          tags$td("95% HDI — prawdopodobieństwo 95%, że parametr tu leży")
        ),
        tags$tr(
          tags$td(tags$b("Dowód za H₀")),
          tags$td("Niemożliwy (brak odrzucenia ≠ akceptacja)"),
          tags$td("Tak: BF₁₀ < 1/3 = dowód za H₀")
        ),
        tags$tr(
          tags$td(tags$b("Prior")),
          tags$td("Brak — wnioskujemy tylko z danych"),
          tags$td("Wymagany — trzeba zdeklarować sprzed badania")
        ),
        tags$tr(
          tags$td(tags$b("Pytanie praktyczne")),
          tags$td("„Przy α = 0.05: odrzucić H₀?‟"),
          tags$td("„Jakie wartości parametru są zgodne z danymi?‟")
        )
      )
    ),

    div(class = "section-title", "Tabela metod: co używać w R?"),

    tags$table(class = "table table-striped table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Problem"),
          tags$th("Częstościowo"),
          tags$th("Bayesowsko")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(tags$b("Jedna próba: μ = μ₀?")),
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
          tags$td(tags$b("Tabela krzyżowa")),
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

    div(class = "section-title", "Kiedy który paradygmat?"),

    div(class = "narrative",
      p(tags$b("Częstościowo jest wygodniejsze gdy:")),
      tags$ul(
        tags$li("Recenzenci/czasopismo oczekują p-wartości (wciąż standard w wielu dziedzinach)"),
        tags$li("Analiza jest rutynowa, bez silnych przekonań sprzed badania"),
        tags$li("Duża próba + prosta hipoteza (daje te same liczby)")
      ),
      p(tags$b("Bayesowsko jest wartościowe gdy:")),
      tags$ul(
        tags$li("Mała próba — regularyzacja priorem stabilizuje estymację"),
        tags$li("Chcesz powiedzieć „efektu nie ma‟ (BF < 1/3 pozwala)"),
        tags$li("Masz wiedzę sprzed badania, którą chcesz włączyć"),
        tags$li("Interesuje Cię bezpośrednio P(H | dane), nie P(dane | H₀)"),
        tags$li("Chcesz zadać pytanie o praktyczny próg: P(różnica > 2 jednostki)")
      )
    ),

    div(class = "callout-success",
      tags$b("Podsumowanie w jednym zdaniu: "),
      "częstościowa statystyka odpowiada na pytanie „jak zaskakujące są moje dane, gdyby H₀ było prawdą?‟,
       bayesowska — „jak prawdopodobne są różne wartości parametru, dając moje dane i prior?‟.
       To dwa różne pytania, oba legalne, często komplementarne."
    ),

    div(class = "section-title", "Skala Jeffreysa dla BF"),

    tags$table(class = "table table-bordered",
      tags$thead(
        tags$tr(
          tags$th("BF₁₀"),
          tags$th("Interpretacja (dowód za H₁)")
        )
      ),
      tags$tbody(
        tags$tr(tags$td("1 – 3"), tags$td("Anekdotyczny (słaby) — prawie brak dowodu")),
        tags$tr(tags$td("3 – 10"), tags$td("Umiarkowany")),
        tags$tr(tags$td("10 – 30"), tags$td("Silny")),
        tags$tr(tags$td("30 – 100"), tags$td("Bardzo silny")),
        tags$tr(tags$td("> 100"), tags$td("Ekstremalny"))
      )
    ),
    p(tags$em("Symetrycznie: BF₁₀ < 1 czytamy jako 1/BF₁₀ dowód za H₀.")),

    div(class = "chapter-transition",
      p("Przejdź do ćwiczeń — praktyczne zadania w Twoim kierunku."),
      actionButton("ch11_to_ch12",
                   "Ćwiczenia kierunkowe →",
                   class = "btn-primary btn-lg")
    )

  )) # column, fluidRow
)

ch11_server <- function(input, output, session) {
  # Brak reactivity - tylko statyczna treść
}
