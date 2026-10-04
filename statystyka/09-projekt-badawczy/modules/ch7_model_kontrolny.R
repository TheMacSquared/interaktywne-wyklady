ch7_ui <- lecture_chapter(id = "ch7", num = "7", title = "Model kontrolny", content = tagList(
  lc_chapter_hero(
    kicker = "Rozdział 07 · Stabilność efektu",
    num = "07",
    title = "Cała wiązka w jednym modelu.",
    lead = "Związek atrakcyjności z oceną kursu przetrwa uwzględnienie płci,
            wieku, typu kursu i odsetka odpowiedzi, i to prawie bez zmiany.
            Między prowadzącymi ocenianymi jako mało i bardzo atrakcyjni
            to około 0.3 punktu na pięciopunktowej skali ocen."
  ),

  lc_p("Rozdział 6 skończył się dwiema obserwacjami. Tropy zachodzą na
    siebie, a płeć, status native speaker i liczba punktów kursu wiążą się
    jednocześnie z atrakcyjnością i z oceną kursu. Testy z rozdziału 5
    nie odpowiedzą na pytanie, co zostaje z każdego tropu, gdy pozostałe
    uwzględnimy naraz, bo każdy z nich patrzył na jedną zmienną. Cel
    badania pozostaje ten sam: ", tags$em(tr_goal)),

  lc_h2("sec-01", "Model kontrolny"),

  lc_p(gloss("regresja wieloraka", "Regresja wieloraka"), " z rozdziału 03
    wykładu 06 pozwala zadać to pytanie wprost. ",
    gloss("współczynnik regresji", "Współczynnik"), " przy atrakcyjności
    mówi w niej, o ile średnio różnią się oceny kursów, których prowadzący
    różnią się oceną atrakcyjności o jedną jednostkę, a pozostałe zmienne
    w modelu mają takie same. Pozostałe zmienne pełnią rolę ",
    gloss("zmienna kontrolna", "zmiennych kontrolnych"), ". W najszerszym
    modelu tego rozdziału:"),

  lc_formula_box(withMathJax(
    "$$\\text{eval} = \\beta_0 + \\beta_1 \\, \\text{beauty} + \\beta_2 \\, \\text{płeć} + \\beta_3 \\, \\text{wiek} + \\ldots + \\beta_{10} \\, \\text{response rate} + \\varepsilon$$"
  )),

  lc_p("Zmienne kontrolne dokładamy warstwami, tak jak w kroku 4 wykładu
    08: najpierw sama atrakcyjność, potem cechy prowadzącego (płeć, wiek,
    mniejszość, native speaker, tenure), potem kontekst kursu (poziom,
    liczba punktów, liczba odpowiedzi), na końcu odsetek odpowiedzi. Tabela
    podaje współczynnik przy atrakcyjności, jego ", gloss("p-wartość"), " oraz dwie miary
    porównawcze z rozdziału 04 wykładu 06: ",
    gloss("skorygowany R²"), " i ", gloss("AIC"), ". Wykres pod tabelą
    pokazuje współczynnik w kolejnych modelach."),

  figure_panel(label = "Ryc. 7.1", title = "Seria modeli kontrolnych",
    uiOutput("ch7_models_table"),
    lc_plot("ch7_beta_plot", max_height = "280px")
  ),

  lc_p("Współczynnik przy atrakcyjności prawie się nie zmienia: 0.133 w modelu
    prostym, 0.136 po dodaniu cech prowadzącego, 0.156 po dodaniu kontekstu
    kursu i 0.134 w pełnym modelu, za każdym razem z p < 0.001. To inny
    obraz niż w wykładzie 08, gdzie współczynnik przy liczbie uczniów na
    nauczyciela po uwzględnieniu zamożności okręgów zmalał do około jednej
    czwartej. Tu zmienne kontrolne nie tłumaczą związku. Wzrost w modelu 3
    zgadza się z tym, co pokazał rozdział 6: kursy jednopunktowe łączą
    niższą atrakcyjność z wyższą oceną, więc dopiero ich uwzględnienie
    odsłania pełniejszy związek. Rozdział 6 przewidywał to samo dla płci
    i rzeczywiście: sama płeć, dodana do modelu z atrakcyjnością, podnosi
    współczynnik do 0.149. W modelu 2 wzrost jest mniejszy, bo razem
    z płcią wchodzą pozostałe cechy prowadzącego."),

  lc_p("Kolejne warstwy wyraźnie poprawiają model jako całość. Skorygowany
    R² rośnie od 0.034 przez 0.094 i 0.141 do 0.178, a AIC spada od 757
    przez 732 i 710 do 690. Nawet pełny model wyjaśnia jednak mniej niż
    jedną piątą zmienności ocen. Większość tego, co różnicuje oceny kursów,
    leży poza zmiennymi, które mamy."),

  lc_p("Panel poniżej pozwala zbudować własny model kontrolny. Startuje od
    zestawu zmiennych zbliżonego do konspektu: płeć, wiek, native speaker,
    poziom kursu, liczba punktów i odsetek odpowiedzi. Tabela podaje dla
    każdego ", gloss("predyktor", "predyktora"), " współczynnik, ",
    gloss("błąd standardowy"), " i p-wartość. Wiersze predyktorów istotnych
    na poziomie 0.05 są wyróżnione. Wykres pokazuje współczynniki z 95% ",
    gloss("przedział ufności", "przedziałami ufności"), "."),

  figure_panel(label = "Ryc. 7.2", title = "Własny model kontrolny",
    lc_toolbar(
      checkboxGroupInput("ch7_vars", "Dodaj kontrole", inline = TRUE,
          choices = c(
            "Płeć" = "gender",
            "Wiek" = "age",
            "Mniejszość" = "minority",
            "Native speaker" = "native",
            "Tenure track" = "tenure",
            "Poziom kursu" = "division",
            "Credits" = "credits",
            "Liczba odpowiedzi" = "students",
            "Response rate" = "response.rate"
          ),
          selected = c("gender", "age", "native", "division", "credits", "response.rate")
            ),
            lc_readouts(uiOutput("ch7_custom_metrics"))
          ),
          uiOutput("ch7_custom_coefs"),
          lc_plot("ch7_custom_coef_plot", max_height = "260px")
  ),

  lc_p("W modelu startowym współczynnik przy atrakcyjności wynosi 0.140
    (p < 0.001), skorygowany R² 0.167, a AIC 694. Obok atrakcyjności istotne
    są płeć (mężczyźni o 0.21 punktu wyżej), status native speaker (0.33),
    kurs jednopunktowy (0.51) i odsetek odpowiedzi (0.006 punktu na punkt
    procentowy, czyli około 0.06 na każde 10 punktów procentowych). Wiek
    i poziom kursu nie wnoszą wiele. Wiek nie przejmuje więc związku
    atrakcyjności z oceną, o co pytał koniec rozdziału 6."),

  lc_p("Jeden wynik zmienia się wyraźnie względem rozdziału 5. W pełnym
    modelu 4 współczynnik przy mniejszości wynosi -0.20 i jest istotny
    (p = 0.011), choć w prostym porównaniu różnica wynosiła -0.12 i nie
    była istotna. Trop osłabiony wraca więc po uwzględnieniu innych cech.
    To dobra ilustracja, że werdykt z pierwszego testu nie zamyka tematu.
    Nie jest to jednak rozstrzygnięcie: grupa mniejszościowa to 12
    prowadzących, a wynik zależy od tego, jakie zmienne są w modelu."),

  lc_p("Skalę efektu atrakcyjności najłatwiej ocenić w jednostkach oceny.
    Ocena atrakcyjności jest na skali o średniej 0 i ", gloss("odchylenie standardowe", "odchyleniu
    standardowym"), " 0.79. Prowadzący z 10% najniżej ocenianych mają wartość
    około -0.98, z 10% najwyżej ocenianych około 1.15. W pełnym modelu
    (współczynnik 0.134, 95% przedział ufności od 0.071 do 0.197) taka
    różnica odpowiada ocenie kursu wyższej średnio o 0.29 punktu,
    z przedziałem od 0.15 do 0.42. To mniej więcej połowa odchylenia
    standardowego ocen kursów (0.55) i więcej niż różnica między kobietami
    i mężczyznami w tym samym modelu (0.20)."),

  lc_p("Te przedziały są jednak zbyt wąskie. Testy i przedziały z regresji
    zakładają ", gloss("niezależność obserwacji"), ", o której mówił rozdział
    01 wykładu 07. Tymczasem 463 kursy prowadziły 94 osoby, a ocena
    atrakcyjności jest jedna na osobę i powtarza się we wszystkich jej
    kursach. Informacji o związku atrakcyjności z oceną jest więc mniej,
    niż sugeruje liczba wierszy. Prawdziwa niepewność jest większa niż
    w tabeli, choć przy p < 0.001 trudno oczekiwać, że związek zniknie."),

  lc_h2("sec-02", "Czego model nie rozstrzygnie"),

  lc_p("Model nie zastępuje sformułowania pytania. Jego wynik da się
    zinterpretować dzięki temu, co powstało wcześniej: celowi, wiązce ",
    gloss("hipoteza badawcza", "hipotez"), ", alternatywnym wyjaśnieniom
    i opisowi pomiaru z rozdziału 3. Bez nich współczynnik 0.134 byłby
    tylko liczbą."),

  lc_p("Skoro efekt atrakcyjności przetrwał kontrolę, naturalne jest kolejne
    pytanie: czy atrakcyjność powoduje wyższe oceny, czy tylko z nimi
    współwystępuje? ", gloss("dane obserwacyjne", "Dane obserwacyjne"),
    " na to nie odpowiedzą. Model kontroluje tylko zmienne, które są
    w zbiorze. Prowadzący oceniani jako atrakcyjniejsi mogą różnić się
    czymś, czego nie zmierzono, na przykład pewnością siebie albo stylem
    prowadzenia, a to mogłoby tłumaczyć część związku. Rozstrzygnięcie ",
    gloss("przyczynowość", "przyczynowości"), " wymagałoby innego badania,
    w którym ocenę atrakcyjności zmienia się niezależnie od reszty, na
    przykład tych samych nagranych zajęć ocenianych z różnymi zdjęciami
    prowadzącego. To granica, której ten zbiór nie przekroczy, i trzeba
    ją zapisać we wniosku."),

  lc_chapter_next("08", "Od konspektu do wniosku",
    "Wracamy do konspektu i dopisujemy, co wyniki zmieniły w interpretacji celu.",
    "ch8")
  )
)

ch7_server <- function(input, output, session) {
  # Seria modeli liczona od razu — to materiał do omówienia z projekcji,
  # nie nagroda za kliknięcie.
  model_series <- reactive({
    models <- list(
      list(label = "1: beauty", model = lm(eval ~ beauty, data = tr_data)),
      list(label = "2: + cechy osoby", model = lm(eval ~ beauty + gender + age + minority + native + tenure, data = tr_data)),
      list(label = "3: + kontekst kursu", model = lm(eval ~ beauty + gender + age + minority + native + tenure + division + credits + students, data = tr_data)),
      list(label = "4: + response rate", model = lm(eval ~ beauty + gender + age + minority + native + tenure + division + credits + students + response.rate, data = tr_data))
    )
    tr_model_table(models)
  })

  output$ch7_models_table <- renderUI({
    df <- model_series()
    df$p_label <- lc_pval(df$p_beauty)
    lc_table_split(df,
      cols = list(
        lc_col("model", "Model", "row"),
        lc_col("beta_beauty", "β beauty", digits = 3),
        lc_col("p_label", "p"),
        lc_col("adj_r2", "adj.R²", digits = 3),
        lc_col("aic", "AIC")
      ),
      groups = list(c("beta_beauty", "p_label"), c("adj_r2", "aic")),
      label = "Seria modeli kontrolnych"
    )
  })

  zoom_plot_server("ch7_beta_plot", reactive({
    df <- model_series()
    df$model <- factor(df$model, levels = df$model)
    ggplot(df, aes(x = model, y = beta_beauty, fill = p_beauty < 0.05)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = 0, color = proj_col_ref) +
      scale_fill_manual(values = c("TRUE" = proj_col_ctrl, "FALSE" = proj_col_ref),
                        guide = "none") +
      labs(x = NULL, y = "Współczynnik przy beauty") +
      theme_upwr() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }))

  custom_model <- reactive({
    vars <- input$ch7_vars
    rhs <- paste(c("beauty", vars), collapse = " + ")
    lm(as.formula(paste("eval ~", rhs)), data = tr_data)
  })

  output$ch7_custom_metrics <- renderUI({
    m <- custom_model()
    g <- broom::glance(m)
    coefs <- broom::tidy(m)
    p_beauty <- coefs$p.value[coefs$term == "beauty"]
    p_beauty_label <- if (is.na(p_beauty)) "—"
                      else if (p_beauty < 0.001) "< 0.001"
                      else sprintf("%.3f", p_beauty)
    tagList(
      lc_readout("R² skor.", lc_fmt(g$adj.r.squared, 3), color = proj_col_ctrl),
      lc_readout("AIC", lc_fmt(AIC(m), 0), color = proj_col_warn),
      lc_readout("β beauty", lc_fmt(coefs$estimate[coefs$term == "beauty"], 3), color = proj_col_hyp),
      lc_readout("p beauty", p_beauty_label, color = proj_col_data)
    )
  })

  output$ch7_custom_coefs <- renderUI({
    coefs <- broom::tidy(custom_model())
    coefs <- coefs[coefs$term != "(Intercept)", ]
    significant <- !is.na(coefs$p.value) & coefs$p.value < 0.05
    lc_table(
      data.frame(
        term = tr_label_term(coefs$term),
        estimate = coefs$estimate,
        se = coefs$std.error,
        p = lc_pval(coefs$p.value),
        stringsAsFactors = FALSE
      ),
      cols = list(
        lc_col("term", "Predyktor", "row"),
        lc_col("estimate", "β", digits = 3),
        lc_col("se", "SE", digits = 3),
        lc_col("p", "p")
      ),
      # Istotne predyktory (p < 0.05) wyróżnione jak dotąd całym wierszem.
      row_class = lapply(significant, function(s) if (s) "is-best")
    )
  })

  zoom_plot_server("ch7_custom_coef_plot", reactive({
    coefs <- broom::tidy(custom_model(), conf.int = TRUE)
    coefs <- coefs[coefs$term != "(Intercept)", ]
    coefs$label <- tr_label_term(coefs$term)
    coefs$label <- factor(coefs$label, levels = rev(coefs$label))
    ggplot(coefs, aes(x = estimate, y = label)) +
      geom_vline(xintercept = 0, color = proj_col_ref, linetype = "dashed") +
      geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2,
                     color = proj_col_ref, orientation = "y") +
      geom_point(color = proj_col_hyp, size = 2.5) +
      labs(x = "Współczynnik z 95% CI", y = NULL) +
      theme_upwr()
  }))
}
