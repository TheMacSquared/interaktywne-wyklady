# ============================================================================
# CHAPTER 4: Dobór próby
# ============================================================================

ch4_ui <- list(
  id    = "ch-dobor",
  num   = "04",
  title = "Dobór próby",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Dane i populacja",
      num    = "04",
      title  = "Duża próba nie naprawi złego losowania.",
      lead   = "W 1936 roku amerykański tygodnik zebrał ponad dwa miliony
                odpowiedzi i pomylił się w prognozie wyborów o kilkanaście
                punktów procentowych. Sposób, w jaki jednostki trafiają
                do próby, nazywamy doborem próby. Od niego zależy, czy
                statystyka trafia w parametr choćby średnio."
    ),

    lc_p("W poprzednim rozdziale każda osoba z listy miała tę samą szansę
      trafienia do próby. Wartości p̂ rozrzucały się wtedy wokół p,
      a większa próba zawężała ten rozrzut. W prawdziwych badaniach próby
      często powstają inaczej: ankietę wypełnia, kto chce, badacz pyta
      tych, których łatwo zastać, a do bazy trafiają tylko klienci, którzy
      wrócili. Ten rozdział porównuje trzy sposoby doboru próby."),

    lc_h2("ch4-sposoby", "Trzy sposoby doboru"),

    lc_h3("Losowanie proste", num = "1"),

    lc_p(gloss("losowanie proste", "Losowanie proste"), " to sposób,
      który znamy z poprzednich rozdziałów: z operatu losujemy n jednostek
      tak, że każda ma tę samą szansę trafienia do próby. Odpowiada to
      wyciąganiu losów z urny, w której każda osoba z listy ma jeden los."),

    lc_h3("Losowanie warstwowe", num = "2"),

    lc_p("W ", gloss("losowanie warstwowe", "losowaniu warstwowym"), " najpierw
      dzielimy populację na rozłączne grupy, zwane warstwami, a potem losujemy
      osobno w każdej z nich. Na wydziale naturalnymi warstwami są lata
      studiów. Jeśli pierwszy rok to 25% wydziału, to także 25% próby
      losujemy spośród pierwszego roku. Dzięki temu proporcje warstw
      w próbie zgadzają się z populacją co do osoby, a nie tylko średnio."),

    lc_h3("Próba wygodna", num = "3"),

    lc_p(gloss("próba wygodna", "Próba wygodna"), " to próba złożona z tych,
      do których najłatwiej dotrzeć. Wyobraźmy sobie, że ankietę o czasie
      dojazdu rozdajemy w stołówce przy akademiku. Ankietę wypełniają też
      osoby spoza akademika, ale mieszkańcy akademika bywają w stołówce
      znacznie częściej. W naszej symulacji mają pięć razy większą szansę
      trafienia do próby niż pozostali. Nikt tu nie oszukuje: to po prostu
      najtańszy sposób zebrania odpowiedzi."),

    lc_h2("ch4-porownanie", "Porównanie trzech sposobów"),

    lc_p("Szukamy parametru μ, czyli średniego czasu dojazdu wszystkich
      studentów wydziału. Panel losuje próby trzema sposobami naraz,
      z tą samą liczebnością n, i dla każdej próby zaznacza jej średnią x̄.
      Każdy wiersz wykresu to jeden sposób doboru, a pionowa linia to μ."),

    figure_panel(
      label = "Ryc. 4.1",
      title = "Średni czas dojazdu w próbach dobranych trzema sposobami",
      width_mode = "wide",
      lc_toolbar(
        lc_slider("ch4_n", "Liczebność próby (n)", 20, 400, 50, 10),
        lc_action_group(label = "Losuj próby",
          ch4_draw_1 = "+1", ch4_draw_20 = "+20", ch4_draw_100 = "+100"),
        lc_action("ch4_reset", icon = "reset", variant = "ghost",
                  aria_label = "Wyczyść próby"),
        lc_readouts(uiOutput("ch4_reads"))
      ),
      conditionalPanel("!output.ch4_has_draws",
        lc_empty("Wylosuj próby, żeby porównać sposoby doboru")),
      conditionalPanel("output.ch4_has_draws",
        lc_plot("ch4_methods_plot", ratio = "2.2/1"),
        uiOutput("ch4_caption")
      )
    ),

    lc_p("Prawdziwy średni czas dojazdu wynosi μ = ", lc_fmt(pop_mu, 1), " min.
      Średnie z prób prostych i warstwowych rozrzucają się po obu stronach
      tej wartości, a ich przeciętna leży tuż przy μ. Średnie z prób wygodnych
      skupiają się wokół około ", lc_fmt(pop_convenience_mu, 0), " min,
      czyli o mniej więcej ", lc_fmt(pop_mu - pop_convenience_mu, 0), " minut
      za nisko. Przyczyna jest prosta: mieszkańcy akademika dojeżdżają
      średnio ", lc_fmt(mean(faculty$dojazd[faculty$akademik]), 0), " minut,
      pozostali ", paste0(lc_fmt(mean(faculty$dojazd[!faculty$akademik]), 0),
      ", a w próbie wygodnej mieszkańców akademika jest znacznie więcej niż ",
      lc_fmt(100 * pop_share_akademik, 0), "%, jakie stanowią na wydziale.")),

    lc_p("Najważniejsze dzieje się po zwiększeniu n. Przy n = 400 średnie
      z prób prostych trzymają się w pasie około ±2 min wokół μ, a nie ±9 min
      jak przy n = 20. Średnie z prób wygodnych też się zwężają, ale wokół
      złej wartości: wszystkie lądują około 8 minut na lewo od μ i żadna
      nie trafia. Większa próba zmniejsza ",
      gloss("zmienność próbkowa", "zmienność próbkową"), ", ale nie usuwa ",
      gloss("obciążenie", "obciążenia"), ", czyli systematycznego błędu
      w jedną stronę."),

    lc_note("Zasada", rule = TRUE,
      "Losowanie zabezpiecza przed obciążeniem, a duże n zmniejsza
       zmienność próbkową. Jedno nie zastąpi drugiego."
    ),

    lc_p("Losowanie warstwowe dało tu prawie ten sam rozrzut co proste,
      bo czas dojazdu niewiele zależy od roku studiów. Warstwy pomagają
      tym bardziej, im silniej różnią się badaną cechą. Odsetek pracujących
      rośnie z roku na rok, od około 18% na pierwszym do ponad 60% na piątym,
      więc warstwy według roku zwężają rozrzut p̂, ale też tylko trochę,
      o kilka procent. Duży zysk dają warstwy, które dzielą populację
      na grupy bardzo do siebie niepodobne, na przykład gospodarstwa rolne
      według wielkości. Główną zaletą warstw jest jednak co innego: żadna
      grupa nie zostanie przypadkiem pominięta ani nadreprezentowana."),

    lc_h2("ch4-digest", "Dwa miliony ankiet i zła prognoza"),

    lc_p("Tygodnik Literary Digest przed wyborami prezydenckimi w USA w 1936
      roku rozesłał około 10 milionów ankiet do swoich czytelników, właścicieli
      telefonów i samochodów. Wróciło ponad 2 miliony odpowiedzi.
      Prognoza dawała wyraźne zwycięstwo Alfowi Landonowi. Wybory wygrał
      Franklin Roosevelt, zdobywając około 61% głosów. W tym samym czasie
      George Gallup na podstawie kilkudziesięciu tysięcy starannie dobranych
      wywiadów wskazał właściwego zwycięzcę."),

    lc_p("Do błędu Literary Digest przyczyniły się dwa mechanizmy, które
      dobrze znamy z panelu. Po pierwsze, operat: w czasach Wielkiego
      Kryzysu telefon i samochód mieli przede wszystkim zamożniejsi wyborcy,
      częściej głosujący na Landona. Po drugie, samodzielny wybór: ankietę
      odesłała tylko część adresatów, a ci, którzy chcieli wyrazić
      niezadowolenie z rządu, odpowiadali chętniej. Dwa miliony odpowiedzi
      dały bardzo małą zmienność próbkową wokół złej wartości."),

    lc_warn("Pułapka",
      "Duża liczba odpowiedzi nie świadczy o jakości próby. Ankieta
       internetowa z dziesięcioma tysiącami odpowiedzi może być gorsza
       od losowej próby tysiąca osób, jeśli o udziale decydowali sami
       odpowiadający."
    ),

    lc_p("Wykład 07 wraca do tego tematu i pokazuje inne sposoby, w jakie dane
      mogą nie odpowiadać na pytanie. Na razie mamy wszystkie pojęcia
      potrzebne do zobaczenia, jak układa się cały kurs."),

    lc_chapter_next(
      num       = "05",
      title     = "Opis i wnioskowanie",
      lead      = "dwa zadania statystyki i mapa kursu",
      target_id = "ch-opis-wnioskowanie"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  empty_draws <- function() {
    data.frame(method = character(0), xbar = numeric(0), stringsAsFactors = FALSE)
  }
  ch4_draws <- reactiveVal(empty_draws())

  draw_methods <- function(k) {
    n <- input$ch4_n %||% 50
    new <- do.call(rbind, lapply(names(pop_method_cols), function(m) {
      data.frame(
        method = m,
        xbar = replicate(k, mean(faculty$dojazd[draw_sample_ids(n, m)])),
        stringsAsFactors = FALSE
      )
    }))
    ch4_draws(rbind(ch4_draws(), new))
  }

  observeEvent(input$ch4_draw_1, draw_methods(1))
  observeEvent(input$ch4_draw_20, draw_methods(20))
  observeEvent(input$ch4_draw_100, draw_methods(100))
  observeEvent(input$ch4_reset, ch4_draws(empty_draws()))
  observeEvent(input$ch4_n, ch4_draws(empty_draws()), ignoreInit = TRUE)

  output$ch4_has_draws <- reactive(nrow(ch4_draws()) > 0)
  outputOptions(output, "ch4_has_draws", suspendWhenHidden = FALSE)

  output$ch4_reads <- renderUI({
    d <- ch4_draws()
    tagList(
      lc_readout("prób każdego rodzaju", nrow(d) / length(pop_method_cols)),
      lc_readout("μ (min)", lc_fmt(pop_mu, 1), color = col_param, swatch = TRUE)
    )
  })

  output$ch4_caption <- renderUI({
    d <- ch4_draws()
    req(nrow(d) > 0)
    m <- tapply(d$xbar, d$method, mean)
    lc_caption(sprintf(
      "Przeciętna x̄: prosta %s min, warstwowa %s min, wygodna %s min (μ = %s min).",
      lc_fmt(m[["Prosta"]], 1), lc_fmt(m[["Warstwowa"]], 1),
      lc_fmt(m[["Wygodna"]], 1), lc_fmt(pop_mu, 1)
    ))
  })

  zoom_plot_server("ch4_methods_plot", reactive({
    d <- ch4_draws()
    req(nrow(d) > 0)
    d$method <- factor(d$method, levels = rev(names(pop_method_cols)))
    means <- aggregate(xbar ~ method, data = d, FUN = mean)
    ggplot(d, aes(xbar, method, color = method)) +
      geom_vline(xintercept = pop_mu, color = col_param, linewidth = 1.1,
                 linetype = "dashed") +
      geom_point(position = position_jitter(height = 0.18, width = 0, seed = 1),
                 alpha = if (nrow(d) > 150) 0.4 else 0.75, size = 2) +
      geom_point(data = means, shape = 124, size = 12, color = "black") +
      annotate("text", x = pop_mu, y = 3.45, label = "μ", color = col_param,
               fontface = "bold", size = 5, hjust = -0.4) +
      scale_color_manual(values = pop_method_cols, guide = "none") +
      scale_x_continuous(limits = c(5, 55), breaks = seq(5, 55, 5)) +
      labs(x = "Średni czas dojazdu w próbie, x̄ (min)", y = NULL)
  }), alt = "Średnie z prób prostych, warstwowych i wygodnych na tle μ")
}
