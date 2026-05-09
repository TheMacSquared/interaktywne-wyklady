# ============================================================================
# ROZDZIAŁ 5: Czytanie wyników — sześć scenariuszy
# ============================================================================

ch5_ui <- lecture_chapter(
  id = "ch-cwiczenie",
  num = "05",
  title = "Czytanie wyników",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · KMNK",
      num = "05",
      title = "Czytanie wyników regresji.",
      lead = "Sześć scenariuszy z różnych branż — jeden na raz. Patrz na tabelę, wybierz interpretację, zobacz werdykt. Każdy scenariusz uczy innej pułapki, której łatwo nie zauważyć."
    ),

    lc_h2("ch5-jak", "Jak korzystać z tego ćwiczenia"),
    lc_p("Pod selectorem niżej znajdziesz sześć fikcyjnych raportów z regresji KMNK. Każdy ma swoją tabelę, swoje pytanie i cztery odpowiedzi do wyboru — jedna jest poprawna, trzy to typowe pułapki. Po wyborze pojawi się komentarz tłumaczący, dlaczego to działa tak, a nie inaczej."),
    lc_p("Najlepiej przejść je w kolejności — każdy zwiększa stopień trudności i pokazuje inny aspekt: jednostki, granicę α, niskie R² mimo istotności, ujemne nachylenie i endogeniczność."),

    figure_panel(
      label = "Ryc. 5.1",
      title = "Wybierz scenariusz",
      full_width = TRUE,
      fluidRow(
        column(
          12,
          selectInput(
            "ch5_scenariusz", NULL,
            choices = c(
              "1. Sprzedaż lodów ~ temperatura"           = "1",
              "2. Wynagrodzenie ~ staż"                   = "2",
              "3. Plon ~ nawóz (granica p ≈ 0,05)"        = "3",
              "4. Cena mieszkania ~ liczba pokoi"         = "4",
              "5. Bezrobocie ~ wzrost PKB"                = "5",
              "6. Reklama ~ przychód firmy (przyczynowość)" = "6"
            ),
            selected = "1",
            width = "100%"
          )
        )
      )
    ),

    figure_panel(
      label = "Tabela wyników",
      title = uiOutput("ch5_tytul"),
      full_width = TRUE,
      uiOutput("ch5_opis"),
      uiOutput("ch5_tabela"),
      uiOutput("ch5_meta")
    ),

    figure_panel(
      label = "Pytanie",
      title = uiOutput("ch5_pytanie_naglowek"),
      full_width = TRUE,
      uiOutput("ch5_radio"),
      uiOutput("ch5_feedback"),
      tags$br(),
      actionButton("ch5_pokaz", "Pokaż pełną interpretację",
                   class = "btn-outline-secondary"),
      uiOutput("ch5_pelna")
    ),

    lc_h2("ch5-postep", "Twój postęp"),
    uiOutput("ch5_postep"),

    inline_callout(
      label = "Wskazówka",
      color = "wskazowka",
      "Wracaj do tych scenariuszy przed kolokwium. Sześć liczb z każdej tabeli (b₀, SE₀, b₁, SE₁, R², SE reszt) plus opis biznesowy = pełen materiał do pisania własnego raportu."
    )
  )
)

ch5_server <- function(input, output, session) {

  # Stan: które scenariusze zaliczone, które już otwierano.
  ch5_zaliczone <- reactiveVal(integer(0))
  ch5_pokaz_state <- reactiveVal(FALSE)

  # Reset „pokaż interpretację” przy zmianie scenariusza
  observeEvent(input$ch5_scenariusz, {
    ch5_pokaz_state(FALSE)
  })

  observeEvent(input$ch5_pokaz, {
    ch5_pokaz_state(TRUE)
  })

  ch5_dane <- reactive({
    eco02_scenariusz_kmnk(as.integer(input$ch5_scenariusz))
  })

  output$ch5_tytul <- renderUI({
    s <- ch5_dane()
    if (is.null(s)) return(NULL)
    s$tytul
  })

  output$ch5_opis <- renderUI({
    s <- ch5_dane()
    if (is.null(s)) return(NULL)
    tags$p(em(s$opis))
  })

  output$ch5_tabela <- renderUI({
    s <- ch5_dane()
    if (is.null(s)) return(NULL)
    rows <- lapply(seq_len(nrow(s$tabela)), function(i) {
      tags$tr(
        tags$td(s$tabela$Wspolczynnik[i]),
        tags$td(s$tabela$Estymata[i]),
        tags$td(s$tabela$SE[i]),
        tags$td(s$tabela$t[i]),
        tags$td(s$tabela$p[i])
      )
    })
    tags$table(
      class = "table",
      tags$thead(tags$tr(
        tags$th("Współczynnik"),
        tags$th("Estymata"),
        tags$th("Błąd standardowy"),
        tags$th("Statystyka t"),
        tags$th("p-wartość")
      )),
      do.call(tags$tbody, rows)
    )
  })

  output$ch5_meta <- renderUI({
    s <- ch5_dane()
    if (is.null(s)) return(NULL)
    tags$p(strong(s$meta))
  })

  output$ch5_pytanie_naglowek <- renderUI({
    s <- ch5_dane()
    if (is.null(s)) return(NULL)
    s$pytanie
  })

  output$ch5_radio <- renderUI({
    s <- ch5_dane()
    if (is.null(s)) return(NULL)
    radioButtons(
      paste0("ch5_q_", input$ch5_scenariusz), NULL,
      choiceNames  = s$opcje,
      choiceValues = as.character(seq_along(s$opcje)),
      selected     = character(0)
    )
  })

  output$ch5_feedback <- renderUI({
    s <- ch5_dane()
    if (is.null(s)) return(NULL)
    odp <- input[[paste0("ch5_q_", input$ch5_scenariusz)]]
    if (is.null(odp) || !nzchar(odp)) return(NULL)

    idx <- as.integer(odp)
    poprawna <- s$poprawna
    wyjasnienie <- s$wyjasnienia[idx]

    if (idx == poprawna) {
      # Aktualizuj listę zaliczonych
      isolate({
        zal <- ch5_zaliczone()
        scen_idx <- as.integer(input$ch5_scenariusz)
        if (!scen_idx %in% zal) {
          ch5_zaliczone(c(zal, scen_idx))
        }
      })
      lc_feedback(
        type = "ok",
        strong("Dobrze! "),
        wyjasnienie
      )
    } else {
      lc_feedback(
        type = "warning",
        strong("Pułapka. "),
        wyjasnienie,
        tags$br(), tags$br(),
        em("Spróbuj jeszcze raz albo zerknij na pełną interpretację niżej.")
      )
    }
  })

  output$ch5_pelna <- renderUI({
    if (!ch5_pokaz_state()) return(NULL)
    s <- ch5_dane()
    if (is.null(s)) return(NULL)
    poprawna <- s$poprawna

    lc_feedback(
      type = "info",
      strong("Pełna interpretacja: "),
      tags$br(), tags$br(),
      tags$p(strong("Poprawna odpowiedź: "), s$opcje[poprawna]),
      tags$p(s$wyjasnienia[poprawna]),
      tags$br(),
      tags$p(strong("Dlaczego inne były pułapką:")),
      tags$ul(
        lapply(setdiff(seq_along(s$opcje), poprawna), function(i) {
          tags$li(strong(paste0("Opcja ", i, ": ")), s$wyjasnienia[i])
        })
      )
    )
  })

  output$ch5_postep <- renderUI({
    zal <- ch5_zaliczone()
    n_zal <- length(zal)
    procent <- round(100 * n_zal / 6)
    lc_stat_grid(
      lc_stat_box(
        label = paste0(n_zal, " z 6"),
        value = paste0(procent, "%"),
        caption = "scenariuszy zaliczonych",
        color = if (n_zal == 6) unname(upwr_cat["szalwia"]) else
                if (n_zal >= 3) upwr_accent else upwr_secondary
      ),
      lc_stat_box(
        label = "Pozostały",
        value = paste(setdiff(1:6, zal), collapse = ", "),
        caption = "numery scenariuszy do zaliczenia",
        color = unname(upwr_cat["niebo"])
      ),
      columns = 2
    )
  })
}
