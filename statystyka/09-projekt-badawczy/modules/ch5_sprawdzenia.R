ch5_ui <- lecture_chapter(id = "ch5", num = "5", title = "Pierwsze sprawdzenia", content = tagList(
  lc_chapter_hero(
    kicker = "Rozdział 05 · Proste testy",
    num = "05",
    title = "Pierwsze sprawdzenia w danych.",
    lead = "Cztery z pięciu tropów przechodzą pierwszy test. Żaden z nich nie
            odpowiada jeszcze na cel badania: test mówi tylko, czy trop ma
            oparcie w danych, a nie, skąd bierze się ocena kursu."
  ),

  lc_p("Konspekt z rozdziału 4 jest gotowy: mamy cel, pięć tropów,
    zmienne i plan interpretacji. Dopiero teraz dobieramy narzędzia
    statystyczne, bo dopiero teraz wiadomo, czego mają szukać. Cel badania
    pozostaje ten sam: ", tags$em(tr_goal)),

  lc_h2("sec-01", "Wiązka spotyka dane — trop po tropie"),

  lc_p("Narzędzie wynika z typu zmiennych, tak jak w drzewie decyzyjnym
    z rozdziału 11 wykładu 04. Atrakcyjność i odsetek odpowiedzi są ",
    gloss("zmienna ilościowa", "zmiennymi ilościowymi"), ", więc ich związek
    z oceną kursu opisuje ", gloss("korelacja"), " Pearsona z rozdziału 06
    wykładu 04. Płeć, status native speaker i status mniejszościowy dzielą
    kursy na dwie grupy, więc porównujemy oceny w grupach. Dla płci,
    gdzie obie grupy są duże, służy do tego ", gloss("test t"), " dla dwóch
    grup z rozdziału 08 wykładu 04. Dla statusu native speaker i statusu
    mniejszościowego jedna z grup jest mała (28 i 64 kursy), dlatego
    konspekt przewiduje ", gloss("test Manna-Whitneya"), " z wykładu 05,
    mniej wrażliwy na nierówne i skośne grupy."),

  lc_p("Każdy trop dostaje ten sam zestaw: wykres, ", gloss("statystyka opisowa", "statystyki opisowe"), " oceny
    kursu, miarę efektu, wynik testu i wstępny werdykt. Werdykt
    „wzmocniony” oznacza, że ", gloss("p-wartość"), " jest mniejsza niż
    0.05, a „osłabiony”, że nie jest. To umowny podział na potrzeby
    tablicy, a nie ocena tropu."),

  uiOutput("ch5_bundle_results"),

  lc_p("Atrakcyjność wiąże się z oceną kursu dodatnio: r = 0.19, z 95% ",
    gloss("przedział ufności", "przedziałem ufności"), " od 0.10 do 0.28
    i p < 0.001. Podobnie, nawet nieco silniej, wiąże się z nią odsetek
    odpowiedzi (r = 0.22, p < 0.001): kursy, w których ankietę wypełniła
    większa część zapisanych, mają przeciętnie wyższe oceny. Mężczyźni
    dostają średnio 4.07 punktu, kobiety 3.90, czyli o 0.17 punktu mniej
    (p = 0.001). Największą różnicę widać przy statusie native speaker:
    28 kursów prowadzonych przez osoby, dla których angielski nie jest
    językiem ojczystym, ma średnią 3.69, pozostałe 4.02. Kursy prowadzone
    przez osoby z grup mniejszościowych mają średnią niższą o 0.12 punktu,
    ale ta różnica nie jest istotna (p = 0.072)."),

  lc_p("Wyniki istotne nie są wynikami dużymi. Rozdział 10 wykładu 04
    rozdzielał p-wartość i ", gloss("wielkość efektu"), ", i tu ta różnica
    ma znaczenie. Przy 463 kursach nawet słaby związek daje małą
    p-wartość. Korelacja r = 0.19 oznacza, że atrakcyjność wyjaśnia 3.6%
    zmienności ocen. Różnica 0.17 punktu między kobietami i mężczyznami
    to mniej więcej 0.3 ", gloss("odchylenie standardowe", "odchylenia standardowego"), " oceny (SD = 0.55),
    a różnica 0.33 dla statusu native speaker to około 0.6 SD, tyle że
    liczona na grupie 28 kursów. Każdy z tych tropów coś mówi o ocenie
    kursu, ale żaden nie tłumaczy jej w większej części."),

  lc_p("Liczebności wymagają jeszcze jednej uwagi. ",
    gloss("jednostka obserwacji", "Jednostką obserwacji"), " jest kurs,
    a nie prowadzący: 463 kursy prowadziły 94 osoby, większość z nich
    więcej niż jeden kurs. Cechy prowadzącego, takie jak ocena
    atrakcyjności czy płeć, powtarzają się więc we wszystkich jego kursach.
    28 kursów osób, dla których angielski nie jest językiem ojczystym, to
    zajęcia zaledwie 7 prowadzących, a 64 kursy w grupie mniejszościowej —
    12 prowadzących. Wrócimy do tego w rozdziale 7."),

  lc_p("Werdykt przy tropie nie jest też wnioskiem o przyczynie. „Wzmocniony”
    znaczy tylko, że związek w danych istnieje i warto zadać kolejne
    pytanie: czy to ten trop, czy coś, co z nim współwystępuje.
    „Osłabiony” nie zamyka tematu. Może znaczyć, że efektu nie ma, ale też,
    że grupa jest za mała albo że prosty test pomija coś, co różnicuje
    porównywane kursy."),

  lc_h2("sec-02", "Tablica tropów po pierwszych testach"),

  lc_p("Wyniki pięciu testów trafiają do tablicy, którą w rozdziale 1
    zostawiliśmy pustą. Każdy wiersz to jeden trop: pytanie, narzędzie,
    miara efektu z p-wartością i wstępny werdykt."),

  figure_panel(label = "Ryc. 5.1", title = "Tablica tropów (po pierwszych testach)",
    tr_board_ui(reveal = tr_trop_order, show_verdict = TRUE)
  ),

  lc_p("Cztery tropy są wzmocnione, jeden osłabiony. Tablica wygląda na
    gotową odpowiedź, ale każdy wiersz powstał osobno, jakby pozostałych
    zmiennych nie było. Tymczasem te zmienne nie są od siebie niezależne:
    kobiety i mężczyźni mogą prowadzić inne kursy, a atrakcyjność może
    wiązać się z wiekiem i płcią. Zanim tablica powie coś o celu, trzeba
    sprawdzić, jak tropy na siebie zachodzą."),

  lc_chapter_next("06", "Wynik nie kończy badania",
    "Tablica jest pełna, ale tropy sprawdzane osobno mogą się nakładać.
     Następny rozdział sprawdza, które zmienne zakłócają główny związek.",
    "ch6")
  )
)

ch5_server <- function(input, output, session) {
  # Wykres jednego tropu — korelacja (ilościowe) albo boxplot (grupy).
  .trop_plot <- function(id) {
    tr <- tr_tropy[[id]]
    if (tr$method == "cor") {
      ggplot(tr_data, aes(x = .data[[tr$var]], y = eval)) +
        geom_point(color = proj_col_ref, alpha = 0.45, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = proj_col_ctrl,
                    fill = proj_col_ctrl, alpha = 0.12) +
        labs(x = unname(tr_labels[tr$var]), y = "Ocena kursu (eval)") +
        theme_upwr()
    } else {
      ggplot(tr_data, aes(x = .data[[tr$var]], y = eval, fill = .data[[tr$var]])) +
        geom_boxplot(alpha = 0.65, outlier.alpha = 0.25) +
        geom_jitter(width = 0.12, alpha = 0.16, size = 1) +
        scale_fill_manual(values = rep(c(proj_col_data, proj_col_hyp, proj_col_ctrl),
                                       length.out = length(unique(tr_data[[tr$var]])))) +
        labs(x = unname(tr_labels[tr$var]), y = "Ocena kursu (eval)") +
        theme_upwr() +
        theme(legend.position = "none")
    }
  }

  # Zarejestruj wykres + render karty wyniku dla każdego tropu.
  lapply(tr_trop_order, function(id) {
    zoom_plot_server(paste0("ch5_plot_", id), reactive(.trop_plot(id)))
  })

  output$ch5_bundle_results <- renderUI({
    blocks <- lapply(tr_trop_order, function(id) {
      tr  <- tr_tropy[[id]]
      row <- tr_board_row(id)
      fb_type <- if (row$supported) "warning" else "ok"

      desc <- tr_desc_table(id)
      first_col <- if (tr$method == "cor") "Zmienna" else "Grupa"
      desc$iqr <- paste0(lc_num(desc$q1, 2), "–", lc_num(desc$q3, 2))
      desc_tbl <- lc_table_split(desc,
        cols = list(
          lc_col("label", first_col, "row"),
          lc_col("mean", "Średnia", digits = 2),
          lc_col("sd", "SD", digits = 2),
          lc_col("median", "Mediana", digits = 2),
          lc_col("iqr", "Q1–Q3")
        ),
        groups = list(c("mean", "sd"), c("median", "iqr")),
        label = "Statystyki opisowe (eval)"
      )

      p_disp <- if (grepl("<", row$p_label)) paste0("p ", row$p_label)
                else paste0("p = ", row$p_label)
      effect_kind <- if (tr$method == "cor") "korelacja" else "różnica średnich"

      figure_panel(label = "Trop", title = tr$short,
        p(tags$strong("Pytanie:"), " ", tr$question),
        lc_plot(paste0("ch5_plot_", id), ratio = "1.9/1", max_height = "320px"),
        tags$p(tags$strong("Statystyki opisowe (eval):")),
        desc_tbl,
        tags$p(tags$strong(paste0("Miara efektu (", effect_kind, "):")), " ",
               row$effect),
               tags$p(tags$strong("Test:"), paste0(" ", tr$test_name, "; ", p_disp)),
        lc_status(
          tags$p(lc_verdict(tags$strong("Werdykt:"), type = fb_type), " ", row$verdict)
        )
      )
    })
    div(blocks)
  })
}
