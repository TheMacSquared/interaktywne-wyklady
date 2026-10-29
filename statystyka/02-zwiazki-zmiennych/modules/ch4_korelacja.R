# ============================================================================
# CHAPTER 4: Dwie zmienne ilościowe — wykres rozrzutu i korelacja Pearsona
# ============================================================================

# Wzór na r ze znacznikami (ten sam zapis co w wykładzie 05, rozdz. 06).
r_formula <- function() {
  lc_formula_annotated(
    "⟦1|r⟧ = \\frac{⟦2|\\sum⟧ (⟦3|x_i⟧ - \\bar{x})(⟦4|y_i⟧ - \\bar{y})}{\\sqrt{\\sum(x_i-\\bar{x})^2 \\cdot \\sum(y_i-\\bar{y})^2}}",
    notes  = list("korelacja z próby", "suma", "i-ta obserwacja x", "i-ta obserwacja y"),
    colors = c("accent", "bursztyn", "niebo", "wrzos"),
    above  = c(2, 3, 4)
  )
}

# Osoba z kadru 1: staż najbliższy 6 lat.
ch4_focus <- which.min(abs(tenure_bio$staz - 6))

ch4_ui <- list(
  id = "ch-korelacja", num = "04", title = "Dwie ilościowe",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Związki dwóch zmiennych",
      num    = "04",
      title  = "Korelacja.",
      lead   = "Dłuższy staż, wyższa pensja; cieplejszy dzień, więcej sprzedanych
                lodów. Związek dwóch zmiennych ilościowych najpierw się rysuje,
                a potem streszcza jedną liczbą: współczynnikiem korelacji."
    ),

    lc_p("Gdy obie zmienne są ilościowe, nie ma oczywistych grup do porównania.
      Staż może wynosić 5.9 albo 6.1 roku, a każda wartość zdarza się zwykle
      raz. Zamiast dzielić obserwacje na grupy, kładziemy obie liczby jednej
      osoby na płaszczyźnie."),

    lc_h2("ch4-staz", "Staż i pensja"),

    figure_panel(
      label = "Ryc. 4.1",
      width_mode = "text",
      frames_widget("ch4_tenure", "Czy pensja rośnie ze stażem?",
        steps = c("Jedna osoba", "Punkt", "Cała firma", "Krzyż średnich"),
        config = list(kind = "staz", x = tenure_bio$staz, y = tenure_bio$pensja,
                      focus = ch4_focus - 1L,
                      mx = mean(tenure_bio$staz), my = mean(tenure_bio$pensja),
                      r = cor(tenure_bio$staz, tenure_bio$pensja),
                      xlim = c(0, 16), ylim = c(6, 14),
                      aria = "Pracownik firmy biotechnologicznej staje się punktem na wykresie stażu i pensji; po dodaniu całej firmy linie średnich dzielą wykres na ćwiartki"))
    ),

    lc_p("Obraz z kadru 3 to ", gloss("wykres rozrzutu"), ": każdy punkt to jedna
      obserwacja, współrzędna pozioma to wartość pierwszej zmiennej, pionowa
      wartość drugiej. Chmura punktów wznosi się w prawo, więc osoby z dłuższym
      stażem zarabiają zwykle więcej. Punkty nie leżą jednak na linii: przy
      tym samym stażu pensje różnią się o kilka tysięcy złotych."),

    lc_p("Krzyż średnich z kadru 4 zamienia to wrażenie w rachunek. Średni staż
      wynosi 9.2 roku, średnia pensja 10.7 tys. zł. Punkt w prawej górnej ćwiartce
      to osoba powyżej średniej w obu zmiennych, w lewej dolnej poniżej w obu.
      Takich zgodnych punktów jest 28 z 40. Pozostałe 12 leży w ćwiartkach,
      w których jedna zmienna jest powyżej średniej, a druga poniżej. Przewaga
      zgodnych punktów oznacza związek dodatni: obie zmienne rosną razem."),

    lc_h2("ch4-pearson", "Współczynnik korelacji Pearsona"),

    lc_p("Jedną liczbą opisuje to współczynnik ",
      gloss("korelacja Pearsona", "korelacji Pearsona"), " \\(r\\):"),

    r_formula(),

    lc_p("Licznik sumuje iloczyny odchyleń od średnich, czyli liczy to samo,
      co krzyż średnich, tylko z wagami. Punkt z ćwiartki zgodnej dodaje do sumy
      wartość dodatnią, punkt z ćwiartki niezgodnej ujemną, a punkty daleko
      od średnich ważą więcej niż te blisko krzyża. Mianownik sprowadza wynik
      do skali niezależnej od jednostek, dlatego \\(r\\) zawsze leży między -1
      a +1 i nie zmienia się, gdy pensję zapiszemy w złotych zamiast w tysiącach
      złotych. Wartość +1 oznacza, że wszystkie punkty leżą dokładnie na
      rosnącej prostej, -1 że na malejącej, a 0 brak związku liniowego. Dla
      stażu i pensji w firmie biotechnologicznej \\(r = 0.66\\)."),

    lc_p("Znak \\(r\\) mówi o kierunku związku. Trzy panele poniżej pokazują
      po 90 punktów o korelacji -0.6, 0 i +0.6."),

    figure_panel(
      label = "Ryc. 4.2",
      title = "Kierunek korelacji",
      tags$img(src = "assets/correlation-direction.png",
               alt = "Trzy wykresy punktowe pokazujące korelację dodatnią, brak korelacji liniowej i korelację ujemną.",
               tabindex = "0", role = "button")
    ),

    lc_p("Przy \\(r = 0\\) prosta dopasowana do punktów jest pozioma: znajomość
      \\(x\\) nic nie mówi o tym, czy \\(y\\) wypadnie powyżej, czy poniżej
      średniej. Przy -0.6 i +0.6 trend widać wyraźnie, choć punkty wciąż
      rozpraszają się szeroko wokół prostej. Ten rozrzut opisuje druga
      informacja zawarta w \\(r\\)."),

    lc_p("Wartość bezwzględna \\(|r|\\) mówi o sile związku liniowego, czyli
      o tym, jak ciasno punkty skupiają się wokół prostej. Na wykresie poniżej
      trend jest stały, a zmienia się tylko rozrzut punktów."),

    figure_panel(
      label = "Ryc. 4.3",
      title = "Siła korelacji",
      tags$div(class = "lc-scat lc-strength", `data-config` = scatter_config(corr_strength),
        lc_toolbar(
          lc_group("Siła związku", tags$div(class = "lc-seg", role = "group",
            lapply(seq_along(corr_strength$labels), function(i) {
              tags$button(type = "button", `data-s-preset` = i - 1L,
                          `aria-pressed` = if (i - 1L == corr_strength$start) "true" else "false",
                          corr_strength$labels[[i]])
            }))),
          lc_readouts(
            lc_readout("r", tags$span(`data-role` = "r", "0.69"), color = "var(--upwr-accent)"),
            lc_readout("r²", tags$span(`data-role` = "r2", "48%"))
          )
        ),
        tags$div(class = "lc-scat-canvas",
          tags$canvas(`data-role` = "plot",
                      `aria-label` = "Wykres rozrzutu 90 punktów wokół prostej o nachyleniu 1; rozrzut wokół prostej można zmieniać")),
        lc_caption("Przeciągnij uchwyt na brzegu przerywanego pasa w górę lub w dół.")
      )
    ),

    lc_p("Przy \\(r = 0.31\\) trend ledwie widać w chmurze punktów, przy 0.69
      jest wyraźny, a przy 0.94 punkty prawie układają się w linię. Kwadrat
      współczynnika ma prostą interpretację: \\(r^2\\) to część zmienności
      \\(y\\), którą da się przypisać liniowemu związkowi z \\(x\\). Nazywa się go ",
      gloss("współczynnik determinacji", "współczynnikiem determinacji"),
      ". Dla \\(r = 0.31\\) to około 10%, dla 0.69 około 48%, a dla 0.94 około
      88%. W firmie biotechnologicznej staż odpowiada za około 44% zróżnicowania
      pensji. Wrócimy do tego w wykładzie 07 o regresji."),

    lc_p("Siła związku to jednak nie to samo co nachylenie prostej. Na wykresie
      poniżej nachylenie zmienia się od 0.1 do 2, a rozrzut rośnie razem z nim,
      więc punkty przy każdym nachyleniu równie ciasno trzymają się prostej."),

    figure_panel(
      label = "Ryc. 4.4",
      title = "r nie zależy od nachylenia",
      tags$div(class = "lc-scat lc-slope", `data-config` = scatter_config(corr_slope),
        lc_toolbar(
          lc_group("Gotowe nachylenia", tags$div(class = "lc-seg", role = "group",
            lapply(seq_along(corr_slope$slopes), function(i) {
              tags$button(type = "button", `data-s-preset` = i - 1L,
                          `aria-pressed` = if (i - 1L == corr_slope$start) "true" else "false",
                          format(corr_slope$slopes[[i]], nsmall = 1))
            }))),
          lc_readouts(
            lc_readout("Nachylenie", tags$span(`data-role` = "b", "0.80")),
            lc_readout("SD reszt", tags$span(`data-role` = "sd", "0.26")),
            lc_readout("r", tags$span(`data-role` = "r", "0.95"), color = "var(--upwr-accent)")
          )
        ),
        tags$div(class = "lc-scat-canvas",
          tags$canvas(`data-role` = "plot",
                      `aria-label` = "Wykres rozrzutu 90 punktów wokół prostej; nachylenie prostej można zmieniać, korelacja pozostaje 0.95")),
        lc_caption("Przeciągnij uchwyt na prostej w górę lub w dół.")
      )
    ),

    lc_p("Przy nachyleniu 0.4, 0.8, 1.6 i każdym innym \\(r\\) wynosi 0.95.
      Współczynnik korelacji nie mówi, o ile wzrośnie \\(y\\), gdy \\(x\\)
      wzrośnie o jednostkę, na przykład o ile złotych rośnie pensja z każdym
      rokiem stażu. Mówi tylko, jak ściśle punkty trzymają się prostej.
      Pytanie „o ile?” należy do regresji."),

    lc_note("Zasada", rule = TRUE,
      "Znak r mówi o kierunku związku liniowego, |r| o tym, jak ciasno punkty
       trzymają się prostej. O nachyleniu prostej r nie mówi nic."
    ),

    lc_chapter_next(
      num       = "05",
      title     = "Pułapki korelacji",
      lead      = "kiedy poprawnie policzone r wprowadza w błąd.",
      target_id = "ch-pulapki"
    )
  )
)

ch4_server <- function(input, output, session) {
  scene_texts(input, output, "ch4_tenure", list(
    tagList("Pracowniczka firmy biotechnologicznej ma dwie liczby: staż
      5.9 roku i pensję 10.3 tys. zł."),
    tagList("Te dwie liczby wyznaczają jeden punkt: staż na osi poziomej,
      pensja na pionowej."),
    tagList("Każda z 40 osób w firmie to jeden punkt. Chmura wznosi się w prawo."),
    tagList("Linie średnich dzielą wykres na ćwiartki. W ćwiartkach „oba powyżej”
      i „oba poniżej” leży 28 osób, w pozostałych 12.")
  ))
}
