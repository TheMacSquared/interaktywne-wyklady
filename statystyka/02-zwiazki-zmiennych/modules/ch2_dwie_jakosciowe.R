# ============================================================================
# CHAPTER 2: Dwie zmienne jakościowe — tabela krzyżowa
# ============================================================================

cross_choices <- c("Płeć" = "plec", "Kierunek" = "kierunek", "Grupa krwi" = "grupa_krwi")

ch2_ui <- list(
  id = "ch-dwie-jakosciowe", num = "02", title = "Dwie jakościowe",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 02 · Związki dwóch zmiennych",
      num    = "02",
      title  = "Dwie zmienne jakościowe.",
      lead   = "Płeć kierowcy i wynik kontroli drogowej to dwie kategorie
                przypisane tej samej osobie. Żeby sprawdzić, czy idą w parze,
                wystarczy je razem policzyć, ale liczyć trzeba uważnie."
    ),

    lc_p("Gdy obie zmienne są ", gloss("zmienna jakościowa", "jakościowe"), ",
      każdą obserwację opisuje para kategorii: kobieta i mandat, mężczyzna
      i pouczenie. Kategorii nie da się dodać ani uśrednić, więc znowu
      zaczynamy od liczenia, tak jak w tabeli częstości z wykładu 01.
      Tym razem liczymy jednak pary."),

    lc_h2("ch2-kontrola", "Kontrola drogowa"),

    figure_panel(
      label = "Ryc. 2.1",
      width_mode = "text",
      frames_widget("ch2_stop", "Kto dostaje mandat?",
        steps = c("Zatrzymani", "Mandat czy pouczenie", "Do tabeli", "Na sto osób"),
        config = list(kind = "stop", sex = as.character(stop_data$plec),
                      fine = stop_data$mandat,
                      aria = "Kierowcy zatrzymani przez patrol trafiają do tabeli płeć × wynik kontroli, a potem każdy wiersz staje się paskiem 100%"))
    ),

    lc_p("Tabela z kadru 3 to ", gloss("tabela kontyngencji", "tabela krzyżowa"),
      ", zwana też tabelą kontyngencji. Jej wiersze to kategorie jednej zmiennej,
      kolumny to kategorie drugiej, a w każdej komórce stoi liczba obserwacji,
      które mają obie cechy jednocześnie. Suma wszystkich komórek to liczba
      zatrzymanych, 70."),

    lc_p("Same liczebności podsuwały prosty wniosek: mężczyźni dostali 15 mandatów,
      kobiety 4, czyli prawie cztery razy mniej. Mężczyzn zatrzymano jednak ponad
      dwa razy więcej, więc więcej mandatów mieliby nawet wtedy, gdyby policjanci
      traktowali wszystkich jednakowo. Dlatego w kadrze 4 każdy wiersz liczymy
      osobno: ", gloss("procenty wierszowe", "procenty wierszowe"), " pokazują,
      jak wynik kontroli rozkłada się w grupie kobiet, a jak w grupie mężczyzn.
      Taki rozkład w jednej grupie to ", gloss("rozkład warunkowy"), ". Mandat
      dostało 18% zatrzymanych kobiet i 31% zatrzymanych mężczyzn."),

    lc_p("Gdyby wynik kontroli nie zależał od płci, oba paski wyglądałyby tak samo:
      w każdym mandat stanowiłby około 27%, tyle co wśród wszystkich 70 kierowców
      (19 z 70). Paski się różnią, więc w tej próbie płeć i mandat są związane.
      Różnica 13 punktów procentowych opisuje, jak silnie. Nie mówi jednak, skąd
      się bierze: mężczyźni mogą częściej przekraczać prędkość, a patrol może ich
      zatrzymywać w innych miejscach niż kobiety. Tabela opisuje związek,
      a nie jego przyczynę."),

    lc_note("Zasada", rule = TRUE,
      "Grupy różnej wielkości porównujemy procentami liczonymi w obrębie każdej
       grupy, a nie liczebnościami."
    ),

    lc_h2("ch2-krzyzowa", "Tabela krzyżowa w ankiecie"),

    lc_p("Ten sam sposób działa dla każdej pary zmiennych jakościowych. Panel
      poniżej buduje tabelę krzyżową z ankiety 200 studentów z wykładu 01:
      można na przykład zapytać, czy osoby różnej płci wybierają te same
      kierunki. Pod tabelą widać odczyt zaznaczonej komórki; kliknięcie innej
      komórki go zmienia."),

    figure_panel(
      label = "Ryc. 2.2",
      title = "Tabela krzyżowa",
      width_mode = "text",
      # Tabela, podpis, legenda i wykres liczą się w przeglądarce
      # (survey_charts.js, rycina „d02-xt”); kliknięcie komórki zmienia odczyt.
      lc_sf_figure("d02-xt", bare = TRUE, config = list(
          labels = as.list(setNames(names(cross_choices), cross_choices)),
          short = list(kierunek = list(Biologia = "Biol.", Ekonomia = "Ekon.",
                                       Informatyka = "Inf.", Psychologia = "Psych."))),
        body = tagList(
          lc_toolbar(
            lc_group("Wiersze × kolumny",
              tags$div(class = "lc-xt-pair",
                lc_sf_ctl(lc_select("ch2_cross_row", cross_choices, selected = "plec",
                                    aria_label = "Zmienna w wierszach")),
                tags$span(class = "lc-xt-times", `aria-hidden` = "true", "×"),
                lc_sf_ctl(lc_select("ch2_cross_col", cross_choices, selected = "kierunek",
                                    aria_label = "Zmienna w kolumnach")),
                lc_sf_action("swap", "ch2_cross_swap", icon = "shuffle", variant = "ghost",
                             aria_label = "Zamień wiersze z kolumnami")
              )
            ),
            tags$div(class = "lc-push",
              lc_sf_ctl(lc_segmented("ch2_cross_type", NULL,
                choices = c("Liczebności" = "counts",
                            "% wierszowe" = "row_pct",
                            "% kolumnowe" = "col_pct"),
                selected = "counts"
              ))
            )
          ),
          tags$div(`data-role` = "table"),
          tags$div(`data-role` = "caption", `aria-live` = "polite"),
          tags$div(class = "lc-xt-bar is-plain",
            tags$div(`data-role` = "legend"),
            lc_sf_ctl(lc_segmented("ch2_cross_chart", NULL,
              choices = c("Słupki" = "bar", "Mapa ciepła" = "heatmap"),
              selected = "bar"
            ))
          ),
          tags$div(style = "position:relative",
            tags$div(class = "lc-sf-zoom", lc_chart_zoom_button()),
            lc_sf_chart(0, ratio = "2.2/1", max_height = "440px")
          )
        )
      )
    ),

    lc_p("Same liczebności łatwo źle odczytać, tak jak w kontroli drogowej.
      W ankiecie jest 99 kobiet i 82 mężczyzn, więc kobiet jest więcej na każdym
      kierunku (na Informatyce 31 wobec 26) przede wszystkim dlatego, że jest ich
      więcej w całej próbie. Osób niebinarnych jest 13, a 6 osób nie chciało podać
      płci, więc w ich wierszach stoją pojedyncze liczby. Żeby porównać grupy
      różnej wielkości, przechodzimy na procenty."),

    lc_p("Procenty wierszowe pokazują, jak rozkładają się kierunki w obrębie
      każdej płci: Informatykę studiuje 31.3% kobiet i 31.7% mężczyzn,
      Psychologię 19.2% kobiet i 20.7% mężczyzn. Procenty kolumnowe pokazują
      skład płci na każdym kierunku: kobiety stanowią od 47.1% osób
      na Biologii do 51.7% na Informatyce. Rozkłady kierunków u kobiet
      i mężczyzn są do siebie podobne, więc w tej próbie wybór kierunku
      niewiele zależy od płci. W małych grupach procenty skaczą: Biologię
      studiuje 38.5% osób niebinarnych, ale to 5 osób na 13, a każda osoba
      więcej lub mniej przesuwa wynik o prawie 8 punktów procentowych. To, które
      procenty policzyć, zależy od pytania: zmienna, której grupy porównujemy,
      wyznacza kierunek procentowania."),

    lc_chapter_next(
      num       = "03",
      title     = "Ilościowa i jakościowa",
      lead      = "pensje absolwentów w trzech branżach.",
      target_id = "ch-grupy"
    )
  )
)


# --------------------------------------------------------------------------
# Chapter 2 Server
# --------------------------------------------------------------------------

ch2_server <- function(input, output, session) {

  scene_texts(input, output, "ch2_stop", list(
    tagList("Patrol drogówki przez jeden dzień zatrzymuje kierowców do kontroli.
      Zatrzymano 22 kobiety i 48 mężczyzn."),
    tagList("Każda kontrola kończy się mandatem albo pouczeniem. Mandat dostało
      19 osób, pouczenie 51."),
    tagList("Kierowcy trafiają do czterech przegródek: płeć w wierszach, wynik
      kontroli w kolumnach. Mężczyźni mają prawie cztery razy więcej mandatów
      niż kobiety. Ale też ponad dwa razy więcej ich zatrzymano."),
    tagList("Każdy wiersz staje się paskiem długości 100%. Mandat dostało 18% kobiet
      i 31% mężczyzn: różnica jest, ale dużo mniejsza, niż sugerowały liczby.")
  ))

  # Tabela krzyżowa z ankiety (Ryc. 2.2) liczy się w przeglądarce:
  # survey_charts.js, rycina „d02-xt”.
}
