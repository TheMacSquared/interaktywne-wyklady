# ============================================================================
# ROZDZIAŁ 3: Typologia zadań programowania liniowego
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-typologia",
  num = "03",
  title = "Typologia PL",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Optymalizacja",
      num = "03",
      title = "Trzy klasyczne rodziny zadań PL.",
      lead = "Schemat z piekarni nie jest zarezerwowany dla produkcji chleba. Te same zmienne decyzyjne, funkcje celu i ograniczenia wracają w transporcie, dietetyce, finansach. Poznajmy najczęstsze typy zadań."
    ),

    lc_h2("ch3-po-co", "Po co osobny rozdział o typach?"),
    lc_p("Programowanie liniowe to nie jeden problem, tylko cała rodzina problemów dzielących wspólny formalizm. Kiedy rozumiesz schemat, każde nowe zadanie — wycena leasingu, planowanie produkcji rolnej, optymalizacja portfela inwestycyjnego — sprowadza się do tej samej procedury: wskaż zmienne, zapisz cel, wypisz ograniczenia."),
    lc_p("W tym rozdziale pokazujemy trzy najczęściej spotykane typy. To nie są wszystkie warianty PL, ale one wystarczą jako rama do późniejszej rozmowy o simpleksie i dualizmie."),

    lc_h2("ch3-produkcja", "1. Zadanie produkcyjne (mix produktów)"),
    lc_p("To rodzina najbliżej naszej piekarni. Firma decyduje, ile czego wyprodukować, by zmaksymalizować zysk, mając ograniczone zasoby (surowce, czas pracy, moce maszyn)."),
    figure_panel(
      label = "Tabela 3.1",
      title = "Typowy zadanie produkcyjne",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Element"),
          tags$th("Co oznacza")
        )),
        tags$tbody(
          tags$tr(tags$td("Zmienne x_j"), tags$td("ile produktu j wytworzyć")),
          tags$tr(tags$td("Funkcja celu"), tags$td("max — suma marż jednostkowych × ilości")),
          tags$tr(tags$td("Ograniczenia"), tags$td("zasoby (mąka, czas pieca, ludzie) ≤ dostępne wielkości")),
          tags$tr(tags$td("Sektor"), tags$td("przemysł, rolnictwo, gastronomia"))
        )
      )
    ),
    lc_p(strong("Przykład:"), " producent okien decyduje, ile typów A, B, C wyprodukować w tygodniu, mając 200 m² szkła, 50 godzin pracy i 30 kg uszczelek. Każdy typ daje inną marżę, zużywa inną mieszankę zasobów."),

    lc_h2("ch3-transport", "2. Zadanie transportowe"),
    lc_p("Zupełnie inny typ problemu — zamiast maksymalizować zysk, minimalizujemy koszt dostawy. Mamy kilka magazynów (źródeł) i kilka odbiorców, każdy z konkretną podażą i popytem. Pytanie: ile jednostek towaru wysłać z którego magazynu do którego klienta, by koszt transportu był jak najmniejszy?"),
    figure_panel(
      label = "Tabela 3.2",
      title = "Zadanie transportowe",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Element"),
          tags$th("Co oznacza")
        )),
        tags$tbody(
          tags$tr(tags$td("Zmienne x_ij"), tags$td("ile jednostek z magazynu i do odbiorcy j")),
          tags$tr(tags$td("Funkcja celu"), tags$td("min — suma kosztów × ilości na każdym połączeniu")),
          tags$tr(tags$td("Ograniczenia podaży"), tags$td("z każdego magazynu wysyłamy nie więcej, niż mamy")),
          tags$tr(tags$td("Ograniczenia popytu"), tags$td("każdy odbiorca dostaje tyle, ile potrzebuje")),
          tags$tr(tags$td("Sektor"), tags$td("logistyka, dystrybucja energii, sieć handlowa"))
        )
      )
    ),
    lc_p(strong("Przykład:"), " sieć trzech hurtowni dostarcza zboże do pięciu młynów. Koszty kilometrowe i przepustowości są znane. Cel: harmonogram dostaw minimalizujący sumaryczny koszt logistyki, przy zaspokojeniu popytu młynów."),

    lc_h2("ch3-dieta", "3. Zadanie dietetyczne (problem mieszanki)"),
    lc_p("Klasyk z lat 40. — wojsko USA musiało zapewnić żołnierzom posiłki o określonej wartości odżywczej przy minimalnym koszcie. Ten sam wzorzec wraca dziś w rolnictwie (mieszanki paszowe), produkcji (paliwa, stopy) i finansach (portfele inwestycyjne)."),
    figure_panel(
      label = "Tabela 3.3",
      title = "Zadanie mieszanki",
      tags$table(
        class = "table",
        tags$thead(tags$tr(
          tags$th("Element"),
          tags$th("Co oznacza")
        )),
        tags$tbody(
          tags$tr(tags$td("Zmienne x_j"), tags$td("ile składnika j zużyć w mieszance")),
          tags$tr(tags$td("Funkcja celu"), tags$td("min — suma kosztów jednostkowych × ilości")),
          tags$tr(tags$td("Ograniczenia"), tags$td("każda norma odżywcza (białko, tłuszcz, kalorie) musi być spełniona ≥")),
          tags$tr(tags$td("Sektor"), tags$td("rolnictwo, hutnictwo, finanse (Markowitz)"))
        )
      )
    ),
    lc_p(strong("Przykład:"), " hodowca bydła komponuje paszę z czterech składników (kukurydza, soja, otręby, premix), tak by spełnić normy białka, energii, włókna i wapnia, przy minimalnym koszcie kilograma mieszanki."),

    lc_h2("ch3-wspolne", "Co je łączy?"),
    lc_p("Mimo różnych dziedzin wszystkie trzy typy mają tę samą strukturę matematyczną: liniowa funkcja celu, liniowe ograniczenia, zmienne decyzyjne nieujemne. Dlatego ten sam algorytm — simpleks, do którego dojdziemy w wykładzie 07 — rozwiązuje je wszystkie."),
    figure_panel(
      label = "Schemat",
      title = "Wspólny szkielet zadania PL",
      tags$ol(
        tags$li(strong("Zmienne decyzyjne"), " — to, co wybieramy. Zawsze nieujemne (x ≥ 0)."),
        tags$li(strong("Funkcja celu"), " — max (zysk, użyteczność) lub min (koszt, ryzyko)."),
        tags$li(strong("Ograniczenia"), " — zasoby, normy, popyty. Zawsze liniowe."),
        tags$li(strong("Postać standardowa"), " — wszystkie nierówności zamieniamy na równości przez dodanie zmiennych dopełniających.")
      )
    ),

    inline_callout(
      label = "Zapamiętaj",
      color = "wskazowka",
      open = TRUE,
      "Większość problemów decyzyjnych w ekonomii i zarządzaniu da się sprowadzić do PL — pod warunkiem, że relacje są liniowe (lub można je takimi przybliżyć). Pierwszy krok analityka: rozpoznać, do której rodziny zadań należy konkretna sytuacja."
    ),

    lc_chapter_next(
      num = "04",
      title = "Metoda graficzna",
      lead = "obszar dopuszczalny i wierzchołki na dwóch wymiarach",
      target_id = "ch-graf"
    )
  )
)

ch3_server <- function(input, output, session) {}
