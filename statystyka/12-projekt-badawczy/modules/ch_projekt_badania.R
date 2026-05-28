ch_projekt_ui <- lecture_chapter(id = "ch_projekt", num = "7", title = "Projekt lepszego badania", content = tagList(
  fluidRow(column(8, offset = 2,
    lc_chapter_hero(
      kicker = "Rozdział 07 · Od obserwacji do eksperymentu",
      num = "07",
      title = "Jak zaprojektować lepsze badanie?",
      lead = "Jeśli pierwsza analiza pokazuje ograniczenia danych, nie kończymy
              rozmowy. Pytamy, jakie badanie lepiej odpowie na pytanie."
    ),

    lc_h2("sec-01", "Współwystępowanie to nie przyczynowość"),

    div(class = "lc-prose",
      p("Dane obserwacyjne pokazują, że pewne cechy współwystępują z ocenami.
        Nie mówią same z siebie, czy atrakcyjność, płeć, akcent albo typ kursu
        powodują różnice w ewaluacjach."),
      p("Nie możemy losować prowadzącym urody ani życiorysu. Możemy jednak
        zaprojektować sytuację, w której lepiej oddzielimy możliwe mechanizmy:
        informację o osobie, jakość materiałów, efekty uczenia się i kontekst uczelni.")
    ),

    lc_h2("sec-02", "Cztery pomysły na mocniejszy projekt"),

    div(class = "lc-figure-panel",
      h4("Wybierz projekt badania"),
      radioButtons("ch_projekt_design", NULL,
        choices = c(
          "Eksperyment z manipulowanym opisem prowadzącego" = "profile",
          "Ślepa ocena materiałów dydaktycznych" = "blind",
          "Pomiar efektów uczenia się" = "learning",
          "Replikacja w różnych kulturach i uczelniach" = "replication"
        ),
        selected = "profile"
      ),
      uiOutput("ch_projekt_feedback")
    ),

    margin_callout(
      label = "Wskazówka",
      "W swoim projekcie grupowym też często nie możesz losować ludzi do warunków.
       Zastanów się, co to oznacza dla siły twoich wniosków.",
      color = "wskazowka"
    ),

    lc_chapter_next("08", "Checklist projektu grupowego",
      "Zamieniamy cały proces w listę kontrolną do projektów studentów.",
      "ch7"),
    div(style = "height: 40px;")
  )))
)

ch_projekt_server <- function(input, output, session) {
  output$ch_projekt_feedback <- renderUI({
    designs <- list(
      profile = list(
        type = "ok",
        title = "Eksperyment z fikcyjnymi profilami",
        shows = "Czy ta sama informacja o kursie jest oceniana inaczej, gdy zmienia się wizerunek/opis prowadzącego.",
        limits = "Sztuczna sytuacja może nie oddawać prawdziwego kontaktu na zajęciach; trzeba uważać etycznie na manipulację zdjęciami.",
        strength = "Silny dowód na mechanizm percepcji, słabszy na realne efekty w sali."
      ),
      blind = list(
        type = "ok",
        title = "Ślepa ocena materiałów dydaktycznych",
        shows = "Czy jakość slajdów, zadań lub instrukcji jest oceniana podobnie bez wiedzy o autorze.",
        limits = "Materiały to tylko część nauczania; nie mierzymy kontaktu, tłumaczenia i pracy ze studentami.",
        strength = "Mocny projekt do izolowania jakości materiałów, ale nie całej jakości zajęć."
      ),
      learning = list(
        type = "ok",
        title = "Pomiar efektów uczenia się",
        shows = "Czy studenci po kursie faktycznie wiedzą więcej lub lepiej rozwiązują problemy.",
        limits = "Trzeba mieć pretest, posttest i porównywalne grupy; wynik egzaminu też ma własne obciążenia.",
        strength = "Bardzo mocny krok w stronę jakości nauczania, bo wychodzi poza samą satysfakcję."
      ),
      replication = list(
        type = "info",
        title = "Replikacja w różnych kulturach i uczelniach",
        shows = "Czy tropy widoczne w tych danych są lokalne, czy powtarzają się w innych kontekstach.",
        limits = "Replikacja sama nie rozwiązuje przyczynowości; różnice między uczelniami mogą mieć wiele źródeł.",
        strength = "Dobry dowód na stabilność zjawiska, słabszy na mechanizm."
      )
    )
    x <- designs[[input$ch_projekt_design]]
    lc_feedback(
      tags$p(tags$strong(x$title)),
      tags$p(tags$strong("Co by to pokazało? "), x$shows),
      tags$p(tags$strong("Ograniczenia: "), x$limits),
      tags$p(tags$strong("Siła dowodu: "), x$strength),
      type = x$type
    )
  })
}
