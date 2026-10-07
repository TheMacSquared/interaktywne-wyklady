# ============================================================================
# CHAPTER 4: Przypadek czy wzorzec?
# ============================================================================

ch4_ui <- list(
  id    = "ch-przypadek",
  num   = "04",
  title = "Przypadek czy wzorzec?",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Dane i populacja",
      num    = "04",
      title  = "Czy ona naprawdę to potrafi?",
      lead   = "Pani twierdzi, że czuje, czy do filiżanki wlano najpierw mleko,
                czy herbatę. Trafia dziewięć razy na dziesięć. Czy to dowód
                umiejętności, czy zwykłe szczęście? Odpowiedź nie wymaga żadnego
                wzoru, tylko tłumu ludzi, którzy zgadują."
    ),

    lc_p("W poprzednim rozdziale p̂ zmieniało się od garści do garści. To samo
      dotyczy każdego wyniku z próby, także liczby trafień w jakimś teście.
      Dlatego pojedynczy dobry wynik niczego jeszcze nie dowodzi: trzeba
      wiedzieć, jak dobre wyniki zdarzają się samym przypadkiem."),

    lc_h2("ch4-herbata", "Herbata z mlekiem"),

    lc_p("Opowieść pochodzi z Cambridge z lat dwudziestych XX wieku. Pewna pani
      miała twierdzić, że po smaku odróżnia herbatę z mlekiem wlanym
      najpierw od herbaty, do której mleko dolano później. Ronald Fisher,
      jeden z twórców współczesnej statystyki, zaproponował próbę: kilka
      filiżanek przygotowanych w losowej kolejności i pytanie o każdą z nich.
      Zobaczmy, jak by to wyglądało z dziesięcioma filiżankami."),

    figure_panel(
      label = "Ryc. 4.1",
      width_mode = "text",
      scene_widget("ch4_herbata", "Herbata z mlekiem: od jednej pani do rozkładu trafień",
        steps = c("Pani", "Zgadujący", "Tłum", "Werdykt"),
        labels = c("Pani próbuje", "Zgadujący próbuje", "Kolejny zgadujący", "Kolejny zgadujący"),
        options = list(list(name = "hits", label = "Pani trafiła",
                            values = c(6, 7, 8, 9, 10), selected = 9)),
        config = list(kind = "tea", cups = 10, hits = 9,
                      aria = "Dziesięć filiżanek herbaty, wynik pani, zgadujący i histogram liczby trafień zgadujących"))
    ),

    lc_p("Wynik 9 na 10 robi wrażenie, ale wystarczy zmienić liczbę trafień
      pani, żeby zobaczyć, jak szybko ono maleje. Przy 7 trafieniach co szósty
      zgadujący jest równie dobry, a przy 8 co osiemnasty. Przy 10
      trafieniach zgadujący równie dobry zdarza się raz na tysiąc."),

    lc_note("Zasada", rule = TRUE,
      "Pytamy nie o to, czy wynik jest wysoki, tylko o to, jak często
       taki wynik dałby sam przypadek."
    ),

    lc_h2("ch4-test", "Skąd wziąć regułę zamiast oka"),

    lc_p("Scena z herbatą to w istocie test statystyczny, tylko bez wzoru.
      Przypuszczamy, że pani nie ma żadnej umiejętności i zgaduje, a potem
      sprawdzamy, jak często zgadujący osiągają jej wynik albo lepszy.
      Jeśli rzadko, wygodniej uznać, że pani coś potrafi. Jeśli często,
      wynik niczego nie dowodzi."),

    lc_p("Wykład 04 rozwija ten pomysł i zastępuje tłum zgadujących jedną
      liczbą: prawdopodobieństwem, że sam przypadek dałby wynik równie dobry
      jak ten, który mamy. Przedziały ufności z wykładu 03 odpowiadają na
      pokrewne pytanie z drugiej strony: w jakim zakresie może leżeć prawdziwa
      wartość parametru."),

    lc_warn("Pułapka",
      "Gdy sprawdzamy wiele par zmiennych albo wiele hipotez, któraś wyjdzie
       „istotna” przypadkiem. Sto zgadujących, z których każdy dostaje tę
       samą szansę, da kilku, którzy trafią dziewięć razy na dziesięć."
    ),

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
  scene_texts(input, output, "ch4_herbata", list(
    tagList("Pani próbuje po kolei dziesięciu filiżanek i za każdym razem mówi, czy najpierw
      wlano mleko, czy herbatę. Wybierz, ile razy trafiła, i obejrzyj próbę. Czy ten wynik
      świadczy o umiejętności?"),
    tagList("Żeby ocenić wynik pani, potrzebujemy punktu odniesienia: kogoś, kto na pewno nie ma
      żadnej umiejętności. Zgadujący do każdej filiżanki rzuca monetą. Zobacz, ile trafień
      wychodzi mu w jednej próbie."),
    tagList("Sprowadzamy tłum zgadujących. Każdy dostaje te same dziesięć filiżanek i zgaduje,
      a jego liczba trafień spada słupkiem nad swoją wartością. Dokładaj po 10, 100
      i 1000. Czerwone słupki to wyniki równie dobre jak wynik pani albo lepsze."),
    tagList("Ułamek tłumu w czerwonych słupkach to szansa, że sam przypadek da wynik pani albo
      lepszy. Kropki pokazują ją dla modelu. Zmień liczbę trafień pani i patrz, jak
      zmienia się ta szansa: rzadki wynik daje powód, by uznać, że to nie przypadek.")
  ))
}
