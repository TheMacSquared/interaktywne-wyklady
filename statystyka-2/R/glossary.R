# ============================================================================
# SLOWNIK TERMINOW STATYSTYCZNYCH
# Uzywany przez gloss() — wstawia klikalny termin z popupem definicji.
# ============================================================================

.GLOSSARY <- list(

  # Podstawy -------------------------------------------------------------------
  "populacja"             = "Cały zbiór obiektów, o których chcemy wnioskować.",
  "próba"                 = "Wybrany podzbiór populacji, na którym przeprowadzamy pomiary.",
  "parametr"              = "Liczbowa cecha opisująca populację (np. μ, σ, p). Zazwyczaj nieznany.",
  "statystyka"            = "Funkcja danych z próby służąca do estymacji parametru (np. x̄, s, p̂).",
  "estymator"             = "Statystyka używana do szacowania nieznanego parametru populacji.",
  "estymacja"             = "Proces wnioskowania o parametrze populacji na podstawie próby.",

  # Miary centralne i rozproszenia ---------------------------------------------
  "średnia"               = "Suma wartości w zbiorze podzielona przez ich liczbę. Oznaczana x̄ (próba) lub μ (populacja).",
  "mediana"               = "Wartość środkowa uporządkowanego zbioru danych. Odporna na wartości odstające.",
  "odchylenie standardowe"= "Miara przeciętnego odchylenia wartości od średniej. Pierwiastek z wariancji.",
  "wariancja"             = "Średnia kwadratów odchyleń od średniej. Mierzy rozproszenie danych.",
  "błąd standardowy"      = "Odchylenie standardowe rozkładu próbkowego statystyki (np. średniej).",

  # Rozkłady -------------------------------------------------------------------
  "rozkład normalny"      = "Symetryczny dzwonowy rozkład prawdopodobieństwa opisywany przez μ i σ.",
  "rozkład próbkowy"      = "Rozkład wartości statystyki obliczonej z wielu niezależnych prób.",
  "centralne twierdzenie graniczne" =
    "Rozkład próbkowy średniej dąży do normalnego, gdy n rośnie — niezależnie od rozkładu populacji.",

  # Przedziały ufności ---------------------------------------------------------
  "przedział ufności"     = "Zakres wartości, który z zadanym prawdopodobieństwem (poziomem ufności) pokrywa nieznany parametr.",
  "poziom ufności"        = "Prawdopodobieństwo, że przedział ufności pokryje prawdziwy parametr. Typowo 95%.",
  "margines błędu"        = "Połowa szerokości przedziału ufności: ±z·SE lub ±t·SE.",

  # Testy hipotez --------------------------------------------------------------
  "hipoteza zerowa"       = "H₀ — hipoteza o braku efektu lub braku różnicy. Odrzucamy ją lub nie.",
  "hipoteza alternatywna" = "Hₐ — hipoteza, którą chcemy udowodnić; przyjmujemy gdy p < α.",
  "p-wartość"             = "Prawdopodobieństwo uzyskania wyniku co najmniej tak ekstremalnego, zakładając że H₀ jest prawdziwa.",
  "poziom istotności"     = "Próg α (zazwyczaj 0,05), poniżej którego odrzucamy H₀.",
  "statystyka testowa"    = "Wartość obliczona z próby (np. t, z, χ²) służąca do podjęcia decyzji o H₀.",
  "test t"                = "Test sprawdzający hipotezę o średniej (lub różnicy średnich) gdy odchylenie populacji jest nieznane.",
  "test chi-kwadrat"      = "Test zgodności lub niezależności dla danych kategorycznych.",

  # Regresja -------------------------------------------------------------------
  "korelacja"             = "Miara liniowego związku między dwiema zmiennymi. Zakres: −1 do +1.",
  "regresja liniowa"      = "Model opisujący liniową zależność zmiennej odpowiedzi od predyktorów.",
  "współczynnik determinacji" = "R² — odsetek wariancji zmiennej zależnej wyjaśniony przez model.",

  # Inne -----------------------------------------------------------------------
  "rozkład dwumianowy"    = "Rozkład liczby sukcesów w n niezależnych próbach Bernoulliego z prawdopodobieństwem p.",
  "wartość odstająca"     = "Obserwacja znacznie odbiegająca od pozostałych wartości w zbiorze.",
  "ANOVA"                 = "Analiza wariancji — test porównujący średnie w więcej niż dwóch grupach.",
  "efekt"                 = "Praktyczna wielkość różnicy lub związku, niezależna od istotności statystycznej."
)

# Wstawia klikalny termin ze słownika.
# Użycie: gloss("średnia") lub gloss("x", "definicja inline")
gloss <- function(term, definition = NULL) {
  def <- if (!is.null(definition)) definition else .GLOSSARY[[term]]
  if (is.null(def)) stop(paste0("gloss(): brak definicji dla '", term, "'"))
  tags$span(class = "lc-gloss", `data-def` = def, term)
}
