# Ryciny wykładu 10 liczone w przeglądarce (modules/wykresy.js, silnik lcSF).
# Tutaj: dane kursu przekazywane rycinom w `config` — tabela interwencji Bananpolu
# z etykietami zawiniętymi tak jak w dawnych wykresach (scales::label_wrap(14)).
# Wymaga: bananpol.R.

i10_config <- list(
  options = lapply(seq_len(nrow(bananpol$interventions)), function(i) {
    d <- bananpol$interventions[i, ]
    list(id = d$id, label = d$label, wrapped = scales::label_wrap(14)(d$label),
         cost_index = d$cost_index, feasibility = d$feasibility)
  })
)

# Nazwy modeli dla pytań z mapy kursu (panel „Wybierz pytanie”).
i10_question_models <- c(
  conditional = "Warunkowe i całkowite", bayes = "Bayes", binomial = "Dwumianowy",
  geometric = "Geometryczny", negative = "Ujemny dwumianowy", threshold = "Rozkład ciągły i ogon",
  survival = "Funkcje czasu życia", system = "Funkcja struktury i FTA"
)

# Rycina bez wykresu (same odczyty albo tekst): kontener lcSF z atrybutem data-lc-nochart.
i10_reads_figure <- function(fig, ..., config = NULL) {
  tags$div(class = "lc-sf", `data-fig` = fig, `data-lc-nochart` = NA,
    `data-config` = if (!is.null(config)) as.character(jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA)),
    ...)
}
