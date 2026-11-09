# Ankieta studencka w przeglądarce: window.lc01Survey dla wykresów JS.
# student_data jest stałe (set.seed(2024) w R/ankieta.R), więc dane trafiają
# do strony raz, a wykresy liczą się bez serwera, także w wersji offline.
# Czynnik: kody 1..k i etykiety poziomów; liczby: wartości jak w R.

lc01_survey_json <- function(data = student_data, meta = variable_meta) {
  columns <- lapply(names(data), function(name) {
    v <- data[[name]]
    key <- iconv(name, "UTF-8", "ASCII//TRANSLIT")
    info <- meta[[key]]
    col <- list(label = if (is.null(info)) name else info$label,
                type = if (is.null(info)) NA_character_ else info$type)
    if (is.factor(v)) {
      col$levels <- levels(v)
      col$values <- as.integer(v)
    } else {
      col$values <- as.numeric(v)
    }
    col
  })
  names(columns) <- names(data)
  jsonlite::toJSON(list(n = nrow(data), columns = columns),
                   auto_unbox = TRUE, digits = NA, na = "null")
}

lc01_survey_script <- function(data = student_data, meta = variable_meta) {
  tags$script(HTML(paste0("window.lc01Survey = ", lc01_survey_json(data, meta), ";")))
}
