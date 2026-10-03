# ==============================================================================
# Pole „Jak to zrobić w jUPWR” — treść z sciaga-jupwr/jupwr.yaml
# ==============================================================================
#
# Użycie w module wykładu (po source() lecture_layout.R):
#
#   source(file.path(project_root, "sciaga-jupwr", "jupwr_box.R"))
#   ...
#   jupwr_box("t-dwie-grupy")
#
# Pole jest zwykłym lc_note(), więc wygląda jak pozostałe notki wykładu.
# Treść edytuje się wyłącznie w jupwr.yaml — ta sama, co w PDF-ie.
# ==============================================================================

.jupwr_env <- new.env(parent = emptyenv())

.jupwr_yaml_path <- function() {
  # Plik leży obok tego skryptu; sys.frame działa przy source(), zapasowo
  # szukamy względem katalogu roboczego aplikacji (statystyka/NN-*/).
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  candidates <- c(
    if (!is.null(here)) file.path(here, "jupwr.yaml"),
    file.path("..", "sciaga-jupwr", "jupwr.yaml"),
    file.path("sciaga-jupwr", "jupwr.yaml")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) stop("jupwr_box: nie znaleziono sciaga-jupwr/jupwr.yaml")
  hit[[1]]
}

jupwr_data <- function() {
  if (is.null(.jupwr_env$data)) {
    .jupwr_env$data <- yaml::read_yaml(.jupwr_yaml_path())
  }
  .jupwr_env$data
}

jupwr_entry <- function(id) {
  wpisy <- jupwr_data()$wpisy
  hit <- Filter(function(w) identical(w$id, id), wpisy)
  if (length(hit) == 0) stop(sprintf("jupwr_box: brak wpisu o id '%s' w jupwr.yaml", id))
  hit[[1]]
}

# Ścieżka menu jako ciąg „A → B → C” z wyróżnionymi etykietami.
jupwr_menu_path <- function(menu) {
  parts <- lapply(menu, function(m) tags$strong(m))
  sep <- rep(list(" → "), length(parts))
  tags$span(class = "jupwr-menu", utils::head(c(rbind(parts, sep)), -1))
}

# title = NULL: „Jak to zrobić w jUPWR — <temat>”; compact = TRUE pomija „uwaga”.
jupwr_box <- function(id, title = NULL, compact = FALSE) {
  w <- jupwr_entry(id)
  zmienne <- if (length(w$zmienne)) {
    tags$ul(lapply(names(w$zmienne), function(pole)
      tags$li(tags$strong(pole), ": ", w$zmienne[[pole]])))
  }
  lc_note(
    "jUPWR",
    title = if (is.null(title)) paste0("Jak to zrobić w jUPWR — ", w$temat) else title,
    tags$p(jupwr_menu_path(w$menu)),
    zmienne,
    if (length(w$kroki)) tags$ol(lapply(w$kroki, tags$li)),
    if (!is.null(w$wynik)) tags$p(tags$em("Wynik: "), w$wynik),
    if (!compact && !is.null(w$uwaga)) tags$p(tags$em("Uwaga: "), w$uwaga)
  )
}
