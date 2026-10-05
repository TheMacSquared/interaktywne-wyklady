# Start huba z pliku uruchomieniowego (Wyklady.bat / Wyklady.command)
# Sprawdza R i pakiety, nie dubluje działającego huba i otwiera przeglądarkę.
#
# Uruchamiane z katalogu głównego repo:  Rscript hub/start.R

port <- as.integer(Sys.getenv("PORT", "7700"))
hub_url <- sprintf("http://127.0.0.1:%d", port)
cran <- "https://cloud.r-project.org"

# ============================================================================
# R I PAKIETY HUBA
# ============================================================================

if (getRversion() < "4.1.0") {
  cat("\nWykłady wymagają R w wersji 4.1 lub nowszej (jest", format(getRversion()), ").\n")
  cat("Zainstaluj nowsze R ze strony", cran, "\n\n")
  quit(status = 1)
}

is_installed <- function(pkgs) {
  vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
}

hub_packages <- c("shiny", "processx")
missing_hub <- hub_packages[!is_installed(hub_packages)]
if (length(missing_hub) > 0) {
  cat("\nBrakuje pakietów R potrzebnych do uruchomienia huba:",
      paste(missing_hub, collapse = ", "), "\n\n")
  cat("Zainstaluj je raz w konsoli R:\n\n")
  cat(sprintf("install.packages(c(%s))\n\n",
              paste(sprintf('"%s"', missing_hub), collapse = ", ")))
  quit(status = 1)
}

# ============================================================================
# PAKIETY WYKŁADÓW
# ============================================================================

# Zależności czytamy z kodu parserem R, bez uruchamiania wykładów — ta sama
# logika co statystyka-2/scripts/check_dependencies.R. library(), require()
# i pkg:: są wymagane; requireNamespace() oznacza świadomy fallback w kodzie.
scan_expr <- function(expr, found = list(required = character(), optional = character())) {
  if (!is.call(expr) && !is.expression(expr) && !is.pairlist(expr)) return(found)

  if (is.call(expr)) {
    head <- if (is.symbol(expr[[1]])) as.character(expr[[1]]) else ""
    if (head %in% c("library", "require", "::", ":::") && length(expr) >= 2) {
      found$required <- c(found$required, as.character(expr[[2]]))
    } else if (head == "requireNamespace" && length(expr) >= 2) {
      found$optional <- c(found$optional, as.character(expr[[2]]))
    }
  }

  for (i in seq_along(expr)) {
    # Wywołania takie jak foo(x, optional_arg = ) zawierają pusty symbol.
    if (identical(expr[[i]], quote(expr = ))) next
    found <- scan_expr(expr[[i]], found)
  }
  found
}

scan_files <- function(files) {
  found <- list(required = character(), optional = character())
  for (file in files) {
    parsed <- tryCatch(parse(file, keep.source = FALSE, encoding = "UTF-8"),
                       error = function(e) NULL)
    if (!is.null(parsed)) found <- scan_expr(parsed, found)
  }
  found
}

# Przedmiot to katalog z R/lecture_layout.R, wykład to <przedmiot>/<wykład>/app.R
# — ten sam warunek co hub_discover() w hub/R/discovery.R.
lecture_requirements <- function(repo_root) {
  base_packages <- rownames(installed.packages(priority = "base"))
  subjects <- list.dirs(repo_root, recursive = FALSE)
  subjects <- subjects[file.exists(file.path(subjects, "R", "lecture_layout.R"))]

  result <- list()
  for (subject in subjects) {
    shared <- scan_files(list.files(file.path(subject, "R"), pattern = "[.]R$",
                                    full.names = TRUE))
    apps <- list.dirs(subject, recursive = FALSE)
    apps <- apps[file.exists(file.path(apps, "app.R"))]
    for (app in apps) {
      files <- c(file.path(app, "app.R"),
                 list.files(file.path(app, "modules"), pattern = "[.]R$",
                            recursive = TRUE, full.names = TRUE))
      deps <- scan_files(files)
      required <- unique(c(shared$required, deps$required))
      required <- setdiff(required, c(shared$optional, deps$optional, base_packages))
      key <- paste(basename(subject), basename(app), sep = "/")
      result[[key]] <- sort(required[nzchar(required)])
    }
  }
  result
}

requirements <- lecture_requirements(getwd())
all_packages <- sort(unique(unlist(requirements)))
missing <- all_packages[!is_installed(all_packages)]

if (length(missing) > 0) {
  cat("\nBrakuje pakietów R:", paste(missing, collapse = ", "), "\n")
  cat("Bez nich nie uruchomią się wykłady:\n")
  for (key in names(requirements)) {
    lacking <- intersect(requirements[[key]], missing)
    if (length(lacking)) cat(sprintf("  %-40s %s\n", key, paste(lacking, collapse = ", ")))
  }
  cat("\nZainstalować je teraz? Potrzebny internet, może to potrwać kilka minut. [T/n] ")
  answer <- tolower(trimws(readLines("stdin", n = 1)))
  if (length(answer) == 0 || answer %in% c("", "t", "tak", "y", "yes")) {
    install.packages(missing, repos = cran)
    still_missing <- missing[!is_installed(missing)]
    if (length(still_missing)) {
      cat("\nNie udało się zainstalować:", paste(still_missing, collapse = ", "), "\n")
      cat("Pozostałe wykłady działają normalnie.\n\n")
    }
  } else {
    cat("\nPomijam instalację. Pozostałe wykłady działają normalnie.\n")
    cat("Polecenie do wklejenia później w konsoli R:\n\n")
    cat(sprintf("install.packages(c(%s))\n\n",
                paste(sprintf('"%s"', missing), collapse = ", ")))
  }
}

# ============================================================================
# START HUBA
# ============================================================================

# Drugi dwuklik, gdy hub już działa: nie startujemy kolejnego (port byłby
# zajęty), tylko otwieramy istniejący w przeglądarce.
con <- try(
  suppressWarnings(
    socketConnection("127.0.0.1", port, open = "r+", blocking = TRUE, timeout = 1)
  ),
  silent = TRUE
)
if (!inherits(con, "try-error")) {
  close(con)
  cat("Hub już działa — otwieram", hub_url, "\n")
  utils::browseURL(hub_url)
  quit(status = 0)
}

cat("\nHub wykładów —", hub_url, "\n")
cat("To okno musi zostać otwarte przez całe zajęcia.\n")
cat("Zamknięcie okna kończy hub i wszystkie wykłady.\n\n")

shiny::runApp("hub", port = port, host = "127.0.0.1", launch.browser = TRUE)
