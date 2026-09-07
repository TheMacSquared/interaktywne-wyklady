# ==============================================================================
# Procesy wykładów — start, status, zatrzymanie
# ==============================================================================
#
# Każdy wykład działa jako osobny proces R (shiny::runApp) na własnym porcie.
# Wielo-procesowość jest konieczna — Shiny nie montuje wielu apek w jednym
# procesie — i to ona daje zachowanie stanu: proces żyje niezależnie od tego,
# którą kartę przeglądarki oglądasz.
#
# Rejestr jest w środowisku na poziomie aplikacji, nie sesji, więc jest wspólny
# dla wszystkich kart huba.
#
# ==============================================================================

HUB_PORT_MIN <- 7701L
HUB_PORT_MAX <- 7799L

.hub_registry <- new.env(parent = emptyenv())


# ---- Porty -------------------------------------------------------------------

# Port jest wolny, gdy nie da się do niego połączyć.
hub_port_free <- function(port) {
  con <- try(
    suppressWarnings(
      socketConnection("127.0.0.1", port, open = "r+", blocking = TRUE, timeout = 1)
    ),
    silent = TRUE
  )
  if (inherits(con, "try-error")) return(TRUE)
  close(con)
  FALSE
}

# Szukamy wolnego portu zamiast liczyć go z indeksu wykładu: dzięki temu hub nie
# wchodzi w drogę niczemu, co już działa (np. analiza-ryzyka/scripts/wyklad na 7710).
hub_find_free_port <- function() {
  for (port in HUB_PORT_MIN:HUB_PORT_MAX) {
    if (hub_port_free(port)) return(port)
  }
  stop("Brak wolnego portu w zakresie ", HUB_PORT_MIN, "-", HUB_PORT_MAX, ".")
}

# Czeka aż wykład zacznie odpowiadać. Apki z ciężkim startem (generowanie danych,
# source() modułów) potrzebują kilku sekund — bez tego przeglądarka trafia w pustkę.
hub_wait_for_port <- function(port, process, timeout = 90) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    if (!process$is_alive()) return(FALSE)
    if (!hub_port_free(port)) return(TRUE)
    Sys.sleep(0.25)
  }
  FALSE
}


# ---- Rejestr -----------------------------------------------------------------

hub_entry <- function(key) {
  entry <- .hub_registry[[key]]
  if (is.null(entry)) return(NULL)
  if (!entry$process$is_alive()) {
    rm(list = key, envir = .hub_registry)
    return(NULL)
  }
  entry
}

hub_is_running <- function(key) !is.null(hub_entry(key))

hub_running_keys <- function() {
  keys <- ls(.hub_registry)
  Filter(hub_is_running, keys)
}


# ---- Start i stop ------------------------------------------------------------

hub_start <- function(key, dir, hub_url) {
  existing <- hub_entry(key)
  if (!is.null(existing)) return(existing)

  port <- hub_find_free_port()
  code <- sprintf(
    "shiny::runApp(%s, port = %d, host = '127.0.0.1', launch.browser = FALSE, quiet = TRUE)",
    deparse(dir), port
  )
  log_path <- file.path(tempdir(), paste0("hub-", gsub("[^A-Za-z0-9]", "-", key), ".log"))

  process <- processx::process$new(
    command   = file.path(R.home("bin"), "Rscript"),
    args      = c("-e", code),
    stdout    = log_path,
    stderr    = "2>&1",
    # LC_HUB_URL pozwala wykładowi pokazać link powrotny do huba; przy ręcznym
    # runApp() zmiennej nie ma i wykład zachowuje się dokładnie jak dotąd.
    env       = c("current", LC_HUB_URL = hub_url),
    # supervise: gdyby hub padł nagle, procesy wykładów nie zostają sierotami.
    supervise = TRUE
  )

  if (!hub_wait_for_port(port, process)) {
    if (process$is_alive()) process$kill()
    log_tail <- if (file.exists(log_path)) {
      paste(utils::tail(readLines(log_path, warn = FALSE), 15), collapse = "\n")
    } else ""
    stop("Wykład nie wystartował.\n", log_tail)
  }

  entry <- list(process = process, port = port, dir = dir, log = log_path, started = Sys.time())
  assign(key, entry, envir = .hub_registry)
  entry
}

hub_stop <- function(key) {
  entry <- .hub_registry[[key]]
  if (is.null(entry)) return(invisible(FALSE))
  if (entry$process$is_alive()) entry$process$kill()
  rm(list = key, envir = .hub_registry)
  invisible(TRUE)
}

hub_stop_all <- function() {
  for (key in ls(.hub_registry)) hub_stop(key)
  invisible(TRUE)
}

hub_url_for <- function(entry) sprintf("http://127.0.0.1:%d", entry$port)
