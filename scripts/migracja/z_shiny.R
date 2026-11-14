# Uruchamia aplikację Shiny w tle i wywołuje skrypt sprawdzający z jej adresem.
# Użycie: Rscript with_shiny.R <katalog aplikacji> <skrypt.R>
args <- commandArgs(TRUE); app_dir <- normalizePath(args[[1]]); port <- 7745L
p <- processx::process$new(file.path(R.home("bin"), "Rscript"),
  c("-e", sprintf("shiny::runApp('%s', port = %d, host = '127.0.0.1', launch.browser = FALSE)", app_dir, port)),
  stdout = "|", stderr = "|")
for (i in 1:40) { ok <- tryCatch({ close(url(sprintf("http://127.0.0.1:%d", port), open = "r")); TRUE }, error = function(e) FALSE, warning = function(w) FALSE); if (ok) break; Sys.sleep(0.5) }
out <- system2(file.path(R.home("bin"), "Rscript"), c(args[[2]], sprintf("http://127.0.0.1:%d", port)), stdout = TRUE, stderr = TRUE)
p$kill()
cat(out, sep = "\n")
