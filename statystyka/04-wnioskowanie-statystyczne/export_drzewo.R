#!/usr/bin/env Rscript
# ============================================================================
# EKSPORT DIAGRAMU DECYZYJNEGO
# ============================================================================
# Generuje dwa pliki w `exports/` gotowe do wrzucenia na tablicę Miro:
#
#   exports/drzewo_graphviz.svg   - wektor (wariant A, czytelne tabele)
#   exports/drzewo_visnet.html    - interaktywny widget (samodzielny plik)
#   exports/drzewo_visnet.png     - bitmapa z webshotem visNetworka
#
# Użycie z katalogu rozdziału:
#   Rscript export_drzewo.R
# lub z poziomu R:
#   source("wnioskowanie-statystyczne/export_drzewo.R")
#
# Wymagane pakiety (jednorazowo):
#   install.packages(c("DiagrammeR", "DiagrammeRsvg",
#                      "visNetwork", "htmlwidgets", "webshot2"))
# ============================================================================

# --- Ustalenie katalogu skryptu (działa i pod Rscript, i pod source()) -----
.here <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(dirname(normalizePath(sub("--file=", "", file_arg))))
  for (i in seq_len(sys.nframe())) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  }
  getwd()
})()

app_dir <- .here
out_dir <- file.path(app_dir, "exports")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Załaduj dane grafu ----------------------------------------------------
source(file.path(app_dir, "modules", "drzewo_data.R"), local = TRUE)

# ============================================================================
# 1. SVG z Graphviza (wariant A)
# ============================================================================
svg_path <- file.path(out_dir, "drzewo_graphviz.svg")
message("→ Generuję SVG (Graphviz): ", svg_path)

svg_xml <- DiagrammeR::grViz(drzewo_dot) |>
  DiagrammeRsvg::export_svg()
writeLines(svg_xml, svg_path, useBytes = TRUE)
message("   OK (", format(file.size(svg_path), big.mark = " "), " B)")

# ============================================================================
# 2. HTML (samodzielny) z visNetworka (wariant B)
# ============================================================================
html_path <- file.path(out_dir, "drzewo_visnet.html")
message("→ Generuję HTML (visNetwork): ", html_path)

vn <- build_drzewo_visnet()
# htmlwidgets::saveWidget z selfcontained=TRUE + libdir=NULL produkuje
# jeden samodzielny plik HTML (wymaga pandoc). visSave() pod spodem wywołuje
# to samo, ale czasem zostawia sidecar dir - saveWidget jest bardziej
# przewidywalny.
htmlwidgets::saveWidget(vn, file = html_path,
                        selfcontained = TRUE, background = "white")
# Posprzątaj ewentualny sidecar, jeśli jednak powstał.
sidecar <- sub("\\.html$", "_files", html_path)
if (dir.exists(sidecar)) unlink(sidecar, recursive = TRUE)
message("   OK (", format(file.size(html_path), big.mark = " "), " B)")

# ============================================================================
# 3. PNG z webshotem visNetworka (wariant B, bitmapa do Miro)
# ============================================================================
png_path <- file.path(out_dir, "drzewo_visnet.png")
message("→ Generuję PNG (webshot visNetworka): ", png_path)

if (!requireNamespace("webshot2", quietly = TRUE)) {
  message("   POMINIĘTO: brak pakietu webshot2. Zainstaluj: install.packages(\"webshot2\")")
} else {
  # webshot2 wymaga przeglądarki Chromium. Odczekanie na rozłożenie layoutu
  # (hierarchiczny) przed zrzutem - visNetwork po załadowaniu musi przeliczyć
  # pozycje, delay 1.5s wystarcza.
  ok <- tryCatch({
    webshot2::webshot(
      url     = paste0("file://", html_path),
      file    = png_path,
      vwidth  = 2400,
      vheight = 1800,
      zoom    = 2,
      delay   = 1.5
    )
    TRUE
  }, error = function(e) {
    message("   POMINIĘTO: ", conditionMessage(e))
    message("   Aby włączyć PNG: zainstaluj chromium (np. `sudo apt install chromium-browser`)")
    message("   lub ustaw zmienną środowiskową CHROMOTE_CHROME na ścieżkę do chrome.exe.")
    FALSE
  })
  if (isTRUE(ok)) message("   OK (", format(file.size(png_path), big.mark = " "), " B)")
}

message("\nGotowe. Pliki w: ", out_dir)
