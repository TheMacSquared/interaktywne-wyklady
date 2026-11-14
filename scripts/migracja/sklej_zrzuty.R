# Skleja pary zrzutów *_shiny.png i *_static.png z katalogu w obrazy *_obok.png
# (Shiny po lewej, strona statyczna po prawej). Używane przez zrzuty_paneli.R.
# Użycie: Rscript scripts/migracja/sklej_zrzuty.R <katalog>
sklej_zrzuty <- function(dir) {
  for (old in Sys.glob(file.path(dir, "*_shiny.png"))) {
    new <- sub("_shiny[.]png$", "_static.png", old)
    if (!file.exists(new)) next
    a <- png::readPNG(old); b <- png::readPNG(new)
    h <- max(nrow(a), nrow(b))
    pad <- function(m) { o <- array(1, c(h, ncol(m), 3)); o[seq_len(nrow(m)), , ] <- m[, , 1:3]; o }
    a <- pad(a); b <- pad(b)
    both <- array(0.5, c(h, ncol(a) + 6 + ncol(b), 3))
    both[, seq_len(ncol(a)), ] <- a
    both[, ncol(a) + 6 + seq_len(ncol(b)), ] <- b
    out <- sub("_shiny[.]png$", "_obok.png", old)
    png::writePNG(both, out)
    cat(out, "\n")
  }
}
if (!interactive() && length(commandArgs(TRUE)) && grepl("sklej_zrzuty", paste(commandArgs(FALSE), collapse = " "))) {
  sklej_zrzuty(commandArgs(TRUE)[[1]])
}
