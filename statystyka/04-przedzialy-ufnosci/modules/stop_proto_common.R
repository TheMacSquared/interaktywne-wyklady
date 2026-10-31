# PROTOTYPY PUNKTÓW STOP (2026-10-30) — wspólne elementy ----------------------
#
# Każdy prototyp ma złotą plakietkę „Prototyp <nr> · <rodzaj>”, numer jak
# w liście propozycji (1a … 5c, D1). Stare ryciny zostają obok do porównania.
# Pliki: stop_proto_ch1.R … stop_proto_ch5.R (ch5 zawiera też domknięcie D1),
# opcjonalnie własne skrypty modules/s4p_*.js (app.R dołącza je wszystkie).
# Każdy plik chN definiuje obiekty s4p_<nr>_ui i funkcję s4p_chN_server().
# Wymaga: palette.R, theme_upwr.R, shared.R, lecture_layout.R, helpers.R.

# proto = FALSE: prototyp zaakceptowany, zwykła plakietka rodzaju.
s4p_panel <- function(num, kind, title, ..., width_mode = "wide", proto = TRUE) {
  if (!proto) return(figure_panel(label = kind, title = title, width_mode = width_mode, ...))
  figure_panel(label = paste0("Prototyp ", num, " · ", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, ...)
}

# Wstawka w tekście rozdziału: UI prototypu albo zaślepka, gdy go jeszcze nie ma.
s4p <- function(num) {
  ui <- get0(paste0("s4p_", num, "_ui"), inherits = TRUE)
  if (!is.null(ui)) return(ui)
  figure_panel(label = paste0("Prototyp ", num), color = upwr_single_alt,
    width_mode = "text", tags$p(class = "lc-muted", "Prototyp w przygotowaniu."))
}

# Tekst kroku widgetu krokowego: lista napisów (albo funkcji bez argumentów).
s4p_step_text <- function(output, id, step, texts) {
  output[[paste0(id, "_text")]] <- renderUI({
    t <- texts[[step()]]
    if (is.function(t)) t() else t
  })
}

s4p_pct <- function(x, digits = 1) paste0(lc_fmt(100 * x, digits), "%")
