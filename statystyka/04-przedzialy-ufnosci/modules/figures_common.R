# Ryciny wstawiane w tekst rozdziałów (figures_ch1.R … figures_ch5.R).
# Każdy plik chN definiuje obiekty fig_<nr>_ui i funkcję fig_chN_server();
# część rycin ma własne skrypty JS (fig_ch2.js, fig_ch4.js).
# Wymaga: palette.R, theme_upwr.R, shared.R, lecture_layout.R, helpers.R.

fig_panel <- function(num, label, title, ..., width_mode = "wide") {
  figure_panel(label = label, title = title, width_mode = width_mode, ...)
}

# Wstawka w tekście rozdziału, np. fig("1a") → fig_1a_ui.
fig <- function(num) get(paste0("fig_", num, "_ui"), inherits = TRUE)

# Tekst kroku widgetu krokowego: lista napisów (albo funkcji bez argumentów).
fig_step_text <- function(output, id, step, texts) {
  output[[paste0(id, "_text")]] <- renderUI({
    t <- texts[[step()]]
    if (is.function(t)) t() else t
  })
}
