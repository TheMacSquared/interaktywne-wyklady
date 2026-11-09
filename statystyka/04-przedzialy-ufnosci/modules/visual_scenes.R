# Scena „Zmierz grupkę” w stylu S1 (ilustracja redakcyjna).
# Styl S1 jest standardem wizualnym scen wykładu 04 (decyzja 2026-11-09);
# S2 (makieta 2.5D) i S3 (szkic markerem) wycofane.
# Rysunek i symulacja: visual_scenes.js, style: visual_scenes.css.
visual_scene_ui <- function(id, title, caption, aria) {
  action <- function(key, text) tags$button(type = "button", class = "lc-action is-outline",
                                           `data-vs-action` = key, text)
  tags$div(class = "vs-widget", id = paste0("vs_", id),
    figure_panel(label = "Scena", title = title, width_mode = "wide",
      lc_toolbar(
        tags$button(type = "button", class = "lc-action is-solid", `data-vs-action` = "go", "Zmierz grupkę"),
        tags$div(class = "lc-action-group", action("m10", "+10"), action("m100", "+100"), action("m1000", "+1000")),
        action("truth", "Pokaż μ"),
        tags$button(type = "button", class = "lc-action is-ghost", `data-vs-action` = "reset",
                    `aria-label` = "Zacznij od nowa", lc_icon("reset"))
      ),
      tags$div(class = "vs-art", role = "img", tabindex = "0", `aria-label` = aria),
      tags$div(class = "vs-read", role = "status", `aria-live` = "polite",
                "Grupka 25 osób czeka na pomiar."),
      lc_caption(caption)
    )
  )
}
