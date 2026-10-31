# Kierunki wizualne istniejącej sceny „Zmierz grupkę”: aktywne S1 i S3.
# S2 (makieta 2.5D) wycofany na prośbę użytkownika.
# To alternatywy do porównania w tekście, nie dodatkowe punkty głównej opowieści.
visual_scene <- function(style, label, title, caption) {
  action <- function(key, text) tags$button(type = "button", class = "lc-action is-outline",
                                           `data-vs-action` = key, text)
  tags$div(class = paste("vs-widget", paste0("vs-", style)), id = paste0("vs_", style),
    `data-style` = style,
    figure_panel(label = label, title = title, width_mode = "wide",
      lc_toolbar(
        tags$button(type = "button", class = "lc-action is-solid", `data-vs-action` = "go", "Zmierz grupkę"),
        tags$div(class = "lc-action-group", action("m10", "+10"), action("m100", "+100"), action("m1000", "+1000")),
        action("truth", "Pokaż μ"),
        tags$button(type = "button", class = "lc-action is-ghost", `data-vs-action` = "reset",
                    `aria-label` = "Zacznij od nowa", lc_icon("reset"))
      ),
      tags$div(class = "vs-art", role = "img", tabindex = "0", `aria-label` = title),
      tags$div(class = "vs-read", role = "status", `aria-live` = "polite",
                "Grupka 25 osób czeka na pomiar."),
      lc_caption(caption)
    )
  )
}

visual_scenes_ui <- tagList(
  visual_scene("editorial", "Wariant S1 · ilustracja redakcyjna", "Przed salą: ludzie stają się pomiarami",
    "Miękkie sylwetki i ograniczona paleta; na ilustracji fragment kolejki, na osi wszystkie 25 pomiarów."),
  visual_scene("marker", "Wariant S3 · szkic markerem", "Narysujmy to na tablicy",
    "Odręczny kontur, kreskowanie i ślad pomiaru; te same dane i ta sama skala co w wariancie ilustracji redakcyjnej.")
)
