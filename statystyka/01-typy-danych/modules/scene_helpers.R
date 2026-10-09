# Sceny (widget krokowy z SVG rysowanym w modules/scenes.js).
# Skopiowane ze statystyki 00 (modules/helpers.R): scene_widget(), scene_texts().

# options: lista list(name, label, values = c(etykieta = "wartość"), selected, from)
# extra: dodatkowe kontrolki na końcu paska (np. lc_step_from(3, lc_slider(...)));
#   scena czyta je sama przez shiny:inputchanged, serwer R ich nie potrzebuje.
scene_widget <- function(id, title, steps, config, labels, options = NULL, extra = NULL,
                         more_from = 3, more = c("+10" = "m10", "+100" = "m100", "+1000" = "m1000")) {
  lc_step_widget(id,
    title = title,
    steps = steps,
    toolbar = lc_toolbar(
      lapply(options, function(o) {
        lc_step_from(o$from %||% 1, lc_group(o$label,
          tags$div(class = "lc-seg", role = "group", `aria-label` = o$label,
            lapply(seq_along(o$values), function(i) {
              v <- unname(o$values[[i]])
              lab <- names(o$values)[[i]] %||% v
              if (is.null(lab) || !nzchar(lab)) lab <- v
              tags$button(type = "button", `data-sc-opt` = paste0(o$name, ":", v),
                `aria-pressed` = if (v == o$selected) "true" else "false", lab)
            })
          )
        ))
      }),
      tags$button(type = "button", class = "lc-action is-solid", `data-sc-act` = "go",
        `data-labels` = jsonlite::toJSON(labels),
        lc_icon("shuffle"), tags$span(labels[[1]])),
      if (!is.null(more)) lc_step_from(more_from,
        tags$div(class = "lc-seg", role = "group", `aria-label` = "Więcej powtórzeń",
          lapply(seq_along(more), function(i) tags$button(type = "button",
            `data-sc-act` = unname(more[[i]]), names(more)[[i]]))
        )
      ),
      extra
    ),
    body = tags$div(class = "lc-sc",
      `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA))
  )
}

# Teksty kroków: lista fragmentów HTML, po jednym na krok.
scene_texts <- function(input, output, id, texts) {
  step <- lc_step_server(id, input)$step
  output[[paste0(id, "_text")]] <- renderUI(texts[[step()]])
}
