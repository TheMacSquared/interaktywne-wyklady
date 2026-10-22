# ============================================================================
# FUNKCJE POMOCNICZE - Case studies
# ============================================================================

case_explore   <- unname(upwr_cat["niebo"])
case_test      <- unname(upwr_cat["wrzos"])
case_model     <- unname(upwr_cat["szalwia"])
case_conclude  <- unname(upwr_cat["bursztyn"])
case_highlight <- upwr_accent
case_reference <- upwr_secondary
case_muted     <- upwr_reference


# Formatowanie wyniku testu
format_decision <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    list(text = "Odrzucamy H₀", color = case_highlight, icon = "✗")
  } else {
    list(text = "Brak podstaw do odrzucenia H₀", color = case_model, icon = "✓")
  }
}


# Pytanie z kartami odpowiedzi (lc-choices): poprawna karta zielenieje po
# wyborze, pod kartami pojawia się wyjaśnienie z case_quiz_server().
case_quiz_ui <- function(id, title, question, choices, correct) {
  figure_panel(
    label = "Pytanie", title = title,
    p(question),
    tags$div(class = "lc-choices", `data-correct` = correct,
      radioButtons(id, NULL, choices = choices, selected = character(0))
    ),
    uiOutput(paste0(id, "_fb"))
  )
}

# feedback: lista nazwana wartościami odpowiedzi; tekst po werdykcie.
case_quiz_server <- function(input, output, id, correct, feedback) {
  output[[paste0(id, "_fb")]] <- renderUI({
    choice <- input[[id]]
    if (is.null(choice) || identical(choice, character(0))) return(NULL)
    ok <- identical(choice, correct)
    lc_status(
      lc_verdict(tags$strong(if (ok) "Tak." else "Nie."),
                 type = if (ok) "ok" else "danger"),
      " ", feedback[[choice]]
    )
  })
}


# Mapa przebiegu analizy (kroki studium jako łańcuch kart).
# steps: lista list(name, tools, ref, kind); kind ∈ explore/test/model/conclude
# daje kolor etapu (te same role co case_explore … case_conclude).
# current = k rysuje wersję skróconą (pasek) z wyróżnionym krokiem k.
case_kind_var <- c(explore = "niebo", test = "wrzos", model = "szalwia",
                   conclude = "bursztyn")

case_pipeline <- function(steps, current = NULL) {
  mini <- !is.null(current)
  cards <- lapply(seq_along(steps), function(k) {
    s <- steps[[k]]
    state <- if (!mini) "" else if (k == current) " is-current" else if (k < current) " is-done" else " is-next"
    tags$li(
      class = paste0("cp-step", state),
      style = paste0("--cp-c:var(--upwr-cat-", case_kind_var[[s$kind]], ");"),
      tags$span(class = "cp-num", sprintf("%02d", k)),
      tags$span(class = "cp-name", s$name),
      if (!mini) tags$span(class = "cp-tools", s$tools),
      if (!mini && !is.null(s$ref)) tags$span(class = "cp-ref", s$ref)
    )
  })
  tags$ol(class = paste("case-pipeline", if (mini) "is-mini"),
    `aria-label` = paste("Przebieg analizy:",
                         paste(vapply(steps, `[[`, "", "name"), collapse = ", ")),
    cards)
}

case_steps_ch1 <- list(
  list(name = "Poznanie danych",   tools = "histogram, korelacje", ref = "w. 01, 08", kind = "explore"),
  list(name = "Prosty związek",    tools = "test r, regresja prosta", ref = "w. 05, 07", kind = "test"),
  list(name = "Zmienna zakłócająca", tools = "korelacje, ANOVA",   ref = "w. 05", kind = "test"),
  list(name = "Kontrola zmiennych", tools = "regresja wieloraka",  ref = "w. 07", kind = "model"),
  list(name = "Wybrany model",     tools = "przedziały, reszty",   ref = "w. 06, 07", kind = "model"),
  list(name = "Decyzja",           tools = "skala efektu, ograniczenia", ref = NULL, kind = "conclude")
)

case_steps_ch2 <- list(
  list(name = "Opis grup",   tools = "wykres pudełkowy, średnie", ref = "w. 01", kind = "explore"),
  list(name = "Założenia",   tools = "wykresy Q-Q, test Levene'a", ref = "w. 06", kind = "test"),
  list(name = "Test i pary", tools = "ANOVA Welcha, Games-Howell", ref = "w. 05", kind = "test"),
  list(name = "Podział na płeć", tools = "porównanie w podgrupach", ref = NULL, kind = "model"),
  list(name = "Wniosek",     tools = "skala różnic, ograniczenia", ref = NULL, kind = "conclude")
)

case_steps_ch3 <- list(
  list(name = "Kto przeżył",    tools = "odsetki w grupach", ref = "w. 01", kind = "explore"),
  list(name = "Test χ²",        tools = "tabela kontyngencji, V Cramera", ref = "w. 05", kind = "test"),
  list(name = "Skład klas",     tools = "odsetki w podgrupach płci", ref = NULL, kind = "test"),
  list(name = "Model",          tools = "regresja logistyczna", ref = "w. 07", kind = "model"),
  list(name = "Wniosek",        tools = "ilorazy szans, ograniczenia", ref = NULL, kind = "conclude")
)


# Schemat zmiennej zakłócającej Z → X, Z → Y, X → Y?
# Grubość krawędzi rośnie z siłą związku (|r| albo V), etykieta to wartość.
# nodes: c(z, x, y); edges: c(zx, zy, xy) jako napisy; strength: liczby 0–1.
case_confounder <- function(nodes, edges, strength,
                            colors = c("bursztyn", "niebo", "grafit")) {
  W <- 600; H <- 250
  pos <- list(z = c(300, 42), x = c(110, 205), y = c(490, 205))
  bw <- 176; bh <- 46
  node <- function(p, label, col) {
    tags$g(class = "cf-node", style = paste0("--cf-c:var(--upwr-cat-", col, ");"),
      tags$rect(x = p[1] - bw / 2, y = p[2] - bh / 2, width = bw, height = bh, rx = 8),
      tags$text(x = p[1], y = p[2] + 6, `text-anchor` = "middle", label))
  }
  # krawędź od brzegu do brzegu prostokątów (przycięta, żeby grot był widoczny)
  edge <- function(a, b, label, s, dashed = FALSE, lab_dx = 0, lab_dy = 0) {
    d <- b - a; len <- sqrt(sum(d^2)); u <- d / len
    clip <- function(u) min(abs((bw / 2 + 6) / u[1]), abs((bh / 2 + 6) / u[2]))
    t0 <- clip(u); p0 <- a + u * t0; p1 <- b - u * (t0 + 14)
    mid <- (p0 + p1) / 2
    tagList(
      tags$line(class = paste("cf-edge", if (dashed) "is-open"),
        x1 = p0[1], y1 = p0[2], x2 = p1[1], y2 = p1[2],
        `stroke-width` = round(1.5 + 7 * s, 1), `marker-end` = "url(#cf-arrow)"),
      tags$text(class = "cf-val", x = mid[1] + lab_dx, y = mid[2] + lab_dy,
                `text-anchor` = "middle", label)
    )
  }
  tags$svg(class = "case-confounder", viewBox = paste(0, 0, W, H),
    role = "img",
    `aria-label` = sprintf("%s wiąże się z %s (%s) i z %s (%s); związek %s z %s: %s.",
      nodes[1], nodes[2], edges[1], nodes[3], edges[2], nodes[2], nodes[3], edges[3]),
    tags$defs(tags$marker(id = "cf-arrow", viewBox = "0 0 10 10", refX = 0, refY = 5,
      markerWidth = 16, markerHeight = 16, markerUnits = "userSpaceOnUse",
      orient = "auto-start-reverse", tags$path(d = "M0,0 L10,5 L0,10 z", class = "cf-head"))),
    edge(pos$z, pos$x, edges[1], strength[1], lab_dx = -58, lab_dy = 0),
    edge(pos$z, pos$y, edges[2], strength[2], lab_dx = 58, lab_dy = 0),
    edge(pos$x, pos$y, edges[3], strength[3], dashed = TRUE, lab_dy = -12),
    node(pos$z, nodes[1], colors[1]),
    node(pos$x, nodes[2], colors[2]),
    node(pos$y, nodes[3], colors[3])
  )
}
