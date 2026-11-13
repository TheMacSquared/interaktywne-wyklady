# Ryciny rozkładów teoretycznych rysowane w przeglądarce (dist_charts.js).
# Definicje scenariuszy zostają w modułach rozdziałów (ch3_binom_defs itd.);
# tu zamieniamy je na data-config: parametry, kolor i gotowy podpis
# „E(X) = …, SD = …” dla każdego scenariusza.

# Kolor scenariusza stały (wg kolejności w definicjach), niezależny od tego,
# które scenariusze są zaznaczone.
d03_scenarios <- function(defs, stats_fn) {
  cols <- upwr_cat_n(length(defs))
  out <- lapply(seq_along(defs), function(i) {
    s <- defs[[i]]
    c(s, list(color = cols[[i]], stats = stats_fn(s)))
  })
  setNames(out, names(defs))
}

# Rycina „kilka scenariuszy jednego rozkładu”: pola wyboru, wykres, lista E(X)/SD.
# dist: "binom", "pois", "geom" (lizaki P(X = k)) albo "unif", "exp", "t",
# "chisq", "lnorm" (gęstości). extra: dodatkowa kontrolka (np. N(0, 1) w Ryc. 5.12).
d03_overlay <- function(input_id, defs, selected, stats_fn, dist,
                        x_name = NULL, y_name = NULL, extra = NULL, normal = NULL,
                        max_height = "400px") {
  discrete <- dist %in% c("binom", "pois", "geom")
  choices <- setNames(names(defs), vapply(defs, `[[`, "", "label"))
  control <- checkboxGroupInput(input_id, "Scenariusze", choices = choices, selected = selected)
  lc_sf_figure(if (discrete) "d03-pmf" else "d03-pdf",
    control = if (is.null(extra)) control else list(control, extra),
    ratio = "2/1", max_height = max_height, legend = TRUE,
    # Bez pól NULL: jsonlite zapisałby je jako {} (w JS prawda logiczna).
    config = Filter(Negate(is.null), list(dist = dist, defs = d03_scenarios(defs, stats_fn),
                                          xName = x_name, yName = y_name, normal = normal)),
    after = tags$ul(`data-role` = "stats")
  )
}
