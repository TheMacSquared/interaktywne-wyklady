# ============================================================================
# FUNKCJE POMOCNICZE
# ============================================================================

render_taxonomy <- function(highlight = NULL, revealed = character(0)) {
  nodes <- data.frame(
    id = c("dane", "ilościowe", "jakościowe",
           "ciagla", "dyskretna", "porzadkowa", "nominalna"),
    label = c("Dane", "Ilosciowe\n(liczbowe)", "Jakosciowe\n(kategoryczne)",
              "Ciagle", "Dyskretne", "Porzadkowe", "Nominalne"),
    x = c(5, 2.5, 7.5, 1.25, 3.75, 6.25, 8.75),
    y = c(3, 2, 2, 1, 1, 1, 1),
    type = c(NA, NA, NA, "ilosciowa_ciagla", "ilosciowa_dyskretna", "porzadkowa", "nominalna"),
    example = c("", "", "",
                "np. wzrost,\nczas dojazdu",
                "np. liczba kursow,\nliczba nieobecnosci",
                "np. rok studiow,\nzadowolenie",
                "np. plec, kierunek,\ngrupa krwi"),
    stringsAsFactors = FALSE
  )

  nodes$fill <- sapply(nodes$type, function(t) {
    if (is.na(t)) return(upwr_rule)
    type_colors[t]
  })
  nodes$alpha <- sapply(nodes$type, function(t) {
    if (is.null(highlight) || is.na(t)) return(1)
    if (t == highlight) return(1) else return(0.3)
  })

  edges <- data.frame(
    x = c(5, 5, 2.5, 2.5, 7.5, 7.5),
    xend = c(2.5, 7.5, 1.25, 3.75, 6.25, 8.75),
    y = c(2.75, 2.75, 1.75, 1.75, 1.75, 1.75),
    yend = c(2.25, 2.25, 1.25, 1.25, 1.25, 1.25)
  )

  ggplot() +
    geom_segment(data = edges,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 color = upwr_rule, linewidth = 1.2) +
    geom_tile(data = nodes,
              aes(x = x, y = y, width = 2, height = 0.45),
              fill = nodes$fill, alpha = nodes$alpha,
              color = upwr_secondary, linewidth = 0.5) +
    geom_text(data = nodes,
              aes(x = x, y = y, label = label),
              size = 5, fontface = "bold", color = upwr_secondary) +
    geom_text(data = nodes %>% filter(id %in% revealed),
              aes(x = x, y = y - 0.35, label = example),
              size = 3.5, color = upwr_ink_soft, lineheight = 0.9) +
    geom_text(data = nodes %>% filter(example != "", !id %in% revealed),
              aes(x = x, y = y - 0.35, label = "kliknij aby odkryc"),
              size = 3, color = upwr_reference, fontface = "italic") +
    coord_cartesian(xlim = c(-0.2, 10.2), ylim = c(0.3, 3.5)) +
    theme_void() +
    theme(plot.margin = margin(10, 10, 10, 10))
}

render_good_plot <- function(x, label, type) {
  col <- type_colors[type]
  df <- data.frame(x = x)

  if (type %in% c("nominalna", "porzadkowa")) {
    ggplot(df, aes(x = x)) +
      geom_bar(fill = col, color = NA) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Wykres słupkowy: ", label), x = label, y = "Liczebność") +
      theme(axis.text.x = element_text(angle = if (nlevels(factor(x)) > 4) 30 else 0, hjust = 1))
  } else if (type == "ilosciowa_dyskretna") {
    ggplot(df, aes(x = factor(x))) +
      geom_bar(fill = col, color = NA) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = paste0("Wykres słupkowy: ", label), x = label, y = "Liczebność")
  } else {
    ggplot(df, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20, fill = col, color = NA, alpha = 0.7) +
      geom_density(color = col, fill = NA, linewidth = 1.2) +
      labs(title = paste0("Histogram z gęstościa: ", label), x = label, y = "Gęstość")
  }
}

render_bad_plot <- function(x, label, type) {
  df <- data.frame(x = x)
  if (type %in% c("nominalna", "porzadkowa")) {
    df$x_num <- as.numeric(factor(x))
    ggplot(df, aes(x = x_num)) +
      geom_histogram(bins = 10, fill = upwr_reference, color = NA) +
      labs(title = paste0("Histogram (NIEODPOWIEDNI): ", label),
           subtitle = "Histogram wymaga danych liczbowych - tu mamy kategorie!",
           x = paste0(label, " (zakodowane jako liczby)"), y = "Liczebność") +
      theme(plot.title = element_text(color = upwr_accent),
            plot.subtitle = element_text(color = upwr_accent, face = "italic"))
  } else {
    n_unique <- length(unique(x))
    ggplot(df, aes(x = x)) +
      geom_bar(fill = upwr_reference, width = 0.3) +
      labs(title = paste0("Wykres słupkowy (NIEODPOWIEDNI): ", label),
           subtitle = paste0(n_unique, " unikalnych wartości - wykres słupkowy jest nieczytelny!"),
           x = label, y = "Liczebność") +
      theme(plot.title = element_text(color = upwr_accent),
            plot.subtitle = element_text(color = upwr_accent, face = "italic"),
            axis.text.x = element_text(size = 5, angle = 90))
  }
}

pie_vs_bar_scenarios <- list(
  list(
    name = "Duze różnice",
    labels = c("Produkt A", "Produkt B", "Produkt C", "Produkt D", "Produkt E"),
    data = c(45, 25, 15, 10, 5),
    colors = upwr_cat_n(5),
    pie_verdict = "Różnice widoczne, ale porównanie kątów jest trudniejsze niż długości",
    bar_verdict = "Natychmiastowe porównanie -- różnice czytelne od razu",
    pie_ok = TRUE
  ),
  list(
    name = "Podobne wartości",
    labels = c("Produkt A", "Produkt B", "Produkt C", "Produkt D", "Produkt E"),
    data = c(22, 21, 20, 19, 18),
    colors = upwr_cat_n(5),
    pie_verdict = "Wycinki prawie identyczne -- nie widać która kategoria prowadzi",
    bar_verdict = "Różnice 1-2 pp. wciąż czytelne dzięki wspólnej osi",
    pie_ok = FALSE
  ),
  list(
    name = "Podobne + zle kolory",
    labels = c("Produkt A", "Produkt B", "Produkt C", "Produkt D", "Produkt E"),
    data = c(22, 21, 20, 19, 18),
    # Świadomie zły dobór kolorów — pięć odcieni tego samego niebieskiego.
    # Ilustracja problemu: "zbliżone wielkości + zbliżone kolory = nieczytelny wykres".
    colors = upwr_seq_burgundy[3:7],
    pie_verdict = "Zblizone wielkości + zbliżone kolory = nieczytelny wykres",
    bar_verdict = "Nawet przy podobnych kolorach pozycja na osi ratuje czytelnosc",
    pie_ok = FALSE
  )
)
