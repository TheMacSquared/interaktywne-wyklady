# PROTOTYPY PUNKTÓW STOP — rozdział 3 (2026-10-29) ---------------------------
#   s3p_3a_ui — Ryc. 3.6 „Czy V-1 celowały?” (zaakceptowany 2026-10-29)
#   s3p_3b_ui — Ryc. 3.9 „Konie Bortkiewicza” (zaakceptowany 2026-10-29)
#   s3p_3g_ui — Ryc. 3.5 „Spotkanie na szlaku” (zaakceptowany 2026-10-29; scena w scenes.js, kind = "trail")
#   Usunięte 2026-10-29: 3c „Reklamacje seriami”, 3d „Wadliwe sztuki parami”, 3e „Monte Carlo, 1913”,
#   3f „Zgadnij rozkład”.
# Serwer: s3p_ch3_server(input, output, session).

# ============================================================================
# (3a) SCENA „CZY V-1 CELOWAŁY?”
# ============================================================================

# Clarke (1946): południowy Londyn, 576 kwadratów po 0.25 km², 537 trafień V-1.
# Liczba kwadratów z 0, 1, 2, 3, 4 i 5+ trafieniami.
s3p_v1_obs <- c(229, 211, 93, 35, 7, 1)
s3p_v1_lambda <- 537 / 576
s3p_v1_exp <- 576 * c(dpois(0:4, s3p_v1_lambda), ppois(4, s3p_v1_lambda, lower.tail = FALSE))

s3p_3a_ui <- s3p_panel("3a", "Ryc. 3.6", "Czy V-1 celowały?", proto = FALSE,
  lc_step_widget("s3p_3a",
    steps = c("Mapa trafień", "Siatka", "Zliczenia", "Losowo?"),
    toolbar = lc_toolbar(lc_action("s3p_3a_shuffle", "Rozrzuć 537 trafień losowo", icon = "shuffle")),
    plot_id = "s3p_3a_plot", ratio = "1.6/1"
  )
)

# ============================================================================
# (3b) PRZYPADEK „KONIE BORTKIEWICZA”
# ============================================================================

# Bortkiewicz (1898): 10 korpusów kawalerii pruskiej × 20 lat = 200 korpuso-lat;
# liczba żołnierzy zabitych kopnięciem konia w ciągu roku.
s3p_bk_k <- 0:4
s3p_bk_obs <- c(109, 65, 22, 3, 1)

s3p_3b_ui <- s3p_panel("3b", "Ryc. 3.9", "Konie Bortkiewicza", proto = FALSE,
  lc_step_widget("s3p_3b",
    steps = c("Dane", "Średnia i wariancja", "Model Pois(λ)"),
    plot_id = "s3p_3b_plot", ratio = "2.2/1"
  )
)

# ============================================================================
# (3g) PRZYPADEK „SPOTKANIE NA SZLAKU” (scenes.js, kind = "trail")
# ============================================================================

# Góra z 5 równorzędnymi szlakami; para spotyka się, gdy wybierze ten sam (p = 1/5).
# Zawody: 6 par, X = liczba spotkanych par ~ B(6, 0.2); medale, gdy X >= 3.
s3p_3g_cfg <- list(kind = "trail", mountain = "Ślęża", trails = 5, pairs = 6, medal = 3, height = 450,
  marks = I(c("czerwony", "niebieski", "zielony", "żółty", "czarny")),
  aria = "Góra z pięcioma szlakami; pary turystów wybierają szlaki losowo i spotykają się na tym samym szlaku; histogram liczby spotkanych par")

s3p_3g_ui <- s3p_panel("3g", "Ryc. 3.5", "Spotkanie na szlaku", width_mode = "text", proto = FALSE,
  scene_widget("s3p_3g", NULL,
    steps = c("Dwóch kolegów", "Zawody", "Wiele zawodów", "Model"),
    labels = c("Ruszają", "Rozegraj zawody", "Rozegraj zawody", "Rozegraj zawody"),
    options = list(list(name = "p", label = "Szansa spotkania", values = c("Ukryj" = "off", "Pokaż" = "on"),
                        selected = "off")),
    more_from = 3, config = s3p_3g_cfg)
)

# ============================================================================
# SERWER
# ============================================================================

s3p_ch3_server <- function(input, output, session) {

  # --- 3a -------------------------------------------------------------------
  step_3a <- lc_step_server("s3p_3a", input)$step
  hits <- reactiveVal({ set.seed(1944); data.frame(x = runif(537, 0, 24), y = runif(537, 0, 24)) })
  observeEvent(input$s3p_3a_shuffle, hits(data.frame(x = runif(537, 0, 24), y = runif(537, 0, 24))))

  zoom_plot_server("s3p_3a_plot", reactive({
    step <- step_3a()
    if (step <= 2) {
      h <- hits()
      p <- ggplot(h, aes(x, y))
      if (step == 2) {
        cnt <- as.data.frame(table(cx = floor(h$x), cy = floor(h$y)))
        cnt$cx <- as.numeric(as.character(cnt$cx)); cnt$cy <- as.numeric(as.character(cnt$cy))
        p <- p + geom_tile(data = cnt, aes(cx + 0.5, cy + 0.5, fill = pmin(Freq, 4)), colour = upwr_rule,
                           inherit.aes = FALSE) +
          scale_fill_gradient(low = "white", high = upwr_cat[["terakota"]], name = "Trafienia", breaks = 0:4,
                              labels = c(0:3, "4+"))
      }
      return(p + geom_point(size = 1.3, colour = upwr_ink, alpha = 0.8) +
        coord_equal(xlim = c(0, 24), ylim = c(0, 24), expand = FALSE) +
        labs(x = NULL, y = NULL, subtitle = "Południowy Londyn, 1944 · 537 trafień (położenia losowe, nie historyczne)") +
        theme_upwr(grid = "none") + theme(axis.text = element_blank()))
    }
    d <- data.frame(k = factor(c(0:4, "5+"), levels = c(0:4, "5+")), obs = s3p_v1_obs, exp = s3p_v1_exp)
    p <- ggplot(d, aes(k)) +
      geom_col(aes(y = obs), fill = upwr_secondary, alpha = 0.85, width = 0.7) +
      geom_text(aes(y = obs, label = obs), vjust = -0.4) +
      scale_y_continuous(limits = c(0, 260)) +
      labs(x = "Liczba trafień w kwadracie", y = "Liczba kwadratów") + theme_upwr(grid = "y")
    if (step == 4) {
      p <- p + geom_point(aes(y = exp), shape = 21, fill = "white", colour = upwr_accent, size = 4, stroke = 1.6) +
        labs(subtitle = "Słupki: Londyn 1944 · kółka: 537 trafień rozrzuconych zupełnie losowo")
    }
    p
  }))

  s3p_step_text(output, "s3p_3a", step_3a, list(
    "Lato 1944. Na południowy Londyn spadają pociski V-1. Mieszkańcy widzą na mapie skupiska trafień i są pewni, że Niemcy celują w konkretne dzielnice. Rozrzuć trafienia losowo kilka razy: skupiska pojawiają się także bez celowania.",
    "Analityk R.D. Clarke dzieli mapę na 576 kwadratów po 0.25 km² i liczy trafienia w każdym. Jeśli pociski celują, powinno być dużo pustych kwadratów i kilka bardzo zatłoczonych.",
    "Prawdziwe dane: 229 kwadratów bez trafienia, 211 z jednym, 93 z dwoma, 35 z trzema, 7 z czterema i jeden z pięcioma lub więcej. Czy to wygląda na celowanie?",
    "Kółka pokazują, czego się spodziewać, gdy każdy pocisk spada w zupełnie losowe miejsce. Zgodność jest prawie idealna. V-1 nie celowały, a skupiska były dziełem przypadku: losowość wygląda na skupiska."
  ))

  # --- 3b -------------------------------------------------------------------
  step_3b <- lc_step_server("s3p_3b", input)$step
  bk_mean <- sum(s3p_bk_k * s3p_bk_obs) / 200
  bk_var <- sum((s3p_bk_k - bk_mean)^2 * s3p_bk_obs) / 199

  zoom_plot_server("s3p_3b_plot", reactive({
    step <- step_3b()
    d <- data.frame(k = s3p_bk_k, obs = s3p_bk_obs, exp = 200 * dpois(s3p_bk_k, bk_mean))
    p <- ggplot(d, aes(k)) +
      geom_col(aes(y = obs), fill = upwr_secondary, alpha = 0.85, width = 0.7) +
      geom_text(aes(y = obs, label = obs), vjust = -0.4) +
      scale_y_continuous(limits = c(0, 125)) +
      labs(x = "Zabici kopnięciem konia w korpusie w ciągu roku", y = "Liczba korpuso-lat") +
      theme_upwr(grid = "y")
    if (step >= 2) {
      p <- p + geom_vline(xintercept = bk_mean, colour = upwr_accent, linetype = "22", linewidth = 0.9) +
        annotate("text", x = bk_mean, y = 118, hjust = -0.05, colour = upwr_accent, fontface = "bold",
                 label = sprintf("średnia %s, wariancja %s", lc_fmt(bk_mean, 2), lc_fmt(bk_var, 2)))
    }
    if (step >= 3) {
      p <- p + geom_point(aes(y = exp), shape = 21, fill = "white", colour = upwr_accent, size = 4, stroke = 1.6) +
        geom_text(aes(y = exp, label = lc_fmt(exp, 1)), colour = upwr_accent, hjust = -0.6, size = 3.5)
    }
    p
  }))

  s3p_step_text(output, "s3p_3b", step_3b, list(
    "Władysław Bortkiewicz, statystyk z Petersburga, zebrał w 1898 roku dane z pruskiej armii: 10 korpusów kawalerii przez 20 lat, razem 200 korpuso-lat. W każdym policzył żołnierzy zabitych kopnięciem konia.",
    sprintf("Razem 122 zgony na 200 korpuso-lat: średnia %s. Wariancja wynosi %s, czyli prawie tyle samo. Równość E(X) = Var(X) to znak rozpoznawczy rozkładu Poissona.",
      lc_fmt(bk_mean, 2), lc_fmt(bk_var, 2)),
    "Kółka to oczekiwane liczby z Pois(0.61): 108.7, 66.3, 20.2, 4.1 i 0.6. Rzadkie, niezależne zdarzenia w wielu jednostkach dają rozkład Poissona nawet wtedy, gdy chodzi o konie."
  ))

  # --- 3g -------------------------------------------------------------------
  scene_texts(input, output, "s3p_3g", list(
    "Na Ślężę prowadzi pięć równie długich i równie ładnych szlaków. Jeden kolega stoi na szczycie, drugi na dole. Każdy wybiera szlak na chybił trafił i ruszają jednocześnie. Jaka jest szansa, że spotkają się na szlaku?",
    "Teraz zawody: sześć par w różnych koszulkach, w każdej jeden zawodnik na szczycie i jeden na dole. Punkt zdobywa para, która spotka się na szlaku. X to liczba par, które się spotkały.",
    "Medale rozdamy tylko wtedy, gdy spotkają się co najmniej trzy pary. Jak często tak się zdarza? Rozegraj wiele zawodów.",
    tagList("Kółka pokazują model: sześć niezależnych par, każda spotyka się z szansą 1/5. To rozkład dwumianowy B(6, 0.2).",
      tags$br(), tags$em(class = "s3p-dedic", "Dedykacja: śp. dr hab. Andrzej Michalski"))
  ))
}
