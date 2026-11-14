# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rc: P5A i P5B (karta detekcji, przykład 10.3) ----
#
#   i10p_5a_ui — Scena „10 000 misji, dwa pytania” (P5A, kadry policzone w R): ta sama
#                tabela 2 × 2 czytana dwa razy. Operator czyta kolumnę alarmów
#                (P(I | alarm)), analityk wiersz zapotrzebowań (P(D | I) = liść drzewa).
#   i10p_5b_ui — Piaskownica „Co zmienia FPR, a co czułość” (P5B): scena JS „detektor”
#                (modules/proto_rc.js). Dwa paski 100%: P(D | I) do drzewa i P(I | alarm)
#                dla operatora; suwaki FPR i czułości.
# Wymaga: stop_proto_common.R (i10p_cfg, i10p_p, i10p_num, i10p_frames_ui, i10p_scene_ui,
# i10p_slider), R/risk_math.R (risk_bayes).

# Populacja porównywalnych misji dla częstości naturalnych (10 000 misji).
i10p5_n <- 10000

# Liczebności tabeli 2 × 2 na i10p5_n misji (wszystkie liczone z i10p_cfg).
i10p5_t <- list(
  need     = i10p5_n * i10p_cfg$init,                          # zapotrzebowanie
  no_need  = i10p5_n * (1 - i10p_cfg$init),                    # brak zapotrzebowania
  true_al  = i10p5_n * i10p_cfg$init * i10p_cfg$sens,          # alarm prawdziwy
  miss     = i10p5_n * i10p_cfg$init * (1 - i10p_cfg$sens),    # przeoczenie
  false_al = i10p5_n * (1 - i10p_cfg$init) * i10p_cfg$fpr,     # alarm fałszywy
  calm     = i10p5_n * (1 - i10p_cfg$init) * (1 - i10p_cfg$fpr) # spokój
)
i10p5_post <- risk_bayes(i10p_cfg$init, i10p_cfg$sens, i10p_cfg$fpr)  # P(I | alarm)
i10p5_dI   <- 1 - i10p_cfg$sens                                        # P(D | I), liść drzewa
stopifnot(
  abs(i10p5_t$true_al / (i10p5_t$true_al + i10p5_t$false_al) - i10p5_post) < 1e-12,
  abs(i10p5_t$miss / i10p5_t$need - i10p5_dI) < 1e-12
)

# Liczba z kropką, tysiące spacją w tekstach („10 000”).
i10p5_thou <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)

# ============================================================================
# P5A — SCENA „10 000 MISJI, DWA PYTANIA”
# ============================================================================

# Geometria tabeli: kolumny (alarm, brak alarmu), wiersze (zapotrzebowanie, brak).
i10p5_col_x <- c(196, 424)
i10p5_col_w <- 220
i10p5_row_y <- c(50, 168)
i10p5_row_h <- 110

# Komórka tabeli: prostokąt, liczba (mono) i podpis. dim = przygaszona (kadry 3–4).
i10p5_cell <- function(row, col, cls, num = NULL, cap = NULL, dim = FALSE) {
  x <- i10p5_col_x[col]; y <- i10p5_row_y[row]
  w <- i10p5_col_w; h <- i10p5_row_h
  cx <- x + w / 2; cy <- y + h / 2
  body <- sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="6"/>', cls, x, y, w, h)
  if (!is.null(num)) body <- paste0(body,
    sprintf('<text class="i10p-mono" x="%g" y="%g" font-size="26" font-weight="700" text-anchor="middle">%s</text>', cx, cy + 4, num),
    sprintf('<text class="i10p-subtle" x="%g" y="%g" font-size="13" text-anchor="middle">%s</text>', cx, cy + 28, cap))
  if (dim) sprintf('<g opacity=".4">%s</g>', body) else body
}

i10p5a_svg <- function(step) {
  t <- i10p5_t
  num <- i10p_num
  # Komórki: (wiersz, kolumna) → liczba i podpis.
  cell_val <- list(
    "1_1" = list(num = num(t$true_al), cap = "alarm prawdziwy"),
    "1_2" = list(num = num(t$miss), cap = "przeoczenie"),
    "2_1" = list(num = num(t$false_al), cap = "alarm fałszywy"),
    "2_2" = list(num = num(t$calm), cap = "spokój")
  )
  cell <- function(row, col) {
    v <- cell_val[[paste0(row, "_", col)]]
    if (step == 1) return(i10p5_cell(row, col, "i10p-fill-idle"))
    cls <- "i10p-paper"; dim <- FALSE
    if (step == 3) {
      if (col == 1) {
        cls <- if (row == 1) "i10p-box-ok" else "i10p-box-hit"
      } else {
        dim <- TRUE
      }
    }
    if (step == 4) {
      if (row == 1) {
        cls <- if (col == 1) "i10p-box-ok" else "i10p-box-hit"
      } else {
        dim <- TRUE
      }
    }
    i10p5_cell(row, col, cls, v$num, v$cap, dim)
  }
  # Nagłówki kolumn i sumy kolumn (od kadru 2); druga kolumna przygaszona w kadrze 3.
  col_names <- c("alarm", "brak alarmu")
  col_sums <- c(num(t$true_al + t$false_al), num(t$miss + t$calm))
  col_head <- function(col) {
    x <- i10p5_col_x[col] + i10p5_col_w / 2
    dim <- step == 3 && col == 2
    s <- sprintf('<text class="i10p-name" x="%g" y="34" font-size="15" text-anchor="middle">%s</text>', x, col_names[col])
    sum_txt <- sprintf('<text class="i10p-mono i10p-subtle" x="%g" y="306" font-size="16" text-anchor="middle">%s</text>', x, col_sums[col])
    if (dim) {
      c(sprintf('<g opacity=".4">%s</g>', s), sprintf('<g opacity=".4">%s</g>', sum_txt))
    } else {
      c(s, sum_txt)
    }
  }
  # Etykiety wierszy i sumy wierszy (od kadru 1).
  row_names <- c("zapotrzebowanie", "brak zapotrzebowania")
  row_sums <- c(num(t$need), num(t$no_need))
  row_part <- function(row) {
    cy <- i10p5_row_y[row] + i10p5_row_h / 2
    dim <- step == 4 && row == 2
    s <- c(
      sprintf('<text class="i10p-name" x="12" y="%g" font-size="14">%s</text>', cy + 5, row_names[row]),
      sprintf('<text class="i10p-mono" x="668" y="%g" font-size="18" font-weight="700">%s</text>', cy + 6, row_sums[row])
    )
    if (dim) sprintf('<g opacity=".4">%s</g>', paste(s, collapse = "")) else paste(s, collapse = "")
  }
  body <- c(
    sprintf('<text class="i10p-subtle" x="668" y="34" font-size="12">suma</text>'),
    if (step >= 2) c(col_head(1), col_head(2), sprintf('<text class="i10p-subtle" x="186" y="306" font-size="12" text-anchor="end">suma</text>')),
    vapply(1:2, row_part, character(1)),
    vapply(1:2, function(r) paste(vapply(1:2, function(c) cell(r, c), character(1)), collapse = ""), character(1))
  )
  # Kolejność: najpierw tło i sumy, potem komórki (komórki nie nachodzą na napisy).
  aria <- c(
    "Tabela z sumami: zapotrzebowanie 50 i brak zapotrzebowania 9950 misji.",
    sprintf("Tabela 2 na 2 z liczbami: alarm prawdziwy %s, przeoczenie %s, alarm fałszywy %s, spokój %s.",
      num(t$true_al), num(t$miss), num(t$false_al), num(t$calm)),
    sprintf("Tabela z kolumną alarmów wyróżnioną: %s alarmów prawdziwych i %s fałszywych z %s alarmów.",
      num(t$true_al), num(t$false_al), num(t$true_al + t$false_al)),
    sprintf("Tabela z wierszem zapotrzebowań wyróżnionym: %s alarmów i %s przeoczeń z %s misji.",
      num(t$true_al), num(t$miss), num(t$need))
  )[step]
  i10p_svg(800, 312, aria, paste(unlist(body), collapse = ""))
}

i10p5a_frames <- list(
  list(name = "Misje",
    text = sprintf("Na %s porównywalnych misji zapotrzebowanie na chłodzenie pojawia się w %s.",
      i10p5_thou(i10p5_n), i10p_num(i10p5_t$need)),
    read = NULL),
  list(name = "Alarmy",
    text = sprintf("Detektor alarmuje w %s z %s misji z zapotrzebowaniem i w %s z %s pozostałych.",
      i10p_num(i10p5_t$true_al), i10p_num(i10p5_t$need), i10p_num(i10p5_t$false_al), i10p_num(i10p5_t$no_need)),
    read = NULL),
  list(name = "Pytanie operatora",
    text = "Operator widzi alarm i pyta, jak mu wierzyć. Czyta kolumnę alarmów.",
    read = sprintf("P(I | alarm) = %s / %s ≈ %s",
      i10p_num(i10p5_t$true_al), i10p_num(i10p5_t$true_al + i10p5_t$false_al), i10p_p(i10p5_post))),
  list(name = "Pytanie analityka",
    text = "Analityk pyta, jak często detektor milczy, gdy powinien alarmować. Czyta wiersz zapotrzebowań.",
    read = sprintf("P(D | I) = %s / %s = %s → liść drzewa",
      i10p_num(i10p5_t$miss), i10p_num(i10p5_t$need), i10p_p(i10p5_dI)))
)

i10p_5a_ui <- i10p_frames_ui("P5A", "pojęcie (kadry)", "10 000 misji, dwa pytania",
  i10p5a_frames, i10p5a_svg)

# ============================================================================
# P5B — PIASKOWNICA „CO ZMIENIA FPR, A CO CZUŁOŚĆ”
# ============================================================================

# Scena „detektor” w modules/proto_rc.js; parametry z i10p_cfg i populacji i10p5_n.
i10p_5b_ui <- i10p_scene_ui("P5B", "piaskownica", "Co zmienia FPR, a co czułość", "detektor",
  c(i10p_cfg, list(n = i10p5_n)),
  tags$div(class = "i10p-nav",
    i10p_slider("i10p5b_fpr", "FPR: P(alarm | brak I)", 0, 0.2, i10p_cfg$fpr, 0.005),
    i10p_slider("i10p5b_sens", "Czułość: P(alarm | I)", 0.5, 1, i10p_cfg$sens, 0.01)),
  i10p_out("stage"),
  i10p_out("read"),
  lc_caption("Do drzewa trafia tylko P(D | I). FPR zmienia zaufanie do alarmu i koszt zbędnych reakcji."))
