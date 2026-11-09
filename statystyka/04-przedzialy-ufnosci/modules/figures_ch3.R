# RYCINY — rozdział 3 -----------------------------------------------------------
#   fig_3a_stats() — przedziały grup i różnicy dla Ryc. 3.4 (ci_explorers.R).

# ============================================================================
# (3a) Dwie linie płatków — obliczenia dla Ryc. 3.4
# ============================================================================

fig_3a_n   <- 120
fig_3a_sd1 <- 1.02
fig_3a_sd2 <- 0.94
fig_3a_m2  <- 9.08   # średnia linii 2; linia 1 ma średnią m2 + d

# Wszystkie wielkości potrzebne do rysunku: przedziały grup, przedział różnicy,
# granice stref (d_ist: różnica istotna, d_roz: przedziały grup rozłączne).
fig_3a_stats <- function(d) {
  n    <- fig_3a_n
  se1  <- fig_3a_sd1 / sqrt(n)
  se2  <- fig_3a_sd2 / sqrt(n)
  m1   <- fig_3a_m2 + d
  m2   <- fig_3a_m2
  t_g  <- qt(0.975, df = n - 1)
  me1  <- t_g * se1
  me2  <- t_g * se2
  se_d <- sqrt(se1^2 + se2^2)
  v1   <- se1^2
  v2   <- se2^2
  df_w <- (v1 + v2)^2 / (v1^2 / (n - 1) + v2^2 / (n - 1))
  t_d  <- qt(0.975, df = df_w)
  me_d <- t_d * se_d
  d_ist <- me_d
  d_roz <- me1 + me2
  zone  <- if (d < d_ist) "nieistotna" else if (d < d_roz) "nakladaja" else "rozlaczne"
  list(d = d, n = n, se1 = se1, se2 = se2, se_d = se_d, df_w = df_w, t_d = t_d,
       m1 = m1, m2 = m2,
       ci1 = c(m1 - me1, m1 + me1), ci2 = c(m2 - me2, m2 + me2),
       ci_d = c(d - me_d, d + me_d),
       d_ist = d_ist, d_roz = d_roz, zone = zone)
}
