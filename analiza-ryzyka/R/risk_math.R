# ==========================================================================
# CZYSTE FUNKCJE OBLICZENIOWE — ANALIZA RYZYKA
# Bez zależności od Shiny; wszystkie funkcje są testowalne osobno.
# ==========================================================================

risk_assert_probability <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) || any(x < 0 | x > 1)) {
    stop(sprintf("%s musi należeć do przedziału [0, 1].", name), call. = FALSE)
  }
  invisible(x)
}

risk_assert_count <- function(x, name = deparse(substitute(x)), minimum = 0L) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
    x != as.integer(x) || x < minimum) {
    stop(sprintf("%s musi być liczbą całkowitą nie mniejszą niż %d.", name, minimum),
      call. = FALSE
    )
  }
  invisible(as.integer(x))
}

risk_conditional_counts <- function(total, p_condition, p_event_given_condition,
                                    p_event_given_no_condition) {
  risk_assert_count(total, "total", 1L)
  risk_assert_probability(p_condition)
  risk_assert_probability(p_event_given_condition)
  risk_assert_probability(p_event_given_no_condition)

  condition <- round(total * p_condition)
  no_condition <- total - condition
  event_condition <- round(condition * p_event_given_condition)
  event_no_condition <- round(no_condition * p_event_given_no_condition)

  data.frame(
    condition = c("Warunek B", "Brak warunku B"),
    event = c(event_condition, event_no_condition),
    no_event = c(condition - event_condition, no_condition - event_no_condition),
    total = c(condition, no_condition),
    stringsAsFactors = FALSE
  )
}

risk_total_probability <- function(p_condition, p_event_given_condition,
                                   p_event_given_no_condition) {
  risk_assert_probability(c(
    p_condition, p_event_given_condition,
    p_event_given_no_condition
  ), "prawdopodobieństwa")
  p_condition * p_event_given_condition +
    (1 - p_condition) * p_event_given_no_condition
}

risk_bayes <- function(prevalence, sensitivity, false_positive_rate) {
  risk_assert_probability(
    c(prevalence, sensitivity, false_positive_rate),
    "parametry Bayesa"
  )
  denominator <- sensitivity * prevalence + false_positive_rate * (1 - prevalence)
  if (denominator == 0) {
    return(NA_real_)
  }
  sensitivity * prevalence / denominator
}

risk_detector_counts <- function(population, prevalence, sensitivity,
                                 false_positive_rate) {
  risk_assert_count(population, "population", 1L)
  risk_assert_probability(
    c(prevalence, sensitivity, false_positive_rate),
    "parametry detektora"
  )
  events <- round(population * prevalence)
  non_events <- population - events
  true_positive <- round(events * sensitivity)
  false_positive <- round(non_events * false_positive_rate)
  data.frame(
    state = c("Awaria", "Brak awarii"),
    alarm = c(true_positive, false_positive),
    no_alarm = c(events - true_positive, non_events - false_positive),
    total = c(events, non_events),
    stringsAsFactors = FALSE
  )
}

risk_binomial_probability <- function(n, p, k, query = c("exactly", "at_least", "at_most")) {
  query <- match.arg(query)
  risk_assert_count(n, "n", 1L)
  risk_assert_probability(p)
  risk_assert_count(k, "k", 0L)
  if (k > n) stop("k nie może być większe od n.", call. = FALSE)
  switch(query,
    exactly = stats::dbinom(k, size = n, prob = p),
    at_least = stats::pbinom(k - 1L, size = n, prob = p, lower.tail = FALSE),
    at_most = stats::pbinom(k, size = n, prob = p)
  )
}

risk_at_least_one <- function(n, p) {
  risk_assert_count(n, "n", 1L)
  risk_assert_probability(p)
  1 - (1 - p)^n
}

risk_negative_binomial_total_pmf <- function(total_trials, r, p) {
  risk_assert_count(total_trials, "total_trials", 1L)
  risk_assert_count(r, "r", 1L)
  risk_assert_probability(p)
  if (p == 0) {
    return(0)
  }
  if (total_trials < r) {
    return(0)
  }
  stats::dnbinom(total_trials - r, size = r, prob = p)
}

risk_negative_binomial_finish <- function(limit, r, p) {
  risk_assert_count(limit, "limit", 1L)
  risk_assert_count(r, "r", 1L)
  risk_assert_probability(p)
  if (limit < r || p == 0) {
    return(0)
  }
  stats::pnbinom(limit - r, size = r, prob = p)
}

risk_normal_exceedance <- function(threshold, mean, sd) {
  if (!all(is.finite(c(threshold, mean, sd))) || sd <= 0) {
    stop("Próg i średnia muszą być skończone, a sd dodatnie.", call. = FALSE)
  }
  stats::pnorm(threshold, mean = mean, sd = sd, lower.tail = FALSE)
}

risk_stress_strength_normal <- function(load_mean, load_sd, strength_mean,
                                        strength_sd, rho = 0) {
  values <- c(load_mean, load_sd, strength_mean, strength_sd, rho)
  if (anyNA(values) || any(!is.finite(values)) || load_sd <= 0 || strength_sd <= 0 ||
    rho < -1 || rho > 1) {
    stop("Niepoprawne parametry modelu obciążenie–wytrzymałość.", call. = FALSE)
  }
  difference_mean <- strength_mean - load_mean
  difference_sd <- sqrt(strength_sd^2 + load_sd^2 -
    2 * rho * strength_sd * load_sd)
  if (difference_sd == 0) {
    return(as.numeric(difference_mean < 0))
  }
  stats::pnorm(0, mean = difference_mean, sd = difference_sd)
}

risk_exponential <- function(time, rate) {
  if (any(time < 0) || !is.finite(rate) || rate <= 0) {
    stop("Czas musi być nieujemny, a intensywność dodatnia.", call. = FALSE)
  }
  list(
    density = stats::dexp(time, rate = rate),
    cdf = stats::pexp(time, rate = rate),
    reliability = stats::pexp(time, rate = rate, lower.tail = FALSE),
    hazard = rep(rate, length(time))
  )
}

risk_weibull <- function(time, shape, scale) {
  if (any(time < 0) || !is.finite(shape) || shape <= 0 ||
    !is.finite(scale) || scale <= 0) {
    stop("Czas musi być nieujemny, a parametry Weibulla dodatnie.", call. = FALSE)
  }
  reliability <- stats::pweibull(time, shape = shape, scale = scale, lower.tail = FALSE)
  density <- stats::dweibull(time, shape = shape, scale = scale)
  hazard <- ifelse(reliability > 0, density / reliability, NA_real_)
  list(
    density = density,
    cdf = 1 - reliability,
    reliability = reliability,
    hazard = hazard
  )
}

risk_series_reliability <- function(reliability) {
  risk_assert_probability(reliability, "niezawodności")
  prod(reliability)
}

risk_parallel_reliability <- function(reliability) {
  risk_assert_probability(reliability, "niezawodności")
  1 - prod(1 - reliability)
}

risk_common_cause_reliability <- function(independent_reliability, p_common) {
  risk_assert_probability(
    c(independent_reliability, p_common),
    "parametry wspólnej przyczyny"
  )
  (1 - p_common) * independent_reliability
}

risk_gate_and <- function(probabilities) {
  risk_assert_probability(probabilities, "prawdopodobieństwa wejść AND")
  prod(probabilities)
}

risk_gate_or <- function(probabilities) {
  risk_assert_probability(probabilities, "prawdopodobieństwa wejść OR")
  1 - prod(1 - probabilities)
}

risk_fta_top <- function(p_initiation, p_detection_failure, p_suppression_failure) {
  risk_assert_probability(
    c(p_initiation, p_detection_failure, p_suppression_failure),
    "parametry FTA"
  )
  p_initiation * risk_gate_or(c(p_detection_failure, p_suppression_failure))
}
