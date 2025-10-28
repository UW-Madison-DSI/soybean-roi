# ──────────────────────────────────────────────────
# Global model parameters & fungicide specs
# ──────────────────────────────────────────────────
MODEL_PARAMS <- list(
  b0   = 0.0733,
  b1   = 0.55,
  b0_se = 0.029,
  b1_se = 0.204
)


FUNGICIDES_SPECS <- list(
  list(
    fungicide_program    = "Cobra-R1",
    alias                = "Cobra R1",
    rate = "8 fl oz/a",
    product_and_application_cost = 12.7,
    application = 'Beginning flowering',
    proportional_disease_reduction = 0.498969
  ),
  list(
    fungicide_program    = "Cobra-V4/V5",
    alias                = "Cobra V4/V5",
    rate = "8 fl oz/a",
    product_and_application_cost = 12.70,
    application = '4th-5th trifoliolate',
    proportional_disease_reduction = 0.8233
  ),
  list(
    fungicide_program    = "DelaroComplete-R3",
    alias                = "Delaro Complete R3",
    rate = "8 fl oz/a",
    product_and_application_cost = 37.03,
    application = 'Beginning pod',
    proportional_disease_reduction = 0.7076
  ),
  list(
    fungicide_program    = "Endura-R1-Endura-R3",
    alias                = "Endura R1 fb R3",
    rate = "8 fl oz/a",
    product_and_application_cost = 112.0,
    application = 'Beginning pod',
    proportional_disease_reduction = 0.35
  ),
  list(
    fungicide_program    = "Endura-R3",
    alias                = "Endura R3",
    rate = "8 fl oz/a",
    product_and_application_cost = 51.32,
    application = 'Beginning pod',
    proportional_disease_reduction = 0.4755
  ),
  list(
    fungicide_program    = "Lektivar/Omega-R1-MiravisNeo-R3",
    alias                = "Omega R1 fb Miravis Neo R3",
    rate = "16+16 fl oz/a",
    product_and_application_cost = 106.96,
    application = 'Beginning flowering fb Beginning pod',
    proportional_disease_reduction = 0.6861
  ),
  list(
    fungicide_program    = "Lektivar/Omega-R3",
    alias                = "Omega R3",
    rate = "16 fl oz/a",
    product_and_application_cost = 69.04,
    application = 'Beginning pod',
    proportional_disease_reduction = 0.6305
  ),
  list(
    fungicide_program    = "MiravisNeo-R3",
    alias                = "Miravis Neo R3",
    rate = "16 fl oz/a",
    product_and_application_cost = 37.92,
    application = 'Beginning pod',
    proportional_disease_reduction = 0.826
  )
)


# ──────────────────────────────────────────────────
# Helper functions (validate, calc_yield_params, calc_netbenefit_metrics, process_fungicide)
# ──────────────────────────────────────────────────

validate_inputs <- function(yield, soybean_price, disease_index) {
  errors <- character(0)
  
  if (is.null(yield) || !is.numeric(yield)) {
    errors <- c(errors, "Yield is required and must be numeric")
  } else if (yield <= 20 || yield > 120) {
    errors <- c(errors, "Yield must be between 0 and 200 bushels/acre")
  }
  
  if (is.null(soybean_price) || !is.numeric(soybean_price)) {
    errors <- c(errors, "Soybean price is required and must be numeric")
  } else if (soybean_price <= 8 || soybean_price > 30) {
    errors <- c(errors, "Soybean price must be between 0 and $50/bushel")
  }
  
  if (is.null(disease_index) || !is.numeric(disease_index)) {
    errors <- c(errors, "Disease index is required and must be numeric")
  } else if (disease_index < 0 || disease_index > 100) {
    errors <- c(errors, "Disease index must be between 0 and 100 percent")
  }
  
  list(valid = length(errors) == 0, errors = errors)
}

calc_yield_params <- function(intercept, slope, intercept_se, slope_se, proportional_disease_reduction, dix_frac) {
  if (any(sapply(list(intercept, slope, intercept_se, slope_se, proportional_disease_reduction, dix_frac), is.null))) {
    return(list(mu = NA, se_mu = NA))
  }
  
  mu <- intercept + slope * (1 - proportional_disease_reduction) * dix_frac
  se_mu <- sqrt(intercept_se^2 + ((1 - proportional_disease_reduction) * dix_frac)^2 * slope_se^2)
  
  list(mu = mu, se_mu = se_mu)
}


calc_netbenefit_metrics <- function(rp, mu, se_mu, cost) {
  expected_nb <- rp * mu - cost
  z_score     <- qnorm(0.975)
  lower_mu    <- mu - z_score * se_mu
  upper_mu    <- mu + z_score * se_mu
  
  ci_low  <- rp * lower_mu - cost
  ci_high <- rp * upper_mu - cost
  
  threshold <- cost / rp
  if (se_mu <= 1e-10) {
    breakeven_prob <- ifelse(mu > threshold, 1, 0)
  } else {
    breakeven_prob <- 1 - pnorm((threshold - mu) / se_mu)
  }
  
  list(
    expected_nb     = expected_nb,
    ci_low          = ci_low,
    ci_high         = ci_high,
    breakeven_prob  = breakeven_prob
  )
}

process_fungicide <- function(fungicide_spec, rp, dix_frac, costs_user = NULL) {
  alias <- fungicide_spec$alias
  default_cost <- fungicide_spec$product_and_application_cost
  
  cost_val <- if (!is.null(costs_user[[alias]])) {
    as.numeric(costs_user[[alias]])
  } else {
    default_cost
  }
  
  yp <- calc_yield_params(
    intercept    = MODEL_PARAMS$b0,
    slope        = MODEL_PARAMS$b1,
    intercept_se = MODEL_PARAMS$b0_se,
    slope_se     = MODEL_PARAMS$b1_se,
    proportional_disease_reduction        = fungicide_spec$proportional_disease_reduction,
    dix_frac     = dix_frac
  )
  
  nbm <- calc_netbenefit_metrics(
    rp    = rp,
    mu    = yp$mu,
    se_mu = yp$se_mu,
    cost  = cost_val
  )
  
  list(
    alias                 = alias,
    fungicide_program     = fungicide_spec$fungicide_program,
    rate                  = fungicide_spec$rate,
    application_timing    = fungicide_spec$application,
    cost_used             = round(cost_val, 2),
    expected_net_benefit  = round(nbm$expected_nb, 2),
    ci_lower_net_benefit  = round(nbm$ci_low, 2),
    ci_upper_net_benefit  = round(nbm$ci_high, 2),
    breakeven_probability = round(nbm$breakeven_prob, 4),
    yield_increase_mean   = round(yp$mu, 4),
    yield_increase_se     = round(yp$se_mu, 4)
  )
}