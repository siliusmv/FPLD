
#' @export
crps_gld = function(y, par) {
  if (any(is.na(par))) return(rep(NA, length(y)))
  ρ = function(u, p) p * u - ifelse(u < 0, u, 0)
  crps = rep(NA, length(y))
  for (i in seq_along(y)) {
    crps[i] = integrate(function(p) 2 * ρ(y[i] - gld::qgl(p, par), p), 0, 1)$value
  }
  crps
}

#' @export
crps_fpld = function(y, par) {
  if (any(is.na(par))) return(rep(NA, length(y)))
  if (par[4] == 0) par[4] = .5 * .Machine$double.eps # Lazy solution, but works well
  if (par[5] == 0) par[5] = .5 * .Machine$double.eps # Lazy solution, but works well
  F = pFPLD(y, par)
  y * (2 * F - 1) - 2 * integrate_p_times_fpld_simple(par) + 2 * integrate_fpld_simple(par, F)
}

#' @export
pit_fpld = function(y, par) {
  pFPLD(y, par)
}

#' @export
scrps_fpld = function(y, par) {
  crps = crps_fpld(y, par)
  S = abs(expected_crps_fpld(par, par))
  crps / S + log(S)
}

#' @export
expected_scrps_fpld = function(par, par_truth) {
  expected_crps = expected_crps_fpld(par, par_truth)
  S = abs(expected_crps_fpld(par, par))
  expected_crps / S + log(S)
}

#' @export
expected_crps_fpld = function(par, par_truth) {
  if (any(is.na(par))) return(NA)
  res = tryCatch({
    integrate(
      f = function(y, par) crps_fpld(y, par) * dFPLD(y, par_truth),
      par = par,
      lower = qFPLD(.00001, par_truth),
      upper = qFPLD(.99999, par_truth))$value
  }, error = function(e) NULL)
  if (is.null(res)) {
    res = mean(crps_fpld(rFPLD(1000, par_truth), par))
  }
  res
}

integrate_fpld_simple = function(par, lower) {
  (1 - lower) * par[1] + par[2] / 2 * ((1 - par[3]) * (
    (1 - lower^(par[4] + 1)) / (par[4] * (par[4] + 1)) - (1 - lower) / par[4]
  ) - (1 + par[3]) * (
    (1 - lower)^(par[5] + 1) / (par[5] * (par[5] + 1)) - (1 - lower) / par[5]
  ))
}

#' This is simply used as a test that integrate_fpld_simple works as it should
integrate_fpld = function(par, lower = 0, upper = 1) {
  (upper - lower) * (par[1] + par[2] / 2 * ((1 + par[3]) / par[5] - (1 - par[3]) / par[4])) +
    par[2] / 2 * (1 - par[3]) * (upper ^ (par[4] + 1) - lower ^ (par[4] + 1)) / (par[4] * (par[4] + 1)) +
    par[2] / 2 * (1 + par[3]) * ((1 - upper) ^ (par[5] + 1) - (1 - lower) ^ (par[5] + 1)) / (par[5] * (par[5] + 1))
}

integrate_p_times_fpld_simple = function(par) {
  .5 * par[1] + par[2] / 2 * (
    -(1 - par[3]) / (2 * par[4] + 4) +
      (1 + par[3]) * (par[5] + 3) / (2 * (par[5] + 1) * (par[5] + 2))
  )
}

#' This is simply used as a test that integrate_p_times_fpld_simple works as it should
integrate_p_times_fpld = function(par, lower = 0, upper = 1) {
  .5 * (upper^2 - lower^2) * (par[1] + par[2] / 2 * ((1 + par[3]) / par[5] - (1 - par[3]) / par[4])) +
    par[2] / 2 * (1 - par[3]) * (upper ^ (par[4] + 2) - lower ^ (par[4] + 2)) / (par[4] * (par[4] + 2)) +
      par[2] / 2 * (1 + par[3]) / ((par[5]) * (par[5] + 1) * (par[5] + 2)) *
      ((1 - upper) ^ (par[5] + 1) * (par[5] * upper + upper + 1) -
         (1 - lower) ^ (par[5] + 1) * (par[5] * lower + lower + 1))
}

