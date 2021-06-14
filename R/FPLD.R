# ==================================================================
# Distribution functions for the five parameter lambda distribution.
# qFPLD - quantile function
# dFPLD - estimation of probability density function
# pFPLD - estimation of cumulative distribution function
# rFPLD - sample from the FPLD
# densqFPLD - quantile density function
# transformFPLDParams - transform the FPLD parameters to some that are
#   better suited for numerical optimisation
# backtransformFPLDParams - transform the FPLD parameters back after
#   having performed numerical optimisation
# ==================================================================

#' @export
qFPLD = function(p, par, transformed = FALSE) {
  if (transformed) par = backtransformFPLDParams(par)
  if (par[3] == 1) {
    left = 0
  } else {
    if (par[4] == 0) {
      left = (1 - par[3]) * log(p)
    } else {
      left = (1 - par[3]) * (p ^ par[4] - 1) / par[4]
    }
  }
  if (par[3] == -1) {
    right = 0
  } else {
    if (par[5] == 0) {
      right = (1 + par[3]) * log(1 - p)
    } else {
      right = (1 + par[3]) * ((1 - p) ^ par[5] - 1) / par[5]
    }
  }
  res = par[1] + (par[2] / 2) * (left - right)
  res
}

#' @export
dFPLD = function(x, par, transformed = FALSE) {
  if (transformed) par = backtransformFPLDParams(par)
  gld::dgl(x, lambda1 = par[1], lambda2 = 2 / par[2],
           lambda3 = par[4], lambda4 = par[5],
           lambda5 = par[3], param = "fm5")
}

#' @export
pFPLD = function(x, par, transformed = FALSE) {
  if (transformed) par = backtransformFPLDParams(par)
  gld::pgl(x, lambda1 = par[1], lambda2 = 2 / par[2],
           lambda3 = par[4], lambda4 = par[5],
           lambda5 = par[3], param = "fm5")
}

#' @export
densqFPLD = function(p, par, transformed = FALSE) {
  if (transformed) par = backtransformFPLDParams(par)
  (par[2] / 2) * (
    (1 - par[3]) * p ^ (par[4] - 1) +
    (1 + par[3]) * (1 - p) ^ (par[5] - 1))
}

#' @export
rFPLD = function(n, par, transformed = FALSE) {
  qFPLD(runif(n), par, transformed)
}

#' @export
transformFPLDParams = function(par, a = .25, b = .75) {
  par[1] = qFPLD(.5, par)
  iqr = qFPLD(b, par) - qFPLD(a, par)
  par[2] = log(exp(iqr) - 1)
  par[3] = log(1 - par[3]) - log(1 + par[3])
  par[4] = log(exp(par[4]) - 1)
  par[5] = log(exp(par[5] + .5) - 1)
  par
}

#' @export
backtransformFPLDParams = function(par, a = .25, b = .75) {
  par[3] = (2 / (1 + exp(par[3]))) - 1
  par[4] = log(1 + exp(par[4]))
  par[5] = log(1 + exp(par[5])) - .5
  par[2] = (log(1 + exp(par[2]))) / (qFPLD(b, c(0, 1, par[3:5])) - qFPLD(a, c(0, 1, par[3:5])))
  par[1] = par[1] - qFPLD(.5, c(0, par[2:5]))
  par
}
