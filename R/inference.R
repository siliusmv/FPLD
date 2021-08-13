#' @export
estimateFPLD = function(x,
                        method = "quantiles",
                        positive_support = FALSE,
                        p_vec = NULL,
                        ...) {
  stopifnot(is.numeric(x))
  stopifnot(length(x) > 0)
  if (any(is.na(x))) x = x[!is.na(x)]

  if (method == "quantiles") {
    lambda = tryCatch(
      method_of_quantiles(
        x = x,
        positive_support = positive_support,
        p_vec = p_vec,
        ...),
      error = function(cond) {
        message("something went wrong in method_of_quantiles")
        message(cond$message)
        NA
      }
    )
  } else if (method == "starship") {
    lambda = tryCatch(
      my_starship(
        x = x,
        positive_support = positive_support,
        ...),
      error = function(cond) {
        message("something went wrong in gld::starship")
        NA
      })
  } else if (method == "mle") {
    lambda = tryCatch(
      mle_fpld(
        x,
        positive_support = positive_support,
        ...),
      error = function(cond) {
        message("something went wrong in mle_fpld")
        NA
      })
  } else {
    stop(paste0("The method '", method, "' is not available"))
  }

  lambda
}


#' @export
method_of_quantiles = function(x,
                               lambda0 = NULL,
                               p_vec = NULL,
                               maxit = 2000,
                               positive_support = FALSE,
                               big_support = TRUE,
                               zero_quantile = .01,
                               trace = 0) {

  if (is.null(p_vec)) {
    x = sort(x)
    p_vec = (seq_along(x) - .5) / length(x)
  }
  stopifnot(length(x) == length(p_vec))

  if (is.null(lambda0)) {
    grid = expand.grid(
      l1 = median(x, na.rm = TRUE),
      l2 = log(exp(diff(quantile(x, c(.25, .75)))) - 1),
      l3 = log(1 - c(-0.5, -0.25, 0, 0.25, 0.5)) - log(1 + c(-0.5, -0.25, 0, 0.25, 0.5)),
      l4 = log(exp(c(0.1, 0.2, 0.4, 0.8, 1, 1.5)) - 1),
      l5 = log(exp(.5 + c(-0.4, -0.1, 0.1, 0.2, 0.4, 0.8, 1, 1.5)) - 1)) %>%
      as.matrix() %>%
      t()
    vals = sapply(
      seq_len(ncol(grid)),
      function(i) {
        quantile_loss(grid[, i], x, transformed = TRUE, p_vec = p_vec)
      })
    lambda0 = grid[, which.min(vals)]
  }

  ineq_fun = get_fpld_ineq_func(
    big_support = big_support, positive_support, zero_quantile = zero_quantile)

  opts = list(
    algorithm = "NLOPT_LN_AUGLAG",
    local_opts = list(
      algorithm = "NLOPT_LN_NELDERMEAD",
      maxeval = maxit,
      xtol_rel = 1e-7,
      ranseed = 1),
    maxeval = maxit,
    print_level = trace,
    randseed = 1)

  res = nloptr::nloptr(
    x0 = lambda0,
    opts = opts,
    eval_f = quantile_loss,
    eval_g_ineq = ineq_fun,
    transformed = TRUE,
    xx = x,
    p_vec = p_vec)

  backtransformFPLDParams(res$solution)
}

#' @export
mle_fpld = function(x,
                    lambda0 = NULL,
                    maxit = 2000,
                    positive_support = FALSE,
                    trace = 0) {

  ineq_fun = get_fpld_ineq_func(big_support = FALSE, positive_support)
  if (!is.null(ineq_fun)) {
    ineq_fun_no_p = function(lambda, xx, transformed) ineq_fun(lambda, xx, transformed)
  } else {
    ineq_fun_no_p = NULL
  }

  if (is.null(lambda0)) {
    grid = expand.grid(
      l1 = median(x, na.rm = TRUE),
      l2 = log(exp(diff(quantile(x, c(.25, .75)))) - 1),
      l3 = log(1 - c(-0.5, -0.25, 0, 0.25, 0.5)) - log(1 + c(-0.5, -0.25, 0, 0.25, 0.5)),
      l4 = log(exp(c(0.1, 0.2, 0.4, 0.8, 1, 1.5)) - 1),
      l5 = log(exp(.5 + c(-0.4, -0.1, 0.1, 0.2, 0.4, 0.8, 1, 1.5)) - 1)) %>%
      as.matrix() %>%
      t()
    vals = sapply(
      seq_len(ncol(grid)),
      function(i) {
        neg_log_lik(grid[, i], x, transformed = TRUE)
      })
    lambda0 = grid[, which.min(vals)]
  }

  opts = list(
    algorithm = "NLOPT_LN_AUGLAG",
    local_opts = list(
      algorithm = "NLOPT_LN_NELDERMEAD",
      maxeval = maxit,
      xtol_rel = 1e-7,
      ranseed = 1),
    maxeval = maxit,
    print_level = trace,
    randseed = 1)

  res = nloptr::nloptr(
    x0 = lambda0,
    opts = opts,
    eval_f = neg_log_lik,
    eval_g_ineq = ineq_fun_no_p,
    transformed = TRUE,
    xx = x)

  backtransformFPLDParams(res$solution)
}

neg_log_lik = function(par, xx, transformed) {
  if (transformed) par = backtransformFPLDParams(par)
  lims = qFPLD(c(0, 1), par, transformed = FALSE)
  if (any(is.na(lims)) || min(xx) < lims[1] || max(xx) > lims[2]) return(Inf)
  p = tryCatch(pFPLD(xx, par, transformed = FALSE), error = function(e) NULL)
  if (is.null(p)) return(Inf)
  res = log(par[2]) + log((1 - par[3]) * p ^ (par[4] - 1) + (1 + par[3]) * (1 - p) ^ (par[5] - 1))
  sum(res)
}

quantile_loss = function(par, xx, transformed, p_vec = NULL) {
  if (is.null(p_vec)) {
    xx = sort(xx)
    xx = xx[!is.na(xx)]
    p_vec = (seq_along(xx) - .5) / length(xx)
  }
  #mean((xx - qFPLD(p_vec, par, transformed)) ^ 2)
  mean(abs(xx - qFPLD(p_vec, par, transformed)))
}

get_fpld_ineq_func = function(big_support = TRUE, positive_support = TRUE, l0 = TRUE, zero_quantile = 1e-4) {
  if (!big_support && !positive_support) return(NULL)
  function(lambda, xx, transformed, p_vec = NULL) {
    ineqs = NULL
    if (big_support) {
      ineqs = c(ineqs,
                qFPLD(0, lambda, transformed) - min(xx - .0001),
                max(xx + .0001) - qFPLD(1, lambda, transformed))
    }
    if (positive_support) {
      ineqs = c(ineqs, -1 * qFPLD(zero_quantile, lambda, transformed))
    }
    if (!l0) ineqs = -ineqs
    ineqs
  }
}

#' @export
my_starship = function(x,
                       lambda0 = NULL,
                       maxit = 2000,
                       positive_support = FALSE,
                       big_support = TRUE,
                       trace = 0) {

  x = sort(x)
  
  ineq_fun = get_fpld_ineq_func(big_support = big_support, positive_support)
  if (!is.null(ineq_fun)) {
    ineq_fun_no_p = function(lambda, xx, transformed) ineq_fun(lambda, xx, transformed)
  } else {
    ineq_fun_no_p = NULL
  }

  if (is.null(lambda0)) {
    grid = expand.grid(
      l1 = median(x, na.rm = TRUE),
      l2 = log(exp(diff(quantile(x, c(.25, .75)))) - 1),
      l3 = log(1 - c(-0.5, -0.25, 0, 0.25, 0.5)) - log(1 + c(-0.5, -0.25, 0, 0.25, 0.5)),
      l4 = log(exp(c(0.1, 0.2, 0.4, 0.8, 1, 1.5)) - 1),
      l5 = log(exp(.5 + c(-0.4, -0.1, 0.1, 0.2, 0.4, 0.8, 1, 1.5)) - 1)) %>%
      as.matrix() %>%
      t()
    vals = sapply(
      seq_len(ncol(grid)),
      function(i) {
        starship_obj(grid[, i], x, transformed = TRUE)
      })
    lambda0 = grid[, which.min(vals)]
  }

  opts = list(
    algorithm = "NLOPT_LN_AUGLAG",
    local_opts = list(
      algorithm = "NLOPT_LN_NELDERMEAD",
      maxeval = maxit,
      xtol_rel = 1e-7,
      ranseed = 1),
    maxeval = maxit,
    print_level = trace,
    randseed = 1)

  res = nloptr::nloptr(
    x0 = lambda0,
    opts = opts,
    eval_f = starship_obj,
    eval_g_ineq = ineq_fun_no_p,
    transformed = TRUE,
    xx = x)

  backtransformFPLDParams(res$solution)
}


starship_obj = function(par, xx, transformed) {
    #xx = sort(xx)
    n = length(xx)
    u = pFPLD(xx, par, transformed = transformed)
    i = seq_along(xx)
    s = (2 * i - 1) / n * (log(u) + log(1 - u[n + 1 - i]))
    res = -n - sum(s)
    res
}
