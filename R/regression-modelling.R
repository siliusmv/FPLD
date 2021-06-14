
#' @export
quantile_regression = function(p, df, covariate_names, num_cores = 1) {
  formula = as.formula(paste("range ~", paste(covariate_names, collapse = " + ")))
  parallel::mclapply(X = p, mc.cores = num_cores, FUN = quantreg::rq, formula = formula, data = df)
}
