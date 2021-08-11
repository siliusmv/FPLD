#' @export
univariate_simulation_study = function(n_vec,
                                       inference_funs,
                                       score_funs = NULL,
                                       positive_support = FALSE,
                                       lambda = NULL,
                                       num_sim = 100,
                                       num_cores = 4) {

  res = list()
  for (n in n_vec) {
    message("n = ", n)
    res[[n]] = pbapply::pblapply(
      X = 1:num_sim,
      cl = num_cores,
      FUN = function(i) {
        if (is.null(lambda)) lambda = random_lambda(positive_support)
        y = rFPLD(n, lambda, transformed = FALSE)
        res = list()
        for (d in names(inference_funs)) {
          time = proc.time()
          estimate = inference_funs[[d]](y)
          time = proc.time() - time
          res[[d]] = data.frame(
            par = paste0("lambda", 1:5),
            estimate = estimate,
            truth = lambda,
            lower_difference = min(y) - qFPLD(0, estimate),
            upper_difference = qFPLD(1, estimate) - max(y),
            d = d,
            time = sum(time[-3]))
          if (!is.null(score_funs)) {
            res[[d]]$score = score_funs[[d]](y, estimate)
          }
        }
        res = do.call(rbind, res)
        res$iter = i
        res
      }
    ) %>%
      do.call(rbind, .)
    res[[n]]$n = n
  }

  do.call(rbind, res)
}


random_lambda = function(positive_support = FALSE) {
  while (TRUE) {
    lambda = c(
      rnorm(1, 5, 3),
      runif(1, 1.5, 8),
      runif(1, -.9, .9),
      runif(1, .01, .9),
      runif(1, -.3, .7))
    lambda = backtransform_location_scale(lambda)
    if (positive_support && qFPLD(0, lambda) <= 0) {
      # Try one more time
    } else {
      # We don't want the support of the FPLD to be super small
      if (diff(qFPLD(c(0, 1), lambda)) > 1) break
    }
  }
  lambda
}

