library(FPLD)
library(dplyr)
library(pbapply)
library(ggplot2)

pbapply::pboptions(type = "timer")

source("simulation_functions.R") # Some functions

# Which methods are we testing?
inference_funs = list(
  MQ = function(x) estimateFPLD(x),
  ML = function(x) estimateFPLD(x, method = "mle"),
  starship = function(x) estimateFPLD(x, method = "starship"))

score_funs = rep(list(function(y, par) mean(crps_fpld(y, par))), 3)
names(score_funs) = c("MQ", "ML", "starship")

n_vec = 2^(7:14)
num_cores = 10
num_sim = 500

# Draw random lambdas
filename = file.path(data_dir(), "MQ_simulation_lambda_random.rds")

set.seed(123, kind = "L'Ecuyer-CMRG")
df = univariate_simulation_study(
  lambda = NULL,
  n_vec = n_vec,
  score_funs = score_funs,
  inference_funs = inference_funs,
  num_sim = num_sim,
  num_cores = num_cores)

saveRDS(df, filename)

if (interactive()) {
  table = crps_latex_table(df)
}


# Draw random and positive lambdas
inference_funs = list(
  MQ = function(x) estimateFPLD(x, positive_support = TRUE),
  ML = function(x) estimateFPLD(x, method = "mle", positive_support = TRUE),
  starship = function(x) estimateFPLD(x, method = "starship", positive_support = TRUE))

score_funs = rep(list(function(y, par) mean(crps_fpld(y, par))), 3)
names(score_funs) = c("MQ", "ML", "starship")

n_vec = 2^(7:14)
num_cores = 10
num_sim = 500

filename = file.path(data_dir(), "MQ_simulation_lambda_random_pos.rds")

set.seed(123, kind = "L'Ecuyer-CMRG")
df = univariate_simulation_study(
  lambda = NULL,
  positive_support = TRUE,
  n_vec = n_vec,
  inference_funs = inference_funs,
  score_funs = score_funs,
  num_sim = num_sim,
  num_cores = num_cores)

saveRDS(df, filename)

if (interactive()) {
  table = crps_latex_table(df)
}

