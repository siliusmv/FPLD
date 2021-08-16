library(FPLD)
library(scoringRules)
library(pbapply)
library(MASS)
library(gld)

pbapply::pboptions(type = "timer")

num_cores = 6

df = readRDS(range_file())

combos = df %>%
  dplyr::select(id, season) %>%
  dplyr::distinct()

set.seed(123, kind = "L'Ecuyer-CMRG")
pars = pbapply::pblapply(
  seq_len(nrow(combos)),
  cl = num_cores,
  FUN = function(i) {
    obs = dplyr::filter(df, id == combos$id[i], season == combos$season[i])$range
    estimate = estimateFPLD(obs, positive_support = TRUE)
    crps = crps_fpld(obs, estimate)
    pit = pit_fpld(obs, estimate)
    data.frame(estimate = estimate,
               crps = mean(crps),
               pit_e1 = mean(pit) - .5,
               pit_e2 = sd(pit) - 1 / sqrt(12),
               par = paste0("lambda", 1:5),
               id = combos$id[i],
               season = combos$season[i])
  })

pars = do.call(rbind, pars)
saveRDS(pars, file.path(data_dir(), "local_fpld_pars.rds"))


distributions = list(
  fpld = function(x) estimateFPLD(x, positive_support = TRUE),
  fpld_mle = function(x) estimateFPLD(x, positive_support = TRUE, method = "mle"),
  fpld_starship = function(x) estimateFPLD(x, positive_support = TRUE, method = "starship"),
  gamma = function(x) MASS::fitdistr(x, "gamma")$estimate,
  exponential = function(x) MASS::fitdistr(x, "exponential")$estimate,
  gaussian = function(x) MASS::fitdistr(x, "normal")$estimate,
  lognormal = function(x) MASS::fitdistr(x, "lognormal")$estimate,
  gld = function(x) gld::fit.fkml(x)$lambda)

parnames = list(
  fpld = paste0("lambda", 1:5),
  fpld_mle = paste0("lambda", 1:5),
  fpld_starship = paste0("lambda", 1:5),
  gamma = c("shape", "rate"),
  exponential = "rate",
  gaussian = c("mean", "sd"),
  lognormal = c("meanlog", "sdlog"),
  gld = paste0("lambda", 1:4))

crps_func = list(
  fpld = function(x, par) mean(crps_fpld(x, par)),
  fpld_mle = function(x, par) mean(crps_fpld(x, par)),
  fpld_starship = function(x, par) mean(crps_fpld(x, par)),
  gamma = function(x, par) mean(scoringRules::crps_gamma(x, par[1], par[2])),
  exponential = function(x, par) mean(scoringRules::crps_exp(x, par)),
  gaussian = function(x, par) mean(scoringRules::crps_norm(x, par[1], par[2])),
  lognormal = function(x, par) mean(scoringRules::crps_lnorm(x, par[1], par[2])),
  gld = function(x, par) mean(crps_gld(x, par)))

quantile_func = list(
  fpld = function(p, par) qFPLD(p, par),
  fpld_mle = function(p, par) qFPLD(p, par),
  fpld_starship = function(p, par) qFPLD(p, par),
  gamma = function(p, par) qgamma(p, par[1], par[2]),
  exponential = function(p, par) qexp(p, par),
  gaussian = function(p, par) qnorm(p, par[1], par[2]),
  lognormal = function(p, par) qlnorm(p, par[1], par[2]),
  gld = function(p, par) gld::qgl(p, par[1], par[2], par[3], par[4]))

cdf_func = list(
  fpld = function(x, par) pFPLD(x, par),
  fpld_mle = function(x, par) pFPLD(x, par),
  fpld_starship = function(x, par) pFPLD(x, par),
  gamma = function(x, par) pgamma(x, par[1], par[2]),
  exponential = function(x, par) pexp(x, par),
  gaussian = function(x, par) pnorm(x, par[1], par[2]),
  lognormal = function(x, par) plnorm(x, par[1], par[2]),
  gld = function(x, par) gld::pgl(x, par[1], par[2], par[3], par[4]))

set.seed(123, kind = "L'Ecuyer-CMRG")
pars = pbapply::pblapply(
  seq_len(nrow(combos)),
  cl = num_cores,
  FUN = function(i) {
    obs = dplyr::filter(df, id == combos$id[i], season == combos$season[i])$range
    res = list()
    for (d in names(distributions)) {
      time = proc.time()
      estimate = distributions[[d]](obs)
      time = proc.time() - time
      res[[d]] = data.frame(
        estimate = estimate,
        distribution = d,
        par = parnames[[d]],
        time = sum(time[-3]),
        id = combos$id[i],
        season = combos$season[i])
    }
    do.call(rbind, res)
  })

pars = do.call(rbind, pars)
row.names(pars) = NULL
saveRDS(pars, file.path(data_dir(), "local_multiple_distributions.rds"))

combos = dplyr::distinct(pars, id, season)
set.seed(123, kind = "L'Ecuyer-CMRG")
pars = pbapply::pblapply(
  seq_len(nrow(combos)),
  cl = num_cores,
  FUN = function(i) {
    tmp = dplyr::filter(pars, id == combos$id[i], season == combos$season[i])
    obs = dplyr::filter(df, id == combos$id[i], season == combos$season[i])$range
    res = list()
    for (d in names(distributions)) {
      res[[d]] = dplyr::filter(tmp, distribution == d)
      res[[d]]$crps = crps_func[[d]](obs, res[[d]]$estimate)
      res[[d]]$lower_difference = min(obs) - quantile_func[[d]](0, res[[d]]$estimate)
      res[[d]]$upper_difference = quantile_func[[d]](1, res[[d]]$estimate) - max(obs)
      res[[d]]$lower_boundary = quantile_func[[d]](0, res[[d]]$estimate)
      res[[d]]$lower_prob = cdf_func[[d]](0, res[[d]]$estimate)
    }
    do.call(rbind, res)
  })

pars = do.call(rbind, pars)
row.names(pars) = NULL
saveRDS(pars, file.path(data_dir(), "local_multiple_distributions.rds"))
