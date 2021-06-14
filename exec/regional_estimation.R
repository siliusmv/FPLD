
library(FPLD)
library(sf)
library(dplyr)
library(pbapply)
pbapply::pboptions(type = "timer")

df = readRDS(range_file())
coords = readRDS(coords_file())

p_vec = seq(.01, .99, length.out = 99)
seasons = unique(df$season)
covariate_names = c("x", "y", "dist_sea", "mean_tam", "sd_tam", "height")
num_cores = 10


# In-sample estimation =======================================================
formula = as.formula(paste("range ~", paste(covariate_names, collapse = " + ")))
coefficients = list()
for (s in seasons) {
  data = dplyr::filter(df, season == s) %>%
    dplyr::left_join(dplyr::filter(coords, season == s), by = "id") %>%
    dplyr::select(c("range", all_of(covariate_names)))
  models = quantile_regression(p_vec, data, covariate_names, num_cores)
  coefficients[[s]] = sapply(models, `[[`, "coefficients")
  coefficients[[s]] = rbind(coefficients[[s]], p = p_vec)
}
saveRDS(coefficients, file.path(data_dir(), "in-sample-regression.rds"))

# Out-of-sample estimation at all individual stations ========================
formula = as.formula(paste("range ~", paste(covariate_names, collapse = " + ")))
coefficients = list()
for (s in seasons) {
  coefficients[[s]] = pbapply::pblapply(
    X = unique(df$id),
    cl = num_cores,
    FUN = function(id) {
      data = dplyr::filter(df, season == s, id != !!id) %>%
        dplyr::left_join(dplyr::filter(coords, season == s), by = "id") %>%
        dplyr::select(c("range", all_of(covariate_names)))
      models = quantile_regression(p_vec, data, covariate_names)
      sapply(models, `[[`, "coefficients") %>%
        t() %>%
        as.data.frame() %>%
        dplyr::mutate(p = p_vec, id = id, season = s)
    })
}
saveRDS(coefficients, file.path(data_dir(), "out-of-sample-regression.rds"))
for (s in seasons) {
  coefficients[[s]] = do.call(rbind, coefficients[[s]])
}
coefficients = do.call(rbind, coefficients)
saveRDS(coefficients, file.path(data_dir(), "out-of-sample-regression.rds"))

# In-sample estimation at all individual stations, no temperature covariates ========================
covariate_names = c("x", "y", "dist_sea", "height")
formula = as.formula(paste("range ~", paste(covariate_names, collapse = " + ")))
coefficients = list()
for (s in seasons) {
  data = dplyr::filter(df, season == s) %>%
    dplyr::left_join(dplyr::filter(coords, season == s), by = "id") %>%
    dplyr::select(c("range", all_of(covariate_names)))
  models = quantile_regression(p_vec, data, covariate_names, num_cores)
  coefficients[[s]] = sapply(models, `[[`, "coefficients")
  coefficients[[s]] = rbind(coefficients[[s]], p = p_vec)
}
saveRDS(coefficients, file.path(data_dir(), "in-sample-regression-no-temp.rds"))

# ==============================================================================
# Estimate FPLD parameter with the method of quantiles
# ==============================================================================

# In-sample ==================================================================
coefficients = readRDS(file.path(data_dir(), "in-sample-regression.rds"))

res = list()
for (s in names(coefficients)) {
  mycoords = coords %>%
    st_drop_geometry() %>%
    dplyr::filter(season == s)
  X = mycoords %>%
    dplyr::select(all_of(row.names(coefficients[[s]])[-c(1, nrow(coefficients[[s]]))])) %>%
    as.matrix() %>%
    cbind("(Intercept)" = 1, .)
  quantiles = X %*% coefficients[[s]][-nrow(coefficients[[s]]), ]
  params = pbapply::pblapply(
    X = seq_len(nrow(quantiles)),
    cl = num_cores,
    FUN = function(i) estimateFPLD(quantiles[i, ], p_vec = p_vec, positive_support = TRUE))
  if (!all(sapply(params, length) == 5)) {
    stop("Something went wrong when estimating the FPLD parameters")
  }
  res[[s]] = data.frame(
    estimate = unlist(params),
    par = rep(paste0("lambda", 1:5), length(params)),
    id = rep(mycoords$id, each = 5),
    season = s)
}

res = do.call(rbind, res)
row.names(res) = NULL
saveRDS(res, file.path(data_dir(), "in-sample-fpld-pars.rds"))

# Evaluate model fit
combos = dplyr::distinct(res, id, season)
tmp = pbapply::pblapply(
  X = seq_len(nrow(combos)),
  cl = num_cores,
  FUN = function(i) {
    obs = dplyr::filter(df, id == combos$id[i], season == combos$season[i])$range
    par = dplyr::filter(res, id == combos$id[i], season == combos$season[i])$estimate
    pit = pit_fpld(obs, par)
    crps = crps_fpld(obs, par)
    data.frame(pit_e1 = rep(mean(pit) - .5, 5),
               pit_e2 = rep(sd(pit) - 1 / sqrt(12), 5),
               crps = rep(mean(crps), 5))
  })
res = cbind(res, do.call(rbind, tmp))
saveRDS(res, file.path(data_dir(), "in-sample-fpld-pars.rds"))

# In-sample, no temperature ==================================================================
coefficients = readRDS(file.path(data_dir(), "in-sample-regression-no-temp.rds"))

res = list()
for (s in names(coefficients)) {
  mycoords = coords %>%
    st_drop_geometry() %>%
    dplyr::filter(season == s)
  X = mycoords %>%
    dplyr::select(all_of(row.names(coefficients[[s]])[-c(1, nrow(coefficients[[s]]))])) %>%
    as.matrix() %>%
    cbind("(Intercept)" = 1, .)
  quantiles = X %*% coefficients[[s]][-nrow(coefficients[[s]]), ]
  params = pbapply::pblapply(
    X = seq_len(nrow(quantiles)),
    cl = num_cores,
    FUN = function(i) estimateFPLD(quantiles[i, ], p_vec = p_vec, positive_support = TRUE))
  if (!all(sapply(params, length) == 5)) {
    stop("Something went wrong when estimating the FPLD parameters")
  }
  res[[s]] = data.frame(
    estimate = unlist(params),
    par = rep(paste0("lambda", 1:5), length(params)),
    id = rep(mycoords$id, each = 5),
    season = s)
}

res = do.call(rbind, res)
row.names(res) = NULL
saveRDS(res, file.path(data_dir(), "in-sample-fpld-pars-no-temp.rds"))

# Evaluate model fit
combos = dplyr::distinct(res, id, season)
tmp = pbapply::pblapply(
  X = seq_len(nrow(combos)),
  cl = num_cores,
  FUN = function(i) {
    obs = dplyr::filter(df, id == combos$id[i], season == combos$season[i])$range
    par = dplyr::filter(res, id == combos$id[i], season == combos$season[i])$estimate
    pit = pit_fpld(obs, par)
    crps = crps_fpld(obs, par)
    data.frame(pit_e1 = rep(mean(pit) - .5, 5),
               pit_e2 = rep(sd(pit) - 1 / sqrt(12), 5),
               crps = rep(mean(crps), 5))
  })
res = cbind(res, do.call(rbind, tmp))
saveRDS(res, file.path(data_dir(), "in-sample-fpld-pars-no-temp.rds"))




# Out-of-sample ==============================================
coefficients = readRDS(file.path(data_dir(), "out-of-sample-regression.rds"))

res = list(quantiles = list(), params = list())
for (s in unique(coefficients$season)) {
  res$quantiles[[s]] = list()
  mycoords = coords %>%
    st_drop_geometry() %>%
    dplyr::filter(season == s)
  season_ids = dplyr::filter(coefficients, season == s)$id

  # Compute quantiles for all ids
  for (id in unique(season_ids)) {
    X = mycoords %>%
      dplyr::filter(id == !!id) %>%
      dplyr::select(all_of(covariate_names)) %>%
      as.matrix() %>%
      cbind("(Intercept)" = 1, .)
    beta_matrix = coefficients %>%
      dplyr::filter(id == !!id, season == s) %>%
      dplyr::select(c("(Intercept)", all_of(covariate_names))) %>%
      as.matrix() %>%
      t()
    quantiles = (X %*% beta_matrix) %>%
      as.numeric()
    res$quantiles[[s]][[length(res$quantiles[[s]]) + 1]] = data.frame(
      quantile = quantiles,
      p = dplyr::filter(coefficients, id == !!id, season == s)$p,
      id = id,
      season = s)
  }

  res$params[[s]] = pbapply::pblapply(
    X = seq_along(res$quantiles[[s]]),
    cl = num_cores,
    FUN = function(i) {
      params = estimateFPLD(res$quantiles[[s]][[i]]$quantile, p_vec = res$quantiles[[s]][[i]]$p)
      data.frame(
        estimate = params,
        par = paste0("lambda", 1:5),
        id = res$quantiles[[s]][[i]]$id[1],
        season = res$quantiles[[s]][[i]]$season[1])
    })

  res$params[[s]] = do.call(rbind, res$params[[s]])
  res$quantiles[[s]] = do.call(rbind, res$quantiles[[s]])
}
res$params = do.call(rbind, res$params)
res$quantiles = do.call(rbind, res$quantiles)
row.names(res$quantiles) = NULL
row.names(res$params) = NULL
saveRDS(res, file.path(data_dir(), "out-of-sample-fpld-pars.rds"))

# Evaluate model fit
combos = dplyr::distinct(res$params, id, season)
tmp = pbapply::pblapply(
  X = seq_len(nrow(combos)),
  cl = num_cores,
  FUN = function(i) {
    obs = dplyr::filter(df, id == combos$id[i], season == combos$season[i])$range
    par = dplyr::filter(res$params, id == combos$id[i], season == combos$season[i])$estimate
    pit = pit_fpld(obs, par)
    crps = crps_fpld(obs, par)
    data.frame(pit_e1 = rep(mean(pit) - .5, 5),
               pit_e2 = rep(sd(pit) - 1 / sqrt(12), 5),
               crps = rep(mean(crps), 5))
  })
res$params = cbind(res$params, do.call(rbind, tmp))
saveRDS(res, file.path(data_dir(), "out-of-sample-fpld-pars.rds"))
