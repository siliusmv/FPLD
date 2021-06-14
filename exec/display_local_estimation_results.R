library(FPLD)
library(ggplot2)
library(dplyr)
library(patchwork)


# Using only the FPlD
params = readRDS(file.path(data_dir(), "local_fpld_pars.rds"))
df = readRDS(range_file())
coords = readRDS(coords_file())

ids = article_stations()$id
seasons = c("summer", "winter", "winter", "spring")
nice_seasons = c("Summer", "Winter", "Winter", "Spring")
names = article_stations()$name

get_label = function(i) paste0(names[i], ",\n", nice_seasons[i])

# Create histograms with estimated densities
p1 = lapply(
  seq_along(ids),
  function(i) {
    obs = dplyr::filter(df, id == ids[i], season == seasons[i])$range
    par = dplyr::filter(params, id == ids[i], season == seasons[i])$estimate
    x = seq(min(obs) - 1, max(obs) + 1, length.out = length(obs))
    y = dFPLD(x, par)
    data.frame(obs = obs, x = x, y = y, i = i)
  }) %>%
  do.call(rbind, .) %>%
  dplyr::mutate(i = factor(i, label = get_label(1:4), levels = 1:4)) %>%
  ggplot() +
  geom_histogram(aes(x = obs, y = ..density..), boundary = 0) +
  geom_line(aes(x = x, y = y)) +
  facet_wrap(~i, nrow = 1) +
  labs(y = "Density", x = "Diurnal temperature range [$^\\circ$C]")

# Create QQ-plots
p2 = lapply(
  seq_along(ids),
  function(i) {
    obs = dplyr::filter(df, id == ids[i], season == seasons[i])$range
    par = dplyr::filter(params, id == ids[i], season == seasons[i])$estimate
    p_vec = seq(.01, .99, by = .005)
    q_obs = quantile(obs, p_vec)
    q_est = qFPLD(p_vec, par)
    data.frame(obs = q_obs, est = q_est, i = i)
  }) %>%
  do.call(rbind, .) %>%
  dplyr::mutate(i = factor(i, label = get_label(1:4), levels = 1:4)) %>%
  ggplot() +
  geom_point(aes(x = obs, y = est)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~i, nrow = 1) +
  labs(y = "Estimated quantiles [$^\\circ$C]", x = "Empirical quantile [$^\\circ$C]")

# Create PIT-plots
p3 = lapply(
  seq_along(ids),
  function(i) {
    obs = dplyr::filter(df, id == ids[i], season == seasons[i])$range
    par = dplyr::filter(params, id == ids[i], season == seasons[i])$estimate
    pit = pFPLD(obs + rnorm(length(obs), sd = .01), par)
    data.frame(x = pit, i = i)
  }) %>%
  do.call(rbind, .) %>%
  dplyr::mutate(i = factor(i, label = get_label(1:4), levels = 1:4)) %>%
  ggplot() +
  geom_histogram(aes(x = x, y = ..density..), binwidth = .1, boundary = 0) +
  geom_abline(intercept = 1, slope = 0) +
  facet_wrap(~i, nrow = 1) +
  labs(y = "Density", x = "Probability integral transform")

text_size = 13
p1 = p1 + theme_light() +
  theme(text = element_text(size = text_size), strip.text = element_text(size = text_size + 3))
p2 = p2 + theme_light() +
  theme(text = element_text(size = text_size), strip.text = element_text(size = text_size + 3))
p3 = p3 + theme_light() +
  theme(text = element_text(size = text_size), strip.text = element_text(size = text_size + 3))

plot = patchwork::wrap_plots(p1, p3, p2, nrow = 3)

tikz_plot(file.path(image_dir(), "local_estimation.pdf"), plot,
          width = 10, height = 8)


# Estimation with a lot of different models
# Using only the FPlD
params = readRDS(file.path(data_dir(), "local_multiple_distributions.rds"))
df = readRDS(range_file())
coords = readRDS(coords_file())

ids = article_stations()$id
seasons = c("summer", "winter", "winter", "spring")
nice_seasons = c("Summer", "Winter", "Winter", "Spring")
names = article_stations()$name

get_label = function(i) paste0(names[i], ", ", nice_seasons[i])

distributions = list(
  fpld = function(x, par) dFPLD(x, par),
  gamma = function(x, par) dgamma(x, par[1], par[2]),
  #exponential = function(x, par) dexp(x, par),
  #gaussian = function(x, par) dnorm(x, par[1], par[2]),
  lognormal = function(x, par) dlnorm(x, par[1], par[2]),
  gld = function(x, par) gld::dgl(x, par[1], par[2], par[3], par[4]))


# Create histograms with estimated densities
plot = lapply(
  seq_along(ids),
  function(i) {
    obs = dplyr::filter(df, id == ids[i], season == seasons[i])$range
    x = seq(min(obs) - 1, max(obs) + 1, length.out = length(obs))
    crps = params %>%
      dplyr::filter(id == ids[i], season == seasons[i], distribution %in% names(distributions)) %>%
      dplyr::distinct(distribution, .keep_all = TRUE) %>%
      dplyr::select(crps, distribution) %>%
      dplyr::mutate(best = crps == min(crps))
    res = list()
    for (d in names(distributions)) {
      par = dplyr::filter(params, id == ids[i], season == seasons[i], distribution == d)$estimate
      y = distributions[[d]](x, par)
      res[[d]] = data.frame(obs = obs, x = x, y = y, i = i, distribution = d)
    }
    res = do.call(rbind, res) %>%
      dplyr::left_join(crps, by = "distribution")
    res
  }) %>%
  do.call(rbind, .) %>%
  dplyr::mutate(i = factor(i, label = get_label(1:4), levels = 1:4),
                distribution = factor(distribution, label = c("FPLD", "Gamma", "GLD", "Lognormal", "Exponential", "Gaussian"),
                                      levels = c("fpld", "gamma", "gld", "lognormal", "exponential", "gaussian")),
                best = factor(best, label = c("Lowest CRPS", "Others"), levels = c(TRUE, FALSE))) %>%
  ggplot() +
  geom_histogram(aes(x = obs, y = ..density..), boundary = 0) +
  geom_line(aes(x = x, y = y, col = distribution, linetype = distribution, group = distribution), size = 1.8) +
  facet_wrap(~i) +
  scale_linetype_manual(values = c("solid", "dashed", "longdash", "dotdash")) +
  labs(y = "Density", x = "Diurnal temperature range [$^\\circ$C]", col = "Distribution", linetype = "Distribution") +
  theme_light() +
  theme(text = element_text(size = 16), strip.text = element_text(size = 16))


tikz_plot(file.path(image_dir(), "local_estimation_multiple_distributions.pdf"), plot,
          width = 10, height = 8)


stats = params %>%
  dplyr::filter(distribution %in% c("fpld", "gamma", "gld", "lognormal")) %>%
  dplyr::mutate(bad_lower_boundary = lower_difference < 0 | is.na(lower_difference),
                negative_lower_boundary = lower_boundary < 0,
                bad_upper_boundary = upper_difference < 0 | is.na(upper_difference)) %>%
  dplyr::group_by(distribution, season, id) %>%
  dplyr::summarise(crps = crps[1],
                   time = time[1],
                   lower_prob = lower_prob[1],
                   negative_lower_boundary = negative_lower_boundary[1],
                   bad_lower_boundary = bad_lower_boundary[1],
                   bad_upper_boundary = bad_upper_boundary[1],
                   lower_exceedance = abs(mean(lower_difference[lower_difference < 0])),
                   upper_exceedance = abs(mean(upper_difference[upper_difference < 0])),
                   negative_exceedance = abs(mean(lower_boundary[lower_boundary < 0]))) %>%
  dplyr::mutate(lower_exceedance = ifelse(is.nan(lower_exceedance), 0, lower_exceedance),
                upper_exceedance = ifelse(is.nan(upper_exceedance), 0, upper_exceedance),
                negative_exceedance = ifelse(is.nan(negative_exceedance), 0, negative_exceedance),
                fail = is.na(crps)) %>%
  dplyr::group_by(season, id) %>%
  dplyr::mutate(best_crps = crps == min(crps)) %>%
  dplyr::group_by(distribution) %>%
  dplyr::summarise(crps = mean(crps, na.rm = TRUE),
                   best_crps_percentage = mean(best_crps, na.rm = TRUE) * 100,
                   lower_prob = mean(lower_prob),
                   time = mean(time),
                   fail_percentage = mean(fail) * 100,
                   bad_lower_boundary_percentage = mean(bad_lower_boundary) * 100,
                   bad_upper_boundary_percentage = mean(bad_upper_boundary) * 100,
                   bad_boundary_percentage = mean(bad_lower_boundary | bad_upper_boundary) * 100,
                   negative_percentage = mean(negative_lower_boundary) * 100,
                   mean_lower_exceedance = mean(lower_exceedance, na.rm = TRUE),
                   mean_upper_exceedance = mean(upper_exceedance, na.rm = TRUE),
                   mean_negative_exceedance = mean(negative_exceedance, na.rm = TRUE),
                   mean_exceedance = mean(lower_exceedance + upper_exceedance, na.rm = TRUE))

table_df = stats %>%
  dplyr::select(distribution, crps, best_crps_percentage) %>%
  dplyr::mutate(distribution = factor(distribution,
                                      labels = c("FPLD", "Gamma", "GLD", "Lognormal"),
                                      levels = c("fpld", "gamma", "gld", "lognormal")))
names(table_df) = c("Distribution", "CRPS", "Best CRPS")
table_df[[1]] = as.character(table_df[[1]])
table_df[[2]] = paste0("\\(", format(round(table_df[[2]], digits = 3)), "\\)")
table_df[[3]] = paste0("\\(", format(round(table_df[[3]], digits = 1)), "\\%\\)")

table = list()
table[[1]] = "\\toprule"
table[[2]] = paste(names(table_df), collapse = " & ")
table[[3]] = "\\midrule"
for (i in seq_len(nrow(table_df))) {
  table[[length(table) + 1]] = paste(c(table_df[i, 1], table_df[i, -1]), collapse = " & ")
}
table[[length(table) + 1]] = "\\bottomrule"
table = paste(table, collapse = " \\\\\n")
table = gsub("rule \\\\\\\\", "rule ", table)
cat(table, "\n")


