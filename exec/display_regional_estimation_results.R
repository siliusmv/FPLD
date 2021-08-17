library(FPLD)
library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)
library(sf)


# ===============================================================
# Compare in-sample and out-of-sample and univariate results
# ===============================================================
p1 = readRDS(file.path(data_dir(), "in-sample-fpld-pars.rds"))
p2 = readRDS(file.path(data_dir(), "out-of-sample-fpld-pars.rds"))$params
p3 = readRDS(file.path(data_dir(), "local_fpld_pars.rds"))
p4 = readRDS(file.path(data_dir(), "in-sample-fpld-pars-no-temp.rds"))
df = readRDS(range_file())
coords = readRDS(coords_file())

params = list(p1, p2, p3, p4)
for (i in seq_along(params)) {
  params[[i]] = dplyr::distinct(params[[i]], id, season, crps, pit_e1, pit_e2) %>%
    dplyr::mutate(tag = c("in-sample", "out-of-sample", "univariate", "in-sample-notemp")[i])
}
params = do.call(rbind, params)

scores = params %>%
  dplyr::mutate(tag = factor(tag, labels = c("Univariate", "In-sample", "Out-of-sample", "Notemp"),
                             levels = c("univariate", "in-sample", "out-of-sample", "in-sample-notemp")),
                season = factor(season, levels = c("winter", "spring", "summer", "autumn"),
                                labels = c("Winter", "Spring", "Summer", "Autumn"))) %>%
  dplyr::group_by(season, tag) %>%
  dplyr::summarise(crps = mean(crps, na.rm = TRUE), pit_e1 = mean(pit_e1), pit_e2 = mean(pit_e2))
names(scores) = c("Season", "Modelling", "CRPS", "\\(e_\\mu\\)", "\\(e_\\sigma\\)")
scores[[3]] = paste0("\\(", format(round(scores[[3]], digits = 2)), "\\)")
scores[[4]] = paste0("\\(", format(round(scores[[4]] * 100, digits = 2)), " \\cdot 10^{-2}\\)")
scores[[5]] = paste0("\\(", format(round(scores[[5]] * 100, digits = 2)), " \\cdot 10^{-2}\\)")
scores[[1]] = as.character(scores[[1]])
scores[[2]] = as.character(scores[[2]])

table = list()
table[[1]] = "\\toprule"
table[[2]] = paste(names(scores), collapse = " & ")
table[[3]] = "\\midrule"
for (i in seq_len(nrow(scores))) {
  table[[length(table) + 1]] = paste(c("", scores[i, -1]), collapse = " & ")
  if ((i %% 4 == 1)) {
    table[[length(table)]] = paste(c(scores[i, 1], table[[length(table)]]), collapse = "")
  }
  if (i %% 4 == 0 && i != nrow(scores)) {
    table[[length(table) + 1]] = "\\midrule"
  }
}
table[[length(table) + 1]] = "\\bottomrule"
table = paste(table, collapse = " \\\\\n")
table = gsub("rule \\\\\\\\", "rule ", table)
cat(table, "\n")


# ===============================================================
# Compare in-sample and out-of-sample FPLD parameter estimates
# ===============================================================
#p1 = readRDS(file.path(data_dir(), "in-sample-fpld-pars.rds"))
p2 = readRDS(file.path(data_dir(), "out-of-sample-fpld-pars.rds"))$params
p3 = readRDS(file.path(data_dir(), "local_fpld_pars.rds"))
df = readRDS(range_file())
coords = readRDS(coords_file())

params = list(p2, p3)
for (i in seq_along(params)) {
  params[[i]] = dplyr::distinct(params[[i]], id, season, estimate, par) %>%
    dplyr::mutate(tag = c("out-of-sample", "univariate")[i])
}
params = do.call(rbind, params)

estimates = params %>%
  dplyr::group_by(season, tag, id) %>%
  dplyr::mutate(transformed = transform_location_scale(estimate)) %>%
  dplyr::group_by(season, tag, par) %>%
  dplyr::summarise(mean = mean(transformed),
                   median = median(transformed),
                   lower = quantile(transformed, .025),
                   upper = quantile(transformed, .975)) %>%
  dplyr::mutate(tag = factor(tag, labels = c("Univariate", "Regression"),
                             levels = c("univariate", "out-of-sample")),
                season = factor(season, levels = c("winter", "spring", "summer", "autumn"),
                                labels = c("Winter", "Spring", "Summer", "Autumn")))
estimates$mean = format(round(estimates$mean, digits = 1))
estimates$median = format(round(estimates$median, digits = 1))
estimates$lower = format(round(estimates$lower, digits = 1))
estimates$upper = format(round(estimates$upper, digits = 1))

my_seasons = c("Winter", "Spring", "Summer", "Autumn")
table = NULL
table[1] = "\\toprule"
table[2] = paste0("\\multicolumn{3}{c}{", my_seasons, "}", collapse = " & ")
table[2] = paste("& &", table[2])
table[3] = paste0("\\cmidrule(lr){", c(3, 6, 9, 12), "-", c(5, 8, 11, 14), "}", collapse = " ")
#table[4] = paste("Model & \\(\\hat{\\bm{\\lambda}}\\) &", paste(rep(c("Mean", "\\(2.5\\%\\)", "\\(97.5\\%\\)"), 4), collapse = " & "))
table[4] = paste("Model & \\(\\hat{\\bm{\\lambda}}\\) &", paste(rep(c("\\(2.5\\%\\)", "\\(50.0\\%\\)", "\\(97.5\\%\\)"), 4), collapse = " & "))
table[5] = "\\midrule"
for (model in c("Univariate", "Regression")) {
  for (i in 1:5) {
    tmp = paste0("& \\(\\hat{\\lambda}_", i, "\\)")
    if (i == 1) tmp = paste(model, tmp)
    for (j in seq_along(my_seasons)) {
      tmp_df = dplyr::filter(estimates, season == my_seasons[j], tag == model)
      tmp = paste(tmp, "&", paste0("\\(", tmp_df[i, c(6, 5, 7)], "\\)", collapse = " & "))
    }
    table[length(table) + 1] = tmp
  }
  table[length(table) + 1] = "\\midrule"
}
table = table[-length(table)]
table[[length(table) + 1]] = "\\bottomrule"
table = paste(table, collapse = " \\\\\n")
table = gsub("rule \\\\\\\\", "rule ", table)
table = gsub("14} \\\\\\\\", "14} ", table)
cat(table, "\n")


# ===============================================================
# Plot the parameter estimates in a map
# ===============================================================
p2 = readRDS(file.path(data_dir(), "out-of-sample-fpld-pars.rds"))$params
p3 = readRDS(file.path(data_dir(), "local_fpld_pars.rds"))
df = readRDS(range_file())
coords = readRDS(coords_file())

params = list(p2, p3)
for (i in seq_along(params)) {
  params[[i]] = dplyr::distinct(params[[i]], id, season, par, estimate) %>%
    dplyr::mutate(tag = c("out-of-sample", "univariate")[i])
}
params = do.call(rbind, params) %>%
  dplyr::left_join(dplyr::select(coords, id), by = "id") %>%
  dplyr::distinct(par, id, season, tag, .keep_all = TRUE) %>%
  dplyr::group_by(id, season, tag) %>%
  dplyr::mutate(transformed = transform_location_scale(estimate)) %>%
  st_as_sf()

plot_df = params %>%
  dplyr::mutate(
    season = factor(season, levels = c("winter", "spring", "summer", "autumn"),
                    labels = c("Winter", "Spring", "Summer", "Autumn")),
    tag = factor(tag, levels = c("univariate", "out-of-sample"), labels = c("Univariate model", "Regression model")),
    par = factor(par, levels = paste0("lambda", 1:5), labels = paste0("$lambda_", 1:5, "$"))) %>%
  dplyr::filter(tag == "Regression model")
plots = list()
for (i in 1:5) {
  plots[[i]] = dplyr::filter(plot_df, par == paste0("$lambda_", i, "$")) %>%
    ggplot() +
    geom_sf(aes(col = transformed)) +
    facet_wrap(~season, nrow = 1)
  if (i %in% c(1, 2)) {
    plots[[i]] = plots[[i]] + scale_color_viridis_c(trans = "log10")
  } else if (i == 4) {
    plots[[i]] = plots[[i]] + scale_color_viridis_c(trans = "sqrt")
  } else {
    plots[[i]] = plots[[i]] + scale_color_viridis_c()
  }
  plots[[i]] = style_map_plot(plots[[i]], params, use_tex = TRUE) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(size = 15))
}
plot = patchwork::wrap_plots(plots, ncol = 1)

tikz_plot("tmp.pdf", plot, width = 10, height = 10)


# ===============================================================
# Plot the PIT and CRPS results in a map
# ===============================================================
p1 = readRDS(file.path(data_dir(), "in-sample-fpld-pars.rds"))
p2 = readRDS(file.path(data_dir(), "out-of-sample-fpld-pars.rds"))$params
p3 = readRDS(file.path(data_dir(), "local_fpld_pars.rds"))
df = readRDS(range_file())
coords = readRDS(coords_file())

params = list(p1, p2, p3)
for (i in seq_along(params)) {
  params[[i]] = dplyr::distinct(params[[i]], id, season, crps, pit_e1, pit_e2) %>%
    dplyr::mutate(tag = c("in-sample", "out-of-sample", "univariate")[i])
}
params = do.call(rbind, params) %>%
  dplyr::left_join(dplyr::select(coords, id), by = "id") %>%
  st_as_sf()

label_df = params %>%
  dplyr::filter(id %in% article_stations()$id) %>%
  dplyr::group_by(id, season, tag) %>%
  dplyr::slice(1) %>%
  {cbind(., st_coordinates(.))} %>%
  dplyr::mutate(x0 = X + rep(c(-25, 25, -25, -40), each = 4 * 3),
                y0 = Y + rep(c(-25, -25, -25, 0), each = 4 * 3),
                label = rep(rep(c("4", "3", "2", "1"), each = 4 * 3))) %>%
  dplyr::filter(season %in% c("summer", "winter"), tag %in% c("univariate", "out-of-sample")) %>%
  dplyr::mutate(
    season = factor(season, levels = c("winter", "summer"), labels = c("Winter", "Summer")),
    tag = factor(tag, levels = c("univariate", "out-of-sample"), labels = c("Univariate model", "Regression model")))

tmp = params %>%
  dplyr::filter(season %in% c("summer", "winter"), tag %in% c("univariate", "out-of-sample")) %>%
  dplyr::mutate(
    season = factor(season, levels = c("winter", "summer"), labels = c("Winter", "Summer")),
    tag = factor(tag, levels = c("univariate", "out-of-sample"), labels = c("Univariate model", "Regression model")))
minmax = tmp$pit_e1 %>% {c(min(.), max(.))}
plot2 = ggplot(tmp) +
  geom_sf(aes(col = pit_e1, size = abs(pit_e1))) +
  geom_segment(data = label_df, aes(x = x0, y = y0, xend = X, yend = Y, group = id)) +
  geom_label(data = label_df, aes(x = x0, y = y0, label = label), size = 3) +
  facet_grid(tag ~ season) +
  scale_size_continuous(range = c(1, 3),
                        breaks = seq(min(abs(params$pit_e1), na.rm = TRUE), max(abs(params$pit_e1), na.rm = TRUE), length = 50)) +
  guides(size = FALSE) +
  scale_color_viridis_c(limits = c(-1, 1) * max(abs(minmax)))
plot2 = style_map_plot(plot2, params, use_tex = TRUE) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 15)) +
  labs(col = "$e_\\mu$")

tmp = params %>%
  dplyr::filter(season %in% c("summer", "winter"), tag %in% c("univariate", "out-of-sample")) %>%
  dplyr::mutate(
    season = factor(season, levels = c("winter", "summer"), labels = c("Winter", "Summer")),
    tag = factor(tag, levels = c("univariate", "out-of-sample"), labels = c("Univariate model", "Regression model")))
minmax = tmp$pit_e2 %>% {c(min(.), max(.))}
plot3 = ggplot(tmp) +
  geom_sf(aes(col = pit_e2, size = abs(pit_e2))) +
  geom_segment(data = label_df, aes(x = x0, y = y0, xend = X, yend = Y, group = id)) +
  geom_label(data = label_df, aes(x = x0, y = y0, label = label), size = 3) +
  facet_grid(tag ~ season) +
  scale_size_continuous(range = c(1, 3),
                        breaks = seq(min(abs(params$pit_e2), na.rm = TRUE), max(abs(params$pit_e2), na.rm = TRUE), length = 50)) +
  guides(size = FALSE) +
  scale_color_viridis_c(limits = c(-1, 1) * max(abs(minmax)))
plot3 = style_map_plot(plot3, params, use_tex = TRUE) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 15)) +
  labs(col = "$e_\\sigma$")

plot = plot2 + plot3

tikz_plot(file.path(image_dir(), "pit-in-map.pdf"), plot,
          width = 13, height = 5)



label_df = params %>%
  dplyr::filter(id %in% article_stations()$id) %>%
  dplyr::group_by(id, season, tag) %>%
  dplyr::slice(1) %>%
  {cbind(., st_coordinates(.))} %>%
  dplyr::mutate(x0 = X + rep(c(-25, 25, -25, -40), each = 4 * 3),
                y0 = Y + rep(c(-25, -25, -25, 0), each = 4 * 3),
                label = rep(rep(c("4", "3", "2", "1"), each = 4 * 3))) %>%
  dplyr::filter(tag %in% c("univariate", "out-of-sample")) %>%
  dplyr::mutate(
    season = factor(season, levels = c("winter", "spring", "summer", "autumn"),
                    labels = c("Winter", "Spring", "Summer", "Autumn")),
    tag = factor(tag, levels = c("univariate", "out-of-sample"), labels = c("Univariate model", "Regression model")))

plot1 = params %>%
  dplyr::filter(tag %in% c("univariate", "out-of-sample")) %>%
  dplyr::mutate(
    season = factor(season, levels = c("winter", "spring", "summer", "autumn"),
                    labels = c("Winter", "Spring", "Summer", "Autumn")),
    tag = factor(tag, levels = c("univariate", "out-of-sample"), labels = c("Univariate model", "Regression model"))) %>%
  ggplot() +
  geom_sf(aes(col = crps, size = abs(crps))) +
  geom_segment(data = label_df, aes(x = x0, y = y0, xend = X, yend = Y, group = id)) +
  geom_label(data = label_df, aes(x = x0, y = y0, label = label), size = 3) +
  facet_grid(tag ~ season) +
  scale_size_continuous(range = c(.5, 3),
                        breaks = seq(min(abs(params$crps), na.rm = TRUE), max(abs(params$crps), na.rm = TRUE), length = 50)) +
  guides(size = FALSE) +
  scale_color_viridis_c(trans = "log10", breaks = c(1, 2, 3, 5))
plot1 = style_map_plot(plot1, params, use_tex = TRUE) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 15)) +
  labs(col = "CRPS")

tikz_plot(file.path(image_dir(), "crps-in-map.pdf"), plot1,
          width = 10, height = 4.5)

# ===============================================================
# In-sample results
# ===============================================================
params = readRDS(file.path(data_dir(), "in-sample-fpld-pars.rds"))
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

tikz_plot(file.path(image_dir(), "in-sample-estimation.pdf"), plot,
          width = 10, height = 8)

# ===============================================================
# Out-of-sample results
# ===============================================================
params = readRDS(file.path(data_dir(), "out-of-sample-fpld-pars.rds"))$params
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

tikz_plot(file.path(image_dir(), "out-of-sample-estimation.pdf"), plot,
          width = 10, height = 8)

# ====================================================================================
# Histograms for estimated QR parameters
# ====================================================================================
coefficients = readRDS(file.path(data_dir(), "out-of-sample-regression.rds"))
covariate_names = c("intercept", "mean_tam", "sd_tam", "height", "x", "y", "dist_sea")
df = readRDS(range_file())

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

plot = coefficients %>%
  dplyr::filter(p == .5) %>%
  dplyr::rename("intercept" = "(Intercept)") %>%
  dplyr::mutate(intercept = (intercept - mean(df$range))) %>%
  tidyr::pivot_longer(dplyr::all_of(covariate_names), names_to = "covariate") %>%
  dplyr::mutate(value = value / sd(df$range)) %>%
  dplyr::mutate(season = factor(season,
                                labels = c("Winter", "Spring", "Summer", "Autumn"),
                                levels = c("winter", "spring", "summer", "autumn")),
                covariate = factor(covariate,
                                   labels = c("Intercept", "Easting", "Northing", "Altitude",
                                              "Distance to the\nopen sea",
                                              "Mean of\ndaily mean\ntemperature",
                                              "SD of\ndaily mean\ntemperature"),
                                   levels = covariate_names[c(1, 5, 6, 7, 4, 2, 3)])) %>%
  ggplot() +
  theme_light() +
  geom_boxplot(aes(y = value, x = covariate, fill = season, col = season),
               position = "identity", size = .4, outlier.size = .5) +
  labs(fill = "Season", col = "Season",
       y = "$\\widehat{\\bm{\\beta}}_{0.5}$", x = "") +
  scale_color_manual(values = gg_color_hue(4)[c(3, 2, 1, 4)]) +
  scale_fill_manual(values = alpha(gg_color_hue(4)[c(3, 2, 1, 4)], .5)) +
  theme(axis.text.x = element_text(angle = 75, vjust = .5, size = rel(1.2)),
        axis.title.y = element_text(angle = 0, vjust = .5)) +
  geom_hline(yintercept = 0) +
  lims(y = c(-1, 1))

tikz_plot(file.path(image_dir(), "qr_beta_densities.pdf"), plot,
          width = 7, height = 4.5)

# ====================================================================================
# Histograms for estimated FPLD parameters
# ====================================================================================
params = list(regional = readRDS(file.path(data_dir(), "in-sample-fpld-pars.rds")),
              univariate = readRDS(file.path(data_dir(), "local_fpld_pars.rds")))
for (n in names(params)) params[[n]]$tag = n
params = do.call(rbind, params)
covariate_names = c("intercept", "mean_tam", "sd_tam", "height", "x", "y", "dist_sea")
df = readRDS(range_file())

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

plot = params %>%
  dplyr::mutate(
    season = factor(season,
                    labels = c("Winter", "Spring", "Summer", "Autumn"),
                    levels = c("winter", "spring", "summer", "autumn")),
    tag = factor(tag,
                 labels = c("Univariate model", "Regression model"),
                 levels = c("univariate", "regional")),
    par = factor(par,
                 labels = paste0("$\\widehat{\\lambda}_", 1:5, "$"),
                 levels = paste0("lambda", 1:5))) %>%
  ggplot() +
  theme_light() +
  geom_boxplot(aes(y = estimate, x = season, fill = season,
                   col = season)) +
  facet_grid(par ~ tag, scales = "free_y") +
  labs(fill = "Season", col = "Season", y = "", x = "") +
  scale_fill_manual(values = alpha(gg_color_hue(4)[c(3, 2, 1, 4)], .6)) +
  scale_color_manual(values = gg_color_hue(4)[c(3, 2, 1, 4)]) +
  guides(fill = FALSE, col = FALSE) +
  theme(strip.text.y = element_text(angle = 0))

tikz_plot(file.path(image_dir(), "fpld_par_densities.pdf"), plot,
          width = 6, height = 6)
