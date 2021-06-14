library(FPLD)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)

df = readRDS(range_file())
coords = readRDS(coords_file())

# =============================================================================
# Plot range against covariates
# =============================================================================
median_range = df %>%
  dplyr::group_by(season, id) %>%
  dplyr::summarise(median_range = median(range, na.rm = TRUE))
coords = dplyr::left_join(coords, median_range, by = c("id", "season"))

covariate_names = c("mean_tam", "sd_tam", "height", "x", "y", "dist_sea")

gg_color_hue = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

plot = coords %>%
  st_drop_geometry() %>%
  tidyr::pivot_longer(dplyr::all_of(covariate_names), names_to = "covariate") %>%
  dplyr::mutate(season = factor(season,
                                labels = c("Winter", "Spring", "Summer", "Autumn"),
                                levels = c("winter", "spring", "summer", "autumn")),
                covariate = factor(covariate,
                                   labels = c("Easting", "Northing", "Altitude",
                                              "Distance to the\nopen sea",
                                              "Mean of\ndaily mean\ntemperature",
                                              "SD of\ndaily mean\ntemperature"),
                                   levels = covariate_names[c(4, 5, 6, 3, 1, 2)])) %>%
  ggplot(aes(y = median_range, x = value)) +
  geom_point(aes(col = season)) +
  facet_grid(season~covariate) +
  scale_color_manual(values = gg_color_hue(4)[c(3, 2, 1, 4)]) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE, col = "black") +
  labs(col = "Season", y = "Median DTR [$^\\circ$C]", x = "Standardised explanatory variable") +
  guides(col = FALSE) +
  theme_light() +
  theme(strip.text = element_text(size = rel(.9)),
        axis.title = element_text(size = rel(1.2)),
        text = element_text(size = 15))

tikz_plot(file.path(image_dir(), "median_covariates.pdf"), plot,
          width = 10, height = 7)


# =============================================================================
# Plot median of diurnal temperature range in a map for each season
# =============================================================================
median_range = df %>%
  dplyr::group_by(season, id) %>%
  dplyr::summarise(median_range = median(range, na.rm = TRUE))
coords = dplyr::left_join(coords, median_range, by = c("id", "season"))

plot_df = coords %>%
  dplyr::mutate(season = factor(season,
                                labels = c("Winter", "Spring", "Summer", "Autumn"),
                                levels = c("winter", "spring", "summer", "autumn")))

label_df = plot_df %>%
  dplyr::filter(id %in% article_stations()$id) %>%
  {cbind(., st_coordinates(.))} %>%
  dplyr::mutate(x0 = X + c(-25, 25, -25, -40),
                y0 = Y + c(-25, -25, -25, 0),
                label = rep(c("4", "3", "2", "1"), 4))

plot = ggplot(plot_df) +
  geom_sf(aes(col = median_range), size = 2) +
  facet_wrap(~season, nrow = 1) +
  scale_color_viridis_c() +
  geom_segment(data = label_df, aes(x = x0, y = y0, xend = X, yend = Y, group = id)) +
  geom_label(data = label_df, aes(x = x0, y = y0, label = label), size = 3) +
  labs(col = "DTR [$^\\circ$C]")
plot = style_map_plot(plot, coords, use_tex = TRUE) +
  theme(text = element_text(size = 15), strip.text = element_text(size = 18),
        axis.text = element_blank(), axis.ticks = element_blank())

tikz_plot(file.path(image_dir(), "median_range_values.pdf"), plot,
          width = 13, height = 4)


# =============================================================================
# Plot histograms for the four chosen article stations
# =============================================================================
ids = article_stations()$id
seasons = c("summer", "winter", "winter", "spring")
nice_seasons = c("Summer", "Winter", "Winter", "Spring")
names = article_stations()$name

get_label = function(i) paste0(names[i], ", ", nice_seasons[i])

plot = lapply(
  seq_along(ids),
  function(i) {
    obs = dplyr::filter(df, id == ids[i], season == seasons[i])$range
    data.frame(obs = obs, i = i)
  }) %>%
  do.call(rbind, .) %>%
  dplyr::mutate(i = factor(i, label = get_label(1:4), levels = 1:4)) %>%
  ggplot() +
  geom_histogram(aes(x = obs, y = ..density..), boundary = 0) +
  facet_wrap(~i, nrow = 1) +
  labs(y = "Density", x = "Diurnal temperature range [$^\\circ$C]") +
  theme_light() +
  theme(text = element_text(size = 15), strip.text = element_text(size = 13))

tikz_plot(file.path(image_dir(), "station_histograms.pdf"), plot,
          width = 10, height = 4)


# =============================================================================
# Examine correlations between range, mean and skewness
# =============================================================================
df$skewness = (df$tam - df$tan) / (df$tax - df$tan)
df$skewness[df$skewness < 0 | df$skewness > 1] = NA

vars = c("tam", "tan", "tax", "range", "skewness")
combos = expand.grid(vars, vars, stringsAsFactors = FALSE)
corr_df = lapply(
  seq_len(nrow(combos)),
  function(i) {
    v1 = combos[[1]][i]; v2 = combos[[2]][i]
    if (v1 == v2) return(NULL)
    tmp = dplyr::select(df, c("id", all_of(c(v1, v2))))
    na_rows = union(which(is.na(tmp[[2]])), which(is.na(tmp[[3]])))
    if (any(na_rows)) tmp = tmp[-na_rows, ]
    tmp %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(corr = cor(.data[[v1]], .data[[v2]])) %>%
      dplyr::mutate(v1 = combos[[1]][i], v2 = combos[[2]][i])
  }
) %>%
  do.call(rbind, .)

ggplot(corr_df) +
  facet_grid(v1 ~ v2) +
  geom_point(aes(y = abs(corr), x = 1, col = id)) +
  guides(col = FALSE)
# We see clearly that the correlations between tam, tan and tax are
# much larger than those between range, skewness and tam
