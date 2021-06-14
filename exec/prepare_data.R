library(FPLD)
library(magrittr)
library(dplyr)
library(sf)

# Locate all data files
path = file.path(data_dir(), "eklima")
files = list.files(path, full.names = TRUE)

# Export all data from eklima
df = lapply(files, extract_eklima_data) %>%
  do.call(rbind, .) %>%
  st_as_sf()

# Calculate diurnal temperature range
df$range = df$tax - df$tan
df$range[df$range <= 0] = NA
df$id = factor(df$id)

# Remove NA
df = dplyr::filter(df, !is.na(range))

# Create an individual coord object
coords = dplyr::select(df, id, name, altitude) %>%
  dplyr::group_by(id) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  sf::st_transform(get_proj_xy()) %>%
  {cbind(., sf::st_coordinates(.))} %>%
  dplyr::rename(x = X, y = Y, height = altitude)

# Remove geographical info from df
df = dplyr::select(df, date, tam, tan, tax, range, id) %>%
  sf::st_drop_geometry()

# Find the distance to the sea from all weather stations
coords$dist_sea = extract_distance_to_sea(coords)

df$season = season(df$date)
df$date = NULL
season_stats = df %>%
  dplyr::group_by(season, id) %>%
  dplyr::summarise(mean_tam = mean(tam, na.rm = TRUE),
                   sd_tam = sd(tam, na.rm = TRUE),
                   n = dplyr::n()) %>%
  dplyr::ungroup()
coords = dplyr::left_join(season_stats, coords, by = "id")

coords = lapply(
  unique(coords$season),
  function(s) {
    tmp = dplyr::filter(coords, season == s)
    standardise(tmp, names = c("x", "y", "height", "dist_sea", "mean_tam", "sd_tam"))
  }) %>%
  do.call(rbind, .)

bad_seasons = dplyr::filter(coords, n < 180)
coords = dplyr::filter(coords, n >= 180)
for (i in seq_len(nrow(bad_seasons))) {
  df = dplyr::filter(df, !(id == bad_seasons$id[i] & season == bad_seasons$season[i]))
}
bad_stations = coords %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(num_seasons = n()) %>%
  dplyr::filter(num_seasons < 4)
coords = dplyr::filter(coords, !id %in% bad_stations$id)
df = dplyr::filter(df, !id %in% bad_stations$id)

coords = sf::st_as_sf(coords)
coords$id = factor(coords$id)
df$id = factor(df$id)

saveRDS(df, range_file())
saveRDS(coords, coords_file())
