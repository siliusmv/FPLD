
#' Function for finding the start and end of different types of
#' data tables in files from eklima
locate_table_in_eklima_data = function(path, start_text) {
  con = file(path, "r")
  count = 0
  table_start_end = rep(NA, 2)
  while (TRUE) {
    line = readLines(con, n = 1)
    count = count + 1
    if (length(line) == 0) {
      break
    }
    if (regexpr(start_text, line) == 1) {
      table_start_end[1] = count
    }
    if (line == "" && !is.na(table_start_end[1])) {
      table_start_end[2] = count - 1
      break
    }
  }
  close(con)
  table_start_end
}

#' Find the number of variables in an eklima-file
num_vars_in_eklima_data = function(filepath, start) {
  data = read.csv2(file = filepath, skip = start, nrows = 3)
  num_var = (ncol(data) - 2) / 2
  num_var
}


get_eklima_coords = function(file) {
  table_start_end = locate_table_in_eklima_data(file, "Stnr")
  data = read.csv2(
    file = file,
    skip = table_start_end[1] - 1,
    nrows = table_start_end[2] - table_start_end[1],
    colClasses = "character")

  names(data) = tolower(names(data))

  data = dplyr::select(data, stnr, latitude, longitude, altitude, name) %>%
    dplyr::rename(id = stnr) %>%
    dplyr::mutate(id = as.integer(id),
                  latitude = as.numeric(latitude),
                  longitude = as.numeric(longitude),
                  altitude = as.numeric(altitude)) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  data
}


#' @export
extract_eklima_data = function(file) {

  # Locate the start and end of the data in the relevant csv file,
  # and also the number of variables
  table_start_end = locate_table_in_eklima_data(file, "St.no")
  num_vars = num_vars_in_eklima_data(file, table_start_end[1])

  # Read the data
  data = read.csv2(
    file = file,
    skip = table_start_end[1] - 1,
    nrows = table_start_end[2] - table_start_end[1],
    colClasses = c(
      "integer", "character",
      rep(c("character", "factor"), num_vars)))

  # Only select the relevant columns
  data = data %>%
    dplyr::select(-dplyr::starts_with("f"), -dplyr::ends_with("D")) %>%
    dplyr::mutate(Date = lubridate::as_date(Date, format = "%d.%m.%Y")) %>%
    dplyr::rename(id = St.no)

  # Fix NA data
  for (i in 3:ncol(data)) {
    data[[i]] = ifelse(data[[i]] == "x", "-9999", data[[i]])
    data[[i]] = as.numeric(data[[i]])
    data[[i]] = ifelse(data[[i]] == -9999, NA, data[[i]])
  }

  # Purely aesthetical
  names(data) = tolower(names(data))

  coords = get_eklima_coords(file)

  data = dplyr::left_join(data, coords, by = "id")

  data
}

#' @export
extract_distance_to_sea = function(df) {
  dist_sea_file = file.path(data_dir(), "distance-to-sea.tif")
  raster = raster::raster(dist_sea_file)
  coords = dplyr::group_by(df, name) %>%
    dplyr::slice(1)
  dist_sea = raster::extract(raster, coords)

  # Manually set some distances equal to 0
  station_names = coords$name
  at_sea_index = grep(" FYR$", station_names)
  at_sea_index = c(at_sea_index, grep("JOMFRULAND", station_names))
  dist_sea[at_sea_index] = 0

  dist_sea
}

#' @export
get_proj_xy = function() {
  sf::st_crs("+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs")
}
