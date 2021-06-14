
#' @export
data_dir = function() file.path(here::here(), "inst", "extdata")
#' @export
range_file = function() file.path(data_dir(), "range_data.rds")
#' @export
coords_file = function() file.path(data_dir(), "stations_data.rds")
#' @export
script_dir = function() file.path(here::here(), "inst", "extdata")
#' @export
image_dir = function() file.path(here::here(), "inst", "extdata", "images")

#' @export
season = function(x) {
  months = lubridate::month(x)
  seasons = factor(c("winter", "spring", "summer", "autumn"))
  dplyr::case_when(
    months %in% c(12, 1, 2) ~ seasons[1],
    months %in% 3:5 ~ seasons[2],
    months %in% 6:8 ~ seasons[3],
    months %in% 9:11 ~ seasons[4])
}

#' @export
article_stations = function() {
  list(id = c(50500, 40880, 35860, 26990),
       name = c("Flesland", "Hovden - Lundane", "Lyngør fyr", "Sande - Galleberg"))
}
