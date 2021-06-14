library(raster)
library(FPLD)

download_DEM = function(filename) {
  url = "https://hoydedata.no/LaserInnsyn/Home/DownloadFile/56"
  zipfile = "dtm-50m-utm33.zip"
  download.file(url, zipfile)
  temp_dir = file.path("dtm-50m-utm33")
  unzip(zipfile, exdir = temp_dir)
  file.remove(zipfile)

  files = list.files(temp_dir, full.names = TRUE)
  tif_files = grep("*.tif$", files, value = TRUE)

  rast = raster::raster(tif_files[1])
  old_crs = as.character(rast@crs)
  new_crs = sub("units=m", "units=km", old_crs)

  command = "gdalwarp"
  args = c("-s_srs", shQuote(old_crs), "-t_srs", shQuote(new_crs), # Change crs
           "-tr 1 1", # Change the resolution to 1x1km
           "-r bilinear", # ... using bilinear interpolation
           "-overwrite", shQuote(tif_files), shQuote(filename))

  execute_shell_script(command, args)
  unlink(temp_dir)
}

execute_shell_script = function(command, args, ...) {
  output = system2(command, args, ...)
  success = output == 0
  if (!success) stop("shell script ", command, " failed with arguments ", args)
}

get_dist_to_sea = function(DEM_path, filename) {
  height_raster = raster::raster(DEM_path)
  # Aggregate data to (4x4)km^2. We do this to smooth out some of the fjords
  height_raster = raster::aggregate(height_raster, fact = 10)
  heights = raster::getValues(height_raster)
  sea_raster = height_raster
  sea_raster[heights > 0] = NA
  sea_points = raster::rasterToPoints(sea_raster, spatial = TRUE)
  distance_to_sea = raster::distanceFromPoints(height_raster, sea_points)
  distances_to_sea = distance_to_sea / 1000 # Change unit to km
  raster::writeRaster(distance_to_sea, filename)
}

# Download a DEM
dem_file = file.path(data_dir(), "dem.tif")
download_DEM(dem_file)

# Compute the distance to sea at all locations
dist_sea_file = file.path(data_dir(), "distance-to-sea.tif")
get_dist_to_sea(dem_file, dist_sea_file)
