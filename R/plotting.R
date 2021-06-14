#' @export
style_map_plot = function(plot, data, use_tex = FALSE, ...) {
   plot = plot +
    add_norway_map(sf::st_crs(data), sf::st_bbox(data), ...) +
    theme_light() +
    labs(x = "", y = "")
  if (use_tex) plot = latex_friendly_map_plot(plot)
   plot
}

latex_friendly_map_plot = function(x) {
  info = ggplot_build(x)$layout$panel_params[[1]]$graticule
  east_ticks = info$degree[info$type == "E"]
  north_ticks = info$degree[info$type == "N"]
  x +
    scale_x_continuous(breaks = east_ticks, labels = paste0(east_ticks, "$^\\circ$E")) +
    scale_y_continuous(breaks = north_ticks, labels = paste0(north_ticks, "$^\\circ$N"))
}

add_norway_map = function(crs = NULL, bbox = NULL, ...) {
  map = rnaturalearth::ne_countries(scale = 50, country = "Norway", returnclass = "sf")
  c(ggplot2::layer_sf(
    geom = ggplot2::GeomSf, data = map, mapping = ggplot2::aes(),
    stat = "sf", position = "identity", show.legend = NA,
    inherit.aes = TRUE,
    params = list(na.rm = FALSE, fill = NA, ...)),
    ggplot2::coord_sf(default = TRUE, crs = crs,
                      xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)]))
}


#' @export
tikz_plot = function(file, plot = NULL, expression = NULL, view = interactive(), ...) {
  # Ensure that you are on an operating system that you have tested
  operating_system = Sys.info()[["sysname"]]
  if (operating_system == "Windows") {
    proceed = readline(paste("This function was written on a Mac.",
                             "I have no idea if it will work on Windows.",
                             "Proceed? (y/n) "))
    if (proceed == "n") {
      return()
    } else if (proceed != "y") {
      stop("Invalid input")
    }
  }

  # Create a temporary file for the tikz-output
  tmp = tempfile(tmpdir = getwd())
  # Clean up after yourself on early interrupt
  on.exit(suppressWarnings(file.remove(tmp)), add = TRUE)

  # Extract default tex usepackages and add the bm package for bold greek letters
  opt = options()
  on.exit(options(opt)) #Reset global options on exit
  tikzDevice::setTikzDefaults(overwrite = FALSE)
  tex_packages = options()$tikzLatexPackages
  if (!any(grepl("usepackage\\{bm\\}", tex_packages))) {
    tex_packages = c(tex_packages, "\\usepackage{bm}\n")
  }

  # Open a device for creating a tex-file
  tikzDevice::tikz(tmp, standAlone = TRUE, packages = tex_packages, ...)
  # Call dev.off() on exit in case of interruptions
  current_device = dev.cur()
  on.exit(dev.off(current_device))

  # Plot something into the tex-file
  if (!is.null(plot)) {
    if (any(class(plot) %in% c("gg", "ggplot", "patchwork"))) {
      print(plot)
    } else {
      for (p in plot) print(p)
    }
  } else {
    eval(substitute(expression), envir = parent.frame())
  }

  # Finish the creation of the tex-file
  dev.off()

  # Compile to pdf
  #system2("pdflatex", tmp)
  system2("lualatex", shQuote(tmp))

  # Copy pdf file to final destination
  file.copy(paste0(tmp, ".pdf"), file, overwrite = TRUE)

  # Clean up all temporary files
  tmp_filename = tail(strsplit(tmp, "/")[[1]], 1)
  files_to_clean = grep(tmp_filename, list.files(full.names = TRUE), value = TRUE)
  file.remove(files_to_clean)

  # Open the pdf with the final output
  if (view) {
    if (operating_system == "Darwin") {
      system2("open", shQuote(file))
    } else {
      message("I don't know the command to open a pdf for your operating system")
    }
  }
}
