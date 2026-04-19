
#' Ensure directory exists
#'
#' @param path Directory path
#' @return The path (invisibly)
#' @export
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message("Created directory: ", path)
  }
  invisible(path)
}

#' Köppen-Geiger lookup table (internal)
kg_lookup <- c(
  "1" = "Tropical, rainforest", "2" = "Tropical, monsoon",
  "3" = "Tropical, savannah", "4" = "Arid, desert, hot",
  "5" = "Arid, desert, cold", "6" = "Arid, steppe, hot",
  "7" = "Arid, steppe, cold", "8" = "Temperate, dry summer, hot summer",
  "9" = "Temperate, dry summer, warm summer", "10" = "Temperate, dry summer, cold summer",
  "11" = "Temperate, dry winter, hot summer", "12" = "Temperate, dry winter, warm summer",
  "13" = "Temperate, dry winter, cold summer", "14" = "Temperate, no dry season, hot summer",
  "15" = "Temperate, no dry season, warm summer", "16" = "Temperate, no dry season, cold summer",
  "17" = "Cold, dry summer, hot summer", "18" = "Cold, dry summer, warm summer",
  "19" = "Cold, dry summer, cold summer", "20" = "Cold, dry summer, very cold winter",
  "21" = "Cold, dry winter, hot summer", "22" = "Cold, dry winter, warm summer",
  "23" = "Cold, dry winter, cold summer", "24" = "Cold, dry winter, very cold winter",
  "25" = "Cold, no dry season, hot summer", "26" = "Cold, no dry season, warm summer",
  "27" = "Cold, no dry season, cold summer", "28" = "Cold, no dry season, very cold winter",
  "29" = "Polar, tundra", "30" = "Polar, frost"
)

#' Land cover lookup table (internal)
lc_lookup <- c(
  "1" = "Water", "2" = "Trees", "4" = "Flooded vegetation",
  "5" = "Crops", "7" = "Built Area", "8" = "Bare ground",
  "9" = "Snow/Ice", "11" = "Rangeland"
)



packages <- c(
  "terra", "sf", "dplyr", "tidyr", "ggplot2",
  "rnaturalearth", "rnaturalearthdata", "leaflet",
  "htmlwidgets", "cluster", "NbClust", "factoextra",
  "viridis", "ggspatial", "purrr"
)

for (pkg in packages) {

  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing package: ", pkg, " (including dependencies...)")

    install.packages(
      pkg,
      dependencies = TRUE,
      quiet = FALSE
    )
  } else {
    message("Package already installed: ", pkg)
  }

  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

