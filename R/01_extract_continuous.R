
# =============================================================================
# 1. extract_continuous_stats
# =============================================================================
#' Extract summary statistics from one or more continuous rasters per polygon.
#'
#' @export
extract_continuous_stats <- function(...) { ... }
#' @param polygons  sf object – the administrative / study polygons.
#' @param rasters   Named list of SpatRaster objects.
#'                  Names become column prefixes in the output.
#' @param funs      Named list of functions to apply (one per raster).
#'                  Can be a single named list recycled to all rasters,
#'                  or a list-of-lists with one entry per raster.
#'                  Default: list(median = median) applied to every raster.
#' @param id_col    Name of the unique row ID column in `polygons`.
#' @param aggregate_fact  Integer. If > 1, aggregate rasters by this factor
#'                        before extraction (speeds up large rasters).
#'                        Recycled: supply a named list for per-raster control.
#'
#' @return The input `polygons` sf object with extra columns appended.
#'
#' @examples
#' \dontrun{
#'   polys <- extract_continuous_stats(
#'     polygons = adm2,
#'     rasters  = list(elev = dem, pop = pop, urban = urban),
#'     funs     = list(elev  = list(median = median),
#'                     pop   = list(mean   = mean),
#'                     urban = list(mean   = mean)),
#'     aggregate_fact = list(elev = 1, pop = 1, urban = 10),
#'     id_col = "ID"
#'   )
#' }
extract_continuous_stats <- function(polygons,
                                     rasters,
                                     funs          = list(mean = mean),
                                     id_col        = "ID",
                                     aggregate_fact = 1) {

  stopifnot(inherits(polygons, "sf"))
  stopifnot(is.list(rasters), !is.null(names(rasters)))

  # Normalise funs → one list per raster
  if (!is.list(funs[[1]])) {
    funs <- setNames(rep(list(funs), length(rasters)), names(rasters))
  }

  # Normalise aggregate_fact → one value per raster
  if (length(aggregate_fact) == 1) {
    aggregate_fact <- setNames(rep(aggregate_fact, length(rasters)), names(rasters))
  }

  result <- polygons

  for (nm in names(rasters)) {
    r <- rasters[[nm]]

    # Optional aggregation
    agg <- aggregate_fact[[nm]]
    if (!is.null(agg) && agg > 1) {
      r <- terra::aggregate(r, fact = agg, fun = "mean", na.rm = TRUE)
    }

    # Re-project to match polygon CRS
    r <- terra::project(r, terra::crs(vect(polygons)))

    fn_list <- funs[[nm]]
    for (fn_nm in names(fn_list)) {
      col_name <- paste0(nm, "_", fn_nm)
      vals <- terra::extract(r, vect(polygons), fun = fn_list[[fn_nm]], na.rm = TRUE)[, 2]
      result[[col_name]] <- vals
    }
  }

  return(result)
}
