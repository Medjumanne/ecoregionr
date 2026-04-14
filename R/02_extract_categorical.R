# =============================================================================
# 2. extract_categorical_composition
# =============================================================================
#' Compute the proportion of each category within each polygon for a
#' categorical SpatRaster.  Returns a wide tibble (one row per polygon).
#'
#' @param polygons     sf object.
#' @param cat_raster   SpatRaster with integer / factor values.
#' @param lookup       Named character vector mapping raster values to labels.
#'                     Set to NULL to keep numeric codes.
#' @param exclude_vals Integer vector of raster values to exclude (e.g. clouds).
#' @param id_col       Name of the row ID column in \code{polygons}.
#' @param prefix       Character prefix added to every output column name.
#'
#' @return A tibble: [id_col] + one column per category (proportions 0-1).
#' @export
extract_categorical_composition <- function(polygons,
                                            cat_raster,
                                            lookup       = NULL,
                                            exclude_vals = NULL,
                                            id_col       = "ID",
                                            prefix       = "") {

  stopifnot(inherits(polygons, "sf"))

  # Harmonise CRS
  cat_raster <- terra::project(cat_raster, terra::crs(vect(polygons)))

  # Mask excluded values
  if (!is.null(exclude_vals)) {
    for (v in exclude_vals) cat_raster[cat_raster == v] <- NA
  }

  ext_df <- terra::extract(cat_raster, vect(polygons))
  # terra::extract returns one row per pixel, with column 1 being a sequential
  # polygon index (1, 2, 3 …) — NOT the polygon's actual ID values.
  # Use that index to look up the real ID for every pixel row.
  ext_df[[1]] <- polygons[[id_col]][ ext_df[[1]] ]
  colnames(ext_df) <- c(id_col, "cat_val")

  # Drop any sentinel / overflow values (e.g. 2^32 = 4294967295/4294967296).
  # Coerce to numeric first so this works for both integer and factor rasters.
  cat_num <- suppressWarnings(as.numeric(as.character(ext_df$cat_val)))
  ext_df  <- ext_df[is.na(cat_num) | cat_num < 2^31, ]

  props <- ext_df |>
    as_tibble() |>
    filter(!is.na(cat_val)) |>
    count(.data[[id_col]], cat_val) |>
    group_by(.data[[id_col]]) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    select(all_of(id_col), cat_val, prop) |>
    pivot_wider(
      names_from  = cat_val,
      values_from = prop,
      values_fill = 0
    )

  # Rename using lookup
  if (!is.null(lookup)) {
    idx     <- match(as.character(names(props)[-1]), names(lookup))
    new_nms <- lookup[idx]
    # Give a fallback name to unmatched classes instead of NA
    new_nms[is.na(new_nms)] <- paste0("class_", names(props)[-1][is.na(new_nms)])
    names(props)[-1] <- new_nms
  }

  # Drop columns whose name is still NA
  props <- props[, !is.na(names(props)), drop = FALSE]

  # Add prefix
  if (nchar(prefix) > 0) {
    names(props)[-1] <- paste0(prefix, names(props)[-1])
  }

  return(props)
}
