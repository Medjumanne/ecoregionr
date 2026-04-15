# =============================================================================
# 3. build_cluster_table
# =============================================================================
#' Join polygon attributes with categorical composition tables,
#' and optionally with a pre-loaded CSV data frame.
#'
#' @param polygons    sf object already enriched with continuous stats.
#' @param cat_tables  List of wide tibbles from extract_categorical_composition.
#' @param keep_cols   Character vector of columns from `polygons` to retain.
#' @param id_col      Join key.
#' @param csv_data    Optional data frame to join by `id_col`.
#'                    Must contain a column whose name matches `id_col` plus
#'                    any additional numeric covariates you want included in
#'                    clustering.  All non-ID numeric columns are retained.
#'                    Pass \code{NULL} (default) to skip.
#'
#' @return A plain tibble ready for clustering.
#' @export
build_cluster_table <- function(polygons,
                                cat_tables = list(),
                                keep_cols,
                                id_col   = "ID",
                                csv_data = NULL) {

  base_df <- polygons |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(c(id_col, keep_cols)))

  for (tbl in cat_tables) {
    base_df <- base_df |> dplyr::left_join(tbl, by = id_col)
  }

  # ── Optional CSV join ──────────────────────────────────────────────────────
  if (!is.null(csv_data)) {
    if (!id_col %in% names(csv_data))
      stop("csv_data must contain the ID column '", id_col, "'.")

    # Keep only ID + numeric columns from the CSV
    csv_numeric <- names(csv_data)[
      sapply(csv_data, is.numeric) & names(csv_data) != id_col
    ]

    if (length(csv_numeric) == 0)
      warning("csv_data has no numeric columns to join beyond the ID column.")

    base_df <- base_df |>
      dplyr::left_join(
        csv_data[, c(id_col, csv_numeric), drop = FALSE],
        by = id_col
      )

    message("  CSV join added ", length(csv_numeric), " column(s): ",
            paste(csv_numeric, collapse = ", "))
  }

  return(base_df)
}






