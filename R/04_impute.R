# =============================================================================
# 4. impute_medians
# =============================================================================
#' Replace NA values in selected columns with column medians.
#'
#' @param df            Data frame.
#' @param numeric_cols  Character vector of column names to impute.
#' @param group_col     Optional. Name of a grouping column (e.g. \code{"ADM0_NAME"}).
#'                      When supplied, medians are computed \emph{within each group}
#'                      so that one country's values never influence another country's
#'                      imputation.  This guarantees that clustering results for a
#'                      given country are identical regardless of which other countries
#'                      are included in the same run.
#'                      When \code{NULL} (default), medians are computed globally
#'                      across all rows — the original behaviour.
#'
#' @return Data frame with NAs replaced.
#' @export
impute_medians <- function(df, numeric_cols, group_col = NULL) {
  if (!is.null(group_col)) {
    df |>
      dplyr::group_by(.data[[group_col]]) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(numeric_cols),
        ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)
      )) |>
      dplyr::ungroup()
  } else {
    df |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(numeric_cols),
        ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)
      ))
  }
}
