# =============================================================================
# 4. impute_medians
# =============================================================================
#' Replace NA values in selected columns with column medians.

#' @param df            Data frame.
#' @param numeric_cols  Character vector of column names to impute.
#'
#' @return Data frame with NAs replaced.
#' @export
impute_medians <- function(df, numeric_cols) {
  df |>
    mutate(across(
      all_of(numeric_cols),
      ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)
    ))
}
