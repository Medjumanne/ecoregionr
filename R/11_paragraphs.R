# =============================================================================
# 11. generate_cluster_paragraphs
# =============================================================================
#' Generate human-readable paragraphs describing each cluster.

#' @param summary_df      Output of summarize_clusters_compact().
#' @param numeric_vars    Character vector of numeric covariate names
#'                        (must match column prefixes in summary_df).
#' @param lc_classes      Character vector of land-cover class names
#'                        (used to separate climate from LC in categories).
#' @param altitude_col    The numeric_var whose range is used to assign
#'                        altitude labels.  Set NULL to skip.
#' @param altitude_breaks Named numeric vector of breaks:
#'                        c(lowland_max = 500, midalt_max = 1500).
#' @param altitude_labels Named character vector of labels for each band.
#' @param group_label     Column name for group (default "Group").
#'
#' @return The summary_df with an extra `paragraph` column.
#' @export
generate_cluster_paragraphs <- function(summary_df,
                                        numeric_vars,
                                        lc_classes     = c("Trees","Crops",
                                                           "Built Area","Rangeland",
                                                           "Water","Bare ground",
                                                           "Flooded vegetation",
                                                           "Snow/Ice"),
                                        altitude_col   = NULL,
                                        altitude_breaks = c(500, 1500),
                                        altitude_labels = c("Lowland (< 500 m)",
                                                            "Mid-altitude (500–1500 m)",
                                                            "Highland (≥ 1500 m)"),
                                        group_label    = "Group") {

  # Helper: assign altitude label from a range string "[min-max]"
  range_to_mean <- function(rng) {
    clean <- gsub("\\[|\\]", "", rng)
    vals  <- suppressWarnings(as.numeric(strsplit(clean, "-")[[1]]))
    mean(vals, na.rm = TRUE)
  }

  get_alt_label <- function(rng) {
    if (is.na(rng)) return("unknown altitude")
    m <- range_to_mean(rng)
    if      (m < altitude_breaks[1]) altitude_labels[1]
    else if (m < altitude_breaks[2]) altitude_labels[2]
    else                              altitude_labels[3]
  }

  # Helper: parse "Class (value), ..." string
  parse_categories <- function(cat_str) {
    if (is.na(cat_str) || nchar(trimws(cat_str)) == 0) return(list())
    items  <- regmatches(cat_str,
                         gregexpr("[^(]+\\([^)]*\\)", cat_str, perl = TRUE))[[1]]
    items  <- trimws(items)
    values <- suppressWarnings(
      as.numeric(gsub(".*\\(([^)]+)\\).*", "\\1", items))
    )
    list(items = items, values = values)
  }

  build_para <- function(row) {
    alt_label <- if (!is.null(altitude_col)) {
      col_nm <- paste0(altitude_col, "_range")
      get_alt_label(row[[col_nm]])
    } else ""

    cat_parsed  <- parse_categories(row$Categories)
    cat_items   <- cat_parsed$items
    cat_vals    <- cat_parsed$values

    is_lc       <- grepl(paste(lc_classes, collapse = "|"), cat_items)
    clim_items  <- cat_items[!is_lc]
    lc_items    <- cat_items[is_lc]

    clim_text <- if (length(clim_items)) {
      paste(clim_items[order(cat_vals[!is_lc], decreasing = TRUE)], collapse = ", ")
    } else "—"
    lc_text <- if (length(lc_items)) {
      paste(lc_items[order(cat_vals[is_lc], decreasing = TRUE)], collapse = ", ")
    } else "—"

    # Build numeric variable sentences
    range_strs <- sapply(numeric_vars, function(v) {
      col <- paste0(v, "_range")
      if (!col %in% names(row) || is.na(row[[col]])) return(NULL)
      paste0(v, ": ", row[[col]])
    })
    range_strs <- Filter(Negate(is.null), range_strs)

    paste0(
      "Cluster ", row$cluster, " (", row[[group_label]], ")",
      if (nchar(alt_label) > 0) paste0(" – ", alt_label) else "", ".\n",
      "  Climate types: ", clim_text, ".\n",
      "  Land cover: ", lc_text, ".\n",
      "  Numeric ranges: ", paste(range_strs, collapse = "; "), ".\n",
      "  Administrative units: ", row$Unit_names, "."
    )
  }

  summary_df |>
    mutate(paragraph = map_chr(seq_len(n()), ~ build_para(summary_df[.x, ])))
}
