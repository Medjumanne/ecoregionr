#' Main workflow function — one call does everything
#'
#' Runs the full ecoregion pipeline: load polygons → extract raster stats →
#' cluster → visualise → export maps, tables, and text descriptions.
#'
#' @param shapefile          Path to the admin polygon shapefile.
#' @param continuous_rasters Named list of continuous raster file paths.
#'   Names become column prefixes (e.g. list(elev = "elev.tif", pop = "pop.tif")).
#' @param categorical_rasters Named list of categorical raster file paths.
#'   Must include elements named \code{kg} and \code{landcover}.
#' @param id_col             Name of the unique polygon ID column.
#'   Created automatically as a sequential integer if absent. Default \code{"ID"}.
#' @param unit_col           Name of the sub-unit label column (e.g. district
#'   or county name). Used in summary tables and popup labels.
#' @param group_col          Column name for grouping (e.g. \code{"ADM0_NAME"}).
#'   Set \code{NULL} for single-country mode (requires \code{country_name}).
#' @param country_name       Country label used when \code{group_col = NULL}.
#' @param study_groups       Character vector of group values to keep.
#'   \code{NULL} keeps all rows (required when \code{group_col = NULL}).
#' @param continuous_funs    Named list of summary functions per raster.
#'   Each element is itself a named list, e.g.
#'   \code{list(elev = list(median = median), pop = list(mean = mean))}.
#'   If \code{NULL} (default), \code{mean} is applied to every raster.
#' @param agg_facts          Named list of aggregation factors (integer) per
#'   raster. Use values > 1 to speed up extraction of high-resolution rasters.
#'   Default \code{1} for all rasters (no aggregation).
#' @param lc_exclude_vals    Integer vector of land-cover raster values to mask
#'   before extraction (e.g. clouds, nodata). Default removes clouds (10) and
#'   common 32-bit sentinel values.
#' @param numeric_vars       Character vector of numeric covariate column names
#'   as they appear after extraction (e.g. \code{c("elev_median","pop_mean")}).
#'   If \code{NULL} (default), auto-detected via \code{_mean|_median|_max}.
#' @param min_k              Minimum number of clusters. Default \code{6}.
#' @param max_k              Maximum number of clusters to test. Default \code{15}.
#' @param pca_var            Cumulative variance threshold for PC retention.
#'   Default \code{0.90}.
#' @param nstart             k-means nstart for the final run. Default \code{50}.
#' @param seed               Random seed. Default \code{1}.
#' @param visualize          Logical. Print PCA/silhouette/consensus plots during
#'   clustering. Default \code{TRUE}.
#' @param output_dir         Path to the output directory. Created if absent.
#'   Default \code{"output"}.
#' @param projection         PROJ string for the static map.
#'   Default Mollweide (\code{"+proj=moll"}).
#' @param viridis_option     Viridis palette name for maps. Default \code{"turbo"}.
#' @param cat_threshold      Minimum mean proportion for a category to appear in
#'   cluster descriptions. Default \code{0.01}.
#' @param altitude_breaks    Numeric vector of two elevation breakpoints (m) used
#'   to assign lowland / mid-altitude / highland labels. Default \code{c(500, 1500)}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{clusters}{Data frame with cluster assignments and PC scores.}
#'   \item{adm2_clusters}{sf object — polygons joined with cluster and popup fields.}
#'   \item{env_df}{Wide covariate table used for clustering.}
#'   \item{cluster_desc}{Compact cluster summary data frame.}
#'   \item{bioregion_text}{Data frame with a \code{paragraph} column per cluster.}
#'   \item{leaflet_map}{Leaflet map object.}
#'   \item{static_map}{ggplot map object.}
#'   \item{heatmap_plots}{Named list of heatmap ggplot objects (one per group).}
#' }
#'
#' @export
run_ecoregion_workflow <- function(shapefile,
                                   continuous_rasters,
                                   categorical_rasters,
                                   id_col           = "ID",
                                   unit_col         = "ADM2_NAME",
                                   group_col        = "ADM0_NAME",
                                   country_name     = NULL,
                                   study_groups     = NULL,
                                   continuous_funs  = NULL,
                                   agg_facts        = 1,
                                   lc_exclude_vals  = c(10, 4294967295, 4294967296),
                                   numeric_vars     = NULL,
                                   min_k            = 6,
                                   max_k            = 15,
                                   pca_var          = 0.90,
                                   nstart           = 50,
                                   seed             = 1,
                                   visualize        = TRUE,
                                   output_dir       = "output",
                                   projection       = "+proj=moll",
                                   viridis_option   = "turbo",
                                   cat_threshold    = 0.01,
                                   altitude_breaks  = c(500, 1500)) {

  ensure_dir(output_dir)

  # ── 1. Load polygons ─────────────────────────────────────────────────────────
  message("=== [1/9] Loading polygons ===")
  polygons <- sf::st_read(shapefile, quiet = TRUE)

  if (!unit_col %in% names(polygons))
    stop("unit_col '", unit_col, "' not found in shapefile. Available: ",
         paste(names(polygons), collapse = ", "))

  if (!id_col %in% names(polygons)) polygons[[id_col]] <- seq_len(nrow(polygons))

  # Single-country guard
  if (is.null(group_col)) {
    if (is.null(country_name))
      stop("country_name must be supplied when group_col = NULL.")
    group_col             <- ".group"
    polygons[[group_col]] <- country_name
    message("Single-country mode: group label = '", country_name, "'")
  }

  if (!is.null(study_groups))
    polygons <- polygons[polygons[[group_col]] %in% study_groups, ]

  polygons <- sf::st_make_valid(polygons)

  # ── 2. Load rasters ──────────────────────────────────────────────────────────
  message("=== [2/9] Loading rasters ===")
  cont_rasts <- lapply(continuous_rasters, terra::rast)
  cat_rasts  <- lapply(categorical_rasters, terra::rast)

  if (!all(c("kg", "landcover") %in% names(cat_rasts)))
    stop("categorical_rasters must have elements named 'kg' and 'landcover'.")

  polygons <- sf::st_transform(polygons, crs = terra::crs(cont_rasts[[1]]))

  # ── 3. Continuous extraction ──────────────────────────────────────────────────
  message("=== [3/9] Extracting continuous raster stats ===")

  # Default: mean for every raster
  if (is.null(continuous_funs))
    continuous_funs <- setNames(
      rep(list(list(mean = mean)), length(cont_rasts)),
      names(cont_rasts)
    )

  # Normalise agg_facts to a named list
  if (length(agg_facts) == 1)
    agg_facts <- setNames(rep(list(agg_facts), length(cont_rasts)),
                          names(cont_rasts))

  stats_sf <- extract_continuous_stats(
    polygons       = polygons,
    rasters        = cont_rasts,
    funs           = continuous_funs,
    id_col         = id_col,
    aggregate_fact = agg_facts
  )

  # ── 4. Categorical extraction ─────────────────────────────────────────────────
  message("=== [4/9] Extracting categorical compositions ===")

  kg_wide <- extract_categorical_composition(
    polygons   = polygons,
    cat_raster = cat_rasts$kg,
    lookup     = kg_lookup,
    id_col     = id_col
  )

  lc_wide <- extract_categorical_composition(
    polygons     = polygons,
    cat_raster   = cat_rasts$landcover,
    lookup       = lc_lookup,
    exclude_vals = lc_exclude_vals,
    id_col       = id_col
  )

  # ── 5. Build & impute covariate table ────────────────────────────────────────
  message("=== [5/9] Building cluster table ===")

  # Auto-detect numeric vars if not supplied
  stat_col_names <- setdiff(names(sf::st_drop_geometry(stats_sf)),
                            c(id_col, group_col, names(polygons)))
  if (is.null(numeric_vars))
    numeric_vars <- grep("_mean$|_median$|_max$", stat_col_names, value = TRUE)

  env_df <- build_cluster_table(
    polygons   = stats_sf,
    cat_tables = list(kg_wide, lc_wide),
    keep_cols  = c(group_col, numeric_vars),
    id_col     = id_col
  )

  # Fill residual NAs in proportion columns with 0
  cat_prop_cols <- setdiff(names(env_df), c(id_col, group_col, numeric_vars))
  env_df <- dplyr::mutate(env_df,
                          dplyr::across(dplyr::all_of(cat_prop_cols), \(x) tidyr::replace_na(x, 0))
  )

  env_df <- impute_medians(env_df, numeric_vars)

  # ── 6. Clustering ─────────────────────────────────────────────────────────────
  message("=== [6/9] Clustering ===")

  clusters <- run_clustering(
    df        = env_df,
    group_col = group_col,
    id_col    = id_col,
    min_k     = min_k,
    max_k     = max_k,
    pca_var   = pca_var,
    nstart    = nstart,
    visualize = visualize,
    seed      = seed
  )

  summary_table <- dplyr::group_by(clusters, .data[[group_col]]) |>
    dplyr::summarise(n_units = dplyr::n(),
                     k       = dplyr::first(k_used),
                     .groups = "drop")
  message("Cluster summary:"); print(summary_table)
  utils::write.csv(summary_table,
                   file.path(output_dir, "cluster_summary.csv"),
                   row.names = FALSE)

  # ── 7. Heatmaps ───────────────────────────────────────────────────────────────
  message("=== [7/9] Heatmaps ===")

  study_groups_used <- unique(clusters[[group_col]])
  heatmap_plots <- lapply(study_groups_used, function(g) {
    p <- make_cluster_heatmap(clusters, group_name = g, group_col = group_col)
    print(p)
    p
  })
  names(heatmap_plots) <- study_groups_used

  # ── 8. Join clusters to polygons + build maps ─────────────────────────────────
  message("=== [8/9] Building maps ===")

  kg_cols <- setdiff(names(kg_wide), id_col)
  lc_cols <- setdiff(names(lc_wide), id_col)

  env_summary <- dplyr::group_by(env_df, .data[[id_col]]) |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(c(kg_cols, lc_cols)),
                    \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
    )

  adm2_clusters <- stats_sf |>
    dplyr::left_join(dplyr::select(clusters, dplyr::all_of(id_col), cluster),
                     by = id_col) |>
    dplyr::left_join(env_summary, by = id_col) |>
    dplyr::filter(!is.na(cluster))

  wrap_popup <- function(vals, nms) {
    keep <- !is.na(vals) & vals > 0
    if (!any(keep)) return("None")
    vals <- vals[keep]; nms <- nms[keep]
    ord  <- order(vals, decreasing = TRUE)
    paste(paste0(nms[ord], ": ", round(vals[ord], 4)), collapse = "<br/>")
  }

  adm2_clusters <- adm2_clusters |>
    dplyr::rowwise() |>
    dplyr::mutate(
      kg_popup = wrap_popup(dplyr::c_across(dplyr::all_of(kg_cols)), kg_cols),
      lc_popup = wrap_popup(dplyr::c_across(dplyr::all_of(lc_cols)), lc_cols)
    ) |>
    dplyr::ungroup()

  # Country border
  border_label <- if (!is.null(study_groups)) study_groups else country_name
  countries_sf <- tryCatch(
    rnaturalearth::ne_countries(country = border_label, returnclass = "sf"),
    error = function(e) NULL
  )

  # Leaflet
  label_fn <- function(row) {
    htmltools::HTML(paste0(
      "<strong>", row[[unit_col]], "</strong><br/>",
      "Cluster: ", row$cluster, "<br/>",
      "<strong>Climate:</strong><br/>",    row$kg_popup, "<br/>",
      "<strong>Land cover:</strong><br/>", row$lc_popup
    ))
  }

  leaflet_map <- build_leaflet_map(
    polygons_clustered = adm2_clusters,
    cluster_col        = "cluster",
    label_fn           = label_fn,
    border_sf          = countries_sf,
    palette_option     = viridis_option,
    title              = "Bioregion"
  )

  htmlwidgets::saveWidget(
    leaflet_map,
    file.path(output_dir, "bioregion_map_interactive.html"),
    selfcontained = FALSE
  )

  # Static ggplot map
  static_map <- make_static_map(
    polygons_clustered = adm2_clusters,
    cluster_col        = "cluster",
    border_sf          = countries_sf,
    projection         = projection,
    viridis_option     = viridis_option,
    legend_title       = "Bioregion",
    simplify           = TRUE,
    keep_prop          = 0.02
  )
  print(static_map)

  ggplot2::ggsave(file.path(output_dir, "bioregion_map_static.pdf"),
                  static_map, width = 12, height = 10)
  ggplot2::ggsave(file.path(output_dir, "bioregion_map_static.png"),
                  static_map, width = 12, height = 10, dpi = 300)

  # ── 9. Summaries & text descriptions ─────────────────────────────────────────
  message("=== [9/9] Summaries & text descriptions ===")

  cluster_desc <- summarize_clusters_compact(
    cluster_df    = clusters,
    polygons_sf   = adm2_clusters,
    numeric_vars  = numeric_vars,
    group_col     = group_col,
    id_col        = id_col,
    name_col      = unit_col,
    cat_threshold = cat_threshold
  )
  utils::write.csv(cluster_desc,
                   file.path(output_dir, "cluster_descriptions.csv"),
                   row.names = FALSE)

  cluster_desc_text <- dplyr::mutate(
    cluster_desc,
    Categories = gsub("PC[0-9]+\\s*\\([^)]*\\),?\\s*", "", Categories)
  )

  bioregion_text <- generate_cluster_paragraphs(
    summary_df      = cluster_desc_text,
    numeric_vars    = numeric_vars,
    lc_classes      = unname(lc_lookup),
    altitude_col    = grep("_median$", numeric_vars, value = TRUE)[1],
    altitude_breaks = altitude_breaks,
    altitude_labels = c(
      paste0("Lowland (< ",      altitude_breaks[1], " m above sea level)"),
      paste0("Mid-altitude (",   altitude_breaks[1], "\u2013",
             altitude_breaks[2], " m above sea level)"),
      paste0("Highland (\u2265", altitude_breaks[2], " m above sea level)")
    ),
    group_label = "Group"
  )

  bioregion_by_group <- dplyr::group_by(bioregion_text, Group) |>
    dplyr::summarise(text = paste(paragraph, collapse = "\n\n"),
                     .groups = "drop")

  writeLines(
    paste(paste0("### ", bioregion_by_group$Group),
          bioregion_by_group$text, sep = "\n\n"),
    file.path(output_dir, "bioregion_descriptions.txt")
  )

  message("\u2705 Workflow complete. Outputs saved to: ", output_dir)

  invisible(list(
    clusters      = clusters,
    adm2_clusters = adm2_clusters,
    env_df        = env_df,
    cluster_desc  = cluster_desc,
    bioregion_text = bioregion_text,
    leaflet_map   = leaflet_map,
    static_map    = static_map,
    heatmap_plots = heatmap_plots
  ))
}
