# =============================================================================
# 13_skater_clustering.R
#
# Spatially-Constrained Clustering via SKATER
# (Spatial 'K'luster Analysis by Tree Edge Removal)
#
# These functions complement the PCA + k-means workflow in 05/06 by adding
# geographic contiguity as a hard constraint: every cluster must form a
# spatially connected region.  The approach is:
#
#   1. Build a spatial adjacency graph (queen / distance threshold).
#   2. Compute edge costs in PCA-score space.
#   3. Derive a Minimum Spanning Tree (MST).
#   4. Prune the MST into k subtrees — each subtree = one bioregion.
#
# Main exported functions
# ─────────────────────────────────────────────────────────────────────────────
#   run_skater_clustering()   Full pipeline: PCA → neighbour graph → MST → SKATER
#   plot_skater_elbow()       Elbow / SSE curve to guide k selection
#   make_skater_heatmap()     Z-score heatmap of original variables by cluster
# =============================================================================


# -----------------------------------------------------------------------------
# Internal helper: build a connected neighbour list from an sf object
# .build_connected_nb(polygons, dist_threshold, k_fallback)
# -----------------------------------------------------------------------------
.build_connected_nb <- function(polygons, dist_threshold = 45000, k_fallback = 1) {

  coords <- sf::st_centroid(sf::st_geometry(polygons))

  # Primary: distance-threshold neighbours
  nb <- spdep::dnearneigh(coords, 0, dist_threshold)

  # If any polygon has no neighbours (islands), force at least k_fallback NN
  if (any(spdep::card(nb) == 0)) {
    message("  Isolated polygons detected — adding ", k_fallback,
            "-nearest-neighbour bridge(s).")
    nb_knn <- spdep::knn2nb(spdep::knearneigh(coords, k = k_fallback))
    nb <- spdep::union.nb(nb, nb_knn)
  }

  # Symmetrise so SKATER's MST is valid
  nb <- spdep::make.sym.nb(nb)

  n_comp <- spdep::n.comp.nb(nb)$nc
  if (n_comp > 1) {
    message("  Warning: neighbour graph has ", n_comp,
            " components — increase dist_threshold or k_fallback ",
            "to obtain fully connected clusters.")
  } else {
    message("  Neighbour graph is fully connected (1 component).")
  }

  nb
}


# =============================================================================
#' Run spatially-constrained clustering using the SKATER algorithm
#'
#' Produces bioregions that respect geographic contiguity: every cluster is a
#' spatially connected region.  The pipeline is:
#' \enumerate{
#'   \item PCA on scaled covariates (optionally with variable-specific weights).
#'   \item Build a spatial adjacency graph from polygon centroids.
#'   \item Compute minimum spanning tree (MST) weighted by Euclidean distance
#'         in PC-score space.
#'   \item Prune the MST into \code{n_clusters} spatially coherent subtrees.
#' }
#'
#' @param polygons_sf  An \code{sf} object of polygons to cluster.  Must already
#'   contain the numeric covariates listed in \code{vars}.
#' @param vars         Character vector of covariate column names used for
#'   clustering.  All must be numeric and present in \code{polygons_sf}.
#' @param n_clusters   Integer.  Number of clusters (bioregions) to produce.
#'   Use \code{\link{plot_skater_elbow}} to guide this choice.
#' @param pca_var      Cumulative variance proportion retained by PCA before
#'   passing scores to SKATER.  Default \code{0.95}.
#' @param elev_col     Optional name of an elevation column to boost in the PCA
#'   space.  If supplied, a scaled copy is appended to the PC matrix with
#'   weight \code{elev_weight}.  Default \code{NULL} (no boost).
#' @param elev_weight  Multiplier applied to the scaled elevation vector before
#'   appending it to the PC matrix.  Only used when \code{elev_col} is not
#'   \code{NULL}.  Default \code{7}.
#' @param dist_threshold Distance threshold (metres, or map units) used to
#'   define neighbours via \code{\link[spdep]{dnearneigh}}.  Default \code{45000}
#'   (45 km — suitable for district-level polygons).  Decrease for dense grids,
#'   increase for sparse/island layouts.
#' @param k_fallback   Minimum number of forced nearest-neighbours added for
#'   isolated polygons (islands) to guarantee graph connectivity.  Default \code{1}.
#' @param seed         Random seed for reproducibility.  Default \code{42}.
#' @param cluster_col  Name of the output column written to \code{polygons_sf}
#'   containing the cluster assignment (factor).  Default \code{"skater_cluster"}.
#'
#' @return The input \code{sf} object with two additional columns:
#' \describe{
#'   \item{\code{cluster_col}}{Factor — SKATER cluster assignment.}
#'   \item{\code{.pca_scores}}{List-column (one numeric vector per row) of the
#'     PC scores used as clustering input.  Useful for downstream diagnostics.}
#' }
#' Three diagnostic objects are stored as attributes on the returned object:
#' \describe{
#'   \item{\code{pca_out}}{The \code{prcomp} result.}
#'   \item{\code{skater_res}}{The raw \code{\link[spdep]{skater}} result.}
#'   \item{\code{X_raw}}{The pre-PCA (scaled) covariate matrix.}
#' }
#'
#' @seealso \code{\link{plot_skater_elbow}}, \code{\link{make_skater_heatmap}}
#'
#' @examples
#' \dontrun{
#' result_sf <- run_skater_clustering(
#'   polygons_sf  = adm2_sf,
#'   vars         = c("elev_median", "pop_mean", "urban_mean",
#'                    "Tropical, savannah", "Trees", "Crops"),
#'   n_clusters   = 8,
#'   elev_col     = "elev_median",
#'   elev_weight  = 5,
#'   dist_threshold = 50000
#' )
#' plot(result_sf["skater_cluster"])
#' }
#'
#' @export
run_skater_clustering <- function(polygons_sf,
                                  vars,
                                  n_clusters     = 8,
                                  pca_var        = 0.95,
                                  elev_col       = NULL,
                                  elev_weight    = 7,
                                  dist_threshold = 45000,
                                  k_fallback     = 1,
                                  seed           = 42,
                                  cluster_col    = "skater_cluster") {

  # ── 0. Validate inputs ─────────────────────────────────────────────────────
  if (!inherits(polygons_sf, "sf"))
    stop("`polygons_sf` must be an sf object.")

  missing_vars <- setdiff(vars, names(polygons_sf))
  if (length(missing_vars) > 0)
    stop("The following variables are not in `polygons_sf`: ",
         paste(missing_vars, collapse = ", "))

  if (!requireNamespace("spdep", quietly = TRUE))
    stop("Package 'spdep' is required. Install with: install.packages('spdep')")

  set.seed(seed)

  # ── 1. Build covariate matrix ──────────────────────────────────────────────
  message("Step 1/5 | Building covariate matrix (", length(vars), " variables)...")

  X_raw <- sf::st_drop_geometry(polygons_sf)[, vars, drop = FALSE]

  # Remove zero-variance and non-finite columns
  ok_cols <- vapply(X_raw, function(x) {
    sd(x, na.rm = TRUE) > 0 && all(is.finite(x))
  }, logical(1))

  if (any(!ok_cols)) {
    message("  Dropping ", sum(!ok_cols), " zero-variance / non-finite column(s): ",
            paste(names(ok_cols)[!ok_cols], collapse = ", "))
    X_raw <- X_raw[, ok_cols, drop = FALSE]
  }

  # ── 2. PCA ─────────────────────────────────────────────────────────────────
  message("Step 2/5 | Running PCA (retaining ", pca_var * 100, "% variance)...")

  pca_out  <- prcomp(X_raw, center = TRUE, scale. = TRUE)
  cum_var  <- cumsum(pca_out$sdev^2) / sum(pca_out$sdev^2)
  num_pcs  <- which(cum_var >= pca_var)[1]
  if (is.na(num_pcs)) num_pcs <- ncol(pca_out$x)

  message("  Using ", num_pcs, " PC(s) (", round(cum_var[num_pcs] * 100, 1), "% explained).")

  X_pca <- pca_out$x[, seq_len(num_pcs), drop = FALSE]

  # Optional elevation boost — NA-safe manual scaling
  if (!is.null(elev_col)) {
    if (!elev_col %in% names(polygons_sf))
      stop("`elev_col` '", elev_col, "' not found in `polygons_sf`.")

    elev_raw    <- sf::st_drop_geometry(polygons_sf)[[elev_col]]
    elev_mu     <- mean(elev_raw, na.rm = TRUE)
    elev_sd     <- sd(elev_raw,   na.rm = TRUE)
    if (is.na(elev_sd) || elev_sd == 0) elev_sd <- 1
    elev_scaled <- ((elev_raw - elev_mu) / elev_sd) * elev_weight
    elev_scaled[is.na(elev_scaled)] <- 0   # impute missing rows with 0 (scaled mean)
    X_pca <- cbind(X_pca, elev_boosted = elev_scaled)
    message("  Elevation boost applied (column '", elev_col,
            "', weight = ", elev_weight, ").")
  }

  # ── 3. Spatial neighbour graph ─────────────────────────────────────────────
  message("Step 3/5 | Building spatial neighbour graph (threshold = ",
          dist_threshold, " map units)...")

  nb    <- .build_connected_nb(polygons_sf, dist_threshold, k_fallback)
  costs <- spdep::nbcosts(nb, X_pca)
  listw <- spdep::nb2listw(nb, costs, style = "B")
  mst   <- spdep::mstree(listw)

  # ── 4. SKATER ──────────────────────────────────────────────────────────────
  message("Step 4/5 | Running SKATER with k = ", n_clusters, "...")

  skater_res <- spdep::skater(mst[, 1:2], X_pca, ncuts = n_clusters - 1L)

  # ── 5. Attach results ──────────────────────────────────────────────────────
  message("Step 5/5 | Attaching results to sf object...")

  polygons_sf[[cluster_col]] <- factor(skater_res$groups)

  # Store diagnostics as attributes for downstream use
  attr(polygons_sf, "pca_out")    <- pca_out
  attr(polygons_sf, "skater_res") <- skater_res
  attr(polygons_sf, "X_raw")      <- X_raw
  attr(polygons_sf, "X_pca")      <- X_pca
  attr(polygons_sf, "vars")       <- names(X_raw)

  message("Done! Cluster assignments stored in column '", cluster_col, "'.")
  polygons_sf
}


# =============================================================================
#' Elbow plot to guide SKATER cluster count selection
#'
#' Computes the within-cluster sum of squared errors (SSE) in PCA-score space
#' for \code{k = 1} to \code{max_k} and plots the result.  The optimal number
#' of clusters is typically at the "elbow" — the point where adding another
#' cluster yields diminishing SSE reductions.
#'
#' @param polygons_sf   An \code{sf} object previously processed by
#'   \code{\link{run_skater_clustering}}, **or** any \code{sf} object if
#'   \code{vars} and \code{pca_var} are supplied directly.
#' @param vars          Character vector of covariate column names.  If
#'   \code{polygons_sf} has a \code{"vars"} attribute (set automatically by
#'   \code{run_skater_clustering}), this argument can be omitted.
#' @param max_k         Maximum number of clusters to evaluate.  Default \code{20}.
#' @param pca_var       Cumulative variance retained by PCA.  Default \code{0.95}.
#' @param elev_col      Optional elevation column name (must match what was used
#'   in \code{run_skater_clustering}).  Default \code{NULL}.
#' @param elev_weight   Elevation boost multiplier.  Default \code{7}.
#' @param dist_threshold Distance threshold for neighbour graph.  Default \code{45000}.
#' @param k_fallback    Forced NN count for isolated polygons.  Default \code{1}.
#' @param seed          Random seed.  Default \code{42}.
#'
#' @return A \code{ggplot} object (the elbow curve).  Also prints the plot.
#'
#' @seealso \code{\link{run_skater_clustering}}
#'
#' @examples
#' \dontrun{
#' plot_skater_elbow(adm2_sf, vars = my_vars, max_k = 20)
#' }
#'
#' @export
plot_skater_elbow <- function(polygons_sf,
                               vars           = NULL,
                               max_k          = 20,
                               pca_var        = 0.95,
                               elev_col       = NULL,
                               elev_weight    = 7,
                               dist_threshold = 45000,
                               k_fallback     = 1,
                               seed           = 42) {

  if (!requireNamespace("spdep", quietly = TRUE))
    stop("Package 'spdep' is required.")

  set.seed(seed)

  # Recover vars from attribute if available
  if (is.null(vars)) {
    vars <- attr(polygons_sf, "vars")
    if (is.null(vars))
      stop("`vars` must be supplied — or run run_skater_clustering() first to set the attribute.")
  }

  # Build PCA matrix (same logic as run_skater_clustering)
  X_raw <- sf::st_drop_geometry(polygons_sf)[, vars, drop = FALSE]
  ok    <- vapply(X_raw, function(x) sd(x, na.rm = TRUE) > 0 && all(is.finite(x)), logical(1))
  X_raw <- X_raw[, ok, drop = FALSE]

  pca_out <- prcomp(X_raw, center = TRUE, scale. = TRUE)
  cum_var <- cumsum(pca_out$sdev^2) / sum(pca_out$sdev^2)
  num_pcs <- which(cum_var >= pca_var)[1]
  if (is.na(num_pcs)) num_pcs <- ncol(pca_out$x)

  X_pca <- pca_out$x[, seq_len(num_pcs), drop = FALSE]

  # Elevation boost — impute NA values with 0 (post-scaling mean) to avoid NaN
  if (!is.null(elev_col) && elev_col %in% names(polygons_sf)) {
    elev_raw    <- sf::st_drop_geometry(polygons_sf)[[elev_col]]
    elev_mu     <- mean(elev_raw, na.rm = TRUE)
    elev_sd     <- sd(elev_raw,   na.rm = TRUE)
    if (is.na(elev_sd) || elev_sd == 0) elev_sd <- 1
    elev_scaled <- ((elev_raw - elev_mu) / elev_sd) * elev_weight
    elev_scaled[is.na(elev_scaled)] <- 0
    X_pca <- cbind(X_pca, elev_boosted = elev_scaled)
  }

  # Drop any rows still containing non-finite values
  finite_rows <- apply(X_pca, 1, function(r) all(is.finite(r)))
  if (any(!finite_rows)) {
    message("  Dropping ", sum(!finite_rows),
            " row(s) with non-finite PCA scores from elbow computation.")
    X_pca       <- X_pca[finite_rows, , drop = FALSE]
    polygons_sf <- polygons_sf[finite_rows, ]
  }

  # Build MST once
  nb    <- .build_connected_nb(polygons_sf, dist_threshold, k_fallback)
  costs <- spdep::nbcosts(nb, X_pca)
  listw <- spdep::nb2listw(nb, costs, style = "B")
  mst   <- spdep::mstree(listw)

  # Compute SSE for k = 1 to max_k
  message("Computing SSE for k = 1 to ", max_k, " (this may take a moment)...")
  sse_values <- numeric(max_k)
  sse_values[1] <- sum(apply(X_pca, 2, function(x) sum((x - mean(x, na.rm=TRUE))^2, na.rm=TRUE)))

  for (k in seq(2, max_k)) {
    sk_tmp  <- spdep::skater(mst[, 1:2], X_pca, ncuts = k - 1L)
    groups  <- sk_tmp$groups
    sse_k   <- 0
    for (g in seq_len(k)) {
      cl_data <- X_pca[groups == g, , drop = FALSE]
      if (nrow(cl_data) > 0) {
        centered <- scale(cl_data, scale = FALSE)
        sse_k    <- sse_k + sum(centered^2, na.rm = TRUE)
      }
    }
    sse_values[k] <- sse_k
    message("  k = ", k, "  SSE = ", round(sse_k, 1))
  }

  elbow_df <- data.frame(k = seq_len(max_k), SSE = sse_values)

  p <- ggplot2::ggplot(elbow_df, ggplot2::aes(x = k, y = SSE)) +
    ggplot2::geom_line(colour = "steelblue", linewidth = 1) +
    ggplot2::geom_point(colour = "darkblue", size = 2.5) +
    ggplot2::scale_x_continuous(breaks = seq_len(max_k)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(
      title    = "SKATER Elbow Method",
      subtitle = paste0("SSE computed in PCA space (", num_pcs, " PCs, ",
                        round(cum_var[num_pcs] * 100, 1), "% variance)"),
      x        = "Number of clusters (k)",
      y        = "Total within-cluster SSE"
    )

  print(p)
  invisible(p)
}


# =============================================================================
#' Z-score heatmap of original variables by SKATER cluster
#'
#' Visualises the mean z-score of each covariate within each cluster, making it
#' easy to interpret what environmental conditions define each bioregion.
#'
#' @param polygons_sf   An \code{sf} object returned by
#'   \code{\link{run_skater_clustering}}, with the \code{"X_raw"} and
#'   \code{"vars"} attributes set automatically.
#' @param cluster_col   Name of the cluster column.  Default \code{"skater_cluster"}.
#' @param vars          Character vector of variable names to include.  If
#'   \code{NULL} (default), uses the \code{"vars"} attribute on
#'   \code{polygons_sf} (set by \code{run_skater_clustering}).
#' @param title         Plot title.  Default \code{"Bioregion Characteristics (SKATER)"}.
#' @param subtitle      Plot subtitle.  Default describes the z-score scaling.
#' @param low_col       Colour for low z-scores.  Default \code{"#0571b0"} (blue).
#' @param high_col      Colour for high z-scores.  Default \code{"#ca0020"} (red).
#'
#' @return A \code{ggplot} object (the heatmap).  Also prints the plot.
#'
#' @seealso \code{\link{run_skater_clustering}}
#'
#' @examples
#' \dontrun{
#' make_skater_heatmap(result_sf, cluster_col = "skater_cluster")
#' }
#'
#' @export
make_skater_heatmap <- function(polygons_sf,
                                cluster_col = "skater_cluster",
                                vars        = NULL,
                                title       = "Bioregion Characteristics (SKATER)",
                                subtitle    = "Values are z-scores relative to the study-area mean",
                                low_col     = "#0571b0",
                                high_col    = "#ca0020") {

  if (!cluster_col %in% names(polygons_sf))
    stop("Column '", cluster_col, "' not found. Run run_skater_clustering() first.")

  # Recover X_raw from attribute or rebuild from vars
  X_raw <- attr(polygons_sf, "X_raw")

  if (is.null(X_raw)) {
    if (is.null(vars))
      stop("`vars` must be supplied when the `X_raw` attribute is absent.")
    X_raw <- sf::st_drop_geometry(polygons_sf)[, vars, drop = FALSE]
  }

  if (is.null(vars)) vars <- colnames(X_raw)

  cluster_vec <- sf::st_drop_geometry(polygons_sf)[[cluster_col]]

  summary_df <- as.data.frame(scale(X_raw)) %>%
    dplyr::mutate(cluster = as.factor(cluster_vec)) %>%
    dplyr::group_by(.data$cluster) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), mean, na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_longer(-.data$cluster,
                        names_to  = "variable",
                        values_to = "mean_zscore")

  p <- ggplot2::ggplot(summary_df,
                       ggplot2::aes(x = .data$cluster,
                                    y = .data$variable,
                                    fill = .data$mean_zscore)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::scale_fill_gradient2(
      low      = low_col,
      mid      = "white",
      high     = high_col,
      midpoint = 0,
      name     = "Z-score"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      x        = "Bioregion cluster",
      y        = "Environmental variable"
    ) +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_text(size = 9),
      axis.text.x  = ggplot2::element_text(face = "bold"),
      panel.grid   = ggplot2::element_blank()
    )

  print(p)
  invisible(p)
}


# =============================================================================
#' PCA loading plot for SKATER input
#'
#' A thin wrapper around \code{\link[factoextra]{fviz_pca_var}} that retrieves
#' the \code{prcomp} result stored by \code{\link{run_skater_clustering}}.
#'
#' @param polygons_sf  An \code{sf} object returned by
#'   \code{\link{run_skater_clustering}}.
#' @param top_n        Number of top-contributing variables to label.
#'   Default \code{15}.
#'
#' @return A \code{ggplot} object (invisible).  The plot is printed as a side-effect.
#'
#' @export
plot_skater_pca_loadings <- function(polygons_sf, top_n = 15) {

  if (!requireNamespace("factoextra", quietly = TRUE))
    stop("Package 'factoextra' is required.")

  pca_out <- attr(polygons_sf, "pca_out")
  if (is.null(pca_out))
    stop("No `pca_out` attribute found. Run run_skater_clustering() first.")

  p <- factoextra::fviz_pca_var(
    pca_out,
    col.var       = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel         = TRUE,
    title         = "PCA Variable Loadings — SKATER Input"
  )

  print(p)
  invisible(p)
}
