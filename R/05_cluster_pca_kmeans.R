
# =============================================================================
# 5. cluster_pca_kmeans  (single-group)
# =============================================================================
#' PCA + k-means clustering for one group (e.g. one country).
#'
#' @param df             Tibble / data.frame for one group.
#' @param group_col      Name of the grouping column (e.g. "ADM0_NAME").
#'                       Used for status messages and excluded from clustering.
#' @param id_col         Name of the unique ID column (excluded from clustering).
#' @param min_k          Minimum number of clusters to allow (default 6).
#' @param max_k          Maximum number of clusters to test (default 15).
#' @param pca_var        Cumulative variance threshold for PC retention (default 0.90).
#' @param nstart         k-means nstart for the final run (default 50).
#' @param weights        Named vector of variable weights for PCA (e.g. c("elev_median" = 3)).
#' @param strict_mode    Force strict parameter settings.
#' @param visualize      Print diagnostic plots.
#' @param seed           Random seed.
#'
#' @return Data frame with `cluster`, `k_used`, and PC score columns.
#' @export
cluster_pca_kmeans <- function(df,
                               group_col   = NULL,
                               id_col      = "ID",
                               min_k       = 2,
                               max_k       = 5,
                               pca_var     = 0.90,
                               nstart      = 50,
                               weights     = NULL,
                               strict_mode = FALSE,
                               visualize   = TRUE,
                               seed        = 1) {
  set.seed(seed)

  group_label <- if (!is.null(group_col)) unique(df[[group_col]])[1] else "group"

  # STRICT MODE OVERRIDES
  if (strict_mode) {
    min_k   <- 6
    max_k   <- 15
    pca_var <- 0.90
    nstart  <- 50
  }

  # 1. Prepare data
  exclude_cols <- unique(c(id_col, if (!is.null(group_col)) group_col))
  X <- df[, !names(df) %in% exclude_cols, drop = FALSE]

  sds <- apply(X, 2, sd, na.rm = TRUE)
  sds[is.na(sds)] <- 0
  removed_vars <- names(X)[sds == 0]
  X <- X[, sds > 0, drop = FALSE]

  if (length(removed_vars) > 0) {
    message(group_label, " | Removed ", length(removed_vars), " zero-variance variables")
  }

  keep_rows <- apply(X, 1, function(r) all(is.finite(r)))
  X  <- X[keep_rows, , drop = FALSE]
  df <- df[keep_rows, , drop = FALSE]

  if (ncol(X) < 3 || nrow(X) < 8) {
    message(group_label, " | Too small → single cluster")
    df$cluster <- factor(1)
    df$k_used  <- 1L
    return(df)
  }

  # 2. Scale
  X_scaled <- scale(X)

  # Apply weights if provided
  if (!is.null(weights)) {
    common_vars <- intersect(names(weights), colnames(X_scaled))
    for (v in common_vars) {
      X_scaled[, v] <- X_scaled[, v] * weights[[v]]
      message(group_label, " | Weighted ", v, " by ", weights[[v]])
    }
  }

  # -----------------------------
  # 3. PCA
  # -----------------------------
  pca_res <- prcomp(X_scaled, center = FALSE, scale. = FALSE)

  cum_var <- cumsum(pca_res$sdev^2) / sum(pca_res$sdev^2)
  n_pc <- which.max(cum_var >= pca_var)
  n_pc <- min(max(n_pc, 3), ncol(X_scaled))

  message(group_label, " | Using ", n_pc, " PCs (",
          round(cum_var[n_pc] * 100, 1), "% variance explained)")

  pca_data <- as.data.frame(pca_res$x[, 1:n_pc])
  df <- cbind(df, pca_data)

  # -----------------------------
  # 4. Guard: cap max_k by data size
  # -----------------------------
  # NbClust and kmeans need at least k+1 data points per cluster.
  # If the stratum is small (common with elevation-band splitting), silently
  # reduce max_k and warn — never let max_k >= nrow(pca_data).
  safe_max_k <- min(max_k, floor(nrow(pca_data) / 2))
  safe_min_k <- min(min_k, safe_max_k)

  if (safe_max_k < max_k) {
    message(group_label, " | Stratum has only ", nrow(pca_data),
            " rows — reducing max_k from ", max_k, " to ", safe_max_k,
            " and min_k from ", min_k, " to ", safe_min_k, ".")
  }

  if (safe_max_k < 2) {
    message(group_label, " | Too few rows for multi-cluster solution → single cluster.")
    df$cluster <- factor(1)
    df$k_used  <- 1L
    return(df)
  }

  # -----------------------------
  # 5. NbClust
  # -----------------------------
  message(group_label, " | Running NbClust (min_k=", safe_min_k,
          ", max_k=", safe_max_k, ")...")

  nb_res <- tryCatch(
    NbClust(pca_data,
            min.nc = safe_min_k,
            max.nc = safe_max_k,
            method = "kmeans",
            index  = "all"),
    error = function(e) {
      message(group_label, " | NbClust error: ", conditionMessage(e))
      NULL
    }
  )

  # Silhouette fallback — also used when vote_data is empty after filtering
  .silhouette_k <- function(pca_data, safe_min_k, safe_max_k) {
    ks  <- seq(safe_min_k, safe_max_k)
    sil <- vapply(ks, function(k) {
      tryCatch({
        km  <- kmeans(pca_data, centers = k, nstart = 25)
        mean(cluster::silhouette(km$cluster, dist(pca_data))[, 3], na.rm = TRUE)
      }, error = function(e) NA_real_)
    }, numeric(1))
    best_idx <- which.max(sil)
    if (length(best_idx) == 0 || is.na(best_idx)) return(safe_min_k)
    ks[best_idx]
  }

  if (!is.null(nb_res)) {
    vote_data <- as.integer(nb_res$Best.nc[1, ])
    vote_data <- vote_data[is.finite(vote_data)]
    vote_data <- vote_data[vote_data >= safe_min_k & vote_data <= safe_max_k]

    if (length(vote_data) == 0) {
      # All index votes fell outside the search range — use silhouette
      message(group_label, " | NbClust votes all out of range — falling back to silhouette.")
      k_raw <- .silhouette_k(pca_data, safe_min_k, safe_max_k)
    } else {
      k_raw <- as.integer(names(which.max(table(vote_data))))
    }
  } else {
    k_raw <- .silhouette_k(pca_data, safe_min_k, safe_max_k)
    message(group_label, " | Silhouette fallback k = ", k_raw)
  }

  # -----------------------------
  # 6. Clamp k (safety net)
  # -----------------------------
  k_opt <- max(safe_min_k, min(as.integer(k_raw), safe_max_k))

  message(group_label, " | Final k =", k_opt)

  # -----------------------------
  # 6. Final clustering
  # -----------------------------
  final_km <- kmeans(pca_data, centers = k_opt, nstart = nstart)

  df$cluster <- factor(final_km$cluster)
  df$k_used  <- k_opt

  # -----------------------------
  # 7. Visualization
  # -----------------------------
  if (visualize) {
    tryCatch({
      cat("\n=== Visual diagnostics for", group_label, "===\n")

      if (!is.null(dev.list())) graphics.off()

      library(RColorBrewer)
      pal <- colorRampPalette(brewer.pal(12, "Paired"))(max(10, k_opt))

      p1 <- tryCatch({
        fviz_eig(pca_res, addlabels = TRUE, ncp = 15) +
          geom_vline(xintercept = n_pc, linetype = "dashed",
                     color = "red", linewidth = 1) +
          labs(title = paste("PCA Scree Plot -", group_label))
      }, error = function(e) {
        message(group_label, " | Scree plot failed: ", e$message)
        NULL
      })

      p2 <- tryCatch({
        fviz_pca_ind(
          pca_res,
          geom = "point",
          habillage = factor(final_km$cluster),
          palette = pal,
          addEllipses = TRUE,
          ellipse.type = "convex",
          label = "none",
          repel = TRUE
        ) +
          labs(title = paste("PCA Clusters -", group_label))
      }, error = function(e) {
        message(group_label, " | PCA cluster plot failed: ", e$message)
        NULL
      })

      p3 <- tryCatch({
        sil_obj <- cluster::silhouette(final_km$cluster, dist(pca_data))
        fviz_silhouette(sil_obj) +
          labs(title = paste("Silhouette (k =", k_opt, ") -", group_label))
      }, error = function(e) {
        message(group_label, " | Silhouette plot failed: ", e$message)
        NULL
      })

      if (!is.null(p1)) print(p1)
      if (!is.null(p2)) print(p2)
      if (!is.null(p3)) print(p3)

    }, error = function(e) {
      message(group_label, " | Visualization failed: ", e$message)
    })
  }
  return(df)
}

