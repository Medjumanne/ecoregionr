# =============================================================================
# 5. cluster_pca_kmeans  (single-group)
# =============================================================================
#' PCA + k-means clustering for one group (e.g. one country).
#'
#' Logic is a direct generic port of the original cluster_country_pca_kmeans().
#' Only the hardcoded column names ("ID", country column) are replaced by
#' parameters; everything else — NbClust call, vote counting, min/max k
#' enforcement, silhouette fallback, visualisations — is identical.

#' @param df          Tibble / data.frame for one group.
#' @param group_col   Name of the grouping column (e.g. "ADM0_NAME").
#'                    Used for status messages and excluded from clustering.
#' @param id_col      Name of the unique ID column (excluded from clustering).
#' @param min_k       Minimum number of clusters to allow (default 6, matching
#'                    the original NbClust min.nc = 6).
#' @param max_k       Maximum number of clusters to test (default 15).
#' @param pca_var     Cumulative variance threshold for PC retention (default 0.90).
#' @param nstart      k-means nstart for the final run (default 50).
#' @param visualize   Logical – print consensus dot-plot, scree, PCA, and
#'                    silhouette plots (default TRUE).
#' @param seed        Random seed (default 1).
#'
#' @return The input data frame with extra columns: `cluster` (factor),
#'         `k_used` (integer), and PC score columns (PC1, PC2, …).
#'@export
cluster_pca_kmeans <- function(df,
                               group_col   = NULL,
                               id_col      = "ID",
                               min_k       = 16,
                               max_k       = 16,
                               pca_var     = 0.90,
                               nstart      = 50,
                               strict_mode = FALSE,
                               visualize   = TRUE,
                               seed        = 1) {
  set.seed(seed)

  group_label <- if (!is.null(group_col)) unique(df[[group_col]])[1] else "group"

  # -----------------------------
  # STRICT MODE OVERRIDES
  # -----------------------------
  if (strict_mode) {
    min_k   <- 6
    max_k   <- 15
    pca_var <- 0.90
    nstart  <- 50
  }

  # -----------------------------
  # 1. Prepare data
  # -----------------------------
  exclude_cols <- unique(c(id_col, if (!is.null(group_col)) group_col))
  X <- df[, !names(df) %in% exclude_cols, drop = FALSE]

  sds <- apply(X, 2, sd, na.rm = TRUE)
  sds[is.na(sds)] <- 0
  removed_vars <- names(X)[sds == 0]
  X <- X[, sds > 0, drop = FALSE]

  if (length(removed_vars) > 0) {
    message(group_label, " | Removed ", length(removed_vars),
            " zero-variance variables")
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

  # -----------------------------
  # 2. Scale
  # -----------------------------
  X_scaled <- scale(X)

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
  # 4. NbClust
  # -----------------------------
  message(group_label, " | Running NbClust...")

  nb_res <- tryCatch(
    NbClust(pca_data,
            min.nc = min_k,
            max.nc = max_k,
            method = "kmeans",
            index = "all"),
    error = function(e) NULL
  )

  if (!is.null(nb_res)) {

    vote_data <- as.integer(nb_res$Best.nc[1, ])
    vote_data <- vote_data[is.finite(vote_data)]
    vote_data <- vote_data[vote_data >= min_k & vote_data <= max_k]

    k_raw <- as.integer(names(which.max(table(vote_data))))

  } else {

    sil <- sapply(seq(min_k, max_k), function(k) {
      km <- kmeans(pca_data, centers = k, nstart = 25)
      mean(silhouette(km$cluster, dist(pca_data))[, 3], na.rm = TRUE)
    })

    # -----------------------------
    # SILHOUETTE RULE (MODE SWITCH)
    # -----------------------------
    if (strict_mode) {
      k_raw <- which.max(sil) + 1
    } else {
      k_raw <- which.max(sil) + min_k - 1
    }

    message(group_label, " | NbClust failed, silhouette k =", k_raw)
  }

  # -----------------------------
  # 5. Clamp k
  # -----------------------------
  k_opt <- max(min_k, min(k_raw, max_k))

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

    cat("\n=== Visual diagnostics for", group_label, "===\n")

    print(
      fviz_eig(pca_res, addlabels = TRUE, ncp = 15) +
        geom_vline(xintercept = n_pc, linetype = "dashed",
                   color = "red", linewidth = 1) +
        labs(title = paste("PCA Scree Plot -", group_label))
    )

    library(RColorBrewer)
    pal <- colorRampPalette(brewer.pal(12, "Paired"))(max(10, k_opt))

    print(
      fviz_pca_ind(
        pca_res,
        geom = "point",
        habillage = final_km$cluster,
        palette = pal,
        addEllipses = TRUE,
        ellipse.type = "convex",
        title = paste("PCA Clusters -", group_label)
      )
    )

    sil_obj <- silhouette(final_km$cluster, dist(pca_data))
    print(
      fviz_silhouette(sil_obj) +
        labs(title = paste("Silhouette (k =", k_opt, ") -", group_label))
    )
  }

  return(df)
}
