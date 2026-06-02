# ecoregionr <img src="man/figures/logo.png" align="right" height="120" alt="ecoregionr logo"/>

> **Environmental Bioregionalization and Clustering Toolkit for Africa**

<!-- badges -->
[![R package](https://img.shields.io/badge/R%20package-v0.3.0-blue)](https://github.com/Medjumanne/ecoregionr)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

---

## Overview

`ecoregionr` is an R package for data-driven **bioregionalization** — the process
of dividing a geographic area into ecologically coherent zones using measurable
environmental signals rather than political boundaries.

The package was developed for the **Vector Grid Africa** programme to support
vector-borne disease surveillance planning, but is fully generic and can be
applied to any country or region where administrative polygon shapefiles and
raster covariates are available.

### Two clustering strategies

| Strategy | Spatial constraint | Best for |
|---|---|---|
| **PCA + k-means** | None — maximises environmental homogeneity | Scientific hypothesis generation; multi-country comparisons |
| **SKATER** | Hard — every cluster is a contiguous region | Operational planning; field surveillance zone design |

Both methods share the same data preparation pipeline (raster extraction,
covariate table, imputation) and produce the same outputs (maps, heatmaps,
text descriptions).

---

## Installation

```r
# Install the development version from GitHub
# install.packages("devtools")
devtools::install_github("Medjumanne/ecoregionr")
```

### System requirements

- R ≥ 4.2
- GDAL / GEOS / PROJ (for `sf` and `terra`) — install via
  [OSGeo4W](https://trac.osgeo.org/osgeo4w/) (Windows),
  `brew install gdal` (macOS), or `sudo apt-get install libgdal-dev` (Linux).
- `spdep` ≥ 1.2 — required for SKATER clustering.

---

## Quick start

```r
library(ecoregionr)

# ── 1. Extract raster statistics per polygon ───────────────────────────────────
adm2_stats <- extract_continuous_stats(
  polygons = my_sf,
  rasters  = list(elev = terra::rast("elevation.tif"),
                  pop  = terra::rast("population.tif")),
  funs     = list(elev = list(median = median),
                  pop  = list(mean   = mean)),
  id_col   = "ID"
)

# ── 2. Build the covariate table ──────────────────────────────────────────────
env_df <- build_cluster_table(
  polygons   = adm2_stats,
  cat_tables = list(kg_wide, lc_wide),
  keep_cols  = c(".group", "elev_median", "pop_mean"),
  id_col     = "ID"
)

# ── Method A: unconstrained PCA + k-means ─────────────────────────────────────
clusters <- run_clustering(
  df        = env_df,
  group_col = ".group",
  id_col    = "ID",
  min_k = 6, max_k = 15
)

# ── Method B: spatially constrained SKATER ────────────────────────────────────
# First, inspect the elbow plot to choose k
plot_skater_elbow(adm2_sf, vars = my_vars, max_k = 20)

adm2_sf <- run_skater_clustering(
  polygons_sf  = adm2_sf,
  vars         = my_vars,
  n_clusters   = 12,
  elev_col     = "elev_median",
  elev_weight  = 7
)

# ── Visualise ─────────────────────────────────────────────────────────────────
make_skater_heatmap(adm2_sf)
build_leaflet_map(adm2_sf, cluster_col = "skater_cluster")
```

---

## Full workflow

A complete, annotated dual-method workflow (data preparation → both clustering
methods → comparison maps) is provided in `ecoregionr_dual_workflow.Rmd`.
Open it in RStudio and knit to HTML.

---

## Package functions

### Data extraction
| Function | Description |
|---|---|
| `extract_continuous_stats()` | Summarise continuous rasters (elevation, population, …) per polygon |
| `extract_categorical_composition()` | Compute proportional composition from categorical rasters (climate, land cover) |

### Table building & imputation
| Function | Description |
|---|---|
| `build_cluster_table()` | Join continuous stats + categorical proportions into a wide covariate table |
| `impute_medians()` | Replace missing values with within-group medians |

### Unconstrained clustering (PCA + k-means)
| Function | Description |
|---|---|
| `cluster_pca_kmeans()` | PCA + NbClust consensus k-means for one group |
| `run_clustering()` | Apply `cluster_pca_kmeans()` across groups, with optional elevation-band stratification |

### Spatially constrained clustering (SKATER)
| Function | Description |
|---|---|
| `run_skater_clustering()` | Full SKATER pipeline: PCA → adjacency graph → MST → pruning |
| `plot_skater_elbow()` | SSE elbow curve to guide k selection |
| `make_skater_heatmap()` | Z-score heatmap of original variables by SKATER cluster |
| `plot_skater_pca_loadings()` | PCA variable loading biplot |

### Visualisation
| Function | Description |
|---|---|
| `make_cluster_heatmap()` | Z-score heatmap for k-means clusters |
| `build_leaflet_map()` | Interactive Leaflet map with custom popups |
| `make_static_map()` | Publication-quality ggplot map |

### Summarisation & reporting
| Function | Description |
|---|---|
| `summarize_clusters_compact()` | Compact summary table of each cluster's dominant traits |
| `generate_cluster_paragraphs()` | Auto-generate natural-language bioregion descriptions |
| `run_ecoregion_workflow()` | One-call wrapper for the full k-means pipeline |

### Utilities
| Function | Description |
|---|---|
| `ensure_dir()` | Create output directory if it does not exist |

---

## Data requirements

| Input | Format | Notes |
|---|---|---|
| Polygon layer | `.shp` / any `sf`-readable format | District / county / grid cells |
| Continuous rasters | GeoTIFF (`.tif`) | Elevation, population, urban extent, transport, species richness, … |
| Categorical rasters | GeoTIFF — integer class codes | Köppen-Geiger climate, ESRI land cover |
| Optional CSV | `.csv` | Any additional tabular covariates joined by the ID column |

Built-in lookup tables (`kg_lookup`, `lc_lookup`) map integer raster codes to
human-readable class names for the default Köppen-Geiger and ESRI land-cover
products. Access via `ecoregionr:::kg_lookup`.

---

## Reproducibility

All randomised steps (k-means `nstart`, SKATER MST) accept a `seed` argument
(default `42`). Setting the same seed guarantees identical outputs across runs.

A complete session snapshot can be captured with:

```r
sessionInfo()
# or
renv::snapshot()   # if using renv for dependency management
```

---

## Citation

If you use `ecoregionr` in published research, please cite:

```
Omari, M. J. (2025). ecoregionr: Environmental Bioregionalization and
Clustering Toolkit for Africa. R package version 0.3.0.
https://github.com/Medjumanne/ecoregionr
```

For the SKATER algorithm itself:

> Assunção, R. M., Neves, M. C., Câmara, G., & Freitas, C. D. C. (2006).
> Efficient regionalisation techniques for socio-economic geographical units
> using minimum spanning trees. *International Journal of Geographical
> Information Science*, 20(7), 797–811.
> <https://doi.org/10.1080/13658810600665111>

---

## Contributing

Contributions are welcome! Please read [CONTRIBUTING.md](CONTRIBUTING.md) before
opening a pull request. Bug reports and feature requests go to the
[issue tracker](https://github.com/Medjumanne/ecoregionr/issues).

---

## License

MIT © 2025 Mohamed Jumanne Omari. See [LICENSE](LICENSE) for full terms.

---

## Acknowledgements

Developed at the **Ifakara Health Institute (IHI)** as part of the
**Vector Grid Africa** programme. The programme maps vector-borne disease
risk across sub-Saharan Africa to support targeted surveillance and
intervention planning.
