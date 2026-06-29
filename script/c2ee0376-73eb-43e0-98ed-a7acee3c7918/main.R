library(tidyverse)
library(jsonlite)
library(Seurat)

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

unwrap_scalar <- function(x) {
	if (is.list(x) && !is.null(x$content)) return(unwrap_scalar(x$content))
	if (is.list(x) && length(x) == 1) return(unwrap_scalar(x[[1]]))
	if (length(x) == 0) return("")
	as.character(x[[1]])
}

to_int <- function(x, default_value) {
	value <- suppressWarnings(as.integer(unwrap_scalar(x)))
	if (is.na(value)) default_value else value
}

to_number <- function(x, default_value) {
	value <- suppressWarnings(as.numeric(unwrap_scalar(x)))
	if (is.na(value)) default_value else value
}

to_bool <- function(x, default_value = FALSE) {
	if (is.null(x) || length(x) == 0) return(default_value)
	value <- tolower(trimws(unwrap_scalar(x)))
	if (value %in% c("true", "1", "yes", "y", "on")) return(TRUE)
	if (value %in% c("false", "0", "no", "n", "off")) return(FALSE)
	default_value
}

to_choice <- function(x, choices, default_value) {
	value <- tolower(trimws(unwrap_scalar(x)))
	if (!(value %in% choices)) return(default_value)
	value
}

parse_char_list <- function(x) {
	text <- unwrap_scalar(x)
	if (is.na(text) || text == "") return(character(0))
	items <- unlist(strsplit(text, ",", fixed = TRUE), use.names = FALSE)
	items <- trimws(items)
	items <- items[items != ""]
	unique(items)
}

parse_dims <- function(x, fallback_npcs) {
	text <- unwrap_scalar(x)
	text <- trimws(text)
	if (is.na(text) || text == "") return(seq_len(fallback_npcs))

	if (grepl("^\\d+\\s*:\\s*\\d+$", text)) {
		parts <- unlist(strsplit(text, ":"), use.names = FALSE)
		start_v <- suppressWarnings(as.integer(trimws(parts[[1]])))
		end_v <- suppressWarnings(as.integer(trimws(parts[[2]])))
		if (!is.na(start_v) && !is.na(end_v) && start_v >= 1 && end_v >= start_v) {
			return(seq.int(start_v, end_v))
		}
	}

	vals <- suppressWarnings(as.integer(trimws(unlist(strsplit(text, ",", fixed = TRUE), use.names = FALSE))))
	vals <- vals[!is.na(vals) & vals >= 1]
	if (length(vals) == 0) return(seq_len(fallback_npcs))
	unique(vals)
}

format_vector_for_info <- function(x) {
	x <- as.character(x)
	x <- x[!is.na(x) & x != ""]
	if (length(x) == 0) return("none")
	paste(x, collapse = ", ")
}

params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)

seurat_path <- unwrap_scalar(params$seuratObject$path %||% "")
if (is.na(seurat_path) || seurat_path == "") {
	stop(sprintf("Missing input path for seuratObject: %s", seurat_path))
}
if (!file.exists(seurat_path)) {
	stop(sprintf("Seurat object file does not exist: %s", seurat_path))
}

output_dir <- unwrap_scalar(params$output_dir %||% params$tools_output_dir %||% "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

random_seed <- to_int(params$random_seed %||% 1234, 1234)
set.seed(random_seed)

normalize_method <- to_choice(params$normalize_method %||% "lognormalize", c("lognormalize", "rc"), "lognormalize")
scale_factor <- max(1, to_number(params$scale_factor %||% 10000, 10000))
selection_method <- to_choice(params$selection_method %||% "vst", c("vst", "mean.var.plot", "dispersion"), "vst")
nfeatures <- max(200L, to_int(params$nfeatures %||% 2000, 2000))
npcs <- max(5L, to_int(params$npcs %||% 30, 30))
resolution <- max(0, to_number(params$resolution %||% 0.5, 0.5))
cluster_algorithm <- to_int(params$cluster_algorithm %||% 1, 1)
run_tsne <- to_bool(params$run_tsne %||% TRUE, TRUE)
run_umap <- to_bool(params$run_umap %||% TRUE, TRUE)
umap_neighbors <- max(2L, to_int(params$umap_n_neighbors %||% 30, 30))
umap_min_dist <- max(0, to_number(params$umap_min_dist %||% 0.3, 0.3))
umap_metric <- to_choice(params$umap_metric %||% "cosine", c("cosine", "euclidean", "manhattan", "correlation"), "cosine")
tsne_perplexity <- max(2, to_number(params$tsne_perplexity %||% 30, 30))
vars_to_regress <- parse_char_list(params$vars_to_regress %||% "")
de_only_pos <- to_bool(params$de_only_pos %||% FALSE, FALSE)
de_min_pct <- max(0, to_number(params$de_min_pct %||% 0.25, 0.25))
de_logfc_threshold <- max(0, to_number(params$de_logfc_threshold %||% 0.25, 0.25))
de_test_use <- to_choice(params$de_test_use %||% "wilcox", c("wilcox", "bimod", "roc", "t", "negbinom", "poisson", "lr", "MAST", "DESeq2"), "wilcox")
de_mode <- to_choice(params$de_mode %||% "all_clusters", c("all_clusters", "target_cluster"), "all_clusters")
de_target_cluster <- trimws(unwrap_scalar(params$de_target_cluster %||% ""))

obj <- readRDS(seurat_path)
if (!inherits(obj, "Seurat")) {
	stop(sprintf("Input file is not a valid Seurat object: %s", seurat_path))
}

if (!("RNA" %in% names(obj@assays))) {
	warning(sprintf("RNA assay not found, using current default assay: %s", DefaultAssay(obj)))
} else {
	DefaultAssay(obj) <- "RNA"
}

obj <- NormalizeData(
	obj,
	normalization.method = if (normalize_method == "lognormalize") "LogNormalize" else "RC",
	scale.factor = scale_factor,
	verbose = FALSE
)
obj <- FindVariableFeatures(obj, selection.method = selection_method, nfeatures = nfeatures, verbose = FALSE)

var_plot <- VariableFeaturePlot(obj)
var_plot_file <- file.path(output_dir, "variable_features.png")
ggplot2::ggsave(var_plot_file, plot = var_plot, width = 8, height = 6, dpi = 300)

if (length(vars_to_regress) > 0) {
	present_vars <- vars_to_regress[vars_to_regress %in% colnames(obj@meta.data)]
	missing_vars <- setdiff(vars_to_regress, present_vars)
	if (length(missing_vars) > 0) {
		warning(sprintf("vars_to_regress not found in metadata and ignored: %s", paste(missing_vars, collapse = ", ")))
	}
	if (length(present_vars) > 0) {
		obj <- ScaleData(obj, vars.to.regress = present_vars, verbose = FALSE)
	} else {
		obj <- ScaleData(obj, verbose = FALSE)
	}
} else {
	obj <- ScaleData(obj, verbose = FALSE)
}

obj <- RunPCA(obj, npcs = npcs, verbose = FALSE)

pca_dimplot <- DimPlot(obj, reduction = "pca", group.by = "orig.ident")
pca_dimplot_file <- file.path(output_dir, "pca_dimplot.png")
ggplot2::ggsave(pca_dimplot_file, plot = pca_dimplot, width = 8, height = 6, dpi = 300)

pca_elbow <- ElbowPlot(obj, ndims = min(50, npcs))
pca_elbow_file <- file.path(output_dir, "pca_elbow.png")
ggplot2::ggsave(pca_elbow_file, plot = pca_elbow, width = 8, height = 6, dpi = 300)

available_pcs <- ncol(Embeddings(obj, reduction = "pca"))
neighbor_dims <- parse_dims(params$neighbor_dims %||% "", npcs)
neighbor_dims <- neighbor_dims[neighbor_dims <= available_pcs]
if (length(neighbor_dims) == 0) neighbor_dims <- seq_len(min(npcs, available_pcs))

obj <- FindNeighbors(obj, dims = neighbor_dims, verbose = FALSE)
obj <- FindClusters(obj, resolution = resolution, algorithm = cluster_algorithm, verbose = FALSE)

if (run_umap) {
	umap_dims <- parse_dims(params$umap_dims %||% "", max(neighbor_dims))
	umap_dims <- umap_dims[umap_dims <= available_pcs]
	if (length(umap_dims) == 0) umap_dims <- neighbor_dims

	obj <- RunUMAP(
		obj,
		dims = umap_dims,
		n.neighbors = umap_neighbors,
		min.dist = umap_min_dist,
		metric = umap_metric,
		verbose = FALSE
	)

	umap_plot <- DimPlot(obj, reduction = "umap", label = TRUE)
	umap_file <- file.path(output_dir, "umap_clusters.png")
	ggplot2::ggsave(umap_file, plot = umap_plot, width = 8, height = 6, dpi = 300)
} else {
	umap_file <- "none"
}

if (run_tsne) {
	tsne_dims <- parse_dims(params$tsne_dims %||% "", max(neighbor_dims))
	tsne_dims <- tsne_dims[tsne_dims <= available_pcs]
	if (length(tsne_dims) == 0) tsne_dims <- neighbor_dims

	max_perplexity <- max(2, floor((ncol(obj) - 1) / 3))
	if (tsne_perplexity > max_perplexity) {
		warning(sprintf("tsne_perplexity %.2f is too large for %d cells, using %d", tsne_perplexity, ncol(obj), max_perplexity))
		tsne_perplexity <- max_perplexity
	}

	obj <- RunTSNE(obj, dims = tsne_dims, perplexity = tsne_perplexity, verbose = FALSE)

	tsne_plot <- DimPlot(obj, reduction = "tsne", label = TRUE)
	tsne_file <- file.path(output_dir, "tsne_clusters.png")
	ggplot2::ggsave(tsne_file, plot = tsne_plot, width = 8, height = 6, dpi = 300)
} else {
	tsne_file <- "none"
}

if (!("seurat_clusters" %in% colnames(obj@meta.data))) {
	stop(sprintf("Cluster column not found after FindClusters: %s", "seurat_clusters"))
}
Idents(obj) <- "seurat_clusters"

if (de_mode == "all_clusters") {
	de_table <- FindAllMarkers(
		obj,
		only.pos = de_only_pos,
		min.pct = de_min_pct,
		logfc.threshold = de_logfc_threshold,
		test.use = de_test_use
	)
} else {
	if (de_target_cluster == "") {
		stop(sprintf("de_target_cluster must be provided when de_mode is target_cluster: %s", de_target_cluster))
	}
	if (!(de_target_cluster %in% levels(Idents(obj)))) {
		stop(sprintf("de_target_cluster is not a valid cluster id: %s", de_target_cluster))
	}
	de_table <- FindMarkers(
		obj,
		ident.1 = de_target_cluster,
		only.pos = de_only_pos,
		min.pct = de_min_pct,
		logfc.threshold = de_logfc_threshold,
		test.use = de_test_use
	)
	de_table$cluster <- de_target_cluster
	de_table$gene <- rownames(de_table)
	de_table <- de_table %>% dplyr::relocate(gene, cluster)
}

if (!("gene" %in% colnames(de_table))) {
	de_table$gene <- rownames(de_table)
	de_table <- de_table %>% dplyr::relocate(gene)
}

de_file <- file.path(output_dir, "differentially_expressed_features.tsv")
readr::write_tsv(de_table, de_file)

processed_rds <- file.path(output_dir, "processed_seurat.rds")
saveRDS(obj, processed_rds)

cluster_counts <- obj@meta.data %>%
	dplyr::count(seurat_clusters, name = "cell_count") %>%
	dplyr::arrange(seurat_clusters)
cluster_count_file <- file.path(output_dir, "cluster_cell_counts.tsv")
readr::write_tsv(cluster_counts, cluster_count_file)

info_lines <- c(
	"# Analysis Output",
	"",
	"## Run Info",
	sprintf("- run_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
	sprintf("- params_path: %s", "params.json"),
	sprintf("- input_seurat: %s", seurat_path),
	sprintf("- output_path: %s", output_dir),
	"",
	"## Params",
	sprintf("- normalize_method: %s", normalize_method),
	sprintf("- selection_method: %s", selection_method),
	sprintf("- nfeatures: %d", nfeatures),
	sprintf("- npcs: %d", npcs),
	sprintf("- neighbor_dims: %s", format_vector_for_info(neighbor_dims)),
	sprintf("- resolution: %.4g", resolution),
	sprintf("- cluster_algorithm: %d", cluster_algorithm),
	sprintf("- run_umap: %s", if (run_umap) "true" else "false"),
	sprintf("- run_tsne: %s", if (run_tsne) "true" else "false"),
	sprintf("- de_mode: %s", de_mode),
	sprintf("- de_test_use: %s", de_test_use),
	"",
	"## Stats",
	sprintf("- cell_count: %d", ncol(obj)),
	sprintf("- feature_count: %d", nrow(obj)),
	sprintf("- cluster_count: %d", length(levels(Idents(obj)))),
	sprintf("- de_feature_count: %d", nrow(de_table)),
	"",
	"## Outputs",
	sprintf("- variable_features_plot: %s", var_plot_file),
	sprintf("- pca_dimplot: %s", pca_dimplot_file),
	sprintf("- pca_elbow_plot: %s", pca_elbow_file),
	sprintf("- umap_plot: %s", umap_file),
	sprintf("- tsne_plot: %s", tsne_file),
	sprintf("- cluster_cell_counts: %s", cluster_count_file),
	sprintf("- processed_rds: %s", processed_rds),
	sprintf("- differential_features_tsv: %s", de_file)
)

readr::write_lines(info_lines, file.path(output_dir, "output.md"))

message(sprintf("Saved processed Seurat object: %s", processed_rds))
message(sprintf("Saved differential expression table: %s", de_file))
message(sprintf("Saved summary report: %s", file.path(output_dir, "output.md")))
