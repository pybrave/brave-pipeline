library(Seurat)
library(jsonlite)
library(readr)

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

format_value <- function(x) {
	if (is.logical(x)) return(if (x) "true" else "false")
	as.character(x)
}

params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)

seurat_path <- unwrap_scalar(params$seuratObject$path %||% "")
if (is.na(seurat_path) || seurat_path == "") {
	stop("Missing input path for seuratObject")
}
if (!file.exists(seurat_path)) {
	stop(sprintf("Seurat object file does not exist: %s", seurat_path))
}

output_dir <- unwrap_scalar(params$output_dir %||% params$tools_output_dir %||% "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

mito_pattern <- unwrap_scalar(params$mito_pattern %||% "^MT-")
if (is.na(mito_pattern) || mito_pattern == "") mito_pattern <- "^MT-"

min_features <- max(0L, to_int(params$min_features %||% 200, 200))
max_features <- to_int(params$max_features %||% 2500, 2500)
max_percent_mt <- max(0, to_number(params$max_percent_mt %||% 5, 5))
use_count_filter <- to_bool(params$use_count_filter %||% FALSE, FALSE)
min_counts <- max(0L, to_int(params$min_counts %||% 0, 0))
max_counts <- max(0L, to_int(params$max_counts %||% 0, 0))

obj <- readRDS(seurat_path)
if (!inherits(obj, "Seurat")) {
	stop(sprintf("Input file is not a valid Seurat object: %s", seurat_path))
}

if (!("RNA" %in% names(obj@assays))) {
	stop("RNA assay not found in Seurat object")
}
DefaultAssay(obj) <- "RNA"
Idents(obj)
levels(Idents(obj))
colnames(obj@meta.data)
Idents(obj) <- "orig.ident"


# PBMC3K tutorial style QC metrics
obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern = mito_pattern)

vln_before <- VlnPlot(obj, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3, pt.size = 0.1)




ggplot2::ggsave(
	filename = file.path(output_dir, "qc_violin_before.png"),
	plot = vln_before,
	width = 12,
	height = 5,
	dpi = 300
)

scatter_count_feature_before <- FeatureScatter(obj, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
ggplot2::ggsave(
	filename = file.path(output_dir, "qc_scatter_ncount_nfeature_before.png"),
	plot = scatter_count_feature_before,
	width = 7,
	height = 6,
	dpi = 300
)

scatter_count_mt_before <- FeatureScatter(obj, feature1 = "nCount_RNA", feature2 = "percent.mt")
ggplot2::ggsave(
	filename = file.path(output_dir, "qc_scatter_ncount_mt_before.png"),
	plot = scatter_count_mt_before,
	width = 7,
	height = 6,
	dpi = 300
)

md <- obj@meta.data
keep_cells <- md$nFeature_RNA >= min_features &
	md$nFeature_RNA <= max_features &
	md$percent.mt <= max_percent_mt

if (use_count_filter) {
	keep_cells <- keep_cells & md$nCount_RNA >= min_counts
	if (max_counts > 0) {
		keep_cells <- keep_cells & md$nCount_RNA <= max_counts
	}
}

cells_before <- ncol(obj)
obj_qc <- subset(obj, cells = rownames(md)[keep_cells])
cells_after <- ncol(obj_qc)

if (cells_after == 0) {
	stop("No cells remain after QC filtering. Please relax thresholds.")
}

vln_after <- VlnPlot(obj_qc, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3, pt.size = 0.1)
ggplot2::ggsave(
	filename = file.path(output_dir, "qc_violin_after.png"),
	plot = vln_after,
	width = 12,
	height = 5,
	dpi = 300
)

scatter_count_feature_after <- FeatureScatter(obj_qc, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
ggplot2::ggsave(
	filename = file.path(output_dir, "qc_scatter_ncount_nfeature_after.png"),
	plot = scatter_count_feature_after,
	width = 7,
	height = 6,
	dpi = 300
)

scatter_count_mt_after <- FeatureScatter(obj_qc, feature1 = "nCount_RNA", feature2 = "percent.mt")
ggplot2::ggsave(
	filename = file.path(output_dir, "qc_scatter_ncount_mt_after.png"),
	plot = scatter_count_mt_after,
	width = 7,
	height = 6,
	dpi = 300
)

qc_metrics <- data.frame(
	metric = c(
		"cells_before",
		"cells_after",
		"cells_removed",
		"retained_fraction",
		"median_nFeature_before",
		"median_nFeature_after",
		"median_nCount_before",
		"median_nCount_after",
		"median_percent_mt_before",
		"median_percent_mt_after"
	),
	value = c(
		cells_before,
		cells_after,
		cells_before - cells_after,
		round(cells_after / cells_before, 6),
		median(obj$nFeature_RNA),
		median(obj_qc$nFeature_RNA),
		median(obj$nCount_RNA),
		median(obj_qc$nCount_RNA),
		median(obj$percent.mt),
		median(obj_qc$percent.mt)
	)
)

metrics_file <- file.path(output_dir, "qc_metrics.tsv")
readr::write_tsv(qc_metrics, metrics_file)

qc_rds <- file.path(output_dir, "qc_seurat.rds")
saveRDS(obj_qc, qc_rds)

summary_lines <- c(
	"# Seurat QC Output",
	"",
	"## Input",
	sprintf("- seurat_input: %s", seurat_path),
	"",
	"## Parameters",
	sprintf("- mito_pattern: %s", mito_pattern),
	sprintf("- min_features: %s", format_value(min_features)),
	sprintf("- max_features: %s", format_value(max_features)),
	sprintf("- max_percent_mt: %s", format_value(max_percent_mt)),
	sprintf("- use_count_filter: %s", format_value(use_count_filter)),
	sprintf("- min_counts: %s", format_value(min_counts)),
	sprintf("- max_counts: %s", format_value(max_counts)),
	"",
	"## QC Summary",
	sprintf("- cells_before: %d", cells_before),
	sprintf("- cells_after: %d", cells_after),
	sprintf("- cells_removed: %d", cells_before - cells_after),
	sprintf("- retained_fraction: %.6f", cells_after / cells_before),
	"",
	"## Outputs",
	sprintf("- qc_rds: %s", qc_rds),
	sprintf("- qc_metrics: %s", metrics_file),
	sprintf("- qc_violin_before: %s", file.path(output_dir, "qc_violin_before.png")),
	sprintf("- qc_violin_after: %s", file.path(output_dir, "qc_violin_after.png")),
	sprintf("- qc_scatter_ncount_nfeature_before: %s", file.path(output_dir, "qc_scatter_ncount_nfeature_before.png")),
	sprintf("- qc_scatter_ncount_nfeature_after: %s", file.path(output_dir, "qc_scatter_ncount_nfeature_after.png")),
	sprintf("- qc_scatter_ncount_mt_before: %s", file.path(output_dir, "qc_scatter_ncount_mt_before.png")),
	sprintf("- qc_scatter_ncount_mt_after: %s", file.path(output_dir, "qc_scatter_ncount_mt_after.png"))
)

readr::write_lines(summary_lines, file.path(output_dir, "output.md"))

message(sprintf("Saved QC Seurat object: %s", qc_rds))
message(sprintf("Saved QC metrics: %s", metrics_file))
message(sprintf("Saved summary report: %s", file.path(output_dir, "output.md")))
