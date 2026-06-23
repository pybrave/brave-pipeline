library(Seurat)
library(jsonlite)

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

normalize_scalar <- function(x) {
	if (is.list(x) && !is.null(x$content)) return(as.character(x$content))
	as.character(x)
}

parse_gene_list <- function(gene_text) {
	if (is.null(gene_text) || length(gene_text) == 0) return(character(0))
	gene_text <- normalize_scalar(gene_text)
	if (is.na(gene_text) || gene_text == "") return(character(0))

	genes <- unlist(strsplit(gene_text, ",", fixed = TRUE), use.names = FALSE)
	genes <- trimws(genes)
	genes <- genes[genes != ""]
	unique(genes)
}

safe_file_stem <- function(x) {
	x <- gsub("[^A-Za-z0-9._-]", "_", x)
	x <- gsub("_+", "_", x)
	x <- gsub("^_|_$", "", x)
	if (identical(x, "")) "feature" else x
}

params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)

seurat_path <- params$seuratObject$path %||% ""
seurat_path <- normalize_scalar(seurat_path)
if (is.na(seurat_path) || seurat_path == "") {
	stop("Missing seuratObject.path in params.json")
}
if (!file.exists(seurat_path)) {
	stop(sprintf("Seurat object file does not exist: %s", seurat_path))
}

output_dir <- params$output_dir %||% params$tools_output_dir %||% "output"
output_dir <- normalize_scalar(output_dir)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

obj <- readRDS(seurat_path)
if (!inherits(obj, "Seurat")) {
	stop("Input file is not a valid Seurat object")
}

if (!("umap" %in% names(obj@reductions))) {
	stop("UMAP reduction not found in Seurat object")
}

umap_plot <- DimPlot(obj, reduction = "umap")
umap_file <- file.path(output_dir, "umap.png")
ggplot2::ggsave(umap_file, plot = umap_plot, width = 8, height = 6, dpi = 300)

gene_text <- params$seuratObject$feature_genes %||% params$genes %||% ""
genes <- parse_gene_list(gene_text)
present_genes <- genes[genes %in% rownames(obj)]
missing_genes <- setdiff(genes, present_genes)

feature_files <- character(0)
if (length(present_genes) > 0) {
	for (gene in present_genes) {
		p <- FeaturePlot(obj, features = gene, reduction = "umap")
		out_name <- paste0("featureplot_", safe_file_stem(gene), ".png")
		out_path <- file.path(output_dir, out_name)
		ggplot2::ggsave(out_path, plot = p, width = 8, height = 6, dpi = 300)
		feature_files <- c(feature_files, out_path)
	}
}

info_lines <- c(
	"# Analysis Output",
	"",
	"## Inputs",
	sprintf("- seurat_path: %s", seurat_path),
	"",
	"## Outputs",
	sprintf("- umap_plot: %s", umap_file),
	sprintf("- featureplot_count: %d", length(feature_files)),
	sprintf("- featureplot_files: %s", if (length(feature_files) == 0) "none" else paste(feature_files, collapse = ", ")),
	sprintf("- missing_genes: %s", if (length(missing_genes) == 0) "none" else paste(missing_genes, collapse = ", "))
)

writeLines(info_lines, file.path(output_dir, "output.md"))

message(sprintf("UMAP plot saved: %s", umap_file))
if (length(feature_files) > 0) {
	message(sprintf("FeaturePlot files saved: %d", length(feature_files)))
}
if (length(missing_genes) > 0) {
	message(sprintf("Missing genes skipped: %s", paste(missing_genes, collapse = ", ")))
}

