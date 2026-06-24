library(Seurat)
library(jsonlite)

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

to_bool <- function(x, default_value = FALSE) {
	if (is.null(x) || length(x) == 0) return(default_value)
	value <- tolower(trimws(unwrap_scalar(x)))
	if (value %in% c("true", "1", "yes", "y", "on")) return(TRUE)
	if (value %in% c("false", "0", "no", "n", "off")) return(FALSE)
	default_value
}

with_future_plan <- function(expr, use_sequential = FALSE) {
	if (!use_sequential) return(eval.parent(substitute(expr)))
	if (!requireNamespace("future", quietly = TRUE)) return(eval.parent(substitute(expr)))

	old_plan <- future::plan()
	on.exit(future::plan(old_plan), add = TRUE)
	future::plan(future::sequential)
	eval.parent(substitute(expr))
}

parse_gene_list <- function(gene_text) {
	if (is.null(gene_text) || length(gene_text) == 0) return(character(0))
	gene_text <- unwrap_scalar(gene_text)
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

extract_paths <- function(x) {
	out <- character(0)
	if (is.null(x) || length(x) == 0) return(out)

	if (is.list(x) && !is.null(x$path)) {
		p <- unwrap_scalar(x$path)
		if (!is.na(p) && nzchar(p)) out <- c(out, p)
	}

	if (is.list(x)) {
		for (item in x) out <- c(out, extract_paths(item))
	}

	unique(out)
}

params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)

future_globals_maxsize_gb <- to_int(params$future_globals_maxsize_gb %||% 0, 0)
if (future_globals_maxsize_gb > 0) {
	options(future.globals.maxSize = future_globals_maxsize_gb * 1024^3)
} else {
	options(future.globals.maxSize = +Inf)
}

integration_force_sequential <- to_bool(params$integration_force_sequential %||% TRUE, TRUE)

output_dir <- unwrap_scalar(params$output_dir %||% params$tools_output_dir %||% "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

input_paths <- extract_paths(params$seuratObject)
if (length(input_paths) == 0) input_paths <- extract_paths(params$seuratObjects)
if (length(input_paths) == 0) stop("No Seurat input files found in params.json (seuratObject/seuratObjects)")

missing_paths <- input_paths[!file.exists(input_paths)]
if (length(missing_paths) > 0) {
	stop(sprintf("Input file does not exist: %s", paste(missing_paths, collapse = ", ")))
}

integration_method <- toupper(unwrap_scalar(params$integration_method %||% "SCT"))
if (!(integration_method %in% c("SCT", "LOGNORMALIZE"))) integration_method <- "SCT"

run_feature_plot <- to_bool(params$run_feature_plot %||% TRUE, TRUE)
project_name <- unwrap_scalar(params$project_name %||% "SeuratIntegration")
nfeatures <- max(200L, to_int(params$nfeatures %||% 3000, 3000))
npcs <- max(5L, to_int(params$npcs %||% 30, 30))
k_anchor <- max(1L, to_int(params$k_anchor %||% 5, 5))
feature_genes <- parse_gene_list(params$feature_genes %||% "")

objects <- lapply(input_paths, function(path) {
	obj <- readRDS(path)
	if (!inherits(obj, "Seurat")) {
		stop(sprintf("Input is not a Seurat object: %s", path))
	}
	obj
})

sample_names <- tools::file_path_sans_ext(basename(input_paths))
sample_names <- make.unique(sample_names)

for (i in seq_along(objects)) {
	objects[[i]]$orig.ident <- sample_names[[i]]
	objects[[i]]@project.name <- project_name
}

if (length(objects) == 1) {
	integrated <- objects[[1]]
	if (integration_method == "SCT") {
		integrated <- SCTransform(integrated, verbose = FALSE)
	} else {
		integrated <- NormalizeData(integrated, verbose = FALSE)
		integrated <- FindVariableFeatures(integrated, selection.method = "vst", nfeatures = nfeatures, verbose = FALSE)
		integrated <- ScaleData(integrated, verbose = FALSE)
	}
	integrated <- RunPCA(integrated, npcs = npcs, verbose = FALSE)
} else if (integration_method == "SCT") {
	objects <- lapply(objects, function(obj) SCTransform(obj, verbose = FALSE))
	features <- SelectIntegrationFeatures(object.list = objects, nfeatures = nfeatures)
	objects <- with_future_plan(
		PrepSCTIntegration(object.list = objects, anchor.features = features, verbose = FALSE),
		use_sequential = integration_force_sequential
	)
	anchors <- with_future_plan(
		FindIntegrationAnchors(
			object.list = objects,
			normalization.method = "SCT",
			anchor.features = features,
			dims = 1:npcs,
			k.anchor = k_anchor
		),
		use_sequential = integration_force_sequential
	)
	integrated <- with_future_plan(
		IntegrateData(
			anchorset = anchors,
			normalization.method = "SCT",
			dims = 1:npcs
		),
		use_sequential = integration_force_sequential
	)
	DefaultAssay(integrated) <- "integrated"
	integrated <- RunPCA(integrated, npcs = npcs, verbose = FALSE)
} else {
	objects <- lapply(objects, function(obj) {
		obj <- NormalizeData(obj, verbose = FALSE)
		FindVariableFeatures(obj, selection.method = "vst", nfeatures = nfeatures, verbose = FALSE)
	})
	features <- SelectIntegrationFeatures(object.list = objects, nfeatures = nfeatures)
	anchors <- FindIntegrationAnchors(
		object.list = objects,
		anchor.features = features,
		dims = 1:npcs,
		k.anchor = k_anchor
	)
	integrated <- IntegrateData(anchorset = anchors, dims = 1:npcs)
	DefaultAssay(integrated) <- "integrated"
	integrated <- ScaleData(integrated, verbose = FALSE)
	integrated <- RunPCA(integrated, npcs = npcs, verbose = FALSE)
}

integrated <- RunUMAP(integrated, dims = 1:npcs, reduction = "pca", verbose = FALSE)

integrated_file <- file.path(output_dir, "integrated_seurat.rds")
saveRDS(integrated, integrated_file)

umap_plot <- DimPlot(integrated, reduction = "umap", group.by = "orig.ident")
umap_file <- file.path(output_dir, "umap.png")
ggplot2::ggsave(umap_file, plot = umap_plot, width = 8, height = 6, dpi = 300)

feature_files <- character(0)
missing_genes <- character(0)

if (run_feature_plot && length(feature_genes) > 0) {
	if ("RNA" %in% names(integrated@assays)) {
		DefaultAssay(integrated) <- "RNA"
	}
	present_genes <- feature_genes[feature_genes %in% rownames(integrated)]
	missing_genes <- setdiff(feature_genes, present_genes)

	if (length(present_genes) > 0) {
		for (gene in present_genes) {
			p <- FeaturePlot(integrated, features = gene, reduction = "umap")
			out_name <- paste0("featureplot_", safe_file_stem(gene), ".png")
			out_path <- file.path(output_dir, out_name)
			ggplot2::ggsave(out_path, plot = p, width = 8, height = 6, dpi = 300)
			feature_files <- c(feature_files, out_path)
		}
	}
}

summary_lines <- c(
	"# Single-cell Integration Output",
	"",
	"## Inputs",
	sprintf("- input_count: %d", length(input_paths)),
	sprintf("- input_files: %s", paste(input_paths, collapse = ", ")),
	"",
	"## Parameters",
	sprintf("- integration_method: %s", integration_method),
	sprintf("- nfeatures: %d", nfeatures),
	sprintf("- npcs: %d", npcs),
	sprintf("- k_anchor: %d", k_anchor),
	sprintf("- run_feature_plot: %s", if (run_feature_plot) "true" else "false"),
	sprintf("- feature_genes: %s", if (length(feature_genes) == 0) "none" else paste(feature_genes, collapse = ", ")),
	"",
	"## Outputs",
	sprintf("- integrated_object: %s", integrated_file),
	sprintf("- umap_plot: %s", umap_file),
	sprintf("- featureplot_count: %d", length(feature_files)),
	sprintf("- featureplot_files: %s", if (length(feature_files) == 0) "none" else paste(feature_files, collapse = ", ")),
	sprintf("- missing_genes: %s", if (length(missing_genes) == 0) "none" else paste(missing_genes, collapse = ", "))
)

writeLines(summary_lines, file.path(output_dir, "output.md"))

message(sprintf("Saved integrated object: %s", integrated_file))
message(sprintf("Saved UMAP plot: %s", umap_file))
if (length(feature_files) > 0) {
	message(sprintf("Saved FeaturePlot files: %d", length(feature_files)))
}
if (length(missing_genes) > 0) {
	message(sprintf("Missing genes skipped: %s", paste(missing_genes, collapse = ", ")))
}


