library(tidyverse)
library(jsonlite)

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

get_param_path <- function(x) {
	if (is.list(x) && !is.null(x$content)) return(as.character(x$content))
	as.character(x)
}

format_vector_for_info <- function(x) {
	x <- as.character(x)
	x <- x[!is.na(x) & x != ""]
	if (length(x) == 0) return("none")
	paste(x, collapse = ", ")
}

read_eigenval <- function(path) {
	if (!file.exists(path)) {
		stop(sprintf("eigenval file not found: %s", path))
	}
	values <- readr::read_lines(path, progress = FALSE)
	values <- suppressWarnings(as.numeric(values))
	values <- values[!is.na(values)]
	if (length(values) == 0) {
		stop(sprintf("no numeric values found in eigenval file: %s", path))
	}
	values
}

read_eigenvec <- function(path) {
	if (!file.exists(path)) {
		stop(sprintf("eigenvec file not found: %s", path))
	}
	data <- readr::read_table(path, col_names = FALSE, show_col_types = FALSE, progress = FALSE)
	if (ncol(data) < 4) {
		stop(sprintf("eigenvec file has too few columns: %s", path))
	}
	data
}

params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)

output_dir <- params$output_dir %||% "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

eigenval_path <- get_param_path(params$eigenval)
eigenvec_path <- get_param_path(params$eigenvec)

if (is.na(eigenval_path) || eigenval_path == "") {
	stop("missing eigenval path in params.json")
}
if (is.na(eigenvec_path) || eigenvec_path == "") {
	stop("missing eigenvec path in params.json")
}

eigenval <- read_eigenval(eigenval_path)
eigenvec <- read_eigenvec(eigenvec_path)

sample_count <- nrow(eigenvec)
pc_count <- ncol(eigenvec) - 2
pc_names <- paste0("PC", seq_len(pc_count))
colnames(eigenvec) <- c("FID", "IID", pc_names)

plot_df <- eigenvec %>%
	transmute(
		FID = .data$FID,
		IID = .data$IID,
		PC1 = .data$PC1,
		PC2 = .data$PC2
	)

variance_pct <- eigenval / sum(eigenval) * 100
pc1_label <- sprintf("PC1 (%.2f%%)", variance_pct[1])
pc2_label <- sprintf("PC2 (%.2f%%)", variance_pct[2])

p <- ggplot(plot_df, aes(x = PC1, y = PC2)) +
	geom_point(color = "#2C7FB8", size = 2.8, alpha = 0.85) +
	geom_hline(yintercept = 0, linewidth = 0.3, color = "grey80") +
	geom_vline(xintercept = 0, linewidth = 0.3, color = "grey80") +
	labs(
		title = sprintf("PCA Plot: %s", params$analysis_name %||% "analysis"),
		x = pc1_label,
		y = pc2_label
	) +
	theme_minimal(base_size = 13) +
	theme(
		plot.title = element_text(hjust = 0.5, face = "bold"),
		panel.grid.minor = element_blank()
	)

plot_file <- file.path(output_dir, "pca_scatter.png")
ggsave(plot_file, plot = p, width = 8, height = 6, dpi = 300)

scores_file <- file.path(output_dir, "pca_scores.tsv")
readr::write_tsv(plot_df, scores_file)

info_lines <- c(
	"# Analysis Output",
	"",
	"## Run Info",
	sprintf("- run_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
	sprintf("- params_path: %s", "params.json"),
	sprintf("- output_dir: %s", output_dir),
	"",
	"## Inputs",
	sprintf("- eigenval: %s", eigenval_path),
	sprintf("- eigenvec: %s", eigenvec_path),
	"",
	"## Stats",
	sprintf("- sample_count: %d", sample_count),
	sprintf("- pc_count: %d", pc_count),
	sprintf("- eigenval_count: %d", length(eigenval)),
	sprintf("- variance_pc1: %.4f", variance_pct[1]),
	sprintf("- variance_pc2: %.4f", variance_pct[2]),
	sprintf("- plot_file: %s", plot_file),
	sprintf("- scores_file: %s", scores_file)
)

readr::write_lines(info_lines, file.path(output_dir, "output.md"))

message(sprintf("PCA plot saved to: %s", plot_file))
