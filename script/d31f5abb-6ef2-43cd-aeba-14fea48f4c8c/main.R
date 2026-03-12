library(tidyverse)
library(jsonlite)
library(pheatmap)

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0) y else x
}

normalize_color <- function(node, fallback = NULL) {
	if (is.null(node) || length(node) == 0) return(fallback)

	if (is.character(node)) {
		value <- node[[1]]
		if (is.na(value) || value == "") return(fallback)
		return(value)
	}

	if (is.list(node)) {
		if (!is.null(node$hex)) return(normalize_color(node$hex, fallback))
		if (!is.null(node$value)) return(normalize_color(node$value, fallback))
		if (!is.null(node$color)) return(normalize_color(node$color, fallback))
		flatten <- unlist(node, use.names = FALSE)
		if (length(flatten) > 0) return(normalize_color(as.character(flatten[[1]]), fallback))
	}

	fallback
}

extract_column_names <- function(node) {
	if (is.null(node)) return(character())

	if (is.character(node)) {
		values <- as.character(node)
		return(values[values != ""])
	}

	if (is.list(node) && !is.null(node$columns_name)) {
		value <- as.character(node$columns_name)
		return(value[value != ""])
	}

	if (is.list(node)) {
		values <- unlist(lapply(node, extract_column_names), use.names = FALSE)
		values <- as.character(values)
		values <- values[values != ""]
		return(unique(values))
	}

	character()
}

to_bool <- function(x, default = FALSE) {
	if (is.null(x) || length(x) == 0) return(default)
	if (is.logical(x)) return(isTRUE(x[[1]]))
	if (is.numeric(x)) return(x[[1]] != 0)

	if (is.character(x)) {
		v <- tolower(trimws(x[[1]]))
		if (v %in% c("true", "1", "yes", "y", "on")) return(TRUE)
		if (v %in% c("false", "0", "no", "n", "off")) return(FALSE)
	}

	default
}

to_number <- function(x, default, min_value = NULL, max_value = NULL) {
	v <- suppressWarnings(as.numeric(x %||% default))
	if (is.na(v)) v <- default
	if (!is.null(min_value) && v < min_value) v <- min_value
	if (!is.null(max_value) && v > max_value) v <- max_value
	v
}

normalize_title_position <- function(x, default = "center") {
	v <- tolower(trimws(as.character(x %||% default)[[1]]))
	if (!(v %in% c("left", "center", "right"))) return(default)
	v
}

normalize_angle_col <- function(x, default = 45) {
	v <- suppressWarnings(as.numeric(x %||% default))
	if (is.na(v)) v <- default
	allowed <- c(0, 45, 90, 270, 315)
	allowed[[which.min(abs(allowed - v))]]
}

format_vector_for_info <- function(x) {
	if (length(x) == 0) return("none")
	paste(x, collapse = ", ")
}

read_selected_matrix <- function(input_node, input_name) {
	if (is.null(input_node$content)) {
		stop(sprintf("%s.content 缺失", input_name))
	}

	file_path <- input_node$content
	if (!file.exists(file_path)) {
		stop(sprintf("%s 文件不存在: %s", input_name, file_path))
	}

	df <- readr::read_tsv(file_path, show_col_types = FALSE)
	if (ncol(df) < 2) {
		stop(sprintf("%s 文件列数不足，至少需要 2 列", input_name))
	}

	selected_cols <- extract_column_names(input_node$feature_vars)
	if (length(selected_cols) == 0) {
		stop(sprintf("%s 未选择任何 feature_vars 列", input_name))
	}

	selected_cols <- unique(selected_cols)
	missing_cols <- setdiff(selected_cols, colnames(df))
	if (length(missing_cols) > 0) {
		stop(sprintf("%s 选择列在文件中不存在: %s", input_name, paste(missing_cols, collapse = ", ")))
	}

	feature_col <- colnames(df)[1]
	matrix_df <- df %>%
		dplyr::select(dplyr::all_of(c(feature_col, selected_cols))) %>%
		dplyr::mutate(dplyr::across(dplyr::all_of(selected_cols), as.numeric)) %>%
		dplyr::filter(!is.na(.data[[feature_col]]) & .data[[feature_col]] != "") %>%
		dplyr::distinct(.data[[feature_col]], .keep_all = TRUE)

	mat <- matrix_df %>%
		tibble::column_to_rownames(feature_col) %>%
		as.matrix()

	storage.mode(mat) <- "numeric"
	mat
}

compute_p_matrix <- function(x_mat_t, y_mat_t, method = "spearman") {
	x_names <- colnames(x_mat_t)
	y_names <- colnames(y_mat_t)

	p_mat <- matrix(NA_real_, nrow = length(x_names), ncol = length(y_names),
									dimnames = list(x_names, y_names))

	for (i in seq_along(x_names)) {
		x_vec <- x_mat_t[, i]
		for (j in seq_along(y_names)) {
			y_vec <- y_mat_t[, j]
			ok <- !(is.na(x_vec) | is.na(y_vec))

			if (sum(ok) < 3) {
				p_mat[i, j] <- NA_real_
			} else {
				test_res <- tryCatch(
					stats::cor.test(x_vec[ok], y_vec[ok], method = method),
					error = function(e) NULL
				)
				p_mat[i, j] <- if (is.null(test_res)) NA_real_ else test_res$p.value
			}
		}
	}

	p_mat
}

build_star_matrix <- function(sig_matrix, level1 = 0.05, level2 = 0.01, level3 = 0.001) {
	if (is.null(sig_matrix)) {
		return(NULL)
	}

	c1 <- suppressWarnings(as.numeric(level1 %||% 0.05))
	c2 <- suppressWarnings(as.numeric(level2 %||% 0.01))
	c3 <- suppressWarnings(as.numeric(level3 %||% 0.001))
	cutoffs <- sort(unique(c(c1, c2, c3)), decreasing = TRUE)
	if (length(cutoffs) != 3 || any(is.na(cutoffs)) || any(cutoffs <= 0)) {
		cutoffs <- c(0.05, 0.01, 0.001)
	}

	c1 <- cutoffs[[1]]
	c2 <- cutoffs[[2]]
	c3 <- cutoffs[[3]]

	stars <- matrix("", nrow = nrow(sig_matrix), ncol = ncol(sig_matrix),
									dimnames = dimnames(sig_matrix))

	stars[!is.na(sig_matrix) & sig_matrix < c1] <- "*"
	stars[!is.na(sig_matrix) & sig_matrix < c2] <- "**"
	stars[!is.na(sig_matrix) & sig_matrix < c3] <- "***"

	stars
}

args <- commandArgs(trailingOnly = TRUE)
# params_path <- if (length(args) >= 1) args[[1]] else "params.json"
# output_path <- if (length(args) >= 2) args[[2]] else "output"
params_path <-"params.json"
output_path <-  "output"

if (!file.exists(params_path)) {
	stop(sprintf("参数文件不存在: %s", params_path))
}

dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

data <- jsonlite::fromJSON(params_path, simplifyVector = FALSE)

x_mat <- read_selected_matrix(data$x_input, "x_input")
y_mat <- read_selected_matrix(data$y_input, "y_input")

x_samples <- colnames(x_mat)
y_samples <- colnames(y_mat)
common_samples <- intersect(colnames(x_mat), colnames(y_mat))
x_only_samples <- setdiff(x_samples, y_samples)
y_only_samples <- setdiff(y_samples, x_samples)
if (length(common_samples) < 3) {
	stop("x_input 与 y_input 的共同样本列少于 3 个，无法进行相关性与显著性检验")
}

x_mat <- x_mat[, common_samples, drop = FALSE]
y_mat <- y_mat[, common_samples, drop = FALSE]

x_mat_t <- t(x_mat)
y_mat_t <- t(y_mat)

corr_method <- as.character(data$corr_method %||% "spearman")
if (!(corr_method %in% c("spearman", "pearson", "kendall"))) {
	corr_method <- "spearman"
}

corr_matrix <- suppressWarnings(stats::cor(x_mat_t, y_mat_t, method = corr_method, use = "pairwise.complete.obs"))
p_matrix <- compute_p_matrix(x_mat_t, y_mat_t, method = corr_method)

adjust_method <- as.character(data$adjust_method %||% "BH")
if (tolower(adjust_method) == "none") {
	q_vector <- as.vector(p_matrix)
} else {
	q_vector <- stats::p.adjust(as.vector(p_matrix), method = adjust_method)
}
q_matrix <- matrix(q_vector, nrow = nrow(p_matrix), ncol = ncol(p_matrix), dimnames = dimnames(p_matrix))

show_signif_marker <- to_bool(data$show_signif_marker, TRUE)
signif_by <- as.character(data$signif_by %||% "p")
sig_source <- if (tolower(signif_by) == "q") q_matrix else p_matrix
star_matrix <- build_star_matrix(
	sig_source,
	level1 = data$sig_level_1 %||% 0.05,
	level2 = data$sig_level_2 %||% 0.01,
	level3 = data$sig_level_3 %||% 0.001
)

low_color <- normalize_color(data$low_color, "#4575B4")
mid_color <- normalize_color(data$mid_color, "#FFFFFF")
high_color <- normalize_color(data$high_color, "#D73027")
plot_title <- as.character(data$plot_title %||% "Correlation Heatmap")

heatmap_width <- suppressWarnings(as.numeric(data$heatmap_width %||% 12))
heatmap_height <- suppressWarnings(as.numeric(data$heatmap_height %||% 10))
if (is.na(heatmap_width) || heatmap_width <= 0) heatmap_width <- 12
if (is.na(heatmap_height) || heatmap_height <= 0) heatmap_height <- 10

cluster_rows <- to_bool(data$cluster_rows, TRUE)
cluster_cols <- to_bool(data$cluster_cols, TRUE)
show_rownames <- to_bool(data$heatmap_show_rownames, TRUE)
show_colnames <- to_bool(data$heatmap_show_colnames, TRUE)

axis_fontsize_x <- to_number(data$axis_fontsize_x, default = 10, min_value = 1)
axis_fontsize_y <- to_number(data$axis_fontsize_y, default = 10, min_value = 1)
legend_fontsize <- to_number(data$legend_fontsize, default = 11, min_value = 1)
title_fontsize <- to_number(data$title_fontsize, default = 14, min_value = 1)
title_position <- normalize_title_position(data$title_position, default = "center")
x_axis_rotation <- normalize_angle_col(data$x_axis_rotation, default = 45)

readr::write_tsv(as.data.frame(corr_matrix) %>% tibble::rownames_to_column("name"),
								 file.path(output_path, "corr_matrix.tsv"))
readr::write_tsv(as.data.frame(p_matrix) %>% tibble::rownames_to_column("name"),
								 file.path(output_path, "p_matrix.tsv"))
readr::write_tsv(as.data.frame(q_matrix) %>% tibble::rownames_to_column("name"),
								 file.path(output_path, "q_matrix.tsv"))

plot_pdf <- file.path(output_path, "correlation_heatmap.pdf")
grDevices::pdf(file = plot_pdf, width = heatmap_width, height = heatmap_height)

heatmap_obj <- pheatmap::pheatmap(
	corr_matrix,
	display_numbers = if (show_signif_marker) star_matrix else FALSE,
	color = colorRampPalette(c(low_color, mid_color, high_color))(100),
	cluster_rows = cluster_rows,
	cluster_cols = cluster_cols,
	show_rownames = show_rownames,
	show_colnames = show_colnames,
	fontsize_number = 10,
	fontsize = legend_fontsize,
	fontsize_row = axis_fontsize_y,
	fontsize_col = axis_fontsize_x,
	angle_col = as.character(x_axis_rotation),
	main = NA,
	silent = TRUE,
	border_color = NA
)

grid::grid.newpage()
grid::grid.draw(heatmap_obj$gtable)

if (!is.na(plot_title) && plot_title != "") {
	title_x <- switch(title_position, left = 0.02, center = 0.5, right = 0.98)
	title_just <- switch(title_position,
		left = c("left", "top"),
		center = c("center", "top"),
		right = c("right", "top")
	)
	grid::grid.text(
		plot_title,
		x = grid::unit(title_x, "npc"),
		y = grid::unit(0.99, "npc"),
		just = title_just,
		gp = grid::gpar(fontsize = title_fontsize, fontface = "bold")
	)
}

grDevices::dev.off()

total_pairs <- nrow(corr_matrix) * ncol(corr_matrix)
valid_corr_count <- sum(!is.na(corr_matrix))
valid_p_count <- sum(!is.na(p_matrix))
valid_q_count <- sum(!is.na(q_matrix))

sig_level_1 <- suppressWarnings(as.numeric(data$sig_level_1 %||% 0.05))
sig_level_2 <- suppressWarnings(as.numeric(data$sig_level_2 %||% 0.01))
sig_level_3 <- suppressWarnings(as.numeric(data$sig_level_3 %||% 0.001))
if (is.na(sig_level_1)) sig_level_1 <- 0.05
if (is.na(sig_level_2)) sig_level_2 <- 0.01
if (is.na(sig_level_3)) sig_level_3 <- 0.001

info_lines <- c(
	"# Correlation Heatmap Output",
	"",
	"## Run Info",
	sprintf("- run_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
	sprintf("- params_path: %s", params_path),
	sprintf("- output_path: %s", output_path),
	"",
	"## Sample Match Info",
	sprintf("- x_sample_count: %d", length(x_samples)),
	sprintf("- y_sample_count: %d", length(y_samples)),
	sprintf("- matched_sample_count: %d", length(common_samples)),
	sprintf("- x_only_sample_count: %d", length(x_only_samples)),
	sprintf("- x_only_samples: %s", format_vector_for_info(x_only_samples)),
	sprintf("- y_only_sample_count: %d", length(y_only_samples)),
	sprintf("- y_only_samples: %s", format_vector_for_info(y_only_samples)),
	"",
	"## Basic Params",
	sprintf("- corr_method: %s", corr_method),
	sprintf("- adjust_method: %s", adjust_method),
	sprintf("- show_signif_marker: %s", show_signif_marker),
	sprintf("- signif_by: %s", signif_by),
	sprintf("- sig_level_1: %s", as.character(sig_level_1)),
	sprintf("- sig_level_2: %s", as.character(sig_level_2)),
	sprintf("- sig_level_3: %s", as.character(sig_level_3)),
	sprintf("- cluster_rows: %s", cluster_rows),
	sprintf("- cluster_cols: %s", cluster_cols),
	sprintf("- show_rownames: %s", show_rownames),
	sprintf("- show_colnames: %s", show_colnames),
	sprintf("- heatmap_width: %s", heatmap_width),
	sprintf("- heatmap_height: %s", heatmap_height),
	sprintf("- axis_fontsize_x: %s", axis_fontsize_x),
	sprintf("- axis_fontsize_y: %s", axis_fontsize_y),
	sprintf("- legend_fontsize: %s", legend_fontsize),
	sprintf("- title_fontsize: %s", title_fontsize),
	sprintf("- title_position: %s", title_position),
	sprintf("- x_axis_rotation: %s", x_axis_rotation),
	sprintf("- low_color: %s", low_color),
	sprintf("- mid_color: %s", mid_color),
	sprintf("- high_color: %s", high_color),
	sprintf("- plot_title: %s", plot_title),
	"",
	"## Matrix Stats",
	sprintf("- x_feature_count: %d", nrow(x_mat)),
	sprintf("- y_feature_count: %d", nrow(y_mat)),
	sprintf("- common_sample_count_used: %d", length(common_samples)),
	sprintf("- corr_matrix_dim: %d x %d", nrow(corr_matrix), ncol(corr_matrix)),
	sprintf("- total_pairs: %d", total_pairs),
	sprintf("- valid_corr_count: %d", valid_corr_count),
	sprintf("- valid_p_count: %d", valid_p_count),
	sprintf("- valid_q_count: %d", valid_q_count),
	sprintf("- significant_count_%s_lt_%s: %d", signif_by, sig_level_1, sum(!is.na(sig_source) & sig_source < sig_level_1)),
	sprintf("- significant_count_%s_lt_%s: %d", signif_by, sig_level_2, sum(!is.na(sig_source) & sig_source < sig_level_2)),
	sprintf("- significant_count_%s_lt_%s: %d", signif_by, sig_level_3, sum(!is.na(sig_source) & sig_source < sig_level_3)),
	sprintf("- plot_pdf: %s", plot_pdf)
)

readr::write_lines(info_lines, file.path(output_path, "output.md"))

message(sprintf("Heatmap saved to: %s", plot_pdf))
