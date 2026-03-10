library(tidyverse)

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0) y else x
}

extract_column_names <- function(node) {
	if (is.null(node)) return(character())

	if (is.character(node)) {
		return(node)
	}

	if (is.list(node) && !is.null(node$columns_name)) {
		return(as.character(node$columns_name))
	}

	if (is.list(node)) {
		values <- unlist(lapply(node, extract_column_names), use.names = FALSE)
		return(unique(values[values != ""]))
	}

	character()
}

extract_single_column <- function(node, default = NULL) {
	values <- extract_column_names(node)
	if (length(values) == 0) return(default)
	values[[1]]
}

normalize_color <- function(node) {
	if (is.null(node) || length(node) == 0) return(NULL)

	if (is.character(node)) {
		value <- node[[1]]
		if (is.na(value) || value == "") return(NULL)
		return(value)
	}

	if (is.list(node)) {
		if (!is.null(node$hex)) return(normalize_color(node$hex))
		if (!is.null(node$value)) return(normalize_color(node$value))
		if (!is.null(node$color)) return(normalize_color(node$color))

		flatten <- unlist(node, use.names = FALSE)
		if (length(flatten) > 0) return(normalize_color(as.character(flatten[[1]])))
	}

	NULL
}

safe_color <- function(color_value, fallback, label) {
	if (is.null(color_value)) return(fallback)
	valid <- tryCatch({
		grDevices::col2rgb(color_value)
		TRUE
	}, error = function(e) {
		FALSE
	})

	if (!valid) {
		warning(sprintf("%s 无效颜色值: %s，使用默认颜色 %s", label, color_value, fallback))
		return(fallback)
	}

	color_value
}

params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)

input_file <- params$input_file
if (is.null(input_file) || is.null(input_file$content)) {
	stop("params.json 缺少 input_file.content")
}

file_path <- input_file$content
df <- readr::read_tsv(file_path, show_col_types = FALSE)

feature_col <- extract_single_column(input_file$x_var, default = "Row.names")
panel_col <- extract_single_column(input_file$panel_var, default = NULL)
# y_label_default <- extract_single_column(input_file$y_var, default = "abundance")

group1_cols <- extract_column_names(input_file$group1_vars)
group2_cols <- extract_column_names(input_file$group2_vars)
selected_samples <- unique(c(group1_cols, group2_cols))

if (length(selected_samples) == 0) {
	stop("group1_vars 与 group2_vars 至少需要选择一列")
}

required_cols <- unique(c(feature_col, panel_col, selected_samples))
required_cols <- required_cols[!is.null(required_cols) & required_cols != ""]
missing_cols <- setdiff(required_cols, colnames(df))
if (length(missing_cols) > 0) {
	stop(sprintf("输入文件缺少列: %s", paste(missing_cols, collapse = ", ")))
}

long_df <- df %>%
	dplyr::select(dplyr::all_of(required_cols)) %>%
	tidyr::pivot_longer(
		cols = dplyr::all_of(selected_samples),
		names_to = "sample",
		values_to = "value"
	) %>%
	dplyr::mutate(
		treatment = dplyr::case_when(
			sample %in% group1_cols ~ "group1",
			sample %in% group2_cols ~ "group2",
			TRUE ~ "other"
		),
		treatment = factor(treatment, levels = c("group1", "group2", "other"))
	)

plot_type <- params$plot_type %||% "violin"
show_stats <- params$show_stats %||% FALSE
stat_label <- params$stat_label %||% "p"
point_size <- as.numeric(params$point_size %||% 1.5)
point_alpha <- as.numeric(params$point_alpha %||% 0.7)
group1_color <- safe_color(normalize_color(params$group1_color), "#1f77b4", "group1_color")
group2_color <- safe_color(normalize_color(params$group2_color), "#ff7f0e", "group2_color")
x_label <- params$x_label %||%  "" #feature_col
y_label <- params$y_label %||%  "" #"abundance"
output_name <- params$output_name %||% "boxplot"

x_var <- feature_col

base_plot <- ggplot(long_df, aes(x = .data[[x_var]], y = value, color = treatment, fill = treatment))

plot_obj <- switch(
	plot_type,
	"violin" = {
		base_plot +
			geom_violin(trim = FALSE, alpha = 0.35) +
			geom_jitter(width = 0.15, size = point_size, alpha = point_alpha)
	},
	"scatter" = {
		base_plot +
			geom_jitter(width = 0.2, size = point_size, alpha = point_alpha)
	},
	"boxplotV1" ={
        box_dodge_width <- 0.75
        base_plot +
        geom_boxplot(outlier.alpha = 0.3, alpha = 0.35,				
            position = position_dodge(width = box_dodge_width)) +
        geom_jitter( size = point_size, alpha = point_alpha,
            position = position_jitterdodge(jitter.width = 0.15, dodge.width = box_dodge_width) )
	  
	},
	"boxplot" = {
		box_dodge_width <- 0.75
		base_plot +
			geom_boxplot(
				outlier.alpha = 0.3,
				alpha = 0.35,
				position = position_dodge(width = box_dodge_width)
			) +
			geom_point(
				position = position_jitterdodge(jitter.width = 0.15, dodge.width = box_dodge_width),
				size = point_size,
				alpha = point_alpha
			)
	},
	{
		warning(sprintf("未知 plot_type=%s，使用 violin", plot_type))
		base_plot +
			geom_violin(trim = FALSE, alpha = 0.35) +
			geom_jitter(width = 0.15, size = point_size, alpha = point_alpha)
	}
)

if (!is.null(panel_col) && panel_col %in% colnames(long_df)) {
	plot_obj <- plot_obj + facet_wrap(vars(.data[[panel_col]]), scales = "free_x")
}

if (isTRUE(show_stats)) {
	p_col <- "P_value"
	q_col <- "Qvalue"

	join_cols <- feature_col
	if (!is.null(panel_col) && panel_col %in% colnames(df)) {
		join_cols <- c(join_cols, panel_col)
	}

	stats_source_cols <- unique(c(join_cols, p_col, q_col))
	stats_source_cols <- intersect(stats_source_cols, colnames(df))

	stats_df <- df %>%
		dplyr::select(dplyr::all_of(stats_source_cols)) %>%
		dplyr::distinct()

	if (all(c(p_col, q_col) %in% colnames(stats_df))) {
		stats_df <- stats_df %>%
			dplyr::mutate(
				stat_text = dplyr::case_when(
					stat_label == "p" ~ sprintf("p=%.3g", .data[[p_col]]),
					stat_label == "q" ~ sprintf("q=%.3g", .data[[q_col]]),
					TRUE ~ sprintf("p=%.3g\nq=%.3g", .data[[p_col]], .data[[q_col]])
				)
			)
	} else if (p_col %in% colnames(stats_df)) {
		stats_df <- stats_df %>%
			dplyr::mutate(stat_text = sprintf("p=%.3g", .data[[p_col]]))
	} else if (q_col %in% colnames(stats_df)) {
		stats_df <- stats_df %>%
			dplyr::mutate(stat_text = sprintf("q=%.3g", .data[[q_col]]))
	} else {
		stats_df <- NULL
	}

	if (!is.null(stats_df)) {
		y_max <- long_df %>%
			dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
			dplyr::summarise(y_pos = max(value, na.rm = TRUE) * 1.05, .groups = "drop")

		stats_df <- stats_df %>%
			dplyr::left_join(y_max, by = join_cols)

		plot_obj <- plot_obj +
			geom_text(
				data = stats_df,
				aes(x = .data[[feature_col]], y = y_pos, label = stat_text),
				inherit.aes = FALSE,
				size = 3,
				angle = 90,
				vjust = -0.2
			)
	}
}

plot_obj <- plot_obj +
	scale_color_manual(values = c(group1 = group1_color, group2 = group2_color, other = "#BDBDBD")) +
	scale_fill_manual(values = c(group1 = group1_color, group2 = group2_color, other = "#BDBDBD")) +
	labs(
		x = x_label,
		y = y_label,
		color = "Group",
		fill = "Group"
	) +
	theme_bw(base_size = 12) +
	theme(
		axis.text.x = element_text(angle = 45, hjust = 1),
		legend.position = "top"
	)
output_path <- str_glue("output/{output_name}.pdf")
ggsave(filename = output_path, plot = plot_obj, width = 12, height = 7, dpi = 300)
message(sprintf("Plot saved to: %s", output_path))
