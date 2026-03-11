library(tidyverse)
library(ggdist)
library(gghalves)

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

safe_max <- function(x, fallback = 0) {
	mx <- suppressWarnings(max(x, na.rm = TRUE))
	if (!is.finite(mx)) fallback else mx
}

significance_to_star <- function(x) {
	dplyr::case_when(
		is.na(x) ~ "ns",
		x < 1e-4 ~ "****",
		x < 1e-3 ~ "***",
		x < 1e-2 ~ "**",
		x < 0.05 ~ "*",
		TRUE ~ "ns"
	)
}

sanitize_filename <- function(x) {
	x <- as.character(x)
	x <- stringr::str_trim(x)
	x <- stringr::str_replace_all(x, "[^A-Za-z0-9._-]", "_")
	x <- stringr::str_replace_all(x, "_+", "_")
	ifelse(x == "", "panel", x)
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
p_col_selected <- extract_single_column(input_file$p_col, default = "P_value")
q_col_selected <- extract_single_column(input_file$q_col, default = "Qvalue")
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
panel_type <- params$panel_type %||% "free_x"
show_stats <- params$show_stats %||% FALSE
stat_label <- params$stat_label %||% "p"
stat_display <- params$stat_display %||% "value"
stat_position <- params$stat_position %||% "group_top"
point_size <- as.numeric(params$point_size %||% 1.5)
point_alpha <- as.numeric(params$point_alpha %||% 0.7)
axis_text_size <- as.numeric(params$axis_text_size %||% 10)
axis_title_size <- as.numeric(params$axis_title_size %||% 12)
legend_text_size <- as.numeric(params$legend_text_size %||% 9)
legend_title_size <- as.numeric(params$legend_title_size %||% 10)
legend_title_text <- params$legend_title_text %||% "Group"
legend_group1_text <- params$legend_group1_text %||% "group1"
legend_group2_text <- params$legend_group2_text %||% "group2"
legend_other_text <- params$legend_other_text %||% "other"
# CNS-like defaults (close to commonly used NPG palette)
group1_color <- safe_color(normalize_color(params$group1_color), "#4DBBD5", "group1_color")
group2_color <- safe_color(normalize_color(params$group2_color), "#E64B35", "group2_color")
x_label <- params$x_label %||%  "" #feature_col
y_label <- params$y_label %||%  "" #"abundance"
plot_title <- params$title %||% ""
title_size <- as.numeric(params$title_size %||% 14)
title_position <- params$title_position %||% "left"
legend_position <- params$legend_position %||% "top"
output_name <- params$output_name %||% "boxplot"

if (!(title_position %in% c("left", "center", "right"))) {
	title_position <- "left"
}
if (!(legend_position %in% c("top", "bottom", "left", "right", "none"))) {
	legend_position <- "top"
}
title_hjust <- dplyr::case_when(
	title_position == "left" ~ 0,
	title_position == "center" ~ 0.5,
	TRUE ~ 1
)

x_var <- feature_col

base_plot <- ggplot(long_df, aes(x = .data[[x_var]], y = value, color = treatment, fill = treatment))

plot_obj <- switch(
	plot_type,
	"violin" = {
		violin_dodge_width <- 0.75
		base_plot +
			geom_violin(
				trim = FALSE,
				alpha = 0.35,
				width = 0.7,
				position = position_dodge(width = violin_dodge_width)
			) +
			geom_point(
				position = position_jitterdodge(
					jitter.width = 0.12,
					dodge.width = violin_dodge_width,
					seed = 1
				),
				size = point_size,
				alpha = point_alpha
			)
	},
	"scatter" = {
		scatter_dodge_width <- 0.75
		base_plot +
			geom_point(
				position = position_jitterdodge(
					jitter.width = 0.14,
					dodge.width = scatter_dodge_width,
					seed = 1
				),
				size = point_size,
				alpha = point_alpha
			)
	},
	"half_violin_scatter" = {
		# if (!requireNamespace("ggdist", quietly = TRUE)) {
		# 	stop("plot_type=half_violin_scatter 需要安装 ggdist 包")
		# }

	  half_dodge_width <- 0.75
	  
	  
	  base_plot +
	    # geom_dots()
  	  geom_half_violin( position = "dodge") + 
      geom_dotplot(binaxis = "y", method="histodot", stackdir="up", position = "dodge")
	    # ggdist::stat_halfeye(
	    #   adjust = 0.7,
	    #   width = 0.55,
	    #   .width = 0,
	    #   justification = 0,
	    #   point_colour = NA,
	    #   side = "left",
	    #   alpha = 0.7,
	    #   position = ggdist::position_dodgejust(width = half_dodge_width)
	    # ) +
	    # ggdist::stat_dots(
	    #   side = "right",
	    #   justification = 0,
	    #   dotsize = 0.6,
	    #   alpha = point_alpha,
	    #   position = ggdist::position_dodgejust(width = half_dodge_width)
	    # )
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
				outlier.shape = NA,
				width = 0.62,
				size = 0.55,
				alpha = 0.85,
				position = position_dodge(width = box_dodge_width)
			) +
			geom_point(
				position = position_jitterdodge(jitter.width = 0.15, dodge.width = box_dodge_width),
				shape = 21,
				stroke = 0.25,
				size = point_size * 0.95,
				alpha = point_alpha * 0.9
			)
	},
	{
		warning(sprintf("未知 plot_type=%s，使用 violin", plot_type))
		base_plot +
			geom_violin(trim = FALSE, alpha = 0.35) +
			geom_jitter(width = 0.15, size = point_size, alpha = point_alpha)
	}
)

add_stats_layer <- function(plot_in, data_for_plot, source_df) {
	if (!isTRUE(show_stats)) {
		return(plot_in)
	}

	p_col <- "P_value"
	q_col <- "Qvalue"
	if (!is.null(p_col_selected) && nzchar(p_col_selected)) {
		p_col <- p_col_selected
	}
	if (!is.null(q_col_selected) && nzchar(q_col_selected)) {
		q_col <- q_col_selected
	}

	join_cols <- feature_col
	if (!is.null(panel_col) && panel_col %in% colnames(source_df) && panel_col %in% colnames(data_for_plot)) {
		join_cols <- c(join_cols, panel_col)
	}

	stats_source_cols <- unique(c(join_cols, p_col, q_col))
	stats_source_cols <- intersect(stats_source_cols, colnames(source_df))
	if (length(stats_source_cols) == 0) {
		return(plot_in)
	}

	stats_df <- source_df %>%
		dplyr::select(dplyr::all_of(stats_source_cols)) %>%
		dplyr::distinct()

	if (isTRUE(stat_display == "star")) {
		if (all(c(p_col, q_col) %in% colnames(stats_df))) {
			stats_df <- stats_df %>%
				dplyr::mutate(
					stat_text = dplyr::case_when(
						stat_label == "p" ~ significance_to_star(.data[[p_col]]),
						stat_label == "q" ~ significance_to_star(.data[[q_col]]),
						TRUE ~ sprintf("p:%s\nq:%s", significance_to_star(.data[[p_col]]), significance_to_star(.data[[q_col]]))
					)
				)
		} else if (p_col %in% colnames(stats_df)) {
			stats_df <- stats_df %>%
				dplyr::mutate(stat_text = significance_to_star(.data[[p_col]]))
		} else if (q_col %in% colnames(stats_df)) {
			stats_df <- stats_df %>%
				dplyr::mutate(stat_text = significance_to_star(.data[[q_col]]))
		} else {
			return(plot_in)
		}
	} else {
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
			return(plot_in)
		}
	}

	if (isTRUE(stat_position == "uniform_top")) {
		if (!is.null(panel_col) && panel_col %in% colnames(data_for_plot) && panel_col %in% colnames(stats_df)) {
			y_max <- data_for_plot %>%
				dplyr::group_by(.data[[panel_col]]) %>%
				dplyr::summarise(y_pos = safe_max(value) * 1.08, .groups = "drop")

			stats_df <- stats_df %>%
				dplyr::left_join(y_max, by = panel_col)
		} else {
			stats_df <- stats_df %>%
				dplyr::mutate(y_pos = safe_max(data_for_plot$value) * 1.08)
		}
	} else {
		y_max <- data_for_plot %>%
			dplyr::group_by(dplyr::across(dplyr::all_of(join_cols))) %>%
			dplyr::summarise(y_pos = safe_max(value) * 1.05, .groups = "drop")

		stats_df <- stats_df %>%
			dplyr::left_join(y_max, by = join_cols)
	}

	plot_in +
		geom_text(
			data = stats_df,
			aes(x = .data[[feature_col]], y = y_pos, label = stat_text),
			inherit.aes = FALSE,
			size = 3,
			# angle = 90,
			vjust = -0.2
		)
}

add_common_style <- function(plot_in, title_text = "") {
	# is_boxplot <- plot_type %in% c("boxplot", "boxplotV1")

	# base_theme <- if (is_boxplot) {
	# 	theme_classic(base_size = 12)
	# } else {
	# 	theme_bw(base_size = 12)
	# }
	base_theme <-  theme_classic(base_size = 12)

	plot_in +
		scale_color_manual(
			values = c(group1 = group1_color, group2 = group2_color, other = "#BDBDBD"),
			breaks = c("group1", "group2", "other"),
			labels = c(legend_group1_text, legend_group2_text, legend_other_text)
		) +
		scale_fill_manual(
			values = c(group1 = group1_color, group2 = group2_color, other = "#BDBDBD"),
			breaks = c("group1", "group2", "other"),
			labels = c(legend_group1_text, legend_group2_text, legend_other_text)
		) +
		labs(
			x = x_label,
			y = y_label,
			title = title_text,
			color = legend_title_text,
			fill = legend_title_text
		) +
		base_theme +
		theme(
			axis.text.x = element_text(size = axis_text_size, angle = 45, hjust = 1, color = "#222222"),
			axis.text.y = element_text(size = axis_text_size, color = "#222222"),
			axis.title = element_text(size = axis_title_size, color = "#111111"),
			legend.position = legend_position,
			legend.title = element_text(size = legend_title_size, face = "bold"),
			legend.text = element_text(size = legend_text_size),
			panel.grid = element_blank(),
			axis.line = element_line(color = "#1A1A1A", linewidth = 0.5),
			axis.ticks = element_line(color = "#1A1A1A", linewidth = 0.45),
			plot.title = element_text(size = title_size, face = "bold", hjust = title_hjust)
		)
}

if (!is.null(panel_col) && panel_col %in% colnames(long_df) && panel_type == "split") {
	panel_values <- unique(long_df[[panel_col]])
	panel_values <- panel_values[!is.na(panel_values)]

	for (panel_value in panel_values) {
		panel_data <- long_df %>% dplyr::filter(.data[[panel_col]] == panel_value)
		panel_source_df <- df %>% dplyr::filter(.data[[panel_col]] == panel_value)
		panel_plot <- plot_obj %+% panel_data
		panel_plot <- add_stats_layer(panel_plot, panel_data, panel_source_df)

		panel_prefix <- as.character(panel_value)
		panel_title <- if (nzchar(plot_title)) {
			stringr::str_c(panel_prefix, " - ", plot_title)
		} else {
			panel_prefix
		}
		panel_plot <- add_common_style(panel_plot, panel_title)

		panel_suffix <- sanitize_filename(panel_value)
		output_path <- str_glue("output/{output_name}_{panel_suffix}.pdf")
		ggsave(filename = output_path, plot = panel_plot, width = 12, height = 7, dpi = 300)
		message(sprintf("Plot saved to: %s", output_path))
	}
} else {
	if (!is.null(panel_col) && panel_col %in% colnames(long_df) && panel_type == "free_x" && panel_type!="none") {
		plot_obj <- plot_obj + facet_wrap(vars(.data[[panel_col]]), scales = "free_x")
	}

	plot_obj <- add_stats_layer(plot_obj, long_df, df)
	plot_obj <- add_common_style(plot_obj, plot_title)

	output_path <- str_glue("output/{output_name}.pdf")
	ggsave(filename = output_path, plot = plot_obj, width = 12, height = 7, dpi = 300)
	message(sprintf("Plot saved to: %s", output_path))
}
