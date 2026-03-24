library(tidyverse)
library(jsonlite)

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0) y else x
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

format_vector_for_info <- function(x) {
	x <- as.character(x)
	x <- x[!is.na(x) & x != ""]
	if (length(x) == 0) return("none")
	paste(x, collapse = ", ")
}

format_kv_pairs_for_info <- function(keys, values) {
	if (length(keys) == 0) return("none")
	paste(paste0(keys, " -> ", values), collapse = "; ")
}

split_rule_tokens <- function(x) {
	if (is.null(x) || length(x) == 0) return(character())
	v <- as.character(x[[1]])
	if (is.na(v) || trimws(v) == "") return(character())
	tokens <- strsplit(v, ";", fixed = TRUE)[[1]]
	tokens <- trimws(tokens)
	tokens[tokens != ""]
}

parse_kv_pairs <- function(kv_text) {
	if (is.null(kv_text) || length(kv_text) == 0) {
		return(list(keys = character(), values = character()))
	}

	raw <- as.character(kv_text[[1]])
	if (is.na(raw) || trimws(raw) == "") {
		return(list(keys = character(), values = character()))
	}

	lines <- unlist(strsplit(raw, "\\r?\\n", perl = TRUE), use.names = FALSE)
	lines <- trimws(lines)
	lines <- lines[lines != ""]

	keys <- character()
	values <- character()
	for (line in lines) {
		sep_pos <- regexpr(":", line, fixed = TRUE)
		if (sep_pos[[1]] <= 0) {
			stop(sprintf("K:V 替换规则格式错误（缺少冒号）: %s", line))
		}

		key <- trimws(substr(line, 1, sep_pos[[1]] - 1))
		value <- trimws(substr(line, sep_pos[[1]] + 1, nchar(line)))
		if (key == "") {
			stop(sprintf("K:V 替换规则格式错误（空 key）: %s", line))
		}

		keys <- c(keys, key)
		values <- c(values, value)
	}

	list(keys = keys, values = values)
}

normalize_replace_mode <- function(mode, regex_from = NULL, kv_text = NULL) {
	v <- tolower(trimws(as.character(mode %||% "")[[1]]))
	if (v %in% c("none", "regex", "kv")) return(v)

	if (length(split_rule_tokens(regex_from)) > 0) return("regex")
	if (length(parse_kv_pairs(kv_text)$keys) > 0) return("kv")
	"none"
}

apply_feature_replace_rules <- function(values, from_rules, to_rules) {
	from_vec <- split_rule_tokens(from_rules)
	to_vec <- split_rule_tokens(to_rules)

	if (length(from_vec) == 0) {
		return(list(
			values = values,
			rule_count = 0,
			from = character(),
			to = character(),
			changed_count = 0
		))
	}

	if (length(to_vec) < length(from_vec)) {
		to_vec <- c(to_vec, rep("", length(from_vec) - length(to_vec)))
	}
	if (length(to_vec) > length(from_vec)) {
		to_vec <- to_vec[seq_along(from_vec)]
	}

	old_values <- values
	new_values <- values
	for (i in seq_along(from_vec)) {
		new_values <- gsub(from_vec[[i]], to_vec[[i]], new_values, perl = TRUE)
	}

	list(
		values = new_values,
		rule_count = length(from_vec),
		from = from_vec,
		to = to_vec,
		changed_count = sum(old_values != new_values)
	)
}

pick_param <- function(primary, fallback = NULL) {
	if (!is.null(primary) && length(primary) > 0) return(primary)
	fallback
}

normalize_mediation_method <- function(x) {
	v <- tolower(trimws(as.character(x %||% "")[[1]]))
	if (v %in% c("current", "mediation_pkg")) return(v)
	"current"
}

parse_positive_integer <- function(x, default_value, min_value = 1L) {
	v <- suppressWarnings(as.integer(as.character(x %||% default_value)[[1]]))
	if (is.na(v) || v < min_value) return(as.integer(default_value))
	as.integer(v)
}

resolve_parallel_cores <- function(user_cores = NULL) {
	detected <- parallel::detectCores(logical = TRUE)
	if (is.na(detected) || detected < 1) detected <- 1L
	default_cores <- max(1L, as.integer(detected) - 1L)
	cores <- parse_positive_integer(user_cores, default_value = default_cores, min_value = 1L)
	cores <- min(as.integer(cores), as.integer(detected))
	if (.Platform$OS.type == "windows") {
		return(1L)
	}
	cores
}

apply_sample_replace <- function(sample_names, mode, regex_from = NULL, regex_to = NULL, kv_text = NULL) {
	mode_value <- normalize_replace_mode(mode, regex_from = regex_from, kv_text = kv_text)

	if (mode_value == "none") {
		return(list(
			values = sample_names,
			mode = mode_value,
			rule_count = 0,
			changed_count = 0,
			regex_from = character(),
			regex_to = character(),
			kv_keys = character(),
			kv_values = character()
		))
	}

	if (mode_value == "regex") {
		res <- apply_feature_replace_rules(sample_names, regex_from, regex_to)
		return(list(
			values = res$values,
			mode = mode_value,
			rule_count = res$rule_count,
			changed_count = res$changed_count,
			regex_from = res$from,
			regex_to = res$to,
			kv_keys = character(),
			kv_values = character()
		))
	}

	kv <- parse_kv_pairs(kv_text)
	new_values <- sample_names
	if (length(kv$keys) > 0) {
		for (i in seq_along(kv$keys)) {
			new_values[new_values == kv$keys[[i]]] <- kv$values[[i]]
		}
	}

	list(
		values = new_values,
		mode = mode_value,
		rule_count = length(kv$keys),
		changed_count = sum(sample_names != new_values),
		regex_from = character(),
		regex_to = character(),
		kv_keys = kv$keys,
		kv_values = kv$values
	)
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
		stop(sprintf("%s 文件列数不足，至少需要 2 列: %s", input_name, file_path))
	}

	legacy_sample_cols <- extract_column_names(input_node$sample_vars)
	control_cols <- extract_column_names(input_node$control_sample_vars)
	treatment_cols <- extract_column_names(input_node$treatment_sample_vars)
	selected_cols <- unique(c(legacy_sample_cols, control_cols, treatment_cols))
	if (length(selected_cols) == 0) {
		stop(sprintf("%s 未选择任何样本列（sample_vars/control_sample_vars/treatment_sample_vars）", input_name))
	}

	overlap_group_cols <- intersect(control_cols, treatment_cols)
	if (length(overlap_group_cols) > 0) {
		stop(sprintf("%s 中 control_sample_vars 与 treatment_sample_vars 存在重复列: %s", input_name, paste(overlap_group_cols, collapse = ", ")))
	}

	selected_cols <- unique(selected_cols)
	missing_cols <- setdiff(selected_cols, colnames(df))
	if (length(missing_cols) > 0) {
		stop(sprintf("%s 选择的样本列在文件中不存在: %s", input_name, paste(missing_cols, collapse = ", ")))
	}

	feature_candidates <- extract_column_names(input_node$feature_var)
	if (length(feature_candidates) == 0) {
		stop(sprintf("%s 未选择 feature_var 列", input_name))
	}

	feature_col <- feature_candidates[[1]]
	if (!(feature_col %in% colnames(df))) {
		stop(sprintf("%s 选择的 feature_var 在文件中不存在: %s", input_name, feature_col))
	}

	if (feature_col %in% selected_cols) {
		stop(sprintf("%s 中 feature_var(%s) 不能与 sample_vars 重复", input_name, feature_col))
	}

	matrix_df <- df %>%
		dplyr::select(dplyr::all_of(c(feature_col, selected_cols))) %>%
		dplyr::filter(!is.na(.data[[feature_col]]) & .data[[feature_col]] != "") %>%
		dplyr::distinct(.data[[feature_col]], .keep_all = TRUE)

	mat <- matrix_df %>%
		tibble::column_to_rownames(feature_col) %>%
		as.matrix()

	list(
		mat = mat,
		selected_cols = selected_cols,
		control_cols = unique(control_cols),
		treatment_cols = unique(treatment_cols),
		legacy_sample_cols = unique(legacy_sample_cols),
		feature_col = feature_col,
		file_path = file_path
	)
}

remap_selected_names <- function(original_all, replaced_all, selected_cols) {
	if (length(selected_cols) == 0) return(character())
	name_map <- stats::setNames(replaced_all, original_all)
	mapped <- unname(name_map[selected_cols])
	mapped <- as.character(mapped)
	mapped <- mapped[!is.na(mapped) & mapped != ""]
	unique(mapped)
}

as_numeric_matrix <- function(mat, input_name) {
	numeric_mat <- suppressWarnings(as.numeric(mat))
	numeric_mat <- matrix(
		numeric_mat,
		nrow = nrow(mat),
		ncol = ncol(mat),
		dimnames = dimnames(mat)
	)

	na_count <- sum(is.na(numeric_mat))
	if (na_count == nrow(numeric_mat) * ncol(numeric_mat)) {
		stop(sprintf("%s 转换为数值矩阵失败，全部为 NA", input_name))
	}

	numeric_mat
}

fit_one_mediation_current <- function(x_vec, m_vec, y_vec) {
	df <- tibble::tibble(
		X = as.numeric(x_vec),
		M = as.numeric(m_vec),
		Y = as.numeric(y_vec)
	) %>%
		dplyr::filter(stats::complete.cases(.))

	if (nrow(df) < 8) return(NULL)
	if (stats::sd(df$X) == 0 || stats::sd(df$M) == 0 || stats::sd(df$Y) == 0) return(NULL)

	model_a <- stats::lm(M ~ X, data = df)
	model_b <- stats::lm(Y ~ X + M, data = df)
	model_t <- stats::lm(Y ~ X, data = df)

	sum_a <- summary(model_a)$coefficients
	sum_b <- summary(model_b)$coefficients
	sum_t <- summary(model_t)$coefficients

	if (!("X" %in% rownames(sum_a)) || !("M" %in% rownames(sum_b)) || !("X" %in% rownames(sum_b))) {
		return(NULL)
	}

	a <- as.numeric(sum_a["X", "Estimate"])
	sa <- as.numeric(sum_a["X", "Std. Error"])
	b <- as.numeric(sum_b["M", "Estimate"])
	sb <- as.numeric(sum_b["M", "Std. Error"])
	c_prime <- as.numeric(sum_b["X", "Estimate"])
	c_total <- as.numeric(sum_t["X", "Estimate"])

	indirect <- a * b
	sobel_se <- sqrt((b^2) * (sa^2) + (a^2) * (sb^2))
	sobel_z <- ifelse(sobel_se > 0, indirect / sobel_se, NA_real_)
	p_indirect <- ifelse(is.na(sobel_z), NA_real_, 2 * stats::pnorm(abs(sobel_z), lower.tail = FALSE))

	tibble::tibble(
		n = nrow(df),
		a_effect = a,
		a_p = as.numeric(sum_a["X", "Pr(>|t|)"]),
		b_effect = b,
		b_p = as.numeric(sum_b["M", "Pr(>|t|)"]),
		direct_effect = c_prime,
		direct_p = as.numeric(sum_b["X", "Pr(>|t|)"]),
		total_effect = c_total,
		total_p = as.numeric(sum_t["X", "Pr(>|t|)"]),
		indirect_effect = indirect,
		indirect_z = sobel_z,
		indirect_p = p_indirect,
		prop_mediated = ifelse(c_total == 0, NA_real_, indirect / c_total)
	)
}

fit_one_mediation_package <- function(x_name, y_name, x_vec, m_vec, y_vec, sims = 1000L) {
	df <- tibble::tibble(
		X = as.numeric(x_vec),
		M = as.numeric(m_vec),
		Y = as.numeric(y_vec)
	) %>%
		dplyr::filter(stats::complete.cases(.))
	message(str_glue("{x_name} vs {y_name} start" ))
	abc <<- df
	
	if (nrow(df) < 8) return(NULL)
	if (stats::sd(df$X) == 0 || stats::sd(df$M) == 0 || stats::sd(df$Y) == 0) return(NULL)
	# model_a <- stats::lm(M ~ X, data = df)
	model_a <- glm(M ~ X, data = df, family ="gaussian")
	model_b <- glm(Y ~ X + M, data = df, family = "binomial")
	model_t <- glm(Y ~ X, data = df)
	# 在 mediate 调用前插入
	# print("Model A Summary:")
	# print(summary(model_a))
	# print("Model B Summary:")
	# print(summary(model_b))

	med_obj <- mediation::mediate(
		model.m = model_a,
		model.y = model_b,
		treat = "X",
		mediator = "M",
		sims = sims,
		boot = TRUE
	)
	message(str_glue("{x_name} vs {y_name} end" ))
	
	sum_a <- summary(model_a)$coefficients
	sum_b <- summary(model_b)$coefficients
	sum_t <- summary(model_t)$coefficients

	if (!("X" %in% rownames(sum_a)) || !("M" %in% rownames(sum_b)) || !("X" %in% rownames(sum_b))) {
		return(NULL)
	}

	a <- as.numeric(sum_a["X", "Estimate"])
	b <- as.numeric(sum_b["M", "Estimate"])
	c_prime <- as.numeric(sum_b["X", "Estimate"])
	c_total <- as.numeric(sum_t["X", "Estimate"])

	indirect <- as.numeric(med_obj$d.avg %||% med_obj$d0 %||% NA_real_)
	p_indirect <- as.numeric(med_obj$d.avg.p %||% med_obj$d0.p %||% NA_real_)
	direct <- as.numeric(med_obj$z.avg %||% med_obj$z0 %||% c_prime)
	direct_p <- as.numeric(med_obj$z.avg.p %||% med_obj$z0.p %||% as.numeric(sum_b["X", "Pr(>|t|)"]))
	total <- as.numeric(med_obj$tau.coef %||% c_total)
	total_p <- as.numeric(med_obj$tau.p %||% as.numeric(sum_t["X", "Pr(>|t|)"]))
	prop_mediated <- as.numeric(med_obj$n.avg %||% med_obj$n0 %||% ifelse(total == 0, NA_real_, indirect / total))
	
	tibble::tibble(
		n = nrow(df),
		a_effect = a,
		a_p = as.numeric(sum_a["X", "Pr(>|t|)"]),
		b_effect = b,
		b_p = as.numeric(sum_b["M", "Pr(>|z|)"]),
		direct_effect = direct,
		direct_p = direct_p,
		total_effect = total,
		total_p = total_p,
		indirect_effect = indirect,
		indirect_z = NA_real_,
		indirect_p = p_indirect,
		prop_mediated = prop_mediated
	)
}

fit_one_mediation <- function(x_name, y_name, x_vec, m_vec, y_vec, method = "current", sims = 1000L) {
	if (identical(method, "mediation_pkg")) {
		return(fit_one_mediation_package(x_name, y_name, x_vec, m_vec, y_vec, sims = sims))
	}
	fit_one_mediation_current(x_vec, m_vec, y_vec)
}

plot_mediation_sankey <- function(res_df, output_file, top_n = 30) {
	plot_df <- res_df %>%
		dplyr::arrange(indirect_q, indirect_p) %>%
		dplyr::slice_head(n = min(top_n, nrow(.)))

	if (nrow(plot_df) == 0) {
		return(FALSE)
	}

	link_xm <- plot_df %>%
		dplyr::transmute(source = x_feature, target = y_feature, level_source = 1L, level_target = 2L, weight = abs(indirect_effect), sign = sign(indirect_effect), edge = "X->M")
	link_my <- plot_df %>%
		dplyr::transmute(source = y_feature, target = group_feature, level_source = 2L, level_target = 3L, weight = abs(indirect_effect), sign = sign(indirect_effect), edge = "M->Y")

	edges <- dplyr::bind_rows(link_xm, link_my) %>%
		dplyr::group_by(source, target, level_source, level_target, edge, sign) %>%
		dplyr::summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop")

	nodes <- dplyr::bind_rows(
		tibble::tibble(name = unique(plot_df$x_feature), level = 1L),
		tibble::tibble(name = unique(plot_df$y_feature), level = 2L),
		tibble::tibble(name = unique(plot_df$group_feature), level = 3L)
	) %>%
		dplyr::distinct() %>%
		dplyr::group_by(level) %>%
		dplyr::arrange(name, .by_group = TRUE) %>%
		dplyr::mutate(y = dplyr::row_number()) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(x = level)

	edges_plot <- edges %>%
		dplyr::left_join(nodes %>% dplyr::select(source = name, x = x, y = y), by = "source") %>%
		dplyr::rename(x = x, y = y) %>%
		dplyr::left_join(nodes %>% dplyr::select(target = name, xend = x, yend = y), by = "target")

	p <- ggplot2::ggplot() +
		ggplot2::geom_curve(
			data = edges_plot,
			ggplot2::aes(x = x, y = y, xend = xend, yend = yend, size = weight, color = factor(sign)),
			curvature = 0.25,
			alpha = 0.65,
			lineend = "round"
		) +
		ggplot2::geom_point(
			data = nodes,
			ggplot2::aes(x = x, y = y),
			size = 3,
			shape = 21,
			fill = "white",
			stroke = 0.8
		) +
		ggplot2::geom_text(
			data = nodes,
			ggplot2::aes(x = x, y = y, label = name),
			nudge_x = 0.08,
			hjust = 0,
			size = 3
		) +
		ggplot2::scale_x_continuous(
			breaks = c(1, 2, 3),
			labels = c("Metabolite (X)", "Microbe (M)", "Phenotype (Y)"),
			limits = c(0.8, 3.8)
		) +
		ggplot2::scale_color_manual(values = c("-1" = "#2C7BB6", "0" = "#7F7F7F", "1" = "#D7191C"), guide = "none") +
		ggplot2::scale_size_continuous(range = c(0.4, 2.6)) +
		ggplot2::labs(
			title = "Mediation Sankey-like Path Map",
			subtitle = sprintf("Top %d mediation paths by indirect q-value", nrow(plot_df)),
			x = NULL,
			y = NULL,
			size = "|indirect effect|"
		) +
		ggplot2::theme_minimal(base_size = 11) +
		ggplot2::theme(
			panel.grid = ggplot2::element_blank(),
			axis.text.y = ggplot2::element_blank(),
			axis.ticks = ggplot2::element_blank(),
			plot.title = ggplot2::element_text(face = "bold")
		)

	ggplot2::ggsave(filename = output_file, plot = p, width = 12, height = 8)
	TRUE
}

plot_triangle_path <- function(one_row, output_file) {
	nodes <- tibble::tibble(
		node = c("X", "M", "Y"),
		label = c(
			sprintf("Metabolite\\n%s", one_row$x_feature),
			sprintf("Microbe\\n%s", one_row$y_feature),
			sprintf("Phenotype\\n%s", one_row$group_feature)
		),
		x = c(-1, 0, 1),
		y = c(0, 1.2, 0)
	)

	edges <- tibble::tibble(
		x = c(-1, 0, -1),
		y = c(0, 1.2, 0),
		xend = c(0, 1, 1),
		yend = c(1.2, 0, 0),
		label = c(
			sprintf("a = %.3g\\np = %.3g", one_row$a_effect, one_row$a_p),
			sprintf("b = %.3g\\np = %.3g", one_row$b_effect, one_row$b_p),
			sprintf("c' = %.3g\\np = %.3g", one_row$direct_effect, one_row$direct_p)
		),
		lx = c(-0.55, 0.55, 0),
		ly = c(0.72, 0.72, -0.16)
	)

	p <- ggplot2::ggplot() +
		ggplot2::geom_curve(
			data = edges,
			ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
			curvature = 0.08,
			arrow = ggplot2::arrow(length = grid::unit(0.18, "cm")),
			linewidth = 0.8,
			color = "#3A3A3A"
		) +
		ggplot2::geom_label(
			data = nodes,
			ggplot2::aes(x = x, y = y, label = label),
			size = 3.3,
			label.size = 0.25,
			fill = "#F7F7F7"
		) +
		ggplot2::geom_label(
			data = edges,
			ggplot2::aes(x = lx, y = ly, label = label),
			size = 3,
			label.size = 0.2,
			fill = "white"
		) +
		ggplot2::annotate(
			"label",
			x = 0,
			y = -0.55,
			label = sprintf(
				"indirect = %.3g (p = %.3g, q = %.3g)\\ndirect = %.3g\\ntotal = %.3g\\nprop_mediated = %.3g",
				one_row$indirect_effect,
				one_row$indirect_p,
				one_row$indirect_q,
				one_row$direct_effect,
				one_row$total_effect,
				one_row$prop_mediated
			),
			size = 3,
			label.size = 0.25,
			fill = "#FFFBEA"
		) +
		ggplot2::coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(-0.9, 1.6), clip = "off") +
		ggplot2::labs(title = "Top Mediation Triangle Path", x = NULL, y = NULL) +
		ggplot2::theme_void(base_size = 11) +
		ggplot2::theme(
			plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
		)

	ggplot2::ggsave(filename = output_file, plot = p, width = 8, height = 6)
}

params_path <- "params.json"
output_dir <- "output"

if (!file.exists(params_path)) {
	stop(sprintf("参数文件不存在: %s", params_path))
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

data <- jsonlite::fromJSON(params_path, simplifyVector = FALSE)

x_parsed <- read_selected_matrix(data$x_input, "x_input")
y_parsed <- read_selected_matrix(data$y_input, "y_input")
x_mat <- as_numeric_matrix(x_parsed$mat, "x_input")
y_mat <- as_numeric_matrix(y_parsed$mat, "y_input")

x_sample_replace_from <- pick_param(data$x_sample_replace_from, data$x_feature_replace_from)
x_sample_replace_to <- pick_param(data$x_sample_replace_to, data$x_feature_replace_to)
y_sample_replace_from <- pick_param(data$y_sample_replace_from, data$y_feature_replace_from)
y_sample_replace_to <- pick_param(data$y_sample_replace_to, data$y_feature_replace_to)
x_sample_replace_mode <- pick_param(data$x_sample_replace_mode, "none")
y_sample_replace_mode <- pick_param(data$y_sample_replace_mode, "none")
x_sample_replace_kv <- pick_param(data$x_sample_replace_kv, NULL)
y_sample_replace_kv <- pick_param(data$y_sample_replace_kv, NULL)
mediation_method <- normalize_mediation_method(pick_param(data$mediation_method, "current"))
mediation_sims <- parse_positive_integer(pick_param(data$mediation_sims, 1000L), default_value = 1000L, min_value = 100L)
parallel_cores <- resolve_parallel_cores(pick_param(data$parallel_cores, NULL))

if (identical(mediation_method, "mediation_pkg") && !requireNamespace("mediation", quietly = TRUE)) {
	stop(sprintf("参数 mediation_method=mediation_pkg 需要安装 R 包 mediation"))
}

x_replace_res <- apply_sample_replace(
	colnames(x_mat),
	mode = x_sample_replace_mode,
	regex_from = x_sample_replace_from,
	regex_to = x_sample_replace_to,
	kv_text = x_sample_replace_kv
)
y_replace_res <- apply_sample_replace(
	colnames(y_mat),
	mode = y_sample_replace_mode,
	regex_from = y_sample_replace_from,
	regex_to = y_sample_replace_to,
	kv_text = y_sample_replace_kv
)

if (anyDuplicated(x_replace_res$values) > 0) {
	dup <- unique(x_replace_res$values[duplicated(x_replace_res$values)])
	stop(sprintf("x_input 替换后样本名存在重复，无法一一匹配: %s", paste(dup, collapse = ", ")))
}
if (anyDuplicated(y_replace_res$values) > 0) {
	dup <- unique(y_replace_res$values[duplicated(y_replace_res$values)])
	stop(sprintf("y_input 替换后样本名存在重复，无法一一匹配: %s", paste(dup, collapse = ", ")))
}

colnames(x_mat) <- x_replace_res$values
colnames(y_mat) <- y_replace_res$values

x_control_samples <- remap_selected_names(colnames(x_parsed$mat), colnames(x_mat), x_parsed$control_cols)
x_treatment_samples <- remap_selected_names(colnames(x_parsed$mat), colnames(x_mat), x_parsed$treatment_cols)

x_group_map <- stats::setNames(rep(NA_character_, ncol(x_mat)), colnames(x_mat))
x_group_map[x_control_samples] <- "control"
x_group_map[x_treatment_samples] <- "treatment"

x_samples <- colnames(x_mat)
y_samples <- colnames(y_mat)
common_samples <- intersect(x_samples, y_samples)
x_only_samples <- setdiff(x_samples, y_samples)
y_only_samples <- setdiff(y_samples, x_samples)

if (length(common_samples) == 0) {
	stop(sprintf("x_input 与 y_input 没有共同样本名，无法对齐。x_sample_count=%d, y_sample_count=%d", length(x_samples), length(y_samples)))
}

group_label <- x_group_map[common_samples]
group_assigned_mask <- !is.na(group_label)
if (sum(group_assigned_mask) == 0) {
	stop(sprintf("无法从 x_input 的 control_sample_vars/treatment_sample_vars 推断分组，请在 x_input 中选择分组样本列"))
}

group_unassigned_samples <- common_samples[!group_assigned_mask]
if (length(group_unassigned_samples) > 0) {
	message(sprintf("有 %d 个共同样本未被分组定义，已从中介分析中移除", length(group_unassigned_samples)))
}

common_samples <- common_samples[group_assigned_mask]
group_label <- group_label[group_assigned_mask]
group_vec <- ifelse(group_label == "control", 0, 1)
names(group_vec) <- common_samples

if (length(common_samples) == 0) {
	stop(sprintf("用于分析的共同样本为空（可能均未分组）"))
}

x_aligned <- x_mat[, common_samples, drop = FALSE]
y_aligned <- y_mat[, common_samples, drop = FALSE]
group_aligned <- group_vec[common_samples]
if(F){
  # Bacteroidetes vs PC(22:6(4Z,7Z,10Z,13Z,16Z,19Z)/P-18:1(11Z))
  x_vec <- x_aligned["Firmicutes",]
  y_vec <- y_aligned["PC(22:6(4Z,7Z,10Z,13Z,16Z,19Z)/P-18:1(11Z))",]
  m_vec <- group_aligned
  fit_one_mediation_package("aa", "bb", x_vec, m_vec, y_vec, sims = 1000)
}
group_levels <- sort(unique(group_aligned))
if (!identical(group_levels, c(0, 1))) {
	stop(sprintf("分组编码异常，期望包含 control=0 和 treatment=1，实际为: %s", paste(group_levels, collapse = ", ")))
}

valid_group_mask <- !is.na(group_aligned)
if (sum(valid_group_mask) < 8) {
	stop(sprintf("group 在共同样本中有效值太少，至少需要 8 个，当前有效数: %d", sum(valid_group_mask)))
}

if (sum(valid_group_mask) < length(group_aligned)) {
	message(sprintf("group 存在 NA，已移除 %d 个样本", sum(!valid_group_mask)))
}

common_samples <- common_samples[valid_group_mask]
x_aligned <- x_aligned[, common_samples, drop = FALSE]
y_aligned <- y_aligned[, common_samples, drop = FALSE]
group_aligned <- group_aligned[valid_group_mask]

x_output <- file.path(output_dir, "x_aligned.tsv")
y_output <- file.path(output_dir, "y_aligned.tsv")
# group_output <- file.path(output_dir, "group_aligned.tsv")
mediation_input_output <- file.path(output_dir, "mediation_input.tsv")
mediation_output <- file.path(output_dir, "mediation_all.tsv")
top_output <- file.path(output_dir, "mediation_top.tsv")
sankey_output <- file.path(output_dir, "mediation_sankey.pdf")
triangle_output <- file.path(output_dir, "mediation_triangle.pdf")

# readr::write_tsv(as.data.frame(x_aligned) %>% tibble::rownames_to_column("feature"), x_output)
# readr::write_tsv(as.data.frame(y_aligned) %>% tibble::rownames_to_column("feature"), y_output)
# readr::write_tsv(
# 	tibble::tibble(sample = names(group_aligned), group = as.integer(group_aligned), group_label = ifelse(group_aligned == 0, "control", "treatment")),
# 	group_output
# )

# x_sample_feature_df <- as.data.frame(t(x_aligned), check.names = FALSE) %>%
# 	tibble::rownames_to_column("sample") %>%
# 	dplyr::mutate(
# 		group = as.integer(group_aligned[.data$sample]),
# 		group_label = ifelse(.data$group == 0, "control", "treatment")
# 	)
# 
# y_sample_feature_df <- as.data.frame(t(y_aligned), check.names = FALSE) %>%
# 	tibble::rownames_to_column("sample")
# 
# x_feature_only <- x_sample_feature_df %>%
# 	dplyr::select(-sample, -group, -group_label)
# colnames(x_feature_only) <- paste0("x__", colnames(x_feature_only))
# 
# y_feature_only <- y_sample_feature_df %>%
# 	dplyr::select(-sample)
# colnames(y_feature_only) <- paste0("m__", colnames(y_feature_only))
# 
# mediation_input_df <- x_sample_feature_df %>%
# 	dplyr::select(sample, group, group_label) %>%
# 	dplyr::bind_cols(x_feature_only, y_feature_only)
# 
# readr::write_tsv(mediation_input_df, mediation_input_output)

calc_for_one_x <- function(i) {
	x_name <- rownames(x_aligned)[[i]]
	x_vec <- as.numeric(x_aligned[i, ])
	one_x_results <- vector("list", length = nrow(y_aligned))
	local_idx <- 1L

	for (j in seq_len(nrow(y_aligned))) {
		y_name <- rownames(y_aligned)[[j]]
		y_vec <- as.numeric(y_aligned[j, ])

		fit <- fit_one_mediation(x_name, y_name,  x_vec, y_vec, group_aligned, method = mediation_method, sims = mediation_sims)
		if (!is.null(fit)) {
			one_x_results[[local_idx]] <- fit %>%
				dplyr::mutate(
					x_feature = x_name,
					y_feature = y_name,
					group_feature = "control_vs_treatment"
				)
			local_idx <- local_idx + 1L
		}
	}

	one_x_results[seq_len(max(0, local_idx - 1L))]
}

x_indices <- seq_len(nrow(x_aligned))
if (parallel_cores > 1L && length(x_indices) > 1L) {
	message(sprintf("中介分析并行计算开启: parallel_cores=%d", parallel_cores))
	result_nested <- parallel::mclapply(x_indices, calc_for_one_x, mc.cores = parallel_cores)
} else {
	message(sprintf("中介分析串行计算: parallel_cores=%d", parallel_cores))
	result_nested <- lapply(x_indices, calc_for_one_x)
}

result_list <- unlist(result_nested, recursive = FALSE, use.names = FALSE)
if (length(result_list) == 0) {
	stop(sprintf("没有可用的中介模型结果（可能是缺失值过多或变量方差为 0）"))
}

mediation_df <- dplyr::bind_rows(result_list) %>%
	mutate(
		indirect_q = p.adjust(indirect_p, method = "BH"),
		direct_q = p.adjust(direct_p, method = "BH"),
		total_q = p.adjust(total_p, method = "BH")
	) %>%
	dplyr::arrange(indirect_q, indirect_p)

readr::write_tsv(mediation_df, mediation_output)

# top_df <- mediation_df %>%
# 	dplyr::slice_head(n = min(30, nrow(mediation_df)))
# # readr::write_tsv(top_df, top_output)
# 
# sankey_ok <- plot_mediation_sankey(top_df, sankey_output, top_n = 30)
# plot_triangle_path(top_df[1, ], triangle_output)

info_lines <- c(
	"# Causal Mediation Analysis Output",
	"",
	"## Run Info",
	sprintf("- run_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
	sprintf("- params_path: %s", params_path),
	sprintf("- output_path: %s", output_dir),
	"",
	"## Sample Match Info",
	sprintf("- x_sample_count: %d", length(x_samples)),
	sprintf("- y_sample_count: %d", length(y_samples)),
	sprintf("- matched_sample_count: %d", length(common_samples)),
	sprintf("- matched_samples: %s", format_vector_for_info(common_samples)),
	sprintf("- x_only_sample_count: %d", length(x_only_samples)),
	sprintf("- x_only_samples: %s", format_vector_for_info(x_only_samples)),
	sprintf("- y_only_sample_count: %d", length(y_only_samples)),
	sprintf("- y_only_samples: %s", format_vector_for_info(y_only_samples)),
	sprintf("- unassigned_group_sample_count: %d", length(group_unassigned_samples)),
	sprintf("- unassigned_group_samples: %s", format_vector_for_info(group_unassigned_samples)),
	"",
	"## Group Info",
	sprintf("- group_source: %s", "x_input.control_sample_vars + x_input.treatment_sample_vars"),
	sprintf("- x_control_sample_count: %d", length(x_control_samples)),
	sprintf("- x_treatment_sample_count: %d", length(x_treatment_samples)),
	sprintf("- group_mapping: %s", "control=0; treatment=1"),
	"",
	"## Method Params",
	sprintf("- mediation_method: %s", mediation_method),
	sprintf("- mediation_sims: %d", mediation_sims),
	sprintf("- parallel_cores: %d", parallel_cores),
	"",
	"## Mediation Stats",
	sprintf("- x_feature_count: %d", nrow(x_aligned)),
	sprintf("- y_feature_count: %d", nrow(y_aligned)),
	sprintf("- tested_pair_count: %d", nrow(mediation_df)),
	sprintf("- significant_indirect_q_lt_0.05: %d", sum(mediation_df$indirect_q < 0.05, na.rm = TRUE)),
	# sprintf("- top_pair_x: %s", top_df$x_feature[[1]]),
	# sprintf("- top_pair_y: %s", top_df$y_feature[[1]]),
	# sprintf("- top_pair_indirect: %.4g", top_df$indirect_effect[[1]]),
	# sprintf("- top_pair_indirect_p: %.4g", top_df$indirect_p[[1]]),
	# sprintf("- top_pair_indirect_q: %.4g", top_df$indirect_q[[1]]),
	"",
	"## Sample Name Replace Rules",
	sprintf("- x_sample_replace_mode: %s", x_replace_res$mode),
	sprintf("- x_sample_rule_count: %d", x_replace_res$rule_count),
	sprintf("- x_sample_changed_count: %d", x_replace_res$changed_count),
	sprintf("- x_sample_replace_from(regex): %s", format_vector_for_info(x_replace_res$regex_from)),
	sprintf("- x_sample_replace_to(regex): %s", format_vector_for_info(x_replace_res$regex_to)),
	sprintf("- x_sample_replace_kv: %s", format_kv_pairs_for_info(x_replace_res$kv_keys, x_replace_res$kv_values)),
	sprintf("- y_sample_replace_mode: %s", y_replace_res$mode),
	sprintf("- y_sample_rule_count: %d", y_replace_res$rule_count),
	sprintf("- y_sample_changed_count: %d", y_replace_res$changed_count),
	sprintf("- y_sample_replace_from(regex): %s", format_vector_for_info(y_replace_res$regex_from)),
	sprintf("- y_sample_replace_to(regex): %s", format_vector_for_info(y_replace_res$regex_to)),
	sprintf("- y_sample_replace_kv: %s", format_kv_pairs_for_info(y_replace_res$kv_keys, y_replace_res$kv_values)),
	"",
	"## Output Files",
	# sprintf("- x_aligned_file: %s", x_output),
	# sprintf("- y_aligned_file: %s", y_output),
	# sprintf("- group_aligned_file: %s", group_output),
	# sprintf("- mediation_input_file: %s", mediation_input_output),
	sprintf("- mediation_all_file: %s", mediation_output),
	# sprintf("- mediation_top_file: %s", top_output),
	# sprintf("- mediation_sankey_file: %s", ifelse(sankey_ok, sankey_output, "none")),
	# sprintf("- mediation_triangle_file: %s", triangle_output),
	"",
	"## mediation_all.tsv Columns",
	sprintf("- n: %s", "用于该条中介模型拟合的有效样本数（complete cases）"),
	sprintf("- a_effect: %s", "路径 a 的效应值，X -> M 的回归系数"),
	sprintf("- a_p: %s", "路径 a 的显著性 p 值"),
	sprintf("- b_effect: %s", "路径 b 的效应值，M -> Y（控制 X）回归系数"),
	sprintf("- b_p: %s", "路径 b 的显著性 p 值"),
	sprintf("- direct_effect: %s", "直接效应 c'，X -> Y（控制 M）"),
	sprintf("- direct_p: %s", "直接效应 c' 的 p 值"),
	sprintf("- total_effect: %s", "总效应 c，X -> Y（不控制 M）"),
	sprintf("- total_p: %s", "总效应 c 的 p 值"),
	sprintf("- indirect_effect: %s", "间接效应，通常为 a*b；mediation 包模式下取 ACME"),
	sprintf("- indirect_z: %s", "间接效应的 Z 统计量；current(Sobel) 模式有效，mediation_pkg 模式为 NA"),
	sprintf("- indirect_p: %s", "间接效应的 p 值；current 模式为 Sobel p 值，mediation_pkg 模式为 ACME p 值"),
	sprintf("- prop_mediated: %s", "中介比例，约为 indirect_effect / total_effect"),
	sprintf("- x_feature: %s", "该模型中的 X 特征名（来自 x_input）"),
	sprintf("- y_feature: %s", "该模型中的 M 特征名（来自 y_input）"),
	sprintf("- group_feature: %s", "该模型中的 Y 变量说明，当前为 control_vs_treatment"),
	sprintf("- indirect_q: %s", "indirect_p 经 BH 方法多重检验校正后的 q 值"),
	sprintf("- direct_q: %s", "direct_p 经 BH 方法多重检验校正后的 q 值"),
	sprintf("- total_q: %s", "total_p 经 BH 方法多重检验校正后的 q 值")
)

readr::write_lines(info_lines, file.path(output_dir, "output.md"))
message(sprintf("Causal mediation outputs saved: %s", output_dir))