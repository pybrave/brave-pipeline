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

	selected_cols <- extract_column_names(input_node$sample_vars)
	if (length(selected_cols) == 0) {
		stop(sprintf("%s 未选择任何 sample_vars 列", input_name))
	}

	selected_cols <- unique(selected_cols)
	missing_cols <- setdiff(selected_cols, colnames(df))
	if (length(missing_cols) > 0) {
		stop(sprintf("%s 选择的 sample_vars 在文件中不存在: %s", input_name, paste(missing_cols, collapse = ", ")))
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
		dplyr::mutate(dplyr::across(dplyr::all_of(selected_cols), as.numeric)) %>%
		dplyr::filter(!is.na(.data[[feature_col]]) & .data[[feature_col]] != "") %>%
		dplyr::distinct(.data[[feature_col]], .keep_all = TRUE)

	mat <- matrix_df %>%
		tibble::column_to_rownames(feature_col) %>%
		as.matrix()

	storage.mode(mat) <- "numeric"
	mat
}

params_path <- "params.json"
output_dir <- "output"

if (!file.exists(params_path)) {
	stop(sprintf("参数文件不存在: %s", params_path))
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

data <- jsonlite::fromJSON(params_path, simplifyVector = FALSE)

x_mat <- read_selected_matrix(data$x_input, "x_input")
y_mat <- read_selected_matrix(data$y_input, "y_input")

x_sample_replace_from <- pick_param(data$x_sample_replace_from, data$x_feature_replace_from)
x_sample_replace_to <- pick_param(data$x_sample_replace_to, data$x_feature_replace_to)
y_sample_replace_from <- pick_param(data$y_sample_replace_from, data$y_feature_replace_from)
y_sample_replace_to <- pick_param(data$y_sample_replace_to, data$y_feature_replace_to)
x_sample_replace_mode <- pick_param(data$x_sample_replace_mode, "none")
y_sample_replace_mode <- pick_param(data$y_sample_replace_mode, "none")
x_sample_replace_kv <- pick_param(data$x_sample_replace_kv, NULL)
y_sample_replace_kv <- pick_param(data$y_sample_replace_kv, NULL)

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

x_samples <- colnames(x_mat)
y_samples <- colnames(y_mat)
common_samples <- intersect(x_samples, y_samples)
x_only_samples <- setdiff(x_samples, y_samples)
y_only_samples <- setdiff(y_samples, x_samples)

if (length(common_samples) == 0) {
	stop(sprintf("x_input 与 y_input 没有共同样本名，无法对齐。x_sample_count=%d, y_sample_count=%d", length(x_samples), length(y_samples)))
}

x_aligned <- x_mat[, common_samples, drop = FALSE]
y_aligned <- y_mat[, common_samples, drop = FALSE]

x_output <- file.path(output_dir, "x_aligned.tsv")
y_output <- file.path(output_dir, "y_aligned.tsv")

readr::write_tsv(as.data.frame(x_aligned) %>% tibble::rownames_to_column("feature"), x_output)
readr::write_tsv(as.data.frame(y_aligned) %>% tibble::rownames_to_column("feature"), y_output)

info_lines <- c(
	"# Sample Name Matching Output",
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
	sprintf("- x_aligned_file: %s", x_output),
	sprintf("- y_aligned_file: %s", y_output)
)

readr::write_lines(info_lines, file.path(output_dir, "output.md"))
message(sprintf("Sample matching output saved: %s", output_dir))