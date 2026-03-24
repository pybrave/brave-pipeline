library(tidyverse)
library(jsonlite)

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0) y else x
}

extract_column_names <- function(node) {
	if (is.null(node)) return(character())

	if (is.character(node)) {
		values <- as.character(node)
		values <- values[!is.na(values) & values != ""]
		return(values)
	}

	if (is.list(node) && !is.null(node$columns_name)) {
		value <- as.character(node$columns_name)
		value <- value[!is.na(value) & value != ""]
		return(value)
	}

	if (is.list(node)) {
		values <- unlist(lapply(node, extract_column_names), use.names = FALSE)
		values <- as.character(values)
		values <- values[!is.na(values) & values != ""]
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

format_optional_number <- function(x) {
	if (is.na(x)) return("NA")
	as.character(x)
}

extract_scalar_string <- function(x, default = "") {
	if (is.null(x) || length(x) == 0) return(default)
	value <- as.character(x[[1]])
	if (is.na(value)) return(default)
	value
}

normalize_output_filename <- function(x, default_name = "merged_by_feature_var.tsv") {
	raw_name <- trimws(extract_scalar_string(x, default = ""))
	if (raw_name == "") return(default_name)

	# Keep output under output_dir by using basename only.
	raw_name <- gsub("\\\\", "/", raw_name)
	name <- basename(raw_name)
	if (name %in% c("", ".", "..")) return(default_name)

	if (!grepl("\\.tsv$", name, ignore.case = TRUE)) {
		name <- paste0(name, ".tsv")
	}

	name
}

read_input_matrix <- function(input_node, input_index) {
	input_name <- sprintf("input_files[%d]", input_index)
	file_path <- input_node$content %||% NA_character_
	if (is.na(file_path) || file_path == "") {
		stop(sprintf("%s.content 缺失", input_name))
	}
	if (!file.exists(file_path)) {
		stop(sprintf("%s 文件不存在: %s", input_name, file_path))
	}

	df <- readr::read_tsv(file_path, show_col_types = FALSE)
	if (ncol(df) < 2) {
		stop(sprintf("%s 文件列数不足，至少需要 2 列: %s", input_name, file_path))
	}

	feature_candidates <- extract_column_names(input_node$feature_var)
	if (length(feature_candidates) == 0) {
		stop(sprintf("%s 未选择 feature_var 列", input_name))
	}
	feature_col <- feature_candidates[[1]]
	if (!(feature_col %in% colnames(df))) {
		stop(sprintf("%s 选择的 feature_var 在文件中不存在: %s", input_name, feature_col))
	}

	sample_cols <- extract_column_names(input_node$sample_vars)
	if (length(sample_cols) == 0) {
		stop(sprintf("%s 未选择任何 sample_vars 列", input_name))
	}
	sample_cols <- unique(sample_cols)

	if (feature_col %in% sample_cols) {
		stop(sprintf("%s 中 feature_var(%s) 不能与 sample_vars 重复", input_name, feature_col))
	}

	missing_cols <- setdiff(sample_cols, colnames(df))
	if (length(missing_cols) > 0) {
		stop(sprintf("%s 选择的 sample_vars 在文件中不存在: %s", input_name, paste(missing_cols, collapse = ", ")))
	}

	selected <- df %>%
		dplyr::select(dplyr::all_of(c(feature_col, sample_cols)))

	selected[[feature_col]] <- as.character(selected[[feature_col]])
	selected <- selected %>%
		dplyr::filter(!is.na(.data[[feature_col]]) & .data[[feature_col]] != "")

	dup_feature_count <- sum(duplicated(selected[[feature_col]]))
	if (dup_feature_count > 0) {
		selected <- selected %>% dplyr::distinct(.data[[feature_col]], .keep_all = TRUE)
	}

	for (col_name in sample_cols) {
		selected[[col_name]] <- suppressWarnings(as.numeric(selected[[col_name]]))
	}

	file_name <- input_node$file_name %||% basename(file_path)
	list(
		input_name = as.character(file_name),
		file_path = as.character(file_path),
		feature_col = feature_col,
		sample_cols = sample_cols,
		dup_feature_count = dup_feature_count,
		data = selected
	)
}

build_sample_consistency_report <- function(inputs) {
	if (length(inputs) == 0) {
		return(tibble::tibble())
	}

	ref_samples <- sort(unique(inputs[[1]]$sample_cols))

	report_rows <- lapply(seq_along(inputs), function(i) {
		cur_samples <- sort(unique(inputs[[i]]$sample_cols))
		missing_vs_ref <- setdiff(ref_samples, cur_samples)
		extra_vs_ref <- setdiff(cur_samples, ref_samples)
		tibble::tibble(
			file_index = i,
			input_name = inputs[[i]]$input_name,
			feature_var = inputs[[i]]$feature_col,
			sample_count = length(cur_samples),
			samples = paste(cur_samples, collapse = ";"),
			consistent_with_first = length(missing_vs_ref) == 0 && length(extra_vs_ref) == 0,
			missing_vs_first = paste(missing_vs_ref, collapse = ";"),
			extra_vs_first = paste(extra_vs_ref, collapse = ";")
		)
	})

	dplyr::bind_rows(report_rows)
}

merge_inputs_by_feature <- function(inputs) {
	if (length(inputs) == 0) {
		stop(sprintf("input_files 为空，无法合并"))
	}

	merged <- inputs[[1]]$data
	colnames(merged)[colnames(merged) == inputs[[1]]$feature_col] <- "feature_var"

	value_conflicts <- list()

	if (length(inputs) >= 2) {
		for (i in 2:length(inputs)) {
			next_df <- inputs[[i]]$data
			colnames(next_df)[colnames(next_df) == inputs[[i]]$feature_col] <- "feature_var"

			overlap_samples <- intersect(setdiff(colnames(merged), "feature_var"), setdiff(colnames(next_df), "feature_var"))

			joined <- dplyr::full_join(merged, next_df, by = "feature_var", suffix = c("", "__dup"))

			if (length(overlap_samples) > 0) {
				for (sample_name in overlap_samples) {
					dup_col <- paste0(sample_name, "__dup")
					if (!(dup_col %in% colnames(joined))) next

					left_vals <- joined[[sample_name]]
					right_vals <- joined[[dup_col]]
					conflict_idx <- which(!is.na(left_vals) & !is.na(right_vals) & left_vals != right_vals)

					if (length(conflict_idx) > 0) {
						conflict_df <- tibble::tibble(
							input_index = i,
							input_name = inputs[[i]]$input_name,
							sample_var = sample_name,
							feature_var = joined$feature_var[conflict_idx],
							left_value = left_vals[conflict_idx],
							right_value = right_vals[conflict_idx]
						)
						value_conflicts[[length(value_conflicts) + 1]] <- conflict_df
					}

					joined[[sample_name]] <- dplyr::coalesce(joined[[sample_name]], joined[[dup_col]])
					joined[[dup_col]] <- NULL
				}
			}

			merged <- joined
		}
	}

	merged <- merged %>% dplyr::arrange(.data$feature_var)

	conflicts_df <- if (length(value_conflicts) == 0) {
		tibble::tibble(
			input_index = integer(),
			input_name = character(),
			sample_var = character(),
			feature_var = character(),
			left_value = double(),
			right_value = double()
		)
	} else {
		dplyr::bind_rows(value_conflicts)
	}

	list(merged = merged, conflicts = conflicts_df)
}

build_feature_missing_report <- function(merged_df) {
	if (nrow(merged_df) == 0 || !("feature_var" %in% colnames(merged_df))) {
		return(tibble::tibble(
			feature_var = character(),
			missing_sample_count = integer(),
			missing_ratio = double(),
			missing_sample_vars = character()
		))
	}

	sample_cols <- setdiff(colnames(merged_df), "feature_var")
	if (length(sample_cols) == 0) {
		return(tibble::tibble(
			feature_var = character(),
			missing_sample_count = integer(),
			missing_ratio = double(),
			missing_sample_vars = character()
		))
	}

	report_rows <- lapply(seq_len(nrow(merged_df)), function(i) {
		missing_idx <- which(is.na(merged_df[i, sample_cols, drop = TRUE]))
		tibble::tibble(
			feature_var = as.character(merged_df$feature_var[[i]] %||% NA_character_),
			missing_sample_count = length(missing_idx),
			missing_ratio = length(missing_idx) / length(sample_cols),
			missing_sample_vars = paste(sample_cols[missing_idx], collapse = ";")
		)
	})

	dplyr::bind_rows(report_rows) %>%
		dplyr::filter(.data$missing_sample_count > 0) %>%
		dplyr::arrange(dplyr::desc(.data$missing_sample_count), .data$feature_var)
}

args <- commandArgs(trailingOnly = TRUE)
params_path <- if (length(args) >= 1) args[[1]] else "params.json"
output_dir <- if (length(args) >= 2) args[[2]] else "output"

if (!file.exists(params_path)) {
	stop(sprintf("参数文件不存在: %s", params_path))
}

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

params <- jsonlite::fromJSON(params_path, simplifyVector = FALSE)
input_files <- params$input_files
merged_output_filename <- normalize_output_filename(
	params$merged_output_filename %||% params$merged_output_name
)

if (is.null(input_files) || length(input_files) == 0) {
	stop(sprintf("参数 input_files 为空"))
}

inputs <- lapply(seq_along(input_files), function(i) {
	read_input_matrix(input_files[[i]], i)
})

sample_report <- build_sample_consistency_report(inputs)
merge_res <- merge_inputs_by_feature(inputs)
merged_df <- merge_res$merged
conflicts_df <- merge_res$conflicts
feature_missing_report <- build_feature_missing_report(merged_df)

merged_output_path <- file.path(output_dir, merged_output_filename)
sample_report_path <- file.path(output_dir, "sample_vars_consistency_report.tsv")
conflict_report_path <- file.path(output_dir, "sample_value_conflicts.tsv")
feature_missing_report_path <- file.path(output_dir, "feature_var_missing_report.tsv")

readr::write_tsv(merged_df, merged_output_path, na = "")
readr::write_tsv(sample_report, sample_report_path, na = "")
readr::write_tsv(conflicts_df, conflict_report_path, na = "")
readr::write_tsv(feature_missing_report, feature_missing_report_path, na = "")

feature_cols <- unique(vapply(inputs, function(x) x$feature_col, character(1)))
all_samples <- sort(unique(unlist(lapply(inputs, function(x) x$sample_cols), use.names = FALSE)))
dup_feature_total <- sum(vapply(inputs, function(x) x$dup_feature_count, integer(1)))
inconsistent_count <- sum(!sample_report$consistent_with_first)
feature_missing_count <- nrow(feature_missing_report)
max_missing_sample_count <- if (feature_missing_count == 0) 0 else max(feature_missing_report$missing_sample_count)

info_lines <- c(
	"# Analysis Output",
	"",
	"## Run Info",
	sprintf("- run_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
	sprintf("- params_path: %s", params_path),
	sprintf("- output_path: %s", output_dir),
	"",
	"## Input Summary",
	sprintf("- input_file_count: %d", length(inputs)),
	sprintf("- feature_var_candidates: %s", format_vector_for_info(feature_cols)),
	sprintf("- all_sample_vars_count: %d", length(all_samples)),
	sprintf("- all_sample_vars: %s", format_vector_for_info(all_samples)),
	"",
	"## Merge Stats",
	sprintf("- merged_feature_count: %d", nrow(merged_df)),
	sprintf("- merged_sample_var_count: %d", ncol(merged_df) - 1),
	sprintf("- feature_var_with_missing_samples_count: %d", feature_missing_count),
	sprintf("- max_missing_samples_for_single_feature_var: %d", max_missing_sample_count),
	sprintf("- duplicated_feature_rows_removed: %d", dup_feature_total),
	sprintf("- sample_vars_inconsistent_file_count: %d", inconsistent_count),
	sprintf("- sample_value_conflict_count: %d", nrow(conflicts_df)),
	"",
	"## Output Files",
	sprintf("- merged_table_filename: %s", merged_output_filename),
	sprintf("- merged_table: %s", merged_output_path),
	sprintf("- sample_vars_report: %s", sample_report_path),
	sprintf("- sample_value_conflicts: %s", conflict_report_path),
	sprintf("- feature_var_missing_report: %s", feature_missing_report_path)
)

if (inconsistent_count > 0) {
	inconsistent_inputs <- sample_report %>%
		dplyr::filter(!.data$consistent_with_first) %>%
		dplyr::pull(.data$input_name)
	info_lines <- c(
		info_lines,
		"",
		"## Consistency Warning",
		sprintf("- inconsistent_inputs: %s", format_vector_for_info(inconsistent_inputs)),
		sprintf("- details_file: %s", sample_report_path)
	)
}

if (nrow(conflicts_df) > 0) {
	info_lines <- c(
		info_lines,
		"",
		"## Value Conflict Warning",
		sprintf("- conflict_file: %s", conflict_report_path),
		sprintf("- first_conflict_feature: %s", conflicts_df$feature_var[[1]] %||% "NA"),
		sprintf("- first_conflict_sample: %s", conflicts_df$sample_var[[1]] %||% "NA"),
		sprintf("- first_conflict_left_value: %s", format_optional_number(conflicts_df$left_value[[1]] %||% NA_real_)),
		sprintf("- first_conflict_right_value: %s", format_optional_number(conflicts_df$right_value[[1]] %||% NA_real_))
	)
}

readr::write_lines(info_lines, file.path(output_dir, "output.md"))
message(sprintf("Merged output saved to: %s", merged_output_path))