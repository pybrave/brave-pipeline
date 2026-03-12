#!/usr/bin/env Rscript

# Lightweight integration tests for main.R without requiring a full test framework.
# Run from repository root:
# Rscript tests/run_main_tests.R

required_pkgs <- c("jsonlite", "readr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_pkgs) > 0) {
  stop(sprintf("Missing required packages for tests: %s", paste(missing_pkgs, collapse = ", ")))
}

script_path <- normalizePath("/opt/brave_prod/workspace/pipeline/script/f30a8a01-277e-4bf9-9a24-0f87bcf28d37/main.R", mustWork = TRUE)

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

assert_all_close <- function(x, y, tol = 1e-8, msg = "Values are not close enough") {
  if (length(x) != length(y)) {
    stop(sprintf("%s (length mismatch %d vs %d)", msg, length(x), length(y)), call. = FALSE)
  }
  ok <- all(abs(x - y) <= tol | (is.na(x) & is.na(y)))
  if (!ok) {
    stop(msg, call. = FALSE)
  }
}

build_input_df <- function() {
  data.frame(
    Row.names = c("F1", "F2", "F3", "F4"),
    Panel = c("A", "A", "B", "B"),
    g1_s1 = c(10, 8, 7, 6),
    g1_s2 = c(11, 9, 8, 7),
    g2_s1 = c(2, 2, 5, 5),
    g2_s2 = c(3, 3, 6, 4),
    P_value = c(0.9, 0.8, 0.7, 0.6),
    Qvalue = c(0.91, 0.81, 0.71, 0.61),
    stringsAsFactors = FALSE
  )
}

build_params <- function(data_path, sig_mode, qvalue_method = "BH") {
  list(
    input_file = list(
      content = data_path,
      x_var = list(columns_name = "Row.names"),
      panel_var = list(columns_name = "Panel"),
      group1_vars = list(
        list(columns_name = "g1_s1"),
        list(columns_name = "g1_s2")
      ),
      group2_vars = list(
        list(columns_name = "g2_s1"),
        list(columns_name = "g2_s2")
      ),
      p_col = list(columns_name = "P_value"),
      q_col = list(columns_name = "Qvalue")
    ),
    sig_mode = sig_mode,
    qvalue_method = qvalue_method,
    panel_type = "none",
    plot_type = "scatter",
    show_stats = FALSE,
    output_name = "test_plot"
  )
}

run_main_with_params <- function(params_obj, input_df) {
  td <- tempfile("plot_test_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  data_path <- file.path(td, "input.tsv")
  params_path <- file.path(td, "params.json")
  out_dir <- file.path(td, "output")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  readr::write_tsv(input_df, data_path)

  params_obj$input_file$content <- data_path
  jsonlite::write_json(params_obj, params_path, auto_unbox = TRUE, pretty = TRUE)

  setwd(td)
  env <- new.env(parent = globalenv())
  source(script_path, local = env)
  env
}

expected_row_p <- function(df, method) {
  vapply(seq_len(nrow(df)), function(i) {
    x <- as.numeric(df[i, c("g1_s1", "g1_s2")])
    y <- as.numeric(df[i, c("g2_s1", "g2_s2")])
    if (method == "wilcox") {
      stats::wilcox.test(x, y, exact = FALSE)$p.value
    } else {
      stats::t.test(x, y)$p.value
    }
  }, numeric(1))
}

test_exist_mode_keeps_existing <- function() {
  input_df <- build_input_df()
  params <- build_params(data_path = "", sig_mode = "exist", qvalue_method = "BH")
  env <- run_main_with_params(params, input_df)

  assert_true("df" %in% ls(env), "main.R should create df in execution environment")
  got <- env$df
  assert_all_close(got$P_value, input_df$P_value, msg = "exist mode should keep original P_value")
  assert_all_close(got$Qvalue, input_df$Qvalue, msg = "exist mode should keep original Qvalue")
}

test_ttest_none_sets_q_equal_p <- function() {
  input_df <- build_input_df()
  params <- build_params(data_path = "", sig_mode = "t-test", qvalue_method = "none")
  env <- run_main_with_params(params, input_df)
  got <- env$df

  expected_p <- expected_row_p(input_df, method = "t-test")
  assert_all_close(got$P_value, expected_p, tol = 1e-8, msg = "t-test P_value mismatch")
  assert_all_close(got$Qvalue, expected_p, tol = 1e-8, msg = "qvalue_method=none should set Qvalue=P_value")
}

test_wilcox_bh_adjusts_q <- function() {
  input_df <- build_input_df()
  params <- build_params(data_path = "", sig_mode = "wilcox", qvalue_method = "BH")
  env <- run_main_with_params(params, input_df)
  got <- env$df

  expected_p <- expected_row_p(input_df, method = "wilcox")
  expected_q <- p.adjust(expected_p, method = "BH")

  assert_all_close(got$P_value, expected_p, tol = 1e-8, msg = "wilcox P_value mismatch")
  assert_all_close(got$Qvalue, expected_q, tol = 1e-8, msg = "BH-adjusted Qvalue mismatch")
}

run_all_tests <- function() {
  tests <- list(
    test_exist_mode_keeps_existing,
    test_ttest_none_sets_q_equal_p,
    test_wilcox_bh_adjusts_q
  )
  test_names <- c(
    "test_exist_mode_keeps_existing",
    "test_ttest_none_sets_q_equal_p",
    "test_wilcox_bh_adjusts_q"
  )

  for (i in seq_along(tests)) {
    test_fun <- tests[[i]]
    test_name <- test_names[[i]]
    test_fun()
    message(sprintf("[PASS] %s", test_name))
  }

  message("All tests passed")
}

run_all_tests()
