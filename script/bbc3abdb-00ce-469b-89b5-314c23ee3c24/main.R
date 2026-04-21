# install.packages("mediation")
library(mediation)
library(tidyverse)

params <- jsonlite::fromJSON("params.json")


# logistic_formula <- as.formula(str_glue( "{outcome_backticked}  ~ {vars_backticked}"))
independent_variable <- params$mediation_file$independent_variable$columns_name  |> trimws()
mediator <- params$mediation_file$mediator$columns_name |> trimws()
outcome <- params$mediation_file$outcome$columns_name |> trimws()



is_continuous <- function(x, max_levels = 5) {
  is.numeric(x) &&
    length(unique(x[!is.na(x)])) > max_levels
}

handle_missing_continuous <- function(
    X,
    strategy = c("median", "mean", "complete_case"),
    max_levels = 5
) {
  strategy <- match.arg(strategy)
  
  X_select <- X
  
  na_cols <- names(X_select)[colSums(is.na(X_select)) > 0]
  message(strategy)
  if (strategy == "complete_case") {
    X <- X_select|> na.omit()
    return(X)
  }
  
  continuous_na_cols = c()
  # 只处理连续变量
  for (col in na_cols) {
    x <- X[[col]]
    
    if (is_continuous(x, max_levels)) {
      continuous_na_cols <- c(continuous_na_cols,col )
      if (strategy == "median") {
        X[[col]][is.na(x)] <- median(x, na.rm = TRUE)
      }
      if (strategy == "mean") {
        X[[col]][is.na(x)] <- mean(x, na.rm = TRUE)
      }
    }
  }
  na_cols_str <- paste0(na_cols,collapse = ", ")
  continuous_na_cols_str <- paste0(continuous_na_cols,collapse = ", ")
  sink(file = str_glue("output/missing_handling.txt"))
  str_glue("na_cols: {na_cols_str}") |>print()
  str_glue("continuous_na_cols: {continuous_na_cols_str} with {strategy}") |>print()
  sink()
  
  X
}



data <- read_tsv(params$mediation_file$content) |>
  select(X = all_of(independent_variable), M =all_of(mediator), Y=all_of(outcome) ) 




data <- handle_missing_continuous(data, params$numeric_missing)

data[["Y"]] <- ifelse(data[["Y"]]  ==1, 1, 0)

# model.m <- lm(M ~ X, data = data)
model.m <- glm(M ~ X, data = data, family ="gaussian")

# model.y <- lm(Y ~ X + M, data = data)
model.y <- glm(Y ~ X + M, data = data, family = "binomial")



med.out <- mediate(model.m, model.y,
                   treat = "X",
                   mediator = "M",
                   boot = TRUE,
                   sims = 1000)
# 
# 
# model.m <- lm(
#   as.formula(paste(mediator, "~", independent_variable)),
#   data = data
# )
# 
# model.y <- lm(
#   as.formula(paste(outcome, "~", independent_variable, "+", mediator)),
#   data = data
# )

summary(med.out) 

extract_path_stats <- function(model, term) {
  coef_table <- summary(model)$coefficients
  if (!(term %in% rownames(coef_table))) {
    return(list(estimate = NA_real_, ci = c(NA_real_, NA_real_), p_value = NA_real_))
  }

  p_col <- grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]
  estimate <- unname(coef_table[term, "Estimate"])
  p_value <- if (!is.na(p_col)) unname(coef_table[term, p_col]) else NA_real_

  ci <- tryCatch(
    stats::confint.default(model, parm = term),
    error = function(e) c(NA_real_, NA_real_)
  )
  if (is.matrix(ci)) {
    ci <- ci[1, ]
  }

  list(estimate = estimate, ci = as.numeric(ci), p_value = p_value)
}

a_path <- extract_path_stats(model.m, "X")
b_path <- extract_path_stats(model.y, "M")

med_df <- data.frame(
  Effect = c("a path (X->M)", "b path (M->Y)", "ACME", "ADE", "Total Effect", "Proportion Mediated"),
  Estimate = c(a_path$estimate,
               b_path$estimate,
               med.out$d0,
               med.out$z0,
               med.out$tau.coef,
               med.out$n0),
  CI.Lower = c(a_path$ci[1],
               b_path$ci[1],
               med.out$d0.ci[1],
               med.out$z0.ci[1],
               med.out$tau.ci[1],
               med.out$n0.ci[1]),
  CI.Upper = c(a_path$ci[2],
               b_path$ci[2],
               med.out$d0.ci[2],
               med.out$z0.ci[2],
               med.out$tau.ci[2],
               med.out$n0.ci[2]),
  p.value = c(a_path$p_value,
              b_path$p_value,
              med.out$d0.p,
              med.out$z0.p,
              med.out$tau.p,
              NA)  # 中介比例没有 p 值
)
write_tsv(med_df,file = str_glue("output/mediation.tsv"))
write_tsv(med_df,file = str_glue("output/prompt.ai"))
pdf(file = str_glue("output/mediation_plot.pdf") )
plot(med.out)
dev.off()

