library(tidyverse)
library(glmnet)

set.seed(2025)

params <- jsonlite::fromJSON("params.json")

# method: "lasso" / "ridge" / "elasticnet"
# alpha_en: Elastic Net alpha，只有 method="elasticnet" 时生效
# standardize: 是否标准化
# family: binomial / gaussian

method  <- params$method
alpha_en <-  params$alpha_en  #0.5
standardize = params$standardize
family <-  params$family #"gaussian"

numeric_missing <- params$numeric_missing


is_continuous <- function(x, max_levels = 5) {
  is.numeric(x) &&
    length(unique(x[!is.na(x)])) > max_levels
}


handle_missing_continuous <- function(
    X,
    independent_variable,
    outcome,
    strategy = c("median", "mean", "complete_case"),
    max_levels = 5
) {
  strategy <- match.arg(strategy)
  
  X_select <- X[,independent_variable]
  # 找出有缺失值的列
  na_cols <- names(X_select)[colSums(is.na(X_select)) > 0]
  message(strategy)
  # 如果选择 complete case：直接删行
  if (strategy == "complete_case") {
    # idx <- complete.cases(X)
    X <- X|> drop_na(all_of(c(independent_variable,outcome)))
    return(X)
    # return(X[idx, , drop = FALSE])
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


df <- read_tsv(params$regression$content)
independent_variable <- params$regression$independent_variable$columns_name  |>trimws()
outcome <- params$regression$outcome$columns_name |>trimws()
df <- df |>
  select(c(all_of(outcome),all_of(independent_variable))) %>%
  filter(!is.na(.data[[outcome]]))

df <- handle_missing_continuous(df, independent_variable,outcome,numeric_missing)

X <- df[, independent_variable] |>as.matrix()
y <-  df[[outcome]]

# median_y <- median(y)

y <- ifelse(y > 10, 1, 0)

# df_x <- df[, independent_variable, drop = FALSE]
# 
# # 2. 删除 0 方差列
# nzv <- apply(df_x, 2, sd, na.rm = TRUE) > 0
# df_x <- df_x[, nzv, drop = FALSE]
# # 3. makeX 只负责 NA（均值填补）
# X <- makeX(df_x)

# X <- as.matrix(df[, independent_variable])
# y <- df[[outcome]]


alpha <- switch(method,
                "lasso" = 1,
                "ridge" = 0,
                "elasticnet" = alpha_en)

cv_model <- cv.glmnet(X, y, alpha = alpha, family = family, standardize = standardize)



pdf(file = str_glue("output/cv_model.pdf") )
plot(cv_model)
dev.off()


sink(file = str_glue("output/cv_model.txt"))
print(cv_model)
sink()


best_lambda <- cv_model$lambda.min

coef_df <- as.data.frame(as.matrix(coef(cv_model, s = "lambda.min")))
names(coef_df) <- "Coefficient"
coef_df$Variable <- rownames(coef_df)
rownames(coef_df) <- NULL

if (family == "binomial") {
  coef_df$OR <- exp(coef_df$Coefficient)
}

write_tsv(coef_df,file = str_glue("output/coef_df.tsv"))
write_tsv(coef_df,file = str_glue("output/prompt.ai"))






