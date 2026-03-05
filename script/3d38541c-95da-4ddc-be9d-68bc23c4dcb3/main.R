library(tidyverse)
params <- jsonlite::fromJSON("params.json")



independent_variable <- params$logistic$independent_variable$columns_name  |>trimws()





vars_backticked <- paste0("`", independent_variable, "`") |>paste0(collapse = " + ") 



interaction_1 <- params$logistic$interaction_1$columns_name|>trimws()
interaction_2 <- params$logistic$interaction_2$columns_name|>trimws()
if(!is.null(interaction_1)  && !is.null(interaction_2) &&  length(interaction_1)!=0 &&  length(interaction_2)!=0){
  interaction_1_backticked <- paste0("`",interaction_1,"`")
  interaction_2_backticked <- paste0("`",interaction_2,"`")
  interaction <- str_glue("{interaction_1_backticked}:{interaction_2_backticked}")
  vars_backticked <- str_glue("{vars_backticked} + {interaction}")
  
}

if(length(interaction_2)){
  
}



outcome <- params$logistic$outcome$columns_name  |> trimws()
outcome_backticked <- paste0("`", outcome, "`")
family <- params$family

# y ~ x1 + x2
# logistic_formula <- as.formula(str_glue("{outcome} ~ {vars_backticked}"))
# df[,`sleep latency`]
# colnames(df)[grepl("sleep",colnames(df))]
# 
# a <- df[,"sleep latency"]

# df <- df |>
#   mutate(outcome = ifelse(df[[outcome]] == "SI", 1, 0)) 

filter_sample <- c()
if(!is.null(params$filter_sample) && params$filter_sample!=""){
  filter_sample <- str_split(params$filter_sample,",")[[1]] |>trimws()
  message(params$filter_sample)
}

sample_columns_name <-  params$logistic$sample$columns_name

df <- read_tsv(params$logistic$content) %>%
  filter(!.data[[sample_columns_name]] %in% filter_sample) 

# a <- df[,c(outcome, independent_variable)]
# a[, independent_variable] <- scale(a[, independent_variable])
df <- df |>
  select(c(all_of(outcome),all_of(independent_variable) )) %>%
  filter(!is.na(.data[[outcome]]))


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
  
  X_select <- X[,c(independent_variable,outcome)]
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

  
  X
}

# df |>
#   select(all_of(outcome))

df_1 <- handle_missing_continuous(df, independent_variable,outcome,numeric_missing) 
# split_y <- mean(df_1[[outcome]])
# split_y <- median(df_1[[outcome]])


if(family=="binomial"){
  df_1[[outcome]] <- ifelse(df_1[[outcome]]  == params$risk_factor_value , 1, 0)
  message("process outcome!")
}

# df <- read_tsv(params$logistic$content) 
# df[["outcome"]] <-  ifelse(df[[outcome]]  ==1, 1, 0)


sink(file = str_glue("output/missing_handling.txt"))
str_glue("missing strategy: {numeric_missing}") |>print()
str_glue("before nrow: {nrow(df)}") |>print()
str_glue("after nrow: {nrow(df_1)}") |>print()
sink()


# df_1 <-handle_missing_continuous(df,independent_variable)




logistic_formula <- as.formula(str_glue( "{outcome_backticked}  ~ {vars_backticked} "))

model <- glm(logistic_formula, data = df_1, family = family)



OR <- exp(coef(model))
OR
CI <- exp(confint.default(model))
CI
p_values <- summary(model)$coefficients[,4]

# 整理成表格
result <- data.frame(
  Variable = names(OR),
  OR = OR,
  Lower95CI = CI[,1],
  Upper95CI = CI[,2],
  Pvalue = p_values
)

write_tsv(result, file = str_glue("output/logistic_res.tsv"))


sink(file = str_glue("output/summary.txt"))
summary(model)
sink()

sink(file = str_glue("output/prompt.ai"))
summary(model)
print(result)
sink()

