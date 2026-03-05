library(tidyverse)
library(gtsummary)
library(ggrepel)


params <- jsonlite::fromJSON("params.json")

independent_variable <- params$input$independent_variable$columns_name 
independent_variable <- trimws(independent_variable)
log_transformed <- params$input$log_transformed$columns_name |> trimws()


filter_sample <- c()

if(!is.null(params$filter_sample) && params$filter_sample!=""){
  filter_sample <- str_split(params$filter_sample,",")[[1]] |>trimws()
  message(params$filter_sample)
}
  
outcome <- params$input$outcome$columns_name 

sample_columns_name <-  params$input$sample$columns_name


filename <- basename(params$input$content)

df <- read_tsv(params$input$content) %>%
  filter(!.data[[sample_columns_name]] %in% filter_sample) 

if(!is.null(log_transformed) ){
  df <- df |>mutate(across(all_of(log_transformed), log1p))
  message("log_transformed:", paste0(log_transformed,collapse = ", "))
}


# if(!"outcome" %in% colnames(df)){
#   df <- df |>
#     mutate(outcome = ifelse(df[[outcome]] == "SI", 1, 0))
#   message("modify outcome!")
# }


contStatMethod <- params$contStatMethod


# X <- as.matrix(df[, independent_variable])
# y <- df$outcome

# 4. 描述性统计分析
# 按suicide分组的基本统计
# df |>
#   select(all_of(independent_variable),all_of(outcome)) |>
#   tbl_summary(
#     by = suicide,
#     statistic = list(
#       all_continuous() ~ "{mean} ({sd})",
#       all_categorical() ~ "{n} ({p}%)"
#     ),
#     missing = "no"
#   ) %>%
#   add_n() %>%
#   add_p(
#     test = list(
#       all_continuous() ~ "oneway.test",         # F test
#       all_categorical() ~ "chisq.test"  # χ² test
#     ),
#   ) |>
#   add_stat(
#     test.statistic ~ "{statistic}",     # F 或 χ²
#   ) |>
#   bold_labels() |>
#   add_stat_label(
#     test.statistic ~ "**F / χ²**",
#     test.df ~ "**df**"
#   ) 



# 连续变量
# my_cont_test <- function(data, variable, by, ...) {
#   t.test(data[[variable]] ~ as.factor(data[[by]])) %>%
#     broom::tidy() %>%
#     select(statistic, p.value)
# }

# 分类变量
# my_cat_test <- function(data, variable, by, ...) {
#   tbl <- table(data[[variable]], data[[by]])
#   # 用 chisq.test，如果期望频数 <5可改 fisher.test
#   test <- chisq.test(tbl)
#   tibble(statistic = unname(test$statistic),
#          p.value = test$p.value)
# }


# for Continuous Variable Method
# my_con_test <- NULL
# if(contStatMethod =="nonparam"){
#   my_con_test <- function(data, variable, by, ...) {
#     res <- wilcox.test(data[[variable]] ~ as.factor(data[[by]]))
#     # message(variable)
#     tibble(statistic = res$statistic, p.value = res$p.value)
#   }
#   message("usage nonparam!")
# }else{
#   my_con_test <- function(data, variable, by, ...) {
#     vars_backticked <- paste0("`", variable, "`") |>paste0(collapse = " + ") 
#     
#     # f <- as.formula(paste(variable, "~", by))
#     f <- as.formula(str_glue( "{vars_backticked} ~ {by}"))
#     
#     print(f)
#     # oneway.test 默认是 Welch’s ANOVA，不要求方差齐性
#     res <- oneway.test(f, data = data, var.equal = FALSE)
#     
#     tibble(
#       statistic = unname(res$statistic),
#       p.value = res$p.value
#     )
#   }
#   
# }
# 



tbl <- df |>
  select(all_of(independent_variable),all_of(outcome)) 

if(!is.null(params$categorical)){
  categorical <- str_split(params$categorical,",")[[1]]
  tbl <- tbl |>
    mutate(across(any_of(categorical), as.factor))
  message(params$categorical)
}

# 
# my_con_test2 <- function(data, variable, by, ...) {
#   
#   x <- data[[variable]]
#   g <- data[[by]]
#   
#   # Wilcoxon
#   p1 <- wilcox.test(x ~ g)$p.value
#   
#   # One-way ANOVA
#   p2 <- oneway.test(x ~ g)$p.value
#   
#   tibble::tibble(
#     `Wilcoxon P` = p1,
#     `ANOVA P` = p2
#   )
# }
# 
# my_wilcox <- function(data, variable, by, ...) {
#   tibble::tibble(
#     `Wilcoxon P` = wilcox.test(data[[variable]] ~ data[[by]])$p.value
#   )
# }
# 
# my_anova <- function(data, variable, by, ...) {
#   tibble::tibble(
#     `ANOVA P` = oneway.test(data[[variable]] ~ data[[by]])$p.value
#   )
# }
#   # mutate(METS =  factor(METS)) |> 


my_cont_tests <- function(data, variable, by, ...) {
  
  x <- data[[variable]]
  g <- data[[by]]
  
  # 去除缺失
  df <- data.frame(x = x, g = g)
  df <- na.omit(df)
  
  n_group <- length(unique(df$g))
  
  if (n_group == 2) {
    # 2 组
    w_test <- wilcox.test(x ~ g, data = df)
    a_test <- oneway.test(x ~ g, data = df)
    
    tibble::tibble(
      `Wilcox W` = round(unname(w_test$statistic), 4),
      `Wilcox P` = round(w_test$p.value, 4),
      `ANOVA F`  = round(unname(a_test$statistic), 4),
      `ANOVA P`  = round(a_test$p.value, 4)
    )
    
  } else {
    # ≥3 组
    k_test <- kruskal.test(x ~ g, data = df)
    a_test <- oneway.test(x ~ g, data = df)
    
    tibble::tibble(
      `Kruskal χ²` = round(unname(k_test$statistic), 4),
      `Kruskal P`  = round(k_test$p.value, 4),
      `ANOVA F`    = round(unname(a_test$statistic), 4),
      `ANOVA P`    = round(a_test$p.value, 4)
    )
  }
}
my_cat_tests <- function(data, variable, by, ...) {
  
  x <- data[[variable]]
  g <- data[[by]]
  
  df <- data.frame(x = x, g = g)
  df <- na.omit(df)
  
  tab <- table(df$x, df$g)
  
  chi <- chisq.test(tab)
  fisher_p <- fisher.test(tab)$p.value
  
  tibble::tibble(
    `Chi-sq χ²` = round(unname(chi$statistic), 4),
    `Chi-sq P`  = round(chi$p.value, 4),
    `Fisher P`  = round(fisher_p, 4)
  )
}

add_fdr_for_all_tests <- function(tbl_obj) {
  p_cols <- tbl_obj$table_styling$header |>
    dplyr::filter(stringr::str_detect(label, " P$")) |>
    dplyr::pull(column)

  if (length(p_cols) == 0) {
    return(tbl_obj)
  }

  fdr_cols <- paste0(p_cols, "_fdr")

  tbl_obj |>
    modify_table_body(
      ~ .x |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(p_cols),
            ~ round(stats::p.adjust(.x, method = "BH"), 4),
            .names = "{.col}_fdr"
          )
        )
    ) |>
    modify_header(
      !!!stats::setNames(
        as.list(stringr::str_replace(tbl_obj$table_styling$header$label[match(p_cols, tbl_obj$table_styling$header$column)], " P$", " FDR")),
        fdr_cols
      )
    ) |>
    modify_column_unhide(columns = dplyr::all_of(fdr_cols))
}

# format_p_sci <- function(x) {
#   dplyr::if_else(
#     is.na(x),
#     NA_character_,
#     format(x, scientific = TRUE, digits = 3)
#   )
# }

tbl <- tbl |>
  tbl_summary(
    by = outcome,
    type = list(
      c("education years")~ "continuous"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ± {sd}"
    ),
    digits = list(
      all_continuous() ~ c(2, 2),
      all_categorical() ~ c(0, 2)
    ),
    missing = "always"
    
  ) %>%
  add_n() %>%
  add_stat(
    fns = c(
      all_continuous() ~ my_cont_tests,
      all_categorical() ~ my_cat_tests
    )
  ) |>

  # add_stat(fns = all_continuous() ~ my_wilcox) %>%
  # add_stat(fns = all_continuous() ~ my_anova) %>%
  modify_header(statistic = "**F/X2**", p.value = "**p-value**") |>
  add_fdr_for_all_tests()
  # modify_fmt_fun(
  #   all_continuous() ~ function(x) sprintf("%.5f", x),
  #   all_categorical() ~ function(x) sprintf("%.5f", x)
  # )
# %>%
#   modify_fmt_fun(
#     dplyr::matches("P$") ~ format_p_sci
#   )
  # modify_fmt_fun(
  #   # 匹配所有列名里含 "P" 的列
  #   dplyr::matches("P$") ~ format_p_sci
  # ) 




clean_colnames <- function(x) {
  x |>
    stringr::str_replace_all("χ²", "chi2") |>
    stringr::str_replace_all("±", "plus_minus") |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^A-Za-z0-9_]", "") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_replace_all("^_|_$", "") |>
    tolower()
}

res  <- tbl %>%
  as_tibble(col_labels = TRUE) 
colnames(res) <- clean_colnames(colnames(res))

# colnames(res) <- make.names(colnames(res))
write_tsv(res, str_glue("output/{filename}_.tsv"))



tbl
tbl |> 
  as_gt() |> 
  gt::gtsave(filename = "output/statistic.html") 
tbl |> 
  as_gt() |> 
  gt::gtsave(filename = "output/statistic.tex")
file.rename("output/statistic.tex", "output/prompt.ai")

# df <- as_tibble(tbl) |>
#   write_tsv(file = str_glue("output/table.tsv"))


# summary(df$Age)


# df_test <- df |>
#   select(c(`IL-2`,"outcome")) |>
#   na.omit()
# oneway.test( as.formula(str_glue( "`IL-2` ~ outcome")) , data = df_test, var.equal = FALSE)
# 
# 



# 
# plot_data <- df |>
#   select(all_of(sample_columns_name),all_of(independent_variable),all_of(outcome))
# 
# df_long <- plot_data %>%
#   pivot_longer(cols =-c("Code",outcome), names_to = "item", values_to = "score")
# 


# 
# outliers <- df_long %>%
#   group_by(item, outcome) %>%
#   mutate(
#     Q1 = quantile(score, 0.25),
#     Q3 = quantile(score, 0.75),
#     IQR = Q3 - Q1,
#     is_outlier = score < (Q1 - 1.5*IQR) | score > (Q3 + 1.5*IQR)
#   ) %>%
#   filter(is_outlier)
# 
# ggplot(df_long, aes(x = item, y = score, fill = factor(outcome))) +
#   geom_boxplot(outlier.shape = NA) +  # 隐藏默认离群点
#   geom_jitter(width = 0.2, alpha = 0.5) +  # 显示所有点
#   theme_minimal() +
#   geom_text_repel(
#     data = outliers,
#     aes(label = Code),
#     size = 3,
#     nudge_x = 0.2
#   ) +
#   labs(x = "问卷条目", y = "评分", fill = "Outcome") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# ggplot(plot_data, aes(x = PSQI7总)) +
#   geom_histogram(binwidth = 2, position = "dodge", color = "black") +
#   labs(x = "PSQI7总分", y = "频数", fill = "Outcome") +
#   theme_minimal()


# df_filter <- df |>
#   select(Code,all_of(independent_variable),all_of(outcome)) 
# 
# df_filter_zscore <-   df_filter |>
#   group_by(outcome) %>%
#   mutate(
#     z_score = (`PSQI7总` - mean(`PSQI7总`)) / sd(`PSQI7总`)
#   ) %>%
#   ungroup()
# 
# # 绝对值大于 2 或 3 的视为离群
# outliers <- df_filter_zscore %>% filter(abs(z_score) > 1.5)
# outliers
# 
# outliers_iqr <- df %>%
#   group_by(outcome) %>%
#   mutate(
#     Q1 = quantile(`PSQI7总`, 0.25),
#     Q3 = quantile(`PSQI7总`, 0.75),
#     IQR = Q3 - Q1,
#     lower = Q1 - 1.5*IQR,
#     upper = Q3 + 1.5*IQR
#   ) %>%
#   filter(`PSQI7总` < lower | `PSQI7总` > upper) %>%
#   select(Code, `PSQI7总`, outcome)
# 
# outliers_iqr
# 
# 
# 
# ggplot(df_filter_zscore, aes(x = outcome, y = `PSQI7总`, label = Code)) +
#   geom_boxplot() +
#   geom_text(data = df_filter_zscore %>% filter(abs(z_score) > 2), 
#             aes(label = Code), color = "red", vjust = -0.5)


# chisq.test(table(df$性别, df$suicide))
# oneway.test(as.formula("WBC白细胞计数 ~ suicide"), data = df, var.equal = FALSE)
# 

# wilcox.test(scale(df[["GLU葡萄糖"]],center = T,scale = T) ~ df$suicide)
# scale(df[["年龄"]],center = T,scale = T)

# 5. 可视化分析
# 年龄分布箱线图
# ggplot(df, aes(x = suicide, y = `年龄`, fill = suicide)) +
#   geom_boxplot() +
#   labs(title = "Suicidality and age distribution", x = "suicidal tendencies", y = "age") +
#   theme_minimal()
# 
# # 性别分布条形图
# ggplot(df, aes(x = `性别`, fill = suicide)) +
#   geom_bar(position = "fill") +
#   labs(title = "Suicide tendency and gender distribution", x = "gender", y = "proportion") +
#   theme_minimal()
# 
# # 6. 统计检验
# # 年龄与suicide的t检验
# t_test_age <- t.test(`年龄` ~ suicide, data = df)
# print(t_test_age)
# 
# 
# # 性别与suicide的卡方检验
# chi_test_gender <- chisq.test(table(df$性别, df$suicide))
# print(chi_test_gender)
# 
# 
# # 7. 逻辑回归分析
# # 单变量逻辑回归
# model_age <- glm(outcome ~ `年龄`, data = df, family = binomial)
# summary(model_age)
# 
# model_gender <- glm(outcome ~ 性别, data = df, family = binomial)
# summary(model_gender)
# 
# 
# # 多变量逻辑回归
# model_full <- glm(outcome ~ `年龄` + `性别`, data = df, family = binomial)
# summary(model_full)

# 8. 计算OR值（优势比）和95%置信区间
# exp(cbind(OR = coef(model_full), confint(model_full)))




# library(pwr)
# pwr.anova.test( k = 5, f = 0.25, sig.level = 0.05, power = 0.80 )
# pwr.anova.test( k = 58, f = 0.25, sig.level = 0.05, power = 0.80 )
# 
# power.t.test(
#   delta = 0.5,   # Cohen's d
#   sd = 1,
#   power = 0.8,
#   sig.level = 0.05
# )
# 
# 
# power.t.test(
#   delta = 2,   # Cohen's d
#   sd = 4,
#   power = 0.8,
#   sig.level = 0.05,
#   alternative="two.sided",
#   type = "two.sample"
# )
# 
# power.t.test(
#   delta = 2.5,        # PSQI 均值差
#   sd = 4.0,           # PSQI 标准差
#   sig.level = 0.05,   # α = 5%
#   power = 0.80,       # 80% power
#   type = "two.sample",
#   alternative = "two.sided"
# )

