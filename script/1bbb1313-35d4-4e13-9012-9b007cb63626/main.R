library(tidyverse)
library(jsonlite)
library(Hmisc)
library(showtext)

showtext_auto()
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


params <- fromJSON("params.json")

df <- read_tsv(params$input$content)

x_axis <- params$input$x_axis$columns_name
y_axis <- params$input$y_axis$columns_name

df <- df %>% mutate(
  x = log10(.data[[x_axis]] + 1),  # +1 防止为0
  y = log10(.data[[y_axis]] + 1)
) |>
  select(x,y)
df <- handle_missing_continuous(df, "mean")

# cor(df$x, df_log$y, method = "pearson")

# fit <- lm(y ~ x, data = df)
# r2_value <- summary(fit)$r.squared
# r2_label <- sprintf("R² = %.3f", r2_value)  # 保留 3 位小数，可改
# 计算 Spearman 相关系数和 p 值
cor_res <- cor.test(df$x, df$y, method = "spearman")
# cor(df$x, df$y, method = "spearman")
rho <- cor_res$estimate
pval <- cor_res$p.value
label <- sprintf("R= %.3f P= %.3g", rho, pval)

title <- paste0(c(x_axis, y_axis), collapse = "_")

df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, color = "#2C7BB6", alpha = 0.8) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "#D7191C",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = label,
    hjust = 1.1, vjust = 1.5,
    size = 5,
    fontface = "bold"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = 0.6, color = "black"),
    axis.ticks = element_line(linewidth = 0.4, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", linewidth = 0.8),
    legend.position = "none"
  ) +
  labs(
    x = x_axis,
    y = y_axis,
    title = title
  )
ggsave(filename = str_glue("output/{title}.pdf"))






