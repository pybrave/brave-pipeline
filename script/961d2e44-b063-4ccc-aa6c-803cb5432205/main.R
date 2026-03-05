library(tidyverse)
library(jsonlite)
library(Hmisc)

params <- fromJSON("params.json")

x_feature <- params$x_file$feature$columns_name

x_file_metadata <- params$x_file$samples |>
  select(sample_name,group= selcted_group_name )

x_file <- read_tsv(params$x_file$content)|>
  select(all_of(x_feature), x_file_metadata$sample_name) |>
  column_to_rownames(x_feature) |>
  t() |> as.data.frame()



y_file_metadata <- params$y_file$samples |>
  select(sample_name,group= selcted_group_name )

y_file <- read_tsv(params$y_file$content)|>
  select(all_of(x_feature), y_file_metadata$sample_name) |>
  column_to_rownames(x_feature) |>
  t() |> as.data.frame()

intersect_feature_list <- str_split(params$query_feature,",")[[1]]
# feature_list_all <- intersect(colnames(x_file), colnames(y_file))
# intersect_feature_list <- intersect(feature_list, feature_list_all)

sample_list <- intersect(rownames(x_file), rownames(y_file))
x_file <- x_file[sample_list,]
y_file <- y_file[sample_list,]

x_name <- params$x_name
y_name <- params$y_name

feature <- intersect_feature_list[1]

point_plot <- function(feature){
  if(!grepl("//",feature)){
    stop("feature must contain //")
  }
  feature_list <- str_split(feature,"//")[[1]]
  # 手动计算 R²
  
  df <- tibble(
    x = x_file[, feature_list[1]],
    y = y_file[, feature_list[2]]
  )
  
  df <- df %>% mutate(
    x = log10(x + 1),  # +1 防止为0
    y = log10(y + 1)
  )
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
  
  title <- paste0(feature_list, collapse = "_")
  
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
      x = x_name,
      y = y_name,
      title = title
    )
  ggsave(filename = str_glue("output/{title}.pdf"))
}


lapply(intersect_feature_list, function(x){
  point_plot(x)
})








