library(tidyverse)
library(ggplot2)
library(stats)
library(jsonlite)

# 读取参数
params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)
file_path <- params$input_file$content
n_components <- params$n_components
scale_data <- params$scale_data
plot_type <- params$plot_type
color_by <- params$color_by
custom_color_col <- params$custom_color_col
output_name <- params$output_name

# 读取输入数据
if (is.null(file_path)) {
  stop("Input file path is missing in params.json")
}
df <- readr::read_tsv(file_path, show_col_types = FALSE)

# 提取变量列
x_vars <- params$input_file$x_vars
if (length(x_vars) == 0) stop("No x_vars specified")

# 构建数值矩阵
mat <- df %>% select(all_of(x_vars)) %>% as.matrix()

# 数据标准化（可选）
if (scale_data) {
  mat <- scale(mat)
}

# PCA 分析
pca_result <- prcomp(mat, center = TRUE, scale. = scale_data, rank. = n_components)

# 主成分得分
scores_df <- as.data.frame(pca_result$x[, 1:n_components])
rownames(scores_df) <- rownames(mat)

# 添加分组信息（如果存在）
if (!is.null(params$input_file$group_vars) && length(params$input_file$group_vars) > 0) {
  group_info <- df %>% select(all_of(params$input_file$group_vars)) %>% 
    mutate(rowname = rownames(df)) %>% 
    pivot_longer(cols = -rowname, names_to = "group_var", values_to = "group_value") %>% 
    filter(!is.na(group_value))
  
  # 合并分组信息到 scores_df
  scores_df$rowname <- rownames(scores_df)
  scores_df <- left_join(scores_df, group_info, by = c("rowname" = "rowname"))
  
  if (color_by == "group") {
    color_var <- "group_value"
  } else if (color_by == "custom" && !is.null(custom_color_col) && custom_color_col != "") {
    if (custom_color_col %in% names(df)) {
      scores_df$custom_color <- df[[custom_color_col]]
      color_var <- "custom_color"
    } else {
      warning(paste("Custom color column '", custom_color_col, "' not found; using default color."))
      color_var <- NULL
    }
  } else {
    color_var <- NULL
  }
} else {
  color_var <- NULL
}

# 输出主成分得分表
scores_output_path <- file.path("output", paste0(output_name, "_scores.tsv"))
readr::write_tsv(scores_df, scores_output_path)
message(sprintf("PCA scores saved to: %s", scores_output_path))

# 绘图逻辑
if (plot_type == "scatter") {
  p <- ggplot(scores_df, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = !!sym(color_var)), size = 3) +
    labs(x = "PC1", y = "PC2", title = "PCA Scatter Plot") +
    theme_minimal()
  
  if (!is.null(color_var)) p <- p + scale_color_viridis_d(option = "plasma")
  
  ggsave(filename = file.path("output", paste0(output_name, "_scatter.png")), plot = p, width = 10, height = 8, dpi = 300)
  
} else if (plot_type == "biplot") {
  # 简化 biplot：仅展示前两个主成分的载荷和得分
  loadings <- pca_result$rotation[, 1:2, drop = FALSE]
  loadings_df <- as.data.frame(loadings) %>%
    rownames_to_column("feature") %>%
    mutate(PC1 = PC1 * 2, PC2 = PC2 * 2)  # 缩放载荷便于可视化
  
  p <- ggplot() +
    geom_point(data = scores_df, aes(x = PC1, y = PC2), alpha = 0.6) +
    geom_segment(data = loadings_df,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.02, "npc")), color = "red") +
    geom_text(data = loadings_df, aes(x = PC1, y = PC2, label = feature),
              hjust = -0.1, vjust = 0.5, size = 3, color = "red") +
    labs(x = "PC1", y = "PC2", title = "PCA Biplot") +
    theme_minimal()
  
  ggsave(filename = file.path("output", paste0(output_name, "_biplot.png")), plot = p, width = 10, height = 8, dpi = 300)
  
} else if (plot_type == "scree") {
  # Scree plot
  eigenvals <- summary(pca_result)$importance[2, ]
  scree_df <- data.frame(
    Component = seq_along(eigenvals),
    Variance = eigenvals
  )
  
  p <- ggplot(scree_df, aes(x = Component, y = Variance)) +
    geom_line() + geom_point() +
    labs(x = "Principal Component", y = "Variance Explained", title = "Scree Plot") +
    theme_minimal()
  
  ggsave(filename = file.path("output", paste0(output_name, "_scree.png")), plot = p, width = 8, height = 6, dpi = 300)
}

message("PCA analysis completed successfully.")