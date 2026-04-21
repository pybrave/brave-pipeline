library(tidyverse)
library(ggplot2)
library(stats)

# 读取参数
params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)
file_path <- params$input_file$content
df <- readr::read_tsv(file_path, show_col_types = FALSE)

# 提取变量
x_vars <- params$input_file$x_vars
if (length(x_vars) == 0) stop("No x variables selected")

group_vars <- params$input_file$group_vars

# 构建数据矩阵
mat <- df[, unlist(x_vars), drop = FALSE]
rownames(mat) <- df[["Row.names"]]

# 数据标准化（可选）
if (params$scale_data) {
  mat <- scale(mat)
}

# PCA分析
pca_result <- prcomp(mat, center = TRUE, scale. = params$scale_data)

# 提取主成分得分
scores <- as.data.frame(pca_result$x)
scores$sample <- rownames(scores)

# 添加分组信息（如果提供）
if (length(group_vars) > 0 && !is.null(df[[group_vars[[1]]$columns_name]])) {
  scores$group <- df[[group_vars[[1]]$columns_name]]
}

# 输出主成分得分表
long_tsv_path <- file.path("output", paste0(params$output_name, "_scores.tsv"))
readr::write_tsv(scores, long_tsv_path)
message(sprintf("PCA scores saved to: %s", long_tsv_path))

# 绘图逻辑
n_comps <- min(2, params$n_components)

if (params$plot_type == "scatter") {
  if ("group" %in% names(scores)) {
    p <- ggplot(scores, aes(x = PC1, y = PC2, color = group)) +
      geom_point(size = 3) +
      labs(x = sprintf("PC1 (%.1f%%)", summary(pca_result)$importance[2,1]*100),
           y = sprintf("PC2 (%.1f%%)", summary(pca_result)$importance[2,2]*100),
           title = "PCA Scatter Plot",
           color = "Group") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    p <- ggplot(scores, aes(x = PC1, y = PC2)) +
      geom_point(size = 3) +
      labs(x = sprintf("PC1 (%.1f%%)", summary(pca_result)$importance[2,1]*100),
           y = sprintf("PC2 (%.1f%%)", summary(pca_result)$importance[2,2]*100),
           title = "PCA Scatter Plot") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  ggsave(filename = file.path("output", paste0(params$output_name, "_scatter.png")), plot = p, width = 10, height = 8, dpi = 300)
  
} else if (params$plot_type == "biplot") {
  # Biplot：加载向量 + 样本点
  loadings <- as.data.frame(pca_result$rotation[, 1:n_comps])
  loadings$feature <- rownames(loadings)
  
  p <- ggplot() +
    geom_point(data = scores, aes(x = PC1, y = PC2), size = 2) +
    geom_segment(data = loadings, 
                 aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5), 
                 arrow = arrow(length = unit(0.02, "npc")), color = "red") +
    geom_text(data = loadings, aes(x = PC1*5.5, y = PC2*5.5, label = feature), 
              color = "red", size = 3) +
    labs(x = sprintf("PC1 (%.1f%%)", summary(pca_result)$importance[2,1]*100),
         y = sprintf("PC2 (%.1f%%)", summary(pca_result)$importance[2,2]*100),
         title = "PCA Biplot") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(filename = file.path("output", paste0(params$output_name, "_biplot.png")), plot = p, width = 12, height = 8, dpi = 300)
  
} else if (params$plot_type == "scree") {
  # Scree plot
  eig <- summary(pca_result)$importance[2, ]
  scree_df <- data.frame(PC = seq_along(eig), Variance = eig * 100)
  
  p <- ggplot(scree_df, aes(x = PC, y = Variance)) +
    geom_line(group = 1, color = "steelblue") +
    geom_point(color = "steelblue") +
    labs(x = "Principal Component", y = "Variance Explained (%)", title = "Scree Plot") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(filename = file.path("output", paste0(params$output_name, "_scree.png")), plot = p, width = 10, height = 6, dpi = 300)
}

# 输出特征载荷
loadings_tsv_path <- file.path("output", paste0(params$output_name, "_loadings.tsv"))
readr::write_tsv(as.data.frame(pca_result$rotation), loadings_tsv_path)
message(sprintf("PCA loadings saved to: %s", loadings_tsv_path))
