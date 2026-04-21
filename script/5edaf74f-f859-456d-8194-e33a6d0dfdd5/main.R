library(tidyverse)
# 1. 设置随机种子，确保每次生成的数据都相同，方便复现
set.seed(123)

# 2. 生成一个 5行10列 的随机数矩阵，模拟基因表达数据
#    rnorm(n, mean, sd) 生成服从正态分布的随机数
heatmap_data <- matrix(rnorm(50, mean = 10, sd = 3), nrow = 5, ncol = 10)

# 3. 为矩阵添加行名和列名
rownames(heatmap_data) <- paste0("Gene_", 1:5)      # 行名：Gene_1 到 Gene_5
colnames(heatmap_data) <- paste0("Sample_", 1:10)  # 列名：Sample_1 到 Sample_10

# 4. 查看生成的数据
print("热图数据预览：")
print(heatmap_data)
heatmap_data |>
  as.data.frame() |>
  rownames_to_column("Gene") |>
  write_tsv(file = "output/data_heatmap.tsv")


# 1. 设置随机种子
set.seed(456)

# 2. 创建分组变量
#    rep() 函数用于重复元素，这里创建了三组，每组30个样本
group <- factor(rep(c("Control", "Treatment_A", "Treatment_B"), each = 30))

# 3. 创建数值变量
#    模拟不同分组的数值，例如，Treatment_A组的值整体偏高
value <- c(rnorm(30, mean = 5, sd = 1),    # Control组数据
           rnorm(30, mean = 7, sd = 1.5),  # Treatment_A组数据
           rnorm(30, mean = 6, sd = 1.2))  # Treatment_B组数据

# 4. 将变量组合成一个数据框
boxplot_data <- data.frame(Group = group, Value = value)

# 5. 查看数据前几行
print("箱线图数据预览：")
print(head(boxplot_data))


write_tsv(boxplot_data,file = "output/data_boxplot.tsv")
