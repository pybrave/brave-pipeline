library(tidyverse)
library(vegan)
library(jsonlite)
library(ggpubr)
library(reshape2)

params_path <- "params.json"
params <- fromJSON(params_path)


otu_table <- read_tsv(params$otu_table$content) |>
  dplyr::rename(taxonomy = 1)

metadata <- params$otu_table$sample |>
  select(sample_name,group) |>
  column_to_rownames("sample_name")
  

tax_sum <- otu_table[c("taxonomy",rownames(metadata) )] |>
  column_to_rownames("taxonomy")
topN <- 8

# idx = rownames(metadata) %in% colnames(tax_sum)
# metadata = metadata[idx, , drop = F]
# tax_sum = tax_sum[, rownames(metadata)]

# 提取样品组信息,默认为group可指定
# sampFile = as.data.frame(metadata[, c("group")], row.names = row.names(metadata))
# colnames(sampFile)[1] = "group"
sampFile <- metadata
# 按丰度降序排序
mean_sort = as.data.frame(tax_sum[(order(-rowSums(tax_sum))),])
# 把末分类调整到最后面
idx = grepl("unassigned|unclassified|unknown",
            rownames(mean_sort),
            ignore.case = T)
mean_sort = rbind(mean_sort[!idx, ], mean_sort[idx, ])
# 筛选前N类，其它归为Other，可设置不同组数
other = colSums(mean_sort[topN:dim(mean_sort)[1],])
mean_sort = mean_sort[1:(topN - 1),]
mean_sort = rbind(mean_sort, other)
rownames(mean_sort)[topN] = c("Other")
# 保存变量备份，并输出至文件
merge_tax = mean_sort


mat_t = t(merge_tax)
mat_t2 = merge(sampFile, mat_t, by = "row.names")
mat_t2 = mat_t2[, c(-1)]

# 按组求均值，转置，再添加列名
mat_mean = aggregate(mat_t2[, -1], by = mat_t2[1], FUN = mean) # mean
mat_mean_final = do.call(rbind, mat_mean)[-1, ]
geno = mat_mean$group
colnames(mat_mean_final) = geno
mean_sort = as.data.frame(mat_mean_final)

# 添加分类学并设置排序方式，默认字母，abundancer按丰度
mean_sort$Taxonomy = rownames(mean_sort)
data_all = as.data.frame(melt(mean_sort, id.vars = c("Taxonomy")))
data_all$Taxonomy  = factor(data_all$Taxonomy, levels = rownames(mean_sort))

data_all$value = as.numeric(data_all$value)
ggplot(data_all, aes(x = variable, y = value, fill = Taxonomy)) +
  geom_bar(stat = "identity",
           position = "fill",
           width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Groups") + ylab("Percentage (%)") + theme_classic() +
  theme(text = element_text(family = "sans", size = 12)) +
  scale_fill_brewer(palette = "Set3")

ggsave(filename = str_glue("output/tax_stackplot.pdf"))
