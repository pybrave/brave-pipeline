library(tidyverse)
library(DESeq2)
# library(vsn)
library(jsonlite)
library(pheatmap)
library(RColorBrewer)
library(ggrepel)   # 用于防止标签重叠

args <- commandArgs(trailingOnly = TRUE)
print(args)

params_path <- args[1]
output_path <- args[2]
if(F){
  params_path <- "params.json"
  output_path <- "output"
}

data <- fromJSON(params_path)
title <- paste0(c(data$groups_name$treatment,data$groups_name$control),collapse = " vs ")


control <- data$control |>
  mutate(select_group= data$groups_name$control)
treatment <- data$treatment |>
  mutate(select_group= data$groups_name$treatment)

list_path <- rbind(control, treatment)

metadata <- list_path[c("sample_name","select_group")] |>
  column_to_rownames("sample_name")
metadata$select_group <- factor(metadata$select_group)

df_list <- apply(list_path,1, function(x){
  profile_path <- x[["count"]]
  sample_name <- x[["sample_name"]]
  df <-  read_tsv(profile_path) |>
    select(gene_id,gene_name,count) |>
    mutate(sample_name=sample_name)
 
  df
})
df_long <- bind_rows(df_list)

merged_df <- df_long %>%
  pivot_wider(names_from = sample_name, values_from = count ) |>
  mutate(across(where(is.numeric), ~replace_na(., 0))) 

exp <- NULL
if(data$feature_type=="gene_id"){
  exp <- merged_df|>
    column_to_rownames("gene_id") |>
    select(-gene_name )
  
}else if(data$feature_type=="gene_name"){
  exp <- merged_df|>
    select(-gene_id ) |>
    filter(gene_name!="-") |>
    mutate(gene_name = make.unique(gene_name)) %>%
    column_to_rownames("gene_name")
  
}






gene_mean <- rowMeans(exp)
gene_var  <- apply(exp, 1, var)

df <- data.frame(mean = gene_mean, variance = gene_var)

ggplot(df, aes(x = mean, y = variance)) +
  geom_point(alpha = 0.4, size = 1) +
  scale_x_log10() + scale_y_log10() +   # log-log 更直观
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Mean-Variance Relationship of Gene Expression",
       x = "Mean Expression",
       y = "Variance of Expression") +
  theme_bw()
ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),".mean-variance-relationship.pdf"))


exp <- exp[rowSums(exp) > 0, ]
all(rownames(metadata) %in% colnames(exp))
identical(rownames(metadata) ,colnames(exp))
dds <- DESeqDataSetFromMatrix(countData = exp,
                              colData = metadata,
                              design = ~ select_group)
dds <- DESeq(dds)
res <- results(dds)
as.data.frame(res) |>
  rownames_to_column("feature") |>
  left_join(rownames_to_column(exp,"feature"),by="feature") |>
  write_tsv(paste0(output_path,"/",str_replace_all(title," ","_"),".deg.tsv"))

deg <- as.data.frame(res) |>
  arrange(padj)

# select <- order(rowMeans(counts(dds,normalized=TRUE)),
#                 decreasing=TRUE)[1:20]
ntd <- normTransform(dds)
df <- as.data.frame(colData(dds)[,c("select_group")])
# pheatmap(assay(ntd)[select,])

ann_colors <- list(
  group = setNames(
    c("steelblue", "tomato"),  # 颜色向量
    c( data$groups_name$control, data$groups_name$treatment)  # 用变量的值作为名字
  )
)

n_feature = ifelse(nrow(deg)>=data$heatmap_gene_num,data$heatmap_gene_num,nrow(deg))
heatmap_df <- assay(ntd)[rownames(deg)[1:n_feature],]

dev.off()
pdf(file =paste0(output_path,"/",str_replace_all(title," ","_"),".heatmap.pdf") , width = 8,height =8)
pheatmap(heatmap_df,
         cluster_rows=T, 
         color =  colorRampPalette(c("darkred", "#FFFFFF","darkblue"))(255),
         show_rownames=T,
         annotation_colors = ann_colors,
         cluster_cols=T, annotation_col=metadata |> dplyr::rename(group=select_group))
dev.off()

vsd <- vst(dds, blind=FALSE)
pdf(file =paste0(output_path,"/",str_replace_all(title," ","_"),".ma.pdf") , width = 8,height =8)
plotMA(res, ylim=c(-2,2))
dev.off()

pdf(file =paste0(output_path,"/",str_replace_all(title," ","_"),".deseq2.pca.pdf") , width = 8,height =8)
plotPCA(ntd, intgroup=c("select_group"))
dev.off()

# deg_genes <- res %>%
#   as.data.frame() %>%
#   filter(padj < 0.05 & abs(log2FoldChange) > 1) %>%
#   rownames()

# sampleDists <- dist(t(assay(ntd)[deg_genes, ]))
topVarGenes <- head(order(rowVars(assay(vsd)), decreasing = TRUE), 1000)
sampleDists <- dist(t(assay(vsd)[topVarGenes, ]))
# sampleDists <- dist(t(assay(ntd)))

sampleDistMatrix <- as.matrix(sampleDists)
rownames(sampleDistMatrix) <-vsd$select_group
colnames(sampleDistMatrix) <- vsd$select_group
colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
pdf(file =paste0(output_path,"/",str_replace_all(title," ","_"),".sample-distances.pdf") , width = 8,height =8)
pheatmap(sampleDistMatrix,
         clustering_distance_rows=sampleDists,
         clustering_distance_cols=sampleDists,
         col=colors,show_rownames=T)
dev.off()


sig_thresh <- data$sig_thresh
logfc_cutoff <- data$logfc_thresh
label_size <- 0
sig_type <- data$sig_type

all_results <- deg %>%
  mutate(sig_value = .data[[sig_type]]) |>
  mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(log2FoldChange) > logfc_cutoff ,
                                   ifelse(log2FoldChange     >0,"Up","Down"),"NS"),
                            levels = c("Up","Down","NS") 
  )) |>
  rownames_to_column("feature")|>
  filter(!is.na(direction))



counts <- table(all_results$direction)
labels <- c(
  Down = paste0("Down (", counts["Down"], ")"),
  NS   = paste0("NS (", counts["NS"], ")"),
  Up   = paste0("Up (", counts["Up"], ")")
)

# 画图
ggplot(all_results, aes(x= log2FoldChange, 
                        y = -log10(sig_value), 
                        colour=direction)) +
  geom_point(alpha=0.9, size=3.5)+
  scale_color_manual(values=c("Down" = "#3B4992FF", "NS"="#d2dae2","Up"="#EE0000FF"),
                     labels = labels) +
  geom_vline(xintercept=c(-logfc_cutoff,logfc_cutoff),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = -log10(sig_thresh),lty=4,col="black",lwd=0.8)+
  ggtitle(paste0("Volcano plot of ", title)) +
  labs(x="log2 foldchange", y=paste0("-log10(",sig_type,")"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(color = 'black',size = 10, family = 'sans', face = 'plain'),
        axis.title = element_text(color = 'black',size = 15, family = 'sans', face = 'plain'),
        legend.position="right", 
        legend.title = element_blank()
  ) +
  geom_text(data = all_results %>% filter(direction!="NS") |> head(label_size),
            aes(label = feature))
# ggplot(all_results, aes(x= log2FoldChange,
#                         y = -log10(sig_value),
#                         colour=direction)) +
#   geom_point(alpha=0.9, size=3.5)+
#   scale_color_manual(values=c("Down" = "#3B4992FF", "NS"="#d2dae2","Up"="#EE0000FF"))+
#   geom_vline(xintercept=c(-logfc_cutoff,logfc_cutoff),lty=4,col="black",lwd=0.8) +
#   geom_hline(yintercept = -log10(sig_thresh),lty=4,col="black",lwd=0.8)+
#   ggtitle(paste0("volcano plot of ",title)) +
#   labs(x="log2 foldchange", y=paste0("-log10(",sig_type,")"))+
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.text = element_text(color = 'black',size = 10, family = 'sans', face = 'plain'),
#         axis.title = element_text(color = 'black',size = 15, family = 'sans', face = 'plain'),
#         legend.position="right",
#         legend.title = element_blank()
#   ) + geom_text(data = all_results %>% filter(direction!="NS") |> head(label_size),
#                 aes(label = feature))

ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),".volcano.pdf"))





# 1. 提取数据并做PCA
ntd_mat <- assay(ntd)   # 取出 variance-stabilizing / rlog 转换后的表达矩阵
pca <- prcomp(t(ntd_mat))   # 样本作为行

# 2. 整理数据框
pca_data <- data.frame(pca$x[, 1:2])  # PC1 和 PC2
pca_data$group <- ntd$select_group    # 分组信息
pca_data$sample <- colnames(ntd_mat)  # 样本名

# 3. 计算PC1/PC2方差贡献度
percentVar <- round(100 * (pca$sdev^2 / sum(pca$sdev^2))[1:2], 1)

# 4. 绘图
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = group)) +
  geom_point(size = 4) +   # 样本点
  geom_text_repel(aes(label = sample), size = 3) +  # 添加样本标签
  stat_ellipse(level = 0.95, linetype = 2) +        # 95%置信椭圆
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "right")
ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),".pca.pdf"),plot = p)




mat <- assay(ntd)

# 计算样本间 Pearson 相关性
sample_cor <- cor(mat, method = "spearman")  # 也可以换成 "spearman"

# 调色板
colors <- colorRampPalette(rev(brewer.pal(9, "RdBu")))(255)

dev.off()
# 绘制热图
pdf(file =paste0(output_path,"/",str_replace_all(title," ","_"),".correlation.pdf") , width = 8,height =8)
pheatmap(sample_cor,
         clustering_distance_rows = "euclidean",  # 行聚类距离
         clustering_distance_cols = "euclidean",  # 列聚类距离
         clustering_method = "complete",          # 聚类方法
         display_numbers = TRUE,                  # 显示相关系数数值
         col = colors,
         main = "Sample Correlation Heatmap")
dev.off()


# library(GGally)
# 
# # 取 log 转换后的表达矩阵，比如 ntd/vsd
# mat <- assay(ntd)
# 
# # 转成数据框
# df <- as.data.frame(mat)
# 
# # 绘制样本间散点图矩阵
# ggpairs(df,
#         lower = list(continuous = "smooth"),   # 下三角显示散点加拟合线
#         diag = list(continuous = "densityDiag"), # 对角线显示密度分布
#         upper = list(continuous = "cor"))      # 上三角显示相关系数

# cor_mat <- cor(mat, method = "pearson")
# 
# # 提取上三角（去掉重复和对角线）
# cor_values <- cor_mat[upper.tri(cor_mat)]
# 
# 
# 
# ggplot(data.frame(cor = cor_values), aes(x = cor^2)) +
#   geom_histogram(bins = 20, fill = "steelblue", color = "white") +
#   geom_density(alpha = 0.3, fill = "tomato") +
#   xlab("R² (Sample correlation)") +
#   ylab("Frequency") +
#   ggtitle("Distribution of Sample Correlation (R²)")


# 
# library(uwot)
# 
# # ========================
# # 输入数据准备
# # ========================
# mat <- assay(ntd)          # 基因 × 样本
# group <- colData(ntd)$select_group
# samples <- colnames(ntd)
# 
# # ========================
# # 1. PCA 分析
# # ========================
# pca <- prcomp(t(mat), scale. = TRUE)
# pca_df <- as.data.frame(pca$x[, 1:2])
# colnames(pca_df) <- c("PC1", "PC2")
# pca_df$group <- group
# pca_df$sample <- samples
# 
# p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = group)) +
#   geom_point(size = 3, alpha = 0.8) +
#   stat_ellipse(aes(fill = group), geom = "polygon", alpha = 0.1, color = NA) +
#   geom_text_repel(aes(label = sample), size = 3) +
#   theme_bw() +
#   labs(title = "PCA of Samples")
# 
# # ========================
# # 2. UMAP 分析
# # ========================
# set.seed(123)
# umap_res <- umap(t(mat), n_neighbors = 10, min_dist = 0.1, metric = "euclidean")
# umap_df <- as.data.frame(umap_res)
# colnames(umap_df) <- c("UMAP1", "UMAP2")
# umap_df$group <- group
# umap_df$sample <- samples
# 
# p_umap <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = group)) +
#   geom_point(size = 3, alpha = 0.8) +
#   stat_ellipse(aes(fill = group), geom = "polygon", alpha = 0.1, color = NA) +
#   geom_text_repel(aes(label = sample), size = 3) +
#   theme_bw() +
#   labs(title = "UMAP of Samples")
# 
# 



# 
# pasCts <- system.file("extdata",
#                       "pasilla_gene_counts.tsv.gz",
#                       package="DESeq2", mustWork=TRUE)
# pasAnno <- system.file("extdata",
#                        "pasilla_sample_annotation.csv",
#                        package="DESeq2", mustWork=TRUE)
# cts <- as.matrix(read.csv(pasCts,sep="\t",row.names="gene_id"))
# coldata <- read.csv(pasAnno, row.names=1)
# coldata <- coldata[,c("condition","type")]
# coldata$condition <- factor(coldata$condition)
# coldata$type <- factor(coldata$type)
# rownames(coldata) <- sub("fb", "", rownames(coldata))
# all(rownames(coldata) %in% colnames(cts))
# cts <- cts[, rownames(coldata)]
# all(rownames(coldata) == colnames(cts))
# 
# dds <- DESeqDataSetFromMatrix(countData = cts,
#                               colData = coldata,
#                               design = ~ condition)
# dds
# 
# 
# featureData <- data.frame(gene=rownames(cts))
# mcols(dds) <- DataFrame(mcols(dds), featureData)
# mcols(dds)
# dds <- DESeq(dds)
# res <- results(dds)
# res
# 
# resultsNames(dds)
# resLFC <- lfcShrink(dds, coef="condition_untreated_vs_treated", type="apeglm")
# resLFC
# plotMA(res, ylim=c(-2,2))
# 
# 
# idx <- identify(res$baseMean, res$log2FoldChange)
# rownames(res)[idx]
# 
# 
# vsd <- vst(dds, blind=FALSE)
# rld <- rlog(dds, blind=FALSE)
# head(assay(vsd), 3)
# library(vsn)
# a <- assay(dds)
# meanSdPlot(a, ranks=F)
# 
# # this gives log2(n + 1)
# ntd <- normTransform(dds)
# library("vsn")
# meanSdPlot(assay(ntd),ranks = F)
# 
# 
# library("pheatmap")
# select <- order(rowMeans(counts(dds,normalized=TRUE)),
#                 decreasing=TRUE)[1:20]
# df <- as.data.frame(colData(dds)[,c("condition","type")])
# pheatmap(assay(ntd)[select,], cluster_rows=FALSE, show_rownames=FALSE,
#          cluster_cols=FALSE, annotation_col=df)
# 
# gene_mean <- rowMeans(a)
# gene_var  <- apply(a, 1, sd)
# 
# df <- data.frame(mean = gene_mean, variance = gene_var)
# 
# ggplot(df, aes(x = mean, y = variance)) +
#   geom_point(alpha = 0.4, size = 1) +
#   scale_x_log10() + scale_y_log10() +   # log-log 更直观
#   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
#   labs(title = "Mean-Variance Relationship of Gene Expression",
#        x = "Mean Expression",
#        y = "Variance of Expression") +
#   theme_bw()
# 
# 
# 
# 
# library(vsn)
# 
# set.seed(1)
# mat <- matrix(rpois(10000, lambda = 10), nrow=1000, ncol=10)
# 
# # gene-level mean vs sd
# gene_mean <- rowMeans(mat)
# gene_sd   <- apply(mat, 1, sd)
# plot(gene_mean, gene_sd, log="xy", main="Gene-level mean vs sd")
# 
# # sample-level meanSdPlot
# meanSdPlot(mat, ranks=FALSE)  # vsn自带函数
# 
# 
# 
# 
