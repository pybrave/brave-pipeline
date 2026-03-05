library(tidyverse)
library(jsonlite)
library(braveR)
library(ggrepel)  
library(RColorBrewer)
library(pheatmap)
library(Maaslin2)


params <- fromJSON("params.json")


df.0 <- read_tsv(params$metabolite$content)
control_meta_df <- params$metabolite$control
treatment_meta_df <-params$metabolite$treatment
intersect_columns  <- intersect(colnames(control_meta_df), colnames(treatment_meta_df))
metadata <- rbind(control_meta_df[,intersect_columns], treatment_meta_df[,intersect_columns]) |>
  select(sample_name=columns_name,group =  re_groups_name)

df.1 <- df.0 |>
  mutate(MS2_name = case_when(is.na(MS2_name)~MS1_name,
                              .default =MS2_name )) |>
  select(MS2_name,KEGG_COMPOUND_ID=`KEGG COMPOUND ID`, metadata$sample_name)
# df.1 <- df.1[!is.na(df.1$MS2_name),]
df.1 <- df.1 |> na.omit()
df <- df.1[!duplicated(df.1$MS2_name),]

control_name <- params$re_groups_name$metabolite$control
treatment_name <- params$re_groups_name$metabolite$treatment

title <- str_glue("{treatment_name}_vs_{control_name}")
fit_data = Maaslin2(input_data     = select(df,-KEGG_COMPOUND_ID) |>
                      column_to_rownames("MS2_name") |>t() |> as.data.frame(), 
                    input_metadata = column_to_rownames(metadata,"sample_name"), 
                    plot_scatter =F,
                    min_prevalence = 0,
                    normalization  = "NONE",
                    output         = ".", 
                    fixed_effects  = c("group"),
                    reference      = c(paste0("group,",control_name)))  

df_map <- df |>
  mutate(feature=make.names(MS2_name))



sig_type <-  params$`__gene_sig_type`
effect_threshold <- params$`__gene_effect_threshold`
sig_threshold <- params$`__gene_sig_threshold`


df_diff <- fit_data$results |>
  left_join(df_map,by="feature") |>
  mutate(sig_value = .data[[sig_type]]) |>
  mutate(direction = factor(ifelse(sig_value  < sig_threshold & abs(coef) > effect_threshold ,
                                     ifelse(coef>0,"Up","Down"),"NS"),
                              levels = c("Up","Down","NS") 
    )) |>
  mutate(feature = MS2_name) |>
  relocate(KEGG_COMPOUND_ID , .after = "feature") |>
  select(-MS2_name) 

write_tsv(df_diff, file = str_glue("output/{title}_diff.tsv"))



counts <- table(df_diff$direction)
labels <- c(
  Down = paste0("Down (", counts["Down"], ")"),
  NS   = paste0("NS (", counts["NS"], ")"),
  Up   = paste0("Up (", counts["Up"], ")")
)

ggplot(df_diff, aes(x= coef, 
                   y = -log10(sig_value), 
                   colour=direction)) +
  geom_point(alpha=0.9, size=3.5)+
  scale_color_manual(values=c("Down" = "#3B4992FF", "NS"="#d2dae2","Up"="#EE0000FF"),  labels = labels)+
  geom_vline(xintercept=c(-effect_threshold,effect_threshold),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = -log10(sig_threshold),lty=4,col="black",lwd=0.8)+
  ggtitle(paste0("volcano plot of ",title)) +
  labs(x="Effect Size (Coefficient)", y=paste0("-log10(",sig_type,")"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(color = 'black',size = 10, family = 'sans', face = 'plain'),
        axis.title = element_text(color = 'black',size = 15, family = 'sans', face = 'plain'),
        legend.position="right", 
        legend.title = element_blank()
  ) + 
  geom_text_repel(data=df_diff %>% filter(direction!="NS") |> head(80),aes(label = feature), 
                  colour = "black", size = 4)


ggsave(filename =str_glue("output/{title}_volcano.pdf"))


if(!is.null(params$query_compound) && params$query_compound!="" ){
  compound_list <- str_split(params$query_compound,",")[[1]]
  message(compound_list)
  lapply(compound_list, function(x){
   plot_data <- df |>
      pivot_longer(-c(MS2_name,KEGG_COMPOUND_ID ),names_to = "sample_name", values_to = "abundance" ) |>
      left_join(metadata,by="sample_name") |>
      mutate(log2abunadance = log2(abundance)) |>
      filter(KEGG_COMPOUND_ID ==x) 

    p<- ggplot(plot_data, aes(x=group,y=log2abunadance)) +
      geom_boxplot()+
      ggtitle(plot_data |>select(MS2_name,KEGG_COMPOUND_ID)|>unique()|>pull("MS2_name"))
    ggsave(filename = str_glue("output/{x}.png"),plot = p)
  })
  
  
}



# 
# 
# 
# {
#   read_by_df_name <- function(df_name){
#     df_obj <- params[[df_name]]
#     df <- read_tsv(df_obj$content) |>
#       dplyr::rename(length="Length")
#     metadata <- lapply(df_obj$groups, function(x){
#       df_list <- df_obj[[x]]
#       df <- map_dfr(df_list, function(x) {
#         x[sapply(x, is.null)] <- NA
#         as_tibble(x)
#       })
#     }) |>bind_rows()
#     select_sample <- metadata[["columns_name"]]
#     df <- df[,c("feature","name","length",select_sample)]
#     result = list(df=df,metadata=metadata)
#     return(result)
#   }
#   
#   df_obj  <- read_by_df_name("count")
#   df <- df_obj$df
#   metadata0 <- df_obj$metadata
# }
# 
# 
# exp <- df |>
#   select(-c("name","length")) |>
#   column_to_rownames("feature")
# 
# metadata <- metadata0 |>
#   select("columns_name","group") |>
#   column_to_rownames("columns_name")
# 
# exp <- exp[rowSums(exp) > 0, ]
# all(rownames(metadata) %in% colnames(exp))
# exp <- exp[,rownames(metadata)]
# identical(rownames(metadata) ,colnames(exp))
# dds <- DESeqDataSetFromMatrix(countData = exp,
#                               colData = metadata,
#                               design = ~ group)
# dds <- DESeq(dds)
# res <- results(dds)
# 
# {
#   sig_type <- params$`__gene_sig_type`
#   sig_thresh <- params$`__gene_sig_threshold`
#   effect_thresh <- params$`__gene_effect_threshold`
#   top_num <- params$top_num
#   title <- paste0(params$groups_name$count,collapse = "_vs_")
#   colors <- params$colors$count
# }
# 
# 
# diff_res <- as.data.frame(res) |>
#   rownames_to_column("feature") |>
#   mutate(sig_value = .data[[sig_type]]) |>
#   mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(log2FoldChange) > effect_thresh ,
#                                    ifelse(log2FoldChange     >0,"Up","Down"),"NS"),
#                             levels = c("Up","Down","NS") 
#   )) |>
#   left_join(df, by="feature") |>
#   relocate(name,.after = "feature") |>
#   arrange(pvalue) 
# (tbl <- table(diff_res$direction))
# 
# write_tsv(diff_res, str_glue("output/{title}_deg.tsv"))
# 
# 
# 
# 
# 
# 
# diffSummaryV1(
#   df=diff_res,
#   criteria = str_glue("{sig_type} < {sig_thresh} & |log2FoldChange| > {effect_thresh}"),
#   title = "Differentially expressed genes summary",
#   filename = "gene"
# )
# 
# png(file =str_glue("output/{title}_ma.png"))
# plotMA(res, ylim=c(-2,2))
# dev.off()
# 
# ntd <- normTransform(dds)
# png(file =str_glue("output/{title}_pca.png") )
# plotPCA(ntd, intgroup=c("group"))
# dev.off()
# 
# 
# 
# 
# vsd <- vst(dds, blind=FALSE)
# topVarGenes <- head(order(rowVars(assay(vsd)), decreasing = TRUE), 1000)
# sampleDists <- dist(t(assay(vsd)[topVarGenes, ]))
# 
# sampleDistMatrix <- as.matrix(sampleDists)
# rownames(sampleDistMatrix) <-vsd$group
# colnames(sampleDistMatrix) <- vsd$group
# 
# colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
# png(type = "cairo",file =str_glue("output/{title}_sample-distances.download.png"))
# pheatmap(sampleDistMatrix,
#          clustering_distance_rows=sampleDists,
#          clustering_distance_cols=sampleDists,
#          col=colors,
#          show_rownames=T)
# dev.off()
# 
# 
# 
# labels <- c(
#   Down = paste0("Down (", tbl["Down"], ")"),
#   NS   = paste0("NS (", tbl["NS"], ")"),
#   Up   = paste0("Up (", tbl["Up"], ")")
# )
# colors
# 
# 
# # 画图
# p <- ggplot(diff_res, aes(x= log2FoldChange, 
#                           y = -log10(sig_value), 
#                           colour=direction)) +
#   geom_point(alpha=0.9, size=3.5)+
#   scale_color_manual(values=c("Down" = "#3B4992FF", "NS"="#d2dae2","Up"="#EE0000FF"),
#                      labels = labels) +
#   geom_vline(xintercept=c(-effect_thresh,effect_thresh),lty=4,col="black",lwd=0.8) +
#   geom_hline(yintercept = -log10(sig_thresh),lty=4,col="black",lwd=0.8)+
#   ggtitle(paste0("Volcano plot of ", title)) +
#   labs(x="log2 foldchange", y=paste0("-log10(",sig_type,")"))+
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5), 
#         axis.text = element_text(color = 'black',size = 10, family = 'sans', face = 'plain'),
#         axis.title = element_text(color = 'black',size = 15, family = 'sans', face = 'plain'),
#         legend.position="right", 
#         legend.title = element_blank()
#   )+    geom_text_repel(data=diff_res %>% filter(direction!="NS") |> head(top_num),aes(label = feature), 
#                         colour = "black", size = 4)
# 
# 
# ggsave(filename = str_glue("output/{title}_volcano.pdf"),plot = p)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ntd <- normTransform(dds)
# 
# ann_colors <- list(
#   group = setNames(
#     c("steelblue", "tomato"),  # 颜色向量
#     c( params$groups_name$count$control, params$groups_name$count$treatment)  # 用变量的值作为名字
#   )
# )
# 
# heatmap_df <- assay(ntd)[diff_res$feature[1:50],]
# 
# 
# png(type = "cairo", file = str_glue("output/{title}_heatmap.png") )
# pheatmap(heatmap_df,
#          scale="row",
#          cluster_rows=T, 
#          color =  colorRampPalette(c("darkred", "#FFFFFF","darkblue"))(255),
#          show_rownames=T,
#          annotation_colors = ann_colors,
#          cluster_cols=T, annotation_col=metadata |> dplyr::rename(group=group))
# dev.off()
# 
# 
# 
# 
# 
# 
# 
# gene_mean <- rowMeans(exp)
# gene_var  <- apply(exp, 1, var)
# 
# df <- data.frame(mean = gene_mean, variance = gene_var)
# 
# p <- ggplot(df, aes(x = mean, y = variance)) +
#   geom_point(alpha = 0.4, size = 1) +
#   scale_x_log10() + scale_y_log10() +   # log-log 更直观
#   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
#   labs(title = "Mean-Variance Relationship of Gene Expression",
#        x = "Mean Expression",
#        y = "Variance of Expression") +
#   theme_bw()
# ggsave(filename = str_glue("output/{title}_mean-variance-relationship.png"), plot = p)
# 
# ggsave(filename = str_glue("output/{title}_mean-variance-relationship.download.pdf"), plot = p)
# 
# 
# 
# 
# 
# mat <- assay(ntd)
# 
# # 计算样本间 Pearson 相关性
# sample_cor <- cor(mat, method = "spearman")  # 也可以换成 "spearman"
# 
# # 调色板
# colors <- colorRampPalette(rev(brewer.pal(9, "RdBu")))(255)
# 
# 
# png(file =str_glue("output/{title}_correlation.png"))
# pheatmap(sample_cor,
#          clustering_distance_rows = "euclidean",  # 行聚类距离
#          clustering_distance_cols = "euclidean",  # 列聚类距离
#          clustering_method = "complete",          # 聚类方法
#          display_numbers = TRUE,                  # 显示相关系数数值
#          col = colors,
#          main = "Sample Correlation Heatmap")
# dev.off()
