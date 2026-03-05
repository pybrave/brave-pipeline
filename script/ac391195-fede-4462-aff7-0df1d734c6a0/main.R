library(tidyverse)
library(jsonlite)
library(pheatmap)
args <- commandArgs(trailingOnly = TRUE)

print(args)
params_path <- args[1]
output_path <- args[2]

if(F){
  params_path <- "params.json"
  output_path <- "output"
}

data <- fromJSON(params_path)
control <- data$control |>
    mutate(select_group= data$groups_name$control)
treatment <- data$treatment |>
    mutate(select_group= data$groups_name$treatment)

list_path <- rbind(control, treatment)

metadata <- list_path[c("sample_name","select_group")]


# df_list <- apply(list_path,1, function(x){
#     term_path <- x[[data$term]]
#     sample_name <- x[["sample_name"]]
#     df <-  read_tsv(term_path)
#     colnames(df) <- c("term",sample_name)
#     df <- df |> filter(!grepl("\\|", term))
#     df
#     # print()
# })
# merged_df <- reduce(df_list,function(x,y){merge(x,y,by="term",all=T)} ) |>
#   column_to_rownames("term") %>%
#   mutate(across(where(is.numeric), ~replace_na(., 0)))

df_list <- apply(list_path,1, function(x){
  term_path <- x[[data$term]]
  sample_name <- x[["sample_name"]]
  df <-  read_tsv(term_path)
  colnames(df) <- c("term","abundance")
  df <- df |> filter(!grepl("\\|", term)) |>
    mutate(sample_name=sample_name)
  df
})


df_long <- bind_rows(df_list)

merged_df <- df_long %>%
  pivot_wider(names_from = sample_name, values_from = abundance) |>
  column_to_rownames("term") |>
  mutate(across(where(is.numeric), ~replace_na(., 0))) 




merged_df <- merged_df[metadata$sample_name]

result <- data.frame(
  term = rownames(merged_df),
  pvalue = apply(merged_df, 1, function(vals) {
    wilcox.test(as.numeric(vals) ~ metadata$select_group)$p.value
  })
)
pseudo_count <- 1
log2fc <- apply(merged_df, 1, function(vals) {
  mean1 <- mean(vals[metadata$select_group == data$groups_name$treatment], na.rm = TRUE)
  mean2 <- mean(vals[metadata$select_group == data$groups_name$control], na.rm = TRUE)
    # print(c(mean1,mean2,log2(mean1+pseudo_count / mean2+pseudo_count)))
  log2( (mean1+pseudo_count) / (mean2+pseudo_count)  )
})
result$log2fc <- log2fc

result$padj <- p.adjust(result$pvalue, method = "BH")
result <- result[order(result$padj), ]
filename <- paste0(unique( metadata$select_group),collapse = "_vs_")

result  |>
    left_join(rownames_to_column(merged_df,"term"),by="term") |>
    write_tsv(paste0(output_path,"/",filename,".tsv"))

# log2fc[rownames(result)[1]]


sig_thresh <-  data$sig_thresh
logfc_cutoff <- data$logfc_thresh
sig_type <- data$sig_type

all_results <- result %>%
  mutate(sig_value = .data[[sig_type]]) |>
  mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(log2fc) > logfc_cutoff ,
                                   ifelse(log2fc     >0,"Up","Down"),"NS"),
                            levels = c("Up","Down","NS") 
  )) |>
  rownames_to_column("feature")



counts <- table(all_results$direction)
labels <- c(
  Down = paste0("Down (", counts["Down"], ")"),
  NS   = paste0("NS (", counts["NS"], ")"),
  Up   = paste0("Up (", counts["Up"], ")")
)
title <- paste0(c(data$groups_name$treatment,data$groups_name$control),collapse = " vs ")

# 画图
ggplot(all_results, aes(x= log2fc, 
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
  geom_text(data = all_results %>% filter(direction!="NS") |> head(data$label_size),
            aes(label = feature))
ggsave(filename = paste0(output_path,"/",filename,".volcano.pdf"))



# ,"log2fc"


df_long |>
  filter(term == rownames(result)[1]) |>
  inner_join(metadata,by="sample_name") |>
  ggplot(aes(x=select_group, y=abundance )) +
    geom_boxplot(fill = "skyblue", color = "black") +
    geom_point()+
    theme_bw()+
    ggtitle(paste0(result[rownames(result)[1],c("term")],"\n",title," log2fc: ",result[rownames(result)[1],c("log2fc")]))
ggsave(filename = paste0(output_path,"/",filename,".boxplot.pdf"))

 
dev.off()
pdf(file =paste0(output_path,"/",str_replace_all(title," ","_"),".heatmap.pdf") , width = data$heatmap_width,height =8)
log2(merged_df[head(rownames(result), n=30),]+1) |>
  pheatmap(scale = "row")
dev.off()


all_results|> head(20) |>
  mutate(log10pvalue = -log10(sig_value)) |>
  arrange(log2fc) %>%
  mutate(feature = factor(feature, levels = feature)) %>%
  ggplot(aes(x=feature,y=log2fc,fill =log10pvalue))+
  geom_col() +
  coord_flip() +   # 翻转：物种在 y 轴，条形横向
  scale_fill_gradient(low = "skyblue", high = "red") +
  geom_hline(yintercept = 0, color = "black") +
  labs(y = "log2 Fold Change", x = "",
       fill = "-log10(p)") +
  theme_bw()
ggsave(filename = paste0(output_path,"/",filename,".barplot.pdf"))


# 
# library(ggplot2)
# library(dplyr)
# 
# set.seed(123)
# df <- data.frame(
#   species = paste0("Sp", 1:20),
#   log2fc = rnorm(20, 0, 2),              # log2 fold change
#   pvalue = runif(20, 0.0001, 0.05)       # p value
# )
# df$neglog10p <- -log10(df$pvalue)
# 
# 
# 
# df %>%
#   arrange(log2fc) %>%
#   mutate(species = factor(species, levels = species)) %>%
#   ggplot(aes(x = species, y = log2fc, fill = neglog10p)) +
#   geom_col() +
#   coord_flip() +   # 翻转：物种在 y 轴，条形横向
#   scale_fill_gradient(low = "skyblue", high = "red") +
#   geom_hline(yintercept = 0, color = "black") +
#   labs(y = "log2 Fold Change", x = "",
#        fill = "-log10(p)") +
#   theme_bw()
# 

# 
# head(rownames(result), n=30)
# 
# df_long |>
#   filter(term == pathway) |>
#   inner_join(metadata,by="sample_name") |>
#   ggplot(aes(x=select_group, y=abundance )) +
#   geom_boxplot(fill = "skyblue", color = "black") +
#   geom_point()+
#   theme_bw()
# 
# rownames(result)[1]
# 
# # log2fc <- apply(merged_df, 1, function(vals) {
# #   mean1 <- mean(vals[metadata$select_group == data$groups_name$treatment], na.rm = TRUE)
# #   mean2 <- mean(vals[metadata$select_group == data$groups_name$control], na.rm = TRUE)
# #   # print(c(mean1,mean2,log2(mean1+pseudo_count / mean2+pseudo_count)))
# #   log2(mean1+pseudo_count / mean2+pseudo_count)
# # })
# pathway <- "PWY-6121: 5-aminoimidazole ribonucleotide biosynthesis I"
# mean1 <- mean( as.numeric(merged_df[pathway,metadata$select_group == data$groups_name$treatment]), na.rm = TRUE)
# mean2 <- mean( as.numeric(merged_df[pathway,metadata$select_group == data$groups_name$control]), na.rm = TRUE)
# pseudo_count <- 1
# log2((mean1+pseudo_count) / (mean2+pseudo_count) )
# # result["PWY-5154: L-arginine biosynthesis III (via N-acetyl-L-citrulline)",]

