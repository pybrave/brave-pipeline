

library(Maaslin2)
library(jsonlite)
library(tidyverse)
library(pheatmap)
# library(ggrepel)
library(ggrepel)   # 用于防止标签重叠
library(lefser)
library(ggpubr)
library(ggalluvial)
library(patchwork)
# 加载包
library(vegan)     # 用于计算距离矩阵和 PCoA
library(ggplot2)   # 用于绘图



log <- function(...){
  cat(paste0(...),file = paste0(output_path,"/run.info"),append = T)
}



args <- commandArgs(trailingOnly = TRUE)
print(args)

params_path <- args[1]
output_path <- args[2]
if(T){
  params_path <- "params.json"
  output_path <- "output"
}


data <- fromJSON(params_path)
control_name <- ifelse(data$re_groups_name$control!="-",data$re_groups_name$control,data$groups_name$control)
treatment_name <- ifelse(data$re_groups_name$treatment!="-",data$re_groups_name$treatment,data$groups_name$treatment)

control <- data$control |>
  mutate(select_group=control_name )
treatment <- data$treatment |>
  mutate(select_group= treatment_name)

list_path <- rbind(control, treatment)

metadata <- list_path[c("sample_name","select_group")]

rank <- data$rank

duplicated(list_path$sample_name)


read_abundabce <- function(path){
  df <-  read_tsv(path,comment = "#",col_names =F)
  colnames(df) <- c("clade_name","NCBI_tax_id","abundance","additional_species")
  df <- select(df,c("clade_name",all_of("abundance")))
  df
}
parse_metaphlan <- function(df,sample_name,rank) {
  df %>%
    mutate(clade_name = as.character(clade_name)) %>%
    separate_wider_delim(
      clade_name,
      delim = "|",
      names = c("KINGDOM", "PHYLUM", "CLASS", "ORDER", "FAMILY", "GENUS", "SPECIES","SGB"),
      too_few = "align_start"
    )|>
    mutate(row_rank=case_when(!is.na(SGB)~'SGB',
                              !is.na(SPECIES)~'SPECIES',
                              !is.na(GENUS)~'GENUS',
                              !is.na(FAMILY)~'FAMILY',
                              !is.na(ORDER)~'ORDER',
                              !is.na(CLASS)~'CLASS',
                              !is.na(PHYLUM)~'PHYLUM',
                              !is.na(KINGDOM)~'KINGDOM'))|>
    mutate(sample_name= sample_name) |> 
    filter(row_rank==rank) |>
    select(sample_name,ptaxonomy=PHYLUM,taxonomy=all_of(rank),abundance) 
  
}



df_list <- apply(list_path,1, function(x){
  profile_path <- x[["profile"]]
  sample_name <- x[["sample_name"]]
  # df <-  read_tsv(term_path,comment = "#")
  # colnames(df) <- c("clade_name",sample_name)
  df <- read_abundabce(profile_path)
  df <- parse_metaphlan(df,sample_name,rank) 
  if(data$filter_unknown_taxonomy){
    df <- filter(df,!grepl("GGB|GBS|SGB",taxonomy)) |>
      mutate(abundance= abundance/sum(abundance)*100)
  }
  # df <- df |> filter(!grepl("\\|", term))
  df
})
df_long_0 <- bind_rows(df_list) 

df_long <-df_long_0 |>select(-ptaxonomy)

merged_df <- df_long %>%
  pivot_wider(names_from = sample_name, values_from = abundance) |>
  mutate(across(where(is.numeric), ~replace_na(., 0))) |>
  column_to_rownames("taxonomy") 



# metadata <- metadata |>column_to_rownames("sample_name")



merged_df_pcoa <- merged_df|>t() |>as.data.frame()




# 安装必要的包
# if(!require(vegan)) install.packages("vegan")
# if(!require(ggplot2)) install.packages("ggplot2")
# if(!require(ggforce)) install.packages("ggforce")  # 用于置信椭圆

library(vegan)
library(ggplot2)
library(ggforce)
# 
# # ---------------------------
# # 1. 模拟数据（可替换为真实距离矩阵）
# # ---------------------------
# set.seed(123)
# # 假设我们有三组样本，每组5个样本
# group <- rep(c("A","B","C"), each=5)
# # 模拟 OTU 表（样本 x 特征）
# otu <- matrix(rpois(15*10, lambda=10), nrow=15)
# rownames(otu) <- paste0("Sample", 1:15)
title <- paste0(c(data$groups_name$treatment,data$groups_name$control),collapse = " vs ")

# ---------------------------
# 2. 计算距离矩阵
# ---------------------------
dist_matrix <- vegdist(merged_df_pcoa, method="bray")  # Bray-Curtis 距离

# ---------------------------
# 3. PCoA
# ---------------------------
pcoa_res <- cmdscale(dist_matrix, k=2, eig=TRUE)  # k=2 -> 2D
pcoa_df <- pcoa_res$points |> as.data.frame() |>
  rownames_to_column("sample_name") |>
  inner_join(metadata, by="sample_name") |>
  dplyr::rename(c(Group=select_group,PC1=V1,PC2=V2) )


pcoa_df |>
  write_tsv(paste0(output_path,"/",title,".pcoa.download.tsv"))

permanova_res <- adonis2(dist_matrix ~ Group, data=pcoa_df, permutations=999)
permanova_p <- permanova_res$`Pr(>F)`[1] 
# ---------------------------
# 4. 绘图 + 置信椭圆
# ---------------------------

group_colors <- setNames(
  c("#67A9CC", "#DD9B26"), 
  c(control_name, treatment_name)
)
ggplot(pcoa_df, aes(x=PC1, y=PC2, color=Group, fill=Group)) +
  geom_point(size=4, shape=21, stroke=1, alpha=0.8) +  # 点
  stat_ellipse(geom="polygon", alpha=0.2, color=NA, level=0.95) +  # 置信椭圆
  # scale_color_brewer(palette="Set1") +
  # scale_fill_brewer(palette="Set1") +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = group_colors) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color="gray80"),
    # panel.grid.minor = element_blank(),
    title = element_text(face="bold",size = 20),
    axis.title = element_text(face="bold",size = 18),
    legend.title = element_text(face="plain",size = 18),
    panel.border = element_rect(color = "black", fill = NA, size = 2),  # 黑色边框
    # legend.position = c(0.99, 0.99),        # 右上角
    # legend.justification = c("right", "top"), # 锚点在右上角
    # legend.background = element_rect(fill="white", color="black"),
    
    
  ) +
  labs(
    title = str_glue("PCoA of {rank}"),
    x = "PCo 1 (29.76%)",
    y = "PCo 2 (17.11%)",
    # caption = paste("PERMANOVA: p-value =", permanova_p)
  )+
  annotate(
    "label",
    x = Inf,
    y = Inf,
    label = paste("PERMANOVA:\n p-value =", permanova_p),
    hjust = 1.1, vjust = 1.1,
    size = 5,
    label.size = 0.8  
  ) 
ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),".pcoa.pdf"))

  # guides(
  #   color = guide_legend(
  #     override.aes = list(
  #       shape = 21,        # 图例点
  #       size = 4,
  #       linetype = 1,      # 线条类型
  #       alpha = 0.8,
  #       color = "black"
  #     )
  #   ),
  #   # fill = guide_legend(
  #   #   override.aes = list(
  #   #     shape = 21,
  #   #     size = 4,
  #   #     linetype = 1,
  #   #     alpha = 0.2,
  #   #     color = "black"
  #   #   )
  #   # )
  # )


