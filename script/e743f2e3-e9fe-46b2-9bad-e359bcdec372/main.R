library(Maaslin2)
library(jsonlite)
library(tidyverse)
library(pheatmap)
# library(ggrepel)
library(ggrepel)   # 用于防止标签重叠
library(lefser)
library(ggpubr)
library(ggalluvial)
library(readxl)
library(patchwork)
library(Hmisc)  # rcorr 用于相关性+p值

params <- fromJSON("params.json")


x_feature_list <- NULL
if(!is.null(params$query_x_feature) && params$query_x_feature!=""){
  x_feature_list <- str_split(params$query_x_feature,",")[[1]]
}
y_feature_list <- NULL
if(!is.null(params$query_y_feature) && params$query_y_feature!=""){
  y_feature_list <- str_split(params$query_y_feature,",")[[1]]
}





# params$x_file$feature$columns_name
# name <- "x_file"
# feature_list <- x_feature_list
# prefix <- ""
get_df <- function(name,prefix,feature_list=NULL){
  
  if(is.null(prefix)){
    prefix <- ""
  }
  
  df <- read_tsv(params[[name]]$content)
  metadata <- params[[name]]$samples |>
    select(sample_name,group= selcted_group_name )
  feature <- params[[name]]$feature$columns_name
  df_select <- read_tsv(params[[name]]$content)|>
    select(all_of(feature), metadata$sample_name) 
  
  df_select <- df_select[!duplicated(df_select[[feature]]),] |>
    column_to_rownames(feature)
  

  
  # metabolite1_exp_matrix <- metabolite1_exp|>
  #   column_to_rownames("MS2_name")
  matrix <- data.frame()
  if(!is.null(feature_list)){
    intersect_feature_list <-  intersect(feature_list,rownames(df_select))
    matrix <- df_select[intersect_feature_list,] |>
      rownames_to_column("feature")|>
      mutate(feature = str_glue("{prefix}{feature}"))|>
      column_to_rownames("feature")
  }else{
    matrix <- df_select |>
      rownames_to_column("feature")|>
      mutate(feature = str_glue("{prefix}{feature}"))|>
      column_to_rownames("feature")
  }

    
  return(matrix)
}





# S-adenosylmethionine，Citrulline，Proline，4-Aminobutanoate(GABA)，N-Acetylornithine，Ornithine，Creatinine，Homocarnosine 
# S-adenosylmethionine，，，4-Aminobutanoate(GABA)，N-Acetylornithine，，， 
# 4-Aminobutanoate(GABA) 
df1 <- get_df("x_file", params$x_prefix, x_feature_list)
df2 <- get_df("y_file", params$y_prefix, y_feature_list)


# metabolite2$matrix |> rownames()
# intersect(metabolite2$exp$MS2_name,c("S-Adenosylmethionine","4-Aminobutanoate(GABA)","N-Acetylornithine"))
# metabolite2$exp$MS2_name[grepl("GABA",metabolite2$exp$MS2_name,)]




# 
# df1 <- x_df$matrix
# df2 <- y_df$matrix

common_samples <- intersect(colnames(df1),colnames(df2))

if(length( intersect(rownames(df1), rownames(df2)) ) > 0){
  stop("Two sets of feature names cannot be the same!")
}

df11 <- df1[, common_samples]
df21 <- df2[, common_samples]
df1_t <- t(df11)
df2_t <- t(df21)
res <- rcorr(as.matrix(df1_t), as.matrix(df2_t), type = "spearman")

cor(df1_t[,1], df2_t[,1],method ="spearman")

n_metab <- ncol(df1_t)
n_micro <- ncol(df2_t)
corr_matrix <-  res$r[colnames(df1_t),colnames(df2_t)]
p_matrix <- res$P[colnames(df1_t),colnames(df2_t)]


p_values <- as.vector(p_matrix)
p_values_no_na <- p_values[!is.na(p_values)]

# 使用 FDR 校正（或改为 "bonferroni"）
p_adjusted <- p.adjust(p_values_no_na, method = "none")

# 把校正后的 p 值重新填回矩阵形状
p_adj_matrix <- matrix(NA, nrow = nrow(p_matrix), ncol = ncol(p_matrix))
p_adj_matrix[!is.na(p_matrix)] <- p_adjusted

# 保留行列名
rownames(p_adj_matrix) <- rownames(p_matrix)
colnames(p_adj_matrix) <- colnames(p_matrix)
write_tsv(as.data.frame(corr_matrix) |> rownames_to_column("name"),file = "output/prompt.ai")

write_tsv(as.data.frame(corr_matrix) |> rownames_to_column("name"),file = "output/corr_matrix.tsv")
write_tsv(as.data.frame(p_adj_matrix)|> rownames_to_column("name"),file = "output/p_adj_matrix.tsv")

sig_matrix <- ifelse(p_adj_matrix < 0.01, "**",
                     ifelse(p_adj_matrix < 0.05, "*", ""))


# "__heatmap_width": 8,
# "__heatmap_height": 8,
# "__heatmap_cluster_rows": true,
# "__heatmap_cluster_cols": true,
# "__heatmap_show_rownames": true,
# "__heatmap_show_colnames": true,

heatmap_width <- params$`__heatmap_width`
heatmap_height <- params$`__heatmap_height`
cluster_rows <- params$`__heatmap_cluster_rows`
cluster_cols <- params$`__heatmap_cluster_cols`
show_rownames <- params$`__heatmap_show_rownames`
show_colnames <- params$`__heatmap_show_colnames`
heatmap_title <- params$`__heatmap_title`
  
pdf(file = str_glue("output/heatmap.pdf") , width =heatmap_width,height =heatmap_height)
pheatmap(
  corr_matrix,
  display_numbers = sig_matrix,
  color = colorRampPalette(c("#9BBBE1", "#FFFFFF", "#F09BA0"))(100), # 蓝白红
  cluster_rows = cluster_rows,
  cluster_cols = cluster_cols,
  show_rownames =show_rownames,
  show_colnames = show_colnames,
  fontsize_number = 10,
  fontsize = 12,
  main = heatmap_title, #"Metabolite - Metabolite Correlation",
  border_color = NA # 去掉边框，更干净
)
dev.off()










