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
name <- "metabolite1"

diff_metabolite <- function(name,prefix,feature_list=NULL){
  metabolite1 <- read_tsv(params[[name]]$content)
  metadata1 <- rbind(params[[name]]$control, params[[name]]$treatment) |>
    select(sample_name,group=selcted_group_name)
  metabolite1_exp  <- metabolite1 |>
    mutate(MS2_name = case_when(is.na(MS2_name)~MS1_name,
                                .default =MS2_name )) |>
    select(MS2_name, metadata1$sample_name) |>
    na.omit() 
  metabolite1_exp <- metabolite1_exp[!duplicated(metabolite1_exp$MS2_name),]
  
  if(is.null(feature_list)){
    control_name1 <- params$groups_name[[name]]$control
    treatment_name1 <- params$groups_name[[name]]$treatment
    
    
    fit_data = Maaslin2(input_data=  column_to_rownames(metabolite1_exp,"MS2_name") |>t() |> as.data.frame(), 
                        input_metadata = column_to_rownames(metadata1,"sample_name"), 
                        plot_scatter =F,
                        min_prevalence = 0,
                        normalization  = "NONE",
                        output         = ".", 
                        fixed_effects  = c("group"),
                        reference      = c(paste0("group,",control_name1)))  
    
    
    metabolite1_map <- metabolite1_exp |>
      mutate(feature=make.names(MS2_name)) |>
      relocate(feature, .after  = "MS2_name")
    
    
    sig_type <-  params$`__metabolite_sig_type`
    effect_threshold <- params$`__metabolite_effect_threshold`
    sig_threshold <- params$`__metabolite_sig_threshold`
    
    
    metabolite1_diff <- fit_data$results |>
      left_join(metabolite1_map, by="feature") |>
      mutate(sig_value = .data[[sig_type]]) |>
      mutate(direction = factor(ifelse(sig_value  < sig_threshold & abs(coef) > effect_threshold ,
                                       ifelse(coef>0,"Up","Down"),"NS"),
                                levels = c("Up","Down","NS") 
      )) |>
      mutate(feature = MS2_name) |>
      select(-MS2_name) 
    feature_list <- metabolite1_diff |>
      filter(direction!="NS") |>pull("feature")
  }

  
  
  metabolite1_exp_matrix <- metabolite1_exp|>
    column_to_rownames("MS2_name")
  intersect_feature_list <-  intersect(feature_list,rownames(metabolite1_exp_matrix))
  matrix <- metabolite1_exp_matrix[intersect_feature_list,] |>
    rownames_to_column("feature")|>
    mutate(feature = str_glue("{prefix}{feature}"))|>
    column_to_rownames("feature")
    
  return(list(diff_metabolite,exp = metabolite1_exp,matrix =matrix ))
}
feature1_list <- NULL
if(!is.null(params$query_metabolite1) && params$query_metabolite1!=""){
  feature1_list <- str_split(params$query_metabolite1,",")[[1]]
}
feature2_list <- NULL
if(!is.null(params$query_metabolite2) && params$query_metabolite2!=""){
  feature2_list <- str_split(params$query_metabolite2,",")[[1]]
}

# S-adenosylmethionine，Citrulline，Proline，4-Aminobutanoate(GABA)，N-Acetylornithine，Ornithine，Creatinine，Homocarnosine 
# S-adenosylmethionine，，，4-Aminobutanoate(GABA)，N-Acetylornithine，，， 
# 4-Aminobutanoate(GABA) 
metabolite1 <- diff_metabolite("metabolite1",params$metabolite1_prefix,feature1_list)
metabolite2 <- diff_metabolite("metabolite2",params$metabolite2_prefix,feature2_list)


# metabolite2$matrix |> rownames()
# intersect(metabolite2$exp$MS2_name,c("S-Adenosylmethionine","4-Aminobutanoate(GABA)","N-Acetylornithine"))
# metabolite2$exp$MS2_name[grepl("GABA",metabolite2$exp$MS2_name,)]

df1 <- metabolite1$matrix
df2 <- metabolite2$matrix

common_samples <- intersect(colnames(df1),colnames(df2))

df11 <- df1[, common_samples]
df21 <- df2[, common_samples]
df1_t <- t(df11)
df2_t <- t(df21)
res <- rcorr(as.matrix(df1_t), as.matrix(df2_t), type = "spearman")

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
  main = "Metabolite - Metabolite Correlation",
  border_color = NA # 去掉边框，更干净
)
dev.off()










