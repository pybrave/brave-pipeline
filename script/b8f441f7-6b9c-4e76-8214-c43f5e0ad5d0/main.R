
# while (TRUE) {
#   Sys.sleep(1000)  # 每次休眠 1000 秒，然后继续循环
# }

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
library(braveR)
# detach("package:braveR", unload = TRUE)



args <- commandArgs(trailingOnly = TRUE)
print(args)

params_path <- args[1]
output_path <- args[2]
if(T){
  params_path <- "params.json"
  output_path <- "output"
}

cat("",file = paste0(output_path,"/run.info"))
log <- function(...){
  cat(paste0(...),file = paste0(output_path,"/run.info"),append = T)
}

data <- fromJSON(params_path)
# 
# 
# control <- data$abundance |>
#   mutate(select_group= data$groups_name$control)
# treatment <- data$treatment |>
#   mutate(select_group= data$groups_name$treatment)

control <- data$control |>
  mutate(select_group= data$groups_name$control)
treatment <- data$treatment |>
  mutate(select_group= data$groups_name$treatment)
# log("microbiome control: ",paste0(control$sample_name,collapse = ", "))
# log("microbiome treatment: ",paste0(treatment$sample_name,collapse = ", "))

list_path <- rbind(control, treatment)

metadata <- list_path[c("sample_name","select_group")]



{
  
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
  
  # sample_name <- "OCC8"
  # df <- read_abundabce("/ssd1/wy/workspace2/nextflow_workspace/289364b1-295c-4710-833e-d68ec7c8918e/131f8806-35e3-4d7c-b234-f14a2119aaa7/2c88b345-822f-4285-9222-a18b9c3daa8b/output/metaphlan/OCC8/OCC8_profile.txt")
  # a <- parse_metaphlan(df,"aa","SPECIES") |>
  #   filter(!grepl("GGB|GBS",taxonomy)) |>
  #   mutate(abundance= abundance/sum(abundance)*100)
  # sum(a$abundance)
  
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
  
  microbiome_df <- df_long %>%
    pivot_wider(names_from = sample_name, values_from = abundance) |>
    mutate(across(where(is.numeric), ~replace_na(., 0))) |>
    column_to_rownames("taxonomy") 
  
  
  
  fit_data = Maaslin2(input_data     = t(microbiome_df) |> as.data.frame(), 
                      input_metadata = column_to_rownames(metadata,"sample_name"), 
                      plot_scatter =F,
                      min_prevalence = 0,
                      normalization  = "NONE",
                      output         = ".", 
                      fixed_effects  = c("select_group"),
                      reference      = c(paste0("select_group,",data$groups_name$control)))  
  
  sig_thresh <-  data$micro_sig_thresh
  effect_cutoff <- data$micro_effect_cutoff
  
  
  microbiome_sig <- fit_data$results |>
    mutate(sig_value = .data[[data$micro_sig_type]]) |>
    mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(coef) > effect_cutoff ,
                                     ifelse(coef>0,"Up","Down"),"NS"),
                              levels = c("Up","Down","NS") 
    )) 
  tbl <- table(microbiome_sig$direction)
  list(
    criteria = str_glue("{data$metabo_sig_type} < {sig_thresh} & |coef| > {effect_cutoff}"),
    title = "Microbiome summary",
    total = sum(tbl),
    up = unname(tbl["Up"]),
    down = unname(tbl["Down"]),
    ns = unname(tbl["NS"])
  )|>toJSON( pretty = TRUE, auto_unbox = TRUE) |>
    cat(file ="output/microbiome.diff" )
  # msg <- paste0("microbiome(",data$sig_type,"<",sig_thresh," & abs(coef)>",effect_cutoff,") Up:",stat["Up"]," Down:",stat["Down"]," NS:",stat["NS"])
  # log(msg)
  
  microbiome_df_res <- microbiome_df[filter(microbiome_sig,direction!="NS") |> pull("feature"),]
  
  
  

}

taxonomy_list = str_split(data$query_taxonomy,",")[[1]]
if(!is.null(taxonomy_list)){
  microbiome_df_res <- microbiome_df[taxonomy_list,]
  message(nrow(microbiome_df_res),"-",length(taxonomy_list))
}



{
  
  metabolism_control <- data$metabolism_control |>
    mutate(select_group= data$groups_name$metabolism_control)
  
  metabolism_treatment <- data$metabolism_treatment |>
    mutate(select_group= data$groups_name$metabolism_treatment)
  
  metabolism_list_path <- rbind(metabolism_control, metabolism_treatment)
  
  metabolism_metadata <- metabolism_list_path[c("sample_name","select_group")]
  
  
  metabolism_df_list <- apply(metabolism_list_path,1, function(x){
    profile_path <- x[["profile"]]
    sample_name <- x[["sample_name"]]
    # message(sample_name)
    
    df <- read_tsv(profile_path) |>
      mutate(MS2_name = case_when(is.na(MS2_name)~MS1_name,
                                  .default =MS2_name )) |>
      select(MS2_name, abundance=last_col())|> 
      mutate(sample_name= sample_name) |> 
      select(MS2_name, abundance,sample_name) |> 
      na.omit()
    df <- df[!duplicated(df$MS2_name),]
    
    df
  })
  
  metabolism_df_long <- bind_rows(metabolism_df_list) 
  
# read_tsv("/data/RESULT/metabolism/gut_metabolism/OCC4.tsv")  |>
#     select(MS2_name,MS1_name,aa = last_col())  |>
#     mutate(MS2_name = case_when(is.na(MS2_name)~MS1_name,
#                      .default =MS2_name ))  -> a

# a[!duplicated(a$MS2_name),] |>
#   filter(MS2_name=="Uracil")
#   
# a |>
#   filter(MS1_name=="Uracil") 
# intersect(a$MS2_name,a$MS1_name)
# interaction(metabolism_metadata$sample_name,colnames(microbiome_df_long))
  
  metabolism_df <- metabolism_df_long %>%
    pivot_wider(names_from = sample_name, values_from = abundance) |>
    mutate(across(where(is.numeric), ~replace_na(., 0))) |>
    column_to_rownames("MS2_name") 
  
  write_tsv(rownames_to_column(metabolism_df,"feature"),file = str_glue("{output_path}/metabolism.table.download.tsv"))
  
  metabolism_fit_data = Maaslin2(input_data     = t(metabolism_df) |> as.data.frame(), 
                                 input_metadata = column_to_rownames(metabolism_metadata,"sample_name"), 
                                 plot_scatter =F,
                                 min_prevalence = 0,
                                 normalization  = "NONE",
                                 output         = ".", 
                                 fixed_effects  = c("select_group"),
                                 reference      = c(paste0("select_group,",data$groups_name$metabolism_control)))  
  
  sig_thresh <-  data$metabo_sig_thresh
  effect_cutoff <- data$metabo_effect_cutoff
  
  
  metabolism_sig <- metabolism_fit_data$results |>
    mutate(sig_value = .data[[data$metabo_sig_type]]) |>
    mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(coef) > effect_cutoff ,
                                     ifelse(coef>0,"Up","Down"),"NS"),
                              levels = c("Up","Down","NS") 
    )) 
  tbl <- table(metabolism_sig$direction)
  diffSummary(
    tbl = tbl,
    criteria = str_glue("{data$metabo_sig_type} < {sig_thresh} & |coef| > {effect_cutoff}"),
    title = "Metabolism summary",
    filename = "metabolism"
  )
  
  
  # msg <- paste0("metabolism(",data$metabo_sig_type,"<",sig_thresh," & abs(coef)>",effect_cutoff,") Up:",stat["Up"]," Down:",stat["Down"]," NS:",stat["NS"])
  # log(msg)
  
  
  feature_sig <- filter(metabolism_sig,direction!="NS") |> pull("feature") 
  feature <- filter(microbiome_sig,direction!="NS") |>pull("feature")
  

  metabolism_df_res <- metabolism_df |>
    rownames_to_column("feature0") |>
    mutate(feature =feature0, feature0=make.names(feature0)) |>
    filter(feature0 %in%feature_sig ) |> 
    column_to_rownames("feature") |>
    select(-feature0)
  
  # rownames(metabolism_df_res) |>
  # str_replace_all(",",".") |>
  # paste0(collapse = ", ") |> write(
  #   file = str_glue("{output_path}/microorganism({data$micro_sig_type}<{sig_thresh}&abs(coef)>{effect_cutoff}).feature.list"))
  
}







if(!is.null(data$query_metabolome) && data$query_metabolome!=""){
  metabolome_list = str_split(data$query_metabolome, ",")[[1]]
  intersect_names <- intersect(metabolome_list,rownames(metabolism_df))
  metabolism_df_res <- metabolism_df[intersect_names,]
  setdiff(metabolome_list,rownames(metabolism_df) )
  message(nrow(metabolism_df_res),"-",length(metabolome_list))
}

setdiff(metabolome_list,rownames(metabolism_df))
# metabolism_df_res <- metabolism_df[metabolism_sig$feature,]

common_samples <- intersect(colnames(metabolism_df_res),colnames(microbiome_df_res))

msg <- paste0("metabolism sample size: ",length(colnames(metabolism_df_res)), " microbiome sample size: ",length(colnames(microbiome_df_res)))
msg
log(msg)
log(" intersect size: ", length(common_samples))
# metabolism <- read_csv("/data/RESULT/metabolism/Sample_data2.csv")
# metabolism_column <- colnames(metabolism) 
# 
# metabolism <- metabolism %>%
#   rename_with(~ str_replace(.x, "MCC", "ACC"))
# metabolite_df <- metabolism[c("MS2_name",metadata$sample_name)]  |>
#   
#   drop_na() |>
#   column_to_rownames("MS2_name")

# metabolism0[duplicated(metabolism0$MS2_name),]

# common_samples <- intersect(colnames(microbiome_df), colnames(metabolite_df))
metabolism_df_res <- metabolism_df_res[, common_samples]
microbiome_df_res <- microbiome_df_res[, common_samples]
dim(metabolism_df_res)
dim(microbiome_df_res)


# 转置矩阵：rcorr 要求行为样本，列为变量
microbiome_t <- t(microbiome_df_res)
metabolite_t <- t(metabolism_df_res)

# 计算 Spearman 相关性
res <- rcorr(as.matrix(metabolite_t), as.matrix(microbiome_t), type = "spearman")


n_metab <- ncol(metabolite_t)
n_micro <- ncol(microbiome_t)
corr_matrix <-  res$r[colnames(metabolite_t),colnames(microbiome_t)]
# corr_matrix <- res$r[1:n_metab, (n_metab + 1):(n_metab + n_micro)]
p_matrix <- res$P[colnames(metabolite_t),colnames(microbiome_t)]



{
  
  p_values <- as.vector(p_matrix)
  p_values_no_na <- p_values[!is.na(p_values)]
  
  # 使用 FDR 校正（或改为 "bonferroni"）
  p_adjusted <- p.adjust(p_values_no_na, method = data$p_adjust_method)
  
  # 把校正后的 p 值重新填回矩阵形状
  p_adj_matrix <- matrix(NA, nrow = nrow(p_matrix), ncol = ncol(p_matrix))
  p_adj_matrix[!is.na(p_matrix)] <- p_adjusted
  
  # 保留行列名
  rownames(p_adj_matrix) <- rownames(p_matrix)
  colnames(p_adj_matrix) <- colnames(p_matrix)
}

write_tsv(as.data.frame(corr_matrix) |> rownames_to_column("name"),file = "output/corr_matrix.tsv")
write_tsv(as.data.frame(p_adj_matrix)|> rownames_to_column("name"),file = "output/p_adj_matrix.tsv")


# 标记显著性 (p<0.05)
sig_matrix <- ifelse(p_adj_matrix < 0.01, "**",
                     ifelse(p_adj_matrix < 0.05, "*", ""))


pdf(file =paste0(output_path,"/heatmap.pdf") , width = data$heatmap_width,height = data$heatmap_height)
# 绘图
pheatmap(
  corr_matrix,
  display_numbers = sig_matrix,
  # color = colorRampPalette(c("blue", "white", "red"))(50),
  color = colorRampPalette(c("#9BBBE1", "#FFFFFF", "#F09BA0"))(100), 
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  show_rownames = data$heatmap_show_rownames,
  show_colnames = data$heatmap_show_colnames,
  fontsize_number = 10,
  main = "Metabolite - Microbiome Correlation"
)
dev.off()





