
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
library(patchwork)
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
  mutate(select_group= control_name)
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

merged_df <- df_long %>%
  pivot_wider(names_from = sample_name, values_from = abundance) |>
  mutate(across(where(is.numeric), ~replace_na(., 0))) |>
  column_to_rownames("taxonomy") 



# merged_df

# if your inputs are counts, then you can use NEGBIN and ZINB, 
# whereas, for non-count (e.g. percentage, CPM, or relative abundance) input, you can use LM and CPLM.


maaslin_metadata <-  metadata |>column_to_rownames("sample_name")
if("phenotype" %in% names(data) && !is.null(data$phenotype)){
  message("存在表型信息!",paste0(data$phenotype,collapse = ", "))
  log("add phenotype: ",paste0(data$phenotype,collapse = ", "))
  pheno <- list_path[c("sample_name","select_group",data$phenotype)] 
  pheno$select_group <- factor(pheno$select_group)
  identical(pheno$sample_name,rownames(merged_df))
  
  
  maaslin_metadata <- pheno %>%
    imap(~{
      col_type <- data$metadata_form %>% filter(name == .y) %>% pull(type)
      if(length(col_type)==0) return(.x)
      if(col_type=="continuous") as.numeric(.x)
      else if(col_type=="category") as.factor(.x)
      else .x
    }) %>%
    as.data.frame() |>
    column_to_rownames("sample_name")
  
  str(pheno)
}


fit_data = Maaslin2(input_data     = t(merged_df) |> as.data.frame(), 
                    input_metadata = maaslin_metadata, 
                    plot_scatter = F,
                    min_prevalence = 0,
                    normalization  = "NONE",
                    output         = ".", 
                    fixed_effects  = c("select_group"),
                    random_effects = data$phenotype,
                    reference      = c(paste0("select_group,",control_name)))  

all_results <-fit_data$results  #read_tsv("output/all_results.tsv")
# read_tsv("output/significant_results.tsv") 

title <- paste0(c(data$groups_name$treatment,data$groups_name$control),collapse = " vs ")


sig_feature <- all_results |>
  arrange(qval) |>
  mutate(sig_value = .data[[data$sig_type]]) |>
  mutate(sig_value  = sprintf("%.2e", sig_value )) |>
  mutate(p.signif = case_when(
    sig_value < 0.001 ~ "***",
    sig_value < 0.01  ~ "**",
    sig_value < 0.05  ~ "*",
    TRUE ~ "ns"
  ))


selcet_taxonomy_list <- str_split(data$query,",")[[1]]


# sig_feature |>
#   filter(feature==selct_taxonomy[1])

group_colors <- setNames(
  c("#67A9CC", "#DD9B26"), 
  c(control_name, treatment_name)
)
my_comparisons <- list(c(control_name,treatment_name ))


boxplot <- function(taxonomy, boxplot_data){
  # 绘图
  p <- ggplot(boxplot_data, aes(x = Group, y = logabundance, fill = Group)) +
    geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.9) +   # 去掉离群点（用 jitter 显示）
    geom_jitter(aes(color = Group), width = 0.15, size = 1.6, alpha = 0.8) +
    stat_summary(fun = median, geom = "point", shape = 23, size = 3, color = "black", fill = "white") +
    scale_fill_manual(values = group_colors) +
    scale_color_manual(values = group_colors) +
    theme_minimal(base_size = 14) +
    theme(
      title = element_text(face="bold",size = 20),
      axis.title = element_text(face="bold",size = 18),
      legend.title = element_text(face="plain",size = 18),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_text(size=14,face = "bold"),
      axis.text.x = element_text(size=14,face = "bold",angle = 45,hjust = 1),
      
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2)
    ) +
    labs(title =str_glue("{taxonomy}"), x = "", y = "Abundace")
    p=p+stat_compare_means(
      aes(group=Group),
      comparisons = my_comparisons,
      method="wilcox.test",   # 或 "t.test"
      # fontface = "bold",
      bracket.size=1,
      size = 5,
      # label="p.signif",       # 显示星号，也可用 "p.format" 显示具体 p 值
      hide.ns=TRUE           # 不显示 ns
    )
    p
 
  ggsave(filename = paste0(output_path,"/",taxonomy,"_",str_replace_all(title," ","_"),".pdf"),width = 6,height = 8)
  # return(p)
}

df_long_ <- merged_df |>
  rownames_to_column("taxonomy") |>
  pivot_longer(-c("taxonomy"),names_to = "sample_name",values_to = "abundance")

# 
# # select_taxonomy <- selcet_taxonomy_list[4]
# # select_taxonomy
# boxplot_data <- df_long_ |>
#   inner_join(metadata,by="sample_name") |>
#   dplyr::rename(c(Group=select_group)) |>
#   mutate(Group = factor(Group,levels = c(control_name,treatment_name))) |>
#   filter(taxonomy==select_taxonomy) |>
#   mutate(logabundance = log2(abundance+1)) 
# boxplot(select_taxonomy,boxplot_data )

lapply(selcet_taxonomy_list, function(select_taxonomy){
  message(select_taxonomy)
  # select_taxonomy <- selct_taxonomy[2]
  boxplot_data <- df_long_ |>
    inner_join(metadata,by="sample_name") |>
    dplyr::rename(c(Group=select_group)) |>
    mutate(Group = factor(Group,levels = c(control_name,treatment_name))) |>
    filter(taxonomy==select_taxonomy) |>
    mutate(logabundance = log2(abundance+1))
  boxplot(select_taxonomy,boxplot_data )
})

# 
# if (data$boxplot_sig_label =="Maaslin2"){
#   p=p+geom_text(
#     data = sig_feature,
#     aes(x = feature, y = max(box_data$abundance), 
#         label = p.signif),
#     inherit.aes = FALSE
#   ) 
# }else{
#  
# }
# 






