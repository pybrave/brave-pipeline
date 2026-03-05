
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
if(F){
  params_path <- "params.json"
  output_path <- "output"
}


data <- fromJSON(params_path)
abundance <- data$abundance 

list_path <- data$abundance

metadata <- list_path[c("sample_name",data$group_field)]

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

# merged_df <- df_long %>%
#   pivot_wider(names_from = sample_name, values_from = abundance) |>
#   mutate(across(where(is.numeric), ~replace_na(., 0))) |>
#   column_to_rownames("taxonomy") 

# merged_df
# metadata <- metadata |>column_to_rownames("sample_name")
group_field <- data$group_field
groups <- unique(metadata[[group_field]])
title <- paste0(groups,collapse = " vs ")
my_comparisons <- combn(groups, 2, simplify = FALSE)
query <- data$query
df <- df_long |>
  filter(taxonomy %in% query) 


max_y <- max(df$abundance, na.rm = TRUE)  # 假设 df 是前面 ggboxplot 的数据
offset <- 0.3 * max_y                 # 给点空隙

library(RColorBrewer)

# 获取 group 的水平数
n_groups <- length(unique(metadata$group))

# 自动生成调色板（比如使用 RColorBrewer）
colors <- brewer.pal(min(n_groups, 8), "Set2")

df|>
  inner_join(metadata,by="sample_name")|>
  ggboxplot( x = "group", y = "abundance",
             color = "group", palette =colors,
             ylab = query,
             xlab = "",
             title = title,
             add = "jitter", shape = "group")+
  labs(color = "Group", shape = "Group")+
  stat_compare_means(comparisons = my_comparisons)+
  stat_compare_means(label.y = max_y + offset)  

ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),".pdf"))




# merged_df <- df_long %>%
#   pivot_wider(names_from = sample_name, values_from = abundance) |>
#   mutate(across(where(is.numeric), ~replace_na(., 0))) |>
#   column_to_rownames("taxonomy") 

# df$abundance
# colSums(merged_df) |>as.data.frame()




