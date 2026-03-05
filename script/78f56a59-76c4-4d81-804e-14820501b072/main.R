
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
control <- data$control |>
  mutate(select_group= data$groups_name$control)
treatment <- data$treatment |>
  mutate(select_group= data$groups_name$treatment)

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
metadata <- metadata |>column_to_rownames("sample_name")
# if your inputs are counts, then you can use NEGBIN and ZINB, 
# whereas, for non-count (e.g. percentage, CPM, or relative abundance) input, you can use LM and CPLM.


maaslin_metadata <-  metadata
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
                    plot_scatter =data$plot_scatter,
                    min_prevalence = 0,
                    normalization  = "NONE",
                    output         = ".", 
                    fixed_effects  = c("select_group"),
                    random_effects = data$phenotype,
                    reference      = c(paste0("select_group,",data$groups_name$control)))  

all_results <-fit_data$results  #read_tsv("output/all_results.tsv")
# read_tsv("output/significant_results.tsv") 

title <- paste0(c(data$groups_name$treatment,data$groups_name$control),collapse = " vs ")



se <- SummarizedExperiment(
  assays = list(count=as.matrix(merged_df)),
  colData =metadata
)

set.seed(1234)
setn_ra <- relativeAb(se)
b <- assay(setn_ra)
res1 <- lefser(setn_ra, # relative abundance only with terminal nodes
               kruskal.threshold=1,
               lda.threshold=0,
               classCol = "select_group")
# dim(res1)
# dim(all_results)
df_res <-  all_results |>
  left_join(dplyr::rename(as.data.frame(res1),"feature"=features),by="feature")



# 转换 qval 为 -log10
sig_thresh <-  data$sig_thresh
effect_cutoff <- data$effect_cutoff
label_size <- data$label_size

results <- df_res %>%
  mutate(sig_value = .data[[data$sig_type]]) |>
  mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(coef) > effect_cutoff ,
                                   ifelse(coef>0,"Up","Down"),"NS"),
                            levels = c("Up","Down","NS") 
  )) 

results |> write_tsv(file = paste0(output_path,"/",str_replace_all(title," ","_"),".score.tsv"))

results_sig <- results |>
  filter(direction!="NS")


plot_df <- df_long_0 |> filter(taxonomy %in% results_sig$feature) |>
  left_join(rownames_to_column(metadata,"sample_name"),by="sample_name") |>
  select("select_group","ptaxonomy","taxonomy") |>
  unique()

df_alluv <- plot_df %>%
  group_by(select_group, ptaxonomy, taxonomy) %>%
  summarise(value = n(), .groups = "drop")

ggplot(df_alluv,
       aes(axis1 = select_group, axis2 = ptaxonomy, axis3 = taxonomy, y = value)) +
  geom_alluvium(aes(fill = ptaxonomy), width = 1/12, alpha = 0.8)+
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  
  theme_void()+
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )
ggsave( paste0(output_path,"/",str_replace_all(title," ","_"),"sankey.pdf"),height = 10,width = 10)
# # CNS 配色示例：蓝-灰-红/绿
# cns_palette <- c(
#   "#5e81ac", "#81a1c1", "#d8dee9", "#a3be8c", "#bf616a", "#4c566a"
# )
# 
# # 生成桑基图
# ggplot(df_alluv,
#        aes(axis1 = select_group, axis2 = ptaxonomy, axis3 = taxonomy, y = value)) +
#   geom_alluvium(aes(fill = ptaxonomy), width = 1/12, alpha = 0.8) + # alpha 可调透明度
#   geom_stratum(width = 1/12, fill = "grey90", color = "black", size = 0.3) +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
#   scale_x_discrete(limits = c("Group", "Phylum", "Species")) +
#   scale_fill_manual(values = cns_palette) + # CNS 风格颜色
#   theme_minimal(base_size = 12) +
#   theme_void() +
#   theme(
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     legend.position = "none"
#   )
# CNS 配色示例：蓝-灰-红/绿
# cns_palette <- c(
#   "#5e81ac", "#81a1c1", "#d8dee9", "#a3be8c", "#bf616a", "#4c566a"
# )
# 
# # 生成桑基图
# ggplot(df_alluv,
#        aes(axis1 = select_group, axis2 = ptaxonomy, axis3 = taxonomy, y = value)) +
#   geom_alluvium(aes(fill = ptaxonomy), width = 1/12, alpha = 0.8) + # alpha 可调透明度
#   geom_stratum(width = 1/12, fill = "grey90", color = "black", size = 0.3) +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
#   scale_x_discrete(limits = c("Group", "Phylum", "Species")) +
#   scale_fill_manual(values = cns_palette) + # CNS 风格颜色
#   theme_minimal(base_size = 12) +
#   theme(
#     axis.title = element_blank(),
#     panel.grid = element_blank(),
#     legend.position = "none"
#   )
# 

# 
# # 安装（如未安装）
# # install.packages("networkD3")
# 
# library(networkD3)
# 
# # 定义节点
# nodes <- data.frame(name = c("A", "B", "C", "D", "E"))
# 
# # 定义链接
# links <- data.frame(
#   source = c(0, 0, 1, 1, 2, 3),  # 节点索引，从 0 开始
#   target = c(2, 3, 3, 4, 4, 4),
#   value  = c(8, 4, 2, 8, 4, 2)
# )
# 
# # 绘制桑基图
# sankeyNetwork(Links = links, Nodes = nodes,
#               Source = "source", Target = "target",
#               Value = "value", NodeID = "name",
#               fontSize = 12, nodeWidth = 30)
# 
# 
# 
# 
# 
# 
# 
# library(dplyr)
# library(networkD3)
# 
# # 示例数据
# # df <- data.frame(
# #   select_group = c(rep("ACC", 10)),
# #   ptaxonomy    = c("p__Bacteroidota", "p__Bacteroidota", "p__Bacteroidota",
# #                    "p__Bacteroidota", "p__Bacteroidota", "p__Firmicutes",
# #                    "p__Bacteria_unclassified", "p__Proteobacteria", "p__Bacteroidota",
# #                    "p__Firmicutes"),
# #   taxonomy     = c("s__Heminiphilus_faecis", "s__Muribaculaceae_bacterium",
# #                    "s__Bacteroidales_bacterium", "s__Duncaniella_dubosii",
# #                    "s__Prevotella_sp_MGM1", "s__Lachnospiraceae_bacterium",
# #                    "s__bacterium_0_1xD8_71", "s__Burkholderiaceae_bacterium",
# #                    "s__Muribaculum_gordoncarteri", "s__Eubacteriaceae_bacterium")
# # )d
# df <- plot_df
# # -------------------------------
# # Step 1: 构建节点表
# nodes <- data.frame(name = unique(c(df$select_group, df$ptaxonomy, df$taxonomy)))
# 
# # Step 2: 构建链接表
# # select_group -> ptaxonomy
# links1 <- df %>%
#   group_by(select_group, ptaxonomy) %>%
#   summarise(value = n(), .groups = "drop") %>%
#   mutate(
#     source = match(select_group, nodes$name) - 1,
#     target = match(ptaxonomy, nodes$name) - 1
#   ) %>%
#   select(source, target, value)
# 
# # ptaxonomy -> taxonomy
# links2 <- df %>%
#   group_by(ptaxonomy, taxonomy) %>%
#   summarise(value = n(), .groups = "drop") %>%
#   mutate(
#     source = match(ptaxonomy, nodes$name) - 1,
#     target = match(taxonomy, nodes$name) - 1
#   ) %>%
#   select(source, target, value)
# 
# # 合并链接
# links <- bind_rows(links1, links2)
# 
# # -------------------------------
# # Step 3: 绘制桑基图
# sankey <- sankeyNetwork(
#   Links = links,
#   Nodes = nodes,
#   Source = "source",
#   Target = "target",
#   Value = "value",
#   NodeID = "name",
#   fontSize = 12,
#   nodeWidth = 30,
#   sinksRight = FALSE  # 叶子节点在右侧
# )
# saveWidget(sankey, "sankey.html", selfcontained = TRUE)
# webshot("sankey.html", "sankey.pdf", vwidth = 1000, vheight = 800)
# 
# library(webshot2)
# library(htmlwidgets)
# 
# library(ggalluvial)
# library(ggplot2)
# 
# # 数据
# df_alluv <- df %>%
#   group_by(select_group, ptaxonomy, taxonomy) %>%
#   summarise(value = n(), .groups = "drop")
# 
# pdf("sankey_static.pdf", width = 10, height = 6)
# ggplot(df_alluv,
#        aes(axis1 = select_group, axis2 = ptaxonomy, axis3 = taxonomy, y = value)) +
#   geom_alluvium(aes(fill = ptaxonomy), width = 1/12) +
#   geom_stratum(width = 1/12, fill = "grey", color = "black") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
#   scale_x_discrete(limits = c("Group", "Phylum", "Species")) +
#   theme_minimal()
# dev.off()


# 
# 
# ggplot(df_alluv,
#        aes(axis1 = select_group, axis2 = ptaxonomy, axis3 = taxonomy, y = value)) +
#   geom_alluvium(aes(fill = ptaxonomy), width = 1/12, alpha = 0.8)+
#   geom_stratum() +
#   geom_text(stat = "stratum",
#             aes(label = after_stat(stratum))) +
# 
#   theme_void()



# 
# ggplot(data = vaccinations,
#        aes(axis1 = survey,   # First variable on the X-axis
#            axis2 = response, # Second variable on the X-axis
#            axis3 = survey,   # Third variable on the X-axis
#            y = freq)) +
#   geom_alluvium(aes(fill = response)) +
#   geom_stratum() +
#   geom_text(stat = "stratum",
#             aes(label = after_stat(stratum))) +
#   scale_x_discrete(limits = c("Survey", "Response"),
#                    expand = c(0.15, 0.05)) +
#   theme_void()


