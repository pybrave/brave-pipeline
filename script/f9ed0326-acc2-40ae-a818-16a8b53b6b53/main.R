
# while (TRUE) {
#   Sys.sleep(1000)  # 每次休眠 1000 秒，然后继续循环
# }
{
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

}


control <- read_tsv(data$control$content) |>
  rename_with(~ "feature", .cols = 1) |>
  select(c("feature",data$control$columns$columns_name))

treatment <- read_tsv(data$treatment$content) |>
  rename_with(~ "feature", .cols = 1) |>
  select(c("feature",data$treatment$columns$columns_name))

metadata <- rbind(
  select(data$control$columns ,c("columns_name","re_groups_name")),
  select(data$treatment$columns ,c("columns_name","re_groups_name"))
)
df <- inner_join(control,treatment,by="feature")

control_name <- data$re_groups_name$control
treatment_name <- data$re_groups_name$treatment



title <- paste0(c(data$groups_name$treatment,data$groups_name$control),collapse = " vs ")

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

df_long_ <- df |>
  pivot_longer(-c("feature"),names_to = "columns_name",values_to = "abundance")


lapply(selcet_taxonomy_list, function(select_taxonomy){
  message(select_taxonomy)
  # select_taxonomy <- selct_taxonomy[2]
  boxplot_data <- df_long_ |>
    inner_join(metadata,by="columns_name") |>
    dplyr::rename(c(Group=re_groups_name)) |>
    mutate(Group = factor(Group,levels = c(control_name,treatment_name))) |>
    filter(feature==select_taxonomy) |>
    mutate(logabundance = log2(abundance+1))
  boxplot(select_taxonomy,boxplot_data )
})








