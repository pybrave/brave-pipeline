library(tidyverse)
library(vegan)
library(jsonlite)
library(ggpubr)


params_path <- "params.json"
params <- fromJSON(params_path)

df_obj <- params$otu_table
metadata <- lapply(df_obj$groups, function(x){
  df_list <- df_obj[[x]]
}) |>bind_rows() |>
  select(sample_name, group=selcted_group_name )
# params$groups_name


otu_table <- read_tsv(params$otu_table$content) |>
  column_to_rownames("otu_id")
otu_table <- otu_table[,metadata$sample_name]
otu <- t(otu_table)
estimateR = t(estimateR(otu))[,c(1,2,4)]
colnames(estimateR) = c("richness", "chao1", "ACE")



shannon = diversity(otu, index = "shannon")
simpson = diversity(otu, index = "simpson")
invsimpson = diversity(otu, index = "invsimpson")

alpha_div = cbind(estimateR, shannon, simpson, invsimpson)
alpha_div_res <- alpha_div |>
  as.data.frame()|>
  rownames_to_column("sample_name") |>
  inner_join(metadata,by="sample_name") |>
  relocate(group, .after = "sample_name") 

write_tsv(alpha_div_res, file = str_glue("output/alpha_div.tsv"))

control_name <- ifelse(!is.null(params$re_groups_name$otu_table$control),
                       params$re_groups_name$otu_table$control,
                       params$groups_name$otu_table$control)
treatment_name <- ifelse(!is.null(params$re_groups_name$otu_tables$treatment),
                         params$re_groups_name$otu_table$treatment,
                         params$groups_name$otu_table$treatment)

# name <- "shannon"
# boxplot_data <- alpha_div_res
group_colors <- setNames(
  c("#67A9CC", "#DD9B26"), 
  c(control_name, treatment_name)
)
my_comparisons <- list(c(control_name,treatment_name ))



boxplot <- function(name, boxplot_data){
  # 绘图
  p <- ggplot(boxplot_data, aes_string(x = "group", y =name, fill = "group")) +
    geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.9) +   # 去掉离群点（用 jitter 显示）
    geom_jitter(aes(color = group), width = 0.15, size = 1.6, alpha = 0.8) +
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
    labs(title =str_glue("{name}"), x = "", y = name)
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
  
  ggsave(filename =str_glue("output/{name}_{treatment_name}_vs_{control_name}.pdf"),width = 6,height = 8)
  # return(p)
}
# boxplot_data
lapply(c("richness","chao1","ACE","shannon","simpson","invsimpson"), function(x){
  boxplot(x, alpha_div_res)
  
})




