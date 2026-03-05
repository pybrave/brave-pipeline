library(tidyverse)
library(jsonlite)
library(vegan)


params <- jsonlite::fromJSON("params.json")

otu_table <- read_tsv(params$otu_table$content) |>
  column_to_rownames("otu_id")



metadata <- params$otu_table$sample |>
  select(sample_name,group) 

otu_table <-otu_table[,metadata$sample_name]

otu_table_pcoa <- otu_table|>t() |>as.data.frame()
dist_matrix <- vegdist(otu_table_pcoa, method="bray")  # Bray-Curtis 距离

pcoa_res <- cmdscale(dist_matrix, k=2, eig=TRUE)  # k=2 -> 2D
pcoa_df <- pcoa_res$points |> as.data.frame() |>
  rownames_to_column("sample_name") |>
  inner_join(metadata, by="sample_name") |>
  dplyr::rename(c(Group=group,PC1=V1,PC2=V2) )
pcoa_df |>
  write_tsv(str_glue("output/pcoa.download.tsv"))

permanova_res <- adonis2(dist_matrix ~ Group, data=pcoa_df, permutations=999)
permanova_p <- permanova_res$`Pr(>F)`[1] 



# group_colors <- setNames(
#   c("#67A9CC", "#DD9B26"), 
#   c(control_name, treatment_name)
# )
ggplot(pcoa_df, aes(x=PC1, y=PC2, color=Group, fill=Group)) +
  geom_point(size=4, shape=21, stroke=1, alpha=0.8) +  # 点
  stat_ellipse(geom="polygon", alpha=0.2, color=NA, level=0.95) +  # 置信椭圆
  # scale_color_brewer(palette="Set1") +
  # scale_fill_brewer(palette="Set1") +
  # scale_color_manual(values = group_colors) +
  # scale_fill_manual(values = group_colors) +
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
    title = str_glue("PCoA"),
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
ggsave(filename = str_glue("output/pcoa.pdf"))
