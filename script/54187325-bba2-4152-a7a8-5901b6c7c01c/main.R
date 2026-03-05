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

library(circlize)
library(RColorBrewer)
library(ComplexHeatmap)
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
term_annotation <- data$gmm_annotation$path

anno <- read_tsv(term_annotation) |>
  `colnames<-`(c("term","anno"))






control <- data$control |>
  mutate(select_group= data$groups_name$control)
treatment <- data$treatment |>
  mutate(select_group= data$groups_name$treatment)

list_path <- rbind(control, treatment)

metadata <- list_path[c("sample_name","select_group")]

read_abundabce <- function(path){
  df <-  read_tsv(path,comment = "#",col_names =F)
  colnames(df) <- c("clade_name","NCBI_
                    tax_id","abundance","additional_species")
  df <- select(df,c("clade_name",all_of("abundance")))
  df
}





df_list <- apply(list_path,1, function(x){
  profile_path <- x[["GMM"]]
  sample_name <- x[["sample_name"]]
  df <- read_tsv(profile_path)|>
    select(-Coverage) |>
    `colnames<-`(c("term","abundance")) |>
    mutate(sample_name = sample_name)
  
  df
})
# read_tsv("/ssd1/wy/workspace2/nextflow_workspace/289364b1-295c-4710-833e-d68ec7c8918e/e2e9e213-8fe9-4d21-a75f-42671c865807/111b9ba1-64ad-4e47-a717-1af5b6af0e2a/output/humann/ACC1/GMM/ACC1_Abundance-RPKs.modules")

df_long <- bind_rows(df_list)

merged_df <- df_long %>%
  pivot_wider(names_from = sample_name, values_from = abundance) |>
  mutate(across(where(is.numeric), ~replace_na(., 0))) |>
  column_to_rownames("term") 

maaslin_metadata <-  column_to_rownames(metadata, "sample_name")
if("phenotype" %in% names(data) && !is.null(data$phenotype) ){
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
                    input_metadata =maaslin_metadata , 
                    plot_scatter =data$plot_scatter,
                    min_prevalence = 0,
                    normalization  = "NONE",
                    output         = ".", 
                    fixed_effects  = c("select_group"),
                    random_effects = data$phenotype,
                    reference      = c(paste0("select_group,",data$groups_name$control)))  
all_results <-fit_data$results |>
  left_join(dplyr::rename(anno, feature=term),by="feature") |>
  inner_join(rownames_to_column(merged_df,"feature"),by="feature")


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

df_res |>
  relocate(anno, .after  = "feature") |>
  write_tsv(file = paste0(output_path,"/",str_replace_all(title," ","_"),".tsv"))




# 转换 qval 为 -log10
sig_thresh <-  data$sig_thresh
effect_cutoff <- data$effect_cutoff
label_size <- data$label_size

all_results <- all_results %>%
  mutate(sig_value = .data[[data$sig_type]]) |>
  mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(coef) > effect_cutoff ,
                                   ifelse(coef>0,"Up","Down"),"NS"),
                            levels = c("Up","Down","NS") 
  )) 
counts <- table(all_results$direction)
labels <- c(
  Down = paste0("Down (", counts["Down"], ")"),
  NS   = paste0("NS (", counts["NS"], ")"),
  Up   = paste0("Up (", counts["Up"], ")")
)

ggplot(all_results, aes(x= coef, 
                        y = -log10(sig_value), 
                        colour=direction)) +
  geom_point(alpha=0.9, size=3.5)+
  scale_color_manual(values=c("Down" = "#3B4992FF", "NS"="#d2dae2","Up"="#EE0000FF"),  labels = labels)+
  geom_vline(xintercept=c(-effect_cutoff,effect_cutoff),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = -log10(sig_thresh),lty=4,col="black",lwd=0.8)+
  ggtitle(paste0("volcano plot of ",title)) +
  labs(x="Effect Size (Coefficient)", y="-log10(q-value)")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(color = 'black',size = 10, family = 'sans', face = 'plain'),
        axis.title = element_text(color = 'black',size = 15, family = 'sans', face = 'plain'),
        legend.position="right", 
        legend.title = element_blank()
  ) + 
  geom_text_repel(data=all_results %>% filter(direction!="NS") |> head(label_size),aes(label = anno), 
                  colour = "black", size = 4)


ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),".pdf"))






sig_all_results <- all_results |>
  filter(direction!="NS")



# 安装依赖
# install.packages(c("circlize", "RColorBrewer"))
# library(circlize)
# library(RColorBrewer)
# library(ComplexHeatmap)
# 
# # 模拟示例数据
# data <- data.frame(
#   pathway = c("Propionate synthesis III", "DOPAC synthesis", "Acetate degradation",
#               "Acetate synthesis II", "Butyrate synthesis II", "CH3 ATP trans",
#               "Acetate synthesis III", "Propionate synthesis II"),
#   gene = c("MGB055", "MGB024", "MGB531", "MGB041", "MGB035", "MGB079", "MGB045", "MGB054"),
#   coef = c(2.0, -1.5, 1.2, 0.8, -2.0, 1.0, 0.5, -1.0),
#   pvalue = c(0.001, 0.05, 0.01, 0.02, 0.0005, 0.04, 0.00005, 0.03)
# )
# data <- sig_all_results
circos_bar <- function(data){
  data$logp <- -log10(data$sig_value)
  
  # 调色板
  # coef_colors <- colorRampPalette(c("#377EB8", "white", "#E41A1C"))(100)  # 蓝-白-红
  # pval_colors <- colorRampPalette(c("red","white", "#4DAF4A"))(100)             # 白-绿渐变
  # coef_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(3)  # 蓝-白-红
  # pval_palette <- colorRampPalette(brewer.pal(9, "YlGn"))(3)    # 黄-绿渐变
  coef_palette <- c("#5e81ac", "#d8dee9", "#bf616a")  # 蓝-灰-红
  
  # pvalue: 小 p 值浅灰，显著 p 值深绿色
  pval_palette <- c("#eceff4", "#a3be8c", "#4c566a")  # 浅灰-中绿-深灰
  
  p_col_fun <- colorRamp2(seq(min(data$logp), max(data$logp), length.out = length(pval_palette)), pval_palette)
  coef_col_fun <- colorRamp2(seq(min(data$coef), max(data$coef), length.out = length(coef_palette)), coef_palette)
  
  data$coef_color <- coef_col_fun(data$coef) #coef_colors[scale_to_range(data$coef)]
  data$pval_color <- p_col_fun(data$logp)#pval_colors[scale_to_range(data$logp)]
  
  # barplot(rep(1, length(data$pval_color)), col = data$pval_color, border = NA, space = 0)
  
  sectors <- unique(data$anno)
  # 自动生成分类颜色
  n <- length(sectors)
  # sector_colors <- setNames(brewer.pal(n = max(3, n), name = "Set3")[1:n], sectors)
  sector_colors <- setNames(colorRampPalette(brewer.pal(12, "Set3"))(n), sectors)
  
  # 初始化绘图
  
  
  pdf(paste0(output_path,"/",str_replace_all(title," ","_"),"_circos_plot.pdf"), width = 12, height = 10) 
  circos.clear()
  # sectors <- data$pathway
  circos.initialize(factors = sectors, xlim = c(0, 1))
  
  circos.trackPlotRegion(
    factors = sectors, ylim = c(0, 1),
    bg.border = NA,
    track.height = 0.1,
    panel.fun = function(x, y) {
      sector = get.cell.meta.data("sector.index")
      val = data[data$anno == sector, ]
      circos.text(
        x = 0.5, y = 0.5,               # 中心位置
        labels = sector,    # 你可以换成 "111" 或 val$gene 等
        cex = 0.6,                      # 字体大小
        col = "#000",                  # 字体颜色
        facing = "inside",              # 文字方向
        niceFacing = TRUE
      )
    }
  )
  
  
  circos.trackPlotRegion(
    factors = sectors, ylim = c(0, 1),
    bg.border = NA,
    track.height = 0.1,
    panel.fun = function(x, y) {
      sector = get.cell.meta.data("sector.index")
      val = data[data$anno == sector, ]
      
      # 绘制矩形
      circos.rect(0, 0, 1, 1,
                  col =sector_colors[sector],
                  border = NA)
      
      # 在矩形中间添加文字
      circos.text(
        x = 0.5, y = 0.5,               # 中心位置
        labels = val$feature,    # 你可以换成 "111" 或 val$gene 等
        cex = 0.5,                      # 字体大小
        col = "#000",                  # 字体颜色
        facing = "inside",              # 文字方向
        niceFacing = TRUE
      )
    }
  )
  
  circos.trackPlotRegion(
    factors = sectors, ylim = c(0, 1),
    bg.border = NA,
    track.height = 0.1,
    panel.fun = function(x, y) {
      sector = get.cell.meta.data("sector.index")
      val = data[data$anno == sector, ]
      circos.rect(0, 0, 1, 1,
                  col = val$pval_color,
                  border = NA)
    }
  )
  
  circos.trackPlotRegion(
    factors = sectors, ylim = c(0, max(abs(data$coef))),  # 设置 y 轴范围覆盖正负值
    bg.border = NA,
    track.height = 0.4,
    bg.col = "#F5F5F5", 
    panel.fun = function(x, y) {
      sector = get.cell.meta.data("sector.index")
      val = data[data$anno == sector, ]
      
      # bar 长度等于 coef 值，方向随正负
      circos.rect(
        xleft = 0.1, xright = 0.9,      # 控制 bar 的宽度
        ybottom = 0, ytop = abs(val$coef),   # 从 0 到 coef
        col = val$coef_color,
        border = NA
      )
    }
  )
  
  lgd1 <- Legend(title = "-log10(pvalue)", col_fun = p_col_fun )
  lgd2 <- Legend(title = "coef", col_fun = coef_col_fun)
  
  # draw(lgd1, x = unit(1, "cm"), y = unit(1, "cm"))
  # draw(lgd2, x = unit(3, "cm"), y = unit(1, "cm"))
  lgd <- packLegend(lgd1, lgd2)
  
  # draw(lgd, x = unit(1, "npc") , 
  #      y = unit(1, "npc") - unit(3, "cm"), just = c("right", "top"))
  
  # 使用 npc 坐标，相对位置
  draw(lgd,
       x = unit(1, "npc"),    # 右侧
       y = unit(0.5, "npc"),  # 垂直居中
       just = c("right", "center"))
  dev.off()
}

circos_bar(sig_all_results)


feature_list <- all_results |> arrange(pval) |>pull(feature)
df_long |>
  filter(term == feature_list[1]) |>
  inner_join(metadata,by="sample_name") |>
  mutate(select_group =factor(select_group, 
                              levels = c(data$groups_name$treatment,data$groups_name$control)) ) |>
  ggplot(aes(x=select_group, y=abundance )) +
  geom_boxplot(fill = "skyblue", color = "black") +
  geom_point()+
  theme_bw()+
  ggtitle(paste0(all_results[all_results$feature==feature_list[1],c("Pathway_Name")],
                 "\n",title," coef: ",
                 all_results[all_results$feature==feature_list[1],c("coef")]))
ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),".boxplot.pdf"))

ann_colors <- list(
  group = setNames(
    c("steelblue", "tomato"),  # 颜色向量
    c( data$groups_name$control, data$groups_name$treatment)  # 用变量的值作为名字
  )
)

# dev.off()
pdf(file =paste0(output_path,"/",str_replace_all(title," ","_"),".heatmap.pdf") , width = data$heatmap_width,height =8)

log2(merged_df[head(feature_list, n=30),]+1) |>
  rownames_to_column("term") |>
  left_join(select(anno,term,anno),by="term") |>
  select(-term) |>
  column_to_rownames("anno")|>
  pheatmap(scale = "row", 
           annotation_colors = ann_colors,
           cluster_cols = data$heatmap_cluster_cols,
           show_colnames = data$heatmap_show_colnames,
           color =  colorRampPalette(c("darkred", "#FFFFFF","darkblue"))(255),
           annotation_col=column_to_rownames(metadata,"sample_name") |> dplyr::rename(group=select_group)
  )
dev.off()




sig_feature <- df_res |>
  arrange(qval) |>
  head(n=data$boxplot_num) |>
  mutate(sig_value = .data[[data$sig_type]]) |>
  mutate(p.signif = case_when(
    sig_value < 0.001 ~ "***",
    sig_value < 0.01  ~ "**",
    sig_value < 0.05  ~ "*",
    TRUE ~ "ns"
  ))
box_data <-  df_long |>
  mutate(abundance=log2(abundance+data$boxplot_pseudo_count))|>
  filter(term %in% pull(sig_feature,"feature")) |>
  left_join(anno,by="term") 

# log2(t(merged_df)[head(feature_list, n=30),]+1) 
p <- box_data |>
  inner_join(metadata,by="sample_name") |>
  ggplot( aes(x=anno, y=abundance, fill=select_group)) +
  geom_boxplot(position=position_dodge(0.8), width=0.6, outlier.shape=NA) + # 去掉离群点，箱子更美观
  geom_jitter(aes(color=select_group), 
              position=position_jitterdodge(jitter.width=0.2, dodge.width=0.8), 
              alpha=0.5, size=1.5) +  # 添加散点，更直观
  labs(title=paste0("Boxplot of ",title), 
       x="", 
       y="log2(abundance)") +
  scale_fill_brewer(palette="Set2") +   # 柔和调色板
  scale_color_brewer(palette="Set2") +  # 散点与箱子颜色一致
  theme_bw(base_size=12) +
  theme(
    axis.text.x = element_text(angle=80, vjust=1, hjust=1, size=10, face="italic"), 
    axis.title.x = element_text(size=12, face="bold"),  
    axis.title.y = element_text(size=12, face="bold"),  
    plot.title = element_text(hjust=0.5, size=14, face="bold"),  
    panel.grid.major.x = element_blank(),   # 去掉竖网格线
    panel.grid.minor = element_blank(),  
    legend.title = element_blank(),        # 去掉图例标题
    legend.position = "top"                # 图例放上方
  )
p
if (data$boxplot_sig_label =="Maaslin2"){
  p=p+geom_text(
    data = sig_feature,
    aes(x = anno, y = max(box_data$abundance), 
        label = p.signif),
    inherit.aes = FALSE
  ) 
}else{
  p=p+stat_compare_means(
    aes(group=select_group),
    method="wilcox.test",   # 或 "t.test"
    label="p.signif",       # 显示星号，也可用 "p.format" 显示具体 p 值
    hide.ns=TRUE           # 不显示 ns
  )
}
p


ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),"multigroup.boxplot.pdf"),plot = p,width = 15,height = 8)


# data(mtcars)
# library(ggplot2)
# ggplot(mtcars, aes(x = wt, y = mpg, size = hp)) +
#   geom_point()

box_data



# 
# 
# df <- data.frame(
#   Level1 = c("Metabolism", "Metabolism", "Human Diseases"),
#   Level2 = c("Carbohydrate metabolism", "Lipid metabolism", "Cancer"),
#   Pathway = c("Glycolysis / Gluconeogenesis", "Fatty acid metabolism", "Central carbon metabolism in cancer"),
#   Count = c(12, 8, 5),          # 富集到的基因数
#   FDR = c(1e-4, 2e-3, 5e-2),    # 校正 p 值
#   Coef = c(-1.2, 0.3, 0.8)      # 统计量（例如回归系数）
# )
# library(ggplot2)
# library(ggalluvial)
# 
# dat <- to_lodes_form(df,
#                      axes = c("Level1", "Level2", "Pathway"),
#                      key = "stage", value = "node", id = "id")
# 
# p1 <- ggplot(dat,
#              aes(x = stage, stratum = node, alluvium = id,
#                  y = Count, fill = Level1)) +
#   geom_flow(stat = "alluvium", alpha = 0.4) +
#   geom_stratum(width = 0.25, color = "grey30") +
#   geom_text(stat = "stratum", aes(label = node), size = 3) +
#   scale_x_discrete(expand = c(.1, .1)) +
#   guides(fill = "none") +
#   theme_void(base_size = 12) +
#   theme(axis.text.y = element_blank(),
#         axis.text.x = element_blank())
# p2 <- ggplot(dat, aes(x = Coef, y = Pathway)) +
#   geom_point(aes(size = Count, color = -log10(FDR))) +
#   scale_color_gradient(low = "green", high = "magenta") +
#   theme_bw(base_size = 12) +
#   labs(x = "Coef", y = NULL, color = "-log10(FDR)", size = "Gene count")
# library(patchwork)
# 
# p1 + p2 + plot_layout(widths = c(2,1))
# 
# 
# 
# 
# library(ggplot2)
# library(ggalluvial)
# library(dplyr)
# 
# # Example data
# df <- data.frame(
#   Stage1 = c("A","A","B","B"),
#   Stage2 = c("X","Y","X","Y"),
#   Count  = c(10, 5, 15, 20)
# )
# 
# # Plot
# ggplot(df,
#        aes(axis1 = Stage1, axis2 = Stage2, y = Count)) +
#   geom_alluvium(aes(fill = Stage1), width = 1/12) +
#   geom_stratum(aes(fill = Stage1), width = 1/8, color = "black") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme_minimal()
# # Compute node sizes
# nodes <- df %>%
#   pivot_longer(cols = c(Stage1, Stage2), values_to = "Node") %>%
#   group_by(Node) %>%
#   summarise(Size = sum(Count))
# 
# # Add bubbles
# ggplot(df,
#        aes(axis1 = Stage1, axis2 = Stage2, y = Count)) +
#   geom_alluvium(aes(fill = Stage1), width = 1/12) +
#   geom_point(data = nodes, aes(x = Node, y = Size, size = Size), inherit.aes = FALSE) +
#   geom_text(data = nodes, aes(x = Node, y = Size, label = Node), inherit.aes = FALSE) +
#   theme_minimal()
# 



# 
# # 
# 
# sig_feature <- df_res |>
#   arrange(qval) |>
#   head(n=20) |>
#   mutate(sig_value = .data[[data$sig_type]]) 
# box_data <-  df_long |>
#   mutate(abundance=log2(abundance+data$boxplot_pseudo_count))|>
#   filter(term %in% pull(sig_feature,"feature")) |>
#   left_join(anno,by="term") 
# pathway_levels <- unique(c(sig_feature$LevelA,sig_feature$Pathway_Name))
# 
# box_data <- box_data|>
#   mutate(LevelA = factor(LevelA)) %>%
#   mutate(Pathway_Name = factor(Pathway_Name))
# dfSankey = to_lodes_form(box_data %>% select(c("LevelA","Pathway_Name")),
#                          key = "x",
#                          axes = c(1,2)) %>%
#   mutate(flowColor = rep(box_data$Pathway_Name,2))
# # 绘制桑基图
# sankeyPlot=ggplot(data = dfSankey,
#                   aes(x = x,
#                       stratum = factor(stratum,levels = rev(pathway_levels)),
#                       alluvium = alluvium,
#                       y = 1,
#                       label = stratum,
#                       fill = stratum
#                   )) +
#   scale_y_discrete(expand = c(0, 0)) +
#   geom_flow(aes(fill = flowColor),alpha = 0.3, width = 0, knot.pos = 0.1) +
#   geom_stratum(width = 0.05, color = "white") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3,
#             hjust = 1, nudge_x = -0.03) +
#   guides(fill = FALSE, color = FALSE) +
#   theme_minimal() +
#   labs(title = "", x = "", y = "") +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     plot.margin = unit(c(0, 0, 0, 0), units = "cm")
#   )+
#   scale_x_discrete(expand = c(1, 0, 0, 0))
# # ggstyle::scale_fill_sci(palette="d3.category20")
# sankeyPlot
# 
# # 准备气泡图数据
# bubbleDf = sig_feature %>%
#   mutate(term_num = row_number()) |>
#   mutate(term_num =   term_num-0.5) 
# 
# dot_plot <- ggplot(bubbleDf, aes(x = coef, y = factor(Pathway_Name,levels = pathway_levels), color = -log10(sig_value))) +
#   geom_point(aes(size = abs(scores))) +
#   scale_color_gradient(low = "blue", high = "red") +
#   labs(x = "Coefficient", y = "", color = "-log10(sig_value)", size = "Score") +
#   theme_minimal()+
#   theme(
#     axis.text.y  = element_blank(),   # 去掉 y 轴文字
#     axis.ticks.y = element_blank(),   # 去掉 y 轴刻度
#     axis.title.y = element_blank(),   # 去掉 y 轴标题
#     axis.line.y  = element_blank(),   # 去掉 y 轴线
#     plot.margin  = margin(0, 0, 0, 0) # 去掉图的外边距
#   )
# dot_plot
# p= sankeyPlot + dot_plot +
#   plot_layout(widths = c(2, 1))
# p
# ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),"sankey_dot.pdf"),plot = p, width = data$sankey_dot_width,height =8)
term_levels <- unique(c(sig_feature$anno))
p <- ggplot(sig_feature, aes(x = coef, y = factor(anno,levels = term_levels), color = -log10(sig_value))) +
  geom_point(aes(size = abs(scores))) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Coefficient", y = "", color = "-log10(sig_value)", size = "Score") +
  theme_minimal()

p
ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),"dot.pdf"),plot = p,width = data$dot_width,height =8)


# dot_plot <- ggplot(bubbleDf, aes(x = coef, y = term_num,color=-log10(sig_value))) +
#   geom_point(aes(size = abs(scores))) +
#   scale_y_continuous(expand = c(0, 0),limits =c(0,nrow(bubbleDf)))+
#   scale_color_gradient(low = "red", high = "#1000ee") +
#   scale_radius(
#     range=c(2,5),
#     name="Size")+
#   guides(
#     color = guide_colorbar(order = 1),        # 决定图例的位置顺序
#     size = guide_legend(order = 2)
#   )+
#   theme_bw() +
#   labs(size = "Count", color = "PValue", y = "", x = "Ratio") +
#   theme(
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.title.y = element_blank(),
#     plot.margin = unit(c(0, 0, 0, 0), "inches"),
#     panel.grid = element_blank()
#   )

# 
# # 代码来源：https://www.r2omics.cn/
# library(tidyverse)
# library(ggalluvial)
# library(patchwork)
# library(ggstyle) # 开发版安装方式devtools::install_github("sz-zyp/ggstyle")
# 
# # 读取数据
# df = read.delim("http://r2omics.cn/res/demodata/sankeyBubble.txt")
# 
# #　整理数据
# dfLong = df %>%
#   separate_rows(IDs,sep = ",") %>%
#   mutate(Term = factor(Term)) %>%
#   mutate(IDs = factor(IDs))
# 
# # 准备桑基图数据
# dfSankey = to_lodes_form(dfLong %>% select(c("IDs","Term")),
#                          key = "x",
#                          axes = c(1,2)) %>%
#   mutate(flowColor = rep(dfLong$Term,2))
# 
# # 绘制桑基图
# sankeyPlot=ggplot(data = dfSankey,
#                   aes(x = x,
#                       stratum = factor(stratum,levels = unique(stratum)),
#                       alluvium = alluvium,
#                       y = 1,
#                       label = stratum,
#                       fill = stratum
#                   )) +
#   scale_y_discrete(expand = c(0, 0)) +
#   geom_flow(aes(fill = flowColor),alpha = 0.3, width = 0, knot.pos = 0.1) +
#   geom_stratum(width = 0.05, color = "white") +
#   geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3,
#             hjust = 1, nudge_x = -0.03) +
#   guides(fill = FALSE, color = FALSE) +
#   theme_minimal() +
#   labs(title = "", x = "", y = "") +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     plot.margin = unit(c(0, 0, 0, 0), units = "cm")
#   )+
#   scale_x_discrete(expand = c(0.2, 0, 0, 0))
#   # ggstyle::scale_fill_sci(palette="d3.category20")
# sankeyPlot
# 
# # 准备气泡图数据
# bubbleDf = df %>%
#   mutate(Term = factor(Term,levels = rev(df$Term))) %>%
#   arrange(Term) %>%
#   mutate(Term_num = cumsum(Count) - Count / 2)
# 
# # 绘制气泡图
# dot_plot <- ggplot(bubbleDf, aes(x = Ratio, y = Term_num,color=PValue)) +
#   geom_point(aes(size = Count)) +
#   scale_y_continuous(expand = c(0, 0),limits =c(0,sum(bubbleDf$Count,na.rm = T)))+
#   scale_color_gradient(low = "red", high = "#1000ee") +
#   scale_radius(
#     range=c(2,5),
#     name="Size")+
#   guides(
#     color = guide_colorbar(order = 1),        # 决定图例的位置顺序
#     size = guide_legend(order = 2)
#   )+
#   theme_bw() +
#   labs(size = "Count", color = "PValue", y = "", x = "Ratio") +
#   theme(
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.title.y = element_blank(),
#     plot.margin = unit(c(0, 0, 0, 0), "inches"),
#     panel.grid = element_blank()
#   )
# 
# # 合并桑基图和气泡图
# sankeyPlot + dot_plot +
#   plot_layout(widths = c(2, 1))
