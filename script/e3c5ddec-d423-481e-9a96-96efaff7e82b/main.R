library(tidyverse)
library(ggpubr)

params <- jsonlite::fromJSON("params.json")
# ss
feature <- params$input$feature$columns_name |> trimws() |> make.names()
group <- params$input$group$columns_name 

filter_sample <- c()
if(!is.null(params$filter_sample) && params$filter_sample!=""){
  filter_sample <- str_split(params$filter_sample,",")[[1]] |>trimws()
  message(params$filter_sample)
}


sample_columns_name <-  params$input$sample$columns_name
df <- read_tsv(params$input$content) %>%
  filter(!.data[[sample_columns_name]] %in% filter_sample)
colnames(df) <- make.names(colnames(df))

cat(str_glue("nrow: {nrow(df)} ncol: ncol: {ncol(df)}") , file = "output/dim.txt")

plot_data <- df |>
  select(all_of(sample_columns_name),all_of(feature),group= all_of(group)) |>
  mutate(group=as.factor(group)) |>
  na.omit()

df_long <- plot_data %>%
  pivot_longer(cols =-c(all_of(sample_columns_name), group), names_to = "item", values_to = "value")


if(!is.null(params$max_value) && params$max_value!=0 ){
  filter_df <- df_long |>
    filter(value > params$max_value)
  if(nrow(filter_df)>0){
    write_tsv(filter_df, file = "output/max_filter.tsv")
  }
}
if(!is.null(params$min_value)  && params$max_value!=0 ){
  filter_df <- df_long |>
    filter(value < params$min_value)
  if(nrow(filter_df)>0){
    write_tsv(filter_df, file = "output/min_filter.tsv")
  }  
}

# outliers <- df_long %>%
#   group_by(item, group) %>%
#   mutate(
#     Q1 = quantile(value, 0.25, na.rm =T),
#     Q3 = quantile(value, 0.75, na.rm =T),
#     IQR = Q3 - Q1,
#     is_outlier = value < (Q1 - 1.5*IQR) | value > (Q3 + 1.5*IQR)
#   ) %>%
#   filter(is_outlier)

method <- params$contStatMethod

# x <- df_long |> filter(group==1) |>pull(value)
# y <- df_long |> filter(group==2) |>pull(value)
# 
# wilcox.test(x , y)
# feature <- "SII"
plot_boxplot <- function(feature){
 p <- df_long |>
    filter(item == feature) |>
    ggplot( aes(x = group, y = value, fill = group)) +
    geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.9) +   # 去掉离群点（用 jitter 显示）
    geom_jitter(aes(color = group), width = 0.15, size = 1.6, alpha = 0.8) +
    stat_summary(fun = median, geom = "point", shape = 23, size = 3, color = "black", fill = "white") +
    # scale_fill_manual(values = group_colors) +
    # scale_color_manual(values = group_colors) +
    # theme_minimal(base_size = 14) +
    theme(
      title = element_text(face="bold",size = 20),
      axis.title = element_text(face="bold",size = 18),
      legend.title = element_text(face="plain",size = 18),
      panel.grid.major.x = element_blank(),
      axis.text.y = element_text(size=14,face = "bold"),
      axis.text.x = element_text(size=14,face = "bold",angle = 45,hjust = 1),
      
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 2)
    ) +stat_compare_means(
      aes(group=group),
      # comparisons = my_comparisons,
      method=method,   # 或 "t.test"
      # fontface = "bold",
      bracket.size=1,
      size = 5,
      # label="p.signif",       # 显示星号，也可用 "p.format" 显示具体 p 值
      hide.ns=TRUE           # 不显示 ns
    )+
 labs(title =str_glue("{feature}"), x = "", y = "")
 
  ggsave(p, filename = str_glue("output/{feature}_boxplot.png"))
}

lapply(feature, function(x){
  plot_boxplot(x)
})
# ggplot(df_long, aes(x = item, y = value, fill = group)) +
#   geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.9) +   # 去掉离群点（用 jitter 显示）
#   geom_jitter(aes(color = group), width = 0.15, size = 1.6, alpha = 0.8) +
#   stat_summary(fun = median, geom = "point", shape = 23, size = 3, color = "black", fill = "white") +
#   facet_wrap(~ item, scales = "free_y") +
#   # scale_fill_manual(values = group_colors) +
#   # scale_color_manual(values = group_colors) +
#   # theme_minimal(base_size = 14) +
#   theme(
#     title = element_text(face="bold",size = 20),
#     axis.title = element_text(face="bold",size = 18),
#     legend.title = element_text(face="plain",size = 18),
#     panel.grid.major.x = element_blank(),
#     axis.text.y = element_text(size=14,face = "bold"),
#     axis.text.x = element_text(size=14,face = "bold",angle = 45,hjust = 1),
#     
#     panel.grid.minor = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, size = 2)
#   )+stat_compare_means(
#     aes(group=group),
#     method="wilcox.test",   # 或 "t.test"
#     label="p.format",       # 显示星号，也可用 "p.format" 显示具体 p 值
#     hide.ns=TRUE           # 不显示 ns
#   ) 






