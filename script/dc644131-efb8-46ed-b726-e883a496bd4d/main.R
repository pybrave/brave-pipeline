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
library(showtext)

showtext_auto()


params <- fromJSON("params.json")

# 
# x_feature_list <- NULL
# if(!is.null(params$query_x_feature) && params$query_x_feature!=""){
#   x_feature_list <- str_split(params$query_x_feature,",")[[1]]
# }
# y_feature_list <- NULL
# if(!is.null(params$query_y_feature) && params$query_y_feature!=""){
#   y_feature_list <- str_split(params$query_y_feature,",")[[1]]
# }
# 
is_continuous <- function(x, max_levels = 5) {
  is.numeric(x) &&
    length(unique(x[!is.na(x)])) > max_levels
}

handle_missing_continuous <- function(
    X,
    strategy = c("median", "mean", "complete_case"),
    max_levels = 5
) {
  strategy <- match.arg(strategy)
  
  X_select <- X
  
  na_cols <- names(X_select)[colSums(is.na(X_select)) > 0]
  message(strategy)
  if (strategy == "complete_case") {
    X <- X_select|> na.omit()
    return(X)
  }
  
  continuous_na_cols = c()
  # 只处理连续变量
  for (col in na_cols) {
    x <- X[[col]]
    
    if (is_continuous(x, max_levels)) {
      continuous_na_cols <- c(continuous_na_cols,col )
      if (strategy == "median") {
        X[[col]][is.na(x)] <- median(x, na.rm = TRUE)
      }
      if (strategy == "mean") {
        X[[col]][is.na(x)] <- mean(x, na.rm = TRUE)
      }
    }
  }
  na_cols_str <- paste0(na_cols,collapse = ", ")
  continuous_na_cols_str <- paste0(continuous_na_cols,collapse = ", ")
  sink(file = str_glue("output/missing_handling.txt"))
  str_glue("na_cols: {na_cols_str}") |>print()
  str_glue("continuous_na_cols: {continuous_na_cols_str} with {strategy}") |>print()
  sink()
  
  X
}


# params$x_file$feature$columns_name
# name <- "input"
# axis <- "x_axis"
# get_df <- function(name,axis){
#   
# 
#   
#   # df <- read_tsv(params[[name]]$content)
# 
#   
#   # df_select <- df_select[!duplicated(df_select[[feature]]),] |>
#   #   column_to_rownames(feature)
#   
# 
#   
#   # metabolite1_exp_matrix <- metabolite1_exp|>
#   #   column_to_rownames("MS2_name")
#   # matrix <- data.frame()
#   # if(!is.null(feature_list)){
#   #   intersect_feature_list <-  intersect(feature_list,rownames(df_select))
#   #   matrix <- df_select[intersect_feature_list,] |>
#   #     rownames_to_column("feature")|>
#   #     mutate(feature = str_glue("{prefix}{feature}"))|>
#   #     column_to_rownames("feature")
#   # }else{
#   #   matrix <- df_select |>
#   #     rownames_to_column("feature")|>
#   #     mutate(feature = str_glue("{prefix}{feature}"))|>
#   #     column_to_rownames("feature")
#   # }
# 
#     
#   return(df_select)
# }


feature <- params$input$feature$columns_name
grouped <- params$input$grouped$columns_name
sorted <- params$input$sorted$columns_name
sample <- params$input$samples$columns_name
annotation <- params$input$annotation_col$columns_name

exp <- read_tsv(params$input$content)
df_select <- exp|>
  select(all_of(sample),all_of(c(feature,grouped,sorted,sample,annotation))) |>
  column_to_rownames(sample)

df <- handle_missing_continuous(df_select, "mean")


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
heatmap_title <- params$`__heatmap_title`




df_log <- log(df + 1)
df_norm <- scale(df_log,center = T, scale = T) |> as.data.frame()
# df_norm <- apply(df_log, 2, function(x) {
#   (x - min(x, na.rm = TRUE)) /
#   (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# }) |> as.data.frame()
if(!is.null(grouped)){
  grouped_median <- median(df_norm[,grouped])
  df_norm <- df_norm |>
    as.data.frame() %>%
    mutate( grouped_ = case_when(.data[[grouped]]>= grouped_median~"high",
                                 T~"low"))|>
    rownames_to_column("sample") |>
    dplyr::group_by(grouped_) 
}
if(!is.null(sorted)){
  df_norm <- df_norm %>%
    dplyr::arrange(dplyr::desc(.data[[sorted]]), .by_group = TRUE)
}
if(!is.null(grouped)){
  df_norm <- df_norm |>
    dplyr::ungroup() |>
    column_to_rownames("sample") |>
    select(-c("grouped_"))
  
}

# |>
#   as.data.frame() %>%
  # dplyr::arrange(dplyr::desc(.data[[sorted]])) |>
  # t()

df_norm <- df_norm|> as.data.frame()

max_abs <- max(abs(df_norm), na.rm = TRUE)

breaks <- seq(-max_abs, max_abs, length.out = 101)
list_column <- c(grouped,sorted)
list_column <- list_column[!is.na(list_column)]
annotation_col <- NULL
if(!is.null(list_column)){
  annotation_col <- select(df_norm,all_of(c(grouped,sorted)))
}
if(!is.null(annotation)){
  annotation_col <- select(df_norm,all_of(c(annotation)))
  df_norm <- select(df_norm,  -all_of(annotation) )
}

pdf(file = str_glue("output/heatmap.pdf") , width =heatmap_width,height =heatmap_height)

pheatmap(
  scale = "none",
  t(df_norm),
  # display_numbers = sig_matrix,
  color = colorRampPalette(c("#9BBBE1", "#FFFFFF", "#F09BA0"))(100),
  breaks = breaks,
  cluster_rows = cluster_rows,
  cluster_cols =  cluster_cols,
  show_rownames = show_rownames,
  show_colnames = show_colnames,
  fontsize_number = 10,
  fontsize = 12,
  main = heatmap_title,
  border_color = NA,
  annotation_col=annotation_col
)
dev.off()











