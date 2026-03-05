library(tidyverse)
library(lefser)
library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
print(args)

params_path <- args[1]
output_path <- args[2]
if(F){
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
    select(sample_name,taxonomy=all_of(rank),abundance) 
  
}

# sample_name <- "OCC8"
# df <- read_abundabce("/ssd1/wy/workspace2/nextflow_workspace/289364b1-295c-4710-833e-d68ec7c8918e/131f8806-35e3-4d7c-b234-f14a2119aaa7/2c88b345-822f-4285-9222-a18b9c3daa8b/output/metaphlan/OCC8/OCC8_profile.txt")
# parse_metaphlan(df,sample_name,rank) |>unique() |> dim()
# 



df_list <- apply(list_path,1, function(x){
  profile_path <- x[["profile"]]
  sample_name <- x[["sample_name"]]
  # df <-  read_tsv(term_path,comment = "#")
  # colnames(df) <- c("clade_name",sample_name)
  df <- read_abundabce(profile_path)
  df <- parse_metaphlan(df,sample_name,rank) 
  # df <- df |> filter(!grepl("\\|", term))
  df
})

df_long <- bind_rows(df_list)

merged_df <- df_long %>%
  pivot_wider(names_from = sample_name, values_from = abundance) |>
  mutate(across(where(is.numeric), ~replace_na(., 0))) |>
  column_to_rownames("taxonomy") 


# merged_df
metadata <- metadata |>column_to_rownames("sample_name")


se <- SummarizedExperiment(
  assays = list(count=as.matrix(merged_df)),
  colData =metadata
)
# colnames(merged_df)
# tn <- get_terminal_nodes(rownames(se))
# setn <- se[tn,]

# all(rownames(metadata) == colnames(merged_df)) 
# assay(se) |> head()
# b <- assay(setn)
set.seed(1234)
setn_ra <- relativeAb(se)
b <- assay(setn_ra)
res1 <- lefser(setn_ra, # relative abundance only with terminal nodes
              # kruskal.threshold=0.3,
              lda.threshold=2,
              classCol = "select_group")
dim(res1)
title <- paste0(c(data$groups_name$treatment,data$groups_name$control),collapse = " vs ")


lefserPlot(res1|> arrange(desc(abs(scores))) |> head(data$top_num),title =title)
pdf_file <- paste0(output_path,"/",str_replace_all(title," ","_"),".pdf")

ggsave(filename = pdf_file)
tsv_file <- paste0(output_path,"/",str_replace_all(title," ","_"),".tsv")

res1 |> inner_join(rownames_to_column(merged_df,"features"),by="features") |>
  write_tsv(file =tsv_file )

# data("zeller14")
# z14 <- zeller14[, zeller14$study_condition != "adenoma"]
# tn <- get_terminal_nodes(rownames(z14))
# z14tn <- z14[tn, ]
# z14tn_ra <- relativeAb(z14tn)
# z14_input <- rowNames2RowData(setn_ra)
# a <- assay(z14_input)
# resCl <- lefserClades(relab = z14_input, classCol = "select_group", lda.threshold=3)
# lefserPlotClad(df = resCl,showTipLabels =T,showNodeLabels="p",colors = "l")


# ggt


# 
# 
# data(zeller14)
# zeller14 <- zeller14[, zeller14$study_condition != "adenoma"]
# table(zeller14$age_category, zeller14$study_condition)
# assay(zeller14) |> class()
# rowData(zeller14)
# colData(zeller14)|> class()
# 
# lefser(zeller14, classCol = "study_condition", subclassCol = "age_category")
# 
# tn <- get_terminal_nodes(rownames(zeller14))
# zeller14tn <- zeller14[tn,]
# zeller14tn_ra <- relativeAb(zeller14tn)
# 
# set.seed(1234)
# res <- lefser(zeller14tn_ra, # relative abundance only with terminal nodes
#               classCol = "study_condition",
#               subclassCol = "age_category")
# head(res)
# lefserPlot(res)
# 
# lefserPlotClad(res, colors = "c", showTipLabels = FALSE, showNodeLabels = "p")
# 
# 
# data("zeller14")
# z14 <- zeller14[, zeller14$study_condition != "adenoma"]
# tn <- get_terminal_nodes(rownames(z14))
# z14tn <- z14[tn, ]
# z14tn_ra <- relativeAb(z14tn)
# z14_input <- rowNames2RowData(z14tn_ra)
# 
# resCl <- lefserClades(relab = z14_input, classCol = "study_condition")
# lefserPlotClad(df = resCl,showTipLabels =T,showNodeLabels="p")
# ggt
# 
# 
# 
# 
# 
# 
# 
# 
# se <- SummarizedExperiment(
#   assays = list(
#     counts = matrix(
#       rep(1, 4), ncol = 1, dimnames = list(LETTERS[1:4], "SAMP")
#     )
#   )
# )
# assay(se)
# assay(relativeAb(se))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# counts <- matrix(
#   c(10, 5, 3, 20, 0, 7, 
#     15, 2, 4, 30, 1, 6), 
#   nrow = 3,
#   byrow = TRUE
# )
# rownames(counts) <- c("GeneA", "GeneB", "GeneC")
# colnames(counts) <- c("Sample1", "Sample2", "Sample3", "Sample4")
# 
# # 行注释 (基因信息)
# rowData <- DataFrame(
#   gene_id = c("GeneA", "GeneB", "GeneC"),
#   pathway = c("Pathway1", "Pathway2", "Pathway1")
# )
# 
# # 列注释 (样本信息)
# colData <- DataFrame(
#   sample_id = c("Sample1", "Sample2", "Sample3", "Sample4"),
#   group = c("Control", "Control", "Case", "Case")
# )
# 
# # 创建 SummarizedExperiment 对象
# se <- SummarizedExperiment(
#   assays = counts,
#   rowData = rowData,
#   colData = colData
# )
# 
# assay(se)
# rowData(se)
# colData(se)
# 
