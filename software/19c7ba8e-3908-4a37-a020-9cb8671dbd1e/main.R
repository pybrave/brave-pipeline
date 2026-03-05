library(tidyverse)
library(jsonlite)

params_path <- "params.json"
output_path <- "output"
params <- fromJSON(params_path)


anno <- read_tsv(params$otu_anno$anno,col_names = F) |>
  na.omit() |>
  select(otu_id= X1, anno= X4)
  

# 分类顺序和列名
tax_ranks <- c("d","p","c","o","f","g","s")
tax_names <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")

# tidyverse 解析
result <- anno %>%
  # 拆分每个anno为 key:value 对
  separate_rows(anno, sep = ",") %>%
  separate(anno, into = c("rank", "taxon"), sep = ":", fill = "right") %>%
  # 保留需要的 rank 顺序
  filter(rank %in% tax_ranks) %>%
  # 填充缺失分类
  complete(otu_id, rank = tax_ranks, fill = list(taxon = "Unassigned")) %>%
  # wide format
  pivot_wider(names_from = rank, values_from = taxon) %>%
  # 按 tax_ranks 顺序重排列
  select(otu_id, all_of(tax_ranks)) %>%
  # 重命名列为标准 taxonomy 名称
  rename_with(~tax_names, -otu_id) 


write_tsv(result, file = str_glue("output/otu_anno_format.tsv"))

