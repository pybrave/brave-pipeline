library(tidyverse)
library(jsonlite)

params <- jsonlite::read_json("params.json")

gene_count <-params$gene_count


gene_count_list <- lapply(gene_count, function(x){
  sample_name <- x$sample_name
  message(x$count)
  read_tsv(x$count,comment = "#") |>
    select(feature=Geneid, Length,count=  last_col()) |>
    mutate(sample=sample_name)
  
})

df_exp <- bind_rows(gene_count_list) |>
  pivot_wider(names_from = "sample",values_from = "count",values_fill = 0) |>
  mutate(feature = str_replace(feature,"_gene",""))

annotations <- params$eggnog$annotations
df_annotations <- read_tsv(annotations,comment  = "##") |>
  dplyr::rename("feature"=`#query`)

dim(df_annotations)
dim(df_exp)
df_exp_anno <- df_exp |>
  left_join(select(df_annotations,"feature",name = "Preferred_name",KO="KEGG_ko",GO="GOs",pathway="KEGG_Pathway"),by="feature") |>
  relocate(c(name,KO,pathway,GO),.after = "feature")

write_tsv(df_exp_anno,"output/count.tsv")


# df_annotations |>
#   select(feature,name = "Preferred_name",KEGG_ko ) |>
#   write_tsv("output/count_anno.tsv")

# write_tsv(df_exp_anno,"output/count2.tsv")
