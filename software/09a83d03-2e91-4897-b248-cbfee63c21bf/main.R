library(tidyverse)
library(jsonlite)
params_path <- "params.json"
output_path <- "output"

params <- fromJSON(params_path)

otu_table <- read_tsv(params$otu_table$table) |>
  dplyr::rename( otu_id =`#OTU ID`)
otu_anno <- read_tsv(params$otu_anno$anno)
dim(otu_table)
dim(otu_anno)


otu_table_anno <- otu_table |> 
  left_join(otu_anno,otu_table, by="otu_id" ) |>
  relocate(colnames(otu_anno), .after = "otu_id")

