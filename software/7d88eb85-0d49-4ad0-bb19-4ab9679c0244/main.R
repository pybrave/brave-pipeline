library(tidyverse)
library(vegan)
library(jsonlite)


params_path <- "params.json"
params <- fromJSON(params_path)

depth <- 10000
set.seed(2025)
otu_table <- read_tsv(params$otu_table$content)|>
  dplyr::rename(otu_id = `#OTU ID`)|>
  column_to_rownames("otu_id")
otu = vegan::rrarefy(t(otu_table), depth)
idx = rowSums(otu) >= depth


t(otu) |> as.data.frame() |>
  rownames_to_column("otu_id") |>
  write_tsv(file = str_glue("output/otutab_rare.tsv"))
