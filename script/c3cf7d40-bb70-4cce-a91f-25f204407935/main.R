library(tidyverse)
library(pheatmap)

params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)

df <- read_tsv(params$tsv) |>
  column_to_rownames("Gene")

png(filename = "output/gene_heatmap.png", width = 8,height = 8,units = "in",res = 100)
pheatmap(df) 
dev.off()
