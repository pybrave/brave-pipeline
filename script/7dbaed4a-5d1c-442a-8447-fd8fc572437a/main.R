library(tidyverse)


params <- jsonlite::fromJSON("params.json")

df <- read_tsv(params$input$content)

feature <- params$input$feature$columns_name
metadata <- params$input$samples

df_select <- df |>
  select(all_of(feature), metadata$columns_name)
  
df_select[!duplicated(df_select[[feature]]),] |>
    column_to_rownames(feature) |>t() |>
    as.data.frame() |>
  rownames_to_column("sample") |>
  write_tsv("output/res.tsv")
