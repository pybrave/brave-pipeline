library(tidyverse)
params <- jsonlite::fromJSON("params.json")


df <- read_tsv(params$file$content) |>
  column_to_rownames("sample") |>t() |>
  as.data.frame() |>
  rownames_to_column("feature")

write_tsv(df,file = str_glue("output/behavior_feature_2.tsv"))
write_tsv(df,file = str_glue("output/behavior_feature_2.csv"))
