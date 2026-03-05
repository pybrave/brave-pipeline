library(tidyverse)

params <- jsonlite::fromJSON("params.json")
df <- read_tsv(params$input_file$content)


table(df$Sex)

df |>
  filter(df$Sex == 1) |> 
  write_tsv(file = str_glue("output/v13_male.tsv"))
  


df |>
  filter(df$Sex == 2) |> 
  write_tsv(file = str_glue("output/v13_female.tsv"))






