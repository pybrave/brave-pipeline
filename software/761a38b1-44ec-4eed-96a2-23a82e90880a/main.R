library(tidyverse)
library(jsonlite)

params <- fromJSON("params.json")

# for( file in  params$metabolite$content){
#   if(grepl(file,"gut")){
#     df <- read_tsv(file) |>
#       rename_with(~ str_replace(.x,"MCC","ACC"), starts_with("MCC"))
#   }
#   
# }

params$metabolite$content

df <- read_tsv("/opt/brave_prod/workspace/data/289364b1-295c-4710-833e-d68ec7c8918e/gut_metabolic.tsv") |>
  rename_with(~ str_replace(.x,"MCC","ACC"), starts_with("MCC"))
write_tsv(df, file = str_glue("output/gut_metabolic_rename.tsv"))

df2 <- read_tsv("/opt/brave_prod/workspace/data/289364b1-295c-4710-833e-d68ec7c8918e/brain_metabolic.tsv") |>
  rename_with(~ str_replace(.x,"AB","OCC"), starts_with("AB")) |>
  rename_with(~ str_replace(.x,"OB","ACC"), starts_with("OB"))

write_tsv(df2, file = str_glue("output/brain_metabolic_rename.tsv"))


df <- read_tsv("/opt/brave_prod/workspace/data/289364b1-295c-4710-833e-d68ec7c8918e/gut_metabolic.tsv") |>
  rename_with(~ str_replace(.x,"MCC","ACC"), starts_with("MCC")) |>
  mutate(feature = case_when(is.na(MS2_name)~MS1_name,
                              .default =MS2_name )) |>
  filter(!is.na(feature)) |>
  relocate(feature, .before = "MS2_name")
df <- df[!duplicated(df$feature),] 
write_tsv(df, file = str_glue("output/gut_metabolic_feature.tsv"))



df2 <- read_tsv("/opt/brave_prod/workspace/data/289364b1-295c-4710-833e-d68ec7c8918e/brain_metabolic.tsv") |>
  rename_with(~ str_replace(.x,"AB","OCC"), starts_with("AB")) |>
  rename_with(~ str_replace(.x,"OB","ACC"), starts_with("OB")) |>
  mutate(feature = case_when(is.na(MS2_name)~MS1_name,
                             .default =MS2_name )) |>
  filter(!is.na(feature)) |>
  relocate(feature, .before = "MS2_name")
df2 <- df2[!duplicated(df2$feature),] 


write_tsv(df2, file = str_glue("output/brain_metabolic_feature.tsv"))


