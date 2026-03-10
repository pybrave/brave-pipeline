library(tidyverse)
library(readxl)

# params <- jsonlite::fromJSON("params.json")
# file_path <- params$input_file$content
# df <- read_tsv(params$input_file$content)

# table(df$Sex)

# df |>
#   filter(df$Sex == 1) |> 
#   write_tsv(file = str_glue("output/v13_male.tsv"))
  


# df |>
#   filter(df$Sex == 2) |> 
#   write_tsv(file = str_glue("output/v13_female.tsv"))

excel_path <- "/opt/brave_prod/workspace/data/weicailing/mk801.xlsx"
output_dir <- "output"

# dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

sheet_names <- excel_sheets(excel_path)

for (sheet_name in sheet_names) {
	sheet_df <- read_excel(excel_path, sheet = sheet_name)
	safe_sheet_name <- str_replace_all(sheet_name, "[^[:alnum:]_-]+", "_")
	out_file <- file.path(output_dir, str_glue("{safe_sheet_name}.tsv"))

	write_tsv(sheet_df, file = out_file)
}









