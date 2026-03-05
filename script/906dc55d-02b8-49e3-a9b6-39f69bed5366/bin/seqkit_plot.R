library(tidyverse)
args <- commandArgs(trailingOnly = TRUE)
file_path <- args[1]
if(length(args)==0){
  file_path<- "output/seqkit.tsv"
}
df <- read_tsv(file_path)

colnames(df)

ggplot(df, aes(x = file, y = sum_len)) +
  geom_col(fill = "#2b8cbe") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "sum_len per file",
       x = "file",
       y = "sum_len")
ggsave("sum_len_per_file.png")
