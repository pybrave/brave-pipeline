args <- commandArgs(trailingOnly = TRUE)
print(args)

kegg_path <- args[1]
kegg_annotation <- args[2]

library(tidyverse)

if(F){
  library(jsonlite)
  data <- fromJSON("params.json")
  kegg_path <- "/ssd1/wy/workspace2/nextflow_workspace/a4f4acb2-e119-4a9e-8c59-cdea6bff3df5/e2e9e213-8fe9-4d21-a75f-42671c865807/9bbcf933-cb86-4419-9f4a-d307c8419545/output/humann/test_s1/test_s1.Pathway.txt"
  kegg_annotation <- data$kegg_annotation$path
}

# 读取文件
lines <- readLines(kegg_annotation)

# 去掉注释行和空行
lines <- lines[!grepl("^!|^\\+|^\\s*$", lines)]

# 用于存储层级
LevelA <- NA
LevelB <- NA

records <- list()

for (line in lines) {
  line <- str_trim(line)
  
  if (str_starts(line, "A")) {
    LevelA <- str_remove(line, "^A\\s*")  # 一级类目
  } else if (str_starts(line, "B")) {
    LevelB <- str_remove(line, "^B\\s*")  # 二级类目
  } else if (str_starts(line, "C")) {
    # 提取编号和名字
    parts <- str_match(line, "^C\\s*([0-9]{5})\\s+(.*)$")
    id <- parts[,2]
    name <- parts[,3]
    
    records[[length(records)+1]] <- data.frame(
      LevelA = LevelA,
      LevelB = LevelB,
      Pathway_ID = id,
      Pathway_Name = name,
      stringsAsFactors = FALSE
    )
  }
}

anno <- bind_rows(records) |>
  mutate(term=paste0("map",Pathway_ID)) |>
  select(-Pathway_ID)

df <- read_tsv(kegg_path)|>
  `colnames<-`(c("term","abundance")) 
dim(df)

df_anno <- df |>
  left_join(anno,by="term") 

