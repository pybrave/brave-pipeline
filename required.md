library(tidyverse)

params <- jsonlite::fromJSON("params.json")
file_path <- params$input_file$content
df <- read_tsv(file_path)




colnames(df)
# [1] "Row.names"     "P_value"       "statistic"     "effect_size"   "mean_abun_CM"  "mean_abun_SM"  "mean_abun_all" "mean_rank_CM"  "mean_rank_SM"  "mean_occ_CM"   "mean_occ_SM"  
# [12] "mean_occ_all"  "enrichment"    "Qvalue"        "cm135"         "cm172"         "cm173"         "cm175"         "cm177"         "cm179"         "sm162"         "sm163"        
# [23] "sm165"         "sm166"         "sm169"         "Sm170"         "Type"      


# type is rank
# Row.names is microbe
# group 1  is  cm135, cm172, cm173, cm175, cm177, cm179
# group 2 is sm162, sm163, sm165, sm166, sm169, Sm170


# panel by rank
# x is microbe
# y is abundance
# group by treatment

# 我的目标是, 首先根据json生成的form表单生成一个交互式的界面，用户可以选择 x 轴的变量，y轴的变量，group1的变量，group2的变量，以及panel的变量。然后根据用户的选择图形参数，
# 生成相应的图表, 例如小提琴图，散点图，箱线图等。用户还可以选择是否显示统计检验结果，例如p值和q值。
# 请帮我设计 json, json schema ，react代码以及R代码


上述json用于生成form表单 其中 "col": 24 占一行 col 12 占一半 col 8 占三分之一  Divider 是分割线，请帮我整理这个json 使得它能够生成一个合理的form表单，并生成schema可以用于后续llm 的context生成表单
{
    "type":"Divider",
    "text":"xxxx"
},