library(tidyverse)
library(jsonlite)

params <-  jsonlite::read_json("params.json")

lapply(params$samples, function(x){
  json_path <- x[["json"]]
  res <- jsonlite::read_json(json_path)
  res 
})
fastp_list <- lapply(params$samples, function(sample) {
  sample_id <- sample$sample_name
  json_path <- sample$json
  res <- jsonlite::read_json(json_path)
  before <- res$summary$before_filtering
  after <- res$summary$after_filtering
  
  tibble(
    sample = sample_id,
    fastp_version = res$summary$fastp_version,
    sequencing = res$summary$sequencing,
    
    `total_reads_before (M)` = before$total_reads / 1e6,
    `total_bases_before (Gb)` = before$total_bases / 1e9,
    `q20_rate_before (%)` = before$q20_rate * 100,
    `q30_rate_before (%)` = before$q30_rate * 100,
    `gc_before (%)` = before$gc_content * 100,
    
    `total_reads_after (M)` = after$total_reads / 1e6,
    `total_bases_after (Gb)` = after$total_bases / 1e9,
    `q20_rate_after (%)` = after$q20_rate * 100,
    `q30_rate_after (%)` = after$q30_rate * 100,
    `gc_after (%)` = after$gc_content * 100
  )
})
fastp_df <- bind_rows(fastp_list)
fastp_df <- fastp_df %>%
  mutate(
    across(contains("reads"), round, 3),
    across(contains("bases"), round, 3),
    across(contains("rate"), round, 3),
    across(contains("gc"), round, 3)
  ) 

write_tsv(fastp_df,file = str_glue("output/statistic.tsv"))

fastp_df_plot <- bind_rows(fastp_list) %>%
  pivot_longer(
    cols = -c(sample, fastp_version, sequencing),
    names_to = c("metric", "stage"),
    names_pattern = "(.*)_(before|after).*",
    values_to = "value"
  ) %>%
  mutate(
    stage = factor(stage, levels = c("before", "after")),
    metric = str_replace_all(metric, "\\s*\\(.*\\)", "") # 去掉单位方便分面显示
  )


# ggplot(
#   fastp_df_plot %>% filter(metric %in% c("total_reads", "total_bases")),
#   aes(x = sample, y = value, fill = stage)
# ) +
#   geom_col(position = "dodge") +
#   facet_wrap(~metric, scales = "free_y") +
#   labs(
#     title = "Fastp Filtering Summary",
#     x = "Sample",
#     y = "Value",
#     fill = "Stage"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# ggplot(
#   fastp_df_plot %>% filter(metric %in% c("q20_rate", "q30_rate")),
#   aes(x = sample, y = value, fill = stage)
# ) +
#   geom_col(position = "dodge") +
#   facet_wrap(~metric) +
#   labs(
#     title = "Quality Score (Q20/Q30) Comparison",
#     y = "Percentage (%)",
#     x = "Sample"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# ggplot(
#   fastp_df_plot %>% filter(metric == "gc"),
#   aes(x = sample, y = value, fill = stage)
# ) +
#   geom_col(position = "dodge") +
#   labs(
#     title = "GC Content Before and After Filtering",
#     y = "GC Content (%)",
#     x = "Sample"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(fastp_df_plot, aes(x = stage, y = value, fill = stage)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~metric, scales = "free_y") +
  labs(title = "Fastp Summary Metrics by Stage", y = "Value") 
  # theme_minimal(base_size = 13)
ggsave(filename = str_glue("output/summary_metrics.png"))

