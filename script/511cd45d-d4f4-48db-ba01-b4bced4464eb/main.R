library(tidyverse)
library(phyloseq)
library(dplyr)
library(jsonlite)


params <- fromJSON("params.json")


otu_table <- read_tsv(params$otu_table$content) |>
  dplyr::rename(taxonomy = 1) |>
  column_to_rownames("taxonomy")

metadata <- params$otu_table$sample |>
  select(sample_name,group) |>
  column_to_rownames("sample_name")



otu <- as.matrix(otu_table)
otu_ps <- otu_table(otu, taxa_are_rows = TRUE)
meta_ps <- sample_data(metadata)

ps <- phyloseq(otu_ps, meta_ps)


set.seed(42)

# 定义抽样深度
depths <- round(seq(100, min(sample_sums(ps)), length.out = 20))

metrics <- c("Observed", "Shannon")

res_list <- list()

d<-100
for (d in depths) {
  rar <- tryCatch(
    rarefy_even_depth(ps, sample.size = d, rngseed = 42, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(rar)) next
  
  alpha_df <- estimate_richness(rar, measures = metrics)
  alpha_df$SampleID <- rownames(alpha_df)
  alpha_df$Depth <- d
  
  alpha_df <- alpha_df %>%
    left_join(metadata %>% tibble::rownames_to_column("SampleID"),
              by = "SampleID")
  # merge metadata
  # alpha_df <- cbind(alpha_df, metadata[alpha_df$SampleID, ])
  # alpha_df
  
  res_list[[as.character(d)]] <- alpha_df
}

df <- bind_rows(res_list)
df$Depth <- as.numeric(df$Depth)


ggplot(df, aes(x = Depth, y = Shannon, color = group, group = SampleID)) +
  geom_line(alpha = 0.4) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.3) +
  labs(title = "Alpha Rarefaction Curve (Shannon)",
       x = "Sequencing depth",
       y = "Shannon index") +
  theme_minimal()

ggsave(filename = str_glue("output/Shannon_arc.pdf"))



ggplot(df, aes(x = Depth, y = Observed, color = group, group = SampleID)) +
  geom_line(alpha = 0.4) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.3) +
  labs(title = "Alpha Rarefaction Curve (Observed OTUs)",
       x = "Sequencing depth",
       y = "Observed OTUs") +
  theme_minimal()
ggsave(filename = str_glue("output/Observed_otu_arc.pdf"))


# library(ggh4x)
# 
# ggplot(df, aes(x = Depth, y = Shannon, color = group)) +
#   stat_summary(fun.data = mean_cl_normal, geom=" ribbon",
#                aes(fill = Group), alpha = 0.2, color = NA) +
#   stat_summary(fun = mean, geom = "line", size = 1.3) +
#   labs(title = "Alpha Rarefaction Curve (Shannon)",
#        x = "Sequencing depth",
#        y = "Shannon index") +
#   theme_minimal()



# df_1 <- otu_table |>
#   select(starts_with("SP"))
# colSums(df_1 >0)
# 
# 
# df_1 <- otu_table |>
#   select(starts_with("SA"))
# colSums(df_1 >0)



