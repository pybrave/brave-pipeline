library(tidyverse)
library(clusterProfiler)
library(jsonlite)
library(KEGGREST)


params <- jsonlite::read_json("params.json")
pathway_feature <-  read_tsv(params$anno_file$content) |>
  select( pathway,feature) |>
  separate_rows(pathway,sep = ",") |>
  filter(pathway!="-") |>
  filter(grepl("ko",pathway))


gene_map <- read_tsv(params$anno_file$content) |>
  select( feature,"name")

gene_map_KO <- read_tsv(params$anno_file$content) |>
  select( feature,KO) |>
  separate_rows(KO,sep = ",") |>
  mutate(KO=str_replace(KO,"ko:","")) %>%
  group_by(feature) %>%
  summarise(KO = paste(unique(KO), collapse = "/"), .groups = "drop")
  

pathway_name <- keggList("pathway", "ko")   |>
  enframe( name = "pathway", value = "name")

gene_list <- str_split(params$gene,",")[[1]] |>
  str_replace_all(" ","")


kegg_rich <- enricher(gene = gene_list,
                      TERM2GENE = pathway_feature,
                      TERM2NAME = pathway_name,
                      pvalueCutoff = 54,
                      pAdjustMethod = 'BH',
                      qvalueCutoff = 100,
                      maxGSSize = 200) 




res <- kegg_rich@result %>%
  rowwise() %>%
  mutate(
    gene_name = paste(
      gene_map$name[
        match(str_split(geneID, "/", simplify = TRUE), gene_map$feature)
      ],
      collapse = "/"
    )
  ) %>%
  ungroup() |>
  rowwise() %>%
  mutate(
    KO = paste(
      gene_map_KO$KO[
        match(str_split(geneID, "/", simplify = TRUE), gene_map_KO$feature)
      ],
      collapse = "/"
    )
  ) %>%
  ungroup() |>
  relocate(c(gene_name,KO),.before = "geneID")




res |>
  write_tsv(file = str_glue("output/kegg_enrichment.download.tsv"))

df_sig_comp_vec <- setNames(df_sig_comp_json$coef, df_sig_comp_json$KEGG_COMPOUND_ID) |> as.list()
enrich_res_df_json <- res |>
  select(-c("geneID"))
list(compound = df_sig_comp_vec,list=enrich_res_df_json) |> toJSON(auto_unbox = TRUE) |>
  write("output/kegg_map.vis")


dotplot(kegg_rich,color ="pvalue")
ggsave(filename = str_glue("output/kegg_dotplot.pdf"))
barplot(kegg_rich,color ="pvalue")
ggsave(filename = str_glue("output/kegg_barplot.pdf"))


# replace_geneid_with_name <- function(df, map) {
#   df %>%
#     separate_rows(geneID, sep = "/") %>%
#     left_join(map, by = c("geneID" = "feature")) %>%
#     group_by(across(-c(name))) %>%
#     summarise(gene_name = paste(unique(na.omit(name)), collapse = "/"), .groups = "drop")
# }
# 
# res2 <- replace_geneid_with_name(res, gene_map)
# 
# res2 <- res %>%
#   # 拆分 geneID
#   separate_rows(geneID, sep = "/") %>%
#   # 左连接基因映射
#   left_join(gene_map, by = c("geneID" = "feature")) %>%
#   # 聚合回去，用基因名代替
#   group_by(ID, Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, Count) %>%
#   summarise(
#     geneID = paste(unique(na.omit(gene_name)), collapse = "/"),
#     .groups = "drop"
#   )


# 
# 
# kegg_compound2pathway <- KEGGREST::keggLink("pathway", "name") 
# kegg_pathway2compound <- split(names(kegg_compound2pathway),
#                                kegg_compound2pathway)
# 
# kegg_pathway2compound_stack <- stack(kegg_pathway2compound)[, 2:1]
# 


