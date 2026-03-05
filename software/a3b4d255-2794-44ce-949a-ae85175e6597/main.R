library(tidyverse)
library(clusterProfiler)
library(jsonlite)
library(KEGGREST)
library(fgsea)
library(ggplot2)

params <- jsonlite::read_json("params.json")
df <-  read_tsv(params$anno$annotations,comment = "##") |>
  dplyr::rename(feature=`#query`,pathway=KEGG_Pathway,name=Preferred_name,KO=KEGG_ko)


pathway_feature <- df |>
  select( pathway,feature) |>
  separate_rows(pathway,sep = ",") |>
  filter(pathway!="-") |>
  filter(grepl("ko",pathway))


gene_map <- df |>
  select( feature,"name") 

gene_map_KO <- df |>
  select( feature,KO) |>
  separate_rows(KO,sep = ",") |>
  mutate(KO=str_replace(KO,"ko:","")) %>%
  group_by(feature) %>%
  summarise(KO = paste(unique(KO), collapse = "/"), .groups = "drop")


pathway_name <- keggList("pathway", "ko")   |>
  enframe( name = "pathway", value = "name")

# gene_list <- str_split(params$gene,",")[[1]] |>
#   str_replace_all(" ","")

df_deg <- read_tsv(params$deg$content)
df_deg_sig <- df_deg |>
  filter(direction!="NS")

gene_list <-  df_deg_sig |> pull("feature")

message(str_glue("Enrichment gene size: {length(gene_list)}"))

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




df_deg_sig_ko <- df_deg_sig |>
  left_join(gene_map_KO,by="feature") |>
  select(feature, KO,log2FoldChange) |>
  filter(KO!="-") |>
  separate_rows(KO,sep = "/") 

df_sig_comp_vec <- setNames(df_deg_sig_ko$log2FoldChange, df_deg_sig_ko$KO) |> as.list()
enrich_res_df_json <- res |>
  mutate(organism ="ko" ) |>
  mutate(pathwayId=str_replace(ID,"ko",""))
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


# deg <- data.frame(
#   gene = c("TP53", "MYC", "EGFR", "CDK1", "GAPDH"),
#   logFC = c(2.5, -1.8, 1.2, -2.1, 0.5)
# )
# gene_list <- setNames(df_deg_sig_ko$log2FoldChange, df_deg_sig_ko$feature)
# # gene_list <- deg$logFC
# # names(gene_list) <- deg$gene
# gene_list <- sort(gene_list, decreasing = TRUE)  |> unique()
# 
# gsea_result <- GSEA(
#   geneList = gene_list,          # 排好序的基因列表（带符号数值）
#   TERM2GENE = pathway_feature,   # 自定义通路映射表
#   TERM2NAME = pathway_name,      # 通路名称表（可选）
#   pvalueCutoff = 0.05,
#   pAdjustMethod = "BH",
#   minGSSize = 10,
#   maxGSSize = 200,
#   verbose = T
# )
# 
# gsea_res <- GSEA(
#   geneList = gene_list,
#   TERM2GENE = pathway_feature,
#   TERM2NAME = pathway_name,
#   pvalueCutoff = 0.05,
#   verbose = FALSE
# )
# gsea_kegg <- gseKEGG(
#   geneList = gene_list,
#   TERM2GENE = pathway_feature,
#   TERM2NAME = pathway_name,
#   organism = 'hsa',   # 物种代码（人类）
#   minGSSize = 10,
#   pvalueCutoff = 0.05
# )
# 
# 
# pathways <- split(pathway_feature$feature, pathway_feature$pathway)
# 
# fgsea_res <- fgsea(
#   pathways = pathways,
#   stats = gene_list,
#   minSize = 10,
#   maxSize = 200,
#   nperm = 10000
# )
# 

pathway <- pathway_feature |>
  left_join(pathway_name,by="pathway") |>
  select(feature,name) |> na.omit()
  
pathway_list <- pathway %>%
  group_by(name) %>%
  summarise(genes = list(unique(feature))) %>%
  # 转成 named list
  { setNames(.$genes, .$name) }

df_deg_sig_ko_unique <- df_deg_sig_ko |>
  select(log2FoldChange,feature) |> unique()
gene_list <- setNames(df_deg_sig_ko_unique$log2FoldChange, df_deg_sig_ko_unique$feature) 
gene_list <- sort(gene_list, decreasing = TRUE)
# data(examplePathways)
# data(exampleRanks)
fgseaRes <- fgsea(pathways = pathway_list, 
                  stats    = gene_list,
                  minSize  = 5,
                  eps      = 0.0,
                  maxSize  = 500)
fgseaRes_df <- fgseaRes |>arrange(pval) |>
  as.data.frame() %>%
  mutate(leadingEdge = sapply(leadingEdge, function(x) paste(x, collapse = ",")))

fgseaRes_df_res <- fgseaRes_df |>
  dplyr::rename(name=pathway) |>
  left_join(pathway_name,by="name") |>
  relocate(pathway,.before = "name") |>
  rowwise() %>%
  mutate(
    KO = paste(
      gene_map_KO$KO[
        match(str_split(leadingEdge, ",", simplify = TRUE), gene_map_KO$feature)
      ],
      collapse = "/"
    )
  ) %>%
  ungroup() 

write_tsv(fgseaRes_df_res,file = str_glue("output/kegg_gsea.download.tsv"))

gsea_res_df_json <- fgseaRes_df_res |>
  mutate(organism ="ko" ) |>
  mutate(pathwayId=str_replace(pathway,"ko",""))
list(compound = df_sig_comp_vec,list=enrich_res_df_json) |> toJSON(auto_unbox = TRUE) |>
  write("output/gsea_kegg_map.vis")




fgseaRes$leadingEdge[[which.max(fgseaRes$NES)]]

plotEnrichment(pathway_list[["Phosphotransferase system (PTS)"]],
               gene_list) + labs(title="Phosphotransferase system (PTS)")

# plotEnrichment(examplePathways[["5991130_Programmed_Cell_Death"]],
#                exampleRanks) + labs(title="Programmed Cell Death")

topPathwaysUp <- fgseaRes[ES > 0][head(order(pval), n=10), pathway]
topPathwaysDown <- fgseaRes[ES < 0][head(order(pval), n=10), pathway]
topPathways <- c(topPathwaysUp, rev(topPathwaysDown))
plotGseaTable(pathway_list[topPathways], gene_list, fgseaRes, 
              gseaParam=0.5) +theme_classic()
ggsave(filename = str_glue("output/gsea_table.png"))


# class(a$leadingEdge)

if(!is.null(params$pathway) &&params$pathway!=""  ){
  message(params$pathway)
  pathway_list_str <- str_split(params$pathway,",")[[1]]
  
  pathway_name_list <- pathway_name |>
    filter(pathway %in% pathway_list_str) |> pull(name)
  
  a <- lapply(pathway_name_list, function(x){
    message(str_glue("plot {x}"))
    plt <- plotEnrichment(pathway_list[[x]],
                   gene_list) + labs(title=x)
    ggsave(filename =str_glue("output/{x}.png") )
    
  })
}





