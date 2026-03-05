library(tidyverse)
library(GSVA)
library(tidyverse)
library(KEGGREST)
library(pheatmap)
library(limma)

params <- jsonlite::read_json("params.json")
df <-  read_tsv(params$eggnog$annotations,comment = "##") |>
  dplyr::rename(feature=`#query`,pathway=KEGG_Pathway,name=Preferred_name,KO=KEGG_ko)

{
  title <- paste0(c(params$groups_name$count$treatment,params$groups_name$count$control),collapse = "_vs_")
  
}

pathway_feature <- df |>
  select( pathway,feature) |>
  separate_rows(pathway,sep = ",") |>
  filter(pathway!="-") |>
  filter(grepl("ko",pathway))

pathway_name <- keggList("pathway", "ko")   |>
  enframe( name = "pathway", value = "name")



pathway <- pathway_feature |>
  left_join(pathway_name,by="pathway") |>
  select(feature,name) |> na.omit()

pathway_list <- pathway %>%
  group_by(name) %>%
  summarise(genes = list(unique(feature))) %>%
  # 转成 named list
  { setNames(.$genes, .$name) }


params$count$control


{
  read_by_df_name <- function(df_name){
    df_obj <- params[[df_name]]
    df <- read_tsv(df_obj$content) |>
      dplyr::rename(length="Length")
    metadata <- lapply(df_obj$groups, function(x){
      df_list <- df_obj[[x]]
      df <- map_dfr(df_list, function(x) {
        x[sapply(x, is.null)] <- NA
        as_tibble(x)
      })
    }) |>bind_rows()
    select_sample <- metadata[["columns_name"]]
    df <- df[,c("feature",select_sample)]
    result = list(df=df,metadata=metadata)
    return(result)
  }
  
  df_obj  <- read_by_df_name("count")
  df <- df_obj$df |> column_to_rownames("feature")
  metadata0 <- df_obj$metadata
}

X <- df |>as.matrix()

gsvaPar <- gsvaParam(X, pathway_list, kcdf="Poisson")
gsva.es <- gsva(gsvaPar, verbose=FALSE) |> as.data.frame()
gsva.es |>
  as.data.frame() |>
  rownames_to_column("pathway") |>
  write_tsv(file = str_glue("output/{title}.tsv"))





# 
# p <- 10000 ## number of genes
# n <- 30    ## number of samples
# ## simulate expression values from a standard Gaussian distribution
# X <- matrix(rnorm(p*n), nrow=p,
#             dimnames=list(paste0("g", 1:p), paste0("s", 1:n)))
# gs <- as.list(sample(10:100, size=100, replace=TRUE))
# gs <- lapply(gs, function(n, p)
#   paste0("g", sample(1:p, size=n, replace=FALSE)), p)
# names(gs) <- paste0("gs", 1:length(gs))
# gsvaPar <- gsvaParam(X, gs)
# gsva.es <- gsva(gsvaPar, verbose=FALSE)



metadata <-metadata0 |> select( sample_name,group_name = selcted_group_name)


# group <- factor(metadata$group_name, levels = c("CF", "SP"))
# design <- model.matrix(~0 + group)
# colnames(design) <- levels(group)
# contrast.matrix <- makeContrasts(SP - CF, levels = design)
# fit <- lmFit(gsva.es, design)
# fit2 <- contrasts.fit(fit, contrast.matrix)
# fit2 <- eBayes(fit2)
# deg1 <- topTable(fit2, number = Inf, adjust.method = "BH")



design <- model.matrix(~ group_name, metadata)
fit <- lmFit(gsva.es, design)
fit.eb <- eBayes(fit, robust=TRUE)
deg <- topTable(fit.eb, number = Inf, adjust.method = "BH")

deg |>
  rownames_to_column("pathway") |>
  write_tsv(file = str_glue("output/{title}_de_pathway.tsv"))

de_pathway <- deg |>
  filter(adj.P.Val <0.05) |> rownames()

png(type = "cairo", file = str_glue("output/{title}_heatmap.png") )
pheatmap(gsva.es[de_pathway,],scale="row")
dev.off()
