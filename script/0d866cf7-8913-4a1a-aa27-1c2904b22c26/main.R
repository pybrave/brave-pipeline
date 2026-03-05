
# while (TRUE) {
#   Sys.sleep(1000)  # 每次休眠 1000 秒，然后继续循环
# }

library(Maaslin2)
library(jsonlite)
library(tidyverse)
library(pheatmap)
# library(ggrepel)
library(ggrepel)   # 用于防止标签重叠
library(lefser)
library(ggpubr)
library(ggalluvial)
library(readxl)
library(patchwork)
library(Hmisc)  # rcorr 用于相关性+p值
library(clusterProfiler)
library(pathview)
library(KEGGREST)
library(braveR)

# 
# args <- commandArgs(trailingOnly = TRUE)
# print(args)
# 
# params_path <- args[1]
# output_path <- args[2]
# if(T){
#   params_path <- "params.json"
#   output_path <- "output"
# }
# 
# cat("",file = paste0(output_path,"/run.info"))
# log <- function(...){
#   cat(paste0(...),file = paste0(output_path,"/run.info"),append = T)
# }

params <- fromJSON("params.json")
# 
# 
# control <- data$abundance |>
#   mutate(select_group= data$groups_name$control)
# treatment <- data$treatment |>
#   mutate(select_group= data$groups_name$treatment)

get_df <- function(control, treatment,control_name){
  list_path <- rbind(control, treatment)
  metadata <- list_path[c("sample_name","select_group")]

 
  
  # rank <- data$rank
  
  duplicated(list_path$sample_name)
  
  
  df_list <- apply(list_path,1, function(x){
    profile_path <- x[["profile"]]
    sample_name <- x[["sample_name"]]
    # message(sample_name)
    
    df <- read_tsv(profile_path) |>
      mutate(MS2_name = case_when(is.na(MS2_name)~MS1_name,
                                  .default =MS2_name )) |>
      select(MS2_name,KEGG_COMPOUND_ID=`KEGG COMPOUND ID`, abundance=last_col())|> 
      mutate(sample_name= sample_name) |> 
      select(MS2_name,KEGG_COMPOUND_ID, abundance,sample_name) |> 
      na.omit()
    df <- df[!duplicated(df$MS2_name),]
    
    df
    # df <- read_tsv(profile_path) |>
    #   mutate(sample_name= sample_name) |> 
    #   select(MS2_name,KEGG_COMPOUND_ID=`KEGG COMPOUND ID`,abundance=all_of(sample_name),sample_name)|> 
    #   na.omit()
    
    
  })
  df_long <- bind_rows(df_list) 
  
  
  df_list[[1]][duplicated( df_list[[1]]$MS2_name),]
  
  microbiome_df.0 <- df_long %>%
    pivot_wider(names_from = sample_name, values_from = abundance) |>
    mutate(across(where(is.numeric), ~replace_na(., 0))) |>
    column_to_rownames("MS2_name") 
  return(microbiome_df.0)
}



get_sig_df <- function(microbiome_df.0,control, treatment,control_name){
  
  
  
  microbiome_df <- select(microbiome_df.0,-KEGG_COMPOUND_ID)
  # microbiome_df[duplicated(rownames(microbiome_df)),]
  
  fit_data = Maaslin2(input_data     = t(microbiome_df) |> as.data.frame(), 
                      input_metadata = column_to_rownames(metadata,"sample_name"), 
                      plot_scatter =F,
                      min_prevalence = 0,
                      normalization  = "NONE",
                      output         = ".", 
                      fixed_effects  = c("select_group"),
                      reference      = c(paste0("select_group,",control_name)))  
  
  microbiome_df_map <- microbiome_df.0 |>
    rownames_to_column("MS2_name") |>
    mutate(feature=make.names(MS2_name)) |>
    select(c("feature","MS2_name","KEGG_COMPOUND_ID"))
  
  sig_thresh <-  data$micro_sig_thresh
  effect_cutoff <- data$micro_effect_cutoff
  
  
  microbiome_df_matrix <- microbiome_df |>
    rownames_to_column("feature") |>
    mutate(feature=make.names(feature)) 
  
  microbiome_sig <- fit_data$results |>
    left_join(microbiome_df_map,by="feature") |>
    mutate(sig_value = .data[[data$micro_sig_type]]) |>
    mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(coef) > effect_cutoff ,
                                     ifelse(coef>0,"Up","Down"),"NS"),
                              levels = c("Up","Down","NS") 
    )) |>
    left_join(microbiome_df_matrix,by="feature")
  stat <- table(microbiome_sig$direction)
  stat
  msg <- paste0("metabolism(",data$sig_type,"<",sig_thresh," & abs(coef)>",effect_cutoff,") Up:",stat["Up"]," Down:",stat["Down"]," NS:",stat["NS"])
  log(msg)
  
  
  # microbiome_df_res <- microbiome_df[filter(microbiome_sig,direction!="NS") |> pull("feature"),]
  
  # microbiome_sig <- filter(microbiome_sig,direction!="NS") |> pull("feature") 
  # 
  # microbiome_df_res <- microbiome_df |>
  #   rownames_to_column("feature0") |>
  #   mutate(feature =feature0, feature0=make.names(feature0)) |>
  #   filter(feature0 %in%microbiome_sig ) |> 
  #   column_to_rownames("feature") |>
  #   select(-feature0)
  return(microbiome_sig)
  
}

# 
# control <- data$control |>
#   mutate(select_group= data$groups_name$control)
# treatment <- data$treatment |>
#   mutate(select_group= data$groups_name$treatment)

control <- params$input$control
treatment <- params$input$treatment

columns_name <- c(control$columns_name, treatment$columns_name)
feature <- params$input$feature$columns_name
optional_feature_name <- params$optional_feature_name
compound <- params$input$compound$columns_name


  
control_name <- params$groups_name$input$control
treatment_name <- params$groups_name$input$treatment

metadata <- rbind(control, treatment) |>
  select("sample_name",group="selcted_group_name")
# metadata <- list_path[c("sample_name","selcted_group_name")]

df <- read_tsv(params$input$content)
if(!is.null(optional_feature_name) && optional_feature_name!=""){
  message(optional_feature_name)
  df <- df |>
    mutate(
      !!feature := case_when(
        is.na(.data[[feature]]) ~ .data[[optional_feature_name]],
        TRUE                    ~ .data[[feature]]
      )
    )
}
df <- df[!duplicated(df[[feature]]),]


df_diff_input <- df |>
  select(all_of(c(feature,columns_name))) 

fit_data = Maaslin2(input_data     = t(column_to_rownames(df_diff_input,feature) ) |> as.data.frame(), 
                    input_metadata = column_to_rownames(metadata,"sample_name"), 
                    plot_scatter =F,
                    min_prevalence = 0,
                    normalization  = "NONE",
                    output         = ".", 
                    fixed_effects  = c("group"),
                    reference      = c(paste0("group,",control_name)))  

all_results <-fit_data$results

{
  sig_type <- params$`__criteria_sig_type`
  sig_thresh <- params$`__criteria_sig_threshold`
  effect_thresh <- params$`__criteria_effect_threshold`
  title <- paste0(c(treatment_name,control_name),collapse = "_vs_")
  
  # colors <- params$colors$count
}

{
  df_diff_input_map <- df_diff_input %>%
    mutate(feature0=make.names(.data[[feature]]) ) |>
    select(feature0, feature)
  
  diff_res <- as.data.frame(all_results) |>
    mutate(sig_value = .data[[sig_type]]) |>
    mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(coef) > effect_thresh ,
                                     ifelse(coef>0,"Up","Down"),"NS"),
                              levels = c("Up","Down","NS") 
    ))|>dplyr::rename(feature0=feature) |>
    left_join(df_diff_input_map,by="feature0") |>
    relocate(feature,.before="feature0") |>
    select(-feature0) 
    
  

    
  
  (tbl <- table(diff_res$direction))
  
  write_tsv(left_join(diff_res,df_diff_input,by=feature), str_glue("output/{title}_deg.tsv"))
  
  
  diffSummaryV1(
    df=dplyr::rename(diff_res,feature=feature),
    criteria = str_glue("{sig_type} < {sig_thresh} & |coef| > {effect_thresh}"),
    title = "diff summary",
    filename = "feature"
  )
  
}

{
  label_size <- params$label_size
  p <- ggplot(diff_res, aes(x= coef, 
                            y = -log10(sig_value), 
                            colour=direction)) +
    geom_point(alpha=0.9, size=3.5)+
    scale_color_manual(values=c("Down" = "#3B4992FF", "NS"="#d2dae2","Up"="#EE0000FF"),
                       labels = labels) +
    geom_vline(xintercept=c(-effect_thresh,effect_thresh),lty=4,col="black",lwd=0.8) +
    geom_hline(yintercept = -log10(sig_thresh),lty=4,col="black",lwd=0.8)+
    ggtitle(paste0("Volcano plot of ", title)) +
    labs(x="Effect Size (Coefficient)", y=paste0("-log10(",sig_type,")"))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text = element_text(color = 'black',size = 10, family = 'sans', face = 'plain'),
          axis.title = element_text(color = 'black',size = 15, family = 'sans', face = 'plain'),
          legend.position="right", 
          legend.title = element_blank()
    )+ 
    geom_text_repel(data=diff_res %>% filter(direction!="NS") |> head(label_size),aes(label = .data[[feature]]), 
                    colour = "black", size = 4)
  
  
  ggsave(filename = str_glue("output/{title}_volcano.pdf"),plot = p)
  
}


# 
# 
# 
# # microbiome_df.0 <- get_df(control, treatment,control_name)
# 
# 
# df_sig <- get_sig_df(microbiome_df.0,control,treatment,control_name)
# 
# title <- paste0(c(data$groups_name$treatment,data$groups_name$control),collapse = " vs ")
# write_tsv(df_sig,file = paste0("output/",title,".download.tsv"))
# 
# 
# sig_thresh <-  data$micro_sig_thresh
# effect_cutoff <- data$micro_effect_cutoff
# 
# 
# counts <- table(df_sig$direction)
# labels <- c(
#   Down = paste0("Down (", counts["Down"], ")"),
#   NS   = paste0("NS (", counts["NS"], ")"),
#   Up   = paste0("Up (", counts["Up"], ")")
# )
# 
# ggplot(df_sig, aes(x= coef, 
#                         y = -log10(sig_value), 
#                         colour=direction)) +
#   geom_point(alpha=0.9, size=3.5)+
#   scale_color_manual(values=c("Down" = "#3B4992FF", "NS"="#d2dae2","Up"="#EE0000FF"),  labels = labels)+
#   geom_vline(xintercept=c(-effect_cutoff,effect_cutoff),lty=4,col="black",lwd=0.8) +
#   geom_hline(yintercept = -log10(sig_thresh),lty=4,col="black",lwd=0.8)+
#   ggtitle(paste0("volcano plot of ",title)) +
#   labs(x="Effect Size (Coefficient)", y=paste0("-log10(",data$sig_type,")"))+
#   theme_bw()+
#   theme(plot.title = element_text(hjust = 0.5), 
#         axis.text = element_text(color = 'black',size = 10, family = 'sans', face = 'plain'),
#         axis.title = element_text(color = 'black',size = 15, family = 'sans', face = 'plain'),
#         legend.position="right", 
#         legend.title = element_blank()
#   ) + 
#   geom_text_repel(data=df_sig %>% filter(direction!="NS") |> head(80),aes(label = MS2_name), 
#                   colour = "black", size = 4)
# 
# 
# ggsave(filename = paste0(output_path,"/",str_replace_all(title," ","_"),".pdf"))
# 
# if(!is.null(compound) && compound!=""){
#   diff_compound <- diff_res |>
#     left_join(df,by=all_of(feature)) |>
#     filter(direction!="NS") |>
#     select(all_of(c(feature,compound)),"coef") |>
#     na.omit()
#   KEGG_COMPOUND_ID <- unique(diff_compound[[compound]])
#   
#   kegg_compound2pathway <- KEGGREST::keggLink("pathway", "compound")
#   kegg_pathway2compound <- split(names(kegg_compound2pathway),
#                                  kegg_compound2pathway)
#   kegg_pathway2compound_stack <- stack(kegg_pathway2compound)[, 2:1] |>
#     mutate(values=str_replace(values,"cpd:","")) |>
#     mutate(ind=str_replace(ind,"path:","")  ) |>
#     select(pathway=ind,compound=values)
#   
#   
#   
#   pathway_name_map <- KEGGREST::keggList("pathway") |>
#     as.data.frame() |>
#     rownames_to_column("pathway") |>
#     `colnames<-`(c("ID","name"))
#   
#   
#   enrich_res <- clusterProfiler::enricher(
#     gene = KEGG_COMPOUND_ID,             
#     TERM2GENE =kegg_pathway2compound_stack ,
#     pvalueCutoff = 1,
#     qvalueCutoff = 1,
#   )
#   # kegg <- setReadable(kegg, 'org.Hs.eg.db', 'ENTREZID')
#   
#   organism <- params$organism
#   enrich_res_df <- as.data.frame(enrich_res) |>
#     left_join(pathway_name_map,by="ID") |>
#     mutate(Description= name) |>
#     select(-name)
#   
#   write_tsv(enrich_res_df,file = paste0("output/",title,".enrich.download.tsv"))
#   
#   # df_sig_comp_json<-  df_sig_comp |>
#   #   select(-MS2_name )
#   df_sig_comp_vec <- setNames(diff_compound$coef, diff_compound[[compound]]) |> as.list()
#   
#   enrich_res_df_json <- enrich_res_df |>
#     mutate(organism=organism,pathwayId=as.character(str_replace(ID,"map",""))) 
#   
#   list(compound = df_sig_comp_vec,list=enrich_res_df_json) |> toJSON(auto_unbox = TRUE) |>
#     write("output/kegg_map.vis")
#   
#   
# }
# 
# {
# # diff_res |>
# #     left_join(df,by=all_of(feature)) |>
# #     filter(direction!="NS") |>
# #     select(all_of(c(feature,compound)),"pval","qval","direction") |> write_tsv(file = "output/prompt.ai")
# 
#   enrich_res_df  |> write_tsv(file = "output/prompt.ai")
#   
#   
# }
















# df_sig_comp <- df_sig |>
#   filter(direction!="NS") |>
#   select(MS2_name,KEGG_COMPOUND_ID,coef) 

# metabolite_list <- c("C00031", "C00022", "C00186", "C00042", "C00149")

# KEGG_COMPOUND_ID <- unique(df_sig_comp$KEGG_COMPOUND_ID)

# kegg_compound2pathway <- KEGGREST::keggLink("pathway", "compound")
# kegg_pathway2compound <- split(names(kegg_compound2pathway),
#                                kegg_compound2pathway)

# kegg_pathway2compound_stack <- stack(kegg_pathway2compound)[, 2:1] |>
#   mutate(values=str_replace(values,"cpd:","")) |>
#   mutate(ind=str_replace(ind,"path:","")  ) |>
#   select(pathway=ind,compound=values)

# length(KEGG_COMPOUND_ID)
# intersect(KEGG_COMPOUND_ID,kegg_pathway2compound_stack$values) |> length()
# setdiff(KEGG_COMPOUND_ID,kegg_pathway2compound_stack$values)
# 
# kegg_pathway2compound_stack |>
#   filter(grepl("C07147",values))



# if(!is.null(data$query_compound) && data$query_compound!="" ){
#   compound_list <- str_split(data$query_compound,",")[[1]]
#   message(compound_list)
#   lapply(compound_list, function(x){
#     p<- df_diff_input |>
#       pivot_longer(-c(anly_of(feature) ),names_to = "sample_name", values_to = "abundance" ) |>
#       left_join(metadata,by="sample_name") |>
#       mutate(log2abunadance = log2(abundance)) |>
#       filter(KEGG_COMPOUND_ID %in% c(x)) |>
#       ggplot(aes(x=select_group,y=log2abunadance)) +
#       geom_boxplot()+
#       ggtitle(x)
#     ggsave(filename = str_glue("output/{x}.png"),plot = p)
#   })
# }



# , pretty = TRUE


  # write_tsv(file = paste0("output/kegg_map.vis"))
# 
# 
# pathview(
#   pathway.id = "hsa00010",  # Glycolysis / Gluconeogenesis
#   species = "hsa",
#   gene.data = NULL,
#   cpd.data = setNames(rep(1, length(metabolite_list)), metabolite_list)
# )
# 
# sim.cpd.data=sim.mol.data(mol.type="cpd", nmol=3000)
# i <- 3
# print(demo.paths$sel.paths[i])
# pv.out <- pathview(gene.data = gse16873.d[, 1], cpd.data = sim.cpd.data,
#                    pathway.id = demo.paths$sel.paths[i], species = "hsa", out.suffix =
#                      "gse16873.cpd", keys.align = "y", kegg.native = TRUE, key.pos = demo.paths$kpos1[i])
# str(pv.out)
# head(pv.out$plot.data.cpd)
# head(summary(enrich_res))
# dotplot(enrich_res, showCategory = 10)
# 
# remotes::install_github("YuLab-SMU/createKEGGdb")
# library(createKEGGdb)
# species <-c("hsa","rno","mmu")
# create_kegg_db(species)

