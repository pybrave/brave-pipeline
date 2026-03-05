library(tidyverse)
library(jsonlite)
library(Maaslin2)
library(braveR)
library(ggrepel)  



params <- jsonlite::fromJSON("params.json")

otu_table <- read_tsv(params$otu_table$content)|>
  dplyr::rename(feature = 1)
df_obj <- params$otu_table
metadata <- lapply(df_obj$groups, function(x){
  df_list <- df_obj[[x]]
}) |>bind_rows() |>
  select(sample_name, group=selcted_group_name )

control_name <- params$groups_name$otu_table$control
treatment_name <- params$groups_name$otu_table$treatment

fit_data = Maaslin2(input_data     = t(column_to_rownames(otu_table,"feature")) |> as.data.frame(), 
                    input_metadata = column_to_rownames(metadata,"sample_name"), 
                    plot_scatter =F,
                    min_prevalence = 0,
                    normalization  = "NONE",
                    output         = ".", 
                    fixed_effects  = c("group"),
                    # random_effects = data$phenotype,
                    reference      = c(paste0("group,",control_name)))  

all_results <-fit_data$results
# title <- paste0(c(treatment_name,control_name),collapse = " vs ")

{
  sig_type <- params$`__taxonomy_sig_type`
  sig_thresh <- params$`__taxonomy_sig_threshold`
  effect_thresh <- params$`__taxonomy_effect_threshold`
  title <- paste0(c(treatment_name,control_name),collapse = "_vs_")
  top_num <- params$top_num
  
  # colors <- params$colors$count
}



diff_res <- as.data.frame(all_results) |>
  mutate(sig_value = .data[[sig_type]]) |>
  mutate(direction = factor(ifelse(sig_value  < sig_thresh & abs(coef) > effect_thresh ,
                                   ifelse(coef>0,"Up","Down"),"NS"),
                            levels = c("Up","Down","NS") 
  )) |>
  left_join(otu_table, by="feature") |>
  arrange(pval) 
(tbl <- table(diff_res$direction))

write_tsv(diff_res, str_glue("output/{title}_deg.tsv"))


diffSummaryV1(
  df=diff_res,
  criteria = str_glue("{sig_type} < {sig_thresh} & |coef| > {effect_thresh}"),
  title = "diff summary",
  filename = "feature"
)

p <- ggplot(diff_res, aes(x= coef, 
                          y = -log10(sig_value), 
                          colour=direction)) +
  geom_point(alpha=0.9, size=3.5)+
  scale_color_manual(values=c("Down" = "#3B4992FF", "NS"="#d2dae2","Up"="#EE0000FF"),
                     labels = labels) +
  geom_vline(xintercept=c(-effect_thresh,effect_thresh),lty=4,col="black",lwd=0.8) +
  geom_hline(yintercept = -log10(sig_thresh),lty=4,col="black",lwd=0.8)+
  ggtitle(paste0("Volcano plot of ", title)) +
  labs(x="log2 foldchange", y=paste0("-log10(",sig_type,")"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(color = 'black',size = 10, family = 'sans', face = 'plain'),
        axis.title = element_text(color = 'black',size = 15, family = 'sans', face = 'plain'),
        legend.position="right", 
        legend.title = element_blank()
  )+    geom_text_repel(data=diff_res %>% filter(direction!="NS") |> head(top_num),aes(label = feature), 
                        colour = "black", size = 4)


ggsave(filename = str_glue("output/{title}_volcano.pdf"),plot = p)






