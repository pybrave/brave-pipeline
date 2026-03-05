# install.packages("mediation")
library(mediation)
library(tidyverse)

params <- jsonlite::fromJSON("params.json")

data <- read_tsv(params$mediation_file$content)

model.m <- lm(M ~ X, data = data)

model.y <- lm(Y ~ X + M, data = data)


med.out <- mediate(model.m, model.y,
                   treat = "X",
                   mediator = "M",
                   boot = TRUE,
                   sims = 1000)
summary(med.out) 

med_df <- data.frame(
  Effect = c("ACME", "ADE", "Total Effect", "Proportion Mediated"),
  Estimate = c(med.out$d0,
               med.out$z0,
               med.out$tau.coef,
               med.out$n0),
  CI.Lower = c(med.out$d0.ci[1],
               med.out$z0.ci[1],
               med.out$tau.ci[1],
               med.out$n0.ci[1]),
  CI.Upper = c(med.out$d0.ci[2],
               med.out$z0.ci[2],
               med.out$tau.ci[2],
               med.out$n0.ci[2]),
  p.value = c(med.out$d0.p,
              med.out$z0.p,
              med.out$tau.p,
              NA)  # 中介比例没有 p 值
)
write_tsv(med_df,file = str_glue("output/mediation.tsv"))

pdf(file = str_glue("output/mediation_plot.pdf") )
plot(med.out)
dev.off()

