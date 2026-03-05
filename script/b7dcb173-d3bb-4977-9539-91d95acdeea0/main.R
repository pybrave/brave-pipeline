# install.packages("semTools")
library(tidyverse)
library(lavaan)
library(MASS)
library(semTools)
library(lavaanPlot)
library(semPlot)

params <- jsonlite::fromJSON("params.json")

df <- read_tsv(params$mediation_file$content)
colnames(df)
data <- df |>
  dplyr::select("PSQI-total","sleep disturbances","daytime dysfunction","Age"
                ,"Sex","GAD-total","PHQ-total","suicide","HAMD-3","NLR","MLR","NEU %","L %","SII") 
  
colnames(data) <- make.names(colnames(data))

# info =~ Age + Sex
# depression ~ sleep
# suicide ~ sleep
# infection ~ sleep
# suicide ~ depression
# 抑郁 → 睡眠问题 → 炎症反应 → 自杀风险
# PSQI.total + 
model_sem <- '
# 测量模型
sleep  =~  PSQI.total + sleep.disturbances + daytime.dysfunction
depression   =~ GAD.total + PHQ.total
infection =~ NLR + MLR + NEU.. + L.. + SII
suicidal  =~ HAMD.3

# 结构路径
sleep ~ depression
infection ~ sleep
suicidal  ~ depression + sleep + infection

'
fit_sem <- sem(model_sem, data = data ,std.lv = TRUE, estimator = "MLR")
sink(file = str_glue("output/sem_summary.txt"))
summary(fit_sem, fit.measures = TRUE, standardized = TRUE)
sink()
summary(fit_sem, fit.measures = TRUE, standardized = TRUE)

sink(file = str_glue("output/sem_measures.txt"))
fitMeasures(fit_sem)
sink()
fitMeasures(fit_sem)
semPaths(fit_sem,
         what = "std",
         layout = "tree",
         edge.label.cex = 1.2)


pdf(file = str_glue("output/sem.pdf") )

semPaths(fit_sem,
         what = "std",
         layout = "tree",
         edge.label.cex = 1.2)
dev.off()


# 
# # 验证性因素分析（CFA, Confirmatory Factor Analysis
# model_cfa <- '
#   LS =~ LS1 + LS2 + LS3
#   OT =~ OT1 + OT2 + OT3
#   JS =~ JS1 + JS2 + JS3
# '
# 
# fit_cfa <- cfa(model_cfa, data=df)
# summary(fit_cfa, fit.measures=TRUE, standardized=TRUE)
# 
# reliability(fit_cfa)
# 
# model_sem <- '
#   # Measurement model
#   LS =~ LS1 + LS2 + LS3
#   OT =~ OT1 + OT2 + OT3
#   JS =~ JS1 + JS2 + JS3
#   
#   # Structural model
#   OT ~ a*LS
#   JS ~ b*OT + c*LS
#   
#   # Indirect effect
#   indirect := a*b
#   
#   # Total effect
#   total := c + (a*b)
# '
# 
# fit_sem <- sem(model_sem, data=df, se="bootstrap", bootstrap=5000)
# sink(file = str_glue("output/sem_summary.txt"))
# summary(fit_sem, fit.measures=TRUE, standardized=TRUE, ci=TRUE)
# sink()
# 
# standardizedSolution(fit_sem)  |>
#   write_tsv(file = str_glue("output/standardized_solution.tsv"))
# 
# # install.packages("lavaanPlot")
# lavaanPlot(model = fit_sem)
# 
# fit_df<-fitMeasures(fit_sem, c("chisq","df","chisq.scaled","cfi","tli","rmsea","srmr")) 
# sink(file = str_glue("output/model_fit_indicators.txt"))
# print(fit_df)
# sink()



if(F){
  
  data("HolzingerSwineford1939")
  head(HolzingerSwineford1939)
  
  # 这个数据包含 9 个测量变量：
  # x1 x2 x3 → 视觉能力
  # x4 x5 x6 → 语言能力
  # x7 x8 x9 → 速度能力
  
  
  
  ## 确认性因子分析（CFA）
  # visual 由 x1 x2 x3 测量
  # textual 由 x4 x5 x6 测量
  # speed 由 x7 x8 x9 测量
  
  
  model_cfa <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
  
  fit <- cfa(model_cfa, data = HolzingerSwineford1939)
  summary(fit, fit.measures = TRUE, standardized = TRUE)
  
  ## 完整结构方程模型（SEM）
  # textual ~ visual
  # speed ~ visual + textual
  
  
  model_sem <- '
# 测量模型
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9

# 结构路径
textual ~ visual
speed   ~ visual + textual
'
  
  model_sem <- '
# 测量模型
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9

# 结构路径
textual ~ visual
speed   ~ visual + textual
'
  fit_sem <- sem(model_sem, data = HolzingerSwineford1939)
  summary(fit_sem, fit.measures = TRUE, standardized = TRUE)
  
  
  fitMeasures(fit_sem)

  semPaths(fit_sem,
           what = "std",
           layout = "tree",
           edge.label.cex = 1.2)
  
  
  
  model_mediation <- '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6

textual ~ a*visual
speed   ~ b*textual + c*visual

indirect := a*b
total := c + (a*b)
'
  fit_group <- sem(model_sem,
                   data = HolzingerSwineford1939,
                   group = "school")
  
  fit_boot <- sem(model_sem,
                  data = HolzingerSwineford1939,
                  se = "bootstrap",
                  bootstrap = 2000)
  
  
  semPaths(fit_boot,
           what = "std",
           layout = "tree",
           edge.label.cex = 1.2)
  
  
}

