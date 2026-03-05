library(tidyverse)

set.seed(123)
df <- data.frame(
  Disease = sample(c(0,1), 100, replace = TRUE),   # 结局：0=No, 1=Yes
  Age = rnorm(100, 50, 10),                        # 连续变量
  Gender = sample(c("Male","Female"), 100, TRUE),  # 分类变量
  BMI = rnorm(100, 25, 4),
  Smoking = sample(c("Yes","No"), 100, TRUE)
)
