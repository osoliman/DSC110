df <- read.csv("https://raw.githubusercontent.com/osoliman/DSC110/refs/heads/main/Datasets/Causal_Inference_RCT.csv")

head(df)

install.packages("effectsize")
library(effectsize)

result <- t.test(df$Treatment, df$Control, paired = FALSE)
es     <- cohens_d(df$Treatment, df$Control)

result$p.value
result$conf.int
es

t.test(df$Treatment)$conf.int   # CI for Treatment mean
t.test(df$Control)$conf.int     # CI for Control mean
