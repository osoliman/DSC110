install.packages("MatchIt")

library(MatchIt)

df <- read.csv("https://raw.githubusercontent.com/osoliman/DSC110/refs/heads/main/Datasets/tylenol_autism.csv")

head(df)

prop.table(table(df$tylenol, df$autism, dnn = c("Tylenol", "Autism")), margin = 1)

naive <- glm(autism ~ tylenol, data = df, family = binomial)
exp(coef(naive))
summary(naive)

#Regression
reg <- glm(autism ~ tylenol + family_history + fever, data = df, family = binomial)
summary(reg)
exp(coef(reg))

#Matching
m <- matchit(tylenol ~ fever + family_history, data = df, caliper = 0.05)
summary(m)$nn
matched <- match.data(m)
head(matched)
match_model <- glm(autism ~ tylenol, data = matched, family = binomial)
exp(coef(match_model))
summary(match_model)

#Weighting
ps_model <- glm(tylenol ~ fever + family_history, data = df, family = binomial)
df$ps <- predict(ps_model, type = 'response')
df$w <- df$w  <- ifelse(df$tylenol == 1, 1 / df$ps, 1 / (1 - df$ps))

ipw <- glm(autism ~ tylenol, data = df, family = quasibinomial, weights = w)
exp(coef(ipw))
summary(ipw)






