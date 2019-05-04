library(readr)
library(tidyverse)
library(data.table)
library(reshape)
library(emmeans)

setwd("/Users/anishakhosla/Desktop/GitHub/behavioural-teamsnax")

df_clean <- read_csv("df_clean.csv")

mdl <- lm (TrialTime ~ Delay*RewardType + (1|Participant), data = df_clean)

summary(mdl)
anova(mdl)

mdl <- lm (TrialTime ~ Delay*RewardType + (1|Participant), data = df_clean)

summary(mdl)
anova(mdl)


emmeans(mdl, list(pairwise ~ RewardType), adjust = "bonf")