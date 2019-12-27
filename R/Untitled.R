# library(tidyverse)
# library(haven)
# library(lme4)
# library(tidystats)
# popular2data <- read_sav(file ="https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%202/popularity/SPSS/popular2.sav?raw=true")
# popular2data <- select(popular2data, pupil, class, extrav, sex, texp, popular)
# 
# x <- lmer(formula = popular ~ 1 + (1|class),
#   data    = popular2data)
# summary(x)
# temp <- tidy_stats(x)
# 
# x <- lmer(formula = popular ~ 1 + sex + extrav + (1|class), 
#   data    = popular2data)
# summary(x)
# temp <- tidy_stats(x)
# 
# x <- lmer(popular ~ 1 + sex + extrav + texp + (1 | class), data=popular2data)
# summary(x)
# temp <- tidy_stats(x)
# 
# x <- lmer(formula = popular ~ 1 + sex + extrav + texp + (1 + sex + extrav | class),
#   data    = popular2data)
# summary(x)
# temp <- tidy_stats(x)
# 
# x <- lmer(formula = popular ~ 1 + sex + extrav + texp + (1 + extrav |class), 
#   data    = popular2data)
# summary(x)
# temp <- tidy_stats(x)
# 
# x<-lmer(formula = popular ~ 1 + sex + extrav + texp+ extrav:texp + (1 + extrav | class), 
#   data    = popular2data)
# summary(x)
# temp <- tidy_stats(x)
# 
