
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(emmeans)

# Create an empty list
results <- list()

# emmeans -----------------------------------------------------------------

# Run tests
warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)

warp_emm_wool_by_tension <- emmeans(warp_lm,  ~ wool | tension)
warp_list <- emmeans(warp_lm, poly ~ tension | wool, adjust = "sidak")
warp_tension_by_wool <- warp_list$emmeans
warp_contrasts <- warp_list$contrasts
warp_pairwise <- emmeans(warp_lm, specs = pairwise ~ wool:tension)

warp_emm_wool_by_tension
warp_emm_wool_by_tension
warp_list
warp_tension_by_wool
warp_contrasts
warp_pairwise$emmeans
warp_pairwise$contrasts

summary(warp_emm_wool_by_tension)
confint(warp_emm_wool_by_tension)
test(warp_emm_wool_by_tension)
contrast(warp_emm_wool_by_tension)
pairs(warp_emm_wool_by_tension)

# Tidy stats
temp <- tidy_stats(warp_emm_wool_by_tension)
temp <- tidy_stats(warp_list)
temp <- tidy_stats(warp_tension_by_wool)
temp <- tidy_stats(warp_contrasts)
temp <- tidy_stats(warp_pairwise$contrasts)

# Add stats
results <- results %>%
  add_stats(warp_emm_wool_by_tension) %>%
  add_stats(warp_list$emmeans) %>%
  add_stats(warp_list$contrasts) %>%
  add_stats(warp_pairwise$contrasts)

# Save stats
write_stats(results, "inst/test_data/emmeans.json")

# Github example ----------------------------------------------------------

library(afex)

anova_mod <- afex::aov_ez(id = "id", dv = "value", data = obk.long, 
  between = "treatment", within = c("phase", "hour"))

anova_emm <- emmeans::emmeans(anova_mod, ~ phase)

anova_emm_pairs <- pairs(anova_emm)
anova_emm_pairs

# tidy stats
temp <- tidy_stats(anova_emm_pairs)


# GitHub issue #8 ---------------------------------------------------------

library(afex)
library(tidystats)
library(emmeans)

participant <- rep(seq(1, 10), each = 21)
condition <- rep(c("cond_1", "cond_2", "cond_3"), times = 70)
time <- rep(seq(0, 30, 5), each = 3, times = 10)
response <- rnorm(n = 210, mean = c(1.1, 1.2, 1.3), sd = 0.1)

df <- data.frame(participant = factor(participant),
                 time = factor(time),
                 condition = factor(condition),
                 response)

model <- mixed(response ~ condition * time +
                 (condition | participant),
               df)

emm_results <- emmeans(model, "condition", by = "time") %>% 
  contrast("pairwise")

results <- list()

results <- results %>% 
  add_stats(emm_results)

write_stats(results, "test_results.json")




noise.lm <- lm(noise/10 ~ size * type * side, data = auto.noise)
anova(noise.lm)
emmeans(noise.lm, pairwise ~ size)
emmeans(noise.lm, ~ size * side * type)
