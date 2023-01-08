
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(emmeans)

# Create an empty list
statistics <- list()

# emmeans() --------------------------------------------------------------------

# Run analyses
model <- lm(displ ~ year + cyl + drv + cty, data = mpg)

emm_year <- emmeans(model, specs = ~ year)
emm_year_cyl <- emmeans(model,  ~ year + cyl)
emm_year_by_cyl <- emmeans(model,  ~ year | cyl)
emm_cyl_by_year <- emmeans(model, ~ cyl | year)
emm_year_cyl_by_drv_cty <- emmeans(model, ~ year + cyl | drv + cty)
emm_poly_year_by_cyl <- emmeans(model, poly ~ year | cyl, adjust = "sidak")

mpg_lm <- lm(displ ~ year + cyl + drv + cty, data = mpg)
emm_2x2 <- emmeans(mpg_lm, ~ cyl + cty | year + drv)
emm_2x2_flipped <- emmeans(mpg_lm, ~ year + drv | cyl + cty)

# Add stats
statistics <- add_stats(statistics, emm_year_cyl_by_drv_cty)

statistics <- statistics %>%
  add_stats(emm_year) %>%
  add_stats(emm_specs) %>%
  add_stats(emm_spec_by) %>%
  add_stats(emm_spec_by_adjust) %>%
  add_stats(emm_2x2) %>%
  add_stats(emm_2x2_flipped)

# Inspect output
emm_spec
emm_specs
emm_spec_by
emm_spec_by_adjust
emm_2x2
emm_2x2_flipped

# test() ------------------------------------------------------------------

# Run analysis
pigs_lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
emmeans_response <- emmeans(pigs_lm, specs = "percent", type = "response")

emmeans_test <- test(
  emmeans_response, 
  null = log(35), 
  delta = log(1.10), 
  side = ">"
)
emmeans_test_joint <- test(emmeans_response, joint = TRUE)

# Add stats
statistics <- statistics %>%
  add_stats(emmeans_response) %>%
  add_stats(emmeans_test) %>%
  add_stats(emmeans_test_joint)

# Inspect output
emmeans_response
emmeans_test
emmeans_test_joint

# contrast() -------------------------------------------------------------

# Run analyses
warp_lm <- lm(breaks ~ wool * tension, data = warpbreaks)
warp_emm <- emmeans(warp_lm, ~ tension | wool)

warp_contrast_poly <- contrast(warp_emm, "poly")
warp_constrast_simple <- contrast(warp_emm, simple = c("wool", "tension"))
warp_contrast_list <- contrast(warp_emm, simple = list("wool", "tension"))

tw_emm <- contrast(
  warp_emm, 
  interaction = c(tension = "poly", wool = "consec"), 
  by = NULL
)

iris_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
emmeans(iris_lm, specs = c("Sepal.Width", "Petal.Length"))
iris_contrast <- contrast(
  warp_emm, 
  interaction = c(tension = "poly", wool = "consec"), 
  by = NULL
)

mod <- lm(Water.Temp ~ poly(stack.loss, degree = 2), data = stackloss)
emm <- emmeans(mod, "stack.loss", at = list(stack.loss = 10 * (1:4)))
emm

# Convert results from Celsius to Fahrenheit:
confint(contrast(emm, "identity", scale = 9/5, offset = 32))

emmeans_contrast <- contrast(pigs_emm, "consec")

# Add stats
statistics <- statistics %>%
  add_stats(tw_emm)

# Inspect output
emmeans_contrast
tw_emm

# mvcontrast() ------------------------------------------------------------

# Get data
MOats.lm <- lm(yield ~ Variety + Block, data = MOats)
MOats.emm <- emmeans(MOats.lm, ~ Variety | rep.meas)

# Run analysis
emmeans_mvcontrast <- mvcontrast(MOats.emm, "consec", show.ests = TRUE)

# Test each mean against a specified null vector
emmeans_mvcontrast_named <- mvcontrast(
  MOats.emm, 
  "identity", 
  name = "Variety", 
  null = c(80, 100, 120, 140)
)

# Add stats
results = results %>%
  add_stats(emmeans_mvcontrast) %>%
  add_stats(emmeans_mvcontrast_named)

# Inspect output
emmeans_mvcontrast
emmeans_mvcontrast_named

# eff_size() --------------------------------------------------------------------

# Run analysis
fiber.lm <- lm(strength ~ diameter + machine, data = fiber)
emm <- emmeans(fiber.lm, "machine")
emmeans_eff_size <- eff_size(emm, sigma = sigma(fiber.lm), edf = df.residual(fiber.lm))

# Add stats
results = results %>%
  add_stats(emmeans_eff_size)

# Inspect output
emmeans_eff_size

# emtrends() --------------------------------------------------------------

# Run analysis
fiber.lm <- lm(strength ~ diameter*machine, data=fiber)
# Suppose we want trends relative to sqrt(diameter)...
emtrends_basic = emtrends(fiber.lm, ~ machine | diameter, 
  var = "sqrt(diameter)", at = list(diameter = c(20, 30)))

# Obtaining a reference grid
mtcars.lm <- lm(mpg ~ poly(disp, degree = 2) * (factor(cyl) + factor(am)), 
  data = mtcars)

# Center trends at mean disp for each no. of cylinders
emtrends_cov_reduce <- emtrends(mtcars.lm, var = "disp", 
  cov.reduce = disp ~ factor(cyl))

# Add stats
results = results %>%
  add_stats(emtrends_basic) %>%
  add_stats(emtrends_cov_reduce)

# Inspect output
emtrends_basic
emtrends_cov_reduce

# joint_tests() -----------------------------------------------------------

# Get data
pigs.lm <- lm(log(conc) ~ source * factor(percent), data = pigs)

# Run analysis
joint_tests_single <- joint_tests(pigs.lm)
## separate joint tests of 'percent'
joint_tests_multi <- joint_tests(pigs.lm, by = "source")

# Add stats
results = results %>%
  add_stats(joint_tests_single) %>%
  add_stats(joint_tests_multi)

# Inspect output
joint_tests_single
joint_tests_multi

# ref_grid() --------------------------------------------------------------

# Get data
fiber.lm <- lm(strength ~ machine*diameter, data = fiber)

# Run analysis
ref_grid_results = ref_grid(fiber.lm)

# Add stats
results = results %>%
  add_stats(ref_grid_results)

# Inspect output
ref_grid_results
summary(ref_grid_results)

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

# tidy_stats_to_data_frame() ----------------------------------------------

df <- tidy_stats_to_data_frame(results)

# write_stats() -----------------------------------------------------------

write_test_stats(statistics, "tests/testthat/data/emmeans.json")

# Cleanup -----------------------------------------------------------------







# archive -----------------------------------------------------------------


# emmeans -----------------------------------------------------------------

group_statistics <- function(df) {
  statistics <- list()
  
  statistics <- add_statistic(
    statistics, "null", 
    df$null, 
    symbol = "ŷ",
    subscript = "null"
  )
  statistics <- add_statistic(
    statistics, 
    name = "estimate", 
    df$emmean, 
    symbol = "EMM", 
    interval = "CI", 
    level = parse_number(attr(df, "mesg")), 
    lower = df$lower.CL,
    upper = df$upper.CL
  )
  statistics <- add_statistic(
    statistics, "estimate", 
    df$estimate, 
    symbol = "b"
  )
  statistics <- add_statistic(
    statistics, "estimate", 
    df$response, 
    symbol = "ŷ",
    interval = "CI", 
    level = parse_number(attr(df, "mesg")[2]), 
    lower = df$lower.CL,
    upper = df$upper.CL
  )
  statistics <- add_statistic(statistics, "SE", df$SE)
  statistics <- add_statistic(
    statistics, 
    "statistic", 
    df$t.ratio, 
    symbol = "t"
  )
  statistics <- add_statistic(statistics, "statistic", df$F.ratio, "F")
  statistics <- add_statistic(statistics, "df", df$df)
  statistics <- add_statistic(
    statistics, 
    "df numerator", 
    df$df1[i], 
    "df", 
    "num."
  )
  statistics <- add_statistic(
    statistics, 
    "df denominator", 
    df$df2[i], 
    "df", 
    "den."
  )
  statistics <- add_statistic(statistics, "p", df$p.value)
}

group_pri_vars_statistics <- function(df, pri_vars) {
  # Create a group for the pri var
  group <- list(name = pri_vars)
  
  # Extract levels
  levels <- unique(df[, pri_vars])
  
  # Loop over the levels of the group
  for (i in 1:length(levels)) {
    # Create an empty terms group list
    group_level <- list(name = as.character(levels[i]))
    
    # Get statistics
    statistics <- group_statistics(df[i, ])
    
    # Add statistics to the term group
    group_level$statistics <- statistics
    
    # Add the level group to the group
    group$groups <- append(group$groups, list(group_level))
  }
  
  return(group)
}

group_vars <- function(vars, df) {
  group <- list(name = vars[1])
  
  # Convert to a factor in case it's a numeric value
  levels <- levels(factor(df[, vars[1]]))
  
  for (i in 1:length(levels)) {
    group_level <- list(name = as.character(levels[i]))
    df_level <- df[df[, vars[1]] == levels[i], ]
    
    if (length(vars) > 1) {
      group_level_group <- group_vars(
        vars[-1], 
        df_level
      )
      group_level$groups <- append(group_level$groups, list(group_level_group))
    } else {
      group_level$statistics <- group_statistics(df_level)
    }
    
    group$groups <- append(group$groups, list(group_level))
  }
  
  return(group)
}

# Function to extract statistics from pri vars
group_pri_vars_statistics <- function(df, pri_vars) {
  # Create a group for the pri var
  group <- list(name = pri_vars)
  
  # Extract levels
  levels <- unique(df[, pri_vars])
  
  # Loop over the levels of the group
  for (i in 1:length(levels)) {
    # Create an empty terms group list
    group_level <- list(name = as.character(levels[i]))
    
    # Create a statistics list
    statistics <- list()
    
    # Add the statistics
    statistics <- add_statistic(
      statistics, "estimate", df$response[i], symbol = "ŷ"
    )
    statistics <- add_statistic(
      statistics, 
      name = "estimate", 
      df$emmean[i], 
      symbol = "EMM", 
      interval = "CI", 
      level = parse_number(attr(df, "mesg")), 
      lower = df$lower.CL[i],
      upper = df$upper.CL[i]
    )
    statistics <- add_statistic(statistics, "SE", df$SE[i])
    statistics <- add_statistic(statistics, "df", df$df[i])
    statistics <- add_statistic(
      statistics, "statistic", df$t.ratio[i], symbol = "t"
    )
    statistics <- add_statistic(statistics, "p", df$p.value[i])
    
    # Add statistics to the term group
    group_level$statistics <- statistics
    
    # Add the level group to the group
    group$groups <- append(group$groups, list(group_level))
  }
  
  return(group)
}

group_by_vars_statistics <- function(df, by_vars, pri_vars) {
  # Create a group for the by var
  group <- list(name = by_vars)
  
  # Loop over the levels of the by var
  levels <- levels(df[, by_vars])
  
  for (i in 1:length(levels)) {
    # Create a group for the level of the by var
    group_level <- list()
    
    # Set the name
    group_level$name <- levels[i]
    
    # Create a group with pri vars statistics
    df_level <- dplyr::filter(
      df, 
      dplyr::if_all(dplyr::all_of(by_vars), ~ . == levels[i])
    )
    
    group_pri <- group_pri_vars_statistics(df_level, pri_vars)
    
    # Add the pri group to the by level group
    group_level$groups <- append(group_level$groups, list(group_pri))
    
    # Add the by level group to the by group
    group$groups <- append(group$groups, list(group_level))
  }
  
  return(group)
}

group_contrast_statistics <- function(df, pri_vars) {
  # Create a group for the pri var
  group <- list(name = pri_vars)
  
  # Extract levels
  levels <- levels(df[, pri_vars])
  
  # Loop over the levels of the group
  for (i in 1:length(levels)) {
    # Create an empty terms group list
    group_level <- list(name = levels[i])
    
    # Loop over the contrasts
    levels_contrast <- levels(df$contrast)
    
    for (j in 1:length(levels_contrast)) {
      # Create a group for the contrast
      group_contrast <- list(name = levels_contrast[j])
      
      # Subset the data frame to the relevant contrast
      df_contrast <- dplyr::filter(df, contrast == levels_contrast[j])
      
      # Create a list for the statistics
      statistics <- list()
      
      statistics <- add_statistic(
        statistics, 
        "estimate", 
        df_contrast$estimate[j], 
        "b"
      )
      statistics <- add_statistic(statistics, "SE", df_contrast$SE[j])
      statistics <- add_statistic(statistics, "df", df_contrast$df[j])
      statistics <- add_statistic(
        statistics, 
        "statistic", 
        df_contrast$t.ratio[j], 
        "t"
      )
      statistics <- add_statistic(statistics, "p", df_contrast$p.value[j])
      
      # Add the statistics to the contrast group
      group_contrast$statistics <- statistics
      
      # Add the contrast group to the level group
      group_level$groups <- append(group_level$groups, list(group_contrast))
    }
    
    # Add the level group to the group
    group$groups <- append(group$groups, list(group_level))
  }
  
  return(group)
}



analysis <- list()

df <- as.data.frame(x)

analysis$method <- dplyr::case_when(
  attr(x, "estName") == "emmean" ~ "Estimated marginal means",
  attr(x, "estName") == "estimate" ~ "Contrasts",
  attr(x, "estName") == "response" ~ "Predicted estimated marginal means",
  attr(x, "estName") == "F.ratio" & "F.ratio" %in% names(df) ~ "F test",
)

pri_vars <- attr(x, "pri.vars")
by_vars <- attr(x, "by.vars")
vars <- c(by_vars, pri_vars)

if (length(vars) > 0) {
  group <- group_vars(vars, df)
  
  analysis$groups <- append(analysis$groups, list(group))
} else if (length(by_vars > 0)) {
  
  names <- ""
  
  for (i in 1:length(by_vars)) {
    names <- c(names, paste(by_vars[i], "=", df[by_vars[i]]))
  }
  
  df[, by_vars]
} else {
  statistics <- group_statistics(df)
  
  analysis$statistics <- statistics
}

analysis <- add_package_info(analysis, "emmeans")

return(analysis)