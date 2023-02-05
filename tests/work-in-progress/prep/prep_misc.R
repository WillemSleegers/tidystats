# Analysis: psych's ICC ---------------------------------------------------

# Load data
sf <- matrix(
  ncol = 4, byrow = TRUE,
  c(
    9, 2, 5, 8,
    6, 1, 3, 2,
    8, 4, 6, 8,
    7, 1, 2, 6,
    10, 5, 6, 9,
    6, 2, 4, 7
  )
)
colnames(sf) <- paste("J", 1:4, sep = "")
rownames(sf) <- paste("S", 1:6, sep = "")

# Perform analysis
psych_ICC <- ICC(sf, lmer = FALSE)

# Tidy stats
tidy_stats(psych_ICC)

# Add stats
results <- add_stats(results, psych_ICC)

# MANOVA

data <- iris

model7_1 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "Roy")
model7_2 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "")
model7_3 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species,
  data = iris
), test = "Roy")

model7_4 <- summary(manova(cbind(Sepal.Length, Petal.Length) ~ Species *
  Petal.Width, data = iris), test = "Roy")

# tidyversity
# Install and load tidyversity
# install_github("mkearney/tidyversity")
library(tidyversity)

# Ordinary Least Squares (OLS)
TVM_1 <- tidy_regression(polcom, follow_trump ~ news_1 + ambiv_sexism_1)
tidy_summary(TVM_1)

# Logistic (dichotomous)
polcom %>%
  tidy_regression(follow_trump ~ news_1 + ambiv_sexism_1, type = "logistic") %>%
  tidy_summary()

# Poisson (count)
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "poisson") %>%
  tidy_summary()

# Negative binomial (overdispersed)
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1, type = "negbinom") %>%
  tidy_summary()

# Robust and quasi- models
polcom %>%
  dplyr::mutate(polarize = abs(therm_1 - therm_2)) %>%
  tidy_regression(polarize ~ news_1 + ambiv_sexism_1,
    type = "quasipoisson",
    robust = TRUE
  ) %>%
  tidy_summary()

# ANOVA
polcom %>%
  dplyr::mutate(
    sex = ifelse(sex == 1, "Male", "Female"),
    vote_choice = dplyr::case_when(
      vote_2016_choice == 1 ~ "Clinton",
      vote_2016_choice == 2 ~ "Trump",
      TRUE ~ "Other"
    )
  ) %>%
  tidy_anova(pp_party ~ sex * vote_choice) %>%
  tidy_summary()

# t-tests
polcom %>%
  tidy_ttest(pp_ideology ~ follow_trump) %>%
  tidy_summary()

# Structural equation modeling (SEM)
polcom %>%
  dplyr::mutate(
    therm_2 = 10 - therm_2 / 10,
    therm_1 = therm_1 / 10
  ) %>%
  tidy_sem(
    news = ~ news_1 + news_2 + news_3 + news_4 + news_5 + news_6,
    ambiv_sexism = ~ ambiv_sexism_1 + ambiv_sexism_2 + ambiv_sexism_3 +
      ambiv_sexism_4 + ambiv_sexism_5 + ambiv_sexism_6,
    partisan = ~ a * therm_1 + a * therm_2,
    ambiv_sexism ~ age + hhinc + edu + news + partisan
  ) %>%
  tidy_summary()

# Cronbach's alpha
cronbachs_alpha(polcom, ambiv_sexism_1:ambiv_sexism_6)

# Analysis: ppcor’s pcor.test() -------------------------------------------

# Load package
library(ppcor)

# Get data
y.data <- data.frame(
  hl = c(7, 15, 19, 15, 21, 22, 57, 15, 20, 18),
  disp = c(0.000, 0.964, 0.000, 0.000, 0.921, 0.000, 0.000, 1.006, 0.000, 1.011),
  deg = c(9, 2, 3, 4, 1, 3, 1, 3, 6, 1),
  BC = c(
    1.78e-02, 1.05e-06, 1.37e-05, 7.18e-03, 0.00e+00, 0.00e+00, 0.00e+00,
    4.48e-03, 2.10e-06, 0.00e+00
  )
)

# Run analysis
pcor_correlation <- pcor.test(y.data$hl, y.data$disp, y.data[, c("deg", "BC")])
pcor_correlation
