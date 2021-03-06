% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_stats.R
\name{tidy_stats}
\alias{tidy_stats}
\alias{tidy_stats.htest}
\alias{tidy_stats.lm}
\alias{tidy_stats.glm}
\alias{tidy_stats.anova}
\alias{tidy_stats.aov}
\alias{tidy_stats.aovlist}
\alias{tidy_stats.tidystats_descriptives}
\alias{tidy_stats.tidystats_counts}
\alias{tidy_stats.lmerMod}
\alias{tidy_stats.lmerModLmerTest}
\alias{tidy_stats.BFBayesFactor}
\alias{tidy_stats.afex_aov}
\alias{tidy_stats.emmGrid}
\alias{tidy_stats.emm_list}
\title{Tidy the output of a statistics object}
\usage{
tidy_stats(x)

\method{tidy_stats}{htest}(x)

\method{tidy_stats}{lm}(x)

\method{tidy_stats}{glm}(x)

\method{tidy_stats}{anova}(x)

\method{tidy_stats}{aov}(x)

\method{tidy_stats}{aovlist}(x)

\method{tidy_stats}{tidystats_descriptives}(x)

\method{tidy_stats}{tidystats_counts}(x)

\method{tidy_stats}{lmerMod}(x)

\method{tidy_stats}{lmerModLmerTest}(x)

\method{tidy_stats}{BFBayesFactor}(x)

\method{tidy_stats}{afex_aov}(x)

\method{tidy_stats}{emmGrid}(x)

\method{tidy_stats}{emm_list}(x)
}
\arguments{
\item{x}{The output of a statistical test.}
}
\description{
\code{tidy_stats} is used to convert the output of a statistical object to a
list of organized statistics. The \code{tidy_stats} function is automatically 
run when \code{add_stats} is used, so there is generally no need to use this 
function explicitly. It can be used, however, to peek at how the output of a 
specific analysis will be organized.
}
\details{
Please note that not all statistical tests are supported. See 'Details' 
below for a list of supported statistical tests.


Currently supported functions:

\code{stats}:
\itemize{
  \item \code{t.test()}
  \item \code{cor.test()}
  \item \code{chisq.test()}
  \item \code{wilcox.test()}
  \item \code{fisher.test()}
  \item \code{oneway.test()}
  \item \code{lm()}
  \item \code{glm()}
  \item \code{aov()}
  \item \code{anova()}
}

\code{lme4}/\code{lmerTest}:
\itemize{
  \item \code{lmer()}
}

\code{BayesFactor}:
\itemize{
  \item \code{generalTestBF()}
  \item \code{lmBF()}
  \item \code{regressionBF()}
  \item \code{ttestBF()}
  \item \code{anovaBF()}
  \item \code{correlationBF()}
  \item \code{contingencyTableBF()}
  \item \code{proportionBF()}
  \item \code{meta.ttestBF()}
}

\code{tidystats}:
\itemize{
  \item \code{describe_data()}
  \item \code{count_data()}
}
}
\section{Methods (by class)}{
\itemize{
\item \code{htest}: tidy_stats method for class 'htest'

\item \code{lm}: tidy_stats method for class 'lm'

\item \code{glm}: tidy_stats method for class 'glm'

\item \code{anova}: tidy_stats method for class 'anova'

\item \code{aov}: tidy_stats method for class 'aov'

\item \code{aovlist}: tidy_stats method for class 'aovlist'

\item \code{tidystats_descriptives}: tidy_stats method for class 'tidystats_descriptives'

\item \code{tidystats_counts}: tidy_stats method for class 'tidystats_counts'

\item \code{lmerMod}: tidy_stats method for class 'lmerMod'

\item \code{lmerModLmerTest}: tidy_stats method for class 'lmerModLmerTest'

\item \code{BFBayesFactor}: tidy_stats method for class 'BayesFactor'

\item \code{afex_aov}: tidy_stats method for class 'afex_aov'

\item \code{emmGrid}: tidy_stats method for class 'emmGrid'

\item \code{emm_list}: tidy_stats method for class 'emm_list'
}}

\examples{
# Conduct statistical tests
# t-test:
sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)

# lm:
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
weight <- c(ctl, trt)
lm_D9 <- lm(weight ~ group)

# ANOVA:
npk_aov <- aov(yield ~ block + N*P*K, npk)

# Tidy the statistics and store each analysis in a separate variable
list_sleep_test <- tidy_stats(sleep_test)
list_lm_D9 <- tidy_stats(lm_D9)
list_npk_aov <- tidy_stats(npk_aov)

# Now you can inspect each of these variables, e.g.,:
names(list_sleep_test)
str(list_sleep_test)

}
