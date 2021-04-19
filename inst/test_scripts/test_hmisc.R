
# Setup -------------------------------------------------------------------

# Load packages
library(tidystats)
library(tidyverse)
library(Hmisc)

# Create an empty list 
results <- list()

# rcorr -------------------------------------------------------------------

# Example 1
x <- c(-2, -1, 0, 1, 2)
y <- c(4,   1, 0, 1, 4)
z <- c(1,   2, 3, 4, NA)
v <- c(1,   2, 3, 4, 5)

rcorr <- rcorr(cbind(x, y, z, v))
rcorr
