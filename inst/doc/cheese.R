## ------------------------------------------------------------------------
#Load packages
require(tidyverse); require(cheese)

#Look at the top ten rows
heart_disease


## ------------------------------------------------------------------------
#Default table
heart_disease %>%
    univariate_table

