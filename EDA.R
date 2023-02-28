library(tidyverse)
library(ggplot2)
options(dplyr.width = Inf)

apps <- read_csv("Data/Google-Playstore-tidied.csv", locale = locale(encoding = "UTF-8"))

# Factorize columns
apps <- apps %>% mutate(
    Category = as.factor(Category),
    ContentRating = as.factor(ContentRating),
)

#-------------------------------------------
#           Data researching
#-------------------------------------------

#-------------------------------------------
#           Data visualization
#-------------------------------------------