library(tidyverse)
library(ggplot2)
options(dplyr.width = Inf)

apps <- read_csv("Data/Google-Playstore-tidied.csv", locale = locale(encoding = "UTF-8"))

# Factorize columns
# Maybe make MinimumInstalls factor ordered ? 
apps <- apps %>% mutate(
    Category = as.factor(Category),
    # Make it Ordered !!!
    # levels = c("Everyone", "Everyone 10+", "Teen", "Mature 17+", "Adults only 18+")
    ContentRating = as.factor(ContentRating),
    Rating = as.ordered(Rating),
    MinimumInstalls = as.ordered(MinimumInstalls)
)


#-------------------------------------------
#           Data researching
#-------------------------------------------

#-------------------------------------------
#           Data visualization
#-------------------------------------------