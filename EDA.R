# Don't forget to download libraries!
library(tidyverse)
library(ggplot2)
options(dplyr.width = Inf)

apps <- read_csv("Data/Google-Playstore.csv")
names(apps) <- str_replace_all(names(apps), c(" " = ""))

str(apps, give.attr = FALSE)

apps <- apps %>% mutate(
    Category = as.factor(Category),
    ContentRating = as.factor(ContentRating),
    # True/False variable (Should it be converted to factor)?
    # Free = as.factor(Free),
    # AdSupported = as.factor(AdSupported), 
    # InAppPurchases = as.factor(InAppPurchases),
    # EditorsChoice = as.factor(EditorsChoice)
)

apps %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%
select(where(~ all(.) > 0))

apps %>% summarise(across(everything(), ~ mean(is.na(.)))) %>%
select(where(~ all(.) > 0))