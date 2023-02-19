# Don't forget to download libraries!
library(tidyverse)
library(ggplot2)
options(dplyr.width = Inf)

apps <- read_csv("Data/Google-Playstore.csv")
names(apps) <- str_replace_all(names(apps), c(" " = ""))

str(apps, give.attr = FALSE)

# Cleaning data
apps <- apps %>% mutate(
    Category = as.factor(Category),
    ContentRating = as.factor(ContentRating)
)

apps <- apps %>% select(-c(Installs, ScrapedTime))
apps["Currency"][apps["Currency"] == "XXX"] <- NA

apps %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%
select(where(~ all(.) > 0))

apps %>% summarise(across(everything(), ~ mean(is.na(.)))) %>%
select(where(~ all(.) > 0))

apps <- apps %>% drop_na(AppName, Rating, RatingCount, Installs,
MinimumInstalls, Currency, Size, MinimumAndroid, DeveloperId, DeveloperEmail)