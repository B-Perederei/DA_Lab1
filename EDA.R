# Don't forget to download libraries!
library(tidyverse)
library(ggplot2)
options(dplyr.width = Inf)

apps <- read_csv("Data/Google-Playstore.csv", locale = locale(encoding = "UTF-8"))
names(apps) <- str_replace_all(names(apps), c(" " = ""))

# Cleaning data

#factorize Category and ContentRating
apps <- apps %>% mutate(
    Category = as.factor(Category),
    ContentRating = as.factor(ContentRating)
)

#convert Released values into a 'year-month-day' date format
apps$Released <- apps$Released %>% parse_date(format = "%b %d, %Y", na = "NA")

apps$LastUpdated <- apps$LastUpdated %>% parse_date(format = "%b %d, %Y", na = "NA")

#remove Installes, ScrapedTime columns
#maybe also remove: "Privacy policy", "developer email", "developer website"?
apps <- apps %>% select(-c(Installs, ScrapedTime))

apps["Currency"][apps["Currency"] == "XXX"] <- NA

#substitute from column "Minimum Android" values "Varies with device" with NA
apps$MinimumAndroid[apps$MinimumAndroid == "Varies with device"] <- NA

#reform "Minimum Android" values appearence from "x.x.x and up" to "x.x.x"
apps$MinimumAndroid <- str_extract(apps$MinimumAndroid, "\\d(\\.\\d)+")

apps$Size[apps$Size == "Varies with device"] <- NA

# function to reform string "Size" values to float representing size of app in mega-bytes
str_to_megabytes_num <- function(x) {
	return(x %>% str_replace(",", "\\.") %>% str_extract("\\d+(\\.\\d+)?") %>% as.numeric)
}

apps$Size <- ifelse(apps$Size %>% str_detect("k$"), convert_to_num(apps$Size)/1000, convert_to_num(apps$Size))

# save tidied data
# there is a bug to be fixed with Size values - they all get lost after saving to csv
write_csv(apps, "Data/Google-Playstore-tidied.csv")

# under a question to delete
apps <- apps %>% drop_na(AppName, Rating, RatingCount, MinimumInstalls, Currency, Size, MinimumAndroid, DeveloperId, DeveloperEmail, Released)

#-------------------------------------------
# 			Data visualization
#-------------------------------------------
apps %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%
select(where(~ all(.) > 0))

apps %>% summarise(across(everything(), ~ mean(is.na(.)))) %>%
select(where(~ all(.) > 0))
