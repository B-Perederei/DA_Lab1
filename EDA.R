library(tidyverse)
library(ggplot2)
options(dplyr.width = Inf)

apps <- read_csv("Data/Google-Playstore.csv", locale = locale(encoding = "UTF-8"))
names(apps) <- str_replace_all(names(apps), c(" " = ""))

#-------------------------------------------
#           Cleaning data
#-------------------------------------------

# Remove Installes, ScrapedTime columns
# Maybe also remove: "Privacy policy", "Developer email", "Developer website"?
apps <- apps %>% select(-c(Installs, ScrapedTime))

# Factorize columns
apps <- apps %>% mutate(
    Category = as.factor(Category),
    ContentRating = as.factor(ContentRating),
)

# Convert Released values into a 'year-month-day' date format
apps$Released <- apps$Released %>% parse_date(format = "%b %d, %Y", na = "NA")
apps$LastUpdated <- apps$LastUpdated %>% parse_date(format = "%b %d, %Y", na = "NA")

#substitute from column "Minimum Android" values "Varies with device" with NA
apps$MinimumAndroid[apps$MinimumAndroid == "Varies with device"] <- NA

# Reform "Minimum Android" values appearence from "x.x.x and up" to "x.x.x"
# Maybe save Maximum Android?
apps$MinimumAndroid <- str_extract(apps$MinimumAndroid, "\\d(\\.\\d)+")

# Tiding size from chr to num un mega-bytes
apps$Size[apps$Size == "Varies with device"] <- NA
# Function to reform string "Size" values to float representing size of app in mega-bytes
str_to_megabytes_num <- function(x) {
	return(x %>% str_replace(",", "\\.") %>% str_extract("\\d+(\\.\\d+)?") %>% as.numeric)
}

apps$Size <- ifelse(apps$Size %>% str_detect("k$"),
                    str_to_megabytes_num(apps$Size) / 1000,
                    str_to_megabytes_num(apps$Size))

apps["Currency"][apps["Currency"] == "XXX"] <- NA

# Save tidied data
# write_csv(apps, "Data/Google-Playstore-tidied.csv")

# Under a question to delete
# apps <- apps %>% drop_na(AppName, Rating, RatingCount, MinimumInstalls,
#                        Currency, Size, MinimumAndroid, DeveloperId,
#                        DeveloperEmail, Released)

#-------------------------------------------
#           Data visualization
#-------------------------------------------