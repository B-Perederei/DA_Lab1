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

# Convert Released values into a 'year-month-day' date format
apps$Released <- apps$Released %>% parse_date(format = "%b %d, %Y", na = "NA")
apps$LastUpdated <- apps$LastUpdated %>% parse_date(format = "%b %d, %Y", na = "NA")

# Substitute from column "Minimum Android" values "Varies with device" with NA
apps$MinimumAndroid[apps$MinimumAndroid == "Varies with device"] <- NA

# Create new column WearOs, which has logical values: true if app is used for wearable device
# like smart watch, false otherwise
apps$WearOS <- with(apps, 
				    case_when(str_detect(apps$MinimumAndroid, "[wW]") ~ TRUE,
					is.na(apps$MinimumAndroid) ~ NA,
					TRUE ~ FALSE))
					
# devide MinimumAndroid into two columns: MinimumAndroid and MaximumAndroid
apps <- apps %>% separate(MinimumAndroid, 
						  into = c("MinimumAndroid", "MaximumAndroid"), 
						  sep = "( \\- )|( and )")


# set value TRUE for Free-apps such that are non-free and cost is 0(remove ambiguaty)
apps$Free[!apps$Free & apps$Price == 0] <- TRUE

# Tiding size from chr to num un mega-bytes
apps$Size[apps$Size == "Varies with device"] <- NA
# Function to reform string "Size" values to float representing size of app in mega-bytes
str_to_megabytes_num <- function(x) {
	return(x %>% str_replace(",", "\\.") %>% str_extract("\\d+(\\.\\d+)?") %>% as.numeric)
}

apps$Size <- ifelse(apps$Size %>% str_detect("k$"),
                    str_to_megabytes_num(apps$Size) / 1000,
                    str_to_megabytes_num(apps$Size))

# Maybe we don't need to do that for not mixing data
# we don't know with values which is valid empty data?
apps["Currency"][apps["Currency"] == "XXX"] <- NA
apps["ContentRating"][apps["ContentRating"] == "Unrated"] <- NA

# Under a question to delete
# apps <- apps %>% drop_na(AppName, Rating, RatingCount, MinimumInstalls,
#                        Currency, Size, MinimumAndroid, DeveloperId,
#                        DeveloperEmail, Released)

# Save tidied data
write_csv(apps, "Data/Google-Playstore-tidied.csv", na = "")