library(dplyr)
library(tidymodels)

source("prep_functions.R")
set.seed(1233)
original <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
# Sales + rating data

# Release year as numeric. Missing coded as N/A - okay to be lost.
original$Year_release_n <- as.numeric(original$Year_of_Release)

# The rows are per release on console. So a multiple game could have
# multiple rows. We need to condense that.
games <- group_by(original, Name) %>%
  summarise(
    Platform = paste(Platform, collapse = "; "),
    # If there are multiple release years for multiple editions, count the earliest.
    Year_release_n = min(Year_release_n),
    Genre = Genre[1],
    Publisher = Publisher[1],
    # For copies sold, sum across editions
    NA_Sales = sum(NA_Sales, na.rm = T),
    Global_Sales = sum(Global_Sales, na.rm = T),
    Developer = Developer[1],
    Rating = Rating[1],
    Consoles_no = n()
  )

# Factors
rating_main_categories <- c("E", "E10+", "M", "T")
categorised_ratings <- games$Rating
categorised_ratings[!categorised_ratings %in% rating_main_categories] <- "Other"

categories_f_lvls <- c(rating_main_categories, "Other")
categorised_ratings <- factor(categorised_ratings, levels = categories_f_lvls)
games$Rating_F <- categorised_ratings

# Misc and missing should be other
genres <- games$Genre
genre_types <- unique(genres)


genres_to_other <- c("", "Misc")
genres[genres %in% genres_to_other] <- "Other"

genre_lvls <- c(
  # leave out the ones we replaced
  genre_types[!genre_types %in% genres_to_other],
  "Other"
  )

genres <- factor(genres, levels = genre_lvls)
games$Genre_F <- genres

# Remove years that are probably inaccurate - dataset from 2016
very_latest_years <- which(games$Year_release_n > 2016)
games$Year_release_n[very_latest_years] <- NA

# Dummy variables for consoles
consoles <- table(original$Platform)

# Make a table - we'll take 6 largest consoles as features and the rest as Other
consoles <- data.frame(
  console = names(consoles),
  count = as.vector(consoles)
) %>% arrange(desc(count))

main_consoles <- consoles$console[1:8]
other_consoles <- setdiff(consoles$console, main_consoles)

# Set up new columns for being released on specific console
console_colnames <- paste0("console_", main_consoles)
console_colnames <- c(console_colnames, "console_other")

for (i in 1:length(console_colnames)) {
  games[, console_colnames[i]] <- NA
}
