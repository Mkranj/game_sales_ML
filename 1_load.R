library(dplyr)
library(tidymodels)

source("prep_functions.R")
set.seed(1233)
original <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
# Sales + rating data

# Release year as numeric. Missing coded as N/A - okay to be lost.
original$Year_release_n <- as.numeric(original$Year_of_Release)


# Factors
rating_main_categories <- c("E", "E10+", "M", "T")
categorised_ratings <- original$Rating
categorised_ratings[!categorised_ratings %in% rating_main_categories] <- "Other"

categories_f_lvls <- c(rating_main_categories, "Other")
categorised_ratings <- factor(categorised_ratings, levels = categories_f_lvls)
original$Rating_F <- categorised_ratings

# Misc and missing should be other
genres <- original$Genre
genre_types <- unique(genres)


genres_to_other <- c("", "Misc")
genres[genres %in% genres_to_other] <- "Other"

genre_lvls <- c(
  # leave out the ones we replaced
  genre_types[!genre_types %in% genres_to_other],
  "Other"
  )

genres <- factor(genres, levels = genre_lvls)
original$Genre_F <- genres
