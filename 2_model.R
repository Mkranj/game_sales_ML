tt_split <- initial_split(original)

tt_training <- training(tt_split)
tt_testing <- testing(tt_split)


# Prep work - specific for partitions
dev_summary <- calculate_dev_summaries(tt_training)

tt_training$Top_performer_dev <- 
  mark_top_performer_dev(input_df = tt_training, summarised_df = dev_summary, summary_metric = "avg_income", n_top = 10)

tt_testing$Top_performer_dev <- 
  mark_top_performer_dev(input_df = tt_testing, summarised_df = dev_summary, summary_metric = "avg_income", n_top = 10)


tt_training$Dev_games_made <- 
  mark_top_performer_dev(input_df = tt_training, summarised_df = dev_summary, summary_metric = "no_games", n_top = 10)

tt_testing$Dev_games_made <- 
  mark_top_performer_dev(input_df = tt_testing, summarised_df = dev_summary, summary_metric = "no_games", n_top = 10)



# Define variables to be used
predictors <- c("Year_release_n", "Genre_F", "Rating_F", "Top_performer_dev", "Dev_games_made")
criterium <- "Global_Sales"

# Imputing missing values
# Postprocessing - can't be negative


linear_model <- linear_reg()

training_fit <- linear_model |> 
  fit_xy(x = tt_training[, predictors],
        y =tt_training[, criterium])
training_fit

# Metrics
training_results <- tt_testing |> 
  select(Global_Sales) |> 
  bind_cols(
    predict(training_fit, new_data = tt_testing[, predictors])
  )

training_results |> metrics(truth = Global_Sales, estimate = .pred)
