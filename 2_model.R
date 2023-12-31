tt_split <- initial_split(games)

tt_training <- training(tt_split)
tt_testing <- testing(tt_split)

library(recipes)
# Prep work - specific for partitions
dev_summary <- calculate_dev_summaries(tt_training)

tt_training$Top_performer_dev <- 
  mark_top_performer_dev(input_df = tt_training,
                         summarised_df = dev_summary,
                         summary_metric = "avg_income",
                         n_top = 10) |> 
  as.factor()

tt_testing$Top_performer_dev <- 
  mark_top_performer_dev(input_df = tt_testing,
                         summarised_df = dev_summary,
                         summary_metric = "avg_income",
                         n_top = 10) |> 
  as.factor()

tt_training$Dev_games_made <- 
  mark_top_performer_dev(input_df = tt_training,
                         summarised_df = dev_summary,
                         summary_metric = "no_games",
                         n_top = 10) |> 
  as.factor()

tt_testing$Dev_games_made <- 
  mark_top_performer_dev(input_df = tt_testing,
                         summarised_df = dev_summary,
                         summary_metric = "no_games",
                         n_top = 10) |> 
  as.factor()

# Define variables to be used
predictors <- c("Year_release_n", "Genre_F", "Rating_F", "Top_performer_dev", "Dev_games_made")
criterium <- "Global_Sales"

model_formula <- paste0(criterium, " ~ ",
                        paste(predictors, collapse = " + ")) |> 
  formula()

model_recipe <- recipe(model_formula, data = tt_training) |> 
# Imputing missing values
  # TODO missing za top perf treba biti F, za Rating i Genre other?
  step_impute_median(Year_release_n) |> 
  step_impute_mode(all_nominal())

# Postprocessing - can't be negative

linear_model <- linear_reg()

model_workflow <- workflow() |>
  add_model(linear_model) |> 
  add_recipe(model_recipe)

fitted_model <- model_workflow |> fit(tt_training)

# Metrics



training_results <- tt_testing |> 
  #select(Global_Sales) |> 
  bind_cols(
    predict(fitted_model, new_data = tt_testing[, predictors])
  )

# show that all the preprocessing steps are applied to the testing data too
# (at predict() stage)
#prep(model_recipe) |>  bake(new_data = tt_testing)

training_results |> metrics(truth = Global_Sales, estimate = .pred)
