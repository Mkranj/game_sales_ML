#' Get table of summarised info for developers
#'
#' @param data Dataframe with following columns: Developer, Global_Sales,
#' Name (for counting games)
#'
#' @return dataframe
calculate_dev_summaries <- function(data) {
  dev_data <- group_by(data, Developer) |> 
    summarise(total_income = sum(Global_Sales, na.rm = T),
              avg_income = mean(Global_Sales, na.rm = T),
              no_games = n())
  
  # Cleanup row for no-name given
  dev_data <- dev_data[-c(dev_data$Developer == "") ,]
  dev_data
}

#' Mark game as made by succesful dev
#'
#' On account of top n_top ranked by avg income per game
#'
#' @param input_df DF with Developer column
#' @param summarised_df Summary DF returned by specialised fn
#' @param n_top How many top performers to pick out?
#'
#' @return logical vector of length == nrow input_df
mark_top_performer_dev <- function(input_df, summarised_df, summary_metric = "avg_income", n_top = 10) {
  metric_df <- arrange(summarised_df, desc(.data[[summary_metric]]))
  top_devs <- metric_df$Developer[1:n_top]
  
  is_top_performer <- input_df$Developer %in% top_devs
  is_top_performer
}