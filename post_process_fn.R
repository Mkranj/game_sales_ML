#' Adjust negative predictions to 0
#'
#' @param dataset Dataset with ".pred" column to adjust
#'
#' @return Dataset with non-zero predictions
adjust_negative_preds <- function(dataset) {
  predictions <- dataset[".pred"]
  predictions[predictions < 0] <- 0
  dataset[".pred"] <- predictions
  
  dataset
}