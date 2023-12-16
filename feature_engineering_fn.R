count_subtitles <- function(game_names){
  # Patterns that count as denoting subtitles:
  subtitle_marker <- c(": ", " - ")
  
  subtitle_count <- rep(0, length(game_names))
  
  for (marker in seq_along(subtitle_marker)) {
    subtitle_count <- subtitle_count +
      str_count(game_names, pattern = subtitle_marker[marker])
  }

  subtitle_present <- subtitle_count > 0
  
  return(list(
    subtitle_present = subtitle_present,
    subtitle_count = subtitle_count
  ))
}

#' Turn missing values to zeroes
#'
#' @param dataset DF to process
#' @param column_name Character, name of column to process
#'
#' @return Dataset with adjusted column
missing_to_zero <- function(dataset, column_name) {
  data <- dataset
  values <-  data[[column_name]]
  values[is.na(values)] <- 0
  data[[column_name]] <- values
  data
}

mark_unusual_titles <- function(game_names) {
  # Unusual: every non-letter, non-digit. Except spaces and : and - used
  # for detecting subtitles
  unusual_characters <- "[^a-z\\d\\w:-]"
  
  # Treat titles as all lowercase for simplicity
  titles <- game_names %>% tolower()
  
  str_detect(titles, pattern = unusual_characters)
}