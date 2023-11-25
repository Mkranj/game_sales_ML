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