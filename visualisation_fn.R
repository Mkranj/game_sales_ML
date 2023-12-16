#' Ggplot x-y relationship with polynomial curve
#'
#' For discrete variables, visualising a geom_point() is likely to be misleading
#' since a lot of data will be stuck together x-axis wise. So this function
#' summarises by levels of x-variable.
#'
#' @param data DF dataset
#' @param xvar character, name of x-variable
#' @param yvar character, name of y-variable
#' @param poly_degree numeric, degree of polynomial
#'
#' @return ggplot
ggplot_discrete_polynomials <- function(data, xvar, yvar, poly_degree = 2) {
  summary_df <- group_by(data, .data[[xvar]]) %>%
    summarise(average = mean(.data[[yvar]]),
              count = n()
    )
  
  ggplot(games, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(data = summary_df, aes(y = average, size = count)) + 
    geom_smooth(method='lm', formula = y ~ poly(x, poly_degree))
}

