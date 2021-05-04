#' lm_eq function
#'
#' This function allows you to output the equation as well as the adjusted
#' R-squared, for a linear model in a ggplot.
#' @keywords lm linear model equation
#' @export
#' @examples
#' \dontrun{
#' cars %>%
#' ggplot(aes(x = x, y = y)) +
#' geom_smooth(method = "lm") +
#' lm_eq()
#' }

lm_eq <- function() {
  ggpmisc::stat_poly_eq(
    ggplot2::aes(
      label =  paste(stat(eq.label), stat(adj.rr.label), sep = "*\", \"*")
    ),
    formula = y ~ x,
    parse = TRUE
  )
}
