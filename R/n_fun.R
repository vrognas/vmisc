#' n_fun function
#'
#' Output the number of data points in a box plot
#'
#' @param x smth
#'
#' @keywords box plot number
#' @export
#'
#' @examples
#' \dontrun{
#' diamonds %>%
#' ggplot(aes(x = cut, y = depth)) +
#'  geom_boxplot() +
#'  stat_summary(fun.data = n_fun, geom = "text", size = 5, color = "blue")
#' )
#' }
n_fun <- function(x){
  return(data.frame(y = median(x), label = paste0("n = ", length(x))))
}
