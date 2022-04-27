#' colorize function
#'
#' This function allows you to color text in a Rmarkdown document. It works with
#' both PDF and HTML output. If used outside of a Rmarkdown document, it just
#' outputs the same string.
#' @param x Text string to colorize.
#' @param color Color string, defaults to "red".
#' @keywords text color
#' @export
#' @examples
#' colorize("text_red", color = "red")
#' colorize("text_white_hex", color = "#FFFFFF")

colorize <- function(x, color = "red") {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
            x)
  } else x
}
