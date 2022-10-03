#' nthroot function
#'
#' nthroot handles negatives, e.g. returning nthroot(-2,3) == -1.259921, rather than NA.
#' https://stackoverflow.com/a/66554084
#' @param x The base.
#' @param n The n'th root.
#' @keywords root nth nthroot
#' @export

nthroot <- function(x, n) {
  (abs(x)^(1 / n)) * sign(x)
}
