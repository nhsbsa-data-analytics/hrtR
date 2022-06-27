#' @title Easy helper for 'not in'
#'
#' @name not_in
#'
#' @description
#' Filter vectors that are not in a given vector using syntax similar to the base
#' R `%in%` operator.
#'
#' @param x vector
#' @param y vector
#'
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' x[x %!in% c(1, 3, 5)]

`%!in%` <- function(x, y) {

  !('%in%'(x,y))

}
