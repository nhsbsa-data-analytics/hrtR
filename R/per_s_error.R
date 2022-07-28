#' @title Easy helper for 'per_s_error'
#'
#' @name per_s_error
#'
#' @description
#' Test for significant change between 2 proportions when bases are known
#'
#' @param base1 the base size of the first sample
#' @param per1 the proportion of the first sample to test
#' @param base2 the base size of the second sample
#' @param per2 the proportion of the second sample to test
#'
#' @export
#'
#' @example
#' per_s_error(100,50,100,10)

per_s_error <- function(base1, per1, base2, per2) {
  pooled_per <-
    ((base1 * per1 / 100) + (base2 * per2 / 100)) / (base1 + base2)
  s_error <-
    sqrt((pooled_per * (1 - pooled_per)) * ((1 / base1) + (1 / base2)))
  sig_value <- abs((per1 / 100 - per2 / 100) / s_error)
  change <- "no change"
  if (sig_value > 1.96) {
    change <- "change"
  }
  change
}
