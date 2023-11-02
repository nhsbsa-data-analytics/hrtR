#' @title Easy helper for 'hrt_options'
#'
#' @name hrt_options
#'
#' @description
#' Set the options used in the MUMH pipeline
#'
#' @export
#'
#' @examples
#' hrt_options()

hrt_options <- function() {
  # prevent scientific notation
  options(scipen = 999)

  # prevent printing of groups from summarise
  options(dplyr.summarise.inform = FALSE)

  # thousand separator for highcharts
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  hcoptslang$numericSymbols <- c("k","M","B","T","P","E")
  options(highcharter.lang = hcoptslang)
}
