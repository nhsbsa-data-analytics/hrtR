#' Easy helper for format_data function
#'
#' @description Function for formatting numbers to 3 significant figures and adding the
#' appropriate wording if needed
#'
#' @param x The numeric to be formatted
#' @param percentage TRUE/FALSE - indicate if the number is a percentage
#' @param currency TRUE/FALSE - indicate if the number is a currency
#'
#' @export
#'
#' @examples
#' format_number(123456)
#' format_number(0.1235)
#' format_number(1.2345, percentage = TRUE)
#' format_number(1000, currency = TRUE)

format_number <- function(x, percentage = FALSE, currency = FALSE) {
  #create blank wording option
  wording <- ""

  x <- abs(x)

  #round to 3 sig figs
  #x <- as.numeric(signif(x, 3))
  if(x >= 1000000000 ) {
    output <- signif(x/1000000000,3)
    wording <- "billion"
  } else if(x >= 1000000) {
    output <- signif(x/1000000,3)
    wording <- "million"
  } else if(x >= 1) {
    #create comma separated value for number < 1 million
    output <- prettyNum(signif(x, 3), big.mark = ",")
  } else {
    output <- x
  }

  #round to 2 decimal places for number < 1
  if((nchar(output) > 4) & (grepl( ".", output, fixed = TRUE))) {
    output <- round(as.numeric(output), 2)
  }

  #add trailing zero to number where needed
  if((nchar(output) == 3) & (grepl( ".", output, fixed = TRUE))) {
    output <- as.character(paste0(output, "0"))
  }

  #add .00 to numbers where needed
  if((nchar(output) == 1)) {
    output <- as.character(paste0(output, ".00"))
  }

  #add .0 if rounds to whole number > 10 and < 100
  if((nchar(output) == 2)) {
    output <- as.character(paste0(output, ".0"))
  }

  #join to wording if needed and trim and trailing white space
  output <- trimws(as.character(paste0(output, " ", wording)), "right")

  #add % symbol if number is percentage
  if(percentage == TRUE) {
    output <- paste0(output, "%")
  }

  #add £ sign if number is currency
  if(currency == TRUE) {
    output <- paste0("£", output)
  }

  return (output)
}
