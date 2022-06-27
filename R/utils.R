#' convert excel column letter to number
#' @param column_letter the letter code of the Excel column you wish to use
#' @noRd
excel_column_to_numeric <- function(column_letter){
  # Uppercase
  s_upper <- toupper(column_letter)
  # Convert string to a vector of single letters
  s_split <- unlist(strsplit(s_upper, split=""))
  # Convert each letter to the corresponding number
  s_number <- sapply(s_split, function(x) {which(LETTERS == x)})
  # Derive the numeric value associated with each letter
  numbers <- 26^((length(s_number)-1):0)
  # Calculate the column number
  column_number <- sum(s_number * numbers)
  column_number
}
#vectorise to allow multiple columns
excel_column_to_numeric <- Vectorize(excel_column_to_numeric)