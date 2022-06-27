#' Easy helper for 'format_data'
#'
#' Format data on selected sheet of base 'wb' created using create_wb function
#'
#' @param workbook the name of the workbook object created using the create_wb function
#' @param sheetname the name of the sheet to apply style to
#' @param column the excel column letter(s) you wish to apply the style to eg. "F" or c("A", "B")
#' @param alignment Horizontal alignment of cell contents
#' \itemize{
#'   \item \strong{left} Left horizontal align cell contents
#'   \item \strong{right} Right horizontal align cell contents
#'   \item \strong{center} Center horizontal align cell contents
#'   \item \strong{justify} Justify horizontal align cell contents
#' }
#' @param number_format the Excel number format code you wish to apply
#' @param filepath the file path to save the workbook to
#'
#' @import openxlsx
#'
#' @export
#'
#' @examples
#' format_data(myworkbook,
#' "test1",
#' "A",
#' "right",
#' "#,###"
#' )
#' format_data(myworkbook,
#' "test1",
#' c("A", "B", "C"),
#' "right",
#' "#,###.00"
#' )
#' format_data(myworkbook,
#' "test1",
#' C("D", "AA", "X"),
#' "left",
#' ""
#' )
format_data <- function(workbook,
                        sheetname,
                        column,
                        alignment,
                        number_format) {

  
  #convert column(s) to numeric values
  column_number <- excel_column_to_numeric(column)
  
  #name workbook
  wb <- workbook
  
  #get full data
  data1 <- openxlsx::read.xlsx(wb, sheetname)
  
  #get full data minus title/notes
  data2 <- openxlsx::read.xlsx(wb, sheetname) %>% na.omit()
  
  #calculate starting row of data
  first_row <- as.numeric(nrow(data1) - nrow(data2) + 2)
  
  #calculate end row
  last_row <- as.numeric(nrow(data1)) + 1
  
  #create style for column header
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = alignment
  )
  
  #add style to header
  openxlsx::addStyle(
    wb,
    sheetname,
    header_style,
    first_row,
    column_number,
    gridExpand = TRUE
  )
  
  #create style for data
  style <- openxlsx::createStyle(
    halign = alignment,
    numFmt = number_format
  )
  
  #add style to data
  openxlsx::addStyle(
    wb,
    sheetname,
    style,
    first_row + 1:last_row,
    column_number,
    gridExpand = TRUE
  )
}