#' Easy helper for 'create_wb'
#'
#' Create base workbook 'wb' and add sheets required
#'
#' @param sheets a list of sheet names require
#'
#' @import openxlsx
#'
#' @export
#'
#' @examples
#' sheetNames <- c("test1","test2","test3")
#' create_wb(sheetNames)

create_wb <- function(
  sheets
) {
  ##create workbook with relevant named sheets
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb,
                         sheetName = "Cover_sheet",
                         gridLines = FALSE)

  openxlsx::addWorksheet(wb,
                         sheetName = "Metadata",
                         gridLines = FALSE)


  for (i in sheets) {
    openxlsx::addWorksheet(wb,
                           sheetName = i,
                           gridLines = FALSE)
  }

  #set font to Arial
  openxlsx::modifyBaseFont(wb, fontName = "Arial", fontSize = 10)

  #return as object to use in global environment
  return(wb)
}
