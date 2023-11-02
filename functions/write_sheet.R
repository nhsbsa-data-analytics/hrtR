#' Easy helper for 'write_sheet'
#'
#' Write data to selected sheet of base 'wb' created using create_wb function
#'
#' @param workbook the name of the workbook object created using the create_wb function
#' @param sheetname the name of the blank sheet to write to
#' @param title the title of the sheet which will go into cell A1
#' @param notes a list object of the notes to be included on the sheet
#' @param dataset the name of the dataset to be written to the names sheet
#' @param column_a_width the width you would like to assign to column A
#'
#' @import openxlsx
#'
#' @export
#'
#' @examples
#' write_sheet(myworkbook,
#' "test1",
#' "title1",
#' c("note1", "note2", "note3", "note4"),
#' mtcars,
#' 10)

write_sheet <-  function(workbook,
                         sheetname,
                         title,
                         notes,
                         dataset,
                         column_a_width) {
  
  wb <- workbook
  
  notes_list <- notes
  
  #write title
  openxlsx::writeData(
    wb,
    sheet = sheetname,
    x = title,
    xy = c(1,1)
  )
  
  #bold title
  openxlsx::addStyle(
    wb,
    sheet = sheetname,
    style = openxlsx::createStyle(textDecoration = "bold"),
    cols = 1,
    rows = 1
  )
  
  #write notes header
  openxlsx::writeData(
    wb,
    sheet = sheetname,
    x = "Notes",
    xy = c(1,2)
  )
  
  #bold notes
  openxlsx::addStyle(
    wb,
    sheet = sheetname,
    style = openxlsx::createStyle(textDecoration = "bold"),
    cols = 1,
    rows = 2
  )
  
  #loop to write all notes
  for(i in 1:length(notes_list)) {
    
    openxlsx::writeData(
      wb,
      sheet = sheetname,
      x = notes_list[i],
      xy = c(1,(i + 2))
    )
    
  }
  
  #write data as named data table
  openxlsx::writeDataTable(wb,
                           sheet = sheetname,
                           x = dataset,
                           startRow = (length(notes_list) + 3),
                           tableStyle = "none",
                           withFilter = FALSE,
                           tableName = sheetname)
  
  #set row heights of full range to 14.5 to imporve accessibility
  openxlsx::setRowHeights(wb,
                          sheet = sheetname,
                          rows = c(1:(nrow(dataset) + length(notes_list) + 3)),
                          heights = 14.5)
  
  #create wider rows to use blank space in place of blank rows
  openxlsx::setRowHeights(wb,
                          sheet = sheetname,
                          rows = 2,
                          heights = 29)
  
  openxlsx::setRowHeights(wb,
                          sheet = sheetname,
                          rows = (length(notes_list) + 3),
                          heights = 29)
  
  #auto column widths
  setColWidths(wb, 
               sheet = sheetname, 
               cols = 1:ncol(dataset), 
               widths = "auto")
  
  #manually set width of column A to account for title being in A1
  setColWidths(wb, 
               sheet = sheetname, 
               cols = 1, 
               widths = column_a_width)
}

