#' Easy helper for 'create_metadata'
#'
#' Create metadata tab for named workbook
#'
#' @param workbook the name of the workbook object created using the create_wb function
#' @param fields a list of field names for each item of metadata
#' @param description a list of descriptions for each item of metadata
#'
#' @export
#'
#' @examples
#' meta_fields <- c("BNF Chapter Code",
#'                  "BNF Chapter Name",
#'                  "BNF Section Code",
#'                  "BNF Section Name",
#'                  "Cost"
#' )
#' meta_descs <- c("The unique code used to refer to the British National Formulary (BNF) chapter.",
#'                 "The name given to a British National Formulary (BNF) chapter. This is the broadest grouping of the BNF therapeutical classification system.",
#'                 "The unique code used to refer to the British National Formulary (BNF) section.",
#'                 "The name given to a British National Formulary (BNF) section. This is the next broadest grouping of the BNF therapeutical classification system after chapter.",
#'                 "There are many costs incurred when a dispensing contractor fulfils a prescription. In primary care the costs reported in this publication represent the basic price of the item. This is sometimes called the 'Net Ingredient Cost' (NIC). This also known as reimbursement of costs to dispensing contractors. In secondary care they are the actual costs paid (including applicable VAT) for drugs, dressing, appliances, and medical devices which have been issued and used in NHS hospitals in England."
#' )
#' create_metadata(test,
#'                 meta_fields,
#'                 meta_descs
#' )

create_metadata <- function(
  workbook,
  fields,
  descriptions
) {
  
  wb <- workbook
  
  meta_fields <- fields
  
  meta_descs <- descriptions
  
  #build meta data as table
  meta_data <- data.frame(
    Field = meta_fields,
    Description = meta_descs
  )
  
  #write title
  openxlsx::writeData(
    wb,
    sheet = "Metadata",
    x = "Metadata - a list of the fields in these tables and their descriptions",
    xy = c(1,1)
  )
  
  #bold title
  openxlsx::addStyle(
    wb,
    sheet = "Metadata",
    style = openxlsx::createStyle(textDecoration = "bold"),
    cols = 1,
    rows = 1
  )
  
  #set width of columns
  openxlsx::setColWidths(
    wb,
    sheet = "Metadata",
    1,
    widths = 34
  )
  
  openxlsx::setColWidths(
    wb,
    sheet = "Metadata",
    2,
    widths = 155
  )
  
  #set row heights of full range to 14.5 to improve accessibility
  openxlsx::setRowHeights(wb,
                          sheet = "Metadata",
                          rows = c(1:nrow(meta_data)+3),
                          heights = 14.5)
  
  #create wider row to use blank space in place of blank rows
  openxlsx::setRowHeights(wb,
                          sheet = "Metadata",
                          rows = 2,
                          heights = 29)
  
  #write meta data
  openxlsx::writeData(wb,
                      sheet = "Metadata",
                      meta_data,
                      startCol = 1,
                      startRow = 2)
  
  #bold headers
  openxlsx::addStyle(
    wb,
    sheet = "Metadata",
    style = openxlsx::createStyle(textDecoration = "bold"),
    cols = c(1:2),
    rows = 2,
    gridExpand = TRUE
  )
  
  #wrap description text
  openxlsx::addStyle(
    wb,
    sheet = "Metadata",
    style = openxlsx::createStyle(wrapText =  TRUE),
    cols = c(1:2),
    rows = c(3:nrow(meta_data)+3),
    gridExpand = TRUE
  )
}