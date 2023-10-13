#' @title Easy helper for 'presentation_extract'
#'
#' @name presentation_extract
#'
#' @description
#' Extract presentation level table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param schema The scheme name to extract data from
#' @param table The fact table name to extract data from
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' presentation_extract(ccon,
#' schema = "GRALI",
#' table = "HRT_FACT_202310", time_frame = "FY")


presentation_extract <- function(con,
                                 schema,
                                 table,
                                 time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        SECTION_NAME,
        SECTION_CODE,
        PARAGRAPH_NAME,
        PARAGRAPH_CODE,
        CHEM_SUB_NAME,
        CHEM_SUB_CODE,
        BNF_CODE,
        BNF_NAME,
        GENERIC_BNF_CODE,
        GENENRIC_BNF_NAME,
        UNIT_OF_MEASURE
      ) %>%
      dplyr::summarise(
        TOTAL_QTY = sum(TOTAL_QTY, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      collect() %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     SECTION_CODE,
                     PARAGRAPH_CODE,
                     CHEM_SUB_CODE,
                     BNF_CODE)
  } else {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        SECTION_NAME,
        SECTION_CODE,
        PARAGRAPH_NAME,
        PARAGRAPH_CODE,
        CHEM_SUB_NAME,
        CHEM_SUB_CODE,
        BNF_CODE,
        BNF_NAME,
        GENERIC_BNF_CODE,
        GENENRIC_BNF_NAME,
        UNIT_OF_MEASURE
      ) %>%
      dplyr::summarise(
        TOTAL_QTY = sum(TOTAL_QTY, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      collect() %>%
      dplyr::arrange(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        SECTION_CODE,
        PARAGRAPH_CODE,
        CHEM_SUB_CODE,
        BNF_CODE
      )
  }

  return(fact)

}
