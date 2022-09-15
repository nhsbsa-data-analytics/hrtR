#' @title Easy helper for 'ssp_extract'
#'
#' @name ssp_extract
#'
#' @description
#' Extract presentation level table from HRT fact table for meds which have been claimed under SPP
#'
#' @param con The database connection object to be used
#' @param table The fact table name to extract data from (defaults to HRT_FACT_DIM)
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' ssp_extract(con, time_frame = "FY")

ssp_extract <- function(con,
                                 table = "HRT_FACT_DIM",
                                 time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)
  
  if (time_frame == "FY") {
    fact <- dplyr::tbl(con,
                       from = "HRT_FACT_DIM") %>%
      mutate(
        ITEM_SSP_FEES = case_when(
          is.na(ITEM_SSP_FEES) ~ 0,
          TRUE ~ ITEM_SSP_FEES
        ),
        ITEM_SSP_VAT_VALUE = case_when(
          is.na(ITEM_SSP_VAT_VALUE) ~ 0,
          TRUE ~ ITEM_SSP_VAT_VALUE
        )
      ) %>%
      filter(
        ITEM_SSP_VAT_VALUE > 0 | ITEM_SSP_FEES > 0
      ) %>%
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
    fact <- dplyr::tbl(con,
                       from = "HRT_FACT_DIM") %>%
      mutate(
        ITEM_SSP_FEES = case_when(
          is.na(ITEM_SSP_FEES) ~ 0,
          TRUE ~ ITEM_SSP_FEES
        ),
        ITEM_SSP_VAT_VALUE = case_when(
          is.na(ITEM_SSP_VAT_VALUE) ~ 0,
          TRUE ~ ITEM_SSP_VAT_VALUE
        )
      ) %>%
      filter(
        ITEM_SSP_VAT_VALUE > 0 | ITEM_SSP_FEES > 0
      ) %>%
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
