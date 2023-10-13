#' @title Easy helper for 'ssp_extract'
#'
#' @name ssp_extract
#'
#' @description
#' Extract presentation level table from HRT fact table for meds which have been claimed under SPP
#'
#' @param con The database connection object to be used
#' @param schema The scheme name to extract data from
#' @param table The fact table name to extract data from
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' ssp_extract(con,
#' schema = "GRALI",
#' table = "HRT_FACT_202310", time_frame = "FY")

ssp_extract <- function(con,
                        schema,
                        table,
                                 time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
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
        SECTION_DESCR,
        BNF_SECTION,
        PARAGRAPH_DESCR,
        BNF_PARAGRAPH,
        CHEMICAL_SUBSTANCE_DESCR,
        BNF_CHEMICAL_SUBSTANCE,
        PRESENTATION_BNF,
        PRESENTATION_BNF_DESCR,
        GENERIC_BNF_CODE,
        GEN_PRESENTATION_BNF_DESCR,
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
                     BNF_SECTION,
                     BNF_PARAGRAPH,
                     BNF_CHEMICAL_SUBSTANCE,
                     PRESENTATION_BNF)
  } else {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
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
        SECTION_DESCR,
        BNF_SECTION,
        PARAGRAPH_DESCR,
        BNF_PARAGRAPH,
        CHEMICAL_SUBSTANCE_DESCR,
        BNF_CHEMICAL_SUBSTANCE,
        PRESENTATION_BNF,
        PRESENTATION_BNF_DESCR,
        GENERIC_BNF_CODE,
        GEN_PRESENTATION_BNF_DESCR,
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
        BNF_SECTION,
        BNF_PARAGRAPH,
        BNF_CHEMICAL_SUBSTANCE,
        PRESENTATION_BNF
      )
  }

  return(fact)

}
