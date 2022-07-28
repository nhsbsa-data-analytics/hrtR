#' @title Easy helper for 'quintile_extract'
#'
#' @name quintile_extract
#'
#' @description
#' Extract imd quintile table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param table The fact table name to extract data from (defaults to HRT_FACT_DIM)
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' quintile_extract(con, time_frame = "FY")

quintile_extract <- function(con,
                             table = "HRT_FACT_DIM",
                             time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- dplyr::tbl(con,
                       from = table) %>%
      dplyr::mutate(
        PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                  TRUE ~ 0),
        IMD_QUINTILE = case_when(
          IMD_DECILE <= 2 ~ 1,
          IMD_DECILE <= 4 ~ 2,
          IMD_DECILE <= 6 ~ 3,
          IMD_DECILE <= 8 ~ 4,
          IMD_DECILE <= 10 ~ 5,
          TRUE ~ IMD_DECILE
        )
      ) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      PATIENT_ID,
                      PATIENT_IDENTIFIED,
                      PATIENT_COUNT,
                      IMD_QUINTILE) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
        .groups = "drop"
      )

    table <- fact %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      PATIENT_IDENTIFIED,
                      IMD_QUINTILE) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     IMD_QUINTILE,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect() %>%
      filter(FINANCIAL_YEAR <= ltst_year)
  } else {
    fact <- dplyr::tbl(con,
                       from = table) %>%
      dplyr::mutate(
        PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                  TRUE ~ 0),
        IMD_QUINTILE = case_when(
          IMD_DECILE <= 2 ~ 1,
          IMD_DECILE <= 4 ~ 2,
          IMD_DECILE <= 6 ~ 3,
          IMD_DECILE <= 8 ~ 4,
          IMD_DECILE <= 10 ~ 5,
          TRUE ~ IMD_DECILE
        )
      ) %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        PATIENT_ID,
        PATIENT_IDENTIFIED,
        PATIENT_COUNT,
        IMD_QUINTILE
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
        .groups = "drop"
      )

    table <- fact %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      PATIENT_IDENTIFIED,
                      IMD_QUINTILE) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     YEAR_MONTH,
                     IMD_QUINTILE,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect()
  }
  return(table)
}
