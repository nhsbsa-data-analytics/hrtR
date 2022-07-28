#' @title Easy helper for 'icb_extract'
#'
#' @name icb_extract
#'
#' @description
#' Extract icb table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param table The fact table name to extract data from (defaults to HRT_FACT_DIM)
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' icb_extract(con, time_frame = "FY")

icb_extract <- function(con,
                        table = "HRT_FACT_DIM",
                        time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- dplyr::tbl(con,
                       from = table) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        PATIENT_ID,
        PATIENT_IDENTIFIED,
        REGION_NAME,
        REGION_CODE,
        STP_NAME,
        STP_CODE,
        PATIENT_COUNT
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
      )

    fact_stp <- fact %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        REGION_NAME,
        REGION_CODE,
        STP_NAME,
        STP_CODE,
        PATIENT_IDENTIFIED
      ) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      collect() %>%
      mutate(
        REGION_CODE_ORDER = case_when(REGION_CODE == "-" ~ 2,
                                      TRUE ~ 1),
        STP_NAME_ORDER = case_when(STP_NAME == "UNKNOWN STP" ~ 2,
                                   TRUE ~ 1)
      ) %>%
      dplyr::arrange(
        FINANCIAL_YEAR,
        REGION_CODE_ORDER,
        REGION_CODE,
        STP_NAME_ORDER,
        STP_NAME,
        desc(PATIENT_IDENTIFIED)
      ) %>%
      select(-REGION_CODE_ORDER, -STP_NAME_ORDER) %>%
      filter(FINANCIAL_YEAR <= ltst_year)
  } else {
    fact <- dplyr::tbl(con,
                       from = table) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        PATIENT_ID,
        PATIENT_IDENTIFIED,
        REGION_NAME,
        REGION_CODE,
        STP_NAME,
        STP_CODE,
        PATIENT_COUNT
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
      )

    fact_stp <- fact %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        REGION_NAME,
        REGION_CODE,
        STP_NAME,
        STP_CODE,
        PATIENT_IDENTIFIED
      ) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      collect() %>%
      mutate(
        REGION_CODE_ORDER = case_when(REGION_CODE == "-" ~ 2,
                                      TRUE ~ 1),
        STP_NAME_ORDER = case_when(STP_NAME == "UNKNOWN STP" ~ 2,
                                   TRUE ~ 1)
      ) %>%
      dplyr::arrange(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        REGION_CODE_ORDER,
        REGION_CODE,
        STP_NAME_ORDER,
        STP_NAME,
        desc(PATIENT_IDENTIFIED)
      ) %>%
      select(-REGION_CODE_ORDER, -STP_NAME_ORDER)
  }

  return(fact_stp)

}
