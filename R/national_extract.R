#' @title Easy helper for 'national_extract'
#'
#' @name national_extract
#'
#' @description
#' Extract national level table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param table The fact table name to extract data from (defaults to HRT_FACT_DIM)
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' national_extract(con, time_frame = "FY")

national_extract <- function(con,
                             table = "HRT_FACT_DIM",
                             time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- dplyr::tbl(con,
                       from = table) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      PATIENT_ID,
                      PATIENT_IDENTIFIED,
                      PATIENT_COUNT) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
        .groups = "drop"
      )

    fact_national <- fact %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect() %>%
      filter(FINANCIAL_YEAR <= ltst_year)
  }
  else {
    fact <- dplyr::tbl(con,
                       from = table) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      PATIENT_ID,
                      PATIENT_IDENTIFIED,
                      PATIENT_COUNT) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
        .groups = "drop"
      )

    fact_national <- fact %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     YEAR_MONTH,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect()
  }

  return(fact_national)
}
