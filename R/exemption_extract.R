#' @title Easy helper for 'exemption_extract'
#'
#' @name exemption_extract
#'
#' @description
#' Extract exemption data table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param table The fact table name to extract data from (defaults to HRT_FACT_DIM)
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' exemption_extract(con, time_frame = "FY")

exemption_extract <- function(con,
                            table = "HRT_FACT_DIM",
                            time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- dplyr::tbl(src = con,
                       from = table) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      PATIENT_ID,
                      PATIENT_IDENTIFIED,
                      PFEA_CHARGE_STATUS,
                      CHARGE_STATUS,
                      PFEA_EXEMPT_CAT,
                      EXEMPT_CAT,
                      PATIENT_COUNT) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
      )

    fact_age <- fact %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      PFEA_CHARGE_STATUS,
                      CHARGE_STATUS,
                      PFEA_EXEMPT_CAT,
                      EXEMPT_CAT,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     PFEA_CHARGE_STATUS,
                     CHARGE_STATUS,
                     PFEA_EXEMPT_CAT,
                     EXEMPT_CAT,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect()
  } else {
    fact <- dplyr::tbl(src = con,
                       from = table) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      PATIENT_ID,
                      PATIENT_IDENTIFIED,
                      PFEA_CHARGE_STATUS,
                      CHARGE_STATUS,
                      PFEA_EXEMPT_CAT,
                      EXEMPT_CAT,
                      PATIENT_COUNT) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
      )

    fact_age <- fact %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      PFEA_CHARGE_STATUS,
                      CHARGE_STATUS,
                      PFEA_EXEMPT_CAT,
                      EXEMPT_CAT,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     YEAR_MONTH,
                     PFEA_CHARGE_STATUS,
                     CHARGE_STATUS,
                     PFEA_EXEMPT_CAT,
                     EXEMPT_CAT,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect()
  }

  return(fact_age)

}
