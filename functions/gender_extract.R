#' @title Easy helper for 'gender_extract'
#'
#' @name gender_extract
#'
#' @description
#' Extract sextable from HRT fact table
#'
#' @param con The database connection object to be used
#' @param schema The scheme name to extract data from
#' @param table The fact table name to extract data from
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' gender_extract(con,
#' schema = "GRALI",
#' table = "HRT_FACT_202310", time_frame = "FY")


gender_extract <- function(con,
                           schema,
                           table,
                           time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      IDENTIFIED_PATIENT_ID,
                      PATIENT_IDENTIFIED,
                      PDS_GENDER,
                      PATIENT_COUNT) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
      )

    fact_gender <- fact %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      PDS_GENDER,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     PDS_GENDER,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect()
  } else {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        IDENTIFIED_PATIENT_ID,
        PATIENT_IDENTIFIED,
        PDS_GENDER,
        PATIENT_COUNT
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
      )

    fact_gender <- fact %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      PDS_GENDER,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     YEAR_MONTH,
                     PDS_GENDER,
                     desc(PATIENT_IDENTIFIED)) %>%
      collect()
  }

  return(fact_gender)

}
