#' @title Easy helper for 'quintile_age_extract'
#'
#' @name quintile_age_extract
#'
#' @description
#' Extract imd quintile and age band table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param schema The scheme name to extract data from
#' @param table The fact table name to extract data from
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' quintile_age_extract(con,
#' schema = "GRALI",
#' table = "HRT_FACT_202310", time_frame = "FY")


quintile_age_extract <- function(con,
                                 schema,
                                 table,
                                 time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
      filter(PATIENT_IDENTIFIED == "Y") %>%
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
        IDENTIFIED_PATIENT_ID,
        #PATIENT_IDENTIFIED,
        PATIENT_COUNT,
        CALC_AGE,
        IMD_QUINTILE
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
        .groups = "drop"
      )

    table <- fact %>%
      dplyr::inner_join(
        dplyr::tbl(con,
                   from = dbplyr::in_schema("DIM", "AGE_DIM")),
        by = c(
          "CALC_AGE" = "AGE"
        )
      ) %>%
      dplyr::mutate(AGE_BAND = dplyr::case_when(is.na(DALL_5YR_BAND) ~ "Unknown",
                                                TRUE ~ DALL_5YR_BAND)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      #PATIENT_IDENTIFIED,
                      AGE_BAND,
                      IMD_QUINTILE) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     AGE_BAND,
                     IMD_QUINTILE) %>%
      collect()
  } else {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
      filter(PATIENT_IDENTIFIED == "Y") %>%
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
        IDENTIFIED_PATIENT_ID,
        #PATIENT_IDENTIFIED,
        PATIENT_COUNT,
        CALC_AGE,
        IMD_QUINTILE
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
        .groups = "drop"
      )

    table <- fact %>%
      dplyr::inner_join(
        dplyr::tbl(con,
                   from = dbplyr::in_schema("DIM", "AGE_DIM")),
        by = c(
          "CALC_AGE" = "AGE"
        )
      ) %>%
      dplyr::mutate(AGE_BAND = dplyr::case_when(is.na(DALL_5YR_BAND) ~ "Unknown",
                                                TRUE ~ DALL_5YR_BAND)) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      #PATIENT_IDENTIFIED,
                      AGE_BAND,
                      IMD_QUINTILE) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     YEAR_MONTH,
                     AGE_BAND,
                     IMD_QUINTILE) %>%
      collect()
  }

  return(table)
}
