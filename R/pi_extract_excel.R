#' @title Easy helper for 'pi_extract_excel'
#'
#' @name pi_extract_excel
#'
#' @description
#' Extract patient identification rate long table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param table The fact table name to extract data from (defaults to HRT_FACT_DIM)
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' pi_extract_excel(con, time_frame = "FY")

pi_extract_excel <- function(con,
                             table = "HRT_FACT_DIM",
                             time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- dplyr::tbl(con,
                       from = table) %>%
      filter(FINANCIAL_YEAR <= ltst_year) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                       .groups = "drop") %>%
      dplyr::arrange(FINANCIAL_YEAR) %>%
      collect() %>%
      tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                         values_from = ITEM_COUNT) %>%
      mutate(RATE = Y / (Y + N) * 100) %>%
      dplyr::select(-Y, -N)
  } else {
    fact <- dplyr::tbl(con,
                       from = table) %>%
      dplyr::group_by(FINANCIAL_YEAR,
                      YEAR_MONTH,
                      PATIENT_IDENTIFIED) %>%
      dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                       .groups = "drop") %>%
      dplyr::arrange(FINANCIAL_YEAR,
                     YEAR_MONTH) %>%
      collect() %>%
      tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                         values_from = ITEM_COUNT) %>%
      mutate(RATE = Y / (Y + N) * 100) %>%
      dplyr::select(-Y, -N)
  }

  return(fact)
}
