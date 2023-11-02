#' @title Easy helper for 'pi_extract_excel'
#'
#' @name pi_extract_excel
#'
#' @description
#' Extract patient identification rate long table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param schema The scheme name to extract data from
#' @param table The fact table name to extract data from
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' pi_extract_excel(con = con,
#' schema = "GRALI",
#' table = "HRT_FACT_202310", time_frame = "FY")

pi_extract_excel <- function(con,
                             schema,
                             table,
                             time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
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
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table))  %>%
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
