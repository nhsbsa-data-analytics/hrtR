#' @title Easy helper for 'pi_extract'
#'
#' @name pi_extract
#'
#' @description
#' Extract patient identification rate wide table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param schema The scheme name to extract data from
#' @param table The fact table name to extract data from
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' pi_extract(con = con,
#' schema = "GRALI",
#' table = "HRT_FACT_202310", time_frame = "FY")

pi_extract <- function(con,
                       schema,
                       table,
                       time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- tbl(src = con,
                 dbplyr::in_schema(schema, table)) %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        `BNF paragraph name` = PARAGRAPH_NAME,
        `BNF paragraph code` = PARAGRAPH_CODE,
        PATIENT_IDENTIFIED
      ) %>%
      dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                       .groups = "drop") %>%
      dplyr::arrange(FINANCIAL_YEAR) %>%
      collect() %>%
      tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                         values_from = ITEM_COUNT) %>%
      mutate(
        RATE = Y / (Y + N) * 100,
        `BNF paragraph code` = factor(`BNF paragraph code`,
                                      levels = c("060401", "070201"))
      ) %>%
      dplyr::select(-Y, -N) %>%
      tidyr::pivot_wider(names_from = FINANCIAL_YEAR,
                         values_from = RATE) %>%
      dplyr::arrange(`BNF paragraph code`)
  }
  else{
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
      dplyr::group_by(
        YEAR_MONTH,
        `BNF paragraph name` = PARAGRAPH_NAME,
        `BNF paragraph code` = PARAGRAPH_CODE,
        PATIENT_IDENTIFIED
      ) %>%
      dplyr::summarise(ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
                       .groups = "drop") %>%
      dplyr::arrange(YEAR_MONTH) %>%
      collect() %>%
      tidyr::pivot_wider(names_from = PATIENT_IDENTIFIED,
                         values_from = ITEM_COUNT) %>%
      mutate(
        RATE = Y / (Y + N) * 100,
        `BNF paragraph code` = factor(`BNF paragraph code`,
                                      levels = c("060401", "070201"))
      ) %>%
      dplyr::select(-Y, -N) %>%
      tidyr::pivot_wider(names_from = YEAR_MONTH,
                         values_from = RATE) %>%
      dplyr::arrange(`BNF paragraph code`)
  }

  return(fact)
}
