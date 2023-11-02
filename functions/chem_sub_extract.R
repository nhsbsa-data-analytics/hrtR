#' @title Easy helper for 'chem_sub_extract'
#'
#' @name chem_sub_extract
#'
#' @description
#' Extract chemical substance level table from HRT fact table
#'
#' @param con The database connection object to be used
#' @param schema The scheme name to extract data from
#' @param table The fact table name to extract data from
#' @param time_frame "FY"/"Monthly" - the time frame you which to summarise to
#'
#' @export
#'
#' @example
#' ageband_extract(con = con,
#' schema = "GRALI",
#' table = "HRT_FACT_202310", time_frame = "FY")

chem_sub_extract <- function(con,
                             schema,
                             table,
                             time_frame = c("FY", "Monthly")) {
  time_frame <- match.arg(time_frame)

  if (time_frame == "FY") {
    fact <- tbl(src = con,
                dbplyr::in_schema(schema, table)) %>%
      dplyr::mutate(PATIENT_COUNT = case_when(PATIENT_IDENTIFIED == "Y" ~ 1,
                                              TRUE ~ 0)) %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        IDENTIFIED_PATIENT_ID,
        PATIENT_IDENTIFIED,
        SECTION_DESCR,
        BNF_SECTION,
        PARAGRAPH_DESCR,
        BNF_PARAGRAPH,
        CHEMICAL_SUBSTANCE_BNF_DESCR,
        BNF_CHEMICAL_SUBSTANCE,
        PATIENT_COUNT
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
        .groups = "drop"
      )

    fact_paragraph <- fact %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        SECTION_DESCR,
        BNF_SECTION,
        PARAGRAPH_DESCR,
        BNF_PARAGRAPH,
        CHEMICAL_SUBSTANCE_BNF_DESCR,
        BNF_CHEMICAL_SUBSTANCE,
        PATIENT_IDENTIFIED
      ) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(
        FINANCIAL_YEAR,
        BNF_SECTION,
        BNF_PARAGRAPH,
        BNF_CHEMICAL_SUBSTANCE,
        desc(PATIENT_IDENTIFIED)
      ) %>%
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
        SECTION_DESCR,
        BNF_SECTION,
        PARAGRAPH_DESCR,
        BNF_PARAGRAPH,
        CHEMICAL_SUBSTANCE_BNF_DESCR,
        BNF_CHEMICAL_SUBSTANCE,
        PATIENT_COUNT
      ) %>%
      dplyr::summarise(
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T),
        .groups = "drop"
      )

    fact_paragraph <- fact %>%
      dplyr::group_by(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        SECTION_DESCR,
        BNF_SECTION,
        PARAGRAPH_DESCR,
        BNF_PARAGRAPH,
        CHEMICAL_SUBSTANCE_BNF_DESCR,
        BNF_CHEMICAL_SUBSTANCE,
        PATIENT_IDENTIFIED
      ) %>%
      dplyr::summarise(
        PATIENT_COUNT = sum(PATIENT_COUNT, na.rm = T),
        ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
        ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T) / 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(
        FINANCIAL_YEAR,
        YEAR_MONTH,
        BNF_SECTION,
        BNF_PARAGRAPH,
        BNF_CHEMICAL_SUBSTANCE,
        desc(PATIENT_IDENTIFIED)
      ) %>%
      collect()
  }

  return(fact_paragraph)

}


chem_sub_extract(con = con,
  schema = "KIGRA",
  table = "HRT_FACT_202310", time_frame = "FY") |> view()
