#' @title Easy helper for 'create_fact'
#'
#' @name create_fact
#'
#' @description
#' Build 'HRT_FACT_DIM' in personal schema
#'
#' @param con The database connection object to be used
#' @param from The first month you wish to collect data from (defaults to 201504)
#' @param to The last month you wish to collect data from (defaults to maxium month in DWH)
#'
#' @import dplyr
#' @import dbplyr
#' @import nhsbsaR
#' @import DBI
#'
#' @export
#'
#' @example
#' create_fact(con)


create_fact <- function(
  con,
  from = 201504L,
  to = dplyr::sql(
  "MGMT.PKG_PUBLIC_DWH_FUNCTIONS.f_get_latest_period('EPACT2')"
  )) {
# Build tdim ---------------------------------------------------------
tdim <- dplyr::tbl(con,
                   from = dbplyr::in_schema("DIM", "YEAR_MONTH_DIM")) %>%
  select(FINANCIAL_YEAR, YEAR_MONTH) %>%
  filter(
    YEAR_MONTH >= from,
    YEAR_MONTH <= to
  )

# Build porg ---------------------------------------------------------
porg <- dplyr::tbl(con,
                   from = dbplyr::in_schema("DIM", "CUR_EP_LEVEL_5_FLAT_DIM")) %>%
  filter(CUR_CTRY_OU == 1) %>%
  select(LVL_5_OUPDT, LVL_5_OU)

# Build drug ---------------------------------------------------------
drug <- dplyr::tbl(con,
                   from = dbplyr::in_schema("DIM", "CDR_EP_DRUG_BNF_DIM")) %>%
  filter(BNF_PARAGRAPH %in% c("060401", "070201")) %>%
  select(
    YEAR_MONTH,
    RECORD_ID,
    BNF_NAME = PRESENTATION_BNF_DESCR,
    BNF_CODE = PRESENTATION_BNF,
    CHEM_SUB_NAME = CHEMICAL_SUBSTANCE_BNF_DESCR,
    CHEM_SUB_CODE = BNF_CHEMICAL_SUBSTANCE,
    PARAGRAPH_NAME = PARAGRAPH_DESCR,
    PARAGRAPH_CODE = BNF_PARAGRAPH,
    SECTION_NAME = SECTION_DESCR,
    SECTION_CODE = BNF_SECTION,
    UNIT_OF_MEASURE = VMPP_UOM
  )

# Build age ---------------------------------------------------------
age <- dplyr::tbl(con,
                  from = dbplyr::in_schema("DIM", "AGE_DIM")) %>%
  select(AGE,
         DALL_5YR_BAND)

# Build imd ---------------------------------------------------------
imd <- dplyr::tbl(con,
                  from = dbplyr::in_schema("GRALI", "ONS_NSPL_MAY_22")) %>%
  select(LSOA11,
         IMD_DECILE)

# Build fact ---------------------------------------------------------
fact <- dplyr::tbl(con,
                   from = dbplyr::in_schema("AML", "PX_FORM_ITEM_ELEM_COMB_FACT_AV")) %>%
  inner_join(tdim,
             by = c("YEAR_MONTH" = "YEAR_MONTH")) %>%
  inner_join(porg,
             by = c("PRESC_TYPE_PRNT" = "LVL_5_OUPDT",
                    "PRESC_ID_PRNT" = "LVL_5_OU")) %>%
  inner_join(drug,
             by = c(
               "CALC_PREC_DRUG_RECORD_ID" = "RECORD_ID",
               "YEAR_MONTH" = "YEAR_MONTH"
             )) %>%
  inner_join(age,
             by = c("CALC_AGE" = "AGE")) %>%
  #pull forward last observation of PATIENT_LSOA_CODE to account for null data
  mutate(
    PATIENT_LSOA_CODE = sql(
      "last_value(PATIENT_LSOA_CODE ignore nulls) over (partition by PATIENT_ID order by YEAR_MONTH rows between unbounded preceding and current row)"
    )
  ) %>%
  left_join(imd,
            by = c("PATIENT_LSOA_CODE" = "LSOA11")) %>%
  #regular exclusions
  filter(
    PAY_DA_END == "N",
    # excludes disallowed items
    PAY_ND_END == "N",
    # excludes not dispensed items
    PAY_RB_END == "N",
    # excludes referred back items
    CD_REQ == "N",
    # excludes controlled drug requisitions
    OOHC_IND == 0L,
    # excludes out of hours dispensing
    PRIVATE_IND == 0L,
    # excludes private dispensers
    IGNORE_FLAG == "N",
    # excludes LDP dummy forms
    PRESC_TYPE_PRNT %NOT IN% c(8L, 54L)
  ) %>%
  select(
    FINANCIAL_YEAR,
    YEAR_MONTH,
    SECTION_NAME,
    SECTION_CODE,
    PARAGRAPH_NAME,
    PARAGRAPH_CODE,
    CHEM_SUB_NAME,
    CHEM_SUB_CODE,
    UNIT_OF_MEASURE,
    PATIENT_ID,
    PATIENT_IDENTIFIED,
    IMD_DECILE,
    PDS_GENDER,
    DALL_5YR_BAND,
    ITEM_CALC_PAY_QTY,
    ITEM_COUNT,
    ITEM_PAY_DR_NIC
  ) %>%
  mutate(
    PDS_GENDER = case_when(PDS_GENDER == 1 ~ "M",
                           PDS_GENDER == 2 ~ "F",
                           TRUE ~ "U"),
    DALL_5YR_BAND = case_when(is.na(DALL_5YR_BAND) ~ "Unknown",
                              TRUE ~ DALL_5YR_BAND)
  ) %>%
  group_by(
    FINANCIAL_YEAR,
    YEAR_MONTH,
    SECTION_NAME,
    SECTION_CODE,
    PARAGRAPH_NAME,
    PARAGRAPH_CODE,
    CHEM_SUB_NAME,
    CHEM_SUB_CODE,
    UNIT_OF_MEASURE,
    PATIENT_ID,
    PATIENT_IDENTIFIED,
    IMD_DECILE,
    PDS_GENDER,
    DALL_5YR_BAND
  ) %>%
  summarise(
    TOTAL_QTY = sum(ITEM_CALC_PAY_QTY, na.rm = T),
    ITEM_COUNT = sum(ITEM_COUNT, na.rm = T),
    ITEM_PAY_DR_NIC = sum(ITEM_PAY_DR_NIC, na.rm = T)
  ) %>%
  ungroup()

# drop time dimension if exists
exists <- con %>%
  DBI::dbExistsTable(name = "HRT_FACT_DIM")
# Drop any existing table beforehand
if (exists) {
  con %>%
    DBI::dbRemoveTable(name = "HRT_FACT_DIM")
}

#build table
fact %>%
  compute("HRT_FACT_DIM", analyze = FALSE, temporary = FALSE)

}

