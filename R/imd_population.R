#' @title Easy helper for 'imd_population'
#'
#' @name imd_population
#'
#' @description
#' Pull 2020 population data by IMD/age band/sex
#'
#' @export
#'
#' @example
#' imd_population()

imd_population <- function() {
  temp <- tempfile()
  imd_url <-
    utils::download.file(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/13773populationsbyindexofmultipledeprivationimddecileenglandandwales2020/populationbyimdenglandandwales2020.xlsx",
                         temp,
                         mode = "wb")
  #read xlsx population file
  df <- readxl::read_xlsx(temp,
                          sheet = 2,
                          range = "A4:CO25",
                          col_names = TRUE)
  #remove blank row, rename columns, recode imd decile into imd quintile
  imd_pop_2020 <- df %>%
    filter_all(any_vars(!is.na(.))) %>%
    dplyr::rename(GENDER = ...1,
                  IMD_DECILE = `Deprivation Decile (IMD 2020)`) %>%
    tidyr::fill(GENDER) %>%
    dplyr::mutate(IMD_QUINTILE = factor(
      case_when(
        IMD_DECILE %in% c(1, 2) ~ "1",
        IMD_DECILE %in% c(3, 4) ~ "2",
        IMD_DECILE %in% c(5, 6) ~ "3",
        IMD_DECILE %in% c(7, 8) ~ "4",
        IMD_DECILE %in% c(9, 10) ~ "5"
      )
    )) %>%
    tidyr::pivot_longer(cols = `0`:`90+`,
                        names_to = "AGE",
                        values_to = "POPULATION") %>%
    mutate(AGE = case_when(AGE == "90+" ~ as.numeric(90),
                           TRUE ~ as.numeric(AGE))) %>%
    dplyr::mutate(
      AGE_BAND = case_when(
        AGE == 90 ~ "90+",
        AGE >= 85 ~ "85-89",
        AGE >= 80 ~ "80-84",
        AGE >= 75 ~ "75-79",
        AGE >= 70 ~ "70-74",
        AGE >= 65 ~ "65-69",
        AGE >= 60 ~ "60-64",
        AGE >= 55 ~ "55-59",
        AGE >= 50 ~ "50-54",
        AGE >= 45 ~ "45-49",
        AGE >= 40 ~ "40-44",
        AGE >= 35 ~ "35-39",
        AGE >= 30 ~ "30-34",
        AGE >= 25 ~ "25-29",
        AGE >= 20 ~ "20-24",
        AGE >= 15 ~ "15-19",
        AGE >= 10 ~ "10-14",
        AGE >= 5 ~ "05-09",
        TRUE ~ "00-04"
      ),
      IMD_QUINTILE = as.numeric(IMD_QUINTILE)
    ) %>%
    dplyr::group_by(GENDER, IMD_QUINTILE, AGE_BAND) %>%
    dplyr::summarise(POPULATION = sum(POPULATION), .groups = "drop") %>%
    mutate(GENDER = case_when(GENDER == "Males" ~ "M",
                              TRUE ~ "F"))
  return(imd_pop_2020)
}
