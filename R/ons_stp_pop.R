#' STP population reference file import function
#'
#' This function downloads mid-year 2020 STP population estimates from ONS, joins 
#' the data with the ONS lookup and returns population totals for each STP.
#'
#' @import data.table
#' @import dplyr
#' @import readxl
#' @import utils
#' 
#' @export
#'
#' @examples
#' ons_stp_pop()
#'
#' @source \url{https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/clinicalcommissioninggroupmidyearpopulationestimates}

ons_stp_pop <- function() {
  
  #create temp file to download xlsx file into
  
  temp <- tempfile()
  
  stp_url <- utils::download.file(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fclinicalcommissioninggroupmidyearpopulationestimates%2fmid2020sape23dt6a/sape23dt6amid2020ccg2021estimatesunformatted.xlsx",
                                  temp,
                                  mode = "wb")
  
  #read xlsx population file
  stp_pop <- readxl::read_xlsx(temp,
                       sheet = 4,
                       range = "A9:G114",
                       col_names = c("CCG_CD","CCG_NM","STP21_CD","STP21_NM",
                                     "REGION_CD","REGION_NM","POP"), 
                       skip = 8)
  
  ods_lookup = "https://opendata.arcgis.com/api/v3/datasets/a458c272484743aa9caa25619ccbe1ac_0/downloads/data?format=csv&spatialRefId=4326"
  
  ods <- data.table::fread(ods_lookup)
  
  #join population data to ods lookup
  
  df <- stp_pop %>% 
    dplyr::left_join(select(ods,CCG21CD,STP21CDH,STP21NM), by = c("CCG_CD" = "CCG21CD")) %>% 
    dplyr::group_by(STP21_NM,STP21CDH,STP21_CD) %>% 
    dplyr::summarise(POP = sum(POP)) %>% 
    ungroup()
  
  return(df)
  
}