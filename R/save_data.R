#' Fast CSV writer
#'
#' @description Write data to storage using `data.table::fwrite`. This function
#' also adds a time stamp to the file name to allow auditing of data files.
#'
#' @param data A data.frame object to be written to CSV
#' @param dir The directory to save the CSV to. defaults to `.` - current
#' working/project directory. Creates `data` sub-directory if it doesn't already
#' exist
#' @param filename Name for the file. This is concatenated with a time stamp
#' for audit purposes.
#' @param ... Optional arguments to be passed to `data.table::fwrite`.
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' save_data(mtcars, filename = "mumh_quarterly")
#'
#' save_data(
#' mtcars,
#' dir = "Y:/Official Stats/MUMH",
#' filename = "mumh_quarterly",
#' quote = TRUE
#' )
save_data <- function (data, dir = ".", filename, ...) {

  # check for directory and create if false
  dir.create(file.path(dir, "data"), showWarnings = FALSE)

  # create time stamp to audit data
  stamp <- format(Sys.time(),format = "%Y%m%d%H%M%S")

  # check for extension in filename
  if(grepl("\\.([[:alnum:]]+)$", filename)) {
    # if extension present return NULL
    ext <- NULL
  } else {
    # if no extension append csv
    ext <- ".csv"
  }

  # construct file path
  path <- file.path(paste0(dir, "/data/", filename,"-", stamp, ext))

  # write file using data.table function. much faster than write.csv
  data.table::fwrite(data, path, ...)
}
