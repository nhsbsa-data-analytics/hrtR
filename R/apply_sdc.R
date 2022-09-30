#' @title Easy helper for 'apply_sdc'
#'
#' @name apply_sdc
#'
#' @description
#' Apply SDC to a data set
#'
#' @param data the data to apply SDC to
#' @param level the value below which SDC will be applied
#' @param rounding whether to apply rounding
#' @param round_val the value to round to
#' @param mask the value to mask values with
#'
#' @export
#'
#' @example
#' apply_sdc(data, 5, FALSE, 5, NA)

apply_sdc <-
  function(data,
           level = 5,
           rounding = FALSE,
           round_val = 5,
           mask = as.numeric(NA)) {
    `%>%` <- magrittr::`%>%`
    rnd <- round_val
    if (is.character(mask)) {
      type <- function(x)
        as.character(x)
    } else {
      type <- function(x)
        x
    }
    data %>% dplyr::mutate(dplyr::across(
      where(is.numeric),
      .fns = ~ dplyr::case_when(
        .x >= level & rounding == T ~ type(rnd * round(.x / rnd)),
        .x < level & .x > 0 & rounding == T ~ mask,
        .x < level & .x > 0 & rounding == F ~ mask,
        TRUE ~ type(.x)
      ),
      .names = "sdc_{.col}"
    ))
  }
