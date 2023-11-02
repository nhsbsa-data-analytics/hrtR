apply_sdc <-
  function(data,
           level = 0,
           rounding = TRUE,
           round_val = 5,
           mask = NA) {
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




apply_sdc <-
  function(data,
           level = 5,
           rounding = TRUE,
           round_val = 5,
           mask = NA) {
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
