#' Build the final data frame
#'
#' This function takes as input the raw data frame as read in by
#' [haven::read_dta()].
#' The building process consists of several steps that can be looked up in
#' detail in the sub-functions and the details to this documentation.
#'
#' * [bdv_correct_names()] corrects inconsistencies in the column naming.
#' * [bdv_correct_variables()] corrects the formating of the variables
#' where necessary.
#'
#' @export

build_df_vdem <- function(vdem_raw) {
  vdem <- vdem_raw %>%
    magrittr::set_names(bdv_correct_names(names(.))) %>%
    bdv_correct_variables()
}

#' Correct the column naming
#'
#' There is an inconsistency in the column naming. Normally the structure is
#' almost always "<variable name>_osp_codelow" (where the latter two parts
#' might be missing). However, there are variables where there is a "_leg" or
#' "_ex" at the end. It is not always clear where the corresponding variables
#' come from - some of them are exactly the same with or without suffix. We will
#' deal with this in another function.

bdv_correct_names <- function(names) {
  # One of the following mixtures has to exist that is not empty:
  grid <- expand.grid(
    c("_osp", "_ord", ""),
    c("_codehigh", "_codelow", "_sd", "")
  )
  proper_end <- paste0(grid[[1]], grid[[2]]) %>%
    magrittr::extract(. != "") %>%
    paste(collapse = "|")
  unproblematic_pattern <- glue::glue(
    "(?:{proper_end})$"
  )
  problematic_pattern <- glue::glue(
    "^([[:alnum:]]{1,50})(<proper_end>)(_[:alpha:]{1,50})$",
    .open = "<", .close = ">"
  )
  names <- dplyr::if_else(stringr::str_detect(names, unproblematic_pattern),
                          names,
                          stringr::str_replace(names, problematic_pattern,
                                               "\\1\\3\\2"))
  names
}


#' Correct the variables
#'
#' These functions correct the different kinds of variables.

bdv_correct_variables <- function(vdem_raw) {
  vdem_list <- list(
    bdv_identifiers(vdem_raw),
    bdv_interval(vdem_raw),
    bdv_type_C(vdem_raw)
  )
  purrr::reduce(vdem_list,
                ~ dplyr::inner_join(.x, .y, by = c("country_id", "year")))
}

#' @describeIn bdv_correct_variables extracts the identifier variables

bdv_identifiers <- function(vdem_raw) {
  identifier_cols <- c(
    "country_name", "country_text_id", "country_id", "year", "historical_date",
    "project", "historical", "histname", "codingstart", "codingend",
    "codingstart_contemp", "codingstart_hist", "codingend_hist", "gapstart1",
    "gapstart2", "gapstart3", "gapend1", "gapend2", "gapend3", "COWcode"
  )
  dplyr::select(vdem_raw, !!identifier_cols)
}

#' @describeIn bdv_correct_variables extract all interval variables.

bdv_interval <- function(vdem_raw) {
  mf_interval <- mf_vdem %>%
    dplyr::filter(bdv_interval_filter(fxInfo_scale),
                  include)
  pattern <- glue::glue(
    "^(?:",
    glue::glue_collapse(mf_interval$name, sep = "|"),
    ")(?:_codehigh|_codelow|_sd|)$"
  )
  interval_cols <- stringr::str_subset(names(vdem_raw), pattern)
  select(vdem_raw, country_id, year, !!interval_cols)
}

#' @rdname bdv_correct_variables

bdv_interval_filter <- function(fxInfo_scale) {
  fxInfo_scale %in%
    c("Interval", "Interva", "Numeric",
      glue::glue(
        "The index scores are derived from a simple spatial model and",
        " theoretically ranges from 0 to 1, with higher scores indicating",
        " more political constraint and thus less feasibility of policy",
        " change"
      )
    )
}

#' @describeIn bdv_correct_variables Type C variables occur as "<name>",
#' "<name>_osp", "<name>_ord". The former two can remain as they are,
#' the latter is a factor and will be changed accordingly. This is very
#' easy by using the `data.frame`-method of [haven::as_factor()]. The series of
#' dichotomous scales are regarded here as well.

bdv_type_C <- function(vdem_raw) {
  mf_type_C <- filter(
    mf_vdem,
    bdv_type_C_filter(
      ind_type, fxInfo_answer_type, fxInfo_scale,
      bdv_interval_filter(fxInfo_scale)
      ),
    include
    )
  lgl_ordered <- stringr::str_detect(mf_type_C$fxInfo_scale, "Ordinal")
  mf_ordered <- filter(mf_type_C, lgl_ordered)
  mf_rest <- filter(mf_type_C, !lgl_ordered)
  suffix <- "(?:_codehigh|_codelow|_sd|)"
  ordered_pattern <- glue::glue("(?:",
    glue::glue_collapse(mf_ordered$name, sep = "|"), ")",
    suffix
  )
  ordered_cols <- stringr::str_subset(names(vdem_raw), ordered_pattern)
  rest_pattern <- glue::glue("(?:",
    glue::glue_collapse(mf_rest$name, sep = "|"), ")",
    suffix
  )
  rest_cols <- stringr::str_subset(names(vdem_raw), rest_pattern)
  inner_join(
    vdem_raw %>% dplyr::select(country_id, year, !!ordered_cols) %>%
      purrr::map_dfc(function(col) {
                       if(is.labelled(col))
                         haven::as_factor(col, ordered = TRUE)
                       else col
                     }),
    vdem_raw %>% dplyr::select(country_id, year, !!rest_cols) %>%
      haven::as_factor(),
    by = c("country_id", "year")
  )
}

bdv_type_C_filter <-
  function(ind_type, fxInfo_answer_type, fxInfo_scale, interval_filter) {
  (ind_type == "C"  |
     (stringr::str_detect(fxInfo_answer_type, "(?:S|s)election") &
        !stringr::str_detect(fxInfo_scale, "Ordinal")) |
     stringr::str_detect(fxInfo_scale, "(?:S|s)eries")) &
    !interval_filter
}
