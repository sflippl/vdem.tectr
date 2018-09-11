#' Build the final metaframe
#'
#' `mf_revisable` allows manual changes at the appropriate places in order to
#' facilitate quick corrections. It is then used to build the actual metaframe.
#' The building process consists of several steps that can be looked up in
#' detail in the sub-functions and the details to this documentation.
#'
#' * [tectr::fx_read()] reads in the json data files that might have been
#' manually changed
#' * [bmv_spread_names()] spreads the names that specify several variables
#' (e. g. "v2eldonate, \*_osp, \*_ord")
#' * [bmv_cautionary_notes()] includes the cautionary notes in the codebook
#' * [bmv_include()] marks all variables where the data release specifies that
#' they have not been included in version 8 as excluded (`include=FALSE`)
#' * [bmv_geom()] adds the geometry arguments
#'
#' @export

build_mf_vdem <- function(mf_revisable, vdem_raw) {
  mf_vdem <- tectr::fx_read(mf_revisable) %>%
    dplyr::mutate(name = bdv_correct_names(name)) %>%
    bmv_spread_names() %>%
    bmv_cautionary_notes() %>%
    bmv_dichotomous_series() %>%
    bmv_include()
}

#' Spread the names
#'
#' Some names of variables actually specify several different variables, most
#' prominently the categorical variables. Moreover there had originally been one
#' typo in the name which
#' has been changed manually in the Codebook.
#'
#' @keywords internal

bmv_spread_names <- function(mf_revisable) {
  # Build a data frame with the names before and the names afterwards
  key <- dplyr::tibble(
    before = mf_revisable$name,
    name = mf_revisable$name %>%
      purrr::map(
      function(name) {
        if(stringr::str_detect(name, "\\*_osp,")) {
          stringr::str_extract(name, "^[^\\s*]*(?=,)") %>%
            return()
        }
        else if(stringr::str_detect(name, "_3C\\s\\/")) {
          stringr::str_extract(name, "^\\S*(?=_3C)") %>%
            paste0(c("_3C", "_4C", "_5C")) %>%
            return()
        }
        else return(name)
      }
    )
  ) %>% tidyr::unnest()
  dplyr::inner_join(key, mf_revisable, by = c(before = "name")) %>%
    dplyr::select(-before)
}

#' Create the variables from the series of dichotomous variables
#'
#' These variables are given in the codebook by one variable although they
#' actually consist of several different variables. The new variable will have
#' a slightly artificial question and these variables will be the subject of
#' further revision. Still, these modification make the variables accessible
#' and do not change them so that the information from the codebook alone is
#' sufficient to work with them.
#'
#' @keywords internal

bmv_dichotomous_series <- function(mf_revisable) {
  is_ds <- mf_revisable %$%
  {(stringr::str_detect(fxInfo_answer_type, "(?:S|s)election") &
      !stringr::str_detect(fxInfo_scale, "Ordinal")) |
      stringr::str_detect(fxInfo_scale, "(?:S|s)eries")}
  mf_rem <- mf_revisable[!is_ds, ]
  mf_dich <- mf_revisable[is_ds, ]
  mf_dich_new <- seq_len(nrow(mf_dich)) %>%
    purrr::map_dfr(
      function(i_row) {
        tmp_old <- mf_dich[i_row, ]
        resps <- tmp_old$fxInfo_responses[[1]] %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(
            value = stringr::str_split_fixed(value,
                pattern = stringr::coll("(0=No, 1=Yes)"),
                n = 2)[, 1] %>% stringr::str_trim()
          )
        question <- tmp_old$fxInfo_question
        question_new <- glue::glue(
          question, " Is the answer \"{resps$value}\"?"
        ) %>% as.character()
        response_new <- "(0=No, 1=Yes)"
        name_new <- paste0(tmp_old$name, "_", resps$key)
        tmp_old <- tmp_old[rep(1, nrow(resps)), ]
        tmp_old %>%
          dplyr::mutate(
            name = name_new,
            fxInfo_question = question_new,
            fxInfo_response = response_new
          )
      }
    )
  dplyr::bind_rows(mf_rem, mf_dich_new)
}

#' Include cautionary notes
#'
#' The cautionary notes specify a few things that will be included as a
#' variable `fxInfo_cautionary`. The cautionary notes concering the number of
#' experts cannot be included here as they depend on the observation unit as
#' well.
#'
#' @keywords internal

bmv_cautionary_notes <- function(mf_revisable) {
  mf_revisable <- mf_revisable %>%
    dplyr::mutate(
      fxInfo_cautionary = dplyr::case_when(
        name %in% c(
          "v2eldonate", "v2elpeace", "v2psprbrch"
        ) ~ glue::glue(
          "While the country-date estimates reached a stationary distribution",
          " according to our standard diagnostic procedures, population-level",
          " thresholds (gamma_mu) did not."
        ),
        name %in% c(
          "v2x_frassoc_thick", "v2x_clpol", "v2xcs_ccsi"
        ) ~ glue::glue(
          "While the country-date estimates converged in the factor models",
          " some of the parameters, although near convergence, did not",
          " converge according to the standard V-Dem criteria."
        ),
        name == "v2x_veracc" ~
          glue::glue(
            "While estimates converged according to standard V-Dem criteria,",
            " some parameters involved in the estimation process for this",
            " variable did not. "
          ),
        name %in% c("v2mefemjrn", "v2clsnlpct") ~
          glue::glue(
            "These variables should be used with caution."
          ),
        name %in% c(
          "v2elnoncit", "v2elmalsuf", "v2elfemsuf", "v2elsnlpop", "v2elsnmpop",
          "v2psswitch", "v2clsnmpct", "v2svstterr", "v2svstpop", "v2meaccess"
        ) ~
          glue::glue(
            "These variables have been excluded since data for these variables",
            " have not gone through full quality control with thorough",
            " cross-referencing yet."
          ),
        name == "v2lgqumin" ~
          glue::glue(
            "Since there are mistakes in the time series, the variable has",
            " not been included."
          ),
        TRUE ~ ""
      )
    )
  mf_revisable
}

#' Which names are included?
#'
#' A few variables are not included and will thus be filtered out. This will be
#' marked by the keyword `include`. Additionally to variables that are not
#' present, variables from external sources will be filtered out as they have a
#' diverse format and I feel that it would be better to read them in from their
#' own source. They can be associated with the variables that are present in
#' this metaframe.

bmv_include <- function(mf_revisable) {
  mf_revisable <- dplyr::mutate(
    mf_revisable,
    include = (stringr::str_detect(fxInfo_data_release, "8") |
                               fxInfo_data_release == "") &
      !stringr::str_detect(
        fxInfo_data_release,
        "(?:O|o)nly (?:included |)in (?:the |)disaggregated dataset") &
      !stringr::str_detect(
        fxInfo_data_release,
        "Only included in the disaggregated dataset."
      ) &
      !(stringr::str_detect(name, "intro")) &
      ind_type != "E"
  )
  mf_revisable
}

#' Specify geometries
#'
#' This function specifies the `fxGeom` columns of the metaframe

bmv_geom <- function(mf_revisable) {
  mf_revisable <- mf_revisable %>%
    filter(
      bdv_interval_filter(fxInfo_scale),
      bdv_type_C_filter(ind_type,
                        bdv_interval_filter(fxInfo_scale))
    )
}
