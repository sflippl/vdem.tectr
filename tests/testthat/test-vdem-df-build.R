context("vdem-df-build")

test_that("bdv_correct_names", {
  ex <- c(
    "v2psprbrch_ord_codehigh",
    "v2elasmoff_codelow_ex",
    "v2elasmoff",
    "v2elasmoff_ord_codelow_ex",
    "v2elasmoff_ex"
  )
  expect_equal(
    bdv_correct_names(ex),
    c(
      "v2psprbrch_ord_codehigh",
      "v2elasmoff_ex_codelow",
      "v2elasmoff",
      "v2elasmoff_ex_ord_codelow",
      "v2elasmoff_ex"
    )
  )
})

test_that("bdv_interval", {
  fxInfo_scale <- c("Interva", "Ordered", "")
  chapter_title <- c("", "", "")
  ind_type <- c(rep("", 2), "D")
  expect_equal(
    bdv_interval_filter(fxInfo_scale),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("bdv_correct_variables", {
  mf_vdem <- build_mf_vdem(mf_revisable)
  tst <- mf_vdem %>% {
    table(c(
    dplyr::filter(., bdv_interval_filter(fxInfo_scale),
                  include)$name,
    dplyr::filter(., !include)$name,
    dplyr::filter(.,
                  bdv_type_C_filter(ind_type,
                                    bdv_interval_filter(
                                      fxInfo_scale
                                    )),
                  include)$name,
    dplyr::filter(., bdv_text_filter(fxInfo_responses), include)$name
  ))
  }
  expect_equal(as.numeric(tst), rep(1, length(tst)))
  rem_mf_vdem <- mf_vdem %>%
    dplyr::filter(
      !bdv_interval_filter(fxInfo_scale),
      !bdv_type_C_filter(ind_type, bdv_interval_filter(fxInfo_scale)),
      !include
    )
})
