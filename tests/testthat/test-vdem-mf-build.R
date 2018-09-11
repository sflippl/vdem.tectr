context("vdem-mf-build")

test_that("bmv_spread_names", {
  mf_test <- dplyr::tibble(
    name = c("v2x_polyarchy",
             "v2peedueq, *_osp, *_ord",
             "e_v2x_api_3C /_4C /_5C"),
    num = 1:3
  )
  expect_equal(mf_test[1, ], bmv_spread_names(mf_test[1, ]))
  expect_equal(nrow(bmv_spread_names(mf_test[2, ])), 3)
  expect_equal(nrow(bmv_spread_names(mf_test[3, ])), 3)
  expect_false(any(stringr::str_detect(bmv_spread_names(mf_test)$name,
                                       "\\s")))
})

test_that("bmv_cautionary_notes", {
  expect_silent(bmv_cautionary_notes(head(mf_revisable)))
})

test_that("bmv_include", {
  mf <- dplyr::tibble(
    fxInfo_data_release =
      c("", "8", "6-8.", "3-6", "8. Only in disaggregated dataset", ""),
    name = c(rep("", 5), "v2elintro"),
    ind_type = rep("", 6)
  )
  expect_equal(bmv_include(mf)$include, c(rep(TRUE, 3), rep(FALSE, 3)))
})
