context("geocode")

test_that("geocoding works", {
  df <- dplyr::tibble(
    country_name = c("Germany", "Germany", "Afghanistan"),
    year = c(1980, 2017, 2017)
  )
  ex <- vdem_geocode(df)
  expect_equal(names(ex), c("country_name", "year", "geometry"))
  expect_true(inherits(ex, "sf"))
  expect_equal(nrow(ex), 3)
})

test_that("metaframes are retained", {
  df <- dplyr::tibble(
    country_name = c("Germany", "Germany", "Afghanistan"),
    year = c(1980, 2017, 2017)
  ) %>% tectr::fx_default()
  ex <- vdem_geocode(df)
  expect_true(tectr::has_metaframe(ex))
})
