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

test_that("geocode returns an empty geometry collection if a shape is unknown",
          {
  df <- dplyr::tibble(
    country_name = c("Germany", "Germany", "Bavaria"),
    year = c(2017, 1800, 2010)
  )
  ex <- vdem_geocode(df)
  expect_equal(nrow(ex), 3)
  expect_true(inherits(ex, "sf"))
  expect_equal(
    ex %>%
      dplyr::filter(year <= 2010) %>%
      magrittr::extract2("geometry"),
    sf::st_sfc(sf::st_multipolygon(), sf::st_multipolygon(),
               crs = sf::st_crs(ex))
  )
})
