#' Add a spatial column with the country polygons
#'
#' This functions transforms the vdem dataframe or a part of it into a
#' [sf::st_sf()]-simple feature. It needs the columns "country_name" and
#' "year".
#'
#' @param data the data frame
#'
#' @export

vdem_geocode <- function(data) {
  ret <-
    data %>%
    dplyr::inner_join(vdem_spatial, by = "country_name") %>%
    sf::st_as_sf() %>%
    dplyr::filter(start_year <= year, year <= end_year) %>%
    dplyr::select(-cow_code, -start_year, -end_year)
  ret <-
    data %>%
    dplyr::anti_join(ret, by = c("country_name", "year")) %>%
    dplyr::mutate(
      geometry =
        rep(sf::st_sfc(sf::st_multipolygon(), crs = sf::st_crs(ret)), nrow(.))
    ) %>%
    sf::st_as_sf() %>%
    rbind(ret)
  if(tectr::has_metaframe(data))
    ret <-
      ret %>%
      tectr::set_metaframe(
        tectr::metaframe(data)
      )
  ret
}
