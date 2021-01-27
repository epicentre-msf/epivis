
library(dplyr)
library(sf)

sf_world_raw <- rnaturalearth::ne_countries(scale = "small", type = "countries", returnclass = "sf")

sf_world <- sf_world_raw %>%
  sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84") %>%
  tibble::as_tibble() %>% # for nicer printing
  sf::st_as_sf() %>%
  dplyr::select(country = name_long, iso_a3, iso_a2, pop_est) %>%
  dplyr::mutate(
    continent = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent")),
    region = suppressWarnings(countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"))
  ) %>%
  dplyr::filter(stringr::str_detect(country, "Antarctic", negate = TRUE)) %>%
  cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(., of_largest_polygon = TRUE)))) %>%
  dplyr::rename(lon = X, lat = Y) %>%
  dplyr::select(continent, region, dplyr::everything())

usethis::use_data(sf_world, overwrite = TRUE)
