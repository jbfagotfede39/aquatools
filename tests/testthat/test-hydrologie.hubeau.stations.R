library(testthat)
library(aquatools)

test_that("hydrologie.hubeau.stations format données en entrée", {
  expect_error(hydrologie.hubeau.stations(), "Aucun paramètre fourni")
  expect_error(hydrologie.hubeau.stations(39, region_insee = 27), "Plus d'un paramètre de sélection saisi : il n'en faut qu'un unique")
})