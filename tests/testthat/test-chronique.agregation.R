library(testthat)
library(aquatools)

test_that("chronique.agregation format données", {
  expect_no_error(mesures_exemple %>% filter(chmes_coderhj == "DRO11-6") %>% chronique.agregation(), message = "Exécution sur données d'exemple filtrées sur une unique station")
})