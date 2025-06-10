library(testthat)
library(aquatools)

test_that("chronique.meteofrance.mesures format données", {
  expect_error(chronique.meteofrance.mesures("Horaire", "391", "2025-01-01T00:00:00Z", "2025-04-01T00:00:00Z", token), "Identifiant de station fourni au mauvais format : 8 chiffres nécessaires")
  expect_error(chronique.meteofrance.mesures("Journalier", "391", "2025-01-01T00:00:00Z", "2025-04-01T00:00:00Z", token))
  expect_error(chronique.meteofrance.mesures(type = NA, "391", "2025-01-01T00:00:00Z", "2025-04-01T00:00:00Z", token))
  # expect_error(chronique.meteofrance.mesures("Horaire", "39159002", "2025-01-01", "2025-04-01T00:00:00Z", token), "date_debut_obs saisie au mauvais format")
})