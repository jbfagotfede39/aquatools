% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BDD.ouverture.R
\name{BDD.ouverture}
\alias{BDD.ouverture}
\title{Chargement des bases de données}
\usage{
BDD.ouverture(
  Type = c("Chroniques", "Poissons", "Macroinvertébrés", "Physico-chimie",
    "Temps de travail", "Data"),
  utilisateur = NA_character_,
  motdepasse = NA_character_
)
}
\arguments{
\item{Type}{Type de base de données. Chroniques par défaut}

\item{utilisateur}{Nom d'utilisateur, si on souhaite en spécifier un}

\item{motdepasse}{Mot de passe du portefeuille keyring, si accès côté RStudio server}
}
\description{
Cette fonction permet de charger les bases de données de la fédération
}
\examples{
BDD.ouverture()
BDD.ouverture(Type = "Poissons")
dbD <- BDD.ouverture(Type = "Data")
dbP <- BDD.ouverture(Type = "Poissons")
tbl(dbD, in_schema("fd_production", "chroniques_commentaires")) \%>\% collect(n = Inf)
}
