% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissons.captures.R
\name{poissons.captures}
\alias{poissons.captures}
\title{Extraction des données de captures de poissons pour une opération}
\usage{
poissons.captures(
  station = "",
  date = "",
  operation = "",
  codeCapture = FALSE,
  codePlacette = FALSE,
  codeOperation = FALSE,
  observations = FALSE,
  profondeur_capture = FALSE
)
}
\arguments{
\item{station}{CodeRHJ de la station}

\item{date}{Date de l'opération}

\item{operation}{Code de l'opération}

\item{codeCapture}{Affichage du codecapture - \code{FALSE} (par défault)}

\item{codePlacette}{Affichage du codeplacette - \code{FALSE} (par défault)}

\item{codeOperation}{Affichage du codeoperation - \code{FALSE} (par défault)}

\item{observations}{Affichage de la colonne observations - \code{FALSE} (par défault)}

\item{profondeur_capture}{Affichage de la colonne profondeurcapture - \code{FALSE} (par défault)}
}
\description{
Récupère les données de captures de poissons d'une opération
}
\examples{
poissons.captures()
poissons.captures("SOR10-2", "2015-05-19")
poissons.captures("SOR10-2", "2015-05-19", codePlacette=T)
poissons.captures(operation = 319)
}
\keyword{donnees}
