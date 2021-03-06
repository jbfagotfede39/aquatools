% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chronique.existence.R
\name{chronique.existence}
\alias{chronique.existence}
\title{Vérification de l'existence de stations/capteurs}
\usage{
chronique.existence(
  Stations = NA_character_,
  Capteurs = NA_character_,
  Station = NA_character_,
  Capteur = NA_character_
)
}
\arguments{
\item{Stations}{Data.frame contenant la liste de référence des stations}

\item{Capteurs}{Data.frame contenant la liste de référence des capteurs}

\item{Station}{Station recherchée}

\item{Capteur}{Capteur recherché}
}
\description{
Cette fonction permet de vérifier l'existence de références de stations/capteurs dans les tables de références
}
\examples{
chronique.existence(Stations, Capteurs, "VAL6-0", "U4129")
chronique.existence(Stations, Capteurs, Station, Capteur)
}
\keyword{chronique}
