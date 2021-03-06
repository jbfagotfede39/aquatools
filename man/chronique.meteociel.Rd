% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chronique.meteociel.R
\name{chronique.meteociel}
\alias{chronique.meteociel}
\title{Collecte des relevés sur meteociel.fr}
\usage{
chronique.meteociel(
  codestationrecherche = NA_integer_,
  moisrecherche = NA_integer_,
  anneerecherche = NA_integer_
)
}
\arguments{
\item{codestationrecherche}{Station Météo-France recherchée, au format 39013004}

\item{moisrecherche}{Mois recherché, au format 01}

\item{anneerecherche}{Année recherchée, au format 2020}
}
\description{
Cette fonction permet d'extraire depuis meteociel.fr les observations d'une station donnée pour un mois donné
}
\examples{
DataToAdd <- chronique.meteociel("39013004", 01, 2020)
}
\keyword{chronique}
