% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MI.indices.R
\name{MI.indices}
\alias{MI.indices}
\title{Calcul d'indices et de listes de MI}
\usage{
MI.indices(
  DataTravail = MI.captures(MI.operations(Sortie = "Complet")),
  Sortie = c("indices", "listefaunistiqueGlobale", "listefaunistiqueMAG20",
    "listefaunistiqueDCEPrelevement", "listefaunistiqueDCEPhase",
    "listefaunistiqueDCEequivalentIBGN", "listefaunistiqueIBGN")
)
}
\arguments{
\item{DataTravail}{Dataframe contenant les données de captures issues de MI.indices()}

\item{Sortie}{Forme du dataframe de sortie - \code{indices} (par défault), \code{listefaunistiqueGlobale}, \code{listefaunistiqueMAG20}, \code{listefaunistiqueDCEPrelevement}, \code{listefaunistiqueDCEPhase}, \code{listefaunistiqueDCEequivalentIBGN}, \code{listefaunistiqueIBGN}}
}
\description{
Calcule d'indices synthétiques à partir de données de captures d'une opération ou plusieurs opérations de suivi MI
}
\examples{
MI.indices(DataTravail)
MI.indices(DataTravail, Sortie = listefaunistiqueMAG20)
}
\keyword{donnees}
