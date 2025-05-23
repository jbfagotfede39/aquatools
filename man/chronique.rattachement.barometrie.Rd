% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chronique.rattachement.barometrie.R
\name{chronique.rattachement.barometrie}
\alias{chronique.rattachement.barometrie}
\title{Rattachement de valeurs piézométriques manquantes}
\usage{
chronique.rattachement.barometrie(data, duree_max_rattachement = 1)
}
\arguments{
\item{data}{Chronique à modifier, avec un champ chmes_date, un champ \code{time} et les jeux de données en colonne (format large)}

\item{duree_max_rattachement}{Durée maximale de rattachement, en heures - \code{1} (par défaut)}
}
\description{
Permet de rattracher une valeur piézométrique à la valeur barométrique la plus proche temporellement
}
\examples{
chronique.rattachement.barometrie(data)
chronique.rattachement.barometrie(duree_max_rattachement = 2)
}
\keyword{chronique}
