% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chronique.bruit.temps.R
\name{chronique.bruit.temps}
\alias{chronique.bruit.temps}
\title{Création de bruit temporel dans données brutes de chronique}
\usage{
chronique.bruit.temps(data, bruit_max = 60)
}
\arguments{
\item{data}{Chronique à modifier, avec un champ chmes_date, un champ chmes_heure et un champ chmes_valeur}

\item{bruit_max}{Durée maximale de bruit (positif et négatif, en minutes) - \code{60} par défaut}

\item{recalcul}{Les dates/heures présentes doivent-elles être entièrement re-calculées, et si oui, depuis le début ou la fin avec l'écart considéré (\code{non} (par défaut), \code{debut} ou \code{fin})}
}
\description{
Permet de créer du bruit temporel dans des données brutes de chroniques
}
\examples{
mesures_2 \%>\% chronique.bruit.temps()
mesures_2 \%>\% chronique.bruit.temps(30)
}
\keyword{chronique}
