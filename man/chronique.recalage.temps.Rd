% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chronique.recalage.temps.R
\name{chronique.recalage.temps}
\alias{chronique.recalage.temps}
\title{Décalage de données brutes de chronique}
\usage{
chronique.recalage.temps(
  data,
  decalage = 2598.581,
  recalcul = c("non", "debut", "fin")
)
}
\arguments{
\item{data}{Chronique à modifier, avec un champ chmes_date, un champ chmes_heure et un champ chmes_valeur}

\item{decalage}{Valeur de décalage (en jours, positif pour avancer ou négatif pour reculer) à calculer par soustration des deux dates au format lubridate}

\item{recalcul}{Les dates/heures présentes doivent-elles être entièrement re-calculées, et si oui, depuis le début ou la fin avec l'écart considéré (\code{non} (par défaut), \code{debut} ou \code{fin})}
}
\description{
Permet de corriger des données brutes de chronique présentant des valeurs de date et heure aberrantes. ATTENTION : ne gère pas les ID
}
\examples{
ymd_hms("2017-07-05 11:40:00")-ymd_hms("2010-05-24 21:44:00")
chronique.recalage.temps(data, decalage = 2598.581)
chronique.recalage.temps(decalage = -57)
chronique.recalage.temps(decalage = -57, recalcul == "debut") # On met un décalage pour que la date et l'heure de la première ligne soit correct
chronique.recalage.temps(decalage = -57, recalcul == "fin") # On met un décalage pour que la date et l'heure de la dernière ligne soit correct
}
\keyword{chronique}
