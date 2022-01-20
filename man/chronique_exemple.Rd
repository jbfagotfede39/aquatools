\name{chronique_exemple}
\alias{chronique_exemple}
\docType{data}
\title{
Jeu de données de chronique d'exemple
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("chronique_exemple")}
\format{
  Différents tibbles de données relatives à des chroniques d'exemple, pouvant être utilisées avec les fonctions de la même thématique.
  \describe{
    \item{\code{stations_exemple}}{un tibble contenant différentes stations}
    \item{\code{suivi_exemple}}{un tibble contenant différentes données de suivi de terrain relatives aux éléments de \code{stations_exemple}}
    \item{\code{mesures_exemple}}{un tibble contenant des mesures de chronique relatives aux éléments de \code{stations_exemple}}
    \item{\code{capteurs_exemple}}{un tibble contenant différentes données de capteurs associées aux éléments de \code{suivi_exemple} et des \code{mesures_exemple}}
    \item{\code{resultats_exemple}}{un tibble contenant des résultats associées aux \code{mesures_exemple}}
    \item{\code{commentaires_exemple}}{un tibble contenant des commentaires des \code{resultats_exemple}}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(chronique_exemple)
stations_exemple
suivi_exemple
mesures_exemple
capteurs_exemple
resultats_exemple
commentaires_exemple
}
\keyword{datasets}
