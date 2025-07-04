% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chronique.meteofrance.mesures.R
\name{chronique.meteofrance.mesures}
\alias{chronique.meteofrance.mesures}
\title{Commande de données Météo-France}
\usage{
chronique.meteofrance.mesures(
  type = c("6_minutes", "Horaire", "Quotidien", "Décadaire", "Mensuel"),
  station = NA_character_,
  date_debut_obs = NA_character_,
  date_fin_obs = NA_character_,
  token = NA_character_
)
}
\arguments{
\item{type}{Type de mesure recherche : données à pas de temps (\code{Quotidien} par défaut, \code{6_minutes}, \code{Horaire}, \code{Décadaire}, \code{Mensuel})}

\item{station}{Identifiant Météo-France de la station concernée}

\item{date_debut_obs}{Date de début des mesures à collecter (exprimée en TU), au format ISO 8601 avec TZ UTC : \code{AAAA-MM-JJT00:00:00Z}}

\item{date_fin_obs}{Date de fin des mesures à collecter (exprimée en TU), au format ISO 8601 avec TZ UTC : \code{AAAA-MM-JJT00:00:00Z}}

\item{token}{Token \code{API Key} créé dans son compte personnel du site \url{https://portail-api.meteofrance.fr}}
}
\description{
Cette fonction permet de commander des données Météo-France via l'API
}
\examples{
chronique.meteofrance.mesures("39159002", "2025-01-01T00:00:00Z", "2025-04-01T00:00:00Z", token)
chronique.meteofrance.mesures("Horaire", "39159002", "2025-01-01 00:00:00", "2025-04-01T00:00:00Z", token)
chronique.meteofrance.mesures("Quotidien", "39159002", "2025-01-01T00:00:00Z", now() - days(1), token)
chronique.meteofrance.mesures("Mensuel", "39159002", "2025-01-01 00:00:00", now() - months(1), token)
}
