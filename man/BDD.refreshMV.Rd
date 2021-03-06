% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BDD.refreshMV.R
\name{BDD.refreshMV}
\alias{BDD.refreshMV}
\title{Rafraîchir les MV des bases de données}
\usage{
BDD.refreshMV(
  table = NA_character_,
  serveurbddip = NA_character_,
  serveurbddutilisateur = NA_character_,
  serveurbddport = 22,
  url = NA_character_,
  localisation = NA_character_
)
}
\arguments{
\item{table}{Table à actualiser, avec le schéma. Exemple : "fd_referentiels.hydrographie_bdtopagecoursdeau"}

\item{serveurbddip}{IP du serveur de la base de données postgres/postgis}

\item{serveurbddutilisateur}{Utilisateur du serveur de la base de données postgres/postgis}

\item{serveurbddport}{Port de connexion ssh du serveur de la base de données postgres/postgis. 22 par défaut}

\item{url}{URL où télécharger le jeu de données, si nécessaire}

\item{localisation}{Répertoire de stockage des fichiers à télécharger sur le serveur postgres/postgis, si nécessaire. Exemple : "/data/"}

\item{motdepasse}{Mot de passe du portefeuille keyring, pour connexion autonome}
}
\description{
Cette fonction permet de rafraîchir les vues matérialisées des bases de données. Fonction à exécuter dans R depuis une machine ayant accès des droits pour lancer une requête dans la BDD
}
\examples{
BDD.refreshMV(table <- "fd_referentiels.hydrographie_bdtopagecoursdeau", serveurbddip = "192.168.1.102", serveurbddutilisateur = "ubuntu", serveurbddport = 22, motdepasse = motdepasse, url = "http://services.sandre.eaufrance.fr/telechargement/geo/ETH/BDTopage/2019/CoursEau/CoursEau_FXX-shp.zip", localisation = "/data/")
}
