#' Rafraîchir les MV des bases de données
#'
#' Cette fonction permet de rafraîchir les vues matérialisées des bases de données. Fonction à exécuter dans R depuis une machine ayant accès des droits pour lancer une requête dans la BDD
#' @name BDD.refreshMV
#' @param table Table à actualiser, avec le schéma. Exemple : "fd_referentiels.hydrographie_bdtopagecoursdeau"
#' @param serveurbddip IP du serveur de la base de données postgres/postgis
#' @param serveurbddutilisateur Utilisateur du serveur de la base de données postgres/postgis
#' @param serveurbddport Port de connexion ssh du serveur de la base de données postgres/postgis. 22 par défaut
#' @param motdepasse Mot de passe du portefeuille keyring, pour connexion autonome
#' @param url URL où télécharger le jeu de données, si nécessaire
#' @param localisation Répertoire de stockage des fichiers à télécharger sur le serveur postgres/postgis, si nécessaire. Exemple : "/data/"
#' @import glue
#' @import RPostgreSQL
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' BDD.refreshMV()

##### TODO LIST #####
#
#####################

BDD.refreshMV <- function(
  table = NA_character_,
  serveurbddip = NA_character_,
  serveurbddutilisateur = NA_character_,
  serveurbddport = 22,
  url = NA_character_,
  localisation = NA_character_
  )
{
  
  #### Évaluation des choix ####
  #Type <- match.arg(Type)
  
  #### Vérifications ####
  if(is.na(url)) warning("Il n'y aura pas de téléchargement de fichier pour mise à jour")
  if(!is.na(url) & is.na(localisation)) stop("Téléchargement spécifié sans localisation")
  if(!is.na(localisation) & is.na(url)) stop("Localisation sans URL")
  if(is.na(table)) stop("Pas de table de spécifiée")
  if(is.na(serveurbddip)) stop("Pas d'adresse IP de serveur spécifiée")
  if(is.na(serveurbddutilisateur)) stop("Pas d'utilisateur de serveur spécifié")

  #### Mise à jour du fichier ####
if(!is.na(url)){
  ### Téléchargement ###
  commande <- glue("ssh {serveurbddutilisateur}@{serveurbddip} -p {serveurbddport} 'sudo wget {url} -P {localisation}'")
  system(commande ,intern=T)
}
  
  #### Gestion du zip si nécessaire ####
  if(str_detect(url, ".zip")){
    ## Dézippage ##
    commande <- glue("ssh {serveurbddutilisateur}@{serveurbddip} -p {serveurbddport} 'sudo unzip -o {localisation}{basename(url)}'") # L'option -o est pour overwrite
    system(commande ,intern=T)
  }
  
  if(str_detect(url, ".zip")){ 
    ## Suppression du zip ##
    commande <- glue("ssh {serveurbddutilisateur}@{serveurbddip} -p {serveurbddport} 'sudo rm {localisation}{basename(url)}'")
    system(commande ,intern=T)
  }
  
  #### Rafraîchissement de la vue matérialisée
  ## Connexion ##
  dbD <- BDD.ouverture("Data", motdepasse)
  
  ## Envoi de la requête ##
  dbGetQuery(dbD, glue("REFRESH MATERIALIZED VIEW {table};"))
  
} # Fin de la fonction
