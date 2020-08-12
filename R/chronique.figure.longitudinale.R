#' Profils en long de chroniques
#'
#' Cette fonction permet de représenter sous forme de profil longitudinal des données de chroniques
#' @name chronique.figure.longitudinale
#' @param data Data.frame issu de la fonction chronique.resultats ou de la base de données Chroniques, avec jointure de chsta_milieu et de chsta_distancesource
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les
#'    figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @keywords chronique
#' @import ggplot2
#' @import tcltk
#' @import tidyverse
#' @export
#' @examples
#' Resultats %>% chronique.figure.longitudinale()
#' Resultats %>% chronique.resultats.filtrage() %>% chronique.figure.longitudinale()
#' Resultats %>% left_join(Stations %>% select(chsta_coderhj, chsta_milieu, chsta_distancesource, chsta_distancesource_confluencedrainprincipal), by = c("chres_coderhj" = "chsta_coderhj")) %>% chronique.resultats.filtrage() %>% chronique.figure.longitudinale()

##### TODO LIST #####
# Modification des couleurs
# 
#####################

chronique.figure.longitudinale <- function(
  data = data,
  save=F,
  projet = NA_character_,
  format=".png"
)
{
  
  #### Évaluation des choix ####
  #Recherche <- match.arg(Recherche)
  
  #### Contexte des données ####
  Contexte <- chronique.contexte(data)
  
  #### Tests ####
  if(Contexte$ntypemesure != 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  if(Contexte$nstation == 0 | Contexte$nannee == 0) stop("Aucune donnée dans la chronique à analyser")
  if(Contexte$nstation <= 2) stop(glue("Seulement {Contexte$nstation} station(s) au sein du jeu de données"))
  if("chsta_milieu" %in% names(data) == FALSE) stop("Pas de champs chsta_milieu dans le jeu de données en entrée")
  if("chsta_distancesource" %in% names(data) == FALSE) stop("Pas de champs chsta_distancesource dans le jeu de données en entrée")
  if("chsta_distancesource_confluencedrainprincipal" %in% names(data) == FALSE) stop("Pas de champs chsta_distancesource_confluencedrainprincipal dans le jeu de données en entrée")
  
  #### Calcul des distances à la source homogènes ####
  if(Contexte$nmilieu != 1){
    ecosystemeppal <- tcltk::tk_select.list(sort(unlist(strsplit(Contexte$milieu, ";"))), multiple = F, title = "Écosystème principal")
  }
  
  data <-
    data %>% 
    {if(Contexte$nmilieu == 1) mutate(., distancesourcesynthetique = chsta_distancesource) else .} %>% 
    {if(Contexte$nmilieu != 1) mutate(., distancesourcesynthetique = ifelse(chsta_milieu == ecosystemeppal, chsta_distancesource, chsta_distancesource_confluencedrainprincipal)) else .}

  
  #### Paramètres de légende ####
  parametres <- Contexte %>% chronique.figure.parametres()
  legendeY <- parametres$legendeY
  legendeTitre <- parametres$legendeTitre
  typemesureTitreSortie <- parametres$typemesureTitreSortie
  
  if(nchar(Titre) == 0 & Contexte$nmilieu == 1) Titre <- Contexte$milieu
  if(nchar(Titre) == 0 & Contexte$nmilieu != 1) Titre <- ecosystemeppal

  #### Calcul d'une clé par écosystème/année ####
  data <-
    data %>%
    chronique.cle(formatcle = "MA")
    
  #### Représentation graphique
  gg <- ggplot(data, aes(x=chsta_distancesource, y=chres_vmaxmoy30j, color = Cle))
  #gg <- gg + geom_point(shape=1)   # Use hollow circles
  gg <- gg + geom_line()
  #gg <- gg + geom_text_repel(data=data, aes(x=distancesourcesynthetique, y=chres_vmaxmoy30j, label=chres_coderhj) , size=3)
  gg <- gg + ylim(0, 29)
  gg <- gg + labs(x = "Distance à la source (km)", y = expression(Tmm30j~(degree*C)), colour = "Milieu - Année biologique") # Pour changer le titre
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.title.x = element_text(size=10),
                   axis.title.y = element_text(size=10)
  )
  gg
  
  #### Affichage des résultats ####
  if(save==T){
    ggsave(file=paste(projet,"/Sorties/Vues/Intersites/Intersites",typemesureTitreSortie,Titre,format,sep=""))

  }
  if(save==F){return(gg)}
} # Fin de la fonction
