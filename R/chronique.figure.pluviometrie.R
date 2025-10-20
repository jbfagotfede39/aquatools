#' Représentation de chroniques de pluviométrie
#'
#' Cette fonction permet de représenter des chroniques de pluviométrie
#' @name chronique.figure.pluviometrie
#' @param data Data.frame de données de mesures brutes, issues de chronique.mesures()
#' @param Titre Titre du graphique (vide par défaut)
#' @param type Type de graphique : \code{montant} (par défaut), ou \code{descendant}
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @import glue
#' @import scales
#' @import tidyverse
#' @export
#' @examples
#' data %>% chronique.figure.pluviometrie()
#' MesuresTest %>% chronique.figure.pluviometrie(datedebutanneebiol = "01-01")
#' MesuresTest %>% chronique.figure.pluviometrie(datedebutanneebiol = "01-01") + scale_x_date(limits = c(ymd("2023-01-01"), ymd("2023-12-31")), labels = scales::date_format("%b", locale = "fr"), date_minor_breaks = "1 month")

chronique.figure.pluviometrie <- function(
  data = data,
  Titre = "",
  type = c("montant", "descendant"),
  datedebutanneebiol = "10-01",
  save = F,
  projet = NA_character_,
  format = ".png"
  )
{
  Sys.setlocale(locale="fr_FR.UTF-8") # Afin d'avoir les abréviations des mois en français sur les figures
  
  #### Évaluation des choix ####
  type <- match.arg(type)
  
  #### Contexte ####
  contexte <- chronique.contexte(data)
  
  #### Test de cohérence ####
  if(contexte$nstation == 0) stop("Aucune donnée dans la chronique à représenter")
  if(contexte$nstation > 1) stop("Différentes stations dans la chronique à représenter - Scénario à développer")
  if(contexte$ntypemesure > 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")
  if(contexte$typemesure != "Pluviométrie") stop("chmes_typemesure au sein du jeu de données ne correspondant pas à de la pluviométrie")
  
  #### Nettoyage & reformatage ####
  ##### Transformation du format des dates #####
  if(class(data$chmes_date) != "Date"){
    data$chmes_date <- ymd(data$chmes_date)}

  ##### Agrégation journalière si nécessaire #####
  # Données issues de la table chroniques_mesuresgroupees
  if(!("chmes_heure" %in% names(data)) & "chmesgr_coderhj_id" %in% names(data)){
    syntjour <-
      data %>% 
      arrange(chmes_date) %>% 
      mutate(VMoyJ = chmesgr_valeur) %>% # on part de données journalières déjà agrégées
      formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) %>% 
      group_by(chmesgr_coderhj_id, chmes_anneebiol) %>% 
      mutate(SommeMoyJ = cumsum(round(VMoyJ,0))) %>% 
      ungroup()
  }
  
  ##### Données issues de la fonction chronique.agregation() #####
  if(!("chmes_heure" %in% names(data)) & "chmes_coderhj" %in% names(data)){
    syntjour <-
      data %>% 
      ungroup() %>% 
      arrange(chmes_date) %>% 
      # mutate(VMoyJ = chmesgr_valeur) %>% # on part de données journalières déjà agrégées
      formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol) %>% 
      group_by(chmes_coderhj, chmes_anneebiol) %>% 
      mutate(SommeMoyJ = cumsum(round(VMoyJ,0))) %>% 
      ungroup()
  }
  
  if("chmes_heure" %in% names(data)) {
  syntjour <- 
    data %>% 
    ungroup() %>% 
    chronique.agregation(instantanne = F, mensuel = F, annuel = F, integral = F)
  }
  
  ##### Calcul de l'année biologique #####
  syntjour <- 
    syntjour %>% 
    formatage.annee.biologique(datedebutanneebiol = datedebutanneebiol)
  
  #### Représentation ####
  gg_pluvio <- ggplot(syntjour)
  gg_pluvio <- gg_pluvio + geom_bar(aes(x = chmes_date, y = VSommeJ), stat = "identity", fill = "#3288BD", alpha = 0.8)
  gg_pluvio <- gg_pluvio + scale_x_date(labels = scales::date_format("%b", locale = "fr"), date_minor_breaks = "1 month")
  if(type == "descendant") gg_pluvio <- gg_pluvio + scale_y_reverse()
  gg_pluvio <- gg_pluvio + xlab("") + ylab("Pluviométrie (mm/j)")
  gg_pluvio <- gg_pluvio + theme_minimal()
  gg_pluvio <- gg_pluvio + theme(legend.position = "none")
  if(type == "descendant") gg_pluvio <- gg_pluvio + theme(axis.title.x = element_blank(),
                                                          axis.text.x = element_blank(),
                                                          axis.ticks.x = element_blank()
                                                          )
  gg_pluvio

  #### Export ####
  if(save == T){
    ggsave(file = glue("{projet}/Sorties/Vues/Annuelles/Annuelle_pluviometrie{Titre}{format}"))
  }
  
  #### Sortie ####
  if(save == F){return(gg_pluvio)}
  
}
