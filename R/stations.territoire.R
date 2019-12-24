#' Permet d'établir un périmètre afin d'en retenir ensuite les stations
#'
#' Cette fonction permet d'extraire sous forme spatiale (sf) un territoire à une échelle donnée afin de le réutiliser par croisement spatial dans les fonctions chronique.stations ou poissons.synthese par exemple
#' @name stations.territoire
#' @param Echelle Échelle spatiale de la synthèse (commune, canton, communauté de communes, département, région, contexte de PDPG, Hydro-écorégion, entité GEMAPI, Maître d'ouvrage, Milieu, Bassin versant, Sous-bassin versant, Polygone autre)
#' @param Territoire Territoire concerné. Unique ou sous forme de vecteur (c("Villerserine", "Villers-Robert") par exemple)
#' @param Liste \code{FALSE} par défault. Permet d'afficher une liste dans laquelle choisir le territoire concerné
#' @keywords stations
#' @import dbplyr
#' @import dplyr
#' @import keyring
#' @import RPostgreSQL
#' @import sf
#' @import tcltk
#' @export
#' @examples
#' stations.territoire(data)
#' stations.territoire("ComCom", "200071595")
#' stations.territoire("Commune", c("Villerserine", "Villers-Robert"))
#' stations.territoire("Commune", c("39568", "39571"))
#' stations.territoire("Commune", c("Villerserine", "39571"))
#' stations.territoire("Milieu", c("Lac de Chalain", "Bonlieu"))

##### TODO LIST #####
# Ajout de l'échelon cantonal
# Ajout de l'échelon ComCom (regroupement dans BDD avec "NAS-DATA/Géographie/Toponymie/table-appartenance-geo-communes-18.xlsx")
# Problème avec par exemple Poligny -> Poligny + Vaux-sur-Poligny
# Il faudrait ajouter un champ "Clé", qui serait ensuite la clé de distinction pour poissons.synthese par exemple, au lieu d'avoir à tout réécrire dans cette fonction pour les mailles de regroupement
#####################

stations.territoire <- function(
  Echelle = c("Commune", "Canton", "ComCom", "Département", "Région", "ContextePDPG", "HER", "GEMAPI", "MO", "Milieu", "Bassin", "Sous-bassin", "Polygone"),
  Territoire = NA_character_,
  Liste = F
)
{
  
  ## Évaluation des choix ##
  Echelle <- match.arg(Echelle)
  
  ## Contexte ##
  if(all(is.na(Territoire)) == TRUE) Liste <- T # Si aucune définition de territoire, alors on affiche forcément une liste
  
  #### Communes #####
  if(Echelle == "Commune"){
    dbD <- BDD.ouverture("Data")
    communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #Territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
      Territoire <- tcltk::tk_select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    }
    Sortie <- communes %>% mutate(nometcode = paste0(tpcomm_commune_libelle, " - ", tpcomm_commune_insee)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Cantons ####
  if(Echelle == "Canton"){
    stop("Traitement des cantons à développer en les ajoutant aux communes")
    # dbD <- BDD.ouverture("Data")
    # communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    # DBI::dbDisconnect(dbD)
    # if(Liste == F){
    #   if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    # }
    # if(Liste == T){
    #   #Territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
    #   Territoire <- tcltk::tk_select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    # }
    # Sortie <- communes %>% mutate(nometcode = paste0(tpcomm_commune_libelle, " - ", tpcomm_commune_insee)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    # if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### ComCom ####
  if(Echelle == "ComCom"){
    stop("Traitement des ComCom à développer en les ajoutant aux communes")
    # dbD <- BDD.ouverture("Data")
    # communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    # DBI::dbDisconnect(dbD)
    # if(Liste == F){
    #   if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    # }
    # if(Liste == T){
    #   #Territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
    #   Territoire <- tcltk::tk_select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    # }
    # Sortie <- communes %>% mutate(nometcode = paste0(tpcomm_commune_libelle, " - ", tpcomm_commune_insee)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    # if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Département ####
  if(Echelle == "Département"){
    dbD <- BDD.ouverture("Data")
    communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #Territoire <- select.list(sort(communes$tpcomm_departement_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
      Territoire <- tcltk::tk_select.list(sort(communes$tpcomm_departement_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    }
    Sortie <- communes %>% mutate(nometcode = paste0(tpcomm_departement_libelle, " - ", tpcomm_departement_insee)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Région ####
  if(Echelle == "Région"){
    dbD <- BDD.ouverture("Data")
    communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #Territoire <- select.list(sort(communes$tpcomm_region_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
      Territoire <- tcltk::tk_select.list(sort(communes$tpcomm_region_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    }
    Sortie <- communes %>% mutate(nometcode = paste0(tpcomm_region_libelle, " - ", tpcomm_region_insee)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### ContextePDPG ####
  if(Echelle == "ContextePDPG"){
    dbD <- BDD.ouverture("Data")
    contextesPDPG <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_contextespdpg;")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #Territoire <- select.list(sort(contextesPDPG$hycont_contexte_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
      Territoire <- tcltk::tk_select.list(sort(contextesPDPG$hycont_contexte_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    }
    Sortie <- contextesPDPG %>% mutate(nometcode = paste0(hycont_contexte_libelle, " - ", hycont_contexte_code)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### HER ####
  if(Echelle == "HER"){
    dbD <- BDD.ouverture("Data")
    HER <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_hydroecoregions;")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #Territoire <- select.list(sort(HER$hyher_her_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
      Territoire <- tcltk::tk_select.list(sort(HER$hyher_her_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    }
    Sortie <- HER %>% mutate(nometcode = paste0(hyher_her_libelle, " - ", hyher_her_libelle)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### GEMAPI ####
  if(Echelle == "GEMAPI"){
    stop("Traitement des entités GEMAPI à développer")
    # dbD <- BDD.ouverture("Data")
    # communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    # DBI::dbDisconnect(dbD)
    # if(Liste == F){
    #   if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    # }
    # if(Liste == T){
    #   #Territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
    #   Territoire <- tcltk::tk_select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    # }
    # Sortie <- communes %>% mutate(nometcode = paste0(tpcomm_commune_libelle, " - ", tpcomm_commune_insee)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    # if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### MO ####
  if(Echelle == "MO"){
    stop("Traitement des MO à développer")
    # dbD <- BDD.ouverture("Data")
    # communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    # DBI::dbDisconnect(dbD)
    # if(Liste == F){
    #   if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    # }
    # if(Liste == T){
    #   #Territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
    #   Territoire <- tcltk::tk_select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    # }
    # Sortie <- communes %>% mutate(nometcode = paste0(tpcomm_commune_libelle, " - ", tpcomm_commune_insee)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    # if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Milieu ####
  if(Echelle == "Milieu"){
    warning("Cours d'eau à développer")
    dbD <- BDD.ouverture("Data")
    plansdeau <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_plansdeau;")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #Territoire <- select.list(sort(communes$tpcomm_commune_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
      Territoire <- tcltk::tk_select.list(sort(plansdeau$hypldo_libelle[!is.na(plansdeau$hypldo_libelle)]), multiple = T) # Pour avoir un menu qui s'ouvre
    }
    Sortie <- plansdeau %>% mutate(nometcode = paste0(hypldo_libelle, " - ", hypldo_libelle)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Bassin ####
  if(Echelle == "Bassin"){
    dbD <- BDD.ouverture("Data")
    BV <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_bvthema;")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #Territoire <- select.list(sort(BV$hybvth_bassin_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
      Territoire <- tcltk::tk_select.list(sort(BV$hybvth_bassin_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    }
    Sortie <- BV %>% mutate(nometcode = paste0(hybvth_bassin_libelle, " - ", hybvth_sousbassin_libelle)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Sous-bassin ####
  if(Echelle == "Sous-bassin"){
    dbD <- BDD.ouverture("Data")
    BV <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_bvthema;")
    DBI::dbDisconnect(dbD)
    if(Liste == F){
      if(class(Territoire) != "character"){stop("Liste de territoires pas au bon format")}
    }
    if(Liste == T){
      #Territoire <- select.list(sort(BV$hybvth_sousbassin_libelle), multiple = T) # Liste directement dans R, avec sélection par numéro
      Territoire <- tcltk::tk_select.list(sort(BV$hybvth_sousbassin_libelle), multiple = T) # Pour avoir un menu qui s'ouvre
    }
    Sortie <- BV %>% mutate(nometcode = paste0(hybvth_sousbassin_libelle, " - ", hybvth_sousbassin_libelle)) %>% filter(grepl(paste(Territoire,collapse="|"), nometcode)) %>% select(-nometcode)
    if(nrow(Sortie) == 0){warning("Aucun résultat")}
  }
  
  #### Polygone ####
  if(Echelle == "Polygone"){
    # Localisation du fichier #
    Sortie <- sf::st_read(adresse.switch(Territoire))
  }
  
  ## Sortie des résultats ##
  return(Sortie)
  
} # Fin de la fonction