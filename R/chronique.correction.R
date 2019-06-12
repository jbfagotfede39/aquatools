#' Correction de données brutes de chronique
#'
#' Permet de corriger des données brutes de chronique présentant des valeurs aberrantes. ATTENTION : ne gère pas les ID
#' @name chronique.correction
#' @param data Chronique à modifier
#' @param cCodeRDT
#' @param cCapteur
#' @param cDate
#' @param cHeure
#' @param cValeur
#' @param cTypeMesure
#' @param cNvelleValeur Valeur de remplacement
#' @keywords chronique
#' @import DBI dplyr lubridate RSQLite stringr
#' @export
#' @examples
#' chronique.correction(data, cCodeRDT = "BONbaro", cCapteur = "P0352", cDate = "2014-12-27", cHeure = "19:00:00", cValeur = 985.3, cTypeMesure = "Piézométrie", cNvelleValeur = 700)

chronique.correction <- function(data, cCodeRDT = "BONbaro", cCapteur = "P0352", cDate = "2014-12-27", cHeure = "19:00:00", cValeur = 985.3, cTypeMesure = "Piézométrie", cNvelleValeur = 700)
{

  #cCodeRDT = "BONbaro"; cCapteur = "P5353"; cDate = "2015-01-05"; cHeure = "23:00:00"; cValeur = 1013.0; cTypeMesure = "Piézométrie"; cNvelleValeur = 700

  cDate <- ymd(cDate)
  data$chmes_date <- ymd(data$chmes_date)

  ##### Recherche de la ligne fausse #####
  # LigneFausse <-
  # data %>%
  # filter(chmes_coderhj == cCodeRDT) %>%
  # filter(chmes_capteur == cCapteur) %>%
  # filter(chmes_date == cDate) %>%
  # filter(chmes_heure == cHeure) %>%
  # filter(chmes_typemesure == cTypeMesure) %>%
  # filter(chmes_valeur == cValeur)
    
  LigneFausse <-
    data %>% 
    filter(chmes_coderhj == cCodeRDT & chmes_capteur == cCapteur & chmes_date == cDate & chmes_heure == cHeure & chmes_valeur == cValeur & chmes_typemesure == cTypeMesure)
    
  if(dim(LigneFausse)[1] == 0) stop("Pas de ligne correspondante")
  if(dim(LigneFausse)[1] >1) stop("Plusieurs lignes correspondantes")
  if(dim(LigneFausse)[1] == 1 && LigneFausse$chmes_validation == "Rejeté") stop("Une ligne correspondante mais déjà rejetée")
  
  ##### Suppression de la ligne fausse #####
  data <-
    anti_join(data, LigneFausse)
  
  ##### Écritures des lignes juste et fausse/corrigée #####
  LigneJuste <- LigneFausse
  LigneJuste[1,6] <- cNvelleValeur
  LigneJuste[1,10] <- "Estimé"
  
  LigneFausse[1,9] <- "Rejeté"
  
  ##### Remise en commun des morceaux #####
  data <-
    dplyr::union(data, LigneJuste, by = c("id", "chmes_coderhj", "chmes_capteur", "chmes_date", "chmes_heure", "chmes_valeur", "chmes_unite", "chmes_typemesure", "chmes_validation", "chmes_mode_acquisition", "chmes_mode_integration", "_modif_utilisateur", "_modif_type", "_modif_date"))
  data <-
    dplyr::union(data, LigneFausse, by = c("id", "chmes_coderhj", "chmes_capteur", "chmes_date", "chmes_heure", "chmes_valeur", "chmes_unite", "chmes_typemesure", "chmes_validation", "chmes_mode_acquisition", "chmes_mode_integration", "_modif_utilisateur", "_modif_type", "_modif_date"))

  ##### Rangement dans l'ordre chronologique #####
  data <-
    data %>% 
    arrange(chmes_date, chmes_heure)
  
  ##### Remise en commun des morceaux #####
  return(data)
  
} # Fin de la fonction