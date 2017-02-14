#' Recode note protocole tronçon
#'
#' Cette fonction permet de recoder les notes brutes du protocole tronçon en classes de qualité
#' 
#' @param data Chronique à valider
#' @keywords data
#' @import dplyr 
#' @export
#' @examples
#' troncon.recode(data)

###### À faire #####
# 
####################

troncon.recode <- function(troncon = data)
{

  # Renommage des colonnes #
  if("score2" %in% names(troncon)){
    troncon <-
      troncon %>%
      rename(ScoreNote = score2)}
    
  if("heterogen2" %in% names(troncon)){
    troncon <-
      troncon %>%
      rename(HétérogénéitéNote = heterogen2)}

  if("attractiv2" %in% names(troncon)){
    troncon <-
      troncon %>%
      rename(AttractivitéNote = attractiv2)}
  
  if("connectiv2" %in% names(troncon)){
    troncon <-
      troncon %>%
      rename(ConnectivitéNote = connectiv2)}
  
  if("stab2" %in% names(troncon)){
    troncon <-
      troncon %>%
      rename(StabilitéNote = stab2)}
  
  # Attribution des classes #
  tronconbis <-
    troncon %>% 
    mutate(ScoreClasse = case_when(.$ScoreNote >= 6500 ~ "A",
                                   .$ScoreNote < 6500 & .$ScoreNote >= 3500  ~ "B",
                                   .$ScoreNote < 3500 & .$ScoreNote >= 1500  ~ "C",
                                   .$ScoreNote < 1500 & .$ScoreNote >= 400  ~ "D",
                                   .$ScoreNote < 400 ~ "E")
    ) %>% 
    mutate(HétérogénéitéClasse = case_when(.$HétérogénéitéNote >= 50 ~ "A",
                                .$HétérogénéitéNote < 50 & .$HétérogénéitéNote >= 40  ~ "B",
                                .$HétérogénéitéNote < 40 & .$HétérogénéitéNote >= 28  ~ "C",
                                .$HétérogénéitéNote < 28 & .$HétérogénéitéNote >= 14  ~ "D",
                                .$HétérogénéitéNote < 14 ~ "E")
    ) %>% 
    mutate(AttractivitéClasse = case_when(.$AttractivitéNote >= 45 ~ "A",
                                           .$AttractivitéNote < 45 & .$AttractivitéNote >= 34  ~ "B",
                                           .$AttractivitéNote < 34 & .$AttractivitéNote >= 23  ~ "C",
                                           .$AttractivitéNote < 23 & .$AttractivitéNote >= 11  ~ "D",
                                           .$AttractivitéNote < 11 ~ "E")
    ) %>% 
    mutate(ConnectivitéClasse = case_when(.$ConnectivitéNote >= 65 ~ "A",
                                          .$ConnectivitéNote < 65 & .$ConnectivitéNote >= 49  ~ "B",
                                          .$ConnectivitéNote < 49 & .$ConnectivitéNote >= 33  ~ "C",
                                          .$ConnectivitéNote < 33 & .$ConnectivitéNote >= 16  ~ "D",
                                          .$ConnectivitéNote < 16 ~ "E")
    ) %>% 
    mutate(StabilitéClasse = case_when(.$StabilitéNote >= 10 ~ "A",
                                          .$StabilitéNote < 10 & .$StabilitéNote >= -10  ~ "B",
                                          .$StabilitéNote < -10 & .$StabilitéNote >= -25  ~ "C",
                                          .$StabilitéNote < -25 & .$StabilitéNote >= -60  ~ "D")
    )
}