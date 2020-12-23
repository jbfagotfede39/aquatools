#' Recode note protocole tronçon
#'
#' Cette fonction permet de recoder les notes brutes du protocole tronçon en classes de qualité
#' 
#' @name troncon.recode
#' @param data Chronique à valider
#' @keywords data
#' @import tidyverse
#' @export
#' @examples
#' troncon.recode(data)

###### À faire #####
# 
####################

troncon.recode <- function(data)
{

  # Renommage des colonnes #
  if("score2" %in% names(data)){
    data <-
      data %>%
      dplyr::rename(ScoreNote = score2)}
    
  if("heterogen2" %in% names(data)){
    data <-
      data %>%
      dplyr::rename(HeterogeneiteNote = heterogen2)}

  if("attractiv2" %in% names(data)){
    data <-
      data %>%
      dplyr::rename(AttractiviteNote = attractiv2)}
  
  if("connectiv2" %in% names(data)){
    data <-
      data %>%
      dplyr::rename(ConnectiviteNote = connectiv2)}
  
  if("stab2" %in% names(data)){
    data <-
      data %>%
      dplyr::rename(StabiliteNote = stab2)}
  
  # Ré-encodage #
  if(class(data$ScoreNote) == "factor") data$ScoreNote <- as.numeric(levels(data$ScoreNote))[data$ScoreNote]
  if(class(data$HeterogeneiteNote) == "factor") data$HeterogeneiteNote <- as.numeric(levels(data$HeterogeneiteNote))[data$HeterogeneiteNote]
  if(class(data$AttractiviteNote) == "factor") data$AttractiviteNote <- as.numeric(levels(data$AttractiviteNote))[data$AttractiviteNote]
  if(class(data$ConnectiviteNote) == "factor") data$ConnectiviteNote <- as.numeric(levels(data$ConnectiviteNote))[data$ConnectiviteNote]
  if(class(data$StabiliteNote) == "factor") data$StabiliteNote <- as.numeric(levels(data$StabiliteNote))[data$StabiliteNote]
  
  # Attribution des classes #
  databis <-
    data %>% 
    mutate(ScoreClasse = case_when(.$ScoreNote >= 6500 ~ "A",
                                   .$ScoreNote < 6500 & .$ScoreNote >= 3500  ~ "B",
                                   .$ScoreNote < 3500 & .$ScoreNote >= 1500  ~ "C",
                                   .$ScoreNote < 1500 & .$ScoreNote >= 400  ~ "D",
                                   .$ScoreNote < 400 ~ "E")
    ) %>% 
    mutate(HeterogeneiteClasse = case_when(.$HeterogeneiteNote >= 50 ~ "A",
                                .$HeterogeneiteNote < 50 & .$HeterogeneiteNote >= 40  ~ "B",
                                .$HeterogeneiteNote < 40 & .$HeterogeneiteNote >= 28  ~ "C",
                                .$HeterogeneiteNote < 28 & .$HeterogeneiteNote >= 14  ~ "D",
                                .$HeterogeneiteNote < 14 ~ "E")
    ) %>% 
    mutate(AttractiviteClasse = case_when(.$AttractiviteNote >= 45 ~ "A",
                                           .$AttractiviteNote < 45 & .$AttractiviteNote >= 34  ~ "B",
                                           .$AttractiviteNote < 34 & .$AttractiviteNote >= 23  ~ "C",
                                           .$AttractiviteNote < 23 & .$AttractiviteNote >= 11  ~ "D",
                                           .$AttractiviteNote < 11 ~ "E")
    ) %>% 
    mutate(ConnectiviteClasse = case_when(.$ConnectiviteNote >= 65 ~ "A",
                                          .$ConnectiviteNote < 65 & .$ConnectiviteNote >= 49  ~ "B",
                                          .$ConnectiviteNote < 49 & .$ConnectiviteNote >= 33  ~ "C",
                                          .$ConnectiviteNote < 33 & .$ConnectiviteNote >= 16  ~ "D",
                                          .$ConnectiviteNote < 16 ~ "E")
    ) %>% 
    mutate(StabiliteClasse = case_when(.$StabiliteNote >= 10 ~ "A",
                                          .$StabiliteNote < 10 & .$StabiliteNote >= -10  ~ "B",
                                          .$StabiliteNote < -10 & .$StabiliteNote >= -25  ~ "C",
                                          .$StabiliteNote < -25 & .$StabiliteNote >= -60  ~ "D")
    )

  return(databis)
  
} # Fin de la fonction