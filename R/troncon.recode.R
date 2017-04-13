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
      rename(HeterogeneiteNote = heterogen2)}

  if("attractiv2" %in% names(troncon)){
    troncon <-
      troncon %>%
      rename(AttractiviteNote = attractiv2)}
  
  if("connectiv2" %in% names(troncon)){
    troncon <-
      troncon %>%
      rename(ConnectiviteNote = connectiv2)}
  
  if("stab2" %in% names(troncon)){
    troncon <-
      troncon %>%
      rename(StabiliteNote = stab2)}
  
  # Ré-encodage #
  if(class(troncon$ScoreNote) == factor) troncon$ScoreNote <- as.numeric(levels(troncon$ScoreNote))[troncon$ScoreNote]
  if(class(troncon$HeterogeneiteNote) == factor) troncon$HeterogeneiteNote <- as.numeric(levels(troncon$HeterogeneiteNote))[troncon$HeterogeneiteNote]
  if(class(troncon$AttractiviteNote) == factor) troncon$AttractiviteNote <- as.numeric(levels(troncon$AttractiviteNote))[troncon$AttractiviteNote]
  if(class(troncon$ConnectiviteNote) == factor) troncon$ConnectiviteNote <- as.numeric(levels(troncon$ConnectiviteNote))[troncon$ConnectiviteNote]
  if(class(troncon$StabiliteNote) == factor) troncon$StabiliteNote <- as.numeric(levels(troncon$StabiliteNote))[troncon$StabiliteNote]
  
  # Attribution des classes #
  tronconbis <-
    troncon %>% 
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
}