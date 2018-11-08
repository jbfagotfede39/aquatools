#' Calcul et regroupement des CAR par espèce par station
#'
#' This function allows you to express your love of cats.
#' @name poissons_synthese_CAR
#' @keywords cats
#' @export
#' @examples
#' poissons_synthese_CAR()


poissons_synthese_CAR <- function(poissons){


##### Calcul du minimum entre CAP et CAN pour obtenir CAR #####
poissons$CAR <- apply(poissons[,(ncol(poissons)-1):ncol(poissons)], 1, min)

##### Extraction des CAR #####
poissonsCAR <- select(poissons,
                    Station,Date,ESPECE,CAR)
poissonsCAR <- filter(poissonsCAR, ESPECE != "TOTAL") # inutile car on pourrait les recalculer ensuite à la demande

##### Création d'une matrice des CAR par espèce par station #####

melt(poissonsCAR, id.vars=c("Station","Date","ESPECE")) %>%
  dcast(Station + Date ~ ESPECE)

##### Calcul du nombe d'espèce par station #####
matriceCAR <- poissonsCAR %>% 
  group_by(Station) %>% 
  summarise( ESPECE = n()) %>%
  inner_join(matriceCAR, by='Station') # Permet de coller le résultat dans le tableau global

##### Réarranger l'ordre des espèces ##### (à améliorer avec une liste complète)
#matriceCAR<-select(matriceCAR, Station, Date, ESPECE, CHA, TRF, VAI, LOF, BLN, CHE, GOU) # Chercher comment on peut automatiser ça à partir de la liste complète des espèces

##### Exportation du résultat #####

write.xlsx(x = matriceCAR, file = "matriceCAR.xlsx",
           sheetName = "FD2014", row.names = T)
}