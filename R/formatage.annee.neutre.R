#' Reformatage de dates en année neutres
#'
#' Reformate les dates selon une année neutre/arbitraire
#' @name formatage.annee.neutre
#' @param data Jeu de données contenant une colonne chmes_date ou chsvi_date au format Date
#' @param datedebutanneeneutre Date de démarrage de l'année neutre : 10-01 (par défaut - 1er octobre)
#' @keywords donnees
#' @import tibble
#' @import tidyverse
#' @export
#' @examples
#' formatage.annee.neutre(data)
#' mesures_annuelles %>% formatage.annee.neutre() %>% filter(month(chmes_date_anneeneutre) %in% c("6", "7", "8", "9")) 

formatage.annee.neutre <- function(
  data = data,
  datedebutanneeneutre = "10-01")
  {
  
  #### Calculs ####
  data <- 
    data %>% 
    mutate(chmes_date_anneeneutre = ifelse(str_sub(chmes_date, 6, 10) >= datedebutanneeneutre, format(chmes_date, "2000-%m-%d"), format(chmes_date, "2001-%m-%d"))) %>% # Attention de bien se caler sur une année bissextile, sinon on peut générer des "2001-02-29" qui n'existent pas, et qui posent donc des problèmes de parsing avec ymd et des warnings et des NA
    filter(chmes_date_anneeneutre != "2001-02-29") %>% # On ne peut pas faire autrement, car si on met 2000 et 2004 par exemple, alors on génère des vides dans les figures utilisées ensuite : les deux années doivent se suivre, donc il y aura forcément des valeurs impossibles sur la première ou bien sur la deuxième année. On est donc obligé de filtrer d'un côté. Ces données seraient de toute manière supprimées du fait du bug de parsing qui générerait alors des NA qui ne seraient pas représentés.
    mutate(chmes_date_anneeneutre = ymd(chmes_date_anneeneutre))
  
  #### Sortie des données  ####
  return(data)

} # Fin de la fonction