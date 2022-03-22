#' Reformatage de dates en année neutres
#'
#' Reformate les dates selon une année neutre/arbitraire
#' @name formatage.annee.neutre
#' @param data Jeu de données contenant une colonne chmes_date ou chsvi_date au format Date
#' @param datedebutanneebiol Date de démarrage de l'année biologique : 10-01 (par défaut - 1er octobre)
#' @keywords donnees
#' @import tidyverse
#' @export
#' @examples
#' formatage.annee.neutre(data)

formatage.annee.neutre <- function(
  data = data,
  datedebutanneebiol = "10-01")
  {
  
  #### Calculs ####
  data <- 
    data %>% 
    mutate(chmes_date_anneeneutre = ifelse(str_sub(chmes_date, 6, 10) >= datedebutanneebiol, format(chmes_date, "2000-%m-%d"), format(chmes_date, "2001-%m-%d"))) %>% # Attention de bien se caler sur une année bisextile, sinon on peut générer des "2001-02-29" qui n'existent pas, et qui posent donc des problèmes de parsing avec ymd et des warnings et des NA
    filter(chmes_date_anneeneutre != "2001-02-29") %>% # On ne peut pas faire autrement, car si on met 2000 et 2004 par exemple, alors on génère des vides dans les figures utilisées ensuite : les deux années doivent se suivre, donc il y aura forcément des valeurs impossibles sur la première ou bien sur la deuxième année. On est donc obligé de filtrer d'un côté. Ces données seraient de toute manière supprimées du fait du bug de parsing qui générerait alors des NA qui ne seraient pas représentés.
    mutate(chmes_date_anneeneutre = ymd(chmes_date_anneeneutre))
  
  #### Sortie des données  ####
  return(data)

} # Fin de la fonction