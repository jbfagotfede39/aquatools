#' Chroniques existantes
#'
#' Cette fonction permet de visualiser les chroniques existantes et leur statut de validation
#' @name chronique.duree
#' @param data Jeu de données issu de la matrice "Mesures" des chroniques
#' @param tri Tri des stations (\code{alphabétique} par défaut, \code{temporel})
#' @keywords chronique
#' @import tidyverse
#' @export
#' @examples
#' chronique.duree(data)
#' chronique.duree(data) + guides(y = guide_axis(n.dodge = 2))
#' data %>% chronique.duree(tri = "temporel") + guides(y = guide_axis(n.dodge = 3)) + theme(legend.position = "none")

chronique.duree <- function(
    data,
    tri = c("alphabétique", "temporel")
    )
  
{
  
  #### Nettoyage & reformatage ####
  tri <- match.arg(tri)
  
  #### Nettoyage & reformatage ####
  data_v2 <-
    data %>% 
    mutate(chmes_date = ymd(chmes_date))

  #### Représentation ####
if(tri == "alphabétique") gg <- ggplot(data_v2 %>% arrange(desc(chmes_coderhj)), aes(x = chmes_date, y = as_factor(chmes_coderhj), color = chmes_validation))
if(tri == "temporel") gg <- ggplot(data_v2, aes(x = chmes_date, y = fct_reorder(chmes_coderhj, chmes_date), color = chmes_validation))
gg <- gg + geom_line()
gg <- gg + labs(y = "Station", x = "Date", colour = "Validation") # Pour changer le titre
gg <- gg + scale_color_manual(values = c("Validé" = "#476F84FF", "À valider" = "#72874EFF", "Rejeté" = "#ED8B00FF"))
gg <- gg + theme_minimal()
gg

  #### Sortie ####
  return(gg)
}