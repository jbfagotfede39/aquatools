#' Vues des préférendums thermiques des especes piscicoles
#'
#' Cette fonction permet de représenter les préférendums thermiques des espèces piscicoles, avec éventuellement la Tmm30j observée
#' @name chronique.figure.preferendums
#' @param staderecherche Stade de vie recherché : \code{Adulte} (par défaut), \code{Reproduction}, \code{Embryon}, \code{Larve}, \code{Juvénile} ou \code{Tous stades}
#' @param tmm30j Valeur de Tmm30j à représenter : 0 par défaut (aucun affichage)
#' @param listeEspeces Liste des espèces à afficher : toutes par défaut
#' @keywords chronique
#' @keywords poissons
#' @export
#' @import glue
#' @import stringr
#' @import tidyverse
#' @examples
#' chronique.figure.preferendums()
#' chronique.figure.preferendums("Reproduction")
#' chronique.figure.preferendums("Tous stades", tmm30j = 17.4)
#' chronique.figure.preferendums("Adulte", tmm30j = 17.4, c("TRF", "CHA", "VAN", "TOX"))


##### TODO LIST #####
# 
#####################

chronique.figure.preferendums <- function(
  staderecherche = c("Adulte", "Reproduction", "Embryon", "Larve", "Juvénile", "Tous stades"),
  tmm30j = 0,
  listeEspeces = c("BAF", "GRE", "PER", "BLN", "BRO", "ABL", "VAN", "TOX", "HOT", "PES", "CHE", "GAR", "SIL", "SAN", "GOU", "BOU", "SPI", "BRB", "BRE")
)
{
  
  #### Évaluation des choix ####
  staderecherche <- match.arg(staderecherche)
  
  #### Récupération des données complètes ####
  data(PoissonsPreferendumsThermiques)
  
  #### Affichage ####
  gg <- ggplot(PoissonsPreferundumsThermiques %>% 
                 filter(codeespece %in% listeEspeces) %>% # filtrage des espèces s'il y a lieu
                 {if (staderecherche != "Tous stades") filter(., stade == staderecherche) else .} %>% # filtrage du stade s'il y a lieu
                 select(codeespece, stade, seuil, value) %>% 
                 pivot_wider(names_from = seuil, values_from = value) %>% 
                 mutate(Optimal_max = ifelse("Optimal_max" %in% names(.), Optimal_max, NA)) %>% 
                 mutate(Resistance_min = ifelse(!is.na(Resistance_max) & stade != "Reproduction", Optimal_max, NA)) %>% 
                 mutate(Optimal_max = ifelse(is.na(Optimal_max) & stade == "Reproduction", Resistance_max, Optimal_max)) %>% 
                 pivot_longer(cols = Optimal_min:Resistance_min, names_to = "seuil", values_to = "value") %>% 
                 mutate(classe = ifelse(grepl("Optimal", seuil), "Gamme optimale", NA_character_)) %>% 
                 mutate(classe = ifelse(grepl("Resistance", seuil), "Zone de résistance", classe)) %>% 
                 mutate(niveau = ifelse(grepl("min", seuil), "min", NA_character_)) %>% 
                 mutate(niveau = ifelse(grepl("max", seuil), "max", niveau)) %>% 
                 select(codeespece, stade, classe, niveau, value) %>% 
                 pivot_wider(names_from = niveau, values_from = value),
               aes(factor(codeespece,listeSp), colour = classe)) 
  gg <- gg + geom_linerange(aes(ymin = min, ymax = max), size=3)
  if(tmm30j != 0) gg <- gg + geom_hline(yintercept = tmm30j, color='#009fee', size = 1.5)
  if(tmm30j != 0 & length(listeEspeces) < 5) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 0.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  if(tmm30j != 0 & length(listeEspeces) >= 5 & length(listeEspeces) < 8) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 1.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  if(tmm30j != 0 & length(listeEspeces) >= 8) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 2.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  gg <- gg + scale_color_manual(values=c("Gamme optimale" = "orange", "Zone de résistance" = "red"))
  gg <- gg + labs(x = "Espèce", y = "Température (°C)", colour = "Confort piscicole", subtitle = glue("Stade de vie : {str_to_lower(staderecherche)}"), caption = "Source des données : Souchon and Tissot, 2012")
  gg <- gg + theme_bw()
  if(staderecherche == "Tous stades") gg <- gg + facet_wrap(~ stade) # pour voir l'ensemble des stades
  gg
  
  #### Affichage des résultats ####
  return(gg)

} # Fin de la fonction
