#' Vues des préférendums thermiques des especes piscicoles
#'
#' Cette fonction permet de représenter les préférendums thermiques des espèces piscicoles, avec éventuellement la Tmm30j observée
#' @name chronique.figure.preferendums
#' @param staderecherche Stade de vie recherché : \code{Adulte} (par défaut), \code{Reproduction}, \code{Embryon}, \code{Larve}, \code{Juvénile} ou \code{Tous stades}
#' @param tmm30j Valeur de Tmm30j à représenter : 0 par défaut (aucun affichage)
#' @param listeEspeces Liste des espèces à afficher : toutes par défaut
#' @param Titre Titre du graphique (vide par défaut)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @keywords poissons
#' @export
#' @import tidyverse
#' @examples
#' chronique.figure.preferendums()
#' chronique.figure.preferendums("Reproduction")
#' chronique.figure.preferendums("Tous stades", tmm30j = 17.4)
#' chronique.figure.preferendums("Toutes espèces", tmm30j = 17.4)
#' chronique.figure.preferendums("Adulte", tmm30j = 17.4, c("TRF", "CHA", "VAN", "TOX"))

##### TODO LIST #####
# Ajouter un filtre qui ne permet d'afficher, à partir de la liste recherchée, que les espèces en présence dans la base (cas de la TRF)
# Ajouter données TRF/CHA/VAI/LOF
#####################

chronique.figure.preferendums <- function(
  Titre="",
  staderecherche = c("Adulte", "Reproduction", "Embryon", "Larve", "Juvénile", "Tous stades"),
  tmm30j = 0,
  listeEspeces = c("Toutes espèces", "BAF", "GRE", "PER", "BLN", "BRO", "ABL", "VAN", "TOX", "HOT", "PES", "CHE", "GAR", "SIL", "SAN", "GOU", "BOU", "SPI", "BRB", "BRE"),
  save=F,
  projet = NA_character_,
  format=".png")
{
  
  #### Évaluation des choix ####
  staderecherche <- match.arg(staderecherche)
  listeEspeces <- match.arg(listeEspeces)
  
  #### Récupération des données complètes ####
  data(PoissonsPreferendumsThermiques)
  
  #### Affichage ####
  gg <- ggplot(PoissonsPreferendumsThermiques %>% 
                 {if (listeEspeces != "Toutes espèces") filter(codeespece %in% listeEspeces) else .} %>% # filtrage des espèces s'il y a lieu
                 {if (staderecherche != "Tous stades") filter(., stade == staderecherche) else .} %>% # filtrage du stade s'il y a lieu
                 select(codeespece, stade, seuil, value) %>% 
                 pivot_wider(names_from = seuil, values_from = value) %>% 
                 {if (!("Optimal_max" %in% names(.))) mutate(Optimal_max = NA) else .} %>% # Création du champ Optimal_max si absent
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
  if(tmm30j != 0 & length(listeEspeces) < 5 & !("Toutes espèces" %in% listeEspeces)) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 0.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  if(tmm30j != 0 & length(listeEspeces) >= 5 & length(listeEspeces) < 8) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 1.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  if(tmm30j != 0 & (length(listeEspeces) >= 8 | "Toutes espèces" %in% listeEspeces)) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 2.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  gg <- gg + scale_color_manual(values=c("Gamme optimale" = "orange", "Zone de résistance" = "red"))
  gg <- gg + labs(x = "Espèce", y = "Température (°C)", colour = "Confort piscicole", subtitle = glue("Stade de vie : {str_to_lower(staderecherche)}"), caption = "Source des données : Souchon and Tissot, 2012")
  if(nchar(Titre) != 0) gg <- gg + labs(title = Titre)
  gg <- gg + theme_bw()
  if((length(listeEspeces) >= 8 | "Toutes espèces" %in% listeEspeces)) gg <- gg + theme(axis.text.x = element_text(size = 6)) # base_size = 11 pour theme_bw
  if(staderecherche == "Tous stades") gg <- gg + facet_wrap(~ stade) # pour voir l'ensemble des stades
  gg
  
  #### Affichage des résultats ####
  if(save==T){
    ggsave(file=glue('{projet}/Sorties/Vues/Preferendums_biologiques/Preferendums_biologiques_{Titre}_{listeEspeces}_{staderecherche}{format}'))
  }
  if(save==F){return(gg)}

} # Fin de la fonction
