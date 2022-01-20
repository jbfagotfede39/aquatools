#' Vues des préférendums thermiques des especes piscicoles
#'
#' Cette fonction permet de représenter les préférendums thermiques des espèces piscicoles, avec éventuellement la Tmm30j observée
#' @name chronique.figure.preferendums
#' @param staderecherche Stade de vie recherché : \code{Adulte} (par défaut), \code{Reproduction}, \code{Embryon}, \code{Larve}, \code{Juvénile} ou \code{Tous stades}
#' @param tmm30j Valeur de Tmm30j à représenter : 0 par défaut (aucun affichage)
#' @param liste_especes Liste des espèces à afficher : toutes par défaut. Les espèces sont affichées dans l'ordre de saisie.
#' @param titre Titre du graphique (vide par défaut)
#' @param save Si \code{FALSE} (par défault), n'enregistre pas les figures. Si \code{TRUE}, les enregistre.
#' @param projet Nom du projet
#' @param format Défini le format d'enregistrement (par défaut .png)
#' @keywords chronique
#' @keywords poissons
#' @export
#' @import cli
#' @import glue
#' @import tidyverse
#' @examples
#' chronique.figure.preferendums()
#' chronique.figure.preferendums("Reproduction")
#' chronique.figure.preferendums("Tous stades", tmm30j = 17.4)
#' chronique.figure.preferendums("Toutes espèces", tmm30j = 17.4)
#' chronique.figure.preferendums("Adulte", tmm30j = 17.4, "TRF,CHA,LOG,VAL")
#' chronique.figure.preferendums("Adulte", tmm30j = 17.4, liste_especes)
#' chronique.figure.preferendums("Adulte", tmm30j = 17.4, .$chsta_sprep)

chronique.figure.preferendums <- function(
  staderecherche = c("Adulte", "Reproduction", "Embryon", "Larve", "Juvénile", "Tous stades"),
  tmm30j = 0,
  liste_especes = c("Toutes espèces", "TRF", "CHA", "LPP", "VAI", "LOF", "OBR", "EPI", "BLE", "BLN", "CHE", "APR", "GOU", "HOT", "TOX", "BAF", "LOT", "SPI", "VAN", "EPT", "BOU", "BRO", "PER", "GAR", "ABL", "CAS", "GRE", "CCO", "SAN", "TAN", "BRB", "BRE", "PES", "ROT", "BBG", "PCH", "SIL"),
  titre="",
  save=F,
  projet = NA_character_,
  format=".png")
{
  
  #### Récupération des données complètes ####
  data(poissons_thermie_preferendums)
  
  #### Nettoyage de la liste des espèces ####
  liste_especes_propre <-
    liste_especes %>% 
    str_split(",") %>% # Séparation des termes à chaque fois qu'il y a une virgule
    purrr::pluck(1) %>% # On extrait l'ensemble de la liste
    str_trim(side = "both") # On supprime les espaces avant ou après les termes
  liste_especes_mis_en_forme <-
    liste_especes %>% 
    str_replace_all(",", "-")
  
  #### Évaluation des choix ####
  staderecherche <- match.arg(staderecherche)

  if(!("Toutes espèces" %in% liste_especes_propre)){
    liste_especes_fausses <- 
      liste_especes_propre %>% 
      as_tibble() %>% 
      filter(!(value %in% poissons_thermie_preferendums$codeespece)) %>% 
      pull()
    
    ## Retour de la réponse ##
    if(length(liste_especes_fausses) == 1) message <- glue("{col_red('Attention')} : l'espèce {liste_especes_fausses} ne figure pas dans la base de référence")
    if(length(liste_especes_fausses) > 1) message <- glue("{col_red('Attention')} : les espèces {glue_collapse(liste_especes_fausses, ', ', last = ' et ')} ne figurent pas dans la base de référence")
    
    ## Affichage ##
    cli_li(
      c(message)
    )
  }
  
  #### Affichage ####
  gg <- ggplot(poissons_thermie_preferendums %>% 
                 {if (!("Toutes espèces" %in% liste_especes_propre)) filter(., codeespece %in% liste_especes_propre) else .} %>% # filtrage des espèces s'il y a lieu
                 {if (staderecherche != "Tous stades") filter(., stade == staderecherche) else .} %>% # filtrage du stade s'il y a lieu
                 dplyr::select(codeespece, stade, seuil, value) %>% 
                 pivot_wider(names_from = seuil, values_from = value) %>% 
                 {if (!("Optimal_max" %in% names(.))) mutate(., Optimal_max = NA) else .} %>% # Création du champ Optimal_max si absent
                 {if (!("Resistance_max" %in% names(.))) mutate(., Resistance_max = NA) else .} %>% # Création du champ Resistance_max si absent - Pour le cas de BRB, HOT et TOX car absence de valeur de résistance max
                 mutate(Resistance_min = ifelse(!is.na(Resistance_max) & stade != "Reproduction", Optimal_max, NA)) %>% 
                 mutate(Optimal_max = ifelse(is.na(Optimal_max) & stade == "Reproduction", Resistance_max, Optimal_max)) %>% 
                 pivot_longer(cols = Optimal_min:Resistance_min, names_to = "seuil", values_to = "value") %>% 
                 mutate(classe = ifelse(grepl("Optimal", seuil), "Gamme optimale", NA_character_)) %>% 
                 mutate(classe = ifelse(grepl("Resistance", seuil), "Zone de résistance", classe)) %>% 
                 mutate(niveau = ifelse(grepl("min", seuil), "min", NA_character_)) %>% 
                 mutate(niveau = ifelse(grepl("max", seuil), "max", niveau)) %>% 
                 dplyr::select(codeespece, stade, classe, niveau, value) %>% 
                 pivot_wider(names_from = niveau, values_from = value) %>% 
                 filter(!is.na(min)) %>% # Pour le cas de BRB, HOT et TOX car absence de valeur de résistance max et génère des NA donc un avertissement
                 filter(!is.na(max)),
               aes(factor(codeespece, listeSp), colour = classe)) 
  gg <- gg + geom_linerange(aes(ymin = min, ymax = max), size=3)
  if(tmm30j != 0) gg <- gg + geom_hline(yintercept = tmm30j, color='#009fee', size = 1.5)
  if(tmm30j != 0 & length(liste_especes_propre) < 5 & !("Toutes espèces" %in% liste_especes_propre)) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 0.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  if(tmm30j != 0 & length(liste_especes_propre) >= 5 & length(liste_especes_propre) < 8) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 1.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  if(tmm30j != 0 & (length(liste_especes_propre) >= 8 | "Toutes espèces" %in% liste_especes_propre)) gg <- gg + geom_text(aes(label = "Tmm30j"), x = 2.5, y = tmm30j + 0.5, size = 3, colour = "#009fee")
  gg <- gg + scale_color_manual(values=c("Gamme optimale" = "orange", "Zone de résistance" = "red"))
  gg <- gg + labs(x = "Espèce", y = "Température (°C)", colour = "Confort piscicole", subtitle = glue("Stade de vie : {str_to_lower(staderecherche)}"), caption = "Source des données : Souchon and Tissot, 2012 & Elliott, 1981 & Elliott, 1994 & Dorts et al., 2012")
  if(nchar(titre) != 0) gg <- gg + labs(title = titre)
  gg <- gg + theme_bw()
  if((length(liste_especes_propre) >= 8 | "Toutes espèces" %in% liste_especes_propre)) gg <- gg + theme(axis.text.x = element_text(size = 6)) # base_size = 11 pour theme_bw
  if(staderecherche == "Tous stades") gg <- gg + facet_wrap(~ stade) # pour voir l'ensemble des stades
  gg
  
  #### Affichage des résultats ####
  if(save==T){
    ggsave(file=glue('{projet}/Sorties/Vues/Preferendums_biologiques/Preferendums_biologiques_{titre}_{liste_especes_mis_en_forme}_{staderecherche}{format}'))
  }
  if(save==F){return(gg)}

} # Fin de la fonction
