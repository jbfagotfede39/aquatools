#' Paramétrisation des vues de physico-chimie
#'
#' Cette fonction permet de sélectionner automatiquement les paramètres de légende des représentations graphiques de physico-chimie à partir des données issues de PC.contexte
#' @name PC.figure.parametres
#' @param data Data.frame issu de la fonction PC.contexte()
#' @param typefigure Type de figure qui est à paramétriser : \code{valeurs} (par défault)
#' @keywords physico-chimie
#' @import glue
#' @import stringr
#' @import tidyverse
#' @export
#' @examples
#' data %>% PC.contexte() %>% PC.figure.parametres()
#' 
#' parametres <- data %>% PC.contexte() %>% PC.figure.parametres()
#' legende_y <- parametres$legende_y
#' legende_titre <- parametres$legende_titre
#' typemesure_titre_sortie <- parametres$typemesure_titre_sortie

PC.figure.parametres <- function(
  data = data,
  typefigure = c("valeurs")
)
{
  
  #### Évaluation des choix ####
  typefigure <- match.arg(typefigure)
  
  #### Contexte des données ####
  contextereference <- structure(list(n_station = integer(0), n_typemesure = integer(0), 
                                      n_annee = integer(0), n_unitenom = integer(0), n_milieu = numeric(0), 
                                      station = structure(character(0), class = c("glue", "character"
                                      )), typemesure = structure(character(0), class = c("glue", 
                                                                                         "character")), annee = structure(character(0), class = c("glue", 
                                                                                                                                                  "character")), unitenom = structure(character(0), class = c("glue", 
                                                                                                                                                                                                              "character")), milieu = structure(character(0), class = c("glue", 
                                                                                                                                                                                                                                                                        "character"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                                                                                                                                                                          "tbl", "data.frame"))
  
  #### Tests ####
  if(any(names(data) == names(contextereference)) == FALSE) stop("Les données d'entrée ne sont pas au format de sortie de PC.contexte")
  if(data$n_typemesure != 1) stop("Plusieurs pcmes_parametrenom au sein du jeu de données")

  #### Création des objets vides pour tests ultérieurs ####
  legende_y <- NA_character_
  legende_titre <- NA_character_
  typemesure_titre_sortie <- NA_character_
  
  #### Ajustement des paramètres en fonction du typemesure ####
  
  ### Traitement général ###
  typemesure <- data$typemesure
  typemesure_minuscule <- str_to_lower(typemesure)
  legende_y <- data$unitenom
  legende_titre <- typemesure
  typemesure_titre_sortie <- glue("_{typemesure_minuscule}_")
  
  ### Adaptations cas particuliers ###
  typemesure <- data$typemesure
  if(typemesure == "Chlorophylle"){
    if(typefigure == "valeurs"){
      legende_y <- expression(Chlorophylles~(paste(mu,g/L)))
      legende_titre <- expression(Chlorophylles~(paste(mu,g/L)))
      typemesure_titre_sortie <- "_chlorophylle_"
    }
  }
  
  if(typemesure == "Conductivité à 25°C"){
    if(typefigure == "valeurs"){
      legende_y <- expression(Conductivite~corrigee~à~25~degree*C~(paste(mu,S)))
      legende_titre <- expression(Conductivite~corrigee~à~25~degree*C~(paste(mu,S)))
      typemesure_titre_sortie <- "_conductivite_"
    }
  }  
  
  if(typemesure == "Nitrates"){
    if(typefigure == "valeurs"){
      legende_y <- expression(Nitrates~(mg~NO[3]/L))
      legende_titre <- expression(Nitrates~(mg~NO[3]/L))
      typemesure_titre_sortie <- "_nitrates_"
    }
  }
  
  if(typemesure == "Nitrites"){
    if(typefigure == "valeurs"){
      legende_y <- expression(Nitrites~(mg~NO[2]/L))
      legende_titre <- expression(Nitrites~(mg~NO[2]/L))
      typemesure_titre_sortie <- "_nitrites_"
    }
  }
  
  if(typemesure == "Oxygène dissous"){
    if(typefigure == "valeurs"){
      legende_y <- expression(Oxygène~dissous~(mg~O[2]/L))
      legende_titre <- expression(Oxygène~dissous~(mg~O[2]/L))
      typemesure_titre_sortie <- "_oxygénation-conc_"
    }
  }
  
  if(typemesure == "Oxygène dissous (saturation)"){
    if(typefigure == "valeurs"){
      legende_y <- expression(Oxygène~dissous~("%"~saturation~O[2]/L))
      legende_titre <- expression(Oxygène~dissous~("%"~saturation~O[2]/L))
      typemesure_titre_sortie <- "_oxygénation-sat_"
    }
  }
  
  if(typemesure == "pH"){
    if(typefigure == "valeurs"){
      legende_y <- expression(pH~(unite~pH))
      legende_titre <- expression(pH~(unite~pH))
      typemesure_titre_sortie <- "_pH_"
    }
  }
  
  if(typemesure == "Phycocyanine"){
    if(typefigure == "valeurs"){
      legende_y <- expression(Phycocyanines~(paste(mu,g/L)))
      legende_titre <- expression(Phycocyanines~(paste(mu,g/L)))
      typemesure_titre_sortie <- "_phycocyanine_"
    }
  }
  
  if(typemesure == "Potentiel d’oxydo-réduction"){
    if(typefigure == "valeurs"){
      legende_y <- expression(paste("Potentiel d'oxydo-réduction (mV)"))
      legende_titre <- expression(paste("Potentiel d'oxydo-réduction (mV)"))
      typemesure_titre_sortie <- "_redox_"
    }
  }

  if(typemesure == "Température"){
    if(typefigure == "valeurs"){
      legende_y <- expression(Temperature~(degree*C))
      legende_titre <- expression(Temperature~(degree*C))
      typemesure_titre_sortie <- "_thermie_"
    }
  }
  
  #### Tests ####
  if(is.na(as.character(legende_y))) stop("Paramètres de titres de figure à créer")
  if(is.na(as.character(legende_titre))) stop("Paramètres de titres de figure à créer")
  if(is.na(typemesure_titre_sortie)) stop("Paramètres de titres de figure à créer")
  
  #### Regroupement des valeurs ####
  parametres <- NULL
  parametres$legende_y <- legende_y
  parametres$legende_titre <- legende_titre
  parametres$typemesure_titre_sortie <- typemesure_titre_sortie
  
  #### Affichage des résultats ####
  return(parametres)

} # Fin de la fonction
