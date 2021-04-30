#' Paramétrisation des vues de physico-chimie
#'
#' Cette fonction permet de sélectionner automatiquement les paramètres de légende des représentations graphiques de physico-chimie à partir des données issues de PC.contexte
#' @name PC.figure.parametres
#' @param data Data.frame issu de la fonction PC.contexte()
#' @param typefigure Type de figure qui est à paramétriser : \code{valeurs} (par défault)
#' @keywords physico-chimie
#' @export
#' @examples
#' data %>% PC.contexte() %>% PC.figure.parametres()
#' 
#' parametres <- data %>% PC.contexte() %>% PC.figure.parametres()
#' legendeY <- parametres$legendeY
#' legendeTitre <- parametres$legendeTitre
#' typemesureTitreSortie <- parametres$typemesureTitreSortie


##### TODO LIST #####
# 
#####################

PC.figure.parametres <- function(
  data = data,
  typefigure = c("valeurs")
)
{
  
  #### Évaluation des choix ####
  typefigure <- match.arg(typefigure)
  
  #### Contexte des données ####
  contextereference <- structure(list(nstation = integer(0), ntypemesure = integer(0), 
                                      nmilieu = integer(0), station = structure(character(0), class = c("glue", 
                                                                                                        "character")), typemesure = structure(character(0), class = c("glue", 
                                                                                                                                                                      "character")), milieu = structure(character(0), class = c("glue", 
                                                                                                                                                                                                                                "character"))), row.names = integer(0), class = c("tbl_df", 
                                                                                                                                                                                                                                                                                  "tbl", "data.frame"))
  
  #### Tests ####
  if(any(names(data) == names(contextereference)) == FALSE) stop("Les données d'entrée ne sont pas au format de sortie de PC.contexte")
  if(data$ntypemesure != 1) stop("Plusieurs pcmes_parametrenom au sein du jeu de données")

  #### Création des objets vides pour tests ultérieurs ####
  legendeY <- NA_character_
  legendeTitre <- NA_character_
  typemesureTitreSortie <- NA_character_
  
  #### Ajustement des paramètres en fonction du typemesure ####
  typemesure <- data$typemesure
  
  if(typemesure == "Température"){
    if(typefigure == "valeurs"){
      legendeY <- expression(Temperature~(degree*C))
      legendeTitre <- expression(Temperature~(degree*C))
      typemesureTitreSortie <- "_thermie_"
    }
  }
  
  if(typemesure == "Oxygène dissous"){
    if(typefigure == "valeurs"){
      legendeY <- expression(Oxygène~dissous~(mg~O[2]/L))
      legendeTitre <- expression(Oxygène~dissous~(mg~O[2]/L))
      typemesureTitreSortie <- "_oxygénation-conc_"
    }
  }
  
  if(typemesure == "Oxygène dissous (saturation)"){
    if(typefigure == "valeurs"){
      legendeY <- expression(Oxygène~dissous~("%"~saturation~O[2]/L))
      legendeTitre <- expression(Oxygène~dissous~("%"~saturation~O[2]/L))
      typemesureTitreSortie <- "_oxygénation-sat_"
    }
  }
  
  if(typemesure == "Conductivité à 25°C"){
    if(typefigure == "valeurs"){
      legendeY <- expression(Conductivite~corrigee~à~25~degree*C~(paste(mu,S)))
      legendeTitre <- expression(Conductivite~corrigee~à~25~degree*C~(paste(mu,S)))
      typemesureTitreSortie <- "_conductivite_"
    }
  }
  
  if(typemesure == "pH"){
    if(typefigure == "valeurs"){
      legendeY <- expression(pH~(unite~pH))
      legendeTitre <- expression(pH~(unite~pH))
      typemesureTitreSortie <- "_pH_"
    }
  }
  
  if(typemesure == "Potentiel d’oxydo-réduction"){
    if(typefigure == "valeurs"){
      legendeY <- expression(paste("Potentiel d'oxydo-réduction (mV)"))
      legendeTitre <- expression(paste("Potentiel d'oxydo-réduction (mV)"))
      typemesureTitreSortie <- "_redox_"
    }
  }
  
  if(typemesure == "Phycocyanine"){
    if(typefigure == "valeurs"){
      legendeY <- expression(Phycocyanines~(paste(mu,g/L)))
      legendeTitre <- expression(Phycocyanines~(paste(mu,g/L)))
      typemesureTitreSortie <- "_phycocyanine_"
    }
  }
  
  if(typemesure == "Chlorophylle"){
    if(typefigure == "valeurs"){
      legendeY <- expression(Chlorophylles~(paste(mu,g/L)))
      legendeTitre <- expression(Chlorophylles~(paste(mu,g/L)))
      typemesureTitreSortie <- "_chlorophylle_"
    }
  }

  #### Tests ####
  if(is.na(as.character(legendeY))) stop("Paramètres de titres de figure à créer")
  if(is.na(as.character(legendeTitre))) stop("Paramètres de titres de figure à créer")
  if(is.na(typemesureTitreSortie)) stop("Paramètres de titres de figure à créer")
  
  #### Regroupement des valeurs ####
  parametres <- NULL
  parametres$legendeY <- legendeY
  parametres$legendeTitre <- legendeTitre
  parametres$typemesureTitreSortie <- typemesureTitreSortie
  
  #### Affichage des résultats ####
  return(parametres)

} # Fin de la fonction
