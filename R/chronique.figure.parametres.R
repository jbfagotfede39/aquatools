#' Paramétrisation des vues de chroniques
#'
#' Cette fonction permet de sélectionner automatiquement les paramètres de légende des représentations graphiques de chroniques à partir des données issues de chronique.contexte
#' @name chronique.figure.parametres
#' @param data Data.frame issu de la fonction chronique.contexte()
#' @param typefigure Type de figure qui est à paramétriser : \code{valeurs} (par défault), \code{cumul} ou \code{vmm30j}
#' @keywords chronique
#' @export
#' @examples
#' chronique.mesures("DRO6-8") %>% chronique.contexte() %>% chronique.figure.parametres()
#' 
#' parametres <- chronique.mesures("DRO6-8") %>% chronique.contexte() %>% chronique.figure.parametres()
#' legendeY <- parametres$legendeY
#' legendeTitre <- parametres$legendeTitre
#' typemesureTitreSortie <- parametres$typemesureTitreSortie


##### TODO LIST #####
# 
#####################

chronique.figure.parametres <- function(
  data = data,
  typefigure = c("valeurs", "cumul", "vmm30j")
)
{
  
  #### Évaluation des choix ####
  typefigure <- match.arg(typefigure)
  
  #### Contexte des données ####
  contextereference <- structure(list(nstation = integer(0), ntypemesure = integer(0), 
                             nannee = integer(0), nmilieu = integer(0), station = structure(character(0), class = c("glue", 
                                                                                                                    "character")), typemesure = structure(character(0), class = c("glue", 
                                                                                                                                                                                  "character")), annee = structure(character(0), class = c("glue", 
                                                                                                                                                                                                                                           "character")), milieu = structure(character(0), class = c("glue", 
                                                                                                                                                                                                                                                                                                     "character"))), row.names = c(NA, 0L), class = c("tbl_df", 
                                                                                                                                                                                                                                                                                                                                                      "tbl", "data.frame"))
  
  #### Tests ####
  if(any(names(data) == names(contextereference)) == FALSE) stop("Les données d'entrée ne sont pas au format de sortie de chronique.contexte")
  if(data$ntypemesure != 1) stop("Plusieurs chmes_typemesure au sein du jeu de données")

  #### Création des objets vides pour tests ultérieurs ####
  legendeY <- NA_character_
  legendeTitre <- NA_character_
  typemesureTitreSortie <- NA_character_
  
  #### Ajustement des paramètres en fonction du typemesure ####
  typemesure <- data$typemesure
  
  if(typemesure == "Thermie" | typemesure == "Thermie barométrique" | typemesure == "Thermie piézométrique"){
    typemesureTitreSortie <- "_thermie_"
    if(typefigure == "valeurs"){
      legendeY <- "Température (°C)"
      legendeTitre <- "Températures :"
    }
    if(typefigure == "cumul"){
      legendeY <- "Somme des degrés-jours"
      if(data$nannee == 1 & data$nstation == 1) legendeTitre <- "Année biologique"
      if(data$nannee != 1 & data$nstation == 1) legendeTitre <- "Années biologiques"
      if(data$nannee != 1 & data$nstation != 1) legendeTitre <- "Couples station-année"
    }
    if(typefigure == "vmm30j"){
      legendeY <- "Tmm30j (°C)"
      legendeTitre <- "Températures :"
    }
  }
  
  if(typemesure == "Barométrie"){
    if(typefigure == "valeurs"){
      legendeY <- "Pression atmosphérique (cm H2O)"
      legendeTitre <- "Barométrie :"
      typemesureTitreSortie <- "_barométrie_"
    }
  }
  
  if(typemesure == "Piézométrie" | typemesure == "Piézométrie brute" | typemesure == "Piézométrie compensée" | typemesure == "Piézométrie calée"){
    if(typefigure == "valeurs"){
      legendeY <- "Hauteur d'eau (cm)"
      legendeTitre <- "Piézométrie :"
      typemesureTitreSortie <- "_piézométrie_"
    }
  }
  
  if(typemesure == "Piézométrie NGF"){
    if(typefigure == "valeurs"){
      legendeY <- "Hauteur d'eau (NGF)"
      legendeTitre <- "Piézométrie :"
      typemesureTitreSortie <- "_piézométrie_"
    }
  }
  
  if(typemesure == "Oxygénation"){
    if(typefigure == "valeurs"){
      legendeY <- expression(Oxygene~dissous~(mg~O[2]/L))
      legendeTitre <- "Oxygénation :"
      typemesureTitreSortie <- "_oxygénation_"
    }
  }
  
  if(typemesure == "Hydrologie"){
    typemesureTitreSortie <- "_hydrologie_"
    if(typefigure == "valeurs"){
      legendeY <- expression(Débit~(m^3/s))
      legendeTitre <- "Hydrologie :"
    }
    if(typefigure == "cumul"){
      stop("Légendes à modifier")
      legendeY = expression(Débit~(m^3/s))
      legendeTitre = "Hydrologie :"
    }
  }
  
  if(typemesure == "Pluviométrie"){
    typemesureTitreSortie <- "_pluviométrie_"
    if(typefigure == "valeurs"){
      legendeY <- expression(Précipitations~(L/m^2))
      legendeTitre <- "Pluviométrie :"
    }
    if(typefigure == "cumul"){
      legendeY = expression(Précipitations~cumulées~(L/m^2))
      legendeTitre = "Pluviométrie :"
    }
  }
  
  #### Tests ####
  if(is.na(as.character(legendeY))) stop("Paramètres de titres de figure à créer")
  if(is.na(legendeTitre)) stop("Paramètres de titres de figure à créer")
  if(is.na(typemesureTitreSortie)) stop("Paramètres de titres de figure à créer")
  
  #### Regroupement des valeurs ####
  parametres <- NULL
  parametres$legendeY <- legendeY
  parametres$legendeTitre <- legendeTitre
  parametres$typemesureTitreSortie <- typemesureTitreSortie
  
  #### Affichage des résultats ####
  return(parametres)

} # Fin de la fonction
