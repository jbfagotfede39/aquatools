#' Extraction des données de référence des espèces
#'
#' Récupère l'ensemble des données de référence des espèces de Multifish
#' @name poissons.especes
#' @keywords donnees
#' @param Sortie Format de la sortie
#' @param Couleurs Présence de couleurs dans le formatage du statut des espèces (\code{FALSE} par défault)
#' @import DBI 
#' @import dplyr
#' @import RSQLite
#' @export
#' @examples
#' poissons.especes()
#' poissons.especes(Sortie = "Simple")
#' poissons.especes("Propre")
#' poissons.especes("Propre", Couleurs = T)

poissons.especes <- function(
  Sortie = c("Complet","Simple","Propre"),
  Couleurs = FALSE
  )
  {
  
  ## Ouverture de la BDD ##
  dbP <- BDD.ouverture(Type = "Poissons")
  
  ## Évaluation des choix
  Sortie <- match.arg(Sortie)
  
  ##### Récupération des données #####
  Especes <- tbl(dbP,"especes") %>% collect(n = Inf)
  
  ## Fermeture de la BDD ##
  DBI::dbDisconnect(dbP)
  
  #### Nettoyage des données ####
  if(Sortie == "Simple"){
    Especes <- 
      Especes %>% 
      select(codeespece, nomfrancais)
  }
  
  if(Sortie == "Propre"){
    Especes <- 
      Especes %>% 
      select(codeespece:nomlatin,famille, protectioniucnlocal)
  }
  
  if(Sortie == "Simple" | Sortie == "Propre"){
    Especes <- 
      Especes %>% 
      rename_at(vars(contains("codeespece")), funs(str_replace(., "codeespece", "Code"))) %>%
      rename_at(vars(contains("nomfrancais")), funs(str_replace(., "nomfrancais", "Espèce"))) %>%
      rename_at(vars(contains("nomlatin")), funs(str_replace(., "nomlatin", "Nom latin"))) %>%
      rename_at(vars(contains("famille")), funs(str_replace(., "famille", "Famille"))) %>%
      rename_at(vars(contains("protectioniucnlocal")), funs(str_replace(., "protectioniucnlocal", "Statut")))
  }
  
  if(Sortie != "Simple" & Couleurs == TRUE){
    Especes <- 
      Especes %>% 
      mutate(Statut = stringr::str_replace(Statut, "(?s) .*", "")) %>%  # Suppression de la chaîne de caractère à partir du premier espace
      dplyr::na_if("NA") %>% 
      mutate(Statut = ifelse(Statut == "EX", "{\\color[RGB]{0,0,0} EX}", Statut)) %>% 
      mutate(Statut = ifelse(Statut == "EW", "{\\color[RGB]{61,25,81} EW}", Statut)) %>% 
      mutate(Statut = ifelse(Statut == "RE", "{\\color[RGB]{90,26,99} RE}", Statut)) %>% 
      mutate(Statut = ifelse(Statut == "CR", "{\\color[RGB]{211,0,27} CR}", Statut)) %>% 
      mutate(Statut = ifelse(Statut == "EN", "{\\color[RGB]{251,191,0} EN}", Statut)) %>% 
      mutate(Statut = ifelse(Statut == "VU", "{\\color[RGB]{255,237,0} VU}", Statut)) %>% 
      mutate(Statut = ifelse(Statut == "NT", "{\\color[RGB]{251,242,202} NT}", Statut)) %>% 
      mutate(Statut = ifelse(Statut == "LC", "{\\color[RGB]{120,183,74} LC}", Statut)) %>% 
      mutate(Statut = ifelse(Statut == "DD", "{\\color[RGB]{211,212,213} DD}", Statut))
  }
  
  return(Especes)
} # Fin de la fonction