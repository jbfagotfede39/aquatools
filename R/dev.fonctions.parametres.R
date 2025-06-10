#' Extraire les valeurs par défaut des paramètres d'une fonction
#'
#' Cette fonction permet d'extraire les valeurs par défaut des paramètres d'une fonction
#' @name dev.fonctions.parametres
#' @param fonction Fonction sur laquelle travailler
#' @keywords Développement
#' @export
#' @examples
#' dev.fonctions.parametres(chronique.DCE)

dev.fonctions.parametres <- function(fonction){
  #### Calcul ####
  # Récupérer les paramètres formels de la fonction
  params <- formals(fonction)
  
  # Initialiser un vecteur pour stocker les résultats
  resultats <- character(0)
  
  # Parcourir chaque paramètre
  for (param in names(params)) {
    # Récupérer la valeur par défaut du paramètre
    valeur_defaut <- ifelse(is.null(params[[param]]), "NULL", deparse(params[[param]]))
    
    # Ajouter la ligne au vecteur de résultats
    resultats <- c(resultats, paste0(param, " = ", valeur_defaut))
  }
  
  # Retourner les résultats sous forme de chaîne de caractères avec une ligne par paramètre
  resultats <- paste(resultats, collapse = "\n")
  
  #### Sortie ####
  return(resultats %>% cat())
}
