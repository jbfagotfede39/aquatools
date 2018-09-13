#' Synthèse des données piscicoles
#'
#' Cette fonction permet de réaliser différentes synthèses des enjeux piscicoles à différentes échelles spatiales (commune, communauté de communes, contexte de PDPG ou département)
#' 
#' @param Synthese Type de synthèse. Présence par défaut
#' @param Echelle Échelle spatiale de la synthèse (commune, communauté de communes, contexte de PDPG ou département)
#' @param Territoire Territoire concerné : code insee ou code contexte
#' @import dbplyr
#' @import dplyr
#' @import keyring
#' @import RPostgreSQL
#' @export
#' @examples
#' poissons.synthese("Présence", "ComCom", "200071595")

##### TODO LIST #####
# Ajout de l'échelon cantonal
#####################

poissons.synthese <- function(
  Synthese = "Présence",
  Echelle = c("Commune", "ComCom", "ContextePDPG", "Département"),
  Territoire = "39"
  )
{
  
  ## Évaluation des choix
  Synthese <- match.arg(Synthese)
  Echelle <- match.arg(Echelle)
  
  ## Connexion à la BDD et chargement des données ##
  dbP <- BDD.ouverture(Type = "Poissons")
  Resultats <- poissons.resultats.BDD()
  CommunesMultifish <- tbl(dbP,"communes") %>% collect(n = Inf)
  
  Communes <- 
    read_excel(adresse.switch("NAS-DATA/Géographie/Toponymie/table-appartenance-geo-communes-18.xlsx"), skip = 5)
  
  ## Filtrage des communes ##
if(Echelle == "Commune"){
  Communes <-
    Communes %>% 
    filter(CODGEO == Territoire)
  if(dim(Communes)[1] == 0) stop("Code Insee de la commune faux")
}
  
if(Echelle == "ComCom"){
  Communes <-
    Communes %>% 
    filter(EPCI == Territoire)
  if(dim(Communes)[1] == 0) stop("Code Insee de la comcom faux")
}
  
if(Echelle == "Département"){
  Communes <-
    Communes %>% 
    filter(DEP == Territoire)
  if(dim(Communes)[1] == 0) stop("Code Insee du département faux")
}

if(Echelle != "ContextePDPG"){
Resultatsbruts <-
  CommunesMultifish %>% 
  filter(noinsee %in% Communes$CODGEO) %>% 
  left_join(Resultats, by = ("commune")) %>% 
  distinct(coderesultat, commune, noinsee, datedebut.y, codeespece) %>% 
  select(coderesultat, everything()) %>% 
  mutate(identifiantbasesource = "Multifish_FJPPMA") %>% 
  left_join(poissons.especes() %>% select(codeespece, nomfrancais, nomlatin, referencefishbase, codesandre, codetaxref), by = "codeespece") %>% 
  rename(Date = datedebut.y)

ResultatsSynthese <-
  Resultatsbruts %>% 
  reshape2::dcast(codeespece ~ commune, value.var = "noinsee", fun.aggregate = length, fill = NA_real_) %>% # permet d'avoir seulement présence/absence, par le nombre d'occurences
  mutate(codeespece = as.character(codeespece)) %>% 
  mutate_if(is.numeric, funs(ifelse(!is.na(.), 1, .))) %>% # Remplace les valeurs qui ne sont pas des NA par des 1 dans les colonnes numériques
  left_join(poissons.especes() %>% select(codeespece, nomfrancais, nomlatin, referencefishbase, codesandre, codetaxref, protectioniucnlocal, protectioniucnnational, protectionconventionberne, protectiondirectivehabitatsfaunefloreannexeii, protectionarrete8decembre1988, protectionexogene, protectionnuisible), by = "codeespece") %>% 
  select(codeespece, nomfrancais, nomlatin, referencefishbase, codesandre, codetaxref, protectioniucnlocal, protectioniucnnational, protectionconventionberne, protectiondirectivehabitatsfaunefloreannexeii, protectionarrete8decembre1988, protectionexogene, protectionnuisible, everything())
}

if(Echelle == "ContextePDPG"){
  stop("Fonction à développer")
}
  
  ## Sortie des résultats ##
return(ResultatsSynthese)
  
} # Fin de la fonction
