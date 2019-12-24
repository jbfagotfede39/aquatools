#' Synthèse des données piscicoles
#'
#' Cette fonction permet de réaliser différentes synthèses des enjeux piscicoles à différentes échelles spatiales (commune, communauté de communes, contexte de PDPG ou département)
#' @name poissons.synthese
#' @param Synthese Type de synthèse. Présence par défaut (pas d'autres types pour l'instant)
#' @param Territoire Territoire concerné : issu de stations.territoire
#' @param Maille Maille de synthèse des données : commune (par défaut), canton, communauté de communes, département, région, contexte de PDPG, Hydro-écorégion, entité GEMAPI, Maître d'ouvrage, Milieu, Bassin versant, Sous-bassin versant, maille INPN
#' @import dbplyr
#' @import dplyr
#' @import keyring
#' @import RPostgreSQL
#' @export
#' @examples
#' poissons.synthese("Présence", Territoire, "Commune")
#' poissons.synthese("Présence", Territoire, "ContextePDPG")
#' poissons.synthese("Présence", stations.territoire("ContextePDPG"), "ContextePDPG")
#' poissons.synthese("Présence", stations.territoire("Milieu", c("Lac de Chalain", "Bonlieu")), "ContextePDPG")

##### TODO LIST #####
# Il faudrait ajouter un champ "Clé" dans stations.territoire, qui serait ensuite la clé de distinction pour poissons.synthese par exemple, au lieu d'avoir à tout réécrire dans cette fonction pour les mailles de regroupement
# Limitation temporelle (peut-être à prévoir dans poissons.resultats.BDD() pour intérêt sur plusieurs fonctions ?)
#####################

poissons.synthese <- function(
  Synthese = "Présence",
  Territoire = NA_character_,
  Maille = c("Commune", "Canton", "ComCom", "Département", "Région", "ContextePDPG", "HER", "GEMAPI", "MO", "Milieu", "Bassin", "Sous-bassin", "INPN")
  )
{
  
  #### Évaluation des choix ####
  Synthese <- match.arg(Synthese)
  Maille <- match.arg(Maille)
  
  #### Vérification ####
  if(all(is.na(Territoire)) == TRUE) stop("Territoire non défini")
  
  #### Connexion à la BDD et chargement des données ####
  dbP <- BDD.ouverture(Type = "Poissons")
  Resultats <- poissons.resultats.BDD()
  DBI::dbDisconnect(dbP)
  poissons.especes <- poissons.especes()
  
  #### Filtrage des résultats ####
  Resultats <- 
    Territoire %>% 
    st_join(
      Resultats %>% 
      filter(!is.na(codeespece)) %>%  # Pour supprimer les résultats avec les espèces sans codeespece (Hypophthalmichthys molitrix)
      filter(typelambert == "L93") %>% 
      st_as_sf(coords = c("xlambert","ylambert")) %>% 
      st_set_crs(2154)
    )
  
  #### Synthèse des données à échelle variable ####
  ## Commune ##
if(Maille == "Commune"){
  if("tpcomm_commune_libellesansarticle" %in% names(Resultats) == FALSE){
    dbD <- BDD.ouverture("Data")
    communes <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.topographie_communes WHERE (tpcomm_departement_insee = '39');")
    DBI::dbDisconnect(dbD)
    Resultats <-
      Resultats %>% 
      st_join(communes)
  }
  Resultatsbruts <-
    Resultats %>% 
    distinct(coderesultat, tpcomm_commune_libellesansarticle, tpcomm_commune_insee, datedebut.y, codeespece)

  ResultatsSynthese <-
    Resultatsbruts %>% 
    reshape2::dcast(codeespece ~ tpcomm_commune_insee, value.var = "tpcomm_commune_insee", fun.aggregate = length, fill = NA_real_) # permet d'avoir seulement présence/absence, par le nombre d'occurences
}
  
  if(Maille == "Complet"){
  Resultatsbruts <-
      Resultats %>% 
      distinct(coderesultat, datedebut.y, codeespece)
    
    ResultatsSynthese <-
      Resultatsbruts %>% 
      st_drop_geometry() %>% 
      count(codeespece) %>% 
      rename(Presence = n)
  }
  
  if(Maille == "ContextePDPG"){
    if("hycont_contexte_libelle" %in% names(Resultats) == FALSE){
      dbD <- BDD.ouverture("Data")
      contextesPDPG <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_contextespdpg;")
      DBI::dbDisconnect(dbD)
      Resultats <-
        Resultats %>% 
        st_join(contextesPDPG)
    }
    Resultatsbruts <-
      Resultats %>% 
      distinct(coderesultat, hycont_contexte_libelle, hycont_contexte_code, datedebut.y, codeespece)
    
    ResultatsSynthese <-
      Resultatsbruts %>% 
      reshape2::dcast(codeespece ~ hycont_contexte_code, value.var = "hycont_contexte_code", fun.aggregate = length, fill = NA_real_) # permet d'avoir seulement présence/absence, par le nombre d'occurences
  }
  
  if(Maille == "Canton"){
stop("Échelle à développer")
  }
  
  if(Maille == "ComCom"){
    stop("Échelle à développer")
  }
  
  if(Maille == "Département"){
    stop("Échelle à développer")
  }
  
  if(Maille == "Région"){
    stop("Échelle à développer")
  }
  
  if(Maille == "HER"){
    stop("Échelle à développer")
  }
  
  if(Maille == "GEMAPI"){
    stop("Échelle à développer")
  }
  
  if(Maille == "MO"){
    stop("Échelle à développer")
  }
  
  if(Maille == "Milieu"){
    warning("Cours d'eau non traités pour l'agrégation par milieu")
    if("hypldo_libelle" %in% names(Resultats) == FALSE){
      dbD <- BDD.ouverture("Data")
      plansdeau <- sf::st_read(dbD, query = "SELECT * FROM fd_referentiels.hydrographie_plansdeau;")
      DBI::dbDisconnect(dbD)
      Resultats <-
        Resultats %>% 
        st_join(plansdeau)
    }
    Resultatsbruts <-
      Resultats %>% 
      distinct(coderesultat, hypldo_libelle, datedebut.y, codeespece)
    
    ResultatsSynthese <-
      Resultatsbruts %>% 
      reshape2::dcast(codeespece ~ hypldo_libelle, value.var = "hypldo_libelle", fun.aggregate = length, fill = NA_real_) # permet d'avoir seulement présence/absence, par le nombre d'occurences
  }
  
  if(Maille == "Bassin"){
    stop("Échelle à développer")
  }
  
  if(Maille == "Sous-bassin"){
    stop("Échelle à développer")
  }
  
  if(Maille == "INPN"){
    stop("Échelle à développer")
  }
  
  #### Mise en forme des données agrégées aux différentes échelles ####
Resultatsbruts <-
  Resultatsbruts %>% 
  select(coderesultat, everything()) %>% 
  mutate(identifiantbasesource = "Multifish_FJPPMA") %>% 
  left_join(poissons.especes %>% select(codeespece, nomfrancais, nomlatin, referencefishbase, codesandre, codetaxref), by = "codeespece") %>% 
  rename(Date = datedebut.y)
  
ResultatsSynthese <-
  ResultatsSynthese %>% 
  mutate(codeespece = as.character(codeespece)) %>% 
  mutate_if(is.numeric, ~ (ifelse(!is.na(.), 1, .))) %>% # Remplace les valeurs qui ne sont pas des NA par des 1 dans les colonnes numériques
  left_join(poissons.especes %>% select(codeespece, groupetaxonomique, nomfrancais, nomlatin, referencefishbase, codetaxref, protectioniucnlocal, protectioniucnlocalcritere, protectioniucnnational, protectionconventionberne, protectiondirectivehabitatsfaunefloreannexeii, protectiondirectivehabitatsfaunefloreannexeiv, protectiondirectivehabitatsfaunefloreannexev, znieffdeterminant, znieffdeterminantconditions, protectionarrete8decembre1988, protectionexogene, protectionnuisible), by = "codeespece") %>% 
  mutate(enjeu = case_when(grepl("EN", protectioniucnlocal) ~ "Très fort",
                           grepl("EN", protectioniucnnational) ~ "Très fort",
                           grepl("CR", protectioniucnlocal) ~ "Très fort",
                           grepl("CR", protectioniucnnational) ~ "Très fort",
                           protectiondirectivehabitatsfaunefloreannexeii == "true" ~ "Fort",
                           grepl("VU", protectioniucnlocal) ~ "Fort",
                           grepl("VU", protectioniucnnational) ~ "Fort",
                           grepl("DD", protectioniucnlocal) ~ "?? Moyen si reproduction locale",
                           grepl("DD", protectioniucnnational) ~ "?? Moyen si reproduction locale",
                           grepl("NT", protectioniucnlocal) ~ "?? Moyen si reproduction locale",
                           grepl("NT", protectioniucnnational) ~ "?? Moyen si reproduction locale",
                           grepl("D : déterminant en Franche-Comté", znieffdeterminant) ~ "Moyen",
                           grepl("d* : déterminant dans certaines conditions", znieffdeterminant) ~ paste0("?? - ",znieffdeterminantconditions))
         ) %>% # Calcul selon le modèle du SCOT bisontin 09/2018 modèle PLUi CCAPS
  select(codeespece, groupetaxonomique, nomfrancais, nomlatin, referencefishbase, codetaxref, protectioniucnlocal, protectioniucnlocalcritere, protectioniucnnational, protectionconventionberne, protectiondirectivehabitatsfaunefloreannexeii, protectiondirectivehabitatsfaunefloreannexeiv, protectiondirectivehabitatsfaunefloreannexev, znieffdeterminant, znieffdeterminantconditions, protectionarrete8decembre1988, protectionexogene, protectionnuisible, enjeu, everything())
   
  ## Sortie des résultats ##
return(ResultatsSynthese)
  
} # Fin de la fonction
