#' Collecte des données de CAA et CAR pour une station donnée
#'
#' Cette fonction permet de synthétiser les données de cotes d'abondance attendues et observées pour une station donnée
#' @name poissons.CAA
#' @keywords poissons
#' @param station Code RHJ de la station ("MAD6-2")
#' @export
#' @import tidyverse
#' @examples
#' poissons.CAA("MAD6-2")

##### -------------- A FAIRE -------------- #####
# 
##### -------------- A FAIRE -------------- #####

poissons.CAA <- function(
  station = as.character(NA),
  periode = c("10ans","20ans","Complet","4campagnes")
)
  {
  
  ## Évaluation des choix
    periode <- match.arg(periode)
  
  ## Vérifications ##
  if(is.na(station)) stop("Aucune station saisie")
  
  #### Import ntt_caR observées ####
  Resultats <- 
    poissons.resultats(data.frame(Nom = station), Sortie = "Complet") %>% 
    select(nom,modedepeche,datedebut.x, codeespece, coderdt, coteabondancenumerique, coteabondanceponderale, typetheorique) %>%
    arrange(nom, datedebut.x, codeespece) %>% 
    rowwise() %>% # Pour grouper les données par ligne pour avoir le min
    mutate(CA = min(coteabondancenumerique, coteabondanceponderale)) %>% 
    ungroup() %>% 
    group_by(nom)%>%
    mutate(NbrModedepeche = n_distinct(modedepeche)) %>%
    ungroup() %>% 
    mutate(datedebut.x = as.character(datedebut.x))
  
  # Limitation temporelle des résultats #
  if(periode == "10ans") limitetemporelle <- now()-years(10)
  if(periode == "20ans") limitetemporelle <- now()-years(20)
  if(periode == "Complet") limitetemporelle <- now()-years(100)
  if(periode == "4campagnes") limitetemporelle <- Resultats %>% distinct(datedebut.x) %>% arrange(desc(datedebut.x)) %>% pull() %>% nth(4)
  Resultats <- Resultats %>% filter(datedebut.x >= limitetemporelle)

  ## Contexte global ##
  Contexte <-
    Resultats %>% select(coderdt) %>% distinct() %>% ungroup() %>% 
    add_column(station = Resultats %>% select(nom) %>% distinct() %>% pull()) %>% 
    add_column(Ntypo = Resultats %>% select(typetheorique) %>% distinct() %>% pull()) %>% 
    add_column(NbrModedepeche = Resultats %>% select(NbrModedepeche) %>% distinct() %>% pull())
  
  #### Import ntt_ca REF ####
  #Récupération des données #
  dbP <- BDD.ouverture("Poissons")
  BddNTT <- 
    tbl(dbP,"poissons_pptth") %>% 
    collect(n = Inf) %>% 
    rename(coderdt = ntt_rhj) %>% 
    rename(typetheorique = ntt_ntt) %>% 
    rename(codeespece = ntt_espece) %>% 
    rename(CA = ntt_ca) %>% 
    select(coderdt:CA)
    
  DBI::dbDisconnect(dbP)
  
  #### Jointure ####
  ### Un seul mode de pêche ###
  if(Contexte$NbrModedepeche == 1){

      BddNTT <- 
        BddNTT %>% 
        mutate(datedebut.y  = "NTT") %>%
        select(datedebut.y, codeespece, coderdt, typetheorique, CA) %>%
        filter(coderdt %in% Contexte$coderdt) %>% 
        filter(typetheorique %in% Contexte$Ntypo) %>% 
        mutate(coderdt = "Référence")
      
      if(nrow(BddNTT) == 0) warning("Pas de peuplement théorique correspondant dans la table poissons_pptth")

      Resultatsvue <- 
        Resultats %>%
        select(codeespece, datedebut.x, coderdt, typetheorique, CA)%>%
        mutate(Annee = year(ymd(datedebut.x)))
      
    ## Le NTT existe dans les stations de multifish
    if(Contexte$Ntypo != ""){
      Resultatsvue <- 
        Resultatsvue %>% 
        bind_rows(BddNTT) %>% 
        mutate(Annee = ifelse(is.na(Annee), datedebut.y, Annee)) %>% 
        select(-datedebut.y)
    }
    
    ## Le NTT n'existe pas dans les stations de multifish
    if(Contexte$Ntypo == ""){
      Resultatsvue <- 
        Resultats %>%
        mutate(Annee = year(ymd(datedebut.x))) %>% ########### ajouter cette ligne #####
        mutate(Annee = ifelse(is.na(Annee), datedebut.x, Annee))
    }
      
      Resultatsvue <- 
        Resultatsvue %>% 
        mutate(datedebut.x = ifelse(is.na (datedebut.x), paste0("NTT ", typetheorique), datedebut.x))
  }
  
  ### Plusieurs modes de pêche ###
  if(Contexte$NbrModedepeche > 1){
    
    BddNTT <- 
      BddNTT %>% 
      mutate(datedebut.y = "NTT") %>%
      select(datedebut.y,codeespece,coderdt,typetheorique,CA) %>%
      filter(coderdt %in% Contexte$coderdt) %>% 
      filter(typetheorique %in% Contexte$Ntypo) %>% 
      mutate(typetheorique = "Référence")
    
    if(nrow(BddNTT) == 0) warning("Pas de peuplement théorique correspondant dans la table poissons_pptth")
    warning("attention deux modes de saisie dans la BDD à traiter") # Car saisie manuelle et automatique de résultats pour pêches multi-technique -> à homogénéiser
    
    # Traitement ECD #
    ResultatFalse <- 
      Resultats %>% 
      filter(modedepeche == "ECD") %>%
      select(codeespece,datedebut.x,coderdt,typetheorique,CA)
    
    # Traitement autre que ECD + jointure #
    ResultatTrue <-
      Resultats %>% 
      filter(modedepeche != "ECD") %>%
      group_by(codeespece) %>%
      mutate(CAcorrigé = max(CA)) %>%
      ungroup() %>%
      mutate(datedebutcorrigé = max(datedebut.x)) %>%
      select(-CA,-datedebut.x) %>%
      rename("CA"="CAcorrigé") %>%
      rename("datedebut.x"="datedebutcorrigé") %>%
      select(codeespece, datedebut.x, coderdt, typetheorique, CA) %>%
      distinct() %>% 
      bind_rows(ResultatFalse)
    
    # Nettoyage pour affichage
    Resultatsvue <- 
      ResultatTrue %>% 
      mutate(Annee = year(ymd(datedebut.x))) %>% 
      bind_rows(BddNTT)%>% 
      mutate(datedebut.x = ifelse(is.na (datedebut.x), paste0("NTT ", typetheorique ),datedebut.x)) %>% 
      mutate(Annee = ifelse(is.na(Annee), datedebut.y, Annee)) %>% 
      select(-datedebut.y) 
  }

  ##### Complément des CAA des écrevisses #####
  Resultatsvue <- 
    Resultatsvue %>%
    mutate(CA = ifelse(typetheorique == "Référence" & codeespece == "OCL", 0, CA)) %>% 
    mutate(CA = ifelse(typetheorique == "Référence" & codeespece == "PFL", 0, CA)) %>% 
    mutate(CA = ifelse(typetheorique == "Référence" & codeespece == "APP", 0, CA))
  
  ##### Complément du nom de la station #####
  Resultatsvue <- 
    Resultatsvue %>%
    mutate(station = Contexte$station)
  
  ##### Renommage de variables #####
  Resultatsvue <- 
    Resultatsvue %>%
    rename(Espece = codeespece) %>% 
    rename(Date = datedebut.x) %>% 
    rename(Ecosysteme = coderdt) %>% 
    rename(NTT = typetheorique) %>% 
    rename(Station = station)
    
  #### Sortie ####
  return(Resultatsvue)
  
} # Fin de la fonction
