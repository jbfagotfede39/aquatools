#' Agrégation de paramètres physio-chimiques
#'
#' Cette fonction permet de regrouper certains paramètres physico-chimiques (PCB 7, HAP 14, pesticides totaux, etc.)
#' @name PC.agregation
#' @param PC Jeu de données
#' @import tidyverse
#' @export
#' @examples
#' PC.agregation(PC)

##### TODO LIST #####
# Il faudrait créer un "tous PCB"
#####################

PC.agregation <- function(
  PC
)
{

  ####### Listes des paramètres ##### 
  PCB7 <- c("1239","1241","1242","1243","1244","1245","1246")
  HAP14 <- c("1453", "1458","1623","1118","1537", "1191", "1524","1476","1117","1621", "1204", "1082","1116", "1115")
  HAP16EPA <- tribble(
    ~pcmes_parametrenom, ~pcmes_parametresandre,
    "Indéno[1,2,3-c,d]pyrène ", "1204",
    "benzo[k]fluoranthène", "1117",
    "benzo[a]pyrène (BaP)", "1115",
    "benzo[g,h,i]pérylène", "1118",
    "fluoranthène", "1191",
    "naphtalène", "1517",
    "anthracène", "1458",
    "phénanthrène", "1524",
    "acénaphtène", "1453",
    "chrysène", "1476",
    "benzo[a]anthracène", "1082",
    "dibenzo[a,h]anthracène", "1621",
    "acénaphtylène", "1622",
    "pyrène", "1537",
    "fluorène", "1623",
    "benzo[b]fluoranthène", "1116"
  )
  
  pesticides <- 
    tbl(dbD, in_schema("fd_referentiels", "physicochimie_groupesparametres")) %>%
    filter(grepl("icides", pcgrpparam_usagegroupeparametres1libelle)|grepl("icides", pcgrpparam_usagegroupeparametres2libelle)|grepl("icides", pcgrpparam_usagegroupeparametres3libelle)|grepl("icides", pcgrpparam_usagegroupeparametres4libelle)|grepl("icides", pcgrpparam_usagegroupeparametres5libelle)) %>%
    collect()
  
  ## Recherche des usages uniques ##
  # pesticides %>% 
  #   distinct(pcgrpparam_usagegroupeparametres1libelle) %>% 
  #   rename(pcgrpparam_usagegroupeparametreslibelle = pcgrpparam_usagegroupeparametres1libelle) %>% 
  #   bind_rows(
  #     pesticides %>% 
  #       distinct(pcgrpparam_usagegroupeparametres2libelle) %>% 
  #       rename(pcgrpparam_usagegroupeparametreslibelle = pcgrpparam_usagegroupeparametres2libelle)
  #   ) %>% 
  #   bind_rows(
  #     pesticides %>% 
  #       distinct(pcgrpparam_usagegroupeparametres3libelle) %>% 
  #       rename(pcgrpparam_usagegroupeparametreslibelle = pcgrpparam_usagegroupeparametres3libelle)
  #   ) %>% 
  #   bind_rows(
  #     pesticides %>% 
  #       distinct(pcgrpparam_usagegroupeparametres4libelle) %>% 
  #       rename(pcgrpparam_usagegroupeparametreslibelle = pcgrpparam_usagegroupeparametres4libelle)
  #   ) %>% 
  #   bind_rows(
  #     pesticides %>% 
  #       distinct(pcgrpparam_usagegroupeparametres5libelle) %>% 
  #       rename(pcgrpparam_usagegroupeparametreslibelle = pcgrpparam_usagegroupeparametres5libelle)
  #   ) %>% 
  #   distinct(pcgrpparam_usagegroupeparametreslibelle)

  ##### Préparation des données #####
  PC <- 
    PC %>%
    mutate(pcmes_valeur = as.numeric(sub(",", ".", pcmes_valeur)))
  
  ############### PCB(7) ###########
  PC <- 
    PC %>% 
    bind_rows(
    PC %>%
    filter(pcmes_parametresandre %in% PCB7) %>%
    group_by(pcmes_coderhj, pcmes_date, pcmes_supportsandre, pcmes_unitenom, pcmes_unitesandre, , pcmes_supportnom) %>%
    summarise(
      pcmes_valeur = sum(pcmes_valeur, na.rm = T)
    ) %>% 
    mutate(pcmes_valeur = dplyr::na_if(pcmes_valeur, 0)) %>% 
    mutate(pcmes_coderemarque = ifelse(is.na(pcmes_valeur), "2", "1")) %>% 
    mutate(pcmes_parametresandre = 7431) %>% 
    mutate(pcmes_parametrenom = "PCB somme(7)") %>% 
    mutate(pcmes_parametresandre = as.character(pcmes_parametresandre))
    )
  
  ############### HAP(14) ###########
  PC <- 
    PC %>% 
    bind_rows(
      PC %>%
        filter(pcmes_parametresandre %in% HAP14) %>%
        group_by(pcmes_coderhj, pcmes_date, pcmes_supportsandre, pcmes_unitenom, pcmes_unitesandre, , pcmes_supportnom) %>%
        summarise(
          pcmes_valeur = sum(pcmes_valeur, na.rm = T)
        ) %>% 
        mutate(pcmes_valeur = dplyr::na_if(pcmes_valeur, 0)) %>% 
        mutate(pcmes_coderemarque = ifelse(is.na(pcmes_valeur), "2", "1")) %>% 
        mutate(pcmes_parametresandre = 200001) %>% # pas de code sandre -> code sandre fictif afin de faire la jointure avec pc.classes
        mutate(pcmes_parametrenom = "HAP somme (14)") %>% 
        mutate(pcmes_parametresandre = as.character(pcmes_parametresandre))
    )
  
  ############### HAP(16) ###########
  PC <- 
    PC %>% 
    bind_rows(
      PC %>%
        filter(pcmes_parametresandre %in% HAP16EPA$pcmes_parametresandre) %>%
        group_by(pcmes_coderhj, pcmes_date, pcmes_supportsandre, pcmes_unitenom, pcmes_unitesandre, , pcmes_supportnom) %>%
        summarise(
          pcmes_valeur = sum(pcmes_valeur, na.rm = T)
        ) %>% 
        mutate(pcmes_valeur = dplyr::na_if(pcmes_valeur, 0)) %>% 
        mutate(pcmes_coderemarque = ifelse(is.na(pcmes_valeur), "2", "1")) %>% 
        mutate(pcmes_parametresandre = 6136) %>%
        mutate(pcmes_parametrenom = "Somme HAP (16) - EPA") %>% 
        mutate(pcmes_parametresandre = as.character(pcmes_parametresandre))
    )
  
  ############### Pesticides ###########
  PC <- 
    PC %>% 
    bind_rows(
      PC %>%
        filter(pcmes_parametresandre %in% pesticides$pcgrpparam_parametresandre) %>%
        group_by(pcmes_coderhj, pcmes_date, pcmes_supportsandre, pcmes_unitenom, pcmes_unitesandre, , pcmes_supportnom) %>%
        summarise(
          pcmes_valeur = sum(pcmes_valeur, na.rm = T)
        ) %>% 
        mutate(pcmes_valeur = dplyr::na_if(pcmes_valeur, 0)) %>% 
        # {if(nrow(.) != 0) mutate(pcmes_coderemarque = ifelse(is.na(pcmes_valeur), "2", "1")) else .} %>% # Pour rendre pcmes_coderemarque en tant que chr plutôt que lgl si aucune valeur dans le dataframe
        {if(nrow(.) == 0) mutate(., pcmes_coderemarque = NA_character_) else .} %>% # Pour rendre pcmes_coderemarque en tant que chr plutôt que lgl si aucune valeur dans le dataframe
        mutate(pcmes_parametresandre = 6276) %>%
        mutate(pcmes_parametrenom = "Pesticides (somme)") %>% 
        mutate(pcmes_parametresandre = as.character(pcmes_parametresandre))
    )

  #### Retour des données ####
  return(PC)
  
} # Fin de la fonction
