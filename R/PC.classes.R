#' Attribution de classes de qualité
#'
#' Cette fonction permet d'attribuer des classes de qualité à des valeurs physico-chimiques
#' 
#' @param PC Jeu de données
#' @param Referentiel Référentiel de qualité (NV par défaut)
#' @import tidyverse
#' @import reshape2
#' @export
#' @examples
#' PC.classes(PC)
#' PC.classes(PC,Referentiel="NV")
#' PC.classes(PC,Referentiel="SEQ-EAU")
#' PC.classes(PC,Referentiel="Quebec")

##### TODO LIST #####
# Si mise à jour des paramètres :
# library(readxl);library(tidyverse);data <- read_excel("data/Seuils_PC.xlsx");save(data,file="data/Seuils_PC.RData")
# PC <- PC %>% filter(SupportSANDRE == 6)
# PCsave <- PC
# 
# Ajout d'un filtre si inférieur au seuil de détection avec commentaire en fonction de la valeur de ce seuil/toxicité
# Notion d'unité
# Nettoyage des colonnes inutiles et sortie d'un tableau lisible en fct du référentiel (à choisir dans appel fct)
# Option permettant différentes sorties (couleurs/valeurs etc.) = Ajout d'une option permettant de choisir le mode de sortie (en l'état avec 2 colonnes de + ou bien différents types de matrices de synthèse (valeurs, classe, couleurs) comme dans fichier PC_V3.R)
# Complément base de données (ETM SEQ-Eau et Québec ok)
# Intégration base de données seuils dans base de données PC
#####################

PC.classes <- function(
  PC,
  Referentiel = c("NV", "SEQ-EAU", "Quebec")
  )
{
  
  ## Évaluation des choix
  Referentiel <- match.arg(Referentiel)
  
  ##### Chargement des référentiels #####
  data(Seuils_PC) # Pour charger les seuils de qualité

###### SEQ-EAU #####
if(Referentiel == "SEQ-EAU") {
  
  ## Création des seuils ##
  Seuils <- 
    data %>% 
    filter(Référentiel == "SEQ-Eau par altération") %>% 
    tidyr::unite(Seuil, c(Seuil, Classe), remove=T, sep = "-") %>% 
    dcast(Référentiel + ParametreSANDRE + SupportSANDRE ~ Seuil, value.var = "ValeurSeuil") %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=T, sep = "-")
  
  ## Attribution des classes de qualité ##
  Classes <-
    PC %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=F, sep = "-") %>% 
    left_join(Seuils, by = c("Cl" = "Cl")) %>% 
    mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
    mutate(Classe = NA) %>% 
    mutate(Classe = ifelse(is.na(`Maximum-4 - Orange`), # Pour le cas où il n'y a que 3 seuils
                           case_when(.$Valeur < .$`Maximum-1 - Bleu` ~ "1 - Bleu",
                                     .$Valeur >= .$`Maximum-1 - Bleu` & .$Valeur < .$`Maximum-2 - Vert` ~ "2 - Vert",
                                     .$Valeur >= .$`Maximum-2 - Vert` & .$Valeur < .$`Maximum-3 - Jaune` ~ "3 - Jaune",
                                     .$Valeur >= .$`Maximum-3 - Jaune` ~ "4 - Orange", # S'il n'existe que 3 limites
                                     #.$Valeur >= .$`Maximum-3 - Jaune` & .$Valeur < .$`Maximum-4 - Orange` ~ "4 - Orange", # S'il existe 4 limites
                                     #.$Valeur >= .$`Maximum-4 - Orange` ~ "5 - Rouge", # S'il existe 4 limites
                                     TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                           Classe)
    ) %>% 
    mutate(Classe = ifelse(!is.na(`Maximum-4 - Orange`), # Pour le cas où il y a 4 seuils
                           case_when(.$Valeur < .$`Maximum-1 - Bleu` ~ "1 - Bleu",
                                     .$Valeur >= .$`Maximum-1 - Bleu` & .$Valeur < .$`Maximum-2 - Vert` ~ "2 - Vert",
                                     .$Valeur >= .$`Maximum-2 - Vert` & .$Valeur < .$`Maximum-3 - Jaune` ~ "3 - Jaune",
                                     #.$Valeur >= .$`Maximum-3 - Jaune` ~ "4 - Orange", # S'il n'existe que 3 limites
                                     .$Valeur >= .$`Maximum-3 - Jaune` & .$Valeur < .$`Maximum-4 - Orange` ~ "4 - Orange", # S'il existe 4 limites
                                     .$Valeur >= .$`Maximum-4 - Orange` ~ "5 - Rouge", # S'il existe 4 limites
                                     TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                           Classe)
    ) %>% 
    mutate(Classe = ifelse(CodeRemarque == 10 & !is.na(`Maximum-1 - Bleu`), "< seuil quantification", Classe)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
    mutate(Classe = ifelse(CodeRemarque == 2 & !is.na(`Maximum-1 - Bleu`), "< seuil détection", Classe)) %>%  # Pour compléter les cas inférieurs au seuil de détection 
    select(-(`Maximum-1 - Bleu`:`Maximum-4 - Orange`), -Cl) %>% 
    mutate(Couleur = case_when(.$Classe == "1 - Bleu" ~ "Bleu",
                               .$Classe == "2 - Vert" ~ "Vert",
                               .$Classe == "3 - Jaune" ~ "Jaune",
                               .$Classe == "4 - Orange" ~ "Orange",
                               .$Classe == "5 - Rouge" ~ "Rouge",
                               .$Classe == "< seuil détection" ~ "Gris clair",
                               .$Classe == "< seuil quantification" ~ "Gris foncé",
                               TRUE ~ "Non évalué"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
}

###### Sédiments Québec #####
  
if(Referentiel == "Quebec") {
  
  ## Création des seuils ##
Seuils <- 
  data %>% 
  filter(Référentiel == "Critères pour l'évaluation de la qualité des sédiments au Québec") %>% 
  tidyr::unite(Seuil, c(Seuil, Classe), remove=T, sep = "-") %>% 
  dcast(Référentiel + ParametreSANDRE + SupportSANDRE ~ Seuil, value.var = "ValeurSeuil") %>% 
  tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=T, sep = "-")
  
  ## Attribution des classes de qualité ##
Classes <-
  PC %>% 
  tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=F, sep = "-") %>% 
  left_join(Seuils, by = c("Cl" = "Cl")) %>% 
  mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
  mutate(Classe = NA) %>% 
  mutate(Classe = ifelse(is.na(`Minimum-CEF`) & is.na(`Minimum-CEP`) & is.na(`Minimum-CER`) & is.na(`Minimum-CSE`), # Pour le cas où il y a 1 seuil
                             case_when(.$Valeur < .$`Minimum-CEO` ~ "< Concentration d’effets occasionnels",
                                       #.$Valeur >= .$`Minimum-CER` & .$Valeur < .$`Minimum-CSE` ~ "Concentration d’effets rares",
                                       #.$Valeur >= .$`Minimum-CSE` & .$Valeur < .$`Minimum-CEO` ~ "Concentration seuil produisant un effet",
                                       .$Valeur >= .$`Minimum-CEO` ~ "Concentration d’effets occasionnels",
                                       #.$Valeur >= .$`Minimum-CEP` & .$Valeur < .$`Minimum-CEF` ~ "Concentration produisant un effet probable",
                                       #.$Valeur >= .$`Minimum-CEF` ~ "Concentration d’effets fréquents",
                                       TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                             Classe)
  ) %>% 
  mutate(Classe = ifelse(!is.na(`Minimum-CEF`), # Pour le cas où il y a 5 seuils
                             case_when(.$Valeur < .$`Minimum-CER` ~ "Concentration sans effet",
                                       .$Valeur >= .$`Minimum-CER` & .$Valeur < .$`Minimum-CSE` ~ "Concentration d’effets rares",
                                       .$Valeur >= .$`Minimum-CSE` & .$Valeur < .$`Minimum-CEO` ~ "Concentration seuil produisant un effet",
                                       .$Valeur >= .$`Minimum-CEO` & .$Valeur < .$`Minimum-CEP` ~ "Concentration d’effets occasionnels",
                                       .$Valeur >= .$`Minimum-CEP` & .$Valeur < .$`Minimum-CEF` ~ "Concentration produisant un effet probable",
                                       .$Valeur >= .$`Minimum-CEF` ~ "Concentration d’effets fréquents",
                                       TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                             Classe)
      ) %>% 
  mutate(Classe = ifelse(CodeRemarque == 10 & !is.na(`Minimum-CER`), "< seuil quantification", Classe)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
  mutate(Classe = ifelse(CodeRemarque == 2 & !is.na(`Minimum-CER`), "< seuil détection", Classe)) %>%  # Pour compléter les cas inférieurs au seuil de détection 
  select(-(`Minimum-CEF`:`Minimum-CSE`), -Cl) %>% 
  mutate(Couleur = case_when(.$Classe == "Concentration sans effet" ~ "Bleu",
                                 .$Classe == "Concentration d’effets rares" ~ "Vert",
                                 .$Classe == "Concentration seuil produisant un effet" ~ "Jaune",
                                 .$Classe == "Concentration d’effets occasionnels" ~ "Orange",
                                 .$Classe == "Concentration produisant un effet probable" ~ "Rouge",
                                 .$Classe == "Concentration d’effets fréquents" ~ "Violet",
                                 .$Classe == "< seuil détection" ~ "Gris clair",
                                 .$Classe == "< seuil quantification" ~ "Gris foncé",
                                 .$Classe == "< Concentration d’effets occasionnels" ~ "Vert clair",
                                 TRUE ~ "Non évalué"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
    
}

#### Retour des données ####
  return(Classes)
  
} # Fin de la fonction