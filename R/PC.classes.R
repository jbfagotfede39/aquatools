#' Attribution de classes de qualité
#'
#' Cette fonction permet d'attribuer des classes de qualité à des valeurs physico-chimiques
#' 
#' @param PC Jeu de données
#' @param Referentiel Referentiel de qualité (NV par défaut)
#' @param Categorie Catégorie piscicole (1 par défaut)
#' @import tidyverse
#' @import reshape2
#' @export
#' @examples
#' PC.ClasseQualites(PC)
#' PC.ClasseQualites(PC,Referentiel="NV")
#' PC.ClasseQualites(PC,Referentiel="SEQ-EAU")
#' PC.ClasseQualites(PC,Referentiel="Quebec")
#' #' PC.ClasseQualites(PC,Referentiel="SEQ-EAU",Categorie = 2)

##### TODO LIST #####
# Si mise à jour des paramètres :
# library(readxl);library(tidyverse);data <- read_excel("data/Seuils_PC_V3.xlsx");save(data,file="data/Seuils_PC.RData")
# PC <- PC %>% filter(SupportSANDRE == 6)
# PCsave <- PC
# library(readxl);library(tidyverse);library(reshape2);library(aquatools);data(Seuils_PC);Categorie = 1
# load("data/Seuils_PC.RData")
# 
# Ajout d'un filtre si inférieur au seuil de détection avec commentaire en fonction de la valeur de ce seuil/toxicité
# Notion d'unité : à faire avec la clé : parametre-Matrice-Unite
# Option permettant différentes sorties (couleurs/valeurs etc.) = Ajout d'une option permettant de choisir le mode de sortie (en l'état avec 2 colonnes de + ou bien différents types de matrices de synthèse (valeurs, ClasseQualite, couleurs) comme dans fichier PC_V3.R)
# Complément base de données (ETM SEQ-Eau et Québec ok)
# Intégration base de données seuils dans base de données PC
#####################

PC.classes <- function(
  PC,
  Referentiel = c("NV", "SEQ-EAU", "Quebec"),
  Categorie = 1
  )
{
  
  ## Évaluation des choix
  Referentiel <- match.arg(Referentiel)
  
  ##### Chargement des Referentiels #####
  data(Seuils_PC) # Pour charger les seuils de qualité

  ## Connexion à la BDD ##
  db <- BDD.ouverture(Type = "Physico-chimie")
  
  ## Récupération des données ##
  PC <- tbl(db,"PC") %>% collect()
  PC <- tbl(db,"PC") %>% collect(n = Inf)
  
###### Nisbet et Verneaux #####
if(Referentiel == "NV") {
  
  ## Création des seuils ##
  Seuils <- 
    data %>% 
    filter(Referentiel == "Nisbet - Verneaux") %>% 
    tidyr::unite(Seuil, c(Seuil, ClasseQualite), remove=T, sep = "-") %>% 
    dcast(Referentiel + ParametreSANDRE + SupportSANDRE ~ Seuil, value.var = "ValeurSeuil") %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=T, sep = "-")
  
  ## Attribution des classes de qualité ##
  ClasseQualites <-
    PC %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=F, sep = "-") %>% 
    left_join(Seuils, by = c("Cl" = "Cl")) %>% 
    mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
    mutate(ClasseQualite = NA) %>% 
    mutate(ClasseQualite = ifelse(is.na(`Minimum-1`) & is.na(`Minimum-2`) & is.na(`Minimum-3`) & is.na(`Minimum-4`) & is.na(`Minimum-5`) & is.na(`Minimum-6`) & is.na(`Minimum-7`), # Pour le cas où il y a 8 classes = 7 seuils <=> SANDRE 1305
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$Valeur >= .$`Maximum-3` & .$Valeur < .$`Maximum-4` ~ "Classe 4",
                                            .$Valeur >= .$`Maximum-4` & .$Valeur < .$`Maximum-5` ~ "Classe 5",
                                            .$Valeur >= .$`Maximum-5` & .$Valeur < .$`Maximum-6` ~ "Classe 6",
                                            .$Valeur >= .$`Maximum-6` & .$Valeur < .$`Maximum-7` ~ "Classe 7",
                                            .$Valeur >= .$`Minimum-8` ~ "Classe 8",
                                            TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1312-3", # Pour le cas où SANDRE 1312 <-> 6 cas
                                  case_when(.$Valeur < .$`Maximum-6` ~ "Classe 6",
                                            .$Valeur >= .$`Minimum-5` & .$Valeur < .$`Minimum-4` ~ "Classe 5",
                                            .$Valeur >= .$`Minimum-4` & .$Valeur < .$`Minimum-3` ~ "Classe 4",
                                            .$Valeur >= .$`Minimum-3` & .$Valeur < .$`Minimum-2` ~ "Classe 3",
                                            .$Valeur >= .$`Minimum-2` & .$Valeur < .$`Minimum-1` ~ "Classe 2",
                                            .$Valeur >= .$`Minimum-1` ~ "Classe 1",
                                            TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1313-3", # Pour le cas où SANDRE 1313 <-> 4 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$Valeur >= .$`Minimum-4` ~ "Classe 4",
                                            TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1335-3", # Pour le cas où SANDRE 1335 <-> 3 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Minimum-3` ~ "Classe 2",
                                            .$Valeur >= .$`Minimum-3` ~ "Classe 3",
                                            TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1337-3" | Cl == "1338-3" , # Pour le cas où SANDRE 1337 et 1338 <-> 7 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$Valeur >= .$`Maximum-3` & .$Valeur < .$`Maximum-4` ~ "Classe 4",
                                            .$Valeur >= .$`Maximum-4` & .$Valeur < .$`Maximum-5` ~ "Classe 5",
                                            .$Valeur >= .$`Maximum-5` & .$Valeur < .$`Maximum-6` ~ "Classe 6",
                                            .$Valeur >= .$`Minimum-7` ~ "Classe 7",
                                            TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1339-3", # Pour le cas où SANDRE 1339 <-> 4 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$Valeur >= .$`Minimum-4` ~ "Classe 4",
                                            TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1333-3" | Cl == "1340-3" , # Pour le cas où SANDRE 1333 et 1340 <-> 6 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$Valeur >= .$`Maximum-3` & .$Valeur < .$`Maximum-4` ~ "Classe 4",
                                            .$Valeur >= .$`Maximum-4` & .$Valeur < .$`Maximum-5` ~ "Classe 5",
                                            .$Valeur >= .$`Minimum-6` ~ "Classe 6",
                                            TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(CodeRemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
    mutate(ClasseQualite = ifelse(CodeRemarque == 2 & !is.na(`Referentiel`), "< seuil détection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de détection 
    select(-(`Maximum-1`:`Minimum-8`), -Cl) %>% 
    mutate(Couleur = case_when(.$ClasseQualite == "Classe 1" ~ "Bleu",
                               .$ClasseQualite == "Classe 2" ~ "Vert",
                               .$ClasseQualite == "Classe 3" ~ "Vert clair",
                               .$ClasseQualite == "Classe 4" ~ "Jaune",
                               .$ClasseQualite == "Classe 5" ~ "Orange",
                               .$ClasseQualite == "Classe 6" ~ "Rouge",
                               .$ClasseQualite == "Classe 7" ~ "Violet",
                               .$ClasseQualite == "Classe 8" ~ "Noir",
                               .$ClasseQualite == "< seuil détection" ~ "Gris clair",
                               .$ClasseQualite == "< seuil quantification" ~ "Gris foncé",
                               TRUE ~ "Non évalué"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
}
  
###### SEQ-EAU #####
if(Referentiel == "SEQ-EAU") {
  
  ## Création des seuils ##
  Seuils <- 
    data %>% 
    filter(Referentiel == "SEQ-Eau par altération" & ParametreSANDRE != "1301") %>% 
    bind_rows(filter(data, Referentiel == "SEQ-Eau par altération" & ParametreSANDRE_condition == 2 & Valeur_condition == Categorie)) %>% 
    tidyr::unite(Seuil, c(Seuil, ClasseQualite), remove=T, sep = "-") %>% 
    dcast(Referentiel + ParametreSANDRE + SupportSANDRE ~ Seuil, value.var = "ValeurSeuil") %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=T, sep = "-")
  
  ## Attribution des classes de qualité ##
  ClasseQualites <-
    PC %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=F, sep = "-") %>% 
    left_join(Seuils, by = c("Cl" = "Cl")) %>% 
    mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
    mutate(ClasseQualite = NA) %>% 
    mutate(ClasseQualite = ifelse(is.na(`Maximum-4`) & !is.na(`Maximum-1`), # Pour le cas où il n'y a que 3 seuils - Avec exclusion des na en max-1 pour 1311 et 1312 traités ensuite
                           case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                     .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                     .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                     .$Valeur >= .$`Maximum-3` ~ "Classe 4", # S'il n'existe que 3 limites
                                     #.$Valeur >= .$`Maximum-3` & .$Valeur < .$`Maximum-Classe 4` ~ "Classe 4", # S'il existe 4 limites
                                     #.$Valeur >= .$`Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                     TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                           ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(!is.na(`Maximum-4`), # Pour le cas où il y a 4 seuils
                           case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                     .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                     .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                     #.$Valeur >= .$`Maximum-3` ~ "Classe 4", # S'il n'existe que 3 limites
                                     .$Valeur >= .$`Maximum-3` & .$Valeur < .$`Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                     .$Valeur >= .$`Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                     TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                           ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1311-3" | Cl == "1312-3", # Pour le cas où il y a 4 seuils mais seulement pour 1311 et 1312 (car par minimum et non par maximum)
                                  case_when(.$Valeur < .$`Minimum-4` ~ "Classe 5",
                                            .$Valeur >= .$`Minimum-4` & .$Valeur < .$`Minimum-3` ~ "Classe 4",
                                            .$Valeur >= .$`Minimum-3` & .$Valeur < .$`Minimum-2` ~ "Classe 3",
                                            .$Valeur >= .$`Minimum-2` & .$Valeur < .$`Minimum-1` ~ "Classe 2",
                                            .$Valeur >= .$`Minimum-1` ~ "Classe 1",
                                            TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(CodeRemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
    mutate(ClasseQualite = ifelse(CodeRemarque == 2 & !is.na(`Referentiel`), "< seuil détection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de détection 
    select(-(`Maximum-1`:`Minimum-4`), -Cl) %>% 
    mutate(Couleur = case_when(.$ClasseQualite == "Classe 1" ~ "Bleu",
                               .$ClasseQualite == "Classe 2" ~ "Vert",
                               .$ClasseQualite == "Classe 3" ~ "Jaune",
                               .$ClasseQualite == "Classe 4" ~ "Orange",
                               .$ClasseQualite == "Classe 5" ~ "Rouge",
                               .$ClasseQualite == "< seuil détection" ~ "Gris clair",
                               .$ClasseQualite == "< seuil quantification" ~ "Gris foncé",
                               TRUE ~ "Non évalué"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
}

###### Sédiments Québec #####
  
if(Referentiel == "Quebec") {
  
  ## Création des seuils ##
Seuils <- 
  data %>% 
  filter(Referentiel == "Critères pour l'évaluation de la qualité des sédiments au Québec") %>% 
  tidyr::unite(Seuil, c(Seuil, ClasseQualite), remove=T, sep = "-") %>% 
  dcast(Referentiel + ParametreSANDRE + SupportSANDRE ~ Seuil, value.var = "ValeurSeuil") %>% 
  tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=T, sep = "-")
  
  ## Attribution des classes de qualité ##
ClasseQualites <-
  PC %>% 
  tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=F, sep = "-") %>% 
  left_join(Seuils, by = c("Cl" = "Cl")) %>% 
  mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
  mutate(ClasseQualite = NA) %>% 
  mutate(ClasseQualite = ifelse(is.na(`Minimum-CEF`) & is.na(`Minimum-CEP`) & is.na(`Minimum-CER`) & is.na(`Minimum-CSE`), # Pour le cas où il y a 1 seuil
                             case_when(.$Valeur < .$`Minimum-CEO` ~ "< Concentration d’effets occasionnels",
                                       #.$Valeur >= .$`Minimum-CER` & .$Valeur < .$`Minimum-CSE` ~ "Concentration d’effets rares",
                                       #.$Valeur >= .$`Minimum-CSE` & .$Valeur < .$`Minimum-CEO` ~ "Concentration seuil produisant un effet",
                                       .$Valeur >= .$`Minimum-CEO` ~ "Concentration d’effets occasionnels",
                                       #.$Valeur >= .$`Minimum-CEP` & .$Valeur < .$`Minimum-CEF` ~ "Concentration produisant un effet probable",
                                       #.$Valeur >= .$`Minimum-CEF` ~ "Concentration d’effets fréquents",
                                       TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                             ClasseQualite)
  ) %>% 
  mutate(ClasseQualite = ifelse(!is.na(`Minimum-CEF`), # Pour le cas où il y a 5 seuils
                             case_when(.$Valeur < .$`Minimum-CER` ~ "Concentration sans effet",
                                       .$Valeur >= .$`Minimum-CER` & .$Valeur < .$`Minimum-CSE` ~ "Concentration d’effets rares",
                                       .$Valeur >= .$`Minimum-CSE` & .$Valeur < .$`Minimum-CEO` ~ "Concentration seuil produisant un effet",
                                       .$Valeur >= .$`Minimum-CEO` & .$Valeur < .$`Minimum-CEP` ~ "Concentration d’effets occasionnels",
                                       .$Valeur >= .$`Minimum-CEP` & .$Valeur < .$`Minimum-CEF` ~ "Concentration produisant un effet probable",
                                       .$Valeur >= .$`Minimum-CEF` ~ "Concentration d’effets fréquents",
                                       TRUE ~ "Non évalué"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                             ClasseQualite)
      ) %>% 
  mutate(ClasseQualite = ifelse(CodeRemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
  mutate(ClasseQualite = ifelse(CodeRemarque == 2 & !is.na(`Referentiel`), "< seuil détection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de détection 
  select(-(`Minimum-CEF`:`Minimum-CSE`), -Cl) %>% 
  mutate(Couleur = case_when(.$ClasseQualite == "Concentration sans effet" ~ "Bleu",
                                 .$ClasseQualite == "Concentration d’effets rares" ~ "Vert",
                                 .$ClasseQualite == "Concentration seuil produisant un effet" ~ "Jaune",
                                 .$ClasseQualite == "Concentration d’effets occasionnels" ~ "Orange",
                                 .$ClasseQualite == "Concentration produisant un effet probable" ~ "Rouge",
                                 .$ClasseQualite == "Concentration d’effets fréquents" ~ "Violet",
                                 .$ClasseQualite == "< seuil détection" ~ "Gris clair",
                                 .$ClasseQualite == "< seuil quantification" ~ "Gris foncé",
                                 .$ClasseQualite == "< Concentration d’effets occasionnels" ~ "Vert clair",
                                 TRUE ~ "Non évalué"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
    
}

#### Retour des données ####
  return(ClasseQualites)
  
} # Fin de la fonction