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
#' PC.classes(PC)
#' PC.classes(PC,Referentiel="NV")
#' PC.classes(PC,Referentiel="SEQ-EAU")
#' PC.classes(PC,Referentiel="Quebec")
#' PC.classes(PC,Referentiel="SEQ-EAU",Categorie = 2)

##### TODO LIST #####
# Si mise à jour des paramètres :
# library(readxl);library(tidyverse);data <- read_excel("data/Seuils_PC_V5.xlsx");save(data,file="data/Seuils_PC.RData")
# PC <- PC %>% filter(SupportSANDRE == 6)
# PCsave <- PC
# library(readxl);library(tidyverse);library(reshape2);library(aquatools);data(Seuils_PC);Categorie = 1
# load("data/Seuils_PC.RData")
# 
# Ajout d'un filtre si inférieur au seuil de detection avec commentaire en fonction de la valeur de ce seuil/toxicité
# Notion d'unité : à faire avec la clé : parametre-Matrice-Unite
# Option permettant différentes sorties (couleurs/valeurs etc.) = Ajout d'une option permettant de choisir le mode de sortie (en l'état avec 2 colonnes de + ou bien différents types de matrices de synthèse (valeurs, ClasseQualite, couleurs) comme dans fichier PC_V3.R)
# Complément base de données (ETM SEQ-Eau et Québec ok)
# Intégration base de données seuils dans base de données PC
# Il faudrait qu'il n'y ait pas de NA dans les colonnes ClasseQualite et Referentiel suite au traitement
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
# 
#   ## Connexion à la BDD ##
#   db <- BDD.ouverture(Type = "Physico-chimie")
#   
#   ## Récupération des données ##
#   PC <- tbl(db,"PC") %>% collect()
#   PC <- tbl(db,"PC") %>% collect(n = Inf)
#   
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
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1312-3", # Pour le cas où SANDRE 1312 <-> 6 cas
                                  case_when(.$Valeur < .$`Maximum-6` ~ "Classe 6",
                                            .$Valeur >= .$`Minimum-5` & .$Valeur < .$`Minimum-4` ~ "Classe 5",
                                            .$Valeur >= .$`Minimum-4` & .$Valeur < .$`Minimum-3` ~ "Classe 4",
                                            .$Valeur >= .$`Minimum-3` & .$Valeur < .$`Minimum-2` ~ "Classe 3",
                                            .$Valeur >= .$`Minimum-2` & .$Valeur < .$`Minimum-1` ~ "Classe 2",
                                            .$Valeur >= .$`Minimum-1` ~ "Classe 1",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1313-3", # Pour le cas où SANDRE 1313 <-> 4 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$Valeur >= .$`Minimum-4` ~ "Classe 4",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1335-3", # Pour le cas où SANDRE 1335 <-> 3 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Minimum-3` ~ "Classe 2",
                                            .$Valeur >= .$`Minimum-3` ~ "Classe 3",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
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
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1339-3", # Pour le cas où SANDRE 1339 <-> 4 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$Valeur >= .$`Minimum-4` ~ "Classe 4",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1333-3" | Cl == "1340-3" , # Pour le cas où SANDRE 1333 et 1340 <-> 6 cas
                                  case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                            .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                            .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                            .$Valeur >= .$`Maximum-3` & .$Valeur < .$`Maximum-4` ~ "Classe 4",
                                            .$Valeur >= .$`Maximum-4` & .$Valeur < .$`Maximum-5` ~ "Classe 5",
                                            .$Valeur >= .$`Minimum-6` ~ "Classe 6",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(CodeRemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
    mutate(ClasseQualite = ifelse(CodeRemarque == 2 & !is.na(`Referentiel`), "< seuil detection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de detection 
    select(-(`Maximum-1`:`Minimum-8`), -Cl) %>% 
    mutate(Couleur = case_when(.$ClasseQualite == "Classe 1" ~ "Bleu",
                               .$ClasseQualite == "Classe 2" ~ "Vert",
                               .$ClasseQualite == "Classe 3" ~ "Vert clair",
                               .$ClasseQualite == "Classe 4" ~ "Jaune",
                               .$ClasseQualite == "Classe 5" ~ "Orange",
                               .$ClasseQualite == "Classe 6" ~ "Rouge",
                               .$ClasseQualite == "Classe 7" ~ "Violet",
                               .$ClasseQualite == "Classe 8" ~ "Noir",
                               .$ClasseQualite == "< seuil detection" ~ "Gris clair",
                               .$ClasseQualite == "< seuil quantification" ~ "Gris",
                               TRUE ~ "Pas de classe"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
}
  
###### SEQ-EAU #####
if(Referentiel == "SEQ-EAU") {
  
  ## Création des seuils ##
  Seuils <- 
    data %>% 
    filter(Referentiel == "SEQ-Eau par alteration" & ParametreSANDRE != "1301") %>% 
    filter(!(SupportSANDRE == 3 & (ParametreSANDRE == "1382" | ParametreSANDRE == "1383" | ParametreSANDRE == "1386" | ParametreSANDRE == "1388" | ParametreSANDRE == "1389" | ParametreSANDRE == "1392"))) %>% # Afin d'éliminer certains ETM car dépendants de la dureté donc traitement à part
    bind_rows(filter(data, Referentiel == "SEQ-Eau par alteration" & (ParametreSANDRE_condition == 2 & Valeur_condition == Categorie))) %>% 
    tidyr::unite(Seuil, c(Seuil, ClasseQualite), remove=T, sep = "-") %>% 
    dcast(Referentiel + ParametreSANDRE + SupportSANDRE ~ Seuil, value.var = "ValeurSeuil") %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=T, sep = "-")
  
  # Calcul de la dureté par operation ##
  Durete <-
    PC %>%
    filter(ParametreSANDRE == "1345") %>%
    mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>%
    distinct(CodeRDT, Date, Valeur) %>%
    rename(Durete = Valeur)
  PC <-
    PC %>%
    left_join(Durete)
  
  # Seuils dureté pour cuivre
  SeuilsETMH2O <-
    data %>% 
    filter(SupportSANDRE == 3 & (ParametreSANDRE == "1382" | ParametreSANDRE == "1383" | ParametreSANDRE == "1386" | ParametreSANDRE == "1388" | ParametreSANDRE == "1389" | ParametreSANDRE == "1392")) %>% # Afin de ne considérer que certains métaux sur l'eau
    mutate(Durete = case_when(Seuil_condition == "Maximum" & Valeur_condition == 5 ~ "DureteFaible",
                                     Seuil_condition == "Maximum" & Valeur_condition == 20 ~ "DureteMoyenne",
                                     Seuil_condition == "Minimum" & Valeur_condition == 20 ~ "DureteForte")
    ) %>% 
    tidyr::unite(Seuil, c(Durete, Seuil, ClasseQualite), remove=T, sep = "-") %>% 
    dcast(Referentiel + ParametreSANDRE + SupportSANDRE ~ Seuil, value.var = "ValeurSeuil") %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=T, sep = "-") %>% 
    rename(Referentie = Referentiel)

  ## Attribution des classes de qualité ##
  ClasseQualites <-
    PC %>% 
    tidyr::unite(Cl, c(ParametreSANDRE, SupportSANDRE), remove=F, sep = "-") %>% 
    left_join(Seuils, by = c("Cl" = "Cl")) %>% 
    left_join(SeuilsETMH2O, by = c("Cl" = "Cl")) %>% # Afin de coller les seuils de qualité pour les ETM qui sont fonction de la dureté
    mutate(Referentiel = ifelse(is.na(Referentie), Referentiel, Referentie)) %>% # Afin de remettre la colonne référentiel en une seule commune aux deux jeux de données de seuils
    select(-Referentie) %>% # Afin d'effacer la colonne temporaire
    mutate(Valeur = as.numeric( sub(",", ".", Valeur))) %>% 
    #mutate(Durete = as.numeric( sub(",", ".", Durete))) %>% 
    mutate(ClasseQualite = NA) %>% 
    mutate(ClasseQualite = ifelse(is.na(`Maximum-4`) & !is.na(`Maximum-1`), # Pour le cas où il n'y a que 3 seuils - Avec exclusion des na en max-1 pour 1311 et 1312 traités ensuite
                           case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                     .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                     .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                     .$Valeur >= .$`Maximum-3` ~ "Classe 4", # S'il n'existe que 3 limites
                                     #.$Valeur >= .$`Maximum-3` & .$Valeur < .$`Maximum-Classe 4` ~ "Classe 4", # S'il existe 4 limites
                                     #.$Valeur >= .$`Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                     TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                           ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(!is.na(`Maximum-4`), # Pour le cas où il y a 4 seuils
                           case_when(.$Valeur < .$`Maximum-1` ~ "Classe 1",
                                     .$Valeur >= .$`Maximum-1` & .$Valeur < .$`Maximum-2` ~ "Classe 2",
                                     .$Valeur >= .$`Maximum-2` & .$Valeur < .$`Maximum-3` ~ "Classe 3",
                                     #.$Valeur >= .$`Maximum-3` ~ "Classe 4", # S'il n'existe que 3 limites
                                     .$Valeur >= .$`Maximum-3` & .$Valeur < .$`Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                     .$Valeur >= .$`Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                     TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                           ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1311-3" | Cl == "1312-3", # Pour le cas où il y a 4 seuils mais seulement pour 1311 et 1312 (car par minimum et non par maximum)
                                  case_when(.$Valeur < .$`Minimum-4` ~ "Classe 5",
                                            .$Valeur >= .$`Minimum-4` & .$Valeur < .$`Minimum-3` ~ "Classe 4",
                                            .$Valeur >= .$`Minimum-3` & .$Valeur < .$`Minimum-2` ~ "Classe 3",
                                            .$Valeur >= .$`Minimum-2` & .$Valeur < .$`Minimum-1` ~ "Classe 2",
                                            .$Valeur >= .$`Minimum-1` ~ "Classe 1",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>% 
    mutate(ClasseQualite = ifelse(Cl == "1382-3" | Cl == "1383-3" | Cl == "1386-3" | Cl == "1388-3" | Cl == "1389-3" | Cl == "1392-3", # Pour le cas où il y a 4 seuils mais seulement pour quelques ETM
                                  case_when(Durete < 5 & Valeur < `DureteFaible-Maximum-1` ~ "Classe 1",
                                            Durete < 5 & Valeur >= `DureteFaible-Maximum-1` & Valeur < `DureteFaible-Maximum-2` ~ "Classe 2",
                                            Durete < 5 & Valeur >= `DureteFaible-Maximum-2` & Valeur < `DureteFaible-Maximum-3` ~ "Classe 3",
                                            Durete < 5 & Valeur >= `DureteFaible-Maximum-3` & Valeur < `DureteFaible-Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                            Durete < 5 & Valeur >= `DureteFaible-Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                            Durete >= 5 & Durete < 20 & Valeur < `DureteMoyenne-Maximum-1` ~ "Classe 1",
                                            Durete >= 5 & Durete < 20 & Valeur >= `DureteMoyenne-Maximum-1` & Valeur < `DureteMoyenne-Maximum-2` ~ "Classe 2",
                                            Durete >= 5 & Durete < 20 & Valeur >= `DureteMoyenne-Maximum-2` & Valeur < `DureteMoyenne-Maximum-3` ~ "Classe 3",
                                            Durete >= 5 & Durete < 20 & Valeur >= `DureteMoyenne-Maximum-3` & Valeur < `DureteMoyenne-Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                            Durete >= 5 & Durete < 20 & Valeur >= `DureteMoyenne-Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                            Durete > 20 & Valeur < `DureteForte-Maximum-1` ~ "Classe 1",
                                            Durete > 20 & Valeur >= `DureteForte-Maximum-1` & Valeur < `DureteForte-Maximum-2` ~ "Classe 2",
                                            Durete > 20 & Valeur >= `DureteForte-Maximum-2` & Valeur < `DureteForte-Maximum-3` ~ "Classe 3",
                                            Durete > 20 & Valeur >= `DureteForte-Maximum-3` & Valeur < `DureteForte-Maximum-4` ~ "Classe 4", # S'il existe 4 limites
                                            Durete > 20 & Valeur >= `DureteForte-Maximum-4` ~ "Classe 5", # S'il existe 4 limites
                                            is.na(Durete) ~ "Pas de durete",
                                            TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                                  ClasseQualite)
    ) %>%
    mutate(ClasseQualite = ifelse(CodeRemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
    mutate(ClasseQualite = ifelse(CodeRemarque == 2 & !is.na(`Referentiel`), "< seuil detection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de detection 
    select(-(`Maximum-1`:`Minimum-4`), -Cl, -Durete,-(`DureteFaible-Maximum-1`:`DureteMoyenne-Maximum-4`)) %>% 
    mutate(Couleur = case_when(.$ClasseQualite == "Classe 1" ~ "Bleu",
                               .$ClasseQualite == "Classe 2" ~ "Vert",
                               .$ClasseQualite == "Classe 3" ~ "Jaune",
                               .$ClasseQualite == "Classe 4" ~ "Orange",
                               .$ClasseQualite == "Classe 5" ~ "Rouge",
                               .$ClasseQualite == "< seuil detection" ~ "Gris clair",
                               .$ClasseQualite == "< seuil quantification" ~ "Gris",
                               .$ClasseQualite == "Pas de durete" ~ "Pas de classe",
                               TRUE ~ "Pas de classe"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
}

###### Sédiments Québec #####
  
if(Referentiel == "Quebec") {
  
  ## Création des seuils ##
Seuils <- 
  data %>% 
  filter(Referentiel == "Criteres pour l'evaluation de la qualite des sediments au Quebec") %>% 
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
                             case_when(.$Valeur < .$`Minimum-CEO` ~ "< Concentration effets occasionnels",
                                       #.$Valeur >= .$`Minimum-CER` & .$Valeur < .$`Minimum-CSE` ~ "Concentration effets rares",
                                       #.$Valeur >= .$`Minimum-CSE` & .$Valeur < .$`Minimum-CEO` ~ "Concentration seuil produisant un effet",
                                       .$Valeur >= .$`Minimum-CEO` ~ "Concentration effets occasionnels",
                                       #.$Valeur >= .$`Minimum-CEP` & .$Valeur < .$`Minimum-CEF` ~ "Concentration produisant un effet probable",
                                       #.$Valeur >= .$`Minimum-CEF` ~ "Concentration effets frequents",
                                       TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                             ClasseQualite)
  ) %>% 
  mutate(ClasseQualite = ifelse(!is.na(`Minimum-CEF`), # Pour le cas où il y a 5 seuils
                             case_when(.$Valeur < .$`Minimum-CER` ~ "Concentration sans effet",
                                       .$Valeur >= .$`Minimum-CER` & .$Valeur < .$`Minimum-CSE` ~ "Concentration effets rares",
                                       .$Valeur >= .$`Minimum-CSE` & .$Valeur < .$`Minimum-CEO` ~ "Concentration seuil produisant un effet",
                                       .$Valeur >= .$`Minimum-CEO` & .$Valeur < .$`Minimum-CEP` ~ "Concentration effets occasionnels",
                                       .$Valeur >= .$`Minimum-CEP` & .$Valeur < .$`Minimum-CEF` ~ "Concentration produisant un effet probable",
                                       .$Valeur >= .$`Minimum-CEF` ~ "Concentration effets frequents",
                                       TRUE ~ "Pas de classe"),  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
                             ClasseQualite)
      ) %>% 
  mutate(ClasseQualite = ifelse(CodeRemarque == 10 & !is.na(`Referentiel`), "< seuil quantification", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de quantification
  mutate(ClasseQualite = ifelse(CodeRemarque == 2 & !is.na(`Referentiel`), "< seuil detection", ClasseQualite)) %>%  # Pour compléter les cas inférieurs au seuil de detection 
  select(-(`Minimum-CEF`:`Minimum-CSE`), -Cl) %>% 
  mutate(Couleur = case_when(.$ClasseQualite == "Concentration sans effet" ~ "Bleu",
                                 .$ClasseQualite == "Concentration effets rares" ~ "Vert",
                                 .$ClasseQualite == "Concentration seuil produisant un effet" ~ "Jaune",
                                 .$ClasseQualite == "Concentration effets occasionnels" ~ "Orange",
                                 .$ClasseQualite == "Concentration produisant un effet probable" ~ "Rouge",
                                 .$ClasseQualite == "Concentration effets frequents" ~ "Violet",
                                 .$ClasseQualite == "< seuil detection" ~ "Gris clair",
                                 .$ClasseQualite == "< seuil quantification" ~ "Gris",
                                 .$ClasseQualite == "< Concentration effets occasionnels" ~ "Vert clair",
                                 TRUE ~ "Pas de classe"))  # cette dernière ligne permet d'ajouter ce qu'on veut aux cas qui ne se sont pas présentés
    
}

#### Retour des données ####
  return(ClasseQualites)
  
} # Fin de la fonction
