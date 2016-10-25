#' Reformatage les paramètres physico-chimiques
#'
#' Reformatage les paramètres physico-chimiques avec les noms uniformisés, les codes SANDRE et les unités
#' 
#' @param data Jeu de données contenant une colonne Parametre
#' @keywords donnees
#' @import tidyverse
#' @export
#' @examples
#' PC.parametres(data)

PC.parametres <- function(data)
{
  
##### TODO LIST #####
# Extraction du CodeRDT et transformation en nom d'écosystème à l'aide de la fonction format.abreviation()
# Nécessité d'arrondir ? Oui mais compliqué car il ne faut pas faire les mêmes arrondis en fonction des paramètres...
#####################

  
##### Paramètres génériques #####
data$ParametreSANDRE <- ""
data$UniteNom <- ""
data$ParametreNom <- ""

data$ParametreSANDRE[data$Parametre == "Temp"] <- "1301"
data$UniteNom[data$Parametre == "Temp"] <- "°C"
data$ParametreNom[data$Parametre == "Temp"] <- "Température"

data$ParametreSANDRE[data$Parametre == "T"] <- "1301"
data$UniteNom[data$Parametre == "T"] <- "°C"
data$ParametreNom[data$Parametre == "T"] <- "Température"

data$ParametreSANDRE[data$Parametre == "ph"] <- "1302"
data$UniteNom[data$Parametre == "ph"] <- "unité pH"
data$ParametreNom[data$Parametre == "ph"] <- "pH"

data$ParametreSANDRE[data$Parametre == "O2mg"] <- "1311"
data$UniteNom[data$Parametre == "O2mg"] <- "mg(O2)/L"
data$ParametreNom[data$Parametre == "O2mg"] <- "Oxygène dissous"

data$ParametreSANDRE[data$Parametre == "Sat"] <- "1312"
data$UniteNom[data$Parametre == "Sat"] <- "%"
data$ParametreNom[data$Parametre == "Sat"] <- "Oxygène dissous (saturation)"

data$ParametreSANDRE[data$Parametre == "O2pourc"] <- "1312"
data$UniteNom[data$Parametre == "O2pourc"] <- "%"
data$ParametreNom[data$Parametre == "O2pourc"] <- "Oxygène dissous (saturation)"

data$ParametreSANDRE[data$Parametre == "Secchi"] <- "1332"
data$UniteNom[data$Parametre == "Secchi"] <- "m"
data$ParametreNom[data$Parametre == "Secchi"] <- "Disque de Secchi"

##### Minéralisation #####

data$ParametreSANDRE[data$Parametre == "cond"] <- "1303"
data$UniteNom[data$Parametre == "cond"] <- "µS/cm"
data$ParametreNom[data$Parametre == "cond"] <- "Conductivité corrigée à 25°C"

data$ParametreSANDRE[data$Parametre == "Cond"] <- "1303"
data$UniteNom[data$Parametre == "Cond"] <- "µS/cm"
data$ParametreNom[data$Parametre == "Cond"] <- "Conductivité corrigée à 25°C"

data$ParametreSANDRE[data$Parametre == "TDS"] <- "5541"
data$UniteNom[data$Parametre == "TDS"] <- "mg(ions chargés mobiles)/L"
data$ParametreNom[data$Parametre == "TDS"] <- "Teneur en fraction soluble"

data$ParametreSANDRE[data$Parametre == "fDOM"] <- "7615"
data$UniteNom[data$Parametre == "fDOM"] <- "ppb"
data$ParametreNom[data$Parametre == "fDOM"] <- "Fluorescence des Matières Organiques Dissoutes aux UV-A"

data$ParametreSANDRE[data$Parametre == "Ca"] <- "1374"
data$UniteNom[data$Parametre == "Ca"] <- "mg(Ca)/L"
data$ParametreNom[data$Parametre == "Ca"] <- "Calcium"

data$ParametreSANDRE[data$Parametre == "NH4"] <- "1335"
data$UniteNom[data$Parametre == "NH4"] <- "mg(NH4)/L"
data$ParametreNom[data$Parametre == "NH4"] <- "Ammonium"

data$ParametreSANDRE[data$Parametre == "HCO3"] <- "1327"
data$UniteNom[data$Parametre == "HCO3"] <- "mg(HCO3)/L"
data$ParametreNom[data$Parametre == "HCO3"] <- "Hydrogénocarbonates"

data$ParametreSANDRE[data$Parametre == "SiO3"] <- "1342"
data$UniteNom[data$Parametre == "SiO3"] <- "mg(SIO3)/L"
data$ParametreNom[data$Parametre == "SiO3"] <- "Silicates"

data$ParametreSANDRE[data$Parametre == "SO4"] <- "1338"
data$UniteNom[data$Parametre == "SO4"] <- "mg(SO4)/L"
data$ParametreNom[data$Parametre == "SO4"] <- "Sulfates"

data$ParametreSANDRE[data$Parametre == "NO2"] <- "1339"
data$UniteNom[data$Parametre == "NO2"] <- "mg(NO2)/L"
data$ParametreNom[data$Parametre == "NO2"] <- "Nitrites"

data$ParametreSANDRE[data$Parametre == "NO3"] <- "1340"
data$UniteNom[data$Parametre == "NO3"] <- "mg(NO3)/L"
data$ParametreNom[data$Parametre == "NO3"] <- "Nitrates"

data$ParametreSANDRE[data$Parametre == "PO4"] <- "1433"
data$UniteNom[data$Parametre == "PO4"] <- "mg(PO4)/L"
data$ParametreNom[data$Parametre == "PO4"] <- "Phosphates"

data$ParametreSANDRE[data$Parametre == "Fer"] <- "1366"
data$UniteNom[data$Parametre == "Fer"] <- "mg(Fe2)/L"
data$ParametreNom[data$Parametre == "Fer"] <- "Fer ferreux (soluble)"

##### Activité biologique #####
data$ParametreSANDRE[data$Parametre == "Chlorophyll"] <- ""
data$UniteNom[data$Parametre == "Chlorophyll"] <- "µg/L"
data$ParametreNom[data$Parametre == "Chlorophyll"] <- "Chlorophylle"

data$ParametreSANDRE[data$Parametre == "BGA-PC"] <- "7844"
data$UniteNom[data$Parametre == "BGA-PC"] <- "µg/L"
data$ParametreNom[data$Parametre == "BGA-PC"] <- "Phycocyanine"

##### Vérification de paramètres absents #####

bug <- data %>% filter(is.na(ParametreSANDRE))

if(dim(bug)[1] > 0 & length(unique(bug$Parametre)) == 1) stop(cat("Paramètre",unique(bug$Parametre),"non inclus dans la fonction"), call. = FALSE)
if(dim(bug)[1] > 0 & length(unique(bug$Parametre)) > 1) stop(cat("Paramètres",unique(bug$Parametre),"non inclus dans la fonction"), call. = FALSE)

##### Renommage des colonnes #####
if("Lac" %in% names(data)){
  data <- 
    data %>% 
    rename(CodeRDT = Lac)
}

if("Resultat" %in% names(data)){
  data <- 
    data %>% 
    rename(Valeur = Resultat)
}

##### Renvoi des données #####
data <-
  data %>% 
  select(-Parametre)
return(data)
}