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
if(!"ParametreSANDRE" %in% colnames(data)) data$ParametreSANDRE <- ""
if(!"UniteNom" %in% colnames(data)) data$UniteNom <- ""
if(!"UniteSANDRE" %in% colnames(data)) data$UniteSANDRE <- ""
if(!"ParametreNom" %in% colnames(data)) data$ParametreNom <- ""

data$ParametreSANDRE[data$Parametre == "Débit"] <- "1420"
data$UniteNom[data$Parametre == "Débit"] <- "m3/s"
data$UniteSANDRE[data$Parametre == "Débit"] <- "117"
data$ParametreNom[data$Parametre == "Débit"] <- "Débit"

data$ParametreSANDRE[data$Parametre == "Temp"] <- "1301"
data$UniteNom[data$Parametre == "Temp"] <- "°C"
data$UniteSANDRE[data$Parametre == "Temp"] <- "27"
data$ParametreNom[data$Parametre == "Temp"] <- "Température"
data$UniteSANDRE[data$Parametre == "Température"] <- "27"

data$ParametreSANDRE[data$Parametre == "T"] <- "1301"
data$UniteNom[data$Parametre == "T"] <- "°C"
data$UniteSANDRE[data$Parametre == "T"] <- "27"
data$ParametreNom[data$Parametre == "T"] <- "Température"
data$UniteSANDRE[data$Parametre == "Température"] <- "27"

data$ParametreSANDRE[data$Parametre == "ph"] <- "1302"
data$UniteNom[data$Parametre == "ph"] <- "unité pH"
data$UniteSANDRE[data$Parametre == "ph"] <- "264"
data$ParametreNom[data$Parametre == "ph"] <- "pH"
data$UniteSANDRE[data$Parametre == "pH"] <- "264"

data$ParametreSANDRE[data$Parametre == "O2mg"] <- "1311"
data$UniteNom[data$Parametre == "O2mg"] <- "mg(O2)/L"
data$UniteSANDRE[data$Parametre == "O2mg"] <- "175"
data$ParametreNom[data$Parametre == "O2mg"] <- "Oxygène dissous"
data$UniteSANDRE[data$Parametre == "Oxygène dissous"] <- "175"

data$ParametreSANDRE[data$Parametre == "Sat"] <- "1312"
data$UniteNom[data$Parametre == "Sat"] <- "%"
data$UniteSANDRE[data$Parametre == "Sat"] <- "243"
data$ParametreNom[data$Parametre == "Sat"] <- "Oxygène dissous (saturation)"
data$UniteSANDRE[data$Parametre == "Oxygène dissous (saturation)"] <- "243"

data$ParametreSANDRE[data$Parametre == "O2pourc"] <- "1312"
data$UniteNom[data$Parametre == "O2pourc"] <- "%"
data$UniteSANDRE[data$Parametre == "O2pourc"] <- "243"
data$ParametreNom[data$Parametre == "O2pourc"] <- "Oxygène dissous (saturation)"
data$UniteSANDRE[data$Parametre == "Oxygène dissous (saturation)"] <- "243"

data$ParametreSANDRE[data$Parametre == "Secchi"] <- "1332"
data$UniteNom[data$Parametre == "Secchi"] <- "m"
data$UniteSANDRE[data$Parametre == "Secchi"] <- "111"
data$ParametreNom[data$Parametre == "Secchi"] <- "Disque de Secchi"
data$UniteSANDRE[data$Parametre == "Disque de Secchi"] <- "111"

##### Minéralisation #####

data$ParametreSANDRE[data$Parametre == "cond"] <- "1303"
data$UniteNom[data$Parametre == "cond"] <- "µS/cm"
data$UniteSANDRE[data$Parametre == "cond"] <- "147"
data$ParametreNom[data$Parametre == "cond"] <- "Conductivité corrigée à 25°C"
data$UniteSANDRE[data$Parametre == "Conductivité corrigée à 25°C"] <- "147"

data$ParametreSANDRE[data$Parametre == "Cond"] <- "1303"
data$UniteNom[data$Parametre == "Cond"] <- "µS/cm"
data$UniteSANDRE[data$Parametre == "Cond"] <- "147"
data$ParametreNom[data$Parametre == "Cond"] <- "Conductivité corrigée à 25°C"
data$UniteSANDRE[data$Parametre == "Conductivité corrigée à 25°C"] <- "147"

data$UniteSANDRE[data$Parametre == "Potentiel d’oxydo-réduction"] <- "476"

data$ParametreSANDRE[data$Parametre == "TDS"] <- "5541"
data$UniteNom[data$Parametre == "TDS"] <- "mg(ions chargés mobiles)/L"
data$UniteSANDRE[data$Parametre == "TDS"] <- ""
data$ParametreNom[data$Parametre == "TDS"] <- "Teneur en fraction soluble"
data$UniteSANDRE[data$Parametre == "Teneur en fraction soluble"] <- ""

data$UniteSANDRE[data$Parametre == "Turbidité"] <- "232"

data$ParametreSANDRE[data$Parametre == "NH4"] <- "1335"
data$UniteNom[data$Parametre == "NH4"] <- "mg(NH4)/L"
data$UniteSANDRE[data$Parametre == "NH4"] <- "169"
data$ParametreNom[data$Parametre == "NH4"] <- "Ammonium"
data$UniteSANDRE[data$Parametre == "Ammonium"] <- "169"

data$ParametreSANDRE[data$Parametre == "NO2"] <- "1339"
data$UniteNom[data$Parametre == "NO2"] <- "mg(NO2)/L"
data$UniteSANDRE[data$Parametre == "NO2"] <- "171"
data$ParametreNom[data$Parametre == "NO2"] <- "Nitrites"
data$UniteSANDRE[data$Parametre == "Nitrites"] <- "171"

data$ParametreSANDRE[data$Parametre == "NO3"] <- "1340"
data$UniteNom[data$Parametre == "NO3"] <- "mg(NO3)/L"
data$UniteSANDRE[data$Parametre == "NO3"] <- "173"
data$ParametreNom[data$Parametre == "NO3"] <- "Nitrates"
data$UniteSANDRE[data$Parametre == "Nitrates"] <- "173"

data$UniteSANDRE[data$Parametre == "Azote Kjeldahl"] <- "168"

data$UniteSANDRE[data$Parametre == "Azote global"] <- "168"

data$ParametreSANDRE[data$Parametre == "PO4"] <- "1433"
data$UniteNom[data$Parametre == "PO4"] <- "mg(PO4)/L"
data$UniteSANDRE[data$Parametre == "PO4"] <- "176"
data$ParametreNom[data$Parametre == "PO4"] <- "Phosphates"
data$UniteSANDRE[data$Parametre == "Phosphates"] <- "176"

data$UniteSANDRE[data$Parametre == "Phosphore total"] <- "177"

data$ParametreSANDRE[data$Parametre == "HCO3"] <- "1327"
data$UniteNom[data$Parametre == "HCO3"] <- "mg(HCO3)/L"
data$UniteSANDRE[data$Parametre == "HCO3"] <- "274"
data$ParametreNom[data$Parametre == "HCO3"] <- "Hydrogénocarbonates"
data$UniteSANDRE[data$Parametre == "Hydrogénocarbonates"] <- "274"

data$ParametreSANDRE[data$Parametre == "SiO3"] <- "1342"
data$UniteNom[data$Parametre == "SiO3"] <- "mg(SIO3)/L"
data$UniteSANDRE[data$Parametre == "SiO3"] <- "273"
data$ParametreNom[data$Parametre == "SiO3"] <- "Silicates"
data$UniteSANDRE[data$Parametre == "Silicates"] <- "273"

data$ParametreSANDRE[data$Parametre == "SO4"] <- "1338"
data$UniteNom[data$Parametre == "SO4"] <- "mg(SO4)/L"
data$UniteSANDRE[data$Parametre == "SO4"] <- "179"
data$ParametreNom[data$Parametre == "SO4"] <- "Sulfates"
data$UniteSANDRE[data$Parametre == "Sulfates"] <- "179"

data$ParametreSANDRE[data$Parametre == "Fer"] <- "1366"
data$UniteNom[data$Parametre == "Fer"] <- "mg(Fe2)/L"
data$UniteSANDRE[data$Parametre == "Fer"] <- "309"
data$ParametreNom[data$Parametre == "Fer"] <- "Fer ferreux (soluble)"
data$UniteSANDRE[data$Parametre == "Fer ferreux (soluble)"] <- "309"

data$UniteSANDRE[data$Parametre == "MeS"] <- "162"

data$UniteSANDRE[data$Parametre == "TAC"] <- "28"

data$UniteSANDRE[data$Parametre == "Dureté"] <- "28"

data$ParametreSANDRE[data$Parametre == "Ca"] <- "1374"
data$UniteNom[data$Parametre == "Ca"] <- "mg(Ca)/L"
data$UniteSANDRE[data$Parametre == "Ca"] <- "292"
data$ParametreNom[data$Parametre == "Ca"] <- "Calcium"
data$UniteSANDRE[data$Parametre == "Calcium"] <- "292"

data$UniteSANDRE[data$Parametre == "Magnésium"] <- "320"

data$UniteSANDRE[data$Parametre == "Chlorures"] <- "164"

data$UniteSANDRE[data$Parametre == "Sodium"] <- "326"

data$UniteSANDRE[data$Parametre == "Potassium"] <- "316"

##### Activité biologique #####
data$ParametreSANDRE[data$Parametre == "fDOM"] <- "7615"
data$UniteNom[data$Parametre == "fDOM"] <- "ppb"
data$UniteSANDRE[data$Parametre == "fDOM"] <- ""
data$ParametreNom[data$Parametre == "fDOM"] <- "Fluorescence des Matières Organiques Dissoutes aux UV-A"
data$UniteSANDRE[data$Parametre == "Fluorescence des Matières Organiques Dissoutes aux UV-A"] <- ""

data$ParametreSANDRE[data$Parametre == "Chlorophyll"] <- ""
data$UniteNom[data$Parametre == "Chlorophyll"] <- "µg/L"
data$UniteSANDRE[data$Parametre == "Chlorophyll"] <- ""
data$ParametreNom[data$Parametre == "Chlorophyll"] <- "Chlorophylle"
data$UniteSANDRE[data$Parametre == "Chlorophylle a"] <- "133"
data$UniteSANDRE[data$Parametre == "Chlorophylle b"] <- "133"
data$UniteSANDRE[data$Parametre == "Chlorophylle c"] <- "133"

data$UniteSANDRE[data$Parametre == "Phéopigments"] <- "133"

data$UniteSANDRE[data$Parametre == "Phycocyanine"] <- "133"

data$ParametreSANDRE[data$Parametre == "BGA-PC"] <- "7844"
data$UniteNom[data$Parametre == "BGA-PC"] <- "µg/L"
data$UniteSANDRE[data$Parametre == "BGA-PC"] <- ""
data$ParametreNom[data$Parametre == "BGA-PC"] <- "Phycocyanine"
data$UniteSANDRE[data$Parametre == "Phycocyanine"] <- ""

data$UniteSANDRE[data$Parametre == "DBO"] <- "175"

data$UniteSANDRE[data$Parametre == "DCO"] <- "162"

data$UniteSANDRE[data$Parametre == "Oxydabilité à froid"] <- "175"

data$UniteSANDRE[data$Parametre == "Carbone organique dissous"] <- "163"

data$UniteSANDRE[data$Parametre == "Carbone organique total"] <- "163"

data$UniteSANDRE[data$Parametre == "Carbone organique"] <- "163"


#### PARAMETRES VALENTIN ####

data$ParametreSANDRE[data$Parametre == "SiO2"] <- "1348"
data$UniteNom[data$Parametre == "SiO2"] <- "mg(SIO2)/L"
data$UniteSANDRE[data$Parametre == "SiO2"] <- "273"
data$ParametreNom[data$Parametre == "SiO2"] <- "Silice"

data$ParametreSANDRE[data$Parametre == "Fer total"] <- "1393"
data$UniteNom[data$Parametre == "Fer total"] <- "ppb"
data$UniteSANDRE[data$Parametre == "Fer total"] <- "478"
data$ParametreNom[data$Parametre == "Fer total"] <- "Fer total"

data$UniteNom[data$Parametre == "Chlorophylle a"] <- "µg/L"
data$UniteNom[data$Parametre == "Chlorophylle b"] <- "µg/L"
data$UniteNom[data$Parametre == "Chlorophylle c"] <- "µg/L"
data$ParametreNom[data$Parametre == "Chlorophylle a"] <- "Chlorophylle a"
data$ParametreNom[data$Parametre == "Chlorophylle b"] <- "Chlorophylle b"
data$ParametreNom[data$Parametre == "Chlorophylle c"] <- "Chlorophylle c"
data$UniteSANDRE[data$Parametre == "Chlorophylle a"] <- "133"
data$UniteSANDRE[data$Parametre == "Chlorophylle b"] <- "133"
data$UniteSANDRE[data$Parametre == "Chlorophylle c"] <- "133"

data$ParametreNom[data$Parametre == "Chlorophylle a active"] <- "Chlorophylle a active"

data$ParametreNom[data$Parametre == "Pheopigments"] <- "Pheopigments"
data$ParametreSANDRE[data$Parametre == "Pheopigments"] <-"1436"
data$UniteNom[data$Parametre == "Pheopigments"] <-"mg/L"
data$UniteSANDRE[data$Parametre == "Pheopigments"] <- "133"

data$ParametreNom[data$Parametre == "Carotenoides"] <- "Carotenoides"
data$ParametreSANDRE[data$Parametre == "Carotenoides"] <-"1436"
data$UniteNom[data$Parametre == "Carotenoides"] <-"mg/L"
data$UniteSANDRE[data$Parametre == "Carotenoides"] <- "133"

data$ParametreNom[data$Parametre == "PsurB"] <- "P/B"

data$UniteSANDRE[data$Parametre == "P tot"] <- "177"
data$ParametreNom[data$Parametre == "P tot"]<- "Phosphore total"
data$ParametreSANDRE[data$Parametre == "P tot"] <- "1350"
data$UniteNom[data$Parametre == "P tot"] <- "mg(P)/L"

data$ParametreSANDRE[data$Parametre == "Fer tot"] <- "1393"
data$UniteSANDRE[data$Parametre == "Fer tot"] <- "309"
data$UniteNom[data$Parametre == "Fer tot"] <- "mg(Fe)/L"
data$ParametreNom[data$Parametre == "Fer tot"] <- "Fer total"

data$ParametreNom[data$Parametre == "Cl"]<- "Chlorures"
data$ParametreSANDRE[data$Parametre == "Cl"]<- "1337"
data$UniteNom[data$Parametre == "Cl"]<- "mg(Cl)/L"
data$UniteSANDRE[data$Parametre == "Cl"]<- "162"

data$ParametreNom[data$Parametre == "MeS"]<-"MeS"
data$ParametreSANDRE[data$Parametre == "MeS"]<- "1305"
data$UniteNom[data$Parametre == "MeS"]<-"mg(MS)/L"
data$UniteSANDRE[data$Parametre == "MeS"] <- "162"

data$ParametreNom[data$Parametre == "COD"]<- "Carbone organique dissous"
data$ParametreSANDRE[data$Parametre == "COD"]<- "7804"
data$UniteNom[data$Parametre == "COD"]<- "mg(COD)/L"
data$UniteSANDRE[data$Parametre == "COD"] <- "163"

data$ParametreNom[data$Parametre == "COT"]<- "Carbone organique Total"
data$ParametreSANDRE[data$Parametre == "COT"]<- "1841"
data$UniteNom[data$Parametre == "COT"]<- "mg(COT)/L"
data$UniteSANDRE[data$Parametre == "COT"] <- "163"

data$ParametreNom[data$Parametre == "N tot"]<-"Azote total"
data$ParametreSANDRE[data$Parametre == "N tot"]<- "6018"
data$UniteNom[data$Parametre == "N tot"]<- "mg(N)/L"
data$UniteSANDRE[data$Parametre == "N tot"] <- "162"

data$ParametreSANDRE[data$Parametre == "DBO5"] <- "1313"
data$ParametreNom[data$Parametre == "DBO5"] <- "Demande Biologique en Oxygene en 5 jours (DBO5)"
data$UniteNom[data$Parametre == "DBO5"]<- "mg(O2)/L"
data$UniteSANDRE[data$Parametre == "DBO5"] <- "175"


data$UniteSANDRE[data$Parametre == "Mg"] <- "320"
data$ParametreSANDRE[data$Parametre == "Mg"] <- "1372"
data$UniteNom[data$Parametre == "Mg"] <- "mg(Mg)/L"
data$ParametreNom[data$Parametre == "Mg"] <- "Magnesium"

data$ParametreNom[data$Parametre == "MeS volatiles"]<-"Matiere en suspension volatiles"
data$ParametreSANDRE[data$Parametre == "MeS volatiles"]<-"1434"
data$UniteNom[data$Parametre == "MeS volatiles"]<-"mg/L"
data$UniteSANDRE[data$Parametre == "MeS volatiles"]<-"162"

data$ParametreNom[data$Parametre == "MeS minerales"]<-"Matiere en suspension minerales"
data$ParametreSANDRE[data$Parametre == "MeS minerales"]<-"6048"
data$UniteNom[data$Parametre == "MeS minerales"]<-"mg/L"
data$UniteSANDRE[data$Parametre == "MeS minerales"]<-"162"

data$ParametreNom[data$Parametre == "CaMg"] <- "Calcium et Magnesium"
data$UniteNom[data$Parametre == "CaMg"]<- "mg(CaMg)/L"
data$UniteSANDRE[data$Parametre == "CaMg"]<-"162"

data$ParametreNom[data$Parametre == "PheosurChla"]<- "Rapport Pheopigments sur Chl a"

data$ParametreNom[data$Parametre == "DCO"]<- "Demande chimique en oxygene"
data$ParametreSANDRE[data$Parametre == "DCO"]<- "1314"
data$UniteSANDRE[data$Parametre == "DCO"] <- "162"
data$UniteNom[data$Parametre == "DCO"]<- "mg(O2)/L"



##### Vérification de paramètres absents #####

bug <- data %>% filter(is.na(ParametreSANDRE))

if(dim(bug)[1] > 0 & length(unique(bug$Parametre)) == 1) warning(cat("Paramètre",unique(bug$Parametre),"non inclus dans la fonction"), call. = FALSE)
if(dim(bug)[1] > 0 & length(unique(bug$Parametre)) > 1) warning(cat("Paramètres",unique(bug$Parametre),"non inclus dans la fonction"), call. = FALSE)

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