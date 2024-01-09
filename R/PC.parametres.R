#' Reformatage les paramètres physico-chimiques
#'
#' Reformatage les paramètres physico-chimiques avec les noms uniformisés, les codes SANDRE et les unités
#' @name PC.parametres
#' @param data Jeu de données contenant une colonne Parametre ou pcmes_parametrenom
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
# Il faudrait mettre tous les codes dans une table afin de pouvoir faire la conversion dans les deux sens : Paramètre <-> puis application dans PC.figure
#####################

  ## Recherche si les colonnes existent, sinon création
  if(!"pcmes_parametresandre" %in% colnames(data)) data$pcmes_parametresandre <- ""
  if(!"pcmes_unitenom" %in% colnames(data)) data$pcmes_unitenom <- ""
  if(!"pcmes_unitesandre" %in% colnames(data)) data$pcmes_unitesandre <- ""
  if(!"pcmes_parametrenom" %in% colnames(data)) data$pcmes_parametrenom <- ""
  
  ## Si entrée avec une colonne pcmes_parametrenom, renommage temporaire en Parametre ##
  if(!"Parametre" %in% colnames(data)) data$Parametre <- data$pcmes_parametrenom

##### Paramètres génériques #####
data$pcmes_parametresandre[data$Parametre == "Débit"] <- "1420"
data$pcmes_unitenom[data$Parametre == "Débit"] <- "m3/s"
data$pcmes_unitesandre[data$Parametre == "Débit"] <- "117"
data$pcmes_parametrenom[data$Parametre == "Débit"] <- "Débit"

data$pcmes_parametresandre[data$Parametre == "Temp"] <- "1301"
data$pcmes_unitenom[data$Parametre == "Temp"] <- "°C"
data$pcmes_unitesandre[data$Parametre == "Temp"] <- "27"
data$pcmes_parametrenom[data$Parametre == "Temp"] <- "Température"
data$pcmes_unitesandre[data$Parametre == "Température"] <- "27"

data$pcmes_parametresandre[data$Parametre == "T"] <- "1301"
data$pcmes_unitenom[data$Parametre == "T"] <- "°C"
data$pcmes_unitesandre[data$Parametre == "T"] <- "27"
data$pcmes_parametrenom[data$Parametre == "T"] <- "Température"
data$pcmes_unitesandre[data$Parametre == "Température"] <- "27"

data$pcmes_parametresandre[data$Parametre == "ph"] <- "1302"
data$pcmes_unitenom[data$Parametre == "ph"] <- "unité pH"
data$pcmes_unitesandre[data$Parametre == "ph"] <- "264"
data$pcmes_parametrenom[data$Parametre == "ph"] <- "pH"
data$pcmes_unitesandre[data$Parametre == "pH"] <- "264"

data$pcmes_parametresandre[data$Parametre == "O2mg"] <- "1311"
data$pcmes_unitenom[data$Parametre == "O2mg"] <- "mg(O2)/L"
data$pcmes_unitesandre[data$Parametre == "O2mg"] <- "175"
data$pcmes_parametrenom[data$Parametre == "O2mg"] <- "Oxygène dissous"
data$pcmes_unitesandre[data$Parametre == "Oxygène dissous"] <- "175"

data$pcmes_parametresandre[data$Parametre == "Sat"] <- "1312"
data$pcmes_unitenom[data$Parametre == "Sat"] <- "%"
data$pcmes_unitesandre[data$Parametre == "Sat"] <- "243"
data$pcmes_parametrenom[data$Parametre == "Sat"] <- "Oxygène dissous (saturation)"
data$pcmes_unitesandre[data$Parametre == "Oxygène dissous (saturation)"] <- "243"

data$pcmes_parametresandre[data$Parametre == "O2pourc"] <- "1312"
data$pcmes_unitenom[data$Parametre == "O2pourc"] <- "%"
data$pcmes_unitesandre[data$Parametre == "O2pourc"] <- "243"
data$pcmes_parametrenom[data$Parametre == "O2pourc"] <- "Oxygène dissous (saturation)"
data$pcmes_unitesandre[data$Parametre == "Oxygène dissous (saturation)"] <- "243"

data$pcmes_parametresandre[data$Parametre == "Secchi"] <- "1332"
data$pcmes_unitenom[data$Parametre == "Secchi"] <- "m"
data$pcmes_unitesandre[data$Parametre == "Secchi"] <- "111"
data$pcmes_parametrenom[data$Parametre == "Secchi"] <- "Disque de Secchi"
data$pcmes_unitesandre[data$Parametre == "Disque de Secchi"] <- "111"

##### Minéralisation #####

data$pcmes_parametresandre[data$Parametre == "cond"] <- "1303"
data$pcmes_unitenom[data$Parametre == "cond"] <- "µS/cm"
data$pcmes_unitesandre[data$Parametre == "cond"] <- "147"
data$pcmes_parametrenom[data$Parametre == "cond"] <- "Conductivité corrigée à 25°C"
data$pcmes_unitesandre[data$Parametre == "Conductivité corrigée à 25°C"] <- "147"

data$pcmes_parametresandre[data$Parametre == "Cond"] <- "1303"
data$pcmes_unitenom[data$Parametre == "Cond"] <- "µS/cm"
data$pcmes_unitesandre[data$Parametre == "Cond"] <- "147"
data$pcmes_parametrenom[data$Parametre == "Cond"] <- "Conductivité corrigée à 25°C"
data$pcmes_unitesandre[data$Parametre == "Conductivité corrigée à 25°C"] <- "147"

data$pcmes_unitesandre[data$Parametre == "Potentiel d’oxydo-réduction"] <- "476"

data$pcmes_parametresandre[data$Parametre == "TDS"] <- "5541"
data$pcmes_unitenom[data$Parametre == "TDS"] <- "mg(ions chargés mobiles)/L"
data$pcmes_unitesandre[data$Parametre == "TDS"] <- ""
data$pcmes_parametrenom[data$Parametre == "TDS"] <- "Teneur en fraction soluble"
data$pcmes_unitesandre[data$Parametre == "Teneur en fraction soluble"] <- ""

data$pcmes_unitesandre[data$Parametre == "Turbidité"] <- "232"

data$pcmes_parametresandre[data$Parametre == "NH4"] <- "1335"
data$pcmes_unitenom[data$Parametre == "NH4"] <- "mg(NH4)/L"
data$pcmes_unitesandre[data$Parametre == "NH4"] <- "169"
data$pcmes_parametrenom[data$Parametre == "NH4"] <- "Ammonium"
data$pcmes_unitesandre[data$Parametre == "Ammonium"] <- "169"
data$pcmes_unitenom[data$Parametre == "Ammonium"] <- "mg(NH4)/L"

data$pcmes_parametresandre[data$Parametre == "NO2"] <- "1339"
data$pcmes_unitenom[data$Parametre == "NO2"] <- "mg(NO2)/L"
data$pcmes_unitesandre[data$Parametre == "NO2"] <- "171"
data$pcmes_parametrenom[data$Parametre == "NO2"] <- "Nitrites"
data$pcmes_unitesandre[data$Parametre == "Nitrites"] <- "171"
data$pcmes_unitenom[data$Parametre == "Nitrites"] <- "mg(NO2)/L"

data$pcmes_parametresandre[data$Parametre == "NO3"] <- "1340"
data$pcmes_unitenom[data$Parametre == "NO3"] <- "mg(NO3)/L"
data$pcmes_unitesandre[data$Parametre == "NO3"] <- "173"
data$pcmes_parametrenom[data$Parametre == "NO3"] <- "Nitrates"
data$pcmes_unitesandre[data$Parametre == "Nitrates"] <- "173"
data$pcmes_unitenom[data$Parametre == "Nitrates"] <- "mg(NO3)/L"

data$pcmes_unitesandre[data$Parametre == "Azote Kjeldahl"] <- "168"

data$pcmes_unitesandre[data$Parametre == "Azote global"] <- "168"

data$pcmes_parametresandre[data$Parametre == "PO4"] <- "1433"
data$pcmes_unitenom[data$Parametre == "PO4"] <- "mg(PO4)/L"
data$pcmes_unitesandre[data$Parametre == "PO4"] <- "176"
data$pcmes_parametrenom[data$Parametre == "PO4"] <- "Phosphates"
data$pcmes_unitesandre[data$Parametre == "Phosphates"] <- "176"
data$pcmes_unitenom[data$Parametre == "Phosphates"] <- "mg(PO4)/L"

data$pcmes_unitesandre[data$Parametre == "Phosphore total"] <- "177"
data$pcmes_unitenom[data$Parametre == "Phosphore total"] <- "mg(P)/L"

data$pcmes_parametresandre[data$Parametre == "HCO3"] <- "1327"
data$pcmes_unitenom[data$Parametre == "HCO3"] <- "mg(HCO3)/L"
data$pcmes_unitesandre[data$Parametre == "HCO3"] <- "274"
data$pcmes_parametrenom[data$Parametre == "HCO3"] <- "Hydrogénocarbonates"
data$pcmes_unitesandre[data$Parametre == "Hydrogénocarbonates"] <- "274"

data$pcmes_parametresandre[data$Parametre == "SiO3"] <- "1342"
data$pcmes_unitenom[data$Parametre == "SiO3"] <- "mg(SIO3)/L"
data$pcmes_unitesandre[data$Parametre == "SiO3"] <- "273"
data$pcmes_parametrenom[data$Parametre == "SiO3"] <- "Silicates"
data$pcmes_unitesandre[data$Parametre == "Silicates"] <- "273"

data$pcmes_parametresandre[data$Parametre == "SO4"] <- "1338"
data$pcmes_unitenom[data$Parametre == "SO4"] <- "mg(SO4)/L"
data$pcmes_unitesandre[data$Parametre == "SO4"] <- "179"
data$pcmes_parametrenom[data$Parametre == "SO4"] <- "Sulfates"
data$pcmes_unitesandre[data$Parametre == "Sulfates"] <- "179"

data$pcmes_parametresandre[data$Parametre == "Fer"] <- "1366"
data$pcmes_unitenom[data$Parametre == "Fer"] <- "mg(Fe2)/L"
data$pcmes_unitesandre[data$Parametre == "Fer"] <- "309"
data$pcmes_parametrenom[data$Parametre == "Fer"] <- "Fer ferreux (soluble)"
data$pcmes_unitesandre[data$Parametre == "Fer ferreux (soluble)"] <- "309"

data$pcmes_unitesandre[data$Parametre == "MeS"] <- "162"

data$pcmes_unitesandre[data$Parametre == "TAC"] <- "28"

data$pcmes_unitesandre[data$Parametre == "Dureté"] <- "28"

data$pcmes_parametresandre[data$Parametre == "Ca"] <- "1374"
data$pcmes_unitenom[data$Parametre == "Ca"] <- "mg(Ca)/L"
data$pcmes_unitesandre[data$Parametre == "Ca"] <- "292"
data$pcmes_parametrenom[data$Parametre == "Ca"] <- "Calcium"
data$pcmes_unitesandre[data$Parametre == "Calcium"] <- "292"

data$pcmes_unitesandre[data$Parametre == "Magnésium"] <- "320"

data$pcmes_unitesandre[data$Parametre == "Chlorures"] <- "164"

data$pcmes_unitesandre[data$Parametre == "Sodium"] <- "326"

data$pcmes_unitesandre[data$Parametre == "Potassium"] <- "316"

##### Activité biologique #####
data$pcmes_parametresandre[data$Parametre == "fDOM"] <- "7615"
data$pcmes_unitenom[data$Parametre == "fDOM"] <- "ppb"
data$pcmes_unitesandre[data$Parametre == "fDOM"] <- ""
data$pcmes_parametrenom[data$Parametre == "fDOM"] <- "Fluorescence des Matières Organiques Dissoutes aux UV-A"
data$pcmes_unitesandre[data$Parametre == "Fluorescence des Matières Organiques Dissoutes aux UV-A"] <- ""

data$pcmes_parametresandre[data$Parametre == "Chlorophyll"] <- ""
data$pcmes_unitenom[data$Parametre == "Chlorophyll"] <- "µg/L"
data$pcmes_unitesandre[data$Parametre == "Chlorophyll"] <- ""
data$pcmes_parametrenom[data$Parametre == "Chlorophyll"] <- "Chlorophylle"
data$pcmes_unitesandre[data$Parametre == "Chlorophylle a"] <- "133"
data$pcmes_unitesandre[data$Parametre == "Chlorophylle b"] <- "133"
data$pcmes_unitesandre[data$Parametre == "Chlorophylle c"] <- "133"

data$pcmes_unitesandre[data$Parametre == "Phéopigments"] <- "133"

data$pcmes_unitesandre[data$Parametre == "Phycocyanine"] <- "133"

data$pcmes_parametresandre[data$Parametre == "BGA-PC"] <- "7844"
data$pcmes_unitenom[data$Parametre == "BGA-PC"] <- "µg/L"
data$pcmes_unitesandre[data$Parametre == "BGA-PC"] <- ""
data$pcmes_parametrenom[data$Parametre == "BGA-PC"] <- "Phycocyanine"
data$pcmes_unitesandre[data$Parametre == "Phycocyanine"] <- ""

data$pcmes_unitesandre[data$Parametre == "DBO"] <- "175"

data$pcmes_unitesandre[data$Parametre == "DCO"] <- "162"

data$pcmes_unitesandre[data$Parametre == "Oxydabilité à froid"] <- "175"

data$pcmes_unitesandre[data$Parametre == "Carbone organique dissous"] <- "163"

data$pcmes_unitesandre[data$Parametre == "Carbone organique total"] <- "163"

data$pcmes_unitesandre[data$Parametre == "Carbone organique"] <- "163"


#### PARAMETRES VALENTIN ####

data$pcmes_parametresandre[data$Parametre == "SiO2"] <- "1348"
data$pcmes_unitenom[data$Parametre == "SiO2"] <- "mg(SIO2)/L"
data$pcmes_unitesandre[data$Parametre == "SiO2"] <- "273"
data$pcmes_parametrenom[data$Parametre == "SiO2"] <- "Silice"

data$pcmes_parametresandre[data$Parametre == "Fer total"] <- "1393"
data$pcmes_unitenom[data$Parametre == "Fer total"] <- "ppb"
data$pcmes_unitesandre[data$Parametre == "Fer total"] <- "478"
data$pcmes_parametrenom[data$Parametre == "Fer total"] <- "Fer total"

data$pcmes_unitenom[data$Parametre == "Chlorophylle a"] <- "µg/L"
data$pcmes_unitenom[data$Parametre == "Chlorophylle b"] <- "µg/L"
data$pcmes_unitenom[data$Parametre == "Chlorophylle c"] <- "µg/L"
data$pcmes_parametrenom[data$Parametre == "Chlorophylle a"] <- "Chlorophylle a"
data$pcmes_parametrenom[data$Parametre == "Chlorophylle b"] <- "Chlorophylle b"
data$pcmes_parametrenom[data$Parametre == "Chlorophylle c"] <- "Chlorophylle c"
data$pcmes_unitesandre[data$Parametre == "Chlorophylle a"] <- "133"
data$pcmes_unitesandre[data$Parametre == "Chlorophylle b"] <- "133"
data$pcmes_unitesandre[data$Parametre == "Chlorophylle c"] <- "133"

data$pcmes_parametrenom[data$Parametre == "Chlorophylle a active"] <- "Chlorophylle a active"

data$pcmes_parametrenom[data$Parametre == "Pheopigments"] <- "Pheopigments"
data$pcmes_parametresandre[data$Parametre == "Pheopigments"] <- "1436"
data$pcmes_unitenom[data$Parametre == "Pheopigments"] <- "mg/L"
data$pcmes_unitesandre[data$Parametre == "Pheopigments"] <- "133"

data$pcmes_parametrenom[data$Parametre == "Carotenoides"] <- "Carotenoides"
data$pcmes_parametresandre[data$Parametre == "Carotenoides"] <- "1436"
data$pcmes_unitenom[data$Parametre == "Carotenoides"] <- "mg/L"
data$pcmes_unitesandre[data$Parametre == "Carotenoides"] <- "133"

data$pcmes_parametrenom[data$Parametre == "PsurB"] <- "P/B"

data$pcmes_unitesandre[data$Parametre == "P tot"] <- "177"
data$pcmes_parametrenom[data$Parametre == "P tot"] <- "Phosphore total"
data$pcmes_parametresandre[data$Parametre == "P tot"] <- "1350"
data$pcmes_unitenom[data$Parametre == "P tot"] <- "mg(P)/L"

data$pcmes_parametresandre[data$Parametre == "Fer tot"] <- "1393"
data$pcmes_unitesandre[data$Parametre == "Fer tot"] <- "309"
data$pcmes_unitenom[data$Parametre == "Fer tot"] <- "mg(Fe)/L"
data$pcmes_parametrenom[data$Parametre == "Fer tot"] <- "Fer total"

data$pcmes_parametrenom[data$Parametre == "Cl"] <- "Chlorures"
data$pcmes_parametresandre[data$Parametre == "Cl"] <- "1337"
data$pcmes_unitenom[data$Parametre == "Cl"] <- "mg(Cl)/L"
data$pcmes_unitesandre[data$Parametre == "Cl"] <- "162"

data$pcmes_parametrenom[data$Parametre == "MeS"] <- "MeS"
data$pcmes_parametresandre[data$Parametre == "MeS"] <- "1305"
data$pcmes_unitenom[data$Parametre == "MeS"] <- "mg(MS)/L"
data$pcmes_unitesandre[data$Parametre == "MeS"] <- "162"

data$pcmes_parametrenom[data$Parametre == "COD"] <- "Carbone organique dissous"
data$pcmes_parametresandre[data$Parametre == "COD"] <- "7804"
data$pcmes_unitenom[data$Parametre == "COD"] <- "mg(COD)/L"
data$pcmes_unitesandre[data$Parametre == "COD"] <- "163"

data$pcmes_parametrenom[data$Parametre == "COT"] <- "Carbone organique Total"
data$pcmes_parametresandre[data$Parametre == "COT"] <- "1841"
data$pcmes_unitenom[data$Parametre == "COT"] <- "mg(COT)/L"
data$pcmes_unitesandre[data$Parametre == "COT"] <- "163"

data$pcmes_parametrenom[data$Parametre == "N tot"] <- "Azote total"
data$pcmes_parametresandre[data$Parametre == "N tot"] <- "6018"
data$pcmes_unitenom[data$Parametre == "N tot"] <- "mg(N)/L"
data$pcmes_unitesandre[data$Parametre == "N tot"] <- "162"

data$pcmes_parametresandre[data$Parametre == "DBO5"] <- "1313"
data$pcmes_parametrenom[data$Parametre == "DBO5"] <- "Demande Biologique en Oxygene en 5 jours (DBO5)"
data$pcmes_unitenom[data$Parametre == "DBO5"] <- "mg(O2)/L"
data$pcmes_unitesandre[data$Parametre == "DBO5"] <- "175"


data$pcmes_unitesandre[data$Parametre == "Mg"] <- "320"
data$pcmes_parametresandre[data$Parametre == "Mg"] <- "1372"
data$pcmes_unitenom[data$Parametre == "Mg"] <- "mg(Mg)/L"
data$pcmes_parametrenom[data$Parametre == "Mg"] <- "Magnesium"

data$pcmes_parametrenom[data$Parametre == "MeS volatiles"] <- "Matiere en suspension volatiles"
data$pcmes_parametresandre[data$Parametre == "MeS volatiles"] <- "1434"
data$pcmes_unitenom[data$Parametre == "MeS volatiles"] <- "mg/L"
data$pcmes_unitesandre[data$Parametre == "MeS volatiles"] <- "162"

data$pcmes_parametrenom[data$Parametre == "MeS minerales"] <- "Matiere en suspension minerales"
data$pcmes_parametresandre[data$Parametre == "MeS minerales"] <- "6048"
data$pcmes_unitenom[data$Parametre == "MeS minerales"] <- "mg/L"
data$pcmes_unitesandre[data$Parametre == "MeS minerales"] <- "162"

data$pcmes_parametrenom[data$Parametre == "CaMg"] <- "Calcium et Magnesium"
data$pcmes_unitenom[data$Parametre == "CaMg"] <- "mg(CaMg)/L"
data$pcmes_unitesandre[data$Parametre == "CaMg"] <- "162"

data$pcmes_parametrenom[data$Parametre == "PheosurChla"] <- "Rapport Pheopigments sur Chl a"

data$pcmes_parametrenom[data$Parametre == "DCO"] <- "Demande chimique en oxygene"
data$pcmes_parametresandre[data$Parametre == "DCO"] <- "1314"
data$pcmes_unitesandre[data$Parametre == "DCO"] <- "162"
data$pcmes_unitenom[data$Parametre == "DCO"] <- "mg(O2)/L"



##### Vérification de paramètres absents #####

bug <- data %>% filter(is.na(pcmes_parametresandre))

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