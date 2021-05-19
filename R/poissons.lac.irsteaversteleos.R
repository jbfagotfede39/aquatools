#' Transformation du format OFB/INRAE vers format Teleos
#'
#' Cette fonction permet de transformer les données de suivis piscicoles de lacs du format OBF/INRAE vers celui Teleos pour import dans Multifish
#' @name poissons.lac.irsteaversteleos
#' @param data Fichier en entrée (format OFB/INRAE)
#' @keywords poissons
#' @export
#' @import tidyverse
#' @examples
#' poissons.lac.irsteaversteleos("NAS-JB/Études/2016_Ilay-Val/Résultats/Poissons/Résultats_ONEMA/iLAY_2016_irstea_brut.xlsx")

##### -------------- A FAIRE -------------- #####
# 
# -------------- A FAIRE -------------- #

poissons.lac.irsteaversteleos <- function(
  data
)
{
  
  #### Formats de sortie (Teleos) ####
  # À partir du format du lac de Clairvaux 2015 qui fonctionne avec Multifish
  TELEOSsettings <- structure(list(Fishec_Action = "Clairvaux_001", Operator = "Fédé39/Teleos", 
                         Location1 = "Clairvaux_Grand_Lac", Num_Net = "50", Type_Fishing = "Vertical_pelagic_net", 
                         Net_Mesh = "30", `Area_net(m2)` = 41, `Depth_Range(m)` = "001-021", 
                         `Depth_Min(m)` = 0, `Depth_Max(m)` = 20.5, Depth_pelagic = NA, 
                         Coordinates_start_N = "46°34,003'", Coordinates_start_E = "5°44,891'", 
                         WP = 33, GPS = "Lowrance", Coord_format = "WGS_1984", Date_Setting = structure(1442793600, class = c("POSIXct", 
                                                                                                                              "POSIXt"), tzone = "UTC"), Date_Fishing = structure(1442880000, class = c("POSIXct", 
                                                                                                                                                                                                        "POSIXt"), tzone = "UTC"), Fishing_Quality = "OK", Habitat_1 = NA_character_, 
                         Habitat_2 = NA, Observation = "Carambar", `Extra_VNet(m)` = 1.5, 
                         `Length_Elec(m)` = NA_real_, `Width_Elec(m)` = NA_real_, 
                         `Area_Elec(m2)` = NA_real_, Electric_pass = NA_real_, `Secci(m)` = NA, 
                         Temp = 18.6, Created_dt = structure(1452729600, class = c("POSIXct", 
                                                                                   "POSIXt"), tzone = "UTC"), Last_mod_dt = NA, Last_mod_user = "Hervé", 
                         Time_Picking = structure(-2209042800, class = c("POSIXct", 
                                                                         "POSIXt"), tzone = "UTC"), Time_Setting = structure(-2209014600, class = c("POSIXct", 
                                                                                                                                                    "POSIXt"), tzone = "UTC"), Project = "Clairvaux", Location2 = NA, 
                         Location3 = NA, Location4 = NA, Mesh_type = NA, Mesh_measure = NA, 
                         Depth_median = NA, Depth_mean = NA, `Distance _shore1(m)` = NA, 
                         `Distance _shore2(m)` = NA, Coordinates_end_N = NA, Coordinates_end_E = NA, 
                         Fishing_time = NA, Substrate_type_general = NA, Substrate_type_measure = NA, 
                         Substrate_type_min = NA, Substrate_type_max = NA, Catch_altitudem = NA, 
                         Net_depth_IN = NA, Net_depth_OUT = NA), row.names = c(NA, 
                                                                               -1L), class = c("tbl_df", "tbl", "data.frame"))
  
  TELEOSfish <- structure(list(Fishec_Action = "Clairvaux_001", Fishec_Num = NA, 
                               Database_Num = NA, Operator = "HD", Meshmm = 30, Taxa_Code = "COR", 
                               Taxa_Latin = "Coregonus_lavaretus", Lengthmm = 296, Weightg = 189, 
                               Weight_origin = "estimated", Eff_Lot = 1, Type_Lot = "N", 
                               Length_Min = NA_real_, Length_Max = NA_real_, Observation = NA_character_, 
                               Field_Num = NA, Sex = NA, Maturity = NA, Surf_distancem = NA, 
                               Catch_altitudem = 15, Museum = NA, Picture = NA, Created_dt = NA, 
                               Last_mod_user = NA, Last_mod_dt = NA, Local_name = "Coregone", 
                               Genus = "Coregonus", Species = "lavaretus", Taxa_field = NA, 
                               `Final identification` = NA, Operator_field = NA, Operator_final_name = NA, 
                               Comments_Identity = NA, behaviour = NA, Foto_field_numbers = NA, 
                               Folder_field_photos = NA, Fish_source = NA, Fish_origin = NA, 
                               Color_morph = NA, Generation = NA), row.names = c(NA, -1L
                               ), class = c("tbl_df", "tbl", "data.frame"))
  
  #### Importation des données d'entrée ####
  IRSTEAcampagne <- 
    read_excel(adresse.switch(data), sheet = "Campagne") %>% 
    filter(!(row_number() %in% 1:2)) # Pour supprimer les deux premières lignes incomplètes en conservant les titres
  
  IRSTEAPointPose <- 
    read_excel(adresse.switch(data), sheet = "PointPose") %>%
    filter(!(row_number() %in% 1:2)) # Pour supprimer les deux premières lignes incomplètes en conservant les titres
  
  IRSTEAPoisson <- 
    read_excel(adresse.switch(data), sheet = "Poisson") %>% 
    filter(!(row_number() %in% 1:2)) # Pour supprimer les deux premières lignes incomplètes en conservant les titres
  
  IRSTEACaractEngin <- 
    read_excel(adresse.switch(data), sheet = "CaractEngin")
  
  #### Test suite à importation ####
  if(exists("IRSTEAcampagne") == FALSE) stop("Impossible d'importer l'onglet Campagne")
  if(exists("IRSTEAPointPose") == FALSE) stop("Impossible d'importer l'onglet PointPose")
  if(exists("IRSTEAPoisson") == FALSE) stop("Impossible d'importer l'onglet Poisson")
  if(exists("IRSTEACaractEngin") == FALSE) stop("Impossible d'importer l'onglet CaractEngin")
  
  #### Regroupement des tables IRSTEA ####
  IRSTEAsettings <-
    IRSTEAPointPose %>% 
    mutate(CodeOperation = 1) %>% 
    left_join(IRSTEAcampagne %>% mutate(CodeOperation = 1), by = c("CodeOperation")) %>% 
    left_join(IRSTEACaractEngin %>% mutate(Type_Engin = case_when(Nom_Engin == "Filet benthique mult-imailles CEN" ~ "FB",
                                                                  Nom_Engin == "Filet pélagique multi-mailles CEN" ~ "FP",
                                                                  TRUE ~ "Autres")
    ),
    by = c("Type_Engin"))
  
  #### Transformation pour settings ####
  IRSTEAsettingsV2 <- 
    IRSTEAsettings %>% 
    rename(Fishec_Action = Num_Pose) %>% 
    formatage.ecosysteme(Operation = "Simplification", ColonneEntree = "NOM_CARTHAGE", ColonneSortie = "temporaire") %>% 
    mutate(temporaire = str_to_sentence(temporaire)) %>% 
    mutate(Fishec_Action = ifelse(Fishec_Action == "1", paste0(temporaire, "_00", Fishec_Action), Fishec_Action)) %>% 
    select(-temporaire) %>% 
    rename(Operator = CdIntervenantResponsable) %>% 
    rename(Location1 = NOM_CARTHAGE) %>% 
    rename(Num_Net = Nom_Pose) %>% 
    rename(Type_Fishing = Nom_Engin) %>% 
    rename(Net_Mesh = Lst_Maille) %>% 
    rename('Area_net(m2)' = Surf_Unit) %>% 
    rename('Depth_Range(m)' = Strate_Pole) %>% 
    rename('Depth_Min(m)' = Prof_Mini) %>% 
    rename('Depth_Max(m)' = Prof_Maxi) %>% 
    mutate(Depth_pelagic = NA_character_) %>% 
    rename(Coordinates_start_N = Abscisse) %>% 
    rename(Coordinates_start_E = Ordonnee) %>% 
    mutate(WP = NA_character_) %>% 
    mutate(GPS = NA_character_) %>% 
    mutate(Coord_format = "WGS_1984") %>% 
    mutate(Date_Setting = ymd("1899-12-30") + as.numeric(Date_Pose)) %>% 
    mutate(Date_Fishing = ymd("1899-12-30") + as.numeric(Date_Releve)) %>% 
    mutate(Fishing_Quality = paste0(Maille_Kaput, " maille(s) abîmée(s)")) %>% 
    mutate(Habitat_1 = NA_character_) %>% 
    mutate(Habitat_2 = NA_character_) %>% 
    mutate(Observation = paste0(Observation, " - ", Meteo)) %>% 
    mutate('Extra_VNet(m)' = NA_real_) %>% 
    mutate('Length_Elec(m)' = NA_real_) %>% 
    mutate('Width_Elec(m)' = NA_real_) %>% 
    mutate('Area_Elec(m2)' = NA_real_) %>% 
    mutate(Electric_pass = NA_integer_) %>% 
    mutate('Secci(m)' = NA_real_) %>% 
    mutate(Temp = NA_real_) %>% 
    mutate(Created_dt = NA_character_) %>% 
    mutate(Last_mod_dt = NA_character_) %>% 
    mutate(Last_mod_user = NA_character_) %>% 
    mutate(Time_Picking = format(ymd_hms(paste0(Date_Fishing, " 00:00:00")) + seconds(as.numeric(Heure_Releve) * 24 * 3600), format="%H:%M:%S")) %>% 
    mutate(Time_Setting = format(ymd_hms(paste0(Date_Setting, " 00:00:00")) + seconds(as.numeric(Heure_Pose) * 24 * 3600), format="%H:%M:%S")) %>% 
    rename(Project = Code_Lac) %>% 
    mutate(Location2 = NA_character_) %>% 
    mutate(Location3 = NA_character_) %>% 
    mutate(Location4 = NA_character_) %>% 
    mutate(Mesh_type = NA_character_) %>% 
    mutate(Mesh_measure = NA_character_) %>% 
    mutate(Depth_median = NA_character_) %>% 
    mutate(Depth_mean = NA_character_) %>% 
    mutate('Distance _shore1(m)' = NA_character_) %>% 
    mutate('Distance _shore2(m)' = NA_character_) %>% 
    mutate(Coordinates_end_N = NA_character_) %>% 
    mutate(Coordinates_end_E = NA_character_) %>% 
    mutate(Fishing_time = NA_character_) %>% 
    mutate(Substrate_type_general = NA_character_) %>% 
    mutate(Substrate_type_measure = NA_character_) %>% 
    mutate(Substrate_type_min = NA_character_) %>% 
    mutate(Substrate_type_max = NA_character_) %>% 
    mutate(Catch_altitudem = NA_character_) %>% 
    mutate(Net_depth_IN = NA_character_) %>% 
    mutate(Net_depth_OUT = NA_character_) %>% 
    formatage.ecosysteme(Operation = "Simplification", ColonneEntree = "Location1", ColonneSortie = "Location1") %>% 
    formatage.ecosysteme(Operation = "Expansion", ColonneEntree = "Location1", ColonneSortie = "Location1") %>% 
    mutate(Type_Fishing = case_when(Type_Fishing == "Filet benthique mult-imailles CEN" ~ "CEN_benthic_net",
                                    Type_Fishing == "Filet benthique multi-mailles CEN" ~ "CEN_benthic_net",
                                    Type_Fishing == "Filet pélagique multi-mailles CEN" ~ "CEN_pelagic_net"
    )) %>% 
    mutate('Depth_Range(m)' = case_when('Depth_Range(m)' == "0" ~ "Inconnu",
                                        'Depth_Range(m)' == "1" ~ "0-3",
                                        'Depth_Range(m)' == "2" ~ "3-6",
                                        'Depth_Range(m)' == "3" ~ "6-12",
                                        'Depth_Range(m)' == "4" ~ "12-20",
                                        'Depth_Range(m)' == "5" ~ "20-35",
                                        'Depth_Range(m)' == "6" ~ "35-50",
                                        'Depth_Range(m)' == "7" ~ "50-75",
                                        'Depth_Range(m)' == "8" ~ ">75",
                                        'Depth_Range(m)' == "-9" ~ "0-6",
                                        'Depth_Range(m)' == "-10" ~ "6-12",
                                        'Depth_Range(m)' == "-11" ~ "12-18",
                                        'Depth_Range(m)' == "-12" ~ "18-24",
                                        'Depth_Range(m)' == "-13" ~ "24-30",
                                        'Depth_Range(m)' == "-14" ~ "30-36",
                                        'Depth_Range(m)' == "-15" ~ "36-42",
                                        'Depth_Range(m)' == "-16" ~ "42-48",
                                        'Depth_Range(m)' == "-17" ~ "48-54",
                                        'Depth_Range(m)' == "-18" ~ "54-60",
                                        'Depth_Range(m)' == "-19" ~ "60-66",
                                        'Depth_Range(m)' == "-20" ~ "66-72",
                                        'Depth_Range(m)' == "-21" ~ ">72"
    )) %>% 
    select(match(colnames(TELEOSsettings),names(.)))
  
  #### Nom du lac ####
  Lac <- IRSTEAsettingsV2 %>% distinct(Location1)
  if(length(Lac) != 1) stop("Présence de différents écosystèmes dans les settings")
  Lac <-
    Lac %>% 
    formatage.ecosysteme(Operation = "Simplification", ColonneEntree = "Location1", ColonneSortie = "temporaire") %>% 
    select(temporaire) %>% 
    pull()
  
  #### Transformation pour fish ####
  IRSTEAPoissonV2 <- 
    IRSTEAPoisson %>% 
    rename(Fishec_Action = Num_Pose) %>% 
    mutate(temporaire = Lac) %>% 
    mutate(temporaire = str_to_sentence(temporaire)) %>% 
    mutate(Fishec_Action = ifelse(Fishec_Action == "1", paste0(temporaire, "_00", Fishec_Action), Fishec_Action)) %>% 
    select(-temporaire) %>% 
    mutate(Fishec_Num = NA_character_) %>% 
    mutate(Database_Num = NA_character_) %>% 
    mutate(Operator = NA_character_) %>% 
    rename(Meshmm = Maille) %>% 
    mutate(Taxa_Code = Code_Taxon) %>% 
    left_join(poissons.especes("Propre") %>% select(Code, `Nom latin`), by = c("Taxa_Code" = "Code")) %>% 
    rename(Taxa_Latin = `Nom latin`) %>% 
    mutate(Taxa_Latin = ifelse(is.na(Taxa_Code), NA_character_, Taxa_Latin)) %>% 
    mutate(Lengthmm = Taille_Indiv) %>% 
    mutate(Weightg = Pds_Lot) %>% 
    mutate(Weight_origin = case_when(Estim_Pds == 0 ~ "Inconnue",
                                     Estim_Pds == 1 ~ "Réelle",
                                     Estim_Pds == 2 ~ "Estimé",
                                     Estim_Pds == 3 ~ "Estimé autre point")
    ) %>%
    mutate(Eff_Lot = Eff_Lot) %>% 
    mutate(Type_Lot = Type_Lot) %>% 
    mutate(Length_Min = Taille_Min) %>% 
    mutate(Length_Max = Taille_Max) %>% 
    mutate(Observation = paste0("Type longueur : ", Type_Long)) %>% 
    mutate(Field_Num = NA_character_) %>% 
    mutate(Sex = case_when(Sexe == "F" ~ "Femelle",
                           Sexe == "M" ~ "Mâle",
                           Sexe == "N" ~ "Inconnu",
                           Sexe == "R" ~ "Non identifié")
    ) %>%
    mutate(Maturity = Mat_Sexe) %>% 
    mutate(Surf_distancem = NA_character_) %>% 
    mutate(Catch_altitudem = Prof_Capture) %>% 
    mutate(Museum = NA_character_) %>% 
    mutate(Picture = NA_character_) %>% 
    mutate(Created_dt = NA_character_) %>% 
    mutate(Last_mod_user = NA_character_) %>% 
    mutate(Last_mod_dt = NA_character_) %>% 
    mutate(Local_name = NA_character_) %>% 
    left_join(poissons.especes("Complet") %>% select(codeespece, latingenre), by = c("Taxa_Code" = "codeespece")) %>% 
    left_join(poissons.especes("Complet") %>% select(codeespece, latinespece), by = c("Taxa_Code" = "codeespece")) %>% 
    rename(Genus = latingenre) %>% 
    rename(Species = latinespece) %>% 
    mutate(Genus = ifelse(is.na(Taxa_Code), NA_character_, Genus)) %>% 
    mutate(Species = ifelse(is.na(Taxa_Code), NA_character_, Species)) %>% 
    mutate(Taxa_field = NA_character_) %>% 
    mutate(`Final identification` = NA_character_) %>% 
    mutate(Operator_field = NA_character_) %>% 
    mutate(Operator_final_name = NA_character_) %>% 
    mutate(Comments_Identity = NA_character_) %>% 
    mutate(behaviour = NA_character_) %>% 
    mutate(Foto_field_numbers = NA_character_) %>% 
    mutate(Folder_field_photos = NA_character_) %>% 
    mutate(Fish_source = NA_character_) %>% 
    mutate(Fish_origin = NA_character_) %>% 
    mutate(Color_morph = NA_character_) %>% 
    mutate(Generation = NA_character_) %>% 
    select(match(colnames(TELEOSfish),names(.))) %>% 
    mutate(Observation = ifelse(is.na(Taxa_Code), "action_without_fish", Observation)) %>%  # filets sans captures
    mutate(Weight_origin = ifelse(is.na(Taxa_Code), NA_character_, Weight_origin)) %>%  # filets sans captures
    mutate(Sex = ifelse(is.na(Taxa_Code), NA_character_, Sex)) %>%  # filets sans captures
    mutate(Maturity = ifelse(is.na(Taxa_Code), NA_character_, Maturity)) # filets sans captures
  
  #### Tests de cohérence ####
  if(IRSTEAsettingsV2 %>% filter(Type_Fishing == "Autres") %>% nrow() > 0) stop("Présence de types d'engin non définis")
  if(IRSTEAsettingsV2 %>% filter(is.na(Num_Net)) %>% nrow() > 0) stop("Présence de filets non nommés")
  if(IRSTEAsettingsV2 %>% filter(!(Fishec_Action %in% IRSTEAPoissonV2$Fishec_Action)) %>% nrow() > 0) stop("Présence d'actions de pêche sans capture (ou capture vide) en face")
  if(IRSTEAPoissonV2 %>% filter(!(Fishec_Action %in% IRSTEAsettingsV2$Fishec_Action)) %>% nrow() > 0) stop("Présence de capture (ou capture vide) sans action de pêche en face")
  
  #### Exportation ####
  nomfichier <- sub('\\..*$', '', basename(data))
  l <- list(setting = IRSTEAsettingsV2, fish = IRSTEAPoissonV2)
  openxlsx::write.xlsx(l, file = paste0(nomfichier, "_format_import_Multifish.xlsx"))
  
} # Fin de la fonction
