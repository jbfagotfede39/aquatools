#' Transformation du format OFB/INRAE vers format Teleos
#'
#' Cette fonction permet de transformer les données de suivis piscicoles de lacs du format OFB/INRAE vers celui Teleos pour import dans Multifish
#' @name poissons.plansdeau.ofbversteleos
#' @param data Fichier en entrée (format OFB/INRAE)
#' @keywords poissons
#' @export
#' @import glue
#' @import openxlsx
#' @import tidyverse
#' @importFrom dplyr select
#' @importFrom dplyr setdiff
#' @importFrom dplyr union
#' @examples
#' poissons.plansdeau.ofbversteleos("NAS-JB/Études/2016_Ilay-Val/Résultats/Poissons/Résultats_ONEMA/iLAY_2016_irstea_brut.xlsx")

poissons.plansdeau.ofbversteleos <- function(
  data
)
{
  
  #### Formats de sortie (Teleos) ####
  # À partir du format du lac de Clairvaux 2015 qui fonctionne avec Multifish
  teleos_settings <- structure(list(Fishec_Action = "Clairvaux_001", Operator = "Fédé39/Teleos", 
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
  
  teleos_fish <- structure(list(Fishec_Action = "Clairvaux_001", Fishec_Num = NA, 
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
  
  #### Données de référence ####
  especes <- poissons.especes("Complet")
  
  #### Importation des données d'entrée ####
  ## Ouverture des fichiers ##
  irstea_campagne <- 
    read_excel(adresse.switch(data), sheet = "Campagne") %>% 
    filter(!(row_number() %in% 1:2)) # Pour supprimer les deux premières lignes incomplètes en conservant les titres
  
  irstea_point_pose <- 
    read_excel(adresse.switch(data), sheet = "PointPose") %>%
    filter(!(row_number() %in% 1:2)) # Pour supprimer les deux premières lignes incomplètes en conservant les titres
  
  irstea_poisson <- 
    read_excel(adresse.switch(data), sheet = "Poisson") %>% 
    filter(!(row_number() %in% 1:2)) # Pour supprimer les deux premières lignes incomplètes en conservant les titres
  
  irstea_caract_engin <- 
    read_excel(adresse.switch(data), sheet = "CaractEngin")
  
  ## Demande informations utiles ##
  projection <- readline(prompt = "Projection utilisée : 1 (Lambert 93) ou 2 (WGS 1984) : ")
  if (!(projection == 1 | projection == 2)) {stop("Valeur non disponible")}
  if (projection == 1) {projection <- "Lambert 93"}
  if (projection == 2) {projection <- "WGS_1984"}
  
  #### Test suite à importation ####
  if(exists("irstea_campagne") == FALSE) stop("Impossible d'importer l'onglet Campagne")
  if(exists("irstea_point_pose") == FALSE) stop("Impossible d'importer l'onglet PointPose")
  if(exists("irstea_poisson") == FALSE) stop("Impossible d'importer l'onglet Poisson")
  if(exists("irstea_caract_engin") == FALSE) stop("Impossible d'importer l'onglet CaractEngin")
  
  if(irstea_poisson %>% filter(Type_Lot == "I") %>% filter(is.na(Ident_Lot)) %>% nrow() > 0) stop("Présence de lots I avec des numéros de lots vides")
  
  #### Regroupement des tables IRSTEA ####
  irstea_settings <-
    irstea_point_pose %>% 
    mutate(CodeOperation = 1) %>% 
    left_join(irstea_campagne %>% mutate(CodeOperation = 1), by = c("CodeOperation")) %>% 
    left_join(irstea_caract_engin %>% mutate(Type_Engin = case_when(Nom_Engin == "Filet benthique mult-imailles CEN" ~ "FB",
                                                                  Nom_Engin == "Filet pélagique multi-mailles CEN" ~ "FP",
                                                                  TRUE ~ "Autres")
    ),
    by = c("Type_Engin"))
  
  #### Transformation pour settings ####
  irstea_settings_v2 <-
    irstea_settings %>% 
    rename(Fishec_Action = Num_Pose) %>% 
    formatage.ecosysteme(Operation = "Simplification", ColonneEntree = "NOM_CARTHAGE", ColonneSortie = "temporaire") %>% 
    mutate(temporaire = str_to_sentence(temporaire)) %>% 
    mutate(Fishec_Action = glue("{temporaire}_00{Fishec_Action}")) %>% 
    select(-temporaire) %>% 
    rename(Operator = CdIntervenantResponsable) %>% 
    rename(Location1 = NOM_CARTHAGE) %>% 
    rename(Num_Net = Nom_Pose) %>% 
    rename(Type_Fishing = Nom_Engin) %>% 
    rename(Net_Mesh = Lst_Maille) %>% 
    rename('Area_net(m2)' = Surf_Unit) %>% 
    rename('Depth_Range(m)' = Strate_Pole) %>% 
    mutate('Depth_Min(m)' = round(as.numeric(sub(",", ".", Prof_Mini)), 2)) %>% 
    mutate('Depth_Max(m)' = round(as.numeric(sub(",", ".", Prof_Maxi)), 2)) %>% 
    mutate(Depth_pelagic = NA_character_) %>% 
    mutate(Coordinates_start_E = as.numeric(sub(",", ".", Abscisse))) %>% 
    mutate(Coordinates_start_N = as.numeric(sub(",", ".", Ordonnee))) %>% 
    mutate(WP = NA_character_) %>% 
    mutate(GPS = NA_character_) %>% 
    mutate(Coord_format = projection) %>%
    {if(testit::has_warning(mutate(., Date_Setting = ymd(Date_Pose))) == FALSE) mutate(., Date_Setting = ymd(Date_Pose), .before = c(1)) else .} %>%
    {if(!("Date_Setting" %in% names(.))) mutate(., Date_Setting = ymd("1899-12-30") + as.numeric(Date_Pose), .before = c(1)) else .} %>%
    {if(testit::has_warning(mutate(., Date_Fishing = ymd(Date_Pose))) == FALSE) mutate(., Date_Fishing = ymd(Date_Releve), .before = c(1)) else .} %>%
    {if(!("Date_Fishing" %in% names(.))) mutate(., Date_Fishing = ymd("1899-12-30") + as.numeric(Date_Releve), .before = c(1)) else .} %>%
    # {if (testit::has_warning(ymd(irstea_settings$Date_Setting))) mutate(., Date_Setting = ymd(.$Date_Setting)) else .} %>%
    # mutate(Date_Setting = ymd("1899-12-30") + as.numeric(Date_Pose)) %>% 
    # mutate(Date_Fishing = ymd("1899-12-30") + as.numeric(Date_Releve)) %>% 
    mutate(Maille_Kaput = ifelse(is.na(Maille_Kaput), 0, Maille_Kaput)) %>%
    mutate(Fishing_Quality = glue("{Maille_Kaput} maille(s) abîmée(s)")) %>%
    mutate(Habitat_1 = NA_character_) %>% 
    mutate(Habitat_2 = NA_character_) %>% 
    mutate(Observation = ifelse(is.na(Observation), "", glue(" - {Observation}"))) %>%
    mutate(Observation = glue("Météo : {Meteo}{Observation}")) %>% 
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
    {if("character" %in% class(irstea_settings$Heure_Releve)) mutate(., Time_Picking = format(ymd_hms(glue("{Date_Fishing} 00:00:00")) + seconds(as.numeric(Heure_Releve) * 24 * 3600), format="%H:%M:%S"), .before = c(1)) else .} %>%
    {if("character" %in% class(irstea_settings$Heure_Releve)) mutate(., Time_Setting = format(ymd_hms(glue("{Date_Setting} 00:00:00")) + seconds(as.numeric(Heure_Pose) * 24 * 3600), format="%H:%M:%S"), .before = c(1)) else .} %>%
    {if(!("Time_Picking" %in% names(.))) mutate(., Time_Picking_format_date = ymd_hms(as.character(Heure_Releve)), .before = c(1)) else .} %>%
    {if(!("Time_Picking" %in% names(.))) mutate(., Time_Picking = as.character(format(Time_Picking_format_date, format="%H:%M:%S")), .before = c(1)) else .} %>% 
    {if(!("Time_Setting" %in% names(.))) mutate(., Time_Setting_format_date = ymd_hms(as.character(Heure_Pose)), .before = c(1)) else .} %>%
    {if(!("Time_Setting" %in% names(.))) mutate(., Time_Setting = as.character(format(Time_Setting_format_date, format="%H:%M:%S")), .before = c(1)) else .} %>% 
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
    select(match(colnames(teleos_settings),names(.)))
  
  #### Nom du lac ####
  lac <- irstea_settings_v2 %>% distinct(Location1)
  if(length(lac) != 1) stop("Présence de différents écosystèmes dans les settings")
  lac <-
    lac %>% 
    formatage.ecosysteme(Operation = "Simplification", ColonneEntree = "Location1", ColonneSortie = "temporaire") %>% 
    select(temporaire) %>% 
    pull()
  
  #### Transformation pour fish ####
  ### Filtrage des captures vides ###
  irstea_poisson <-
    irstea_poisson %>% 
    filter(!is.na(Num_Pose))
  
  ### Transformation des lots I en lots G
  lots_a_calculer_tailles_min_max <-
    irstea_poisson %>% 
    filter(Type_Lot == "I") #%>% 
    # filter(is.na(Pds_Lot))
  
  if(lots_a_calculer_tailles_min_max %>% nrow() > 0){
    warning("Attention : les lots I ont été regroupés en lots G afin de n'avoir qu'une taille min et une taille max par lot")
    
    lots_calcules_tailles_min_max <-
      lots_a_calculer_tailles_min_max %>% 
      group_by(Ident_Lot, Code_Taxon) %>% 
      summarise(Taille_Min_bis = min(Taille_Indiv),
                Taille_Max_bis = max(Taille_Indiv)) %>% 
      ungroup() %>%
      mutate(cle = glue("{Ident_Lot}_{Code_Taxon}")) %>% 
      select(-Ident_Lot, -Code_Taxon) %>% 
      left_join(irstea_poisson %>% 
                  filter(Type_Lot == "I") %>% 
                  filter(!is.na(Pds_Lot)) %>% 
                  mutate(cle = glue("{Ident_Lot}_{Code_Taxon}")),
                by = c("cle")) %>% 
      mutate(Taille_Min = Taille_Min_bis) %>% 
      mutate(Taille_Max = Taille_Max_bis) %>% 
      mutate(Taille_Indiv = NA) %>% 
      mutate(Type_Lot = "G") %>% 
      select(match(names(lots_a_calculer_tailles_min_max), names(.)))
    
    irstea_poisson_recalcule <- 
      irstea_poisson %>% 
      setdiff(lots_a_calculer_tailles_min_max) %>% 
      union(lots_calcules_tailles_min_max)
    
    ### Vérifications ###
    n_original <- irstea_poisson %>% summarise(total = sum(as.numeric(Eff_Lot), na.rm = T)) %>% pull() # Les individus des lots I sont déjà dénombrés dans les lots, donc on peut supprimer les NA des lignes individuelles
    n_recalcule <- irstea_poisson_recalcule %>% summarise(total = sum(as.numeric(Eff_Lot))) %>% pull()
    if(n_original != n_recalcule) stop("Il y a une différence d'effectif total des captures après le recalcul des lots I en lots G")
  }
  
  if(lots_a_calculer_tailles_min_max %>% nrow() == 0){
    irstea_poisson_recalcule <- irstea_poisson
  }
  
  ### Regroupement ###
  irstea_poisson_v2 <- 
    irstea_poisson_recalcule %>% 
    rename(Fishec_Action = Num_Pose) %>% 
    mutate(temporaire = lac) %>% 
    mutate(temporaire = str_to_sentence(temporaire)) %>% 
    # mutate(Fishec_Action = ifelse(Fishec_Action == "1", glue("{temporaire}_00{Fishec_Action}"), Fishec_Action)) %>% 
    mutate(Fishec_Action = glue("{temporaire}_00{Fishec_Action}")) %>% 
    select(-temporaire) %>% 
    mutate(Fishec_Num = NA_character_) %>% 
    mutate(Database_Num = NA_character_) %>% 
    mutate(Operator = NA_character_) %>% 
    mutate(Meshmm = as.numeric(sub(",", ".", Maille))) %>% 
    mutate(Taxa_Code = Code_Taxon) %>% 
    left_join(especes %>% select(codeespece, nomlatin), by = c("Taxa_Code" = "codeespece")) %>% 
    rename(Taxa_Latin = nomlatin) %>% 
    mutate(Taxa_Latin = ifelse(is.na(Taxa_Code), NA_character_, Taxa_Latin)) %>% 
    mutate(Lengthmm = as.numeric(Taille_Indiv)) %>% 
    mutate(Weightg = as.numeric(sub(",", ".", Pds_Lot))) %>% 
    mutate(Weight_origin = case_when(Estim_Pds == 0 ~ "Inconnue",
                                     Estim_Pds == 1 ~ "Réelle",
                                     Estim_Pds == 2 ~ "Estimé",
                                     Estim_Pds == 3 ~ "Estimé autre point")
    ) %>%
    mutate(Eff_Lot = as.numeric(Eff_Lot)) %>% 
    mutate(Type_Lot = Type_Lot) %>% 
    mutate(Length_Min = as.numeric(Taille_Min)) %>% 
    mutate(Length_Max = as.numeric(Taille_Max)) %>% 
    mutate(Type_Long = ifelse(is.na(Type_Long), "", Type_Long)) %>%
    mutate(Observation = glue("Type longueur : {Type_Long}")) %>% 
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
    left_join(especes %>% select(codeespece, latingenre), by = c("Taxa_Code" = "codeespece")) %>% 
    left_join(especes %>% select(codeespece, latinespece), by = c("Taxa_Code" = "codeespece")) %>% 
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
    mutate(Observation = ifelse(is.na(Taxa_Code), "action_without_fish", Observation)) %>%  # filets sans captures
    mutate(Weight_origin = ifelse(is.na(Taxa_Code), NA_character_, Weight_origin)) %>%  # filets sans captures
    mutate(Sex = ifelse(is.na(Taxa_Code), NA_character_, Sex)) %>%  # filets sans captures
    mutate(Maturity = ifelse(is.na(Taxa_Code), NA_character_, Maturity)) %>% # filets sans captures
    select(match(colnames(teleos_fish), names(.)))
  
  #### Tests de cohérence ####
  if(irstea_settings_v2 %>% filter(Type_Fishing == "Autres") %>% nrow() > 0) stop("Présence de types d'engin non définis")
  if(irstea_settings_v2 %>% filter(is.na(Num_Net)) %>% nrow() > 0) stop("Présence de filets non nommés")
  if(irstea_settings_v2 %>% filter(!(Fishec_Action %in% irstea_poisson_v2$Fishec_Action)) %>% nrow() > 0) warning("Présence d'actions de pêche sans capture (ou capture vide) en face")
  if(irstea_poisson_v2 %>% filter(!(Fishec_Action %in% irstea_settings_v2$Fishec_Action)) %>% nrow() > 0) stop("Présence de capture (ou capture vide) sans action de pêche en face")
  
  #### Exportation ####
  nomfichier <- sub('\\..*$', '', basename(data))
  
  resultats_poissons <- createWorkbook()
  addWorksheet(resultats_poissons, sheetName = "setting")
  addWorksheet(resultats_poissons, sheetName = "fish")
  writeData(resultats_poissons, "setting", irstea_settings_v2, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  writeData(resultats_poissons, "fish", irstea_poisson_v2, startCol = 1, startRow = 1, colNames = T) # writing content on the left-most column to be merged
  freezePane(resultats_poissons, "setting", firstRow = TRUE) ## shortcut to firstActiveRow = 2
  freezePane(resultats_poissons, "fish", firstRow = TRUE) ## shortcut to firstActiveRow = 2
  
  saveWorkbook(resultats_poissons, glue("{nomfichier}_format_import_Multifish.xlsx"), overwrite = T) # save workbook
  
  
} # Fin de la fonction
