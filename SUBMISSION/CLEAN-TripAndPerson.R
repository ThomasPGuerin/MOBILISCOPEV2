



############### CREATING THE TRIP AND PERSON TABLES FOR D2P ###################
#        CRÉATION DES TABLES DE DÉPLACEMENT ET DE PERSONNE POUR D2P
#                             FRANCAIS/ENGLISH


# EN: This code was an iterative process  of adapting the data to the format
# expected by the D2P function.
# FR: Ce code était un processus itératif d'adaptation des données au format
# attendu par la fonction D2P.





# Loading required libristes
# Chargement des bibliothèques requises
library(stringr)
library(TraMineR)
library(plyr)
library(tidyverse) 
library(Hmisc)
library(dplyr)
library(readr)
library(lubridate)
library(hms)
library(rlang)

######################### CREATING THE TRIP TABLE ##############################
#                   CRÉATION DE LA TABLE DE DÉPLACEMENT


# EN: The trip table columns are partly determined by the input the d2p function
#     takes. The transformations involve both renaming columns and creating 
#     dictionaries to consistently map values to the ones used by the mobiliscope

# FR: Les colonnes de la table de déplacement sont en partie déterminées par 
#     l'entrée de la fonction d2p.Les transformations impliquent à la fois le 
#     renommage des colonnes et la création de dictionnaires pour mapper de 
#     manière cohérente les valeurs à celles utilisées par le mobiliscope


# Variables: ID_IND, ID_ED, LIB_ED, ENQUETE, NDEP, RES_SEC, O_SEC, D_SEC,
#            H_START, M_START, H_END, M_END, D9, D2 = D2A, O_PURPOSE, D5 = D5A, 
#            D_PURPOSE, MODE, MODE_ADH, METH_HA


# Importation de l'étape 1 nettoyée
# Importing the cleaned step 1
trip1 <- read_rds("step1cleaned.RDS")

# Renaming columns to match d2p expected names
# Renommage des colonnes pour correspondre aux noms attendus par d2p
trip1 <- trip1 %>% 
  rename(H_START = Hr_START, M_START = Min_START, H_END = Hr_END, M_END = Min_END,
         D9 = Duration,
         O_SEC = Start_Ward_Code, D_SEC = End_Ward_Code, RES_SEC = Home_Ward_Code)










######################### CREATING THE KAGE VARIABLE ############################
#                        CRÉATION DE LA VARIABLE KAGE

# EN: Creating KAGE with a dictionary
# FR: Création de KAGE avec un dictionnaire
ageband_dict <- c(
  '1' = 0, '2' = 0, 
  '3' = 1, '4' = 1, '5' = 1,
  '6' = 2,
  '7' = 3, '8' = 3, '9' = 3, '10' = 3,
  '11' = 4, '12' = 4
)

trip1$ageband <- as.character(trip1$ageband)
trip1$ageband <- ageband_dict[trip1$ageband]
trip1$ageband <- as.numeric(trip1$ageband)
trip1 <- trip1 %>% rename(KAGE = ageband)






######################### CREATING THE OCC VARIABLE #############################
#                             CRÉATION DE OCC  

# EN: Creating OCC with a dictionary
# FR: Création de OCC avec un dictionnaire
work_dict <- c(
  '2' = 1, '3' = 1, '4' = 1, '5' = 5,
  '6' = 1, '7' = 2, '8' = 2, '9' = 1,
  '10' = 3, '11' = 5, '12' = 1
)

trip1$WorkStatusID <- as.character(trip1$WorkStatusID)
trip1$WorkStatusID <- work_dict[trip1$WorkStatusID] 
trip1$WorkStatusID <- as.numeric(trip1$WorkStatusID)
trip1 <- trip1 %>% rename(OCC = WorkStatusID)

######################### CREATING THE PURPOSE VARIABLE #########################
#                               CRÉATION DU MOTIF  



# EN: The raw purposes for trips correspond to the original purpose of the trip
#     We also need to create a column that corresponds to the purpose of the 
#     return trip

# FR: Les motifs bruts des déplacements correspondent au but initial du 
#     déplacement Nous devons également créer une colonne qui correspond au but 
#     du déplacement de retour

purpose_dict <- c(
  '1' = 2,   # Commuting -> travail
  '2' = 2,   # Business -> travail  
  '3' = 3,   # Education -> études
  '4' = 4,   # Shopping -> achats
  '5' = 6,   # Personal Business -> autre
  '7' = 5,   # Visiting Friends -> loisirs
  '8' = 5,   # Sport and Entertainment -> loisirs
  '9' = 5,   # Holiday and Round Trip -> loisirs
  '61' = 3,  # Escort Education -> études 
  '62' = 6   # Escort Other -> autre
)

trip1$NTSPurposeCode <- as.character(trip1$NTSPurposeCode) 
trip1$Purpose_Category <- purpose_dict[trip1$NTSPurposeCode]

trip1 <- trip1[order(trip1$HouseholdIDNumber, trip1$PersonNumber, trip1$StartTime),]
trip1$D_PURPOSE <- purpose_dict[as.character(trip1$NTSPurposeCode)]
trip1$O_PURPOSE <- NA

for (i in 2:nrow(trip1)) {
  if (trip1$HouseholdIDNumber[i] == trip1$HouseholdIDNumber[i-1] && trip1$PersonNumber[i] == trip1$PersonNumber[i-1]) {
    trip1$O_PURPOSE[i] <- trip1$D_PURPOSE[i-1]
  }
}

trip1$O_PURPOSE[1] <- ifelse(trip1$O_SEC[1] == trip1$RES_SEC[1], 1, trip1$D_PURPOSE[1])
trip1$O_PURPOSE[is.na(trip1$O_PURPOSE)] <- ifelse(trip1$O_SEC[is.na(trip1$O_PURPOSE)] == trip1$RES_SEC[is.na(trip1$O_PURPOSE)], 1, trip1$D_PURPOSE[is.na(trip1$O_PURPOSE)])
trip1$D_PURPOSE[trip1$D_SEC == trip1$RES_SEC] <- 1

head(trip1, 10)







######################### CREATING THE MODE VARIABLE #############################
#                             CRÉATION DU MODE  

# EN: Creating MODE with a dictionary to map the mode of transport to 3 
#     categories:
#     1: public transport, 2: private motorised vehicle, 3: soft transport

# FR: Création de MODE avec un dictionnaire pour mapper le mode de transport en 
#     3 catégories:
#     1: transport public, 2 : véhicule motorisé privé, 3: transport doux

mode_dict <- c(
  '1' = 3,  # Walk -> transport doux
  '2' = 3,  # Bicycle -> transport doux 
  '3' = 2,  # Motorcycle, scooter, moped -> véhicule motorisé privé
  '4' = 2,  # Car or van driver -> véhicule motorisé privé
  '5' = 2,  # Car or van passenger -> véhicule motorisé privé
  '6' = 1,  # Train -> transport public
  '7' = 1,  # Metrolink -> transport public
  '8' = 1,  # Bus, minibus, coach -> transport public
  '9' = 2,  # Taxi, minicab -> véhicule motorisé privé
  '10' = 2  # Other (assuming these are private vehicles unless specified otherwise)
)

trip1$Mainmode2 <- as.character(trip1$Mainmode2) 
trip1$MODE <- mode_dict[trip1$Mainmode2]
trip1$MODE <- as.numeric(trip1$MODE)









################ CREATING THE MORE EXPECTED COLLUMNS BY D2P ####################
#           CRÉATION DE COLONNES SUPPLÉMENTAIRES ATTENDUES PAR D2P

trip1 <- trip1 %>% rename(NDEP = TripNumber)
trip1$ID_IND <- paste(trip1$HouseholdIDNumber, trip1$PersonNumber, sep = "_")

trip1$PAYS <- "UK"
trip1$ZONEGEO <- "MANCHESTER"
trip1$ENQUETE <- "MANCHESTER"
trip1$LIB_ED <- "MANCHESTER, 2022"

trip1 <- trip1 %>%
  group_by(ID_IND) %>%
  mutate(MOBILITE = if_else(all(RES_SEC == O_SEC & O_SEC == D_SEC), 2, 1)) %>%
  ungroup()

head(trip1)

trip1 <- trip1 %>%
  mutate(METH_HA = if_else(MOBILITE == 1, 1, 0))

trip1$D2 <- trip1$O_PURPOSE
trip1$D5 <- trip1$D_PURPOSE

trip1 <- trip1 %>%
  mutate(MODE_ADH = if_else(MODE == 3, 1, if_else(MODE %in% c(1, 2), 0, NA_integer_)))







############################# ENCODING WARDS ###################################
# ENCODAGE NUMERIQUE DES SECTEURS  

# EN: The raw data uses 2021 LSOAs as the sector code, which are too small for 
#     the mobiliscope. We will create a lookup table to map LSOAs to 2023 Wards, 
#     which are more appropriate.For this we use the 2021 LSOA to 2023 Ward 
#     lookup table provided by the ONS



# FR: Les données brutes utilisent les LSOA de 2021 comme code de secteur, qui 
#     sont trop petits pour le mobiliscope. Nous allons créer une table de 
#     recherche pour mapper les LSOA aux Wards de 2023, qui sont plus appropriés. 
#     Pour cela,nous utilisons la table de recherche LSOA 2021 vers Ward 2023 
#     fournie par l'ONS.



# Creating the lookup table
# Création de la table de recherche
unique_sec <- unique(trip1$O_SEC)
sec_lookup <- data.frame(SEC = unique_sec, COG = seq_along(unique_sec))

write.csv(sec_lookup, "NEWsec_lookup.csv")

trip1 <- trip1 %>%
  left_join(sec_lookup, by = c("O_SEC" = "SEC")) %>%
  mutate(O_COG = COG) %>%
  select(-COG) %>%
  
  left_join(sec_lookup, by = c("D_SEC" = "SEC")) %>%
  mutate(D_COG = COG) %>%
  select(-COG) %>%
  
  left_join(sec_lookup, by = c("RES_SEC" = "SEC")) %>%
  mutate(RES_COG = COG) %>%
  select(-COG)

trip1$O_ZF <- trip1$O_COG
trip1$D_ZF <- trip1$D_COG  
trip1$RES_ZF <- trip1$RES_COG

trip1$ZONAGE_SEC <- trip1$RES_SEC
trip1$ZONAGE_COG <- trip1$RES_COG
trip1$ZONAGE_ZF <- trip1$RES_ZF
trip1$ID_ED <- "MANCHESTER, 2022"


# Creation a large file with all formated variables from the raw data

# Création d'un grand fichier avec toutes les variables formatées à partir des
# données brutes

saveRDS(trip1, "EVERYTHING.RDS")



# Selecting relevant columns that conform to the expected input of d2p and 
# saving new trip table


# Sélection des colonnes pertinentes qui correspondent à l'entrée attendue de
# d2p et sauvegarde de la nouvelle table de déplacement
FinalTripTable <- trip1 %>%
  transmute(ID_IND, ID_ED, LIB_ED, ENQUETE,
            NDEP, RES_SEC, O_SEC, D_SEC, 
            H_START, M_START, H_END, M_END, D9,
            D2, O_PURPOSE, D5, D_PURPOSE,
            MODE, MODE_ADH, METH_HA, PAYS, ZONEGEO,
            O_ZF, D_COG, D_ZF, O_COG)

FinalTripTable <- FinalTripTable %>% drop_na()

saveRDS(FinalTripTable, "FINALTRIPTABLEV3.RDS")









######################## CREATING THE PERSON TABLE ##############################
# CRÉATION DE LA TABLE DES PERSONNES

person1 <- read_rds("EVERYTHING.RDS")

person1$P9 <- person1$OCC
person1$DISAB <- trip1$DisabilityLimited
person1$ETH <- trip1$EthnicGroup
person1 <- person1 %>% rename(W_IND = weight.y)
person1 <- person1 %>% rename(SEX = Gender)
person1$ID_SEC <- person1$RES_SEC

# Formatting the individual table
# Mise en forme de la table des individus
indTable <- person1 %>%
  transmute(ID_IND, ID_ED, LIB_ED, ENQUETE,
            RES_SEC, SEX, KAGE, P9, OCC, W_IND,
            MOBILITE, DISAB, ETH, METH_HA, PAYS, ZONEGEO, RES_COG, RES_ZF,
            ID_SEC, ZONAGE_SEC, ZONAGE_COG, ZONAGE_ZF)

indTable <- indTable %>% drop_na()

saveRDS(indTable, "FINALPERSONTABLEV3.RDS")



# The final tables are now ready to be used as input for the D2P function
# Les tables finales sont maintenant prêtes à être utilisées comme entrée pour
# la fonction D2P