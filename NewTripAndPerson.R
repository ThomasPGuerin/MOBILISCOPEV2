library(stringr)
library(TraMineR)
library(plyr) # mapvalues
library(tidyverse)
library(Hmisc)
library(dplyr)
library(readr)
library(lubridate)
library(hms)
library(rlang)

#CREATING THE TRIPTABLE
# COLUMNS I NEEDID_IND, ID_ED, LIB_ED, ENQUETE, NDEP, RES_SEC, O_SEC, D_SEC, 
# H_START, M_START, H_END, M_END, D9, D2 = D2A, O_PURPOSE, D5 = D5A, D_PURPOSE, 
# MODE, MODE_ADH, METH_HA


#READING THE DATA
trip1 <- read_rds("step1cleaned.RDS")
#tripref <- read_rds("TRIPTABLEV12.RDS")

# renaming Hr_START to H_START and Min_START to M_START and Hr_END to H_END and Min_END to M_END
trip1 <- trip1 %>% rename(H_START = Hr_START, M_START = Min_START, H_END = Hr_END, M_END = Min_END)

# rename Duration to D9
trip1 <- trip1 %>% rename(D9 = Duration)

# rename start ward code to O_SEC and end ward code to D_SEC and home ward code to RES_SEC
trip1 <- trip1 %>% rename(O_SEC = Start_Ward_Code, D_SEC = End_Ward_Code, RES_SEC = Home_Ward_Code)



######################### CREATING THE KAGE VARIABLE ############################
# CREATING KAGE
# Define the ageband dictionary
ageband_dict <- c(
  '1' = 0, '2' = 0, 
  '3' = 1, '4' = 1, '5' = 1,
  '6' = 2,
  '7' = 3, '8' = 3, '9' = 3, '10' = 3,
  '11' = 4, '12' = 5
)


# Ensure the ageband column is character for matching
trip1$ageband <- as.character(trip1$ageband)

# Apply the dictionary to reclassify agebands
trip1$ageband <- ageband_dict[trip1$ageband]

# Convert ageband back to numeric if needed
trip1$ageband <- as.numeric(trip1$ageband)

# rename AGEBAND to KAGE
trip1 <- trip1 %>% rename(KAGE = ageband)




######################### CREATING OCC  ############################

work_dict <- c(
  '2' = 1, 
  '3' = 1, 
  '4' = 1, 
  '5' = 5,
  '6' = 1,
  '7' = 2,
  '8' = 2, 
  '9' = 1,
  '10' = 3,
  '11' = 5,
  '12' = 1
)


# Ensure the WorkStatusID column is character for matching
trip1$WorkStatusID <- as.character(trip1$WorkStatusID)

# Apply the dictionary to reclassify WorkStatusID
trip1$WorkStatusID <- work_dict[trip1$WorkStatusID]

# Convert WorkStatusID back to numeric if needed
trip1$WorkStatusID <- as.numeric(trip1$WorkStatusID)

# rename WorkStatusID to OCC
trip1 <- trip1 %>% rename(OCC = WorkStatusID)



######################### CREATING PURPOSE  ############################

# Define the NTSPurposeCode dictionary
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

# Map NTSPurposeCode to new categories
trip1$NTSPurposeCode <- as.character(trip1$NTSPurposeCode)  # Ensure NTSPurposeCode is character
trip1$Purpose_Category <- purpose_dict[trip1$NTSPurposeCode]



# Sort the data by HouseholdIDNumber, PersonNumber, and StartTime to ensure correct sequence processing
trip1 <- trip1[order(trip1$HouseholdIDNumber, trip1$PersonNumber, trip1$StartTime),]

# Assign D_PURPOSE based on the purpose dictionary
trip1$D_PURPOSE <- purpose_dict[as.character(trip1$NTSPurposeCode)]

# Initialize O_PURPOSE
trip1$O_PURPOSE <- NA

# Loop through each row to assign O_PURPOSE based on the context of each trip
for (i in 2:nrow(trip1)) {
  if (trip1$HouseholdIDNumber[i] == trip1$HouseholdIDNumber[i-1] && trip1$PersonNumber[i] == trip1$PersonNumber[i-1]) {
    # If continuing a sequence of trips by the same person
    trip1$O_PURPOSE[i] <- trip1$D_PURPOSE[i-1]
  }
}

# Assign O_PURPOSE for the first trip of each sequence and any standalone trips
trip1$O_PURPOSE[1] <- ifelse(trip1$O_SEC[1] == trip1$RES_SEC[1], 1, trip1$D_PURPOSE[1])
trip1$O_PURPOSE[is.na(trip1$O_PURPOSE)] <- ifelse(trip1$O_SEC[is.na(trip1$O_PURPOSE)] == trip1$RES_SEC[is.na(trip1$O_PURPOSE)], 1, trip1$D_PURPOSE[is.na(trip1$O_PURPOSE)])

# Handle cases where trips end at home (residential sector)
trip1$D_PURPOSE[trip1$D_SEC == trip1$RES_SEC] <- 1

# Re-examine the dataset for confirmation
head(trip1, 10)






######################### CREATING MODE  ############################


## MODE (MODP en 3 modalités) ---
## 1: transport public ; 2 : véhicule motorisé privé ; 3: transport doux

# Define the mainmode2 dictionary to map original modes to new categories
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

# Apply the dictionary to reclassify Mainmode2 in your dataframe
trip1$Mainmode2 <- as.character(trip1$Mainmode2)  # Ensure mainmode2 is character for matching
trip1$MODE <- mode_dict[trip1$Mainmode2]

# Convert MODE back to numeric if needed
trip1$MODE <- as.numeric(trip1$MODE)

# View the updated data to confirm changes
# head(trip1, n = 10)



# ADDING ALL OTHER COLUMNS TO RUN D2P

#rename trip number to NODEP
trip1 <- trip1 %>% rename(NDEP = TripNumber)

trip1$ID_IND <- paste(trip1$HouseholdIDNumber, trip1$PersonNumber, sep = "_")

# ADD a PAYS column filled with UK
trip1$PAYS <- "UK"

# add a ZONEGEO column willed with MANCHESTER
trip1$ZONEGEO <- "MANCHESTER"

# Create a column called ENQUETE filled with MANCHESTER
trip1$ENQUETE <- "MANCHESTER"

# Create a column called LIB_ED filled with MANCHESTER, 2022
trip1$LIB_ED <- "MANCHESTER, 2022"

# Create a column called MOBILITE filtiering people with 1 trip and no trip (2)
# Group data by ID_IND and analyze trip activity based on sector comparison
trip1 <- trip1 %>%
  group_by(ID_IND) %>%
  mutate(MOBILITE = if_else(all(RES_SEC == O_SEC & O_SEC == D_SEC), 2, 1)) %>%
  ungroup()

# View the updated dataframe to confirm the creation of MOBILITE
head(trip1)

# Add METH_HA column based on MOBILITE
trip1 <- trip1 %>%
  mutate(METH_HA = if_else(MOBILITE == 1, 1, 0))


# Add column called D2 copying values form O_PURPOSE
trip1$D2 <- trip1$O_PURPOSE

# Add column called D5 copying values form D_PURPOSE
trip1$D5 <- trip1$D_PURPOSE



#ADDING MODE_ADH
# Assuming the column 'MODE' exists in 'trip1', add 'MODE_ADH' based on 'MODE'
trip1 <- trip1 %>%
  mutate(MODE_ADH = if_else(MODE == 3, 1, if_else(MODE %in% c(1, 2), 0, NA_integer_)))




################ ENCODING WARDS###########################33
# CREATION OF O_COG, D_COG, RES_COG





# Create a lookup table from unique values in O_SEC
# Assuming tripTable has the columns O_SEC, D_SEC, and possibly RES_SEC
unique_sec <- unique(trip1$O_SEC)
sec_lookup <- data.frame(SEC = unique_sec, COG = seq_along(unique_sec))

#save sec lookup as csv
write.csv(sec_lookup, "NEWsec_lookup.csv")


# Apply the lookup to encode O_SEC, D_SEC, and RES_SEC
trip1 <- trip1 %>%
  left_join(sec_lookup, by = c("O_SEC" = "SEC")) %>%
  mutate(O_COG = COG) %>%
  select(-COG) %>%  # Remove the temporary COG column right after using it
  
  left_join(sec_lookup, by = c("D_SEC" = "SEC")) %>%
  mutate(D_COG = COG) %>%
  select(-COG) %>%  # Remove the temporary COG column right after using it
  
  left_join(sec_lookup, by = c("RES_SEC" = "SEC")) %>%
  mutate(RES_COG = COG) %>%
  select(-COG)  # Remove the temporary COG column right after using it



# New columns O_ZF, D_ZF, and RES_ZF copying O_COG, D_COG, and RES_COG
trip1$O_ZF <- trip1$O_COG
trip1$D_ZF <- trip1$D_COG
trip1$RES_ZF <- trip1$RES_COG



# create new columns ZONAGE_SEC copying values from RES_SEC
trip1$ZONAGE_SEC <- trip1$RES_SEC

# create new columns ZONAGE_COG copying values from RES_COG
trip1$ZONAGE_COG <- trip1$RES_COG

# create new columns ZONAGE_ZF copying values from RES_ZF
trip1$ZONAGE_ZF <- trip1$RES_ZF


# Create column called ID_ED filled with MANCHESTER, 2022
trip1$ID_ED <- "MANCHESTER, 2022"

# save trip 1 as everything RDS
saveRDS(trip1, "EVERYTHING.RDS")

# selecting relevant columns and saving new trip table
## Mise en forme
FinalTripTable <- trip1 %>%
  transmute(ID_IND, ID_ED, LIB_ED, ENQUETE,
            NDEP, RES_SEC, O_SEC, D_SEC,
            H_START, M_START, H_END, M_END, D9,
            D2, O_PURPOSE, D5, D_PURPOSE,
            MODE, MODE_ADH, METH_HA, PAYS, ZONEGEO,
            O_ZF, D_COG, D_ZF, O_COG)

# drop rows with missing values from the final trip table
FinalTripTable <- FinalTripTable %>% drop_na()

## save
saveRDS(FinalTripTable, "FINALTRIPTABLEV3.RDS")







#CREATING THE PERSON TABLE*

person1 <- read_rds("EVERYTHING.RDS")


# create a new column P9 copying OCC values
person1$P9 <- person1$OCC

person1$DISAB <- trip1$DisabilityLimited

person1$ETH <- trip1$EthnicGroup

# renaming weight.x to W_IND
person1 <- person1 %>% rename(W_IND = weight.y)

# rename Gender to Sex
person1 <- person1 %>% rename(SEX = Gender)

# CREATING ID_SEC
person1$ID_SEC <- person1$RES_SEC


## Mise en forme
indTable <- person1 %>%
  transmute(ID_IND, ID_ED, LIB_ED, ENQUETE, 
            RES_SEC, SEX, KAGE, P9, OCC, W_IND, 
            MOBILITE, DISAB, ETH, METH_HA, PAYS, ZONEGEO, RES_COG, RES_ZF, ID_SEC,
            ZONAGE_SEC, ZONAGE_COG, ZONAGE_ZF)

# drop rows with missing values from the individual table
indTable <- indTable %>% drop_na()

## save
saveRDS(indTable, "FINALPERSONTABLEV3.RDS")
