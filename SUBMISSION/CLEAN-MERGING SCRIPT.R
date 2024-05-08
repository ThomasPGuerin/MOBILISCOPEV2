



######################### FIRST PREPROCESSING SCRIPT ###########################
#                       Premier script de prétraitment


# EN: The raw files were given to us by TFGM in 4 different files. The objective
#     of this script is to combine them to move to the next step.

# FR: Les données brutes nous ont étés donnés en 4 dossiers différents. L'objectif
#     de ce script est de les combiner afin de passer à l'étape suivante.





library(dplyr)
library(readr)
library(lubridate)
library(hms)
library(rlang)


directory <- "C:/Users/thoma/Desktop/"
demo_file_text <- paste0(directory, "HouseholdPerson 2017-22 - Mobiliscope (FINAL) - values.csv")
demo_file_num <- paste0(directory, "HouseholdPerson 2017-22 - Mobiliscope (FINAL).csv")


df1 <- read_csv(demo_file_text)
df2 <- read_csv(demo_file_num)




# EN: Perform corrections and replacements
# FR: Effectuer des corrections et des remplacements
df2 <- df2 %>%
  mutate(PersonNumber = if_else(HouseholdIDNumber == "BL39404H" & UniqueID == 4941, 2, PersonNumber),
         ageband = if_else(ageband == " ", 0, ageband))
df1 <- df1 %>%
  mutate(ageband = if_else(ageband == " ", "Not Given", ageband))


# EN: Merge the data frames
# FR: Fusionner les data frames
merged_df <- left_join(df1, df2, by = c("UniqueID", "HouseholdIDNumber", "PersonNumber", "Home_LSOA_Code", "Home_LSOA_Name"))



# EN: Define a function to convert columns to single-value dictionaries
# FR: Définir une fonction pour convertir les colonnes en dictionnaires à valeur unique
convert_to_single_value_dict <- function(df, group_column, value_column) {
  group_column <- sym(group_column)
  value_column <- sym(value_column)
  mappings <- df %>%
    group_by(!!group_column) %>%
    summarise(values = list(unique(.data[[value_column]])), .groups = "drop")
  mappings_dict <- setNames(lapply(mappings$values, function(x) if (length(x) == 1) x else x), mappings[[group_column]])
  return(mappings_dict)
}




# EN: Use the function to create dictionaries for specified columns
# FR: Utiliser la fonction pour créer des dictionnaires pour les colonnes spécifiées
columns <- c("HomeDistrictCode", "Gender", "ageband", "WorkStatusID", "DisabilityLimited", "EthnicGroup")
dicts <- list()
for (column in columns) {
  group_col <- paste0(column, ".y")
  value_col <- paste0(column, ".x")
  dicts[[column]] <- convert_to_single_value_dict(merged_df, group_col, value_col)
}




# EN: Define trip file paths and load the data
# FR: Définir les chemins d'accès aux fichiers de déplacements et charger les données
trip_file_text <- paste0(directory, "Trip 2017-22 - Mobiliscope (FINAL) - values.csv")
trip_file_num <- paste0(directory, "Trip 2017-22 - Mobiliscope (FINAL).csv")
trip_df1 <- read_csv(trip_file_text)
trip_df2 <- read_csv(trip_file_num)




# EN: Add a reference date to StartTime and EndTime in both trip data frames
# FR: Ajouter une date de référence à StartTime et EndTime dans les deux data frames de déplacements
reference_date <- as.Date("2000-01-01")
trip_df1 <- trip_df1 %>%
  mutate(StartTime = as.POSIXct(paste(reference_date, format(as_hms(StartTime), "%H:%M:%S")), tz = "UTC"),
         EndTime = as.POSIXct(paste(reference_date, format(as_hms(EndTime), "%H:%M:%S")), tz = "UTC"))
trip_df2 <- trip_df2 %>%
  mutate(StartTime = as.POSIXct(paste(reference_date, format(as_hms(StartTime), "%H:%M:%S")), tz = "UTC"),
         EndTime = as.POSIXct(paste(reference_date, format(as_hms(EndTime), "%H:%M:%S")), tz = "UTC"))



# EN: Merge trip data frames
# FR: Fusionner les data frames de déplacements
trip_merged_df <- left_join(trip_df1, trip_df2, by = c("UniqueID", "HouseholdIDNumber", "PersonNumber", "TripNumber", "Start_LSOA_Code", "Start_LSOA_Name", "End_LSOA_Code", "End_LSOA_Name", "StartTime", "EndTime"))



# EN: Create additional dictionaries for trip data
# FR: Créer des dictionnaires supplémentaires pour les données de déplacements
additional_columns <- c("NTSPurposeCode", "Mainmode2")
for (column in additional_columns) {
  dicts[[column]] <- convert_to_single_value_dict(trip_merged_df, sym(paste0(column, ".y")), sym(paste0(column, ".x")))
}



# EN: Perform final data merging and preprocessing
# FR: Effectuer la fusion finale des données et le prétraitement
data_merged_df <- left_join(trip_df2, df2, by = c("HouseholdIDNumber", "PersonNumber")) %>%
  select(-matches("UniqueID")) %>%
  mutate(across(c(PersonNumber, TripNumber, NTSPurposeCode, Mainmode2, HomeDistrictCode, Gender, ageband, WorkStatusID, DisabilityLimited, EthnicGroup), as.integer),
         Hr_START = hour(StartTime),
         Min_START = minute(StartTime),
         Hr_END = hour(EndTime),
         Min_END = minute(EndTime),
         Duration = as.integer(difftime(EndTime, StartTime, units = "mins")))




# EN: Check the resulting data structure
# FR: Vérifier la structure des données résultantes
str(data_merged_df)



# EN: Write the merged data to a CSV file
# FR: Écrire les données fusionnées dans un fichier CSV
write_csv(data_merged_df, "C:/Users/thoma/Desktop/merged_dataV2.csv")








