# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)
library(hms)
library(rlang)

# Set the directory and define file paths
directory <- "C:/Users/thoma/Desktop/"
demo_file_text <- paste0(directory, "HouseholdPerson 2017-22 - Mobiliscope (FINAL) - values.csv")
demo_file_num <- paste0(directory, "HouseholdPerson 2017-22 - Mobiliscope (FINAL).csv")

# Read the data files
df1 <- read_csv(demo_file_text)
df2 <- read_csv(demo_file_num)

# Corrections and replacements
df2 <- df2 %>%
  mutate(PersonNumber = if_else(HouseholdIDNumber == "BL39404H" & UniqueID == 4941, 2, PersonNumber),
         ageband = if_else(ageband == " ", 0, ageband))
df1 <- df1 %>%
  mutate(ageband = if_else(ageband == " ", "Not Given", ageband))

# Merge the data frames
merged_df <- left_join(df1, df2, by = c("UniqueID", "HouseholdIDNumber", "PersonNumber", "Home_LSOA_Code", "Home_LSOA_Name"))

# Update the function to use `.data` for dynamic column referencing
convert_to_single_value_dict <- function(df, group_column, value_column) {
  group_column <- sym(group_column)
  value_column <- sym(value_column)
  
  mappings <- df %>%
    group_by(!!group_column) %>%
    summarise(values = list(unique(.data[[value_column]])), .groups = "drop")
  
  mappings_dict <- setNames(lapply(mappings$values, function(x) if (length(x) == 1) x else x), mappings[[group_column]])
  return(mappings_dict)
}

# Use the function with dynamic column references
columns <- c("HomeDistrictCode", "Gender", "ageband", "WorkStatusID", "DisabilityLimited", "EthnicGroup")
dicts <- list()
for (column in columns) {
  group_col <- paste0(column, ".y")
  value_col <- paste0(column, ".x")
  dicts[[column]] <- convert_to_single_value_dict(merged_df, group_col, value_col)
}

# Trip file paths and data loading
trip_file_text <- paste0(directory, "Trip 2017-22 - Mobiliscope (FINAL) - values.csv")
trip_file_num <- paste0(directory, "Trip 2017-22 - Mobiliscope (FINAL).csv")

trip_df1 <- read_csv(trip_file_text)
trip_df2 <- read_csv(trip_file_num)

# Add a reference date to StartTime and EndTime
reference_date <- as.Date("2000-01-01")  # Using a placeholder date; adjust as needed
trip_df1 <- trip_df1 %>%
  mutate(StartTime = as.POSIXct(paste(reference_date, format(as_hms(StartTime), "%H:%M:%S")), tz = "UTC"),
         EndTime = as.POSIXct(paste(reference_date, format(as_hms(EndTime), "%H:%M:%S")), tz = "UTC"))

#Convert StartTime and EndTime in trip_df2 similar to trip_df1
trip_df2 <- trip_df2 %>%
  mutate(StartTime = as.POSIXct(paste(reference_date, format(as_hms(StartTime), "%H:%M:%S")), tz = "UTC"),
         EndTime = as.POSIXct(paste(reference_date, format(as_hms(EndTime), "%H:%M:%S")), tz = "UTC"))


# Merge trip data frames - ERROR HERE
trip_merged_df <- left_join(trip_df1, trip_df2, by = c("UniqueID", "HouseholdIDNumber", "PersonNumber", "TripNumber", "Start_LSOA_Code", "Start_LSOA_Name", "End_LSOA_Code", "End_LSOA_Name", "StartTime", "EndTime"))

# Create additional dictionaries for trip data
additional_columns <- c("NTSPurposeCode", "Mainmode2")
for (column in additional_columns) {
  dicts[[column]] <- convert_to_single_value_dict(trip_merged_df, sym(paste0(column, ".y")), sym(paste0(column, ".x")))
}

# Final data merging and preprocessing
data_merged_df <- left_join(trip_df2, df2, by = c("HouseholdIDNumber", "PersonNumber")) %>%
  select(-matches("UniqueID")) %>%
  mutate(across(c(PersonNumber, TripNumber, NTSPurposeCode, Mainmode2, HomeDistrictCode, Gender, ageband, WorkStatusID, DisabilityLimited, EthnicGroup), as.integer)) %>%
  mutate(Hr_START = hour(StartTime),
         Min_START = minute(StartTime),
         Hr_END = hour(EndTime),
         Min_END = minute(EndTime),
         Duration = as.integer(difftime(EndTime, StartTime, units = "mins")))

# Check the resulting data structure
str(data_merged_df)

write_csv(data_merged_df, "C:/Users/thoma/Desktop/merged_dataV2.csv")
