# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)
library(hms)
library(rlang)

big_file <- read_csv("LSOA2021 to WARD2023.csv")

data <- read_csv("merged_dataV2.csv")

# select relevant columns
ward_lookup <- big_file %>% select(LSOA21CD, LSOA21NM, WD23CD, WD23NM)

# Function to join LSOA to ward and rename columns
join_ward_info <- function(data, lsoa_code_col, lsoa_name_col, prefix) {
  data %>%
    left_join(ward_lookup, by = setNames("LSOA21CD", lsoa_code_col)) %>%
    rename(!!paste0(prefix, "_Ward_Code") := WD23CD,
           !!paste0(prefix, "_Ward_Name") := WD23NM) %>%
    select(-matches("LSOA21NM|WD23NM"))  # Remove LSOA and Ward names from lookup to avoid duplication
}

# Apply the function to each relevant column pair
data <- join_ward_info(data, "Start_LSOA_Code", "Start_LSOA_Name", "Start")
data <- join_ward_info(data, "End_LSOA_Code", "End_LSOA_Name", "End")
data <- join_ward_info(data, "Home_LSOA_Code", "Home_LSOA_Name", "Home")

# Check the structure of the updated data
str(data)

# Explore the first few rows to confirm successful joins
head(data)

# Exclude LSOA columns and retain all others
data_clean <- data %>%
  select(-c(Start_LSOA_Code, Start_LSOA_Name,
            End_LSOA_Code, End_LSOA_Name,
            Home_LSOA_Code, Home_LSOA_Name))

# Check the structure of the cleaned data
str(data_clean)

# Explore the first few rows to confirm the cleanup
head(data_clean)

# Save the cleaned data to a new RDS file
saveRDS(data_clean, "step1cleaned.RDS")

