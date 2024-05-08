



######################### CREATING THE GPKG FOR P2M ###########################
#                         CREATION DU GPKG POUR P2M


# EN: The GPKG is created through using a shape file from the ONS that contains
#     all the geometries for all wards in the UK. The GPKG is then merged with
#     the lookup table for numerically encoded wards. Columns that p2m excpects are
#     are also to the file as needed, as they are necessary for the function to
#     run

# FR: Le GPKG est créé en utilisant un fichier shape de l'ONS qui contient
#     toutes les géométries de toutes les Wards (circonscriptions) du Royaume-Uni. 
#     Le GPKG est ensuite fusionné avec la table de recherche pour les Wards codées 
#     numériquement. Les colonnes que p2m attend sont également ajoutées au fichier
#     selon les besoins, car elles sont nécessaires pour que la fonction s'exécute.



library(tidyr)
library(dplyr) 
library(readr)
library(sf)
library(st)
library(sda)
library(tidyverse)
library(readxl)

# EN: Read the lookup table from a CSV file
# FR: Lire la table de correspondance à partir d'un fichier CSV
df <- read_csv("NEWsec_lookup.csv")


# EN: Drop the first column
# FR: Supprimer la première colonne
df <- df[,-1]


# EN: Rename the 'SEC' column to 'WD23CD'
# FR: Renommer la colonne 'SEC' en 'WD23CD'
df <- df %>% rename(WD23CD = SEC)


# EN: Read the shapefile
# FR: Lire le fichier de forme (shapefile)
uk_data <- st_read("WD_DEC_2023_UK_BGC.shp")


# EN: List layers in the shapefile
# FR: Lister les couches dans le fichier de forme
st_layers("WD_DEC_2023_UK_BGC.shp")


# EN: Join the lookup table with the shapefile data
# FR: Joindre la table de correspondance avec les données du fichier de forme
sec <- left_join(df, uk_data, by = "WD23CD")


# EN: Write the merged data to a geopackage (GPKG) file
# FR: Écrire les données fusionnées dans un fichier geopackage (GPKG)
st_write(merged_data, "output_data.gpkg", layer = "secteur_60ed_w84", delete_layer = FALSE, driver = "GPKG")


# EN: Read the geopackage file
# FR: Lire le fichier geopackage
sec <- st_read(paste0(cheminIn, "/BD_geo/bdgeo_v4-3.gpkg"), "secteur_60ed_w84")


# EN: Add columns with specific values
# FR: Ajouter des colonnes avec des valeurs spécifiques
sec$CITYKEY <- "MANCHESTER"
sec$ENQUETE <- "MANCHESTER"
sec$ANNEE <- 2022
sec$CODE_SEC <- sec$COG
sec$LIB <- "MANCHESTER, 2022"
sec$ZONAGE_SEC <- sec$COG
sec$AREA <- sec$WD23NM
sec$X_W84 <- sec$LONG
sec$Y_W84 <- sec$LAT


# EN: Drop the empty column 'WD23NMW'
# FR: Supprimer la colonne vide 'WD23NMW'
sec <- sec %>% select(-WD23NMW)


# EN: Write the updated data to the geopackage file
# FR: Écrire les données mises à jour dans le fichier geopackage
st_write(sfSec, "BD_geo/bdgeo_v4-3.gpkg", layer = "secteur_60ed_w84", delete_layer = FALSE, driver = "GPKG")


# EN: Read the ward measurements from an Excel file
# FR: Lire les mesures des secteurs à partir d'un fichier Excel
aream <- read_excel("C:/Users/thoma/Desktop/ward measurments.xlsx")


# EN: Join the ward measurements with the geopackage data
# FR: Joindre les mesures des secteurs avec les données du geopackage
newfile <- sec %>%
  left_join(aream, by = "WD23CD")


# EN: Rename the 'AREA_ms' column to 'AREA' and drop the original 'AREA' column
# FR: Renommer la colonne 'AREA_ms' en 'AREA' et supprimer la colonne 'AREA' d'origine
gpkg <- newfile %>%
  select(-AREA) %>%
  rename(AREA = AREA_ms)


# EN: Drop unnecessary columns
# FR: Supprimer les colonnes inutiles
gpkg <- gpkg %>% select(-WD23NMW)
gpkg <- gpkg %>% select(-WD23NM.y)
gpkg <- gpkg %>% select(-AREAEHECT)
gpkg <- gpkg %>% select(-AREACHECT)
gpkg <- gpkg %>% select(-AREAIHECT)
gpkg <- gpkg %>% select(-AREALHECT)
gpkg <- gpkg %>% select(-AREACHECT)


# EN: Write the updated data to the geopackage file
# FR: Écrire les données mises à jour dans le fichier geopackage
st_write(gpkg, "BD_geo/bdgeo_v4-3.gpkg", layer = "secteur_60ed_w84", delete_layer = FALSE, driver = "GPKG")


# EN: Read the updated geopackage file
# FR: Lire le fichier geopackage mis à jour
gpkg2 <- st_read("BD_geo/bdgeo_v4-3.gpkg")


# EN: Check the number of features and empty geometries
# FR: Vérifier le nombre d'entités et les géométries vides
cat("Total features:", nrow(gpkg2), "\n")
cat("Empty geometries:", sum(st_is_empty(gpkg2)), "\n")


# EN: Remove empty geometries
# FR: Supprimer les géométries vides
gpkg2 <- gpkg2[!st_is_empty(gpkg2),]


# EN: Check the geometry properties
# FR: Vérifier les propriétés de la géométrie
str(gpkg2$geom)
st_is_valid(gpkg2$geom)
st_crs(gpkg2)  
st_geometry_type(gpkg2)


# EN: Transform the coordinate reference system (CRS) to WGS84 (EPSG:4326)
# FR: Transformer le système de référence de coordonnées (CRS) en WGS84 (EPSG:4326)
target_crs <- "EPSG:4326"  # WGS84
gpkg_transformed <- st_transform(gpkg2, crs = target_crs)




# EN: Check the updated CRS
# FR: Vérifier le CRS mis à jour
st_crs(gpkg_transformed)


# EN: Check the number of features after removing empty geometries
# FR: Vérifier le nombre d'entités après avoir supprimé les géométries vides
cat("Total features after filtering:", nrow(gpkg_transformed), "\n")


# EN: Check for missing values in the transformed geopackage
# FR: Vérifier les valeurs manquantes dans le geopackage transformé
gpkg_transformed %>% summarise_all(~sum(is.na(.)))


# EN: Write the transformed geopackage to a file
# FR: Écrire le geopackage transformé dans un fichier
st_write(gpkg_transformed, "BD_geo/bdgeo_v4-3.gpkg", layer = "secteur_60ed_w84", delete_layer = FALSE, driver = "GPKG")


# EN: Read the transformed geopackage file
# FR: Lire le fichier geopackage transformé
gpkg3 <- st_read("BD_geo/bdgeo_v4-3.gpkg")



# EN: Add columns with specific values
# FR: Ajouter des colonnes avec des valeurs spécifiques
gpkg3$SECTEUR_EM <- gpkg3$CODE_SEC
gpkg3$PERIM <- word(gpkg3$ZONAGE_SEC, 1, 1, "-")
gpkg3$CENTROID_X <- gpkg3$X_W84
gpkg3$CENTROID_Y <- gpkg3$Y_W84




# EN: Write the updated geopackage to a file
# FR: Écrire le geopackage mis à jour dans un fichier
st_write(gpkg3, "BD_geo/bdgeo_v4-3.gpkg", layer = "secteur_60ed_w84", delete_layer = FALSE, driver = "GPKG")




