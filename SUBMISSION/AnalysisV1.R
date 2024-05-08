
library(tidyr)
library(dplyr) 
library(readr)
library(sf)
library(st)
library(sda)
library(tidyverse)
library(readxl)


df <- read_rds("C:/Users/thoma/Desktop/DELIVERABLE/FINALTRIPTABLEV3.RDS")
df2 <- read_rds("C:/Users/thoma/Desktop/DELIVERABLE/FINALPERSONTABLEV3.RDS")

# merge on ID_IND
df3 <- merge(df, df2, by = "ID_IND")

# slect columns we want from df3 lik ID_IND

df4 <- df3 %>% select(ID_IND, NDEP, O_SEC, D_SEC, H_START, M_START, H_END,
                   M_END, D9, D2, O_PURPOSE, D_PURPOSE,D5 ,MODE , RES_SEC.x,
                   DISAB,ETH ,KAGE ,SEX ,W_IND , O_COG, D_COG, RES_COG)

#save df4 as CSV
write.csv(df4, "C:/Users/thoma/Desktop/ANALYSISDATAV1.csv")

