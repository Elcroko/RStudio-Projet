library(readr)
library(dplyr)

data_raw <- read_csv("donnee-dep-data.gouv-2024-geographie2025-produit-le2025-06-04.csv")

dim(data_raw)
names(data_raw)
head(data_raw)


table(data_raw$ANNEE)

